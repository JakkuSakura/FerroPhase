Set-StrictMode -Version Latest
Set-PSDebug -Trace 1
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
$script:FpHosts['web-1'] = @{ transport = 'ssh'; address = '10.0.0.11'; user = 'deploy' }
$script:FpHosts['web-2'] = @{ transport = 'ssh'; address = '10.0.0.12'; user = 'deploy' }
function Invoke-FpRuntimeValidation {
    if (-not (Get-Command -Name 'Invoke-Expression' -ErrorAction SilentlyContinue)) { throw 'missing required command: Invoke-Expression' }
    if (-not (Get-Command -Name 'New-PSSession' -ErrorAction SilentlyContinue)) { throw 'missing required command: New-PSSession' }
    if (-not (Get-Command -Name 'docker' -ErrorAction SilentlyContinue)) { throw 'missing required command: docker' }
    if (-not (Get-Command -Name 'kubectl' -ErrorAction SilentlyContinue)) { throw 'missing required command: kubectl' }
    if (-not (Get-Command -Name 'pwsh' -ErrorAction SilentlyContinue)) { throw 'missing required command: pwsh' }
    if (-not (Get-Command -Name 'ssh' -ErrorAction SilentlyContinue)) { throw 'missing required command: ssh' }
}

Invoke-FpRuntimeValidation

function host_transport {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].transport)
}

function host_address {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].address)
}

function host_user {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].user)
}

function host_port {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].port)
}

function host_container {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].container)
}

function host_pod {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].pod)
}

function host_namespace {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].namespace)
}

function host_context {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].context)
}

function run_local_host {
    param([string]$cmd)
    Write-Output $(Invoke-Expression $cmd)
}

function run_host {
    param([string]$host, [string]$cmd)
    $transport = $(host_transport $host)
    switch -Exact ($transport) {
        'local' {
            Write-Output $(run_local_host $cmd)
        }
        'ssh' {
            Write-Output $(run_ssh_host $host $cmd)
        }
        'docker' {
            Write-Output $(run_docker_host $host $cmd)
        }
        'kubectl' {
            Write-Output $(run_kubectl_host $host $cmd)
        }
        'winrm' {
            Write-Output $(New-PSSession Invoke-Command Remove-PSSession $host $cmd)
        }
        default {
            Write-Output $(runtime_fail "unsupported transport: $transport")
        }
    }
}

function ssh_target {
    param([string]$host)
    $user = $(host_user $host)
    $address = $(host_address $host)
    if ($user -ne '') {
        Write-Output "$user@$address"
    } else {
        Write-Output $address
    }
}

function run_ssh_host {
    param([string]$host, [string]$cmd)
    $target = $(ssh_target $host)
    $port = $(host_port $host)
    if ($port -ne '') {
        Write-Output $(ssh -p $port $target $cmd)
    } else {
        Write-Output $(ssh $target $cmd)
    }
}

function run_docker_host {
    param([string]$host, [string]$cmd)
    $container = $(host_container $host)
    $user = $(host_user $host)
    if ($user -ne '') {
        Write-Output $(docker exec --user $user $container 'sh' '-lc' $cmd)
    } else {
        Write-Output $(docker exec $container 'sh' '-lc' $cmd)
    }
}

function run_kubectl_host {
    param([string]$host, [string]$cmd)
    $context = $(host_context $host)
    $namespace = $(host_namespace $host)
    $container = $(host_container $host)
    $pod = $(host_pod $host)
    switch -Exact ($context) {
        '' {
            switch -Exact ($namespace) {
                '' {
                    switch -Exact ($container) {
                        '' {
                            Write-Output $(kubectl exec $pod '--' 'sh' '-lc' $cmd)
                        }
                        default {
                            Write-Output $(kubectl exec -c $container $pod '--' 'sh' '-lc' $cmd)
                        }
                    }
                }
                default {
                    switch -Exact ($container) {
                        '' {
                            Write-Output $(kubectl -n $namespace 'exec' $pod '--' 'sh' '-lc' $cmd)
                        }
                        default {
                            Write-Output $(kubectl -n $namespace 'exec' '-c' $container $pod '--' 'sh' '-lc' $cmd)
                        }
                    }
                }
            }
        }
        default {
            switch -Exact ($namespace) {
                '' {
                    switch -Exact ($container) {
                        '' {
                            Write-Output $(kubectl --context $context 'exec' $pod '--' 'sh' '-lc' $cmd)
                        }
                        default {
                            Write-Output $(kubectl --context $context 'exec' '-c' $container $pod '--' 'sh' '-lc' $cmd)
                        }
                    }
                }
                default {
                    switch -Exact ($container) {
                        '' {
                            Write-Output $(kubectl --context $context '-n' $namespace 'exec' $pod '--' 'sh' '-lc' $cmd)
                        }
                        default {
                            Write-Output $(kubectl --context $context '-n' $namespace 'exec' '-c' $container $pod '--' 'sh' '-lc' $cmd)
                        }
                    }
                }
            }
        }
    }
}

function command_with_options {
    param([string]$command, [string]$cwd, [string]$sudo)
    if ($cwd -ne '') {
        if ($sudo) {
            Write-Output "sudo cd $cwd && $command"
        } else {
            Write-Output "cd $cwd && $command"
        }
    } else {
        if ($sudo) {
            Write-Output "sudo $command"
        } else {
            Write-Output $command
        }
    }
}

function process_ok {
    param([string]$command)
    Write-Output $(ok $command)
}

function shell_run {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if ($only_if -ne '') {
        if (process_ok $only_if) {
            Write-Output $(shell_run_after_only_if $host $command $unless $creates $removes)
        }
    } else {
        Write-Output $(shell_run_after_only_if $host $command $unless $creates $removes)
    }
}

function shell_run_after_only_if {
    param([string]$host, [string]$command, [string]$unless, [string]$creates, [string]$removes)
    if ($unless -ne '') {
        if ($true) {
            Write-Output $(shell_run_after_unless $host $command $creates $removes)
        }
    } else {
        Write-Output $(shell_run_after_unless $host $command $creates $removes)
    }
}

function shell_run_after_unless {
    param([string]$host, [string]$command, [string]$creates, [string]$removes)
    if ($creates -ne '') {
        if (process_ok "test ! -e $creates") {
            Write-Output $(shell_run_after_creates $host $command $removes)
        }
    } else {
        Write-Output $(shell_run_after_creates $host $command $removes)
    }
}

function shell_run_after_creates {
    param([string]$host, [string]$command, [string]$removes)
    if ($removes -ne '') {
        if (process_ok "test -e $removes") {
            run_host $host $command
            Write-Output $(runtime_set_changed $true)
        }
    } else {
        run_host $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function shell {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function transport {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].transport)
}

function address {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].address)
}

function user {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].user)
}

function port {
    param([string]$host)
    Write-Output $($script:FpHosts[$host].port)
}

function ok {
    param([string]$command)
    Write-Output $((& { pwsh -Command $command; $LASTEXITCODE -eq 0 }))
}

$__fpTrySuccess1 = $false
$__fpTryHandled2 = $false
try {
    $__fpTrySuccess3 = $false
    $__fpTryHandled4 = $false
    try {
        shell 'echo deploy body' '' '' '' '' '' '' ''
        $__fpTrySuccess3 = $true
    } catch {
        if (-not $__fpTryHandled4) {
            throw
        }
    } finally {
        shell 'echo cleanup' '' '' '' '' '' '' ''
    }
    $__fpTrySuccess1 = $true
} catch {
    if (-not $__fpTryHandled2) {
        $err = $_
        shell "echo deploy failed=$err" '' '' '' '' '' '' ''
        $__fpTryHandled2 = $true
    }
    if (-not $__fpTryHandled2) {
        throw
    }
} finally {
    shell 'echo deploy finally' '' '' '' '' '' '' ''
}
if ($__fpTrySuccess1) {
    shell 'echo deploy success' '' '' '' '' '' '' ''
}
