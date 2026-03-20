Set-StrictMode -Version Latest
Set-PSDebug -Trace 1
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
$script:FpHosts['localhost'] = @{ transport = 'local' }
$script:FpHosts['ssh-web'] = @{ transport = 'ssh'; address = '10.0.0.11'; user = 'deploy'; port = 22 }
$script:FpHosts['windows-admin'] = @{ transport = 'winrm'; address = '10.0.0.21'; user = 'Administrator'; port = 5985; password = 'change-me'; scheme = 'http' }
$script:FpHosts['k8s-api'] = @{ transport = 'kubectl'; container = 'api'; pod = 'api-7f9f6'; namespace = 'prod'; context = 'prod-cluster' }
$script:FpHosts['docker-app'] = @{ transport = 'docker'; user = 'root'; container = 'app' }
function Invoke-FpRuntimeValidation {
    if (-not (Get-Command -Name 'Invoke-Expression' -ErrorAction SilentlyContinue)) { throw 'missing required command: Invoke-Expression' }
    if (-not (Get-Command -Name 'New-PSSession' -ErrorAction SilentlyContinue)) { throw 'missing required command: New-PSSession' }
    if (-not (Get-Command -Name 'docker' -ErrorAction SilentlyContinue)) { throw 'missing required command: docker' }
    if (-not (Get-Command -Name 'kubectl' -ErrorAction SilentlyContinue)) { throw 'missing required command: kubectl' }
    if (-not (Get-Command -Name 'pwsh' -ErrorAction SilentlyContinue)) { throw 'missing required command: pwsh' }
    if (-not (Get-Command -Name 'ssh' -ErrorAction SilentlyContinue)) { throw 'missing required command: ssh' }
}

Invoke-FpRuntimeValidation

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

function should_apply {
    param([string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    if ($only_if -ne '') {
        if ($true) {
        }
    }
    if ($unless -ne '') {
        if (process_ok $unless) {
        }
    }
    if ($creates -ne '') {
        if ($true) {
        }
    }
    if ($removes -ne '') {
        if ($true) {
        }
    }
    Write-Output $true
}

function shell_run_local {
    param([string]$_host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (should_apply $only_if $unless $creates $removes) {
        run_local_host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function shell_run_ssh {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (should_apply $only_if $unless $creates $removes) {
        run_ssh_host $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function shell_run_docker {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (should_apply $only_if $unless $creates $removes) {
        run_docker_host $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function shell_run_kubectl {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (should_apply $only_if $unless $creates $removes) {
        run_kubectl_host $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function shell_run_winrm {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (should_apply $only_if $unless $creates $removes) {
        $__fpHost = $host
        $__fpEntry = $script:FpHosts[$__fpHost]
        $__fpSessionArgs = @{ ComputerName = $__fpEntry.address }
        if ($__fpEntry.port) { $__fpSessionArgs.Port = $__fpEntry.port }
        $__fpScheme = if ($__fpEntry.scheme) { $__fpEntry.scheme.ToLowerInvariant() } else { 'http' }
        switch ($__fpScheme) {
            'http' {}
            'https' { $__fpSessionArgs.UseSSL = $true }
            default { throw "unsupported winrm scheme for $__fpHost: $($__fpEntry.scheme)" }
        }
        if (-not $__fpEntry.password) { throw "winrm password is required for non-interactive PowerShell target: $__fpHost" }
        $__fpSecurePassword = ConvertTo-SecureString $__fpEntry.password -AsPlainText -Force
        $__fpCredential = New-Object System.Management.Automation.PSCredential($__fpEntry.user, $__fpSecurePassword)
        $__fpSession = New-PSSession -Credential $__fpCredential @__fpSessionArgs
        try {
            Invoke-Command -Session $__fpSession -ScriptBlock ([scriptblock]::Create($command))
        } finally {
            Remove-PSSession -Session $__fpSession
        }
        Write-Output $(runtime_set_changed $true)
    }
}

function shell_local {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run_local $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function shell_ssh {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run_ssh $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function shell_docker {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run_docker $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function shell_kubectl {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run_kubectl $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function shell_winrm {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run_winrm $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
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

shell_local 'echo local hello' 'localhost' '' '' '' '' '' ''
shell_ssh 'echo ssh hello' 'ssh-web' '' '' '' '' '' ''
shell_docker 'echo docker hello' 'docker-app' '' '' '' '' '' ''
shell_kubectl 'echo kubectl hello' 'k8s-api' '' '' '' '' '' ''
shell_winrm 'Write-Host winrm hello' 'windows-admin' '' '' '' '' '' ''
