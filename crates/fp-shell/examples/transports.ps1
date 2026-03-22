Set-StrictMode -Version Latest
Set-PSDebug -Trace 1
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
function Invoke-FpRuntimeValidation {
    if (-not (Get-Command -Name 'Invoke-Expression' -ErrorAction SilentlyContinue)) { throw 'missing required command: Invoke-Expression' }
    if (-not (Get-Command -Name 'New-PSSession' -ErrorAction SilentlyContinue)) { throw 'missing required command: New-PSSession' }
    if (-not (Get-Command -Name 'docker' -ErrorAction SilentlyContinue)) { throw 'missing required command: docker' }
    if (-not (Get-Command -Name 'kubectl' -ErrorAction SilentlyContinue)) { throw 'missing required command: kubectl' }
    if (-not (Get-Command -Name 'pwsh' -ErrorAction SilentlyContinue)) { throw 'missing required command: pwsh' }
    if (-not (Get-Command -Name 'ssh' -ErrorAction SilentlyContinue)) { throw 'missing required command: ssh' }
}

Invoke-FpRuntimeValidation

function __fp_std_hosts_address_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output ''
        }
        'ssh-web' {
            Write-Output '10.0.0.11'
        }
        'docker-app' {
            Write-Output ''
        }
        'k8s-api' {
            Write-Output ''
        }
        'windows-admin' {
            Write-Output '10.0.0.21'
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_hosts_user_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output ''
        }
        'ssh-web' {
            Write-Output 'deploy'
        }
        'docker-app' {
            Write-Output 'root'
        }
        'k8s-api' {
            Write-Output ''
        }
        'windows-admin' {
            Write-Output 'Administrator'
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_hosts_port_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output '0'
        }
        'ssh-web' {
            Write-Output '22'
        }
        'docker-app' {
            Write-Output '0'
        }
        'k8s-api' {
            Write-Output '0'
        }
        'windows-admin' {
            Write-Output '5985'
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_hosts_container_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output ''
        }
        'ssh-web' {
            Write-Output ''
        }
        'docker-app' {
            Write-Output 'app'
        }
        'k8s-api' {
            Write-Output 'api'
        }
        'windows-admin' {
            Write-Output ''
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_hosts_pod_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output ''
        }
        'ssh-web' {
            Write-Output ''
        }
        'docker-app' {
            Write-Output ''
        }
        'k8s-api' {
            Write-Output 'api-7f9f6'
        }
        'windows-admin' {
            Write-Output ''
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_hosts_namespace_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output ''
        }
        'ssh-web' {
            Write-Output ''
        }
        'docker-app' {
            Write-Output ''
        }
        'k8s-api' {
            Write-Output 'prod'
        }
        'windows-admin' {
            Write-Output ''
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_hosts_context_ {
    param([string]$host)
    switch -Exact ($host) {
        'localhost' {
            Write-Output ''
        }
        'ssh-web' {
            Write-Output ''
        }
        'docker-app' {
            Write-Output ''
        }
        'k8s-api' {
            Write-Output 'prod-cluster'
        }
        'windows-admin' {
            Write-Output ''
        }
        default {
            Write-Output ''
        }
    }
}

function __fp_std_ops_server_shell_local_ {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(__fp_std_shell_backend_command_with_options_ $command $cwd $sudo)
    __fp_std_shell_backend_shell_run_local_ $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function __fp_std_ops_server_shell_ssh_ {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(__fp_std_shell_backend_command_with_options_ $command $cwd $sudo)
    __fp_std_shell_backend_shell_run_ssh_ $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function __fp_std_ops_server_shell_docker_ {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(__fp_std_shell_backend_command_with_options_ $command $cwd $sudo)
    __fp_std_shell_backend_shell_run_docker_ $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function __fp_std_ops_server_shell_kubectl_ {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(__fp_std_shell_backend_command_with_options_ $command $cwd $sudo)
    __fp_std_shell_backend_shell_run_kubectl_ $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function __fp_std_ops_server_shell_winrm_ {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(__fp_std_shell_backend_command_with_options_ $command $cwd $sudo)
    __fp_std_shell_backend_shell_run_winrm_ $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function __fp_std_shell_backend_host_address_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_address_ $host)
}

function __fp_std_shell_backend_host_user_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_user_ $host)
}

function __fp_std_shell_backend_host_port_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_port_ $host)
}

function __fp_std_shell_backend_host_container_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_container_ $host)
}

function __fp_std_shell_backend_host_pod_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_pod_ $host)
}

function __fp_std_shell_backend_host_namespace_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_namespace_ $host)
}

function __fp_std_shell_backend_host_context_ {
    param([string]$host)
    Write-Output $(__fp_std_hosts_context_ $host)
}

function __fp_std_shell_backend_run_local_host_ {
    param([string]$cmd)
    Write-Output $(Invoke-Expression $cmd)
}

function __fp_std_shell_backend_ssh_target_ {
    param([string]$host)
    $user = $(__fp_std_shell_backend_host_user_ $host)
    $address = $(__fp_std_shell_backend_host_address_ $host)
    if ($user -ne '') {
        Write-Output "$user@$address"
    } else {
        Write-Output $address
    }
}

function __fp_std_shell_backend_run_ssh_host_ {
    param([string]$host, [string]$cmd)
    $target = $(__fp_std_shell_backend_ssh_target_ $host)
    $port = $(__fp_std_shell_backend_host_port_ $host)
    if ($port -ne '') {
        Write-Output $(ssh -p $port $target $cmd)
    } else {
        Write-Output $(ssh $target $cmd)
    }
}

function __fp_std_shell_backend_run_docker_host_ {
    param([string]$host, [string]$cmd)
    $container = $(__fp_std_shell_backend_host_container_ $host)
    $user = $(__fp_std_shell_backend_host_user_ $host)
    if ($user -ne '') {
        Write-Output $(docker exec --user $user $container 'sh' '-lc' $cmd)
    } else {
        Write-Output $(docker exec $container 'sh' '-lc' $cmd)
    }
}

function __fp_std_shell_backend_run_kubectl_host_ {
    param([string]$host, [string]$cmd)
    $context = $(__fp_std_shell_backend_host_context_ $host)
    $namespace = $(__fp_std_shell_backend_host_namespace_ $host)
    $container = $(__fp_std_shell_backend_host_container_ $host)
    $pod = $(__fp_std_shell_backend_host_pod_ $host)
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

function __fp_std_shell_backend_command_with_options_ {
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

function __fp_std_shell_backend_process_ok_ {
    param([string]$command)
    Write-Output $(__fp_std_shell_process_process_ok_ $command)
}

function __fp_std_shell_backend_should_apply_ {
    param([string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    if ($only_if -ne '') {
        if ($true) {
        }
    }
    if ($unless -ne '') {
        if (__fp_std_shell_backend_process_ok_ $unless) {
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

function __fp_std_shell_backend_shell_run_local_ {
    param([string]$_host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (__fp_std_shell_backend_should_apply_ $only_if $unless $creates $removes) {
        __fp_std_shell_backend_run_local_host_ $command
        Write-Output $(runtime_set_changed $true)
    }
}

function __fp_std_shell_backend_shell_run_ssh_ {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (__fp_std_shell_backend_should_apply_ $only_if $unless $creates $removes) {
        __fp_std_shell_backend_run_ssh_host_ $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function __fp_std_shell_backend_shell_run_docker_ {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (__fp_std_shell_backend_should_apply_ $only_if $unless $creates $removes) {
        __fp_std_shell_backend_run_docker_host_ $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function __fp_std_shell_backend_shell_run_kubectl_ {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (__fp_std_shell_backend_should_apply_ $only_if $unless $creates $removes) {
        __fp_std_shell_backend_run_kubectl_host_ $host $command
        Write-Output $(runtime_set_changed $true)
    }
}

function __fp_std_shell_backend_shell_run_winrm_ {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if (__fp_std_shell_backend_should_apply_ $only_if $unless $creates $removes) {
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

function __fp_std_shell_process_process_ok_ {
    param([string]$command)
    Write-Output $((& { pwsh -Command $command; $LASTEXITCODE -eq 0 }))
}

__fp_std_ops_server_shell_local_ 'echo local hello' 'localhost' '' '' '' '' '' ''
__fp_std_ops_server_shell_ssh_ 'echo ssh hello' 'ssh-web' '' '' '' '' '' ''
__fp_std_ops_server_shell_docker_ 'echo docker hello' 'docker-app' '' '' '' '' '' ''
__fp_std_ops_server_shell_kubectl_ 'echo kubectl hello' 'k8s-api' '' '' '' '' '' ''
__fp_std_ops_server_shell_winrm_ 'Write-Host winrm hello' 'windows-admin' '' '' '' '' '' ''
