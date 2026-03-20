Set-StrictMode -Version Latest
Set-PSDebug -Trace 1
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
$script:FpHosts['localhost'] = @{ transport = 'local' }
$script:FpHosts['docker-app'] = @{ transport = 'docker'; user = 'root'; container = 'app' }
$script:FpHosts['k8s-api'] = @{ transport = 'kubectl'; container = 'api'; pod = 'api-7f9f6'; namespace = 'prod'; context = 'prod-cluster' }
$script:FpHosts['windows-admin'] = @{ transport = 'winrm'; address = '10.0.0.21'; user = 'Administrator'; port = 5985; password = 'change-me'; scheme = 'http' }
$script:FpHosts['ssh-web'] = @{ transport = 'ssh'; address = '10.0.0.11'; user = 'deploy'; port = 22 }
function Invoke-FpRuntimeValidation {
    if (-not (Get-Command -Name 'Copy-Item' -ErrorAction SilentlyContinue)) { throw 'missing required command: Copy-Item' }
    if (-not (Get-Command -Name 'Get-Command' -ErrorAction SilentlyContinue)) { throw 'missing required command: Get-Command' }
    if (-not (Get-Command -Name 'Get-Content' -ErrorAction SilentlyContinue)) { throw 'missing required command: Get-Content' }
    if (-not (Get-Command -Name 'Invoke-Expression' -ErrorAction SilentlyContinue)) { throw 'missing required command: Invoke-Expression' }
    if (-not (Get-Command -Name 'New-PSSession' -ErrorAction SilentlyContinue)) { throw 'missing required command: New-PSSession' }
    if (-not (Get-Command -Name 'Remove-Item' -ErrorAction SilentlyContinue)) { throw 'missing required command: Remove-Item' }
    if (-not (Get-Command -Name 'Test-Path' -ErrorAction SilentlyContinue)) { throw 'missing required command: Test-Path' }
    if (-not (Get-Command -Name 'docker' -ErrorAction SilentlyContinue)) { throw 'missing required command: docker' }
    if (-not (Get-Command -Name 'kubectl' -ErrorAction SilentlyContinue)) { throw 'missing required command: kubectl' }
    if (-not (Get-Command -Name 'pwsh' -ErrorAction SilentlyContinue)) { throw 'missing required command: pwsh' }
    if (-not (Get-Command -Name 'rsync' -ErrorAction SilentlyContinue)) { throw 'missing required command: rsync' }
    if (-not (Get-Command -Name 'scp' -ErrorAction SilentlyContinue)) { throw 'missing required command: scp' }
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
    Invoke-Expression $cmd
}

function copy_local_host {
    param([string]$src, [string]$dest)
    Copy-Item -Force $src $dest
}

function run_host {
    param([string]$host, [string]$cmd)
    $transport = $(host_transport $host)
    switch -Exact ($transport) {
        'local' {
            run_local_host $cmd
        }
        'ssh' {
            run_ssh_host $host $cmd
        }
        'docker' {
            run_docker_host $host $cmd
        }
        'kubectl' {
            run_kubectl_host $host $cmd
        }
        'winrm' {
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
                Invoke-Command -Session $__fpSession -ScriptBlock ([scriptblock]::Create($cmd))
            } finally {
                Remove-PSSession -Session $__fpSession
            }
        }
        default {
            throw "unsupported transport: $transport"
        }
    }
}

function copy_host {
    param([string]$host, [string]$src, [string]$dest)
    $transport = $(host_transport $host)
    switch -Exact ($transport) {
        'local' {
            copy_local_host $src $dest
        }
        'ssh' {
            copy_ssh_host $host $src $dest
        }
        'docker' {
            copy_docker_host $host $src $dest
        }
        'kubectl' {
            copy_kubectl_host $host $src $dest
        }
        'winrm' {
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
            $__fpDestination = $dest
            $__fpDirectory = [System.IO.Path]::GetDirectoryName($__fpDestination)
            try {
                if ($__fpDirectory) {
                    Invoke-Command -Session $__fpSession -ScriptBlock { param([string]$Directory) [System.IO.Directory]::CreateDirectory($Directory) | Out-Null } -ArgumentList $__fpDirectory
                }
                Copy-Item -ToSession $__fpSession -Path $src -Destination $__fpDestination -Force
            } finally {
                Remove-PSSession -Session $__fpSession
            }
        }
        default {
            throw "unsupported transport for copy: $transport"
        }
    }
}

function template_host {
    param([string]$host, [string]$src, [string]$dest, [string]$vars)
    $tmp = $([System.IO.Path]::GetTempFileName())
    $__fpContent = Get-Content -Raw $src
    foreach ($pair in ($vars -split ';')) {
        if ($pair) {
            $name, $value = $pair -split '=', 2
            $__fpContent = $__fpContent.Replace("`${$name}", $value)
        }
    }
    Set-Content -Path $tmp -Value $__fpContent
    copy_host $host $tmp $dest
    Remove-Item -Force -ErrorAction SilentlyContinue $tmp
}

function rsync_host {
    param([string]$host, [string]$flags, [string]$src, [string]$dest)
    $transport = $(host_transport $host)
    switch -Exact ($transport) {
        'local' {
            rsync $flags $src $dest '' '' '' '' '' '' '' ''
        }
        default {
            rsync_remote_host $host $flags $src $dest
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
        ssh -p $port $target $cmd
    } else {
        ssh $target $cmd
    }
}

function copy_ssh_host {
    param([string]$host, [string]$src, [string]$dest)
    $target = $(ssh_target $host)
    $remote = "$target:$dest"
    $port = $(host_port $host)
    if ($port -ne '') {
        scp -P $port $src $remote
    } else {
        scp $src $remote
    }
}

function run_docker_host {
    param([string]$host, [string]$cmd)
    $container = $(host_container $host)
    $user = $(host_user $host)
    if ($user -ne '') {
        docker exec --user $user $container 'sh' '-lc' $cmd
    } else {
        docker exec $container 'sh' '-lc' $cmd
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
                            kubectl exec $pod '--' 'sh' '-lc' $cmd
                        }
                        default {
                            kubectl exec -c $container $pod '--' 'sh' '-lc' $cmd
                        }
                    }
                }
                default {
                    switch -Exact ($container) {
                        '' {
                            kubectl -n $namespace 'exec' $pod '--' 'sh' '-lc' $cmd
                        }
                        default {
                            kubectl -n $namespace 'exec' '-c' $container $pod '--' 'sh' '-lc' $cmd
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
                            kubectl --context $context 'exec' $pod '--' 'sh' '-lc' $cmd
                        }
                        default {
                            kubectl --context $context 'exec' '-c' $container $pod '--' 'sh' '-lc' $cmd
                        }
                    }
                }
                default {
                    switch -Exact ($container) {
                        '' {
                            kubectl --context $context '-n' $namespace 'exec' $pod '--' 'sh' '-lc' $cmd
                        }
                        default {
                            kubectl --context $context '-n' $namespace 'exec' '-c' $container $pod '--' 'sh' '-lc' $cmd
                        }
                    }
                }
            }
        }
    }
}

function copy_docker_host {
    param([string]$host, [string]$src, [string]$dest)
    $container = $(host_container $host)
    docker cp $src "$container:$dest"
}

function copy_kubectl_host {
    param([string]$host, [string]$src, [string]$dest)
    $context = $(host_context $host)
    $namespace = $(host_namespace $host)
    $remote = "$(host_pod $host):$dest"
    switch -Exact ($context) {
        '' {
            switch -Exact ($namespace) {
                '' {
                    kubectl cp $src $remote
                }
                default {
                    kubectl -n $namespace 'cp' $src $remote
                }
            }
        }
        default {
            switch -Exact ($namespace) {
                '' {
                    kubectl --context $context 'cp' $src $remote
                }
                default {
                    kubectl --context $context '-n' $namespace 'cp' $src $remote
                }
            }
        }
    }
}

function rsync_remote_target {
    param([string]$host)
    $address = $(host_address $host)
    if ($address -eq '') {
        throw "host is not rsync-reachable: missing address for $host"
        Write-Output ''
    } else {
        $user = $(host_user $host)
        if ($user -ne '') {
            Write-Output "$user@$address"
        } else {
            Write-Output $address
        }
    }
}

function rsync_remote_host {
    param([string]$host, [string]$flags, [string]$src, [string]$dest)
    $remote = "$(rsync_remote_target $host):$dest"
    $port = $(host_port $host)
    if ($port -ne '') {
        rsync -e "ssh -p $port" $flags $src $remote
    } else {
        rsync $flags $src $remote '' '' '' '' '' '' '' ''
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

function rsync_flag_string {
    param([string]$archive, [string]$compress, [string]$delete, [string]$checksum)
    if ($archive) {
        if ($compress) {
            Write-Output $(rsync_flag_string_suffix '-az' $delete $checksum)
        } else {
            Write-Output $(rsync_flag_string_suffix '-a' $delete $checksum)
        }
    } else {
        if ($compress) {
            Write-Output $(rsync_flag_string_suffix '-z' $delete $checksum)
        } else {
            Write-Output $(rsync_flag_string_suffix '' $delete $checksum)
        }
    }
}

function rsync_flag_string_suffix {
    param([string]$base, [string]$delete, [string]$checksum)
    if ($delete) {
        if ($checksum) {
            if ($base -ne '') {
                Write-Output "$base --delete --checksum"
            } else {
                Write-Output '--delete --checksum'
            }
        } else {
            if ($base -ne '') {
                Write-Output "$base --delete"
            } else {
                Write-Output '--delete'
            }
        }
    } else {
        if ($checksum) {
            if ($base -ne '') {
                Write-Output "$base --checksum"
            } else {
                Write-Output '--checksum'
            }
        } else {
            Write-Output $base
        }
    }
}

function shell_run {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if ($only_if -ne '') {
        if (process_ok $only_if) {
            shell_run_after_only_if $host $command $unless $creates $removes
        }
    } else {
        shell_run_after_only_if $host $command $unless $creates $removes
    }
}

function shell_run_after_only_if {
    param([string]$host, [string]$command, [string]$unless, [string]$creates, [string]$removes)
    if ($unless -ne '') {
        if ($true) {
            shell_run_after_unless $host $command $creates $removes
        }
    } else {
        shell_run_after_unless $host $command $creates $removes
    }
}

function shell_run_after_unless {
    param([string]$host, [string]$command, [string]$creates, [string]$removes)
    if ($creates -ne '') {
        if (process_ok "test ! -e $creates") {
            shell_run_after_creates $host $command $removes
        }
    } else {
        shell_run_after_creates $host $command $removes
    }
}

function shell_run_after_creates {
    param([string]$host, [string]$command, [string]$removes)
    if ($removes -ne '') {
        if (process_ok "test -e $removes") {
            run_host $host $command
            $script:fpLastChanged = $true
        }
    } else {
        run_host $host $command
        $script:fpLastChanged = $true
    }
}

function shell_copy {
    param([string]$host, [string]$src, [string]$dest, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if ($only_if -ne '') {
        if (process_ok $only_if) {
            shell_copy_after_only_if $host $src $dest $unless $creates $removes
        }
    } else {
        shell_copy_after_only_if $host $src $dest $unless $creates $removes
    }
}

function shell_copy_after_only_if {
    param([string]$host, [string]$src, [string]$dest, [string]$unless, [string]$creates, [string]$removes)
    if ($unless -ne '') {
        if ($true) {
            shell_copy_after_unless $host $src $dest $creates $removes
        }
    } else {
        shell_copy_after_unless $host $src $dest $creates $removes
    }
}

function shell_copy_after_unless {
    param([string]$host, [string]$src, [string]$dest, [string]$creates, [string]$removes)
    if ($creates -ne '') {
        if (process_ok "test ! -e $creates") {
            shell_copy_after_creates $host $src $dest $removes
        }
    } else {
        shell_copy_after_creates $host $src $dest $removes
    }
}

function shell_copy_after_creates {
    param([string]$host, [string]$src, [string]$dest, [string]$removes)
    if ($removes -ne '') {
        if (process_ok "test -e $removes") {
            copy_host $host $src $dest
            $script:fpLastChanged = $true
        }
    } else {
        copy_host $host $src $dest
        $script:fpLastChanged = $true
    }
}

function shell_template {
    param([string]$host, [string]$src, [string]$dest, [string]$vars, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if ($only_if -ne '') {
        if (process_ok $only_if) {
            shell_template_after_only_if $host $src $dest $vars $unless $creates $removes
        }
    } else {
        shell_template_after_only_if $host $src $dest $vars $unless $creates $removes
    }
}

function shell_template_after_only_if {
    param([string]$host, [string]$src, [string]$dest, [string]$vars, [string]$unless, [string]$creates, [string]$removes)
    if ($unless -ne '') {
        if ($true) {
            shell_template_after_unless $host $src $dest $vars $creates $removes
        }
    } else {
        shell_template_after_unless $host $src $dest $vars $creates $removes
    }
}

function shell_template_after_unless {
    param([string]$host, [string]$src, [string]$dest, [string]$vars, [string]$creates, [string]$removes)
    if ($creates -ne '') {
        if (process_ok "test ! -e $creates") {
            shell_template_after_creates $host $src $dest $vars $removes
        }
    } else {
        shell_template_after_creates $host $src $dest $vars $removes
    }
}

function shell_template_after_creates {
    param([string]$host, [string]$src, [string]$dest, [string]$vars, [string]$removes)
    if ($removes -ne '') {
        if (process_ok "test -e $removes") {
            template_host $host $src $dest $vars
            $script:fpLastChanged = $true
        }
    } else {
        template_host $host $src $dest $vars
        $script:fpLastChanged = $true
    }
}

function shell_rsync {
    param([string]$host, [string]$flags, [string]$src, [string]$dest, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if ($only_if -ne '') {
        if (process_ok $only_if) {
            shell_rsync_after_only_if $host $flags $src $dest $unless $creates $removes
        }
    } else {
        shell_rsync_after_only_if $host $flags $src $dest $unless $creates $removes
    }
}

function shell_rsync_after_only_if {
    param([string]$host, [string]$flags, [string]$src, [string]$dest, [string]$unless, [string]$creates, [string]$removes)
    if ($unless -ne '') {
        if ($true) {
            shell_rsync_after_unless $host $flags $src $dest $creates $removes
        }
    } else {
        shell_rsync_after_unless $host $flags $src $dest $creates $removes
    }
}

function shell_rsync_after_unless {
    param([string]$host, [string]$flags, [string]$src, [string]$dest, [string]$creates, [string]$removes)
    if ($creates -ne '') {
        if (process_ok "test ! -e $creates") {
            shell_rsync_after_creates $host $flags $src $dest $removes
        }
    } else {
        shell_rsync_after_creates $host $flags $src $dest $removes
    }
}

function shell_rsync_after_creates {
    param([string]$host, [string]$flags, [string]$src, [string]$dest, [string]$removes)
    if ($removes -ne '') {
        if (process_ok "test -e $removes") {
            rsync_host $host $flags $src $dest
            $script:fpLastChanged = $true
        }
    } else {
        rsync_host $host $flags $src $dest
        $script:fpLastChanged = $true
    }
}

function shell {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function copy {
    param([string]$src, [string]$dest, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    shell_copy $hosts $src $dest $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function template {
    param([string]$src, [string]$dest, [string]$hosts, [string]$vars, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    shell_template $hosts $src $dest $vars $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function rsync {
    param([string]$src, [string]$dest, [string]$hosts, [string]$archive, [string]$compress, [string]$delete, [string]$checksum, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $flags = $(rsync_flag_string $archive $compress $delete $checksum)
    shell_rsync $hosts $flags $src $dest $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function restart {
    param([string]$name, [string]$hosts, [string]$sudo, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    Write-Output $(shell "systemctl restart $name" $hosts $only_if $unless $creates $removes $sudo '')
}

function has_command {
    param([string]$command)
    Write-Output $(Get-Command $command)
}

function file_exists {
    param([string]$path)
    Write-Output $(Test-Path -PathType Leaf $path)
}

function dir_exists {
    param([string]$path)
    Write-Output $(Test-Path -PathType Container $path)
}

function path_exists {
    param([string]$path)
    Write-Output $(Test-Path $path)
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

function has_rsync {
    Write-Output $(has_command 'rsync')
}

function has_ssh {
    Write-Output $(has_command 'ssh')
}

function has_docker {
    Write-Output $(has_command 'docker')
}

function has_kubectl {
    Write-Output $(has_command 'kubectl')
}

function has_pwsh {
    Write-Output $(has_command 'pwsh')
}

function host_supports_rsync {
    param([string]$host)
    switch -Exact ($(transport $host)) {
        'ssh' {
        }
        'docker' {
        }
        'kubectl' {
        }
        'winrm' {
        }
        'local' {
            Write-Output $(has_rsync)
        }
        default {
            Write-Output $false
        }
    }
}

function raw {
    param([string]$text)
    Write-Output $text
}

function pipe {
    param([string]$lhs, [string]$rhs)
    Write-Output "$lhs | $rhs"
}

function stdout_to {
    param([string]$command, [string]$path)
    Write-Output "$command > $path"
}

function stdout_append {
    param([string]$command, [string]$path)
    Write-Output "$command >> $path"
}

function stderr_to {
    param([string]$command, [string]$path)
    Write-Output "$command 2> $path"
}

function stderr_append {
    param([string]$command, [string]$path)
    Write-Output "$command 2>> $path"
}

function run {
    param([string]$command)
    shell $command '' '' '' '' '' '' ''
}

function ok {
    param([string]$command)
    Write-Output $((& { pwsh -Command $command; $LASTEXITCODE -eq 0 }))
}

shell 'echo local hello' '' '' '' '' '' '' ''
shell 'echo ssh hello' 'ssh-web' '' '' '' '' '' ''
shell 'echo docker hello' 'docker-app' '' '' '' '' '' ''
shell 'echo kubectl hello' 'k8s-api' '' '' '' '' '' ''
shell 'Write-Host winrm hello' 'windows-admin' '' '' '' '' '' ''
