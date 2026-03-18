Set-StrictMode -Version Latest
Set-PSDebug -Trace 1
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
$script:FpHosts['ssh-web'] = @{ transport = 'ssh'; address = '10.0.0.11'; user = 'deploy'; port = 22 }
$script:FpHosts['localhost'] = @{ transport = 'local' }
$script:FpHosts['docker-app'] = @{ transport = 'docker'; container = 'app'; user = 'root' }
$script:FpHosts['k8s-api'] = @{ transport = 'kubectl'; pod = 'api-7f9f6'; namespace = 'prod'; container = 'api'; context = 'prod-cluster' }
$script:FpHosts['windows-admin'] = @{ transport = 'winrm'; address = '10.0.0.21'; user = 'Administrator'; password = 'change-me'; port = 5985; scheme = 'http' }
function Invoke-FpRuntimeValidation {
    if (-not (Get-Command -Name 'Invoke-Expression' -ErrorAction SilentlyContinue)) { throw 'missing required command: Invoke-Expression' }
    if (-not (Get-Command -Name 'New-PSSession' -ErrorAction SilentlyContinue)) { throw 'missing required command: New-PSSession' }
    if (-not (Get-Command -Name 'docker' -ErrorAction SilentlyContinue)) { throw 'missing required command: docker' }
    if (-not (Get-Command -Name 'kubectl' -ErrorAction SilentlyContinue)) { throw 'missing required command: kubectl' }
    if (-not (Get-Command -Name 'ssh' -ErrorAction SilentlyContinue)) { throw 'missing required command: ssh' }
}

Invoke-FpRuntimeValidation

function host_transport {
    param([string]$host)
    runtime_host_transport $host
}

function run_local_host {
    param([string]$cmd)
    $cmd
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
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }
            $__fpTarget = if ($__fpEntry.user) { "$($__fpEntry.user)@$($__fpAddress)" } else { $__fpAddress }
            if ($__fpEntry.port) {
                & ssh -p $__fpEntry.port $__fpTarget $cmd
            } else {
                & ssh $__fpTarget $cmd
            }
        }
        'docker' {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            if ($__fpEntry.user) {
                & docker exec --user $__fpEntry.user $__fpEntry.container sh -lc $cmd
            } else {
                & docker exec $__fpEntry.container sh -lc $cmd
            }
        }
        'kubectl' {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpArgs = @()
            if ($__fpEntry.context) { $__fpArgs += @('--context', $__fpEntry.context) }
            if ($__fpEntry.namespace) { $__fpArgs += @('-n', $__fpEntry.namespace) }
            $__fpArgs += 'exec'
            if ($__fpEntry.container) { $__fpArgs += @('-c', $__fpEntry.container) }
            $__fpArgs += @($__fpEntry.pod, '--', 'sh', '-lc', $cmd)
            & kubectl @__fpArgs
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
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }
            $__fpDestination = $dest
            $__fpTarget = if ($__fpEntry.user) { "$($__fpEntry.user)@$($__fpAddress):$__fpDestination" } else { "$($__fpAddress):$__fpDestination" }
            if ($__fpEntry.port) {
                & scp -P $__fpEntry.port $src $__fpTarget
            } else {
                & scp $src $__fpTarget
            }
        }
        'docker' {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            & docker cp $src "$($__fpEntry.container):$dest"
        }
        'kubectl' {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpArgs = @()
            if ($__fpEntry.context) { $__fpArgs += @('--context', $__fpEntry.context) }
            if ($__fpEntry.namespace) { $__fpArgs += @('-n', $__fpEntry.namespace) }
            & kubectl cp @__fpArgs $src "$($__fpEntry.pod):$dest"
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
    Remove-Item -Force $tmp -ErrorAction SilentlyContinue
}

function rsync_host {
    param([string]$host, [string]$flags, [string]$src, [string]$dest)
    $transport = $(host_transport $host)
    switch -Exact ($transport) {
        'ssh' {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }
            $__fpDestination = $dest
            $__fpTarget = if ($__fpEntry.user) { "$($__fpEntry.user)@$($__fpAddress):$__fpDestination" } else { "$($__fpAddress):$__fpDestination" }
            & rsync $flags -- $src $__fpTarget
        }
        default {
            throw "rsync is only supported for ssh in shell target, got: $transport"
        }
    }
}

function shell_run {
    param([string]$host, [string]$command, [string]$only_if, [string]$unless, [string]$creates, [string]$removes)
    $script:fpLastChanged = $false
    if ($only_if -ne '') {
        if ($only_if) {
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
        if (test ! -e $creates) {
            shell_run_after_creates $host $command $removes
        }
    } else {
        shell_run_after_creates $host $command $removes
    }
}

function shell_run_after_creates {
    param([string]$host, [string]$command, [string]$removes)
    if ($removes -ne '') {
        if (test -e $removes) {
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
        if ($only_if) {
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
        if (test ! -e $creates) {
            shell_copy_after_creates $host $src $dest $removes
        }
    } else {
        shell_copy_after_creates $host $src $dest $removes
    }
}

function shell_copy_after_creates {
    param([string]$host, [string]$src, [string]$dest, [string]$removes)
    if ($removes -ne '') {
        if (test -e $removes) {
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
        if ($only_if) {
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
        if (test ! -e $creates) {
            shell_template_after_creates $host $src $dest $vars $removes
        }
    } else {
        shell_template_after_creates $host $src $dest $vars $removes
    }
}

function shell_template_after_creates {
    param([string]$host, [string]$src, [string]$dest, [string]$vars, [string]$removes)
    if ($removes -ne '') {
        if (test -e $removes) {
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
        if ($only_if) {
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
        if (test ! -e $creates) {
            shell_rsync_after_creates $host $flags $src $dest $removes
        }
    } else {
        shell_rsync_after_creates $host $flags $src $dest $removes
    }
}

function shell_rsync_after_creates {
    param([string]$host, [string]$flags, [string]$src, [string]$dest, [string]$removes)
    if ($removes -ne '') {
        if (test -e $removes) {
            rsync_host $host $flags $src $dest
            $script:fpLastChanged = $true
        }
    } else {
        rsync_host $host $flags $src $dest
        $script:fpLastChanged = $true
    }
}

shell_run 'localhost' "echo local hello" '' '' '' ''
shell_run 'ssh-web' "echo ssh hello" '' '' '' ''
shell_run 'docker-app' "echo docker hello" '' '' '' ''
shell_run 'k8s-api' "echo kubectl hello" '' '' '' ''
shell_run 'windows-admin' "Write-Host winrm hello" '' '' '' ''
