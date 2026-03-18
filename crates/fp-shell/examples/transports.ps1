Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
$script:FpHosts['windows-admin'] = @{ transport = 'winrm'; address = '10.0.0.21'; user = 'Administrator'; password = 'change-me'; port = 5985; scheme = 'http' }
$script:FpHosts['localhost'] = @{ transport = 'local' }
$script:FpHosts['docker-app'] = @{ transport = 'docker'; container = 'app'; user = 'root' }
$script:FpHosts['k8s-api'] = @{ transport = 'kubectl'; pod = 'api-7f9f6'; namespace = 'prod'; container = 'api'; context = 'prod-cluster' }
$script:FpHosts['ssh-web'] = @{ transport = 'ssh'; address = '10.0.0.11'; user = 'deploy'; port = 22 }


function New-FpWinRmSession {
    param($Entry, [string]$Host)
    $sessionArgs = @{
        ComputerName = $Entry.address
    }
    if ($Entry.port) { $sessionArgs.Port = $Entry.port }
    $scheme = if ($Entry.scheme) { $Entry.scheme.ToLowerInvariant() } else { 'http' }
    switch ($scheme) {
        'http' {}
        'https' { $sessionArgs.UseSSL = $true }
        default { throw "unsupported winrm scheme for $Host: $($Entry.scheme)" }
    }
    if (-not $Entry.password) { throw "winrm password is required for non-interactive PowerShell target: $Host" }
    $securePassword = ConvertTo-SecureString $Entry.password -AsPlainText -Force
    $credential = New-Object System.Management.Automation.PSCredential($Entry.user, $securePassword)
    New-PSSession -Credential $credential @sessionArgs
}

function run_host {
    param([string]$host, [string]$cmd)
    $transport = $script:FpHosts[$host].transport
    if ($transport -eq 'local') {
        $cmd
    } else {
        if ($transport -eq 'ssh') {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }
            $__fpTarget = if ($__fpEntry.user) { "$($__fpEntry.user)@$($__fpAddress)" } else { $__fpAddress }
            if ($__fpEntry.port) {
                & ssh -p $__fpEntry.port $__fpTarget $cmd
            } else {
                & ssh $__fpTarget $cmd
            }
        } else {
            if ($transport -eq 'docker') {
                $__fpHost = $host
                $__fpEntry = $script:FpHosts[$__fpHost]
                if ($__fpEntry.user) {
                    & docker exec --user $__fpEntry.user $__fpEntry.container sh -lc $cmd
                } else {
                    & docker exec $__fpEntry.container sh -lc $cmd
                }
            } else {
                if ($transport -eq 'kubectl') {
                    $__fpHost = $host
                    $__fpEntry = $script:FpHosts[$__fpHost]
                    $__fpArgs = @()
                    if ($__fpEntry.context) { $__fpArgs += @('--context', $__fpEntry.context) }
                    if ($__fpEntry.namespace) { $__fpArgs += @('-n', $__fpEntry.namespace) }
                    $__fpArgs += 'exec'
                    if ($__fpEntry.container) { $__fpArgs += @('-c', $__fpEntry.container) }
                    $__fpArgs += @($__fpEntry.pod, '--', 'sh', '-lc', $cmd)
                    & kubectl @__fpArgs
                } else {
                    if ($transport -eq 'winrm') {
                        $__fpHost = $host
                        $__fpEntry = $script:FpHosts[$__fpHost]
                        $__fpSession = New-FpWinRmSession -Entry $__fpEntry -Host $__fpHost
                        try {
                            Invoke-Command -Session $__fpSession -ScriptBlock ([scriptblock]::Create($cmd))
                        } finally {
                            Remove-PSSession -Session $__fpSession
                        }
                    } else {
                        throw "unsupported transport: $transport"
                    }
                }
            }
        }
    }
}

function copy_host {
    param([string]$host, [string]$src, [string]$dest)
    $transport = $script:FpHosts[$host].transport
    if ($transport -eq 'local') {
        Copy-Item -Force $src $dest
    } else {
        if ($transport -eq 'ssh') {
            $__fpHost = $host
            $__fpEntry = $script:FpHosts[$__fpHost]
            $__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }
            $__fpTarget = if ($__fpEntry.user) { "$($__fpEntry.user)@$($__fpAddress):$__fpDestination" } else { "$($__fpAddress):$__fpDestination" }
            $__fpDestination = $dest
            if ($__fpEntry.port) {
                & scp -P $__fpEntry.port $src $__fpTarget
            } else {
                & scp $src $__fpTarget
            }
        } else {
            if ($transport -eq 'docker') {
                $__fpHost = $host
                $__fpEntry = $script:FpHosts[$__fpHost]
                & docker cp $src "$($__fpEntry.container):$dest"
            } else {
                if ($transport -eq 'kubectl') {
                    $__fpHost = $host
                    $__fpEntry = $script:FpHosts[$__fpHost]
                    $__fpArgs = @()
                    if ($__fpEntry.context) { $__fpArgs += @('--context', $__fpEntry.context) }
                    if ($__fpEntry.namespace) { $__fpArgs += @('-n', $__fpEntry.namespace) }
                    & kubectl cp @__fpArgs $src "$($__fpEntry.pod):$dest"
                } else {
                    if ($transport -eq 'winrm') {
                        $__fpHost = $host
                        $__fpEntry = $script:FpHosts[$__fpHost]
                        $__fpSession = New-FpWinRmSession -Entry $__fpEntry -Host $__fpHost
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
                    } else {
                        throw "unsupported transport for copy: $transport"
                    }
                }
            }
        }
    }
}

function template_host {
    param([string]$host, [string]$src, [string]$dest, [string]$vars)
    $tmp = [System.IO.Path]::GetTempFileName()
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
    $transport = $script:FpHosts[$host].transport
    if ($transport -eq 'ssh') {
        $__fpHost = $host
        $__fpEntry = $script:FpHosts[$__fpHost]
        $__fpAddress = if ($__fpEntry.address) { $__fpEntry.address } else { $__fpHost }
        $__fpDestination = $dest
        $__fpTarget = if ($__fpEntry.user) { "$($__fpEntry.user)@$($__fpAddress):$__fpDestination" } else { "$($__fpAddress):$__fpDestination" }
        & rsync $flags -- $src $__fpTarget
    } else {
        throw "rsync is only supported for ssh in powershell target, got: $transport"
    }
}

$script:fpChanged = $false
echo local hello
$script:fpChanged = $true
$script:fpLastChanged = $script:fpChanged
$script:fpChanged = $false
run_host 'ssh-web' "echo ssh hello"
$script:fpChanged = $true
$script:fpLastChanged = $script:fpChanged
$script:fpChanged = $false
run_host 'docker-app' "echo docker hello"
$script:fpChanged = $true
$script:fpLastChanged = $script:fpChanged
$script:fpChanged = $false
run_host 'k8s-api' "echo kubectl hello"
$script:fpChanged = $true
$script:fpLastChanged = $script:fpChanged
$script:fpChanged = $false
run_host 'windows-admin' "Write-Host winrm hello"
$script:fpChanged = $true
$script:fpLastChanged = $script:fpChanged
