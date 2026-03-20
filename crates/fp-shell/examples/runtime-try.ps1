Set-StrictMode -Version Latest
Set-PSDebug -Trace 1
$ErrorActionPreference = 'Stop'
$script:fpLastChanged = $false

$script:FpHosts = @{}
function Invoke-FpRuntimeValidation {
    if (-not (Get-Command -Name 'Invoke-Expression' -ErrorAction SilentlyContinue)) { throw 'missing required command: Invoke-Expression' }
    if (-not (Get-Command -Name 'pwsh' -ErrorAction SilentlyContinue)) { throw 'missing required command: pwsh' }
}

Invoke-FpRuntimeValidation

function run_local_host {
    param([string]$cmd)
    Write-Output $(Invoke-Expression $cmd)
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

function shell_local {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(command_with_options $command $cwd $sudo)
    shell_run_local $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
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
        shell_local 'echo deploy body' 'localhost' '' '' '' '' '' ''
        $__fpTrySuccess3 = $true
    } catch {
        if (-not $__fpTryHandled4) {
            throw
        }
    } finally {
        shell_local 'echo cleanup' 'localhost' '' '' '' '' '' ''
    }
    $__fpTrySuccess1 = $true
} catch {
    if (-not $__fpTryHandled2) {
        $err = $_
        shell_local "echo deploy failed=$err" 'localhost' '' '' '' '' '' ''
        $__fpTryHandled2 = $true
    }
    if (-not $__fpTryHandled2) {
        throw
    }
} finally {
    shell_local 'echo deploy finally' 'localhost' '' '' '' '' '' ''
}
if ($__fpTrySuccess1) {
    shell_local 'echo deploy success' 'localhost' '' '' '' '' '' ''
}
