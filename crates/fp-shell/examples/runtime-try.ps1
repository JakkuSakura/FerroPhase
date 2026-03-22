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

function __fp_std_ops_server_shell_local_ {
    param([string]$command, [string]$hosts, [string]$only_if, [string]$unless, [string]$creates, [string]$removes, [string]$sudo, [string]$cwd)
    $command = $(__fp_std_shell_backend_command_with_options_ $command $cwd $sudo)
    __fp_std_shell_backend_shell_run_local_ $hosts $command $only_if $unless $creates $removes
    Write-Output $(if ($script:fpLastChanged) { 'true' } else { 'false' })
}

function __fp_std_shell_backend_run_local_host_ {
    param([string]$cmd)
    Write-Output $(Invoke-Expression $cmd)
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

function __fp_std_shell_process_process_ok_ {
    param([string]$command)
    Write-Output $((& { pwsh -Command $command; $LASTEXITCODE -eq 0 }))
}

$__fpTrySuccess1 = $false
$__fpTryHandled2 = $false
try {
    $__fpTrySuccess3 = $false
    $__fpTryHandled4 = $false
    try {
        __fp_std_ops_server_shell_local_ 'echo deploy body' 'localhost' '' '' '' '' '' ''
        $__fpTrySuccess3 = $true
    } catch {
        if (-not $__fpTryHandled4) {
            throw
        }
    } finally {
        __fp_std_ops_server_shell_local_ 'echo cleanup' 'localhost' '' '' '' '' '' ''
    }
    $__fpTrySuccess1 = $true
} catch {
    if (-not $__fpTryHandled2) {
        $err = $_
        __fp_std_ops_server_shell_local_ "echo deploy failed=$err" 'localhost' '' '' '' '' '' ''
        $__fpTryHandled2 = $true
    }
    if (-not $__fpTryHandled2) {
        throw
    }
} finally {
    __fp_std_ops_server_shell_local_ 'echo deploy finally' 'localhost' '' '' '' '' '' ''
}
if ($__fpTrySuccess1) {
    __fp_std_ops_server_shell_local_ 'echo deploy success' 'localhost' '' '' '' '' '' ''
}
