Set-Alias nano C:\msys64\usr\bin\nano.exe
Set-Alias open Start-Process

function tree { tree.com /f @args }
function which { (Get-Command @args).Source }

Remove-Alias -Force sl
function sl { sl.exe -ad5w @args }

Set-Alias c Clear-Host
Set-Alias ipy "ipython.exe"
# Set-Alias haste "C:\src\WinHaste\WinHaste.exe"

Set-Alias -Force cat "bat.exe"
Set-Alias grep rg.exe
Set-Alias less "C:\msys64\usr\bin\less.exe"

# VirtualEnvWrapper
$PSConfigDir = Split-Path $PROFILE.CurrentUserAllHosts
$VenvWrapperScript = "$PSConfigDir\Modules\VirtualEnvWrapper.psm1"

if (![System.IO.File]::Exists($VenvWrapperScript)) {
    $VenvWrapperScriptDownload = "https://raw.githubusercontent.com/regisf/virtualenvwrapper-powershell/master/VirtualEnvWrapper.psm1"
    Invoke-WebRequest -Uri $VenvWrapperScriptDownload -OutFile $VenvWrapperScript
}

Import-Module $VenvWrapperScript

# VcXSrv
if (Get-Process VcxSrv -ErrorAction SilentlyContinue) {
    $ENV:DISPLAY = "localhost:0.0"
}

function startx {
    if (!$(Get-Process VcxSrv -ErrorAction SilentlyContinue)) {
        xlaunch.exe
    }
    $ENV:DISPLAY = "localhost:0.0"
}

function stopx {
    $srv = Get-Process VcxSrv -ErrorAction SilentlyContinue
    
    if ($srv) {
        # Try gracefully first
        $srv | Stop-Process

        # kill after five seconds
        Sleep 5
        if (!$srv.HasExited) {
            $srv | Stop-Process -Force
        }
    }

    Remove-Variable srv
}
