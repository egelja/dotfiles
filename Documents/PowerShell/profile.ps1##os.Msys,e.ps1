Set-Alias nano C:\msys64\usr\bin\nano.exe

function tree { tree.com /f @args }
function la { Get-ChildItem -Force @args }
function which { (Get-Command @args).Source }

Remove-Alias -Force sl
function sl { sl.exe -ad5w @args }

function startx {
    xlaunch.exe
    $ENV:DISPLAY = "localhost:0.0"
}

Set-Alias c Clear-Host
Set-Alias ipy "ipython.exe"
Set-Alias make "mingw32-make.exe"
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
