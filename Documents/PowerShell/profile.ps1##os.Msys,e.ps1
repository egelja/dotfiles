Set-Alias nano C:\msys64\usr\bin\nano.exe

function tree { tree.com /f @args }
function la { Get-ChildItem -Force @args }

Set-Alias c Clear-Host
Set-Alias ipy "ipython.exe"
Set-Alias make "mingw32-make.exe"
# Set-Alias haste "C:\src\WinHaste\WinHaste.exe"
Set-Alias time Measure-Command

$PowerShellProfile = $PROFILE.CurrentUserAllHosts
$PowerShellPath = Split-Path $PowerShellProfile
# Import-Module $PowerShellPath\Modules\VirtualEnvWrapper.psm1
