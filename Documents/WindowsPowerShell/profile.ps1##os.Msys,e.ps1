Set-Alias nano C:\msys64\usr\bin\nano.exe

function Tree-AllFiles { tree.com /f $args }
Set-Alias tree Tree-AllFiles -Force

function Get-AllChildItems { Get-ChildItem -Force $args }
Set-Alias la Get-AllChildItems -Force

Set-Alias c Clear-Host
Set-Alias ipy "ipython.exe"
Set-Alias make "mingw32-make.exe"
# Set-Alias haste "C:\src\WinHaste\WinHaste.exe"

$PowerShellProfile = $PROFILE.CurrentUserAllHosts
$PowerShellPath = Split-Path $PowerShellProfile
# Import-Module $PowerShellPath\Modules\VirtualEnvWrapper.psm1
