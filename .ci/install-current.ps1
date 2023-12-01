$net = new-object net.webclient

# seems we can't rely on where this is executed
md -Force \tmp
# install the latest Sagittarius
$url = 'https://bitbucket.org/ktakashi/sagittarius-scheme/downloads'
$net.DownloadFile("$($url)/latest-version.txt", '\tmp\latest-version.txt')
$version=(Get-Content \tmp\latest-version.txt)
$net.DownloadFile("$($url)/setup_sagittarius_$($version)_x64.exe", '\tmp\setup_sagittarius.exe')
\tmp\setup_sagittarius.exe /VERYSILENT /SUPPRESSMSGBOXES /ALLUSERS /LOG="\tmp\setup.log"
