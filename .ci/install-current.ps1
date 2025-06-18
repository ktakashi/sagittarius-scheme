$net = new-object net.webclient

# seems we can't rely on where this is executed
md -Force \tmp

# install the latest Sagittarius
$url = 'https://github.com/ktakashi/sagittarius-scheme/releases'
$response = Invoke-WebRequest -Uri "$url/latest" -MaximumRedirection 0 -ErrorAction Ignore
$response.Headers.Location -match "$url/tag/v(?<version>.+)"
$version = $matches['version']
$download_url = "$url/download/v$version/setup_sagittarius_$($version)_x64.exe"
$net.DownloadFile($download_url, '\tmp\setup_sagittarius.exe')

Start-Process -FilePath '\tmp\setup_sagittarius.exe' -ArgumentList '/VERYSILENT', '/SUPPRESSMSGBOXES', '/ALLUSERS', '/LOG="\tmp\setup.log"' -Wait
