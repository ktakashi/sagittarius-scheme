#!/bin/bash

echo 'brew "bdw-gc"' >> Brewfile
echo 'brew "libffi"' >> Brewfile
echo 'brew "openssl"' >> Brewfile
echo 'brew "curl"' >> Brewfile
brew bundle install --file Brewfile
