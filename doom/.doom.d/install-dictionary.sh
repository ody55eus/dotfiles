#!/usr/bin/env zsh
set -euo pipefail

# Install German dictionary
npm install dictionary-de
mkdir -p ~/Library/Spelling
ln -sf ~/node_modules/dictionary-de/index.aff ~/Library/Spelling/de.aff
ln -sf ~/node_modules/dictionary-de/index.dic ~/Library/Spelling/de.dic

# Install British English
curl -L https://sourceforge.net/projects/wordlist/files/speller/2020.12.07/hunspell-en_GB-large-2020.12.07.zip/download > hunspell-en_GB-large-2020.12.07.zip
unzip hunspell-en_GB-large-2020.12.07.zip
cp en_GB-large.aff ~/Library/Spelling/
cp en_GB-large.dic ~/Library/Spelling/

# Install American English
curl -L https://sourceforge.net/projects/wordlist/files/speller/2020.12.07/hunspell-en_US-large-2020.12.07.zip/download > hunspell-en_US-large-2020.12.07.zip
unzip hunspell-en_US-large-2020.12.07.zip
cp en_US-large.aff ~/Library/Spelling/
cp en_US-large.dic ~/Library/Spelling/

# Cleanup
rm -f hunspell-en_*.zip en_*.aff en_*.dic README_en_*.txt
