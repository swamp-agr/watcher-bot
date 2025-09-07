#!/bin/bash

set -x

# output errors and logs in the same stream
exec 2>&1

# prepare log directory
mkdir -p ./log

source ~/.bashrc

# normalise config
~/.cabal/bin/dhall --output ./config/settings.dhall <<< ./config/default.dhall

~/.cabal/bin/watcher-bot migration

~/.cabal/bin/watcher-bot bot +RTS -A64m -AL256m -qn2 -RTS >> log/watcher-bot.log 2>&1 &

# write PID to file
echo $! > ./watcher-bot.pid
