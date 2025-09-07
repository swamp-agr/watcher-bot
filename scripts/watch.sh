#!/bin/bash

set -e

# get bot PID from ps output.
WATCHER_BOT_PID=$(ps aux | grep 'watcher-bot bot' | grep -v grep | awk '{print $2}')

# test if process is running.
if ! [ -n "$WATCHER_BOT_PID" ];
then
    cd ~/git/watcher-bot &&  ./scripts/start.sh &
fi
