#!/bin/bash

set -e
set -x

# read PID from file
PID=$(cat ./watcher-bot.pid)

# stop process
kill $PID

# drop PID from file
echo "" > ./watcher-bot.pid
