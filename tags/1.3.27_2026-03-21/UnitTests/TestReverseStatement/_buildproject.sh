#!/bin/bash
# Builds the Reverse Statement Unit Tests under WSL by calling the batch file.

cd "$(dirname "$0")"
cmd.exe /c "_BuildProject.cmd --nowait"
