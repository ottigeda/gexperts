#!/bin/bash
# Builds DfmFilter standalone tool under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
