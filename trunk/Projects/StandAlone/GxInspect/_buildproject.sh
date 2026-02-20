#!/bin/bash
# Builds GxInspect standalone tool under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
