#!/bin/bash
# Builds the GExperts DLL for Delphi XE6 under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
