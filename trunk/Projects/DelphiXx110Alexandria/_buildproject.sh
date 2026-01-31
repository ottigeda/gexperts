#!/bin/bash
# Builds the GExperts DLL for RAD Studio 11 Alexandria under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
