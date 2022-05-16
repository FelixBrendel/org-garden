@echo off
pushd %~dp0

set PATH=%~dp0emacs\bin;%PATH%
set PATH=%~dp0emacs\home\.emacs.d\bin;%PATH%
set HOME=%~dp0emacs\home

call doom env

cmd

emacs\bin\runemacs.exe