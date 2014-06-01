@echo off
powershell -noprofile -executionpolicy bypass -command "& { Import-Module .\AWSDevTools; Install-AWSDevToolsModule }"
