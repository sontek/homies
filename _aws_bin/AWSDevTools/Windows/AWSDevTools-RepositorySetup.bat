@echo off
powershell -noprofile -executionpolicy bypass -Command "& { Import-Module AWSDevTools; Initialize-AWSElasticBeanstalkRepository }"