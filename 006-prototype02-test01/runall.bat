
@echo off
setlocal

:: map working network drive
net use Z: "\\esdfiles\internal\Sections\Data Science Hub\DSCoE\300-projects\2019-2020\001-TrIPW"

:: ########################################################
:: get folder containing this script
set thisFolder=%~dp0
set thisFolder=%thisFolder:~0,-1%

:: get task name
for %%f in (%thisFolder%) do set task=%%~nf

:: ########################################################
:: defining folder names
set codeDIR=%thisFolder%\code
set dataDIR=Z:\500-data\input
set outROOT=Z:\600-temp\%username%\%task%\output

:: make output root directory if not already exists
if not exist %outROOT%\code mkdir %outROOT%\code

:: copy this batch script to output root
copy %0 %outROOT%\code
:: copy code folder to output root
xcopy /E /I /Y %codeDIR% %outROOT%\code

:: ########################################################
:: R command
Rscript %codeDIR%\runall.R %codeDIR% %outROOT% 1> %outROOT%\stdout.R.runall 2> %outROOT%\stderr.R.runall

:: unmap network drives
net use Z: /delete /yes
