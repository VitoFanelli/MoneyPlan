@echo off
cd /d "%~dp0"

set RSCRIPT=

where Rscript >nul 2>&1
if %errorlevel% equ 0 (
    set RSCRIPT=Rscript
) else (
    for /d %%i in ("C:\Program Files\R\R-*") do set RSCRIPT=%%i\bin\Rscript.exe
)

if "%RSCRIPT%"=="" (
    echo Rscript non trovato. Installa R da https://cran.r-project.org/
    pause
    exit /b 1
)

"%RSCRIPT%" run.R
