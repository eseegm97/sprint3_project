@echo off
echo Simple Movie Data Analysis
echo ==========================

echo.
echo Step 1: Installing required R packages...
echo.
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" install_packages.r

echo.
echo Step 2: Running movie data analysis...
echo.
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" simple_analysis.r

echo.
echo Analysis complete!
pause