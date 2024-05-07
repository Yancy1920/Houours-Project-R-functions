ECHO Converting fsa files to csv files...
@ECHO OFF
REM This batch file takes fsa files and extracts the data within as csv files
REM Convert with HidFileExtract
FOR %%f IN (*.hid) DO HidFileExtract --in %%f --out "%%~nf.xml"
REM Convert all the xml files to csv files and delete the xml
REM You now have a dataset to run with
ECHO Conversion complete