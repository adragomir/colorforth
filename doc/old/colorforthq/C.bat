ml /coff /c  /nologo /Fr /Zi /Fl color.asm
if errorlevel 1 goto end
rc /v resource.rc
if errorlevel 1 goto end
cvtres /machine:ix86 resource.res 

link /SUBSYSTEM:WINDOWS /debug /entry:_start /WARN:5 /section:.text,ERW /section:_STACK,ERW color.obj resource.obj
if errorlevel 1 goto end
echo #### SUCCESS ####
:end