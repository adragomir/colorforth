REM Start qemu on windows.
@ECHO OFF
set QEMU="\Program Files\QEMU"
path %path%;%QEMU%

REM SDL_VIDEODRIVER=directx is faster than windib. But keyboard cannot work well.
SET SDL_VIDEODRIVER=windib

REM SDL_AUDIODRIVER=waveout or dsound can be used. Only if QEMU_AUDIO_DRV=sdl.
SET SDL_AUDIODRIVER=dsound

REM QEMU_AUDIO_DRV=dsound or fmod or sdl or none can be used. See qemu -audio-help.
SET QEMU_AUDIO_DRV=dsound

REM QEMU_AUDIO_LOG_TO_MONITOR=1 displays log messages in QEMU monitor.
SET QEMU_AUDIO_LOG_TO_MONITOR=0

qemu.exe -L %QEMU% -m 32 -fda 1024x768.ima -soundhw all -localtime -full-screen
