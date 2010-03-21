; pcdinst.nsi
;
; Installer for cfEmu.

;--------------------------------

!ifdef HAVE_UPX
!packhdr tmp.dat "upx\upx -9 tmp.dat"
!endif

!ifdef NOCOMPRESS
SetCompress off
!endif

;--------------------------------

Name "cfEmu"
Caption "colorForth in a Bochs"
Icon "${NSISDIR}\Contrib\Graphics\Icons\nsis1-install.ico"
OutFile "cf_emu.exe"

SetDateSave on
SetDatablockOptimize on
CRCCheck on
SilentInstall normal
BGGradient 000000 800000 FFFFFF
InstallColors FF8080 000030
XPStyle on

InstallDir "$APPDATA\net\sourceforge\colorforth"
InstallDirRegKey HKLM "Software\net\sourceforge\colorforth" ""

CheckBitmap "${NSISDIR}\Contrib\Graphics\Checks\classic-cross.bmp"

LicenseText "GNU Public License"
LicenseData "LICENSE.txt"

;--------------------------------

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

!ifndef NOINSTTYPES ; only if not defined
  InstType "Full"
  ;InstType /NOCUSTOM
  ;InstType /COMPONENTSONLYONCUSTOM
!endif

AutoCloseWindow false
ShowInstDetails show

;--------------------------------

Section "" ; empty string makes it hidden, so would starting with -

  ; write registry string
  WriteRegStr HKLM SOFTWARE\net\sourceforge\colorforth "Install_Dir" "$INSTDIR"

  ; write uninstall strings
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cfEmu" "DisplayName" "cfEmu (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cfEmu" "UninstallString" '"$INSTDIR\unwise.exe"'

  SetOutPath $INSTDIR
  CreateDirectory "$INSTDIR"
  WriteUninstaller "unwise.exe"

SectionEnd

Section "Copy Files"

  SectionIn 1

  SetOutPath $INSTDIR
  File /a "bochs.exe" "bochsrc.bxrc" "800x600.bat" "1024x768.bat" "800x600.ima" "1024x768.ima" "LICENSE.rtf" "README.rtf" "cf.vmx" "cfqemu.bat"

SectionEnd

Section "Create Shortcuts"
  
  SectionIn 1
  CreateDirectory "$SMPROGRAMS\cfEmu"
  SetOutPath $INSTDIR ; for working directory
  CreateShortCut "$DESKTOP\cfBochs.lnk" "$INSTDIR\800x600.bat" "" "$INSTDIR\bochs.exe" 0
  CreateShortCut "$DESKTOP\cfBochs fullscreen.lnk" "$INSTDIR\1024x768.bat" "" "$INSTDIR\bochs.exe" 0
  CreateShortCut "$DESKTOP\colorforth on VMware.lnk" "$INSTDIR\cf.vmx" "" "\Program Files\VMware\VMware Player\vmplayer.exe" 0
  CreateShortCut "$DESKTOP\colorForth on QEMU.lnk" "$INSTDIR\cfqemu.bat" "" "\Program Files\QEMU\qemu.exe" 0
  CreateShortCut "$SMPROGRAMS\cfEmu\Uninstall cfEmu.lnk" "$INSTDIR\unwise.exe" ; use defaults for parameters, icon, etc.
  CreateShortCut "$SMPROGRAMS\cfEmu\License.lnk" "$INSTDIR\LICENSE.rtf" "" "" 
  CreateShortCut "$SMPROGRAMS\cfEmu\Readme.lnk" "$INSTDIR\README.rtf" "" "" 

SectionEnd

;--------------------------------

; Uninstaller

UninstallText "This will uninstall cfEmu. Hit next to continue."
UninstallIcon "${NSISDIR}\Contrib\Graphics\Icons\nsis1-uninstall.ico"

Section "Uninstall"

  DeleteRegKey HKLM "SOFTWARE\net\sourceforge\colorforth"
  Delete "$INSTDIR\unwise.exe"
  Delete "$INSTDIR\bochs.exe"
  Delete "$INSTDIR\800x600.bat"
  Delete "$INSTDIR\1024x768.bat"
  Delete "$INSTDIR\bochsrc.bxrc"
  Delete "$INSTDIR\800x600.ima"
  Delete "$INSTDIR\1024x768.ima"
  Delete "$INSTDIR\README.rtf"
  Delete "$INSTDIR\LICENSE.rtf"
  Delete "$INSTDIR\cf.vmx"
  Delete "$INSTDIR\cfqemu.bat"
  Delete "$INSTDIR\cfBochs.log"
  Delete "$INSTDIR\vmware.log"
  Delete "$INSTDIR\vmware-0.log"
  Delete "$INSTDIR\vmware-1.log"
  Delete "$INSTDIR\cf.vmem"
  Delete "$INSTDIR\cf.vmss"
  Delete "$INSTDIR\cf.vmsd"
  Delete "$INSTDIR\cf.vmx.lck"
  Delete "$INSTDIR\test.nvram"
  Delete "$DESKTOP\cfBochs.lnk"
  Delete "$DESKTOP\cfBochs fullscreen.lnk"
  Delete "$DESKTOP\colorForth on VMware.lnk"
  Delete "$DESKTOP\colorForth on QEMU.lnk"
  Delete "$SMPROGRAMS\cfEmu\Uninstall cfEmu.lnk"
  Delete "$SMPROGRAMS\cfEmu\cfBochs.lnk"
  Delete "$SMPROGRAMS\cfEmu\License.lnk"
  Delete "$SMPROGRAMS\cfEmu\Readme.lnk"
  RMDir "$SMPROGRAMS\cfEmu"
  RMDir "$INSTDIR"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\cfEmu"

SectionEnd
