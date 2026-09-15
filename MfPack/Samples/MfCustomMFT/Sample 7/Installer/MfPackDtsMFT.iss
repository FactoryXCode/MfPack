#define MyAppName "MfPack FFmpeg DTS Decoder MFT"
#define MyAppVersion "1.0.0"
#define MyAppPublisher "FactoryX"

[Setup]
AppId={{B2638064-18C7-4D05-BFB5-CFC76D482D08}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
DefaultDirName={autopf}\MfPack\DTS Decoder MFT
DisableProgramGroupPage=yes
PrivilegesRequired=admin
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
OutputDir=Output
OutputBaseFilename=MfPackDtsDecoderMFT-{#MyAppVersion}
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
SetupLogging=yes
UninstallDisplayName={#MyAppName}
LicenseFile=..\ThirdParty\COPYING.LGPLv2.1

[Files]
Source: "..\Runtime\Win32\MfPackDtsMFT.dll"; DestDir: "{app}\Win32"; Flags: ignoreversion
Source: "..\Runtime\Win32\MfPackDtsBridge.dll"; DestDir: "{app}\Win32"; Flags: ignoreversion
Source: "..\Runtime\Win32\avcodec-63.dll"; DestDir: "{app}\Win32"; Flags: ignoreversion
Source: "..\Runtime\Win32\avutil-61.dll"; DestDir: "{app}\Win32"; Flags: ignoreversion
Source: "..\Runtime\Win32\swresample-7.dll"; DestDir: "{app}\Win32"; Flags: ignoreversion
Source: "..\Runtime\Win64\MfPackDtsMFT.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "..\Runtime\Win64\MfPackDtsBridge.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "..\Runtime\Win64\avcodec-63.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "..\Runtime\Win64\avutil-61.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "..\Runtime\Win64\swresample-7.dll"; DestDir: "{app}\Win64"; Flags: ignoreversion
Source: "..\ThirdParty\NOTICE.md"; DestDir: "{app}\ThirdParty"; Flags: ignoreversion
Source: "..\ThirdParty\configure-command.txt"; DestDir: "{app}\ThirdParty"; Flags: ignoreversion
Source: "..\ThirdParty\changes.diff"; DestDir: "{app}\ThirdParty"; Flags: ignoreversion
Source: "..\ThirdParty\ffmpeg-9.0.1.tar.xz"; DestDir: "{app}\ThirdParty"; Flags: ignoreversion
Source: "..\ThirdParty\ffmpeg-9.0.1.tar.xz.asc"; DestDir: "{app}\ThirdParty"; Flags: ignoreversion
Source: "..\ThirdParty\COPYING.LGPLv2.1"; DestDir: "{app}\ThirdParty"; Flags: ignoreversion

[Run]
Filename: "{sys}\regsvr32.exe"; Parameters: "/s ""{app}\Win64\MfPackDtsMFT.dll"""; StatusMsg: "Registering the 64-bit DTS decoder..."; Flags: runhidden waituntilterminated
Filename: "{syswow64}\regsvr32.exe"; Parameters: "/s ""{app}\Win32\MfPackDtsMFT.dll"""; StatusMsg: "Registering the 32-bit DTS decoder..."; Flags: runhidden waituntilterminated

[UninstallRun]
Filename: "{syswow64}\regsvr32.exe"; Parameters: "/s /u ""{app}\Win32\MfPackDtsMFT.dll"""; Flags: runhidden waituntilterminated; RunOnceId: "UnregisterMfPackDtsMFT32"
Filename: "{sys}\regsvr32.exe"; Parameters: "/s /u ""{app}\Win64\MfPackDtsMFT.dll"""; Flags: runhidden waituntilterminated; RunOnceId: "UnregisterMfPackDtsMFT64"
