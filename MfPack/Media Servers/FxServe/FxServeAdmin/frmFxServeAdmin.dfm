object frmFxServeAdmin: TfrmFxServeAdmin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FxServe Admin 1.1'
  ClientHeight = 571
  ClientWidth = 714
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object FLogMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 282
    Width = 708
    Height = 263
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 718
    ExplicitHeight = 267
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 279
    Align = alTop
    Color = clGray
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    ExplicitWidth = 724
    object Bevel3: TBevel
      Left = 7
      Top = 16
      Width = 482
      Height = 211
    end
    object lblServer: TLabel
      Left = 3
      Top = 36
      Width = 106
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Server:'
    end
    object lblService: TLabel
      Left = 3
      Top = 70
      Width = 106
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Service:'
    end
    object lblShare: TLabel
      Left = 3
      Top = 102
      Width = 106
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FxServe share:'
    end
    object lblServerRoot: TLabel
      Left = 3
      Top = 134
      Width = 106
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Server local path:'
    end
    object lblPublicHost: TLabel
      Left = 3
      Top = 166
      Width = 106
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Public hostname:'
    end
    object Bevel1: TBevel
      Left = 1
      Top = 233
      Width = 712
      Height = 45
      Align = alBottom
      ExplicitLeft = 10
      ExplicitTop = 241
      ExplicitWidth = 777
    end
    object Bevel2: TBevel
      Left = 489
      Top = 9
      Width = 213
      Height = 211
    end
    object Label1: TLabel
      Left = 19
      Top = 9
      Width = 58
      Height = 15
      Alignment = taRightJustify
      Caption = '  Settings  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object Label2: TLabel
      Left = 510
      Top = 9
      Width = 40
      Height = 15
      Alignment = taRightJustify
      Caption = '  Tools  '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
    end
    object FServerEdit: TEdit
      Left = 115
      Top = 33
      Width = 366
      Height = 25
      Hint = 'The server'#39's network name. Like: \\MYSERVER1\ '
      AutoSize = False
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TextHint = 'MY_SERVER1'
      StyleElements = [seClient, seBorder]
    end
    object FServiceEdit: TEdit
      Left = 115
      Top = 67
      Width = 366
      Height = 23
      Hint = 'FxServe uses the fixed Windows service name FxServe.'
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TextHint = 'FxServe'
    end
    object FShareEdit: TEdit
      Left = 115
      Top = 99
      Width = 368
      Height = 23
      Hint = 
        'The network path where FxServe is deployed. Like: \\MYSERVER1\Fx' +
        'Serve'
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      TextHint = '\\MY_SERVER1\FxServe'
    end
    object FServerRootEdit: TEdit
      Left = 113
      Top = 131
      Width = 368
      Height = 23
      Hint = 
        'The path on the server where FxServe is deployed. Like: C:\FxSer' +
        've  '
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      TextHint = 'C:\FxServe'
    end
    object FPublicHostEdit: TEdit
      Left = 115
      Top = 163
      Width = 366
      Height = 23
      Hint = 'Your public radio station name on the internet.'
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      TextHint = 'myradio.myhostsite.com'
    end
    object btnStatus: TButton
      Left = 10
      Top = 242
      Width = 90
      Height = 28
      Caption = 'Status'
      TabOrder = 5
      OnClick = StatusClick
    end
    object btnStart: TButton
      Left = 106
      Top = 242
      Width = 90
      Height = 28
      Caption = 'Start'
      TabOrder = 6
      OnClick = StartClick
    end
    object btnStop: TButton
      Left = 202
      Top = 242
      Width = 90
      Height = 28
      Caption = 'Stop'
      TabOrder = 7
      OnClick = StopClick
    end
    object btnRestart: TButton
      Left = 298
      Top = 242
      Width = 90
      Height = 28
      Caption = 'Restart'
      TabOrder = 8
      OnClick = RestartClick
    end
    object btnInstall: TButton
      Left = 505
      Top = 35
      Width = 90
      Height = 28
      Caption = 'Install'
      TabOrder = 9
      OnClick = InstallClick
    end
    object btnUninstall: TButton
      Left = 505
      Top = 70
      Width = 90
      Height = 28
      Caption = 'Uninstall'
      TabOrder = 11
      OnClick = UninstallClick
    end
    object btnOpenFolder: TButton
      Left = 611
      Top = 36
      Width = 90
      Height = 28
      Caption = 'Open folder'
      TabOrder = 10
      OnClick = OpenFolderClick
    end
    object btnOpenLog: TButton
      Left = 505
      Top = 105
      Width = 90
      Height = 28
      Caption = 'Open log'
      TabOrder = 13
      OnClick = OpenLogClick
    end
    object btnEditIni: TButton
      Left = 505
      Top = 140
      Width = 90
      Height = 28
      Caption = 'Edit INI'
      TabOrder = 15
      OnClick = EditConfigClick
    end
    object btnHealth: TButton
      Left = 406
      Top = 242
      Width = 90
      Height = 28
      Caption = 'Health check'
      TabOrder = 17
      OnClick = HealthClick
    end
    object btnOpenLan: TButton
      Left = 611
      Top = 71
      Width = 90
      Height = 28
      Caption = 'Open LAN'
      TabOrder = 12
      OnClick = OpenLanClick
    end
    object btnOpenHttps: TButton
      Left = 611
      Top = 106
      Width = 90
      Height = 28
      Caption = 'Open HTTPS'
      TabOrder = 14
      OnClick = OpenPublicClick
    end
    object btnSaveSettings: TButton
      Left = 611
      Top = 242
      Width = 90
      Height = 28
      Caption = 'Save settings'
      TabOrder = 18
      OnClick = SaveClick
    end
    object btnChangePassword: TButton
      Left = 611
      Top = 141
      Width = 90
      Height = 28
      Hint = 'Change Admin Password'
      Caption = 'Password'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = ChangePasswordClick
    end
  end
  object FStatusBar: TStaticText
    Left = 0
    Top = 548
    Width = 714
    Height = 23
    Align = alBottom
    Caption = 'Status: not checked'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    Transparent = False
  end
end
