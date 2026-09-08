object frmFxServeAdmin: TfrmFxServeAdmin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FxServe Admin 1.1'
  ClientHeight = 472
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
    Top = 268
    Width = 708
    Height = 182
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
    ExplicitTop = 265
    ExplicitHeight = 281
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 265
    Align = alTop
    Color = clGray
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = 2
    object Bevel3: TBevel
      Left = 6
      Top = 17
      Width = 480
      Height = 196
    end
    object lblServer: TLabel
      Left = 10
      Top = 33
      Width = 97
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Server:'
      Transparent = True
    end
    object lblService: TLabel
      Left = 10
      Top = 62
      Width = 99
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Service:'
      Transparent = True
    end
    object lblShare: TLabel
      Left = 10
      Top = 89
      Width = 99
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FxServe share:'
      Transparent = True
    end
    object lblServerRoot: TLabel
      Left = 10
      Top = 117
      Width = 99
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Server local path:'
      Transparent = True
    end
    object lblPublicHost: TLabel
      Left = 10
      Top = 145
      Width = 97
      Height = 15
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Public hostname:'
      Transparent = True
    end
    object Bevel1: TBevel
      Left = 1
      Top = 219
      Width = 712
      Height = 45
      Align = alBottom
      ExplicitLeft = 0
      ExplicitTop = 225
    end
    object Bevel2: TBevel
      Left = 492
      Top = 17
      Width = 217
      Height = 196
    end
    object Label1: TLabel
      Left = 14
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
      Left = 499
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
      Left = 113
      Top = 30
      Width = 368
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
      Left = 113
      Top = 59
      Width = 368
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
      Left = 113
      Top = 86
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
      Top = 114
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
      Left = 113
      Top = 142
      Width = 368
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
      Top = 228
      Width = 90
      Height = 28
      Caption = 'Status'
      TabOrder = 14
      OnClick = StatusClick
    end
    object btnStart: TButton
      Left = 106
      Top = 228
      Width = 90
      Height = 28
      Caption = 'Start'
      TabOrder = 15
      OnClick = StartClick
    end
    object btnStop: TButton
      Left = 202
      Top = 228
      Width = 90
      Height = 28
      Caption = 'Stop'
      TabOrder = 16
      OnClick = StopClick
    end
    object btnRestart: TButton
      Left = 298
      Top = 228
      Width = 90
      Height = 28
      Caption = 'Restart'
      TabOrder = 17
      OnClick = RestartClick
    end
    object btnInstall: TButton
      Left = 505
      Top = 36
      Width = 90
      Height = 28
      Caption = 'Install'
      TabOrder = 8
      OnClick = InstallClick
    end
    object btnUninstall: TButton
      Left = 505
      Top = 70
      Width = 90
      Height = 28
      Caption = 'Uninstall'
      TabOrder = 9
      OnClick = UninstallClick
    end
    object btnOpenFolder: TButton
      Left = 611
      Top = 35
      Width = 90
      Height = 28
      Caption = 'Open folder'
      TabOrder = 10
      OnClick = OpenFolderClick
    end
    object btnOpenLog: TButton
      Left = 611
      Top = 138
      Width = 90
      Height = 28
      Caption = 'Open log'
      TabOrder = 13
      OnClick = OpenLogClick
    end
    object btnEditIni: TButton
      Left = 209
      Top = 176
      Width = 90
      Height = 28
      Caption = 'Edit INI'
      TabOrder = 6
      OnClick = EditConfigClick
    end
    object btnHealth: TButton
      Left = 408
      Top = 228
      Width = 90
      Height = 28
      Caption = 'Health check'
      TabOrder = 18
      OnClick = HealthClick
    end
    object btnOpenLan: TButton
      Left = 611
      Top = 70
      Width = 90
      Height = 28
      Caption = 'Open LAN'
      TabOrder = 11
      OnClick = OpenLanClick
    end
    object btnOpenHttps: TButton
      Left = 611
      Top = 104
      Width = 90
      Height = 28
      Caption = 'Open HTTPS'
      TabOrder = 12
      OnClick = OpenPublicClick
    end
    object btnSaveSettings: TButton
      Left = 391
      Top = 176
      Width = 90
      Height = 28
      Hint = 
        'Save this profile and apply the server name and public hostname ' +
        'to the deployed FxServe configuration.'
      Caption = 'Save && apply'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = SaveClick
    end
    object btnChangePassword: TButton
      Left = 113
      Top = 176
      Width = 90
      Height = 28
      Hint = 'Change Admin Password'
      Caption = 'Password'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = ChangePasswordClick
    end
  end
  object FStatusBar: TStaticText
    Left = 0
    Top = 453
    Width = 714
    Height = 19
    Margins.Left = 6
    Margins.Right = 6
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
    ExplicitTop = 552
  end
end
