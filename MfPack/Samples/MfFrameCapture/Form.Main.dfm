object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Width = 1010
  Height = 799
  AutoScroll = True
  Caption = 'Media Foundation Frame Capture Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = HandleFormCreate
  OnDestroy = HandleFormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 994
    Height = 265
    Align = alTop
    TabOrder = 0
    DesignSize = (
      994
      265)
    object lblVideo: TLabel
      Left = 16
      Top = 27
      Width = 49
      Height = 13
      Caption = 'File / URL:'
    end
    object lblLog: TLabel
      Left = 16
      Top = 96
      Width = 21
      Height = 13
      Caption = 'Log:'
    end
    object lblCurrentPosition: TLabel
      Left = 73
      Top = 197
      Width = 14
      Height = 13
      Caption = 'NA'
    end
    object lblMs: TLabel
      Left = 323
      Top = 167
      Width = 63
      Height = 13
      Caption = '(milliseconds)'
    end
    object lblAccuracy: TLabel
      Left = 192
      Top = 167
      Width = 51
      Height = 13
      Caption = 'Accuracy: '
    end
    object lblFramesToSkip: TLabel
      Left = 403
      Top = 167
      Width = 123
      Height = 13
      Caption = 'Maximum Frames To Skip:'
    end
    object lblPosition: TLabel
      Left = 16
      Top = 217
      Width = 41
      Height = 13
      Caption = 'Position:'
    end
    object lblMethod: TLabel
      Left = 16
      Top = 166
      Width = 40
      Height = 13
      Caption = 'Method:'
    end
    object edtVideoFile: TEdit
      Left = 73
      Top = 24
      Width = 650
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnBrowse: TButton
      Left = 729
      Top = 20
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Browse...'
      TabOrder = 1
      OnClick = HandleBrowseClick
    end
    object memLog: TMemo
      Left = 73
      Top = 51
      Width = 899
      Height = 102
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object btnClearLog: TButton
      Left = 897
      Top = 159
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear Log'
      TabOrder = 3
      OnClick = HandleClearLogClick
    end
    object tbVideoPosition: TTrackBar
      Left = 63
      Top = 214
      Width = 909
      Height = 45
      Anchors = [akLeft, akTop, akRight]
      Max = 1000000
      PageSize = 1
      TabOrder = 4
      OnChange = HandleTrackbarChange
    end
    object btnClose: TButton
      Left = 895
      Top = 20
      Width = 77
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close Video'
      TabOrder = 5
      OnClick = HandleCloseVideoClick
    end
    object btnOpen: TButton
      Left = 814
      Top = 20
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Open Video'
      TabOrder = 6
      OnClick = HandleOpenClick
    end
    object spAccuracy: TSpinEdit
      Left = 249
      Top = 159
      Width = 68
      Height = 22
      MaxValue = 5000
      MinValue = 100
      TabOrder = 7
      Value = 200
    end
    object spMaxSkipFrames: TSpinEdit
      Left = 532
      Top = 162
      Width = 56
      Height = 22
      MaxValue = 200
      MinValue = 1
      TabOrder = 8
      Value = 120
    end
    object cboMethod: TComboBox
      Left = 73
      Top = 161
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 9
      Text = 'Synchronous'
      OnChange = HandleMethodChanged
      Items.Strings = (
        'Synchronous'
        'Asynchronous')
    end
    object btnSaveImage: TButton
      Left = 800
      Top = 159
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save Image'
      TabOrder = 10
      OnClick = HandleSaveImageClick
    end
  end
  object pnlFrameCapture: TPanel
    Left = 0
    Top = 265
    Width = 994
    Height = 475
    Align = alClient
    Caption = 'No Image. Waiting frame capture...'
    TabOrder = 1
    object picFrame: TImage
      Left = 1
      Top = 1
      Width = 992
      Height = 473
      Align = alClient
      AutoSize = True
      Center = True
      Proportional = True
      ExplicitLeft = 2
      ExplicitTop = 0
    end
  end
  object fdOpenVideo: TOpenDialog
    Filter = 
      'Supported Videos |*.asf;*.wma;*.wmv;*.avi;*.m4a;*.m4v;*.mov;*.mp' +
      '4;*.mts'
    Title = 'Select a video to open'
    Left = 32
    Top = 272
  end
  object MainMenu1: TMainMenu
    Left = 336
    Top = 401
    object File1: TMenuItem
      Caption = 'File'
      object mnEdit: TMenuItem
        Caption = 'Exit'
        OnClick = HandleExitClick
      end
    end
    object mnLogLevel: TMenuItem
      Caption = 'Log Level'
      object mnDebugLevel: TMenuItem
        Caption = 'Debug'
        OnClick = HandleLogLevelChange
      end
      object mnInfoLevel: TMenuItem
        Tag = 1
        Caption = 'Info'
        OnClick = HandleLogLevelChange
      end
      object mnWarningLevel: TMenuItem
        Tag = 2
        Caption = 'Warning'
        OnClick = HandleLogLevelChange
      end
      object mnErrorLevel: TMenuItem
        Tag = 3
        Caption = 'Error'
        OnClick = HandleLogLevelChange
      end
    end
  end
  object sdSaveFrame: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 
      'BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png|JPEG image (*.jp' +
      'g, *.jpeg)|*.jpg'
    Left = 32
    Top = 328
  end
end
