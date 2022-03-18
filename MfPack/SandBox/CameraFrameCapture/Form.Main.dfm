object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Media Foundation Camera Capture Demo'
  ClientHeight = 723
  ClientWidth = 1205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = HandlFormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1205
    Height = 161
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1205
      161)
    object lblSelectDevice: TLabel
      Left = 16
      Top = 13
      Width = 72
      Height = 15
      Caption = 'Select Device:'
    end
    object lblResoltution: TLabel
      Left = 448
      Top = 10
      Width = 59
      Height = 15
      Caption = 'Resolution:'
    end
    object lblLog: TLabel
      Left = 16
      Top = 73
      Width = 23
      Height = 15
      Caption = 'Log:'
    end
    object lblSupported: TLabel
      Left = 814
      Top = 13
      Width = 22
      Height = 15
      Caption = 'N/A'
    end
    object lblMethod: TLabel
      Left = 16
      Top = 128
      Width = 45
      Height = 15
      Caption = 'Method:'
    end
    object lblSeconds: TLabel
      Left = 487
      Top = 128
      Width = 43
      Height = 15
      Caption = 'seconds'
    end
    object cbxCaptureDevices: TComboBox
      Left = 94
      Top = 8
      Width = 259
      Height = 23
      DropDownCount = 30
      TabOrder = 0
      OnChange = HandleSelectedDeviceChange
    end
    object cbxResolution: TComboBox
      Left = 513
      Top = 8
      Width = 288
      Height = 23
      TabOrder = 1
      OnChange = cbxResolutionChange
    end
    object btnRefreshDevices: TButton
      Left = 359
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnRefreshDevicesClick
    end
    object memLog: TMemo
      Left = 94
      Top = 37
      Width = 1091
      Height = 79
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object btnClearLog: TButton
      Left = 1110
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Clear Log'
      TabOrder = 4
      OnClick = btnClearLogClick
    end
    object btnCaptureFrame: TButton
      Left = 783
      Top = 122
      Width = 91
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Capture Frame'
      TabOrder = 5
      OnClick = btnCaptureFrameClick
    end
    object btnSaveImage: TButton
      Left = 896
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save Image'
      TabOrder = 6
      OnClick = HandleSaveImageClick
    end
    object btnStartBurst: TButton
      Left = 313
      Top = 122
      Width = 113
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Start Burst Capture'
      TabOrder = 7
      OnClick = HandleStartBurstCapture
    end
    object btnStopBurst: TButton
      Left = 536
      Top = 122
      Width = 113
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Stop Burst Capture'
      TabOrder = 8
      OnClick = HandleStopBurstCapture
    end
    object btnCopyLog: TButton
      Left = 1022
      Top = 122
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Copy Log'
      TabOrder = 9
      OnClick = HandleCopyLog
    end
    object cboMethod: TComboBox
      Left = 94
      Top = 122
      Width = 105
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 10
      Text = 'Synchronous'
      OnChange = HandleMethodChanged
      Items.Strings = (
        'Synchronous'
        'Asynchronous')
    end
    object chkEnablePreview: TCheckBox
      Left = 205
      Top = 126
      Width = 97
      Height = 17
      Caption = 'Enable Preview'
      Checked = True
      State = cbChecked
      TabOrder = 11
      OnClick = HandleChangeEnablePreview
    end
    object cbxDuration: TComboBox
      Left = 432
      Top = 122
      Width = 49
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 12
      Text = '10'
      Items.Strings = (
        '10'
        '30'
        '60'
        '90'
        '120')
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 161
    Width = 1205
    Height = 562
    Align = alClient
    TabOrder = 1
    object tcCapture: TPageControl
      Left = 1
      Top = 1
      Width = 1203
      Height = 560
      ActivePage = tsFrame
      Align = alClient
      TabOrder = 0
      object tsFrame: TTabSheet
        Caption = 'Frame'
        object pnlFrameCapture: TPanel
          Left = 0
          Top = 0
          Width = 1195
          Height = 530
          Align = alClient
          Caption = 'No Image. Waiting frame capture...'
          Color = clMedGray
          DoubleBuffered = False
          ParentBackground = False
          ParentDoubleBuffered = False
          TabOrder = 0
          object pbCapture: TPaintBox
            Left = 1
            Top = 1
            Width = 1193
            Height = 528
            Align = alClient
            OnPaint = HandleCapturePaint
            ExplicitLeft = 0
            ExplicitTop = 0
          end
        end
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
  object MainMenu1: TMainMenu
    Left = 336
    Top = 401
    object File1: TMenuItem
      Caption = 'File'
      object mnEdit: TMenuItem
        Caption = 'Exit'
        OnClick = OnExit
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
end
