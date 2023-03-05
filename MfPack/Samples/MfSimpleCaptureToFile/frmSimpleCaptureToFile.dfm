object Frm_SimpleCapture: TFrm_SimpleCapture
  Left = 0
  Top = 0
  Caption = 'Mf Simple Capture To File example'
  ClientHeight = 197
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI Semilight'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 158
    Width = 341
    Height = 39
    Align = alBottom
    BevelInner = bvLowered
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI Semilight'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitTop = 160
    ExplicitWidth = 320
    object butStopCapture: TButton
      Left = 92
      Top = 7
      Width = 83
      Height = 25
      Hint = 'Stop Capturing'
      Caption = 'Stop Capture'
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = butStopCaptureClick
    end
    object butStartCapture: TButton
      Left = 6
      Top = 7
      Width = 83
      Height = 25
      Caption = 'Start Capture'
      Default = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 1
      OnClick = butStartCaptureClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 341
    Height = 158
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI Semilight'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 1
    object Label1: TLabel
      Left = 18
      Top = 8
      Width = 109
      Height = 13
      Caption = 'Select a capture device'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 18
      Top = 56
      Width = 56
      Height = 13
      Caption = 'Capture File'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
    end
    object Bevel1: TBevel
      Left = 6
      Top = 99
      Width = 325
      Height = 50
    end
    object cbxSelectDevice: TComboBox
      Left = 18
      Top = 24
      Width = 304
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object edOutputFile: TEdit
      Left = 18
      Top = 72
      Width = 304
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = 'Capture001.mp4'
    end
    object rbMp4: TRadioButton
      Left = 14
      Top = 104
      Width = 113
      Height = 17
      Caption = 'MP4'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      TabStop = True
      OnClick = rbMp4Click
    end
    object rbWmf: TRadioButton
      Left = 14
      Top = 127
      Width = 113
      Height = 17
      Caption = 'WMV'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI Semilight'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = rbWmfClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 231
    Top = 103
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
end
