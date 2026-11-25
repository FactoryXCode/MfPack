object Frm_SimpleCapture: TFrm_SimpleCapture
  Left = 0
  Top = 0
  Caption = 'MfSimpleCapture Sample 3 - Media Foundation camera controls'
  ClientHeight = 541
  ClientWidth = 724
  Color = clBtnFace
  Constraints.MinHeight = 580
  Constraints.MinWidth = 740
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 17
  object pnlCommands: TPanel
    Left = 0
    Top = 373
    Width = 724
    Height = 104
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 494
    ExplicitWidth = 924
    object lblRotation: TLabel
      Left = 318
      Top = 72
      Width = 52
      Height = 17
      Caption = 'Rotation:'
    end
    object btnSelect: TButton
      Left = 14
      Top = 19
      Width = 116
      Height = 32
      Hint = 'Select a capture device'
      Caption = 'Select device'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = SelectClick
    end
    object btnStart: TButton
      Left = 134
      Top = 19
      Width = 116
      Height = 32
      Caption = 'Start preview'
      TabOrder = 1
      OnClick = StartClick
    end
    object btnCloseCamera: TButton
      Left = 254
      Top = 19
      Width = 116
      Height = 32
      Caption = 'Close camera'
      TabOrder = 2
      OnClick = CloseCameraClick
    end
    object btnControls: TButton
      Left = 374
      Top = 19
      Width = 116
      Height = 32
      Caption = 'Camera controls...'
      TabOrder = 3
      OnClick = ControlsClick
    end
    object chkAutoPreview: TCheckBox
      Left = 514
      Top = 25
      Width = 280
      Height = 22
      Caption = 'Start preview automatically'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object cboRotation: TComboBox
      Left = 390
      Top = 67
      Width = 100
      Height = 25
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = '0'
      OnChange = HandleRotationChanged
      Items.Strings = (
        '0'
        '90'
        '180'
        '270')
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 477
    Width = 724
    Height = 64
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 596
    ExplicitWidth = 1080
    DesignSize = (
      724
      64)
    object lblDevice: TLabel
      Left = 16
      Top = 8
      Width = 692
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'No camera open'
      ExplicitWidth = 1048
    end
    object lblStatus: TLabel
      Left = 16
      Top = 35
      Width = 692
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Select a capture device to begin.'
      ExplicitWidth = 1048
    end
    object Bevel1: TBevel
      Left = 0
      Top = 2
      Width = 724
      Height = 62
      Align = alBottom
      ExplicitWidth = 1080
    end
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 724
    Height = 373
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    OnResize = VideoResize
    ExplicitLeft = 4
    ExplicitTop = 20
    ExplicitWidth = 1080
    ExplicitHeight = 492
  end
  object StateTimer: TTimer
    Interval = 250
    OnTimer = StateTimerTimer
    Left = 1008
    Top = 16
  end
end
