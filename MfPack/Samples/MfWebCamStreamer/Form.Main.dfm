object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfWebCamStreamer'
  ClientHeight = 606
  ClientWidth = 724
  Color = clBtnFace
  Constraints.MinHeight = 560
  Constraints.MinWidth = 740
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 724
    Height = 184
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 900
    object lblCamera: TLabel
      Left = 4
      Top = 19
      Width = 104
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Camera:'
    end
    object lblMicrophone: TLabel
      Left = 4
      Top = 47
      Width = 104
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Microphone:'
    end
    object lblPublishFolder: TLabel
      Left = 4
      Top = 78
      Width = 104
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'FxServe folder:'
    end
    object lblPublicUrl: TLabel
      Left = 2
      Top = 108
      Width = 104
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Browser URL:'
    end
    object cbCamera: TComboBox
      Left = 114
      Top = 16
      Width = 459
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object cbMicrophone: TComboBox
      Left = 112
      Top = 43
      Width = 461
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object edPublishFolder: TEdit
      Left = 112
      Top = 73
      Width = 499
      Height = 21
      TabOrder = 2
    end
    object btnBrowsePublishFolder: TButton
      Left = 614
      Top = 68
      Width = 90
      Height = 29
      Caption = 'Select folder...'
      TabOrder = 3
      OnClick = btnBrowsePublishFolderClick
    end
    object edPublicUrl: TEdit
      Left = 112
      Top = 103
      Width = 499
      Height = 21
      TabOrder = 4
    end
    object btnOpenBrowser: TButton
      Left = 614
      Top = 135
      Width = 90
      Height = 29
      Caption = 'Open browser'
      TabOrder = 5
      OnClick = btnOpenBrowserClick
    end
    object btnStart: TButton
      Left = 112
      Top = 140
      Width = 90
      Height = 32
      Caption = 'Start streaming'
      TabOrder = 6
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 208
      Top = 140
      Width = 90
      Height = 32
      Caption = 'Stop'
      TabOrder = 7
      OnClick = btnStopClick
    end
    object chkMonitorAudio: TCheckBox
      Left = 318
      Top = 147
      Width = 188
      Height = 17
      Caption = 'Monitor microphone locally'
      TabOrder = 8
    end
    object btnSelectBrowserUrl: TButton
      Left = 614
      Top = 100
      Width = 90
      Height = 29
      Caption = 'Select folder...'
      TabOrder = 9
      OnClick = btnSelectBrowserUrlClick
    end
  end
  object memStatus: TMemo
    Left = 0
    Top = 527
    Width = 724
    Height = 79
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitTop = 534
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 184
    Width = 724
    Height = 343
    Align = alClient
    BevelOuter = bvLowered
    Caption = 'Camera preview appears when streaming'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 900
    ExplicitHeight = 296
  end
  object tmrStatus: TTimer
    Interval = 250
    OnTimer = tmrStatusTimer
    Left = 602
    Top = 12
  end
end
