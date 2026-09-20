object frmCapture: TfrmCapture
  Left = 0
  Top = 0
  Caption = 'Sample 15 - Live camera and microphone audio delay'
  ClientHeight = 689
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPreview: TPanel
    Left = 0
    Top = 129
    Width = 625
    Height = 361
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 624
    ExplicitHeight = 277
  end
  object Panel1: TPanel
    Left = 0
    Top = 490
    Width = 625
    Height = 199
    Align = alBottom
    ShowCaption = False
    TabOrder = 1
    object lblDelay: TLabel
      Left = 6
      Top = 9
      Width = 27
      Height = 13
      Caption = 'Delay'
    end
    object lblWet: TLabel
      Left = 6
      Top = 72
      Width = 39
      Height = 13
      Caption = 'Wet mix'
    end
    object lblHeadphones: TLabel
      Left = 212
      Top = 146
      Width = 231
      Height = 13
      Caption = 'Use headphones to avoid microphone feedback.'
    end
    object lblStatus: TLabel
      Left = 1
      Top = 178
      Width = 623
      Height = 20
      Margins.Left = 6
      Margins.Right = 6
      Align = alBottom
      AutoSize = False
      Caption = 'Ready'
      ExplicitLeft = 2
      ExplicitTop = 198
    end
    object tbDelay: TTrackBar
      Left = 6
      Top = 32
      Width = 612
      Height = 40
      Max = 10000
      Min = 1
      Frequency = 1000
      Position = 1
      TabOrder = 0
      OnChange = tbEffectChange
    end
    object tbWet: TTrackBar
      Left = 4
      Top = 91
      Width = 612
      Height = 40
      Max = 100
      Frequency = 10
      TabOrder = 1
      OnChange = tbEffectChange
    end
    object btnStart: TButton
      Left = 14
      Top = 140
      Width = 90
      Height = 27
      Caption = 'Start live'
      TabOrder = 2
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 110
      Top = 140
      Width = 90
      Height = 27
      Caption = 'Stop'
      Enabled = False
      TabOrder = 3
      OnClick = btnStopClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 625
    Height = 129
    Align = alTop
    ShowCaption = False
    TabOrder = 2
    ExplicitWidth = 624
    object lblCamera: TLabel
      Left = 8
      Top = 6
      Width = 37
      Height = 13
      Caption = 'Camera'
    end
    object lblMicrophone: TLabel
      Left = 8
      Top = 47
      Width = 55
      Height = 13
      Caption = 'Microphone'
    end
    object cbCamera: TComboBox
      Left = 8
      Top = 24
      Width = 500
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object cbMicrophone: TComboBox
      Left = 8
      Top = 65
      Width = 500
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object btnRefresh: TButton
      Left = 8
      Top = 92
      Width = 92
      Height = 27
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnRefreshClick
    end
  end
end
