object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfSimpleWebCamStreamer'
  ClientHeight = 330
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblCamera: TLabel
    Left = 16
    Top = 20
    Width = 74
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Camera:'
  end
  object lblMicrophone: TLabel
    Left = 16
    Top = 45
    Width = 74
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Microphone:'
  end
  object lblOutput: TLabel
    Left = 16
    Top = 71
    Width = 74
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Output:'
  end
  object lblHttpPort: TLabel
    Left = 16
    Top = 98
    Width = 74
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'HTTP port:'
  end
  object cbCamera: TComboBox
    Left = 96
    Top = 16
    Width = 500
    Height = 21
    Style = csDropDownList
    TabOrder = 0
  end
  object cbMicrophone: TComboBox
    Left = 96
    Top = 42
    Width = 500
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object edOutput: TEdit
    Left = 96
    Top = 68
    Width = 500
    Height = 21
    TabOrder = 2
  end
  object edHttpPort: TEdit
    Left = 96
    Top = 95
    Width = 64
    Height = 21
    TabOrder = 3
    Text = '8080'
  end
  object btnStart: TButton
    Left = 17
    Top = 138
    Width = 120
    Height = 30
    Caption = 'Start'
    TabOrder = 4
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 143
    Top = 138
    Width = 120
    Height = 30
    Caption = 'Stop'
    TabOrder = 5
    OnClick = btnStopClick
  end
  object memStatus: TMemo
    Left = 16
    Top = 180
    Width = 580
    Height = 145
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object btnPauseHttp: TButton
    Left = 271
    Top = 138
    Width = 104
    Height = 30
    Caption = 'Pause HTTP'
    TabOrder = 6
    OnClick = btnPauseHttpClick
  end
  object btnResumeHttp: TButton
    Left = 383
    Top = 138
    Width = 104
    Height = 30
    Caption = 'Resume HTTP'
    TabOrder = 7
    OnClick = btnResumeHttpClick
  end
  object tmrStatus: TTimer
    OnTimer = tmrStatusTimer
    Left = 576
    Top = 120
  end
end
