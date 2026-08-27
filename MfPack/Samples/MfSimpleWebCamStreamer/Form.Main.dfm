object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'MfSimpleWebCamStreamer'
  ClientHeight = 330
  ClientWidth = 616
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
    Left = 3
    Top = 18
    Width = 90
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Camera:'
  end
  object lblMicrophone: TLabel
    Left = 3
    Top = 46
    Width = 90
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Microphone:'
  end
  object lblOutput: TLabel
    Left = 3
    Top = 73
    Width = 90
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Output folder:'
  end
  object lblHttpPort: TLabel
    Left = 3
    Top = 101
    Width = 90
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
    Top = 43
    Width = 500
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object edOutput: TEdit
    Left = 96
    Top = 70
    Width = 408
    Height = 21
    TabOrder = 2
  end
  object btnBrowseOutput: TButton
    Left = 510
    Top = 68
    Width = 86
    Height = 25
    Caption = 'Select folder...'
    TabOrder = 3
    OnClick = btnBrowseOutputClick
  end
  object edHttpPort: TEdit
    Left = 96
    Top = 98
    Width = 64
    Height = 21
    TabOrder = 4
    Text = '8080'
  end
  object btnStart: TButton
    Left = 4
    Top = 150
    Width = 100
    Height = 30
    Caption = 'Start'
    TabOrder = 5
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 108
    Top = 150
    Width = 100
    Height = 30
    Caption = 'Stop'
    TabOrder = 6
    OnClick = btnStopClick
  end
  object memStatus: TMemo
    Left = 0
    Top = 185
    Width = 616
    Height = 145
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 9
    ExplicitTop = 186
    ExplicitWidth = 609
  end
  object btnPauseHttp: TButton
    Left = 260
    Top = 150
    Width = 100
    Height = 30
    Caption = 'Pause HTTP'
    TabOrder = 7
    OnClick = btnPauseHttpClick
  end
  object btnResumeHttp: TButton
    Left = 364
    Top = 150
    Width = 100
    Height = 30
    Caption = 'Resume HTTP'
    TabOrder = 8
    OnClick = btnResumeHttpClick
  end
  object tmrStatus: TTimer
    OnTimer = tmrStatusTimer
    Left = 514
    Top = 116
  end
end
