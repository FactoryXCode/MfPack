object frmAudioTopology: TfrmAudioTopology
  Left = 0
  Top = 0
  Caption = 'MfCustomMFT Sample 12 - Registered Audio Delay'
  ClientHeight = 365
  ClientWidth = 564
  Color = clBtnFace
  Constraints.MinHeight = 404
  Constraints.MinWidth = 580
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    564
    365)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDelay: TLabel
    Left = 16
    Top = 66
    Width = 62
    Height = 13
    Caption = 'Delay: 60 ms'
  end
  object lblMix: TLabel
    Left = 16
    Top = 132
    Width = 69
    Height = 13
    Caption = 'Wet mix: 30%'
  end
  object lblState: TLabel
    Left = 16
    Top = 199
    Width = 77
    Height = 13
    Caption = 'No audio loaded'
  end
  object tbDelay: TTrackBar
    Left = 8
    Top = 85
    Width = 540
    Height = 38
    Anchors = [akLeft, akTop, akRight]
    Max = 2000
    Min = 1
    PageSize = 10
    Position = 60
    ShowSelRange = False
    TabOrder = 3
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEffectChange
  end
  object tbMix: TTrackBar
    Left = 8
    Top = 151
    Width = 540
    Height = 30
    Anchors = [akLeft, akTop, akRight]
    Max = 100
    PageSize = 5
    Position = 30
    PositionToolTip = ptTop
    ShowSelRange = False
    TabOrder = 4
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEffectChange
  end
  object btnOpen: TButton
    Left = 16
    Top = 17
    Width = 90
    Height = 29
    Caption = 'Open audio...'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object btnPlayPause: TButton
    Left = 112
    Top = 17
    Width = 90
    Height = 29
    Caption = 'Play'
    TabOrder = 1
    OnClick = btnPlayPauseClick
  end
  object btnStop: TButton
    Left = 208
    Top = 17
    Width = 90
    Height = 29
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object memLog: TMemo
    Left = 0
    Top = 218
    Width = 564
    Height = 147
    Align = alBottom
    Color = 1515798
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object dlgOpenAudio: TOpenDialog
    Left = 406
    Top = 12
  end
end
