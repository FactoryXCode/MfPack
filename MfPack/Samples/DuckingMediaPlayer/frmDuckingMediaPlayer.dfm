object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = '"Media Player"'
  ClientHeight = 260
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 13
    Width = 59
    Height = 13
    Caption = 'File to play:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 73
    Width = 555
    Height = 145
  end
  object Label2: TLabel
    Left = 21
    Top = 66
    Width = 126
    Height = 13
    Caption = ' "Media Player" Controls '
    Transparent = False
  end
  object Bevel2: TBevel
    Left = 18
    Top = 85
    Width = 534
    Height = 33
    ParentCustomHint = False
    ParentShowHint = False
    ShowHint = False
  end
  object Label3: TLabel
    Left = 276
    Top = 136
    Width = 41
    Height = 13
    Caption = 'Volume:'
  end
  object Bevel3: TBevel
    Left = 310
    Top = 167
    Width = 185
    Height = 33
    ParentCustomHint = False
    ParentShowHint = False
    ShowHint = False
  end
  object Label4: TLabel
    Left = 318
    Top = 201
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label5: TLabel
    Left = 471
    Top = 201
    Width = 18
    Height = 13
    Caption = '100'
  end
  object edFileName: TEdit
    Left = 18
    Top = 33
    Width = 441
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object butBrowse: TButton
    Left = 485
    Top = 32
    Width = 78
    Height = 23
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = butBrowseClick
  end
  object trbPlayBackPosition: TTrackBar
    Left = 19
    Top = 90
    Width = 531
    Height = 23
    TabOrder = 2
    ThumbLength = 18
    TickMarks = tmBoth
    TickStyle = tsNone
  end
  object butPlay: TButton
    Left = 18
    Top = 132
    Width = 78
    Height = 23
    Caption = 'Play'
    Enabled = False
    TabOrder = 3
    OnClick = butPlayClick
  end
  object butPause: TButton
    Left = 97
    Top = 132
    Width = 78
    Height = 23
    Caption = 'Pause'
    Enabled = False
    TabOrder = 4
    OnClick = butPauseClick
  end
  object butStop: TButton
    Left = 176
    Top = 132
    Width = 78
    Height = 23
    Caption = 'Stop'
    Enabled = False
    TabOrder = 5
    OnClick = butStopClick
  end
  object cbCheckPauseOnDuck: TCheckBox
    Left = 18
    Top = 169
    Width = 109
    Height = 29
    Caption = 'Pause On Duck'
    TabOrder = 6
    OnClick = cbCheckPauseOnDuckClick
  end
  object cbOptOutOnDucking: TCheckBox
    Left = 133
    Top = 169
    Width = 124
    Height = 29
    Caption = 'Opt out on Ducking'
    TabOrder = 7
    OnClick = cbOptOutOnDuckingClick
  end
  object tbVolumeSlider: TTrackBar
    Left = 310
    Top = 173
    Width = 185
    Height = 23
    Max = 100
    TabOrder = 8
    ThumbLength = 18
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbVolumeSliderChange
  end
  object butOK: TButton
    Left = 405
    Top = 229
    Width = 78
    Height = 23
    Caption = 'Ok'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = butOKClick
  end
  object butCancel: TButton
    Left = 485
    Top = 229
    Width = 78
    Height = 23
    Caption = 'Cancel'
    TabOrder = 10
    OnClick = butOKClick
  end
  object cbCheckMute: TCheckBox
    Left = 506
    Top = 174
    Width = 47
    Height = 19
    Caption = 'Mute'
    TabOrder = 11
    OnClick = cbCheckMuteClick
  end
  object dlgOpen: TOpenDialog
    Title = 'Open mediafile'
    Left = 24
    Top = 216
  end
  object tmrProgress: TTimer
    Interval = 40
    OnTimer = tmrProgressTimer
    Left = 82
    Top = 217
  end
end
