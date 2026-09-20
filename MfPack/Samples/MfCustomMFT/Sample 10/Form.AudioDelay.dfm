object frmAudioDelay: TfrmAudioDelay
  Left = 0
  Top = 0
  Caption = 'MfCustomMFT Sample 10 - Audio File Through Delay MFT'
  ClientHeight = 390
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblInput: TLabel
    Left = 16
    Top = 18
    Width = 44
    Height = 13
    Caption = 'Audio file'
  end
  object lblDelay: TLabel
    Left = 16
    Top = 74
    Width = 51
    Height = 13
    Caption = 'Delay (ms)'
  end
  object lblMix: TLabel
    Left = 152
    Top = 74
    Width = 61
    Height = 13
    Caption = 'Wet mix (%)'
  end
  object edtInput: TEdit
    Left = 16
    Top = 37
    Width = 585
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 612
    Top = 35
    Width = 90
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object edtDelay: TEdit
    Left = 16
    Top = 93
    Width = 112
    Height = 21
    TabOrder = 2
  end
  object edtMix: TEdit
    Left = 152
    Top = 93
    Width = 112
    Height = 21
    TabOrder = 3
  end
  object btnProcess: TButton
    Left = 290
    Top = 89
    Width = 90
    Height = 29
    Hint = 'Create delayed WAV...'
    Caption = 'Create WAV...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = btnProcessClick
  end
  object btnPlay: TButton
    Left = 389
    Top = 89
    Width = 90
    Height = 29
    Caption = 'Play result'
    TabOrder = 5
    OnClick = btnPlayClick
  end
  object memLog: TMemo
    Left = 16
    Top = 137
    Width = 688
    Height = 236
    Color = 1515798
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object dlgOpenAudio: TOpenDialog
    Left = 534
    Top = 88
  end
  object dlgSaveWave: TSaveDialog
    Left = 612
    Top = 88
  end
end
