object AudioFormatDlg: TAudioFormatDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Select Audio Format'
  ClientHeight = 339
  ClientWidth = 425
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  OnShow = FormShow
  TextHeight = 15
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 425
    Height = 302
    Align = alTop
    Shape = bsFrame
    ExplicitWidth = 422
  end
  object lblAudioFmt: TLabel
    Left = 8
    Top = 8
    Width = 406
    Height = 32
    AutoSize = False
    Caption = 'Audiofmt'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object butOk: TButton
    Left = 262
    Top = 308
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 339
    Top = 308
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object butSaveToFile: TButton
    Left = 8
    Top = 308
    Width = 83
    Height = 25
    Hint = 'Save formats to file (AudioProfiles.txt)'
    Caption = 'Save Formats'
    Default = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = butSaveToFileClick
  end
  object stxtBitRate: TStaticText
    Left = 11
    Top = 46
    Width = 101
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = sbsSunken
    Caption = 'Bit Rate (kbps)'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 3
    Transparent = False
    OnClick = stxtBitRateClick
  end
  object stxtSampleRate: TStaticText
    Left = 112
    Top = 46
    Width = 101
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSunken
    Caption = 'Sampling Rate (Khz)'
    Color = clMenuHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabOrder = 4
    Transparent = False
    OnClick = stxtSampleRateClick
  end
  object stxtChannels: TStaticText
    Left = 313
    Top = 46
    Width = 101
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSunken
    Caption = 'Channels'
    Color = clMenuHighlight
    ParentColor = False
    TabOrder = 5
    Transparent = False
    OnClick = stxtChannelsClick
  end
  object sgAudioFormats: TStringGrid
    Left = 9
    Top = 66
    Width = 405
    Height = 229
    DefaultColWidth = 100
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 6
    OnClick = sgAudioFormatsClick
  end
  object stxtBitsPerSample: TStaticText
    Left = 213
    Top = 46
    Width = 101
    Height = 20
    Alignment = taCenter
    AutoSize = False
    BorderStyle = sbsSunken
    Caption = 'Bits per sample'
    Color = clMenuHighlight
    ParentColor = False
    TabOrder = 7
    Transparent = False
    OnClick = stxtBitsPerSampleClick
  end
end
