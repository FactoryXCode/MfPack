object AudioFormatDlg: TAudioFormatDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Select Audio Format'
  ClientHeight = 408
  ClientWidth = 424
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 424
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
    Left = 259
    Top = 379
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 340
    Top = 379
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object butSaveToFile: TButton
    Left = 8
    Top = 379
    Width = 83
    Height = 25
    Hint = 'Save formats to file (AudioProfiles.txt)'
    Caption = 'Save Formats'
    Default = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Visible = False
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
    Left = 8
    Top = 67
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
    OnDblClick = sgAudioFormatsDblClick
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
  object stxtExtraInfo: TStaticText
    AlignWithMargins = True
    Left = 3
    Top = 305
    Width = 418
    Height = 65
    Align = alTop
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
end

