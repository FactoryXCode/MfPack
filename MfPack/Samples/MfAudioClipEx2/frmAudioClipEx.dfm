object AudioClipExfrm: TAudioClipExfrm
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsSingle
  Caption = 'AudioClipEx2 Sample'
  ClientHeight = 328
  ClientWidth = 718
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 25
  object lblStatus: TLabel
    Left = 39
    Top = 198
    Width = 57
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Ready.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblGetSourceFile: TLabel
    Left = 65
    Top = 12
    Width = 94
    Height = 32
    Cursor = crHandPoint
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Source file:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Layout = tlCenter
    OnClick = lblGetSourceFileClick
  end
  object lblSourceFile: TLabel
    Left = 168
    Top = 14
    Width = 7
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '-'
  end
  object lblTargetFile: TLabel
    Left = 168
    Top = 59
    Width = 7
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '-'
  end
  object lblSetTartgetFile: TLabel
    Left = 62
    Top = 53
    Width = 95
    Height = 31
    Cursor = crHandPoint
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Target file:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Layout = tlCenter
    OnClick = lblSetTartgetFileClick
  end
  object Label2: TLabel
    Left = 39
    Top = 144
    Width = 120
    Height = 32
    Hint = 'High or low (default = Low)'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Sample speed: '
    ParentShowHint = False
    ShowHint = True
    Layout = tlCenter
  end
  object Label1: TLabel
    Left = 0
    Top = 98
    Width = 159
    Height = 31
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Sourcefile duration:'
    Layout = tlCenter
  end
  object lblTime: TLabel
    Left = 172
    Top = 101
    Width = 68
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '00:00:00'
    ParentShowHint = False
    ShowHint = False
  end
  object pbProgress: TProgressBar
    Left = 39
    Top = 232
    Width = 672
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 0
  end
  object butStart: TButton
    Left = 25
    Top = 278
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Start'
    TabOrder = 1
    OnClick = butStartClick
  end
  object butCancel: TButton
    Left = 148
    Top = 278
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = butCancelClick
  end
  object tbPriority: TTrackBar
    Left = 158
    Top = 144
    Width = 178
    Height = 53
    Hint = 'High or low (default = Low)'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ParentShowHint = False
    PositionToolTip = ptRight
    ShowHint = True
    TabOrder = 3
    ThumbLength = 30
    OnChange = tbPriorityChange
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 378
    Top = 26
  end
end
