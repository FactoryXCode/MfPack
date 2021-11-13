object AudioClipExFrm: TAudioClipExFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'AudioClipEx Audio Extractor'
  ClientHeight = 222
  ClientWidth = 532
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object lblGetSourceFile: TLabel
    Left = 17
    Top = 8
    Width = 63
    Height = 21
    Cursor = crHandPoint
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Source file:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Layout = tlCenter
    OnClick = Open1Click
    OnMouseMove = lblGetSourceFileMouseMove
  end
  object lblSetTartgetFile: TLabel
    Left = 14
    Top = 35
    Width = 63
    Height = 21
    Cursor = crHandPoint
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Target file:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Layout = tlCenter
    OnClick = Extractto1Click
    OnMouseMove = lblSetTartgetFileMouseMove
  end
  object lblSourceFile: TLabel
    Left = 86
    Top = 9
    Width = 4
    Height = 13
    Caption = '-'
  end
  object lblTargetFile: TLabel
    Left = 86
    Top = 39
    Width = 4
    Height = 13
    Caption = '-'
  end
  object Label1: TLabel
    Left = 14
    Top = 65
    Width = 63
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Clip length:'
    Layout = tlCenter
  end
  object lblTime: TLabel
    Left = 154
    Top = 69
    Width = 44
    Height = 13
    Caption = '00:00:00'
    ParentShowHint = False
    ShowHint = False
  end
  object Label2: TLabel
    Left = 0
    Top = 96
    Width = 80
    Height = 21
    Hint = 'High or low (default = Low)'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Sample speed: '
    ParentShowHint = False
    ShowHint = True
    Layout = tlCenter
  end
  object lblProgress: TLabel
    Left = 77
    Top = 133
    Width = 448
    Height = 18
    AutoSize = False
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    Layout = tlCenter
  end
  object Label3: TLabel
    Left = 26
    Top = 133
    Width = 51
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Status: '
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    Layout = tlCenter
  end
  object Bevel1: TBevel
    Left = 0
    Top = 179
    Width = 532
    Height = 43
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 178
  end
  object edClipDuration: TEdit
    Left = 86
    Top = 66
    Width = 59
    Height = 21
    Hint = 'Enter the clip duration in seconds.'
    NumbersOnly = True
    TabOrder = 0
    Text = '0'
    OnKeyUp = edClipDurationKeyUp
  end
  object tbPriority: TTrackBar
    Left = 79
    Top = 96
    Width = 42
    Height = 35
    Hint = 'High or low (default = Low)'
    Max = 1
    ParentShowHint = False
    Position = 1
    PositionToolTip = ptRight
    ShowHint = True
    TabOrder = 1
    OnChange = tbPriorityChange
  end
  object prbProgress: TProgressBar
    Left = 76
    Top = 157
    Width = 448
    Height = 15
    ParentCustomHint = False
    ParentShowHint = False
    Smooth = True
    ShowHint = False
    TabOrder = 2
  end
  object butExtract: TButton
    Left = 7
    Top = 188
    Width = 82
    Height = 26
    Caption = 'E&xtract'
    Enabled = False
    TabOrder = 3
    OnClick = butExtractClick
  end
  object butCancel: TButton
    Left = 95
    Top = 188
    Width = 82
    Height = 26
    Caption = '&Cancel'
    Enabled = False
    TabOrder = 4
    OnClick = butCancelClick
  end
  object butClose: TButton
    Left = 183
    Top = 188
    Width = 82
    Height = 26
    Caption = 'C&lose'
    Enabled = False
    TabOrder = 5
    OnClick = butCloseClick
  end
  object MainMenu1: TMainMenu
    Left = 207
    Top = 8
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = '&Open Source File'
        OnClick = Open1Click
      end
      object Extractto1: TMenuItem
        Caption = '&Extract to'
        OnClick = Extractto1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 269
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wav'
    Title = 'Save as'
    Left = 334
    Top = 8
  end
end
