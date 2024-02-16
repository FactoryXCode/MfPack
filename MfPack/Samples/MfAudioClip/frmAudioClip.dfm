object frmAudioClip: TfrmAudioClip
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Audio Clip'
  ClientHeight = 152
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 5
    Width = 77
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Source file:'
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 0
    Top = 35
    Width = 77
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Target file:'
    Layout = tlCenter
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
  object Label3: TLabel
    Left = 0
    Top = 65
    Width = 77
    Height = 21
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Clip length:'
    Layout = tlCenter
  end
  object Bevel1: TBevel
    Left = 0
    Top = 108
    Width = 510
    Height = 44
    Align = alBottom
    Shape = bsTopLine
  end
  object Edit1: TEdit
    Left = 86
    Top = 66
    Width = 49
    Height = 21
    Hint = 'Enter the clip length in seconds.'
    NumbersOnly = True
    TabOrder = 0
    Text = '5'
  end
  object butCancel: TButton
    Left = 93
    Top = 118
    Width = 82
    Height = 26
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 1
    OnClick = butCancelClick
    OnEnter = butCancelClick
  end
  object butExtract: TButton
    Left = 5
    Top = 118
    Width = 82
    Height = 26
    Caption = 'Extract'
    Enabled = False
    TabOrder = 2
    OnClick = butExtractClick
  end
  object MainMenu1: TMainMenu
    Left = 310
    Top = 4
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open Source File'
        OnClick = Open1Click
      end
      object Extractto1: TMenuItem
        Caption = 'Extract to'
        OnClick = Extractto1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 371
    Top = 4
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wav'
    Title = 'Extract to'
    Left = 436
    Top = 4
  end
end
