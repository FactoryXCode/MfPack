object dlgMediaTransformTool: TdlgMediaTransformTool
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Media Foundation Transform tool'
  ClientHeight = 394
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel1: TBevel
    Left = 0
    Top = 355
    Width = 640
    Height = 39
    Align = alBottom
    ExplicitTop = 358
  end
  object Bevel2: TBevel
    Left = 0
    Top = 18
    Width = 640
    Height = 337
    Align = alBottom
    ExplicitWidth = 735
  end
  object mmoList: TMemo
    Left = 8
    Top = 33
    Width = 627
    Height = 316
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object butListMFTs: TButton
    Left = 115
    Top = 361
    Width = 100
    Height = 27
    Caption = 'List MFT'#39's'
    TabOrder = 1
    OnClick = butListMFTsClick
  end
  object butExit: TButton
    Left = 9
    Top = 361
    Width = 100
    Height = 27
    Caption = 'Exit'
    TabOrder = 2
  end
  object cbxChoose: TComboBox
    Left = 8
    Top = 8
    Width = 207
    Height = 22
    TabOrder = 3
    Text = 'cbxChoose'
  end
end
