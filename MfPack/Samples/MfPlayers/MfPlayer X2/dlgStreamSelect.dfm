object dlgSelectStreams: TdlgSelectStreams
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Audio Info'
  ClientHeight = 239
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object butClose: TButton
    Left = 8
    Top = 207
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 0
    OnClick = butCloseClick
  end
  object lvStreams: TListView
    Left = 0
    Top = 0
    Width = 440
    Height = 201
    Align = alTop
    Checkboxes = True
    Columns = <
      item
        Caption = 'Stream ID'
        MaxWidth = 80
        MinWidth = 80
        Width = 80
      end
      item
        Caption = 'Stream Name'
        MaxWidth = 120
        MinWidth = 120
        Width = 120
      end
      item
        Caption = 'Media Type'
        MaxWidth = 100
        MinWidth = 100
        Width = 100
      end
      item
        Caption = 'Language'
        MaxWidth = 140
        MinWidth = 140
        Width = 140
      end>
    ColumnClick = False
    Ctl3D = False
    GridLines = True
    TabOrder = 1
    ViewStyle = vsReport
    OnItemChecked = lvStreamsItemChecked
  end
end
