object DevicesDlg: TDevicesDlg
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Audio Endpoints'
  ClientHeight = 249
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 44
    Height = 13
    Caption = 'Devices'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 468
    Height = 25
    Align = alTop
    Shape = bsTopLine
    ExplicitLeft = 342
    ExplicitTop = 14
    ExplicitWidth = 177
  end
  object butOk: TButton
    Left = 109
    Top = 214
    Width = 95
    Height = 27
    Caption = '&Ok'
    TabOrder = 2
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 210
    Top = 214
    Width = 95
    Height = 27
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = butCancelClick
  end
  object butRefresh: TButton
    Left = 8
    Top = 214
    Width = 95
    Height = 27
    Caption = '&Refresh'
    TabOrder = 1
    OnClick = butRefreshClick
  end
  object sgRenderingDevices: TStringGrid
    Left = 0
    Top = 25
    Width = 419
    Height = 183
    BorderStyle = bsNone
    ColCount = 6
    DefaultRowHeight = 18
    DrawingStyle = gdsClassic
    FixedColor = clSilver
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
    TabOrder = 0
  end
end
