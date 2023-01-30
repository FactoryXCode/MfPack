object dlgSelectDevice: TdlgSelectDevice
  Left = 227
  Top = 108
  ActiveControl = butOk
  BorderStyle = bsDialog
  Caption = 'Select Device'
  ClientHeight = 80
  ClientWidth = 377
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 281
    Height = 80
    Align = alLeft
    Shape = bsFrame
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitHeight = 162
  end
  object butOk: TButton
    Left = 294
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 292
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ComboBox1: TComboBox
    Left = 5
    Top = 12
    Width = 268
    Height = 21
    TabOrder = 2
    Text = 'ComboBox1'
  end
end
