object dlgChooseDevice: TdlgChooseDevice
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Select Caption Device'
  ClientHeight = 80
  ClientWidth = 291
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 291
    Height = 45
    Align = alTop
    Shape = bsFrame
  end
  object butOk: TButton
    Left = 122
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = butOkClick
  end
  object butCancel: TButton
    Left = 203
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = butCancelClick
  end
  object cbxCaptureDevices: TComboBox
    Left = 12
    Top = 12
    Width = 265
    Height = 21
    TabOrder = 2
    Text = 'cbxCaptureDevices'
  end
end
