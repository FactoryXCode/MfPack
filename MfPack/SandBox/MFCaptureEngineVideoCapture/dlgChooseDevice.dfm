object ChooseDeviceDlg: TChooseDeviceDlg
  Left = 227
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Camera'
  ClientHeight = 299
  ClientWidth = 509
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 345
    Top = 246
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 426
    Top = 246
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object lbxDeviceList: TListBox
    Left = 8
    Top = 8
    Width = 493
    Height = 183
    ItemHeight = 13
    TabOrder = 2
  end
end
