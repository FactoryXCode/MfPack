object ChooseDeviceDlg: TChooseDeviceDlg
  Left = 227
  Top = 108
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Camera'
  ClientHeight = 281
  ClientWidth = 709
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 15
    Width = 211
    Height = 13
    Caption = 'Camera devices found on this system'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnOK: TButton
    Left = 541
    Top = 246
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 622
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
    Top = 34
    Width = 261
    Height = 201
    Hint = 'Select a camera device'
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = lbxDeviceListClick
  end
  object sgResolutions: TStringGrid
    Left = 275
    Top = 34
    Width = 438
    Height = 201
    DefaultRowHeight = 18
    DrawingStyle = gdsClassic
    FixedColor = clSilver
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 3
    ColWidths = (
      83
      108
      91
      118
      64)
  end
  object cbxSupportedFormatsOnly: TCheckBox
    Left = 275
    Top = 13
    Width = 246
    Height = 15
    Hint = 'Un-check if you want to show all formats.'
    Caption = 'Supported resolutions and framerates only'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 4
    OnClick = cbxSupportedFormatsOnlyClick
  end
end
