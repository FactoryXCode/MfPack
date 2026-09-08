object FxServePasswordDlg: TFxServePasswordDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FactoryX FxServe Admin'
  ClientHeight = 93
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 127
    Height = 15
    Caption = 'Enter admin password:'
  end
  object edtPassword: TEdit
    Left = 8
    Top = 27
    Width = 217
    Height = 23
    PasswordChar = '*'
    TabOrder = 0
  end
  object btnOk: TButton
    Left = 69
    Top = 60
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 150
    Top = 60
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
