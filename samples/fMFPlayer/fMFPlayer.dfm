object F_MFPlayer: TF_MFPlayer
  Left = 0
  Top = 0
  Caption = 'FMFPlayer'
  ClientHeight = 451
  ClientWidth = 767
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OpenDialog1: TOpenDialog
    Left = 144
    Top = 8
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 14
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 78
    Top = 8
    object Qrquivo1: TMenuItem
      Caption = 'File'
      object Abrir1: TMenuItem
        Caption = 'Open'
        OnClick = Abrir1Click
      end
      object Sair1: TMenuItem
        Caption = '-'
      end
      object Sair2: TMenuItem
        Caption = 'Exit'
        OnClick = Sair2Click
      end
    end
  end
end
