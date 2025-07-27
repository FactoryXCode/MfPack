object frm_MfPlayer: Tfrm_MfPlayer
  Left = 0
  Top = 0
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  Caption = 'MfPlayer I'
  ClientHeight = 307
  ClientWidth = 581
  Color = clGray
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object dlgOpenUrl: TOpenDialog
    Title = 'Open mediafile'
    Left = 98
    Top = 10
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 10
    object muFile: TMenuItem
      Caption = '&File'
      object muOpen: TMenuItem
        Caption = '&Open'
        OnClick = muOpenClick
      end
      object muSeparator1: TMenuItem
        Caption = '-'
      end
      object muExit: TMenuItem
        Caption = 'E&xit'
        OnClick = muExitClick
      end
    end
  end
end
