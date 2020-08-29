object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SimplePlay sample'
  ClientHeight = 539
  ClientWidth = 940
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 42
    Top = 14
    object File1: TMenuItem
      Caption = 'File'
      object OpenFile1: TMenuItem
        Caption = 'Open File'
        OnClick = OpenFile1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object dlgOpenFile: TOpenDialog
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a File to Play'
    Left = 116
    Top = 14
  end
end
