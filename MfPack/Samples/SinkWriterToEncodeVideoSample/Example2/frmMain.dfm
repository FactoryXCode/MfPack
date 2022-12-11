object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Using sinkwriter to encode video Sample2'
  ClientHeight = 522
  ClientWidth = 667
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 0
    Top = 498
    Width = 667
    Height = 24
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    Caption = 'Select '#39'Open Bitmap'#39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 536
    ExplicitWidth = 883
  end
  object imgBitmap: TImage
    Left = 14
    Top = 8
    Width = 640
    Height = 480
    Proportional = True
  end
  object MainMenu: TMainMenu
    Left = 44
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open Bitmap'
        Hint = 'Open a bitmapfile'
        OnClick = Open1Click
      end
      object mnuCreateFile: TMenuItem
        Caption = 'Render Video File'
        Enabled = False
        OnClick = mnuCreateFileClick
      end
      object mnuPlayVideoFile: TMenuItem
        Caption = 'Play Video File'
        Enabled = False
        OnClick = mnuPlayVideoFileClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object Videooutput1: TMenuItem
        Caption = 'Video Output'
        OnClick = Videooutput1Click
      end
    end
  end
  object dlgOpenPicture: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 124
    Top = 24
  end
end
