object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Using sinkwriter to encode video Sample2'
  ClientHeight = 334
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
    Top = 310
    Width = 667
    Height = 24
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    Caption = 'Choose '#39'Select Single Bitmap'#39' or '#39'Select Multiple Bitmaps'#39
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object imgBitmap: TImage
    Left = 0
    Top = 0
    Width = 667
    Height = 287
    Align = alTop
    ExplicitLeft = -2
  end
  object MainMenu: TMainMenu
    Left = 46
    Top = 30
    object File1: TMenuItem
      Caption = 'File'
      object mnuSelectOneBitmap: TMenuItem
        Caption = 'Select Single Bitmap'
        Hint = 'Open a bitmapfile'
        OnClick = mnuSelectOneBitmapClick
      end
      object mnuSelectMultipleBitmaps: TMenuItem
        Caption = 'Select Multiple Bitmaps'
        Hint = 'Select the first bitmapfile in the directory.'
        OnClick = mnuSelectMultipleBitmapsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuRender: TMenuItem
        Caption = 'Render Video File'
        Enabled = False
        OnClick = mnuRenderClick
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
        OnClick = Exit1Click
      end
    end
    object mmnuSettings: TMenuItem
      Caption = 'Settings'
      object mnuVideoOutput: TMenuItem
        Caption = 'Video Output'
        OnClick = mnuVideoOutputClick
      end
    end
  end
  object dlgOpenPicture: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 126
    Top = 24
  end
end
