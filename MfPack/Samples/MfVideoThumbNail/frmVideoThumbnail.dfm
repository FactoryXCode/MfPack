object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Video Thumbnail Sample'
  ClientHeight = 640
  ClientWidth = 1224
  Color = 5197615
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnMouseDown = FormMouseDown
  OnShow = FormShow
  TextHeight = 13
  object dlgOpen: TOpenDialog
    Filter = 
      'Avi files (*.avi)|*.avi|Mpeg files (*.mpeg)|*.mpeg|Mp4 files (*.' +
      'mp4)|*.mp4|Mov files (*.mov)|*.mov|All files (*.*)|*.*'
    FilterIndex = 5
    Title = 'Select a File to Play'
    Left = 126
    Top = 22
  end
  object MainMenu: TMainMenu
    Left = 68
    Top = 22
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
end
