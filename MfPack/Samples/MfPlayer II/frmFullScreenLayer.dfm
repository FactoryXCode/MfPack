object FullScreenLayer: TFullScreenLayer
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 270
  ClientWidth = 521
  Color = clNone
  DefaultMonitor = dmMainForm
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  WindowState = wsMaximized
  StyleElements = [seFont, seClient]
  OnKeyUp = FormKeyUp
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu1: TPopupMenu
    Left = 30
    Top = 64
    object Fullscreen1: TMenuItem
      AutoCheck = True
      Caption = 'Fullscreen'
      OnClick = Fullscreen1Click
    end
    object mnuEnableSubtitling: TMenuItem
      AutoCheck = True
      Caption = 'Enable Subtitling'
      Checked = True
    end
  end
end
