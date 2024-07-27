object MainWindow: TMainWindow
  Left = 0
  Top = 0
  BorderWidth = 1
  Caption = 'Capture Engine Application Sample'
  ClientHeight = 1145
  ClientWidth = 1909
  Color = 5197615
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 168
  DesignSize = (
    1909
    1145)
  TextHeight = 23
  object pnlSnapShot: TPanel
    Left = 1365
    Top = 0
    Width = 543
    Height = 410
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Color = clBackground
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 1353
    object pbCapture: TPaintBox
      Left = 0
      Top = 0
      Width = 543
      Height = 361
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Color = clBackground
      ParentColor = False
    end
    object pnlControls: TPanel
      Left = 0
      Top = 361
      Width = 543
      Height = 49
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      object butSaveToFile: TButton
        Left = 159
        Top = 2
        Width = 128
        Height = 44
        Hint = 'Save photo to file'
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = '&Save To File'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = butSaveToFileClick
      end
      object butTakePhoto: TButton
        Left = 21
        Top = 2
        Width = 128
        Height = 44
        Hint = 'Save photo to file'
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = '&Take Photo'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = butTakePhotoClick
      end
      object chkNoPreview: TCheckBox
        Left = 306
        Top = 11
        Width = 170
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Auto Save'
        TabOrder = 2
      end
    end
  end
  object pnlInfo: TPanel
    Left = -4
    Top = 1097
    Width = 1924
    Height = 49
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alCustom
    Anchors = [akLeft, akRight]
    AutoSize = True
    Caption = '-'
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    ExplicitTop = 1095
    ExplicitWidth = 1912
  end
  object MainMenu: TMainMenu
    Left = 58
    Top = 69
    object Capture1: TMenuItem
      Caption = 'Capture'
      object mnuChooseDevice: TMenuItem
        Caption = 'Choose Device'
        OnClick = mnuChooseDeviceClick
      end
      object mnuStartPreview: TMenuItem
        Caption = 'Start Preview'
        OnClick = mnuStartPreviewClick
      end
      object mnuStartRecording: TMenuItem
        Caption = 'Start Recording'
        Enabled = False
        OnClick = mnuStartRecordingClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object mnuSetVideoOutputFormat: TMenuItem
        Caption = 'Set Video Output Format'
        OnClick = mnuSetVideoOutputFormatClick
      end
    end
  end
  object dlgSaveSnapShot: TSaveDialog
    DefaultExt = '.bmp'
    FileName = 'MyPhoto.bmp'
    Filter = 
      'BMP image (*.bmp)|*.bmp|PNG image (*.png)|*.png|JPEG image (*.jp' +
      'g, *.jpeg)|*.jpg'
    Left = 222
    Top = 69
  end
  object dlgSaveVideo: TSaveDialog
    DefaultExt = '.mp4'
    FileName = 'MyVideo.mp4'
    Filter = 
      'MPEG-4 (MP4)|.mp4|Windows Media Video (WMV)|.wmv|Audio Video Int' +
      'erleave (AVI)|.avi'
    Left = 387
    Top = 69
  end
end
