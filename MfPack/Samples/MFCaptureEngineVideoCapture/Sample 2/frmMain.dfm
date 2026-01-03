object MainWindow: TMainWindow
  Left = 0
  Top = 0
  Anchors = [akTop]
  BorderWidth = 1
  Caption = 'Capture Engine Application Sample 2'
  ClientHeight = 1011
  ClientWidth = 1640
  Color = clDarkslategray
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 144
  DesignSize = (
    1640
    1011)
  TextHeight = 21
  object pnlSnapShot: TPanel
    Left = 1131
    Top = 0
    Width = 507
    Height = 351
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
    object pbCapture: TPaintBox
      Left = 0
      Top = 0
      Width = 507
      Height = 309
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
      Top = 309
      Width = 507
      Height = 42
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      object butSaveToFile: TButton
        Left = 137
        Top = 2
        Width = 109
        Height = 37
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
        Left = 18
        Top = 2
        Width = 110
        Height = 37
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
        Left = 263
        Top = 9
        Width = 145
        Height = 23
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
    Left = -3
    Top = 974
    Width = 1652
    Height = 42
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
  end
  object pnlCameraAndVideoControl: TPanel
    Left = 1131
    Top = 353
    Width = 507
    Height = 333
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alCustom
    Anchors = [akTop, akRight]
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    Visible = False
    object Label1: TLabel
      Left = 21
      Top = 15
      Width = 143
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Camera Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 21
      Top = 131
      Width = 143
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Video Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 21
      Top = 48
      Width = 143
      Height = 20
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Control Property'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 21
      Top = 161
      Width = 143
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Control Property'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 282
      Top = 48
      Width = 143
      Height = 20
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Value'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 282
      Top = 173
      Width = 80
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Value'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 387
      Top = 48
      Width = 143
      Height = 20
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Control'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 387
      Top = 173
      Width = 143
      Height = 24
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 'Control'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblRotation: TLabel
      Left = 27
      Top = 239
      Width = 74
      Height = 21
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Rotation: '
    end
    object cbxCameraControlProperty: TComboBox
      Left = 21
      Top = 72
      Width = 245
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 0
      TabOrder = 0
      Text = 'Pan'
      OnSelect = cbxCameraControlPropertySelect
      Items.Strings = (
        'Pan'
        'Tilt'
        'Roll'
        'Zoom'
        'Exposure'
        'Iris'
        'Focus')
    end
    object cbxVideoControlProperty: TComboBox
      Left = 21
      Top = 192
      Width = 245
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
      Text = 'Brightness '
      Items.Strings = (
        'Brightness '
        'Contrast'
        'Hue'
        'Saturation'
        'Sharpness'
        'Gamma'
        'ColorEnable'
        'WhiteBalance'
        'BacklightCompensation'
        'Gain')
    end
    object cbxCameraControlFlags: TComboBox
      Left = 387
      Top = 72
      Width = 110
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 2
      Text = 'Manual'
      OnSelect = cbxCameraControlFlagsSelect
      Items.Strings = (
        'Manual'
        'Automatic'
        'Asynchronous'
        'Absolute'
        'Relative')
    end
    object cbxVideoControlFlags: TComboBox
      Left = 387
      Top = 194
      Width = 110
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 0
      TabOrder = 3
      Text = 'Manual'
      OnSelect = cbxVideoControlFlagsSelect
      Items.Strings = (
        'Manual'
        'Automatic')
    end
    object cboRotation: TComboBox
      Left = 24
      Top = 269
      Width = 146
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = '0'
      OnChange = cboRotationChange
      Items.Strings = (
        '0'
        '90'
        '180'
        '270')
    end
    object cbxCameraValues: TComboBox
      Left = 282
      Top = 72
      Width = 89
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 0
      TabOrder = 5
      Text = '0'
      OnSelect = cbxCameraValuesSelect
      Items.Strings = (
        '0')
    end
    object cbxVideoValues: TComboBox
      Left = 282
      Top = 192
      Width = 89
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ItemIndex = 0
      TabOrder = 6
      Text = '0'
      OnSelect = cbxVideoValuesSelect
      Items.Strings = (
        '0')
    end
  end
  object MainMenu: TMainMenu
    Left = 74
    Top = 37
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
      object mnuCameraVideoSettings: TMenuItem
        Caption = 'Camera And Video Settings'
        Enabled = False
        OnClick = mnuCameraVideoSettingsClick
      end
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
    Left = 72
    Top = 101
  end
  object dlgSaveVideo: TSaveDialog
    DefaultExt = '.mp4'
    FileName = 'MyVideo.mp4'
    Filter = 
      'MPEG-4 (MP4)|.mp4|Windows Media Video (WMV)|.wmv|Audio Video Int' +
      'erleave (AVI)|.avi'
    Left = 71
    Top = 165
  end
end
