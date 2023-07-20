object Frm_SimpleCapture: TFrm_SimpleCapture
  Left = 0
  Top = 0
  Caption = 'MfSimpleCapture example'
  ClientHeight = 343
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 294
    Width = 402
    Height = 49
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    object lblRotation: TLabel
      Left = 216
      Top = 20
      Width = 48
      Height = 13
      Caption = 'Rotation: '
    end
    object Button1: TButton
      Left = 10
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 0
      OnClick = Button1Click
    end
    object butGetDevice: TButton
      Left = 91
      Top = 13
      Width = 108
      Height = 25
      Caption = 'Get Capture Device'
      TabOrder = 1
      OnClick = butGetDeviceClick
    end
    object cboRotation: TComboBox
      Left = 266
      Top = 17
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = '0'
      OnChange = HandleRotationChanged
      Items.Strings = (
        '0'
        '90'
        '180'
        '270')
    end
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 294
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    Color = clNone
    Ctl3D = False
    DoubleBuffered = True
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 1
    OnResize = pnlVideoResize
  end
end
