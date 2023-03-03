object Frm_SimpleCapture: TFrm_SimpleCapture
  Left = 0
  Top = 0
  Caption = 'MfSimpleCapture example'
  ClientHeight = 344
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 295
    Width = 406
    Height = 49
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
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
  end
  object pnlVideo: TPanel
    Left = 0
    Top = 0
    Width = 406
    Height = 295
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
