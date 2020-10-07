object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '"Chat" Demo'
  ClientHeight = 172
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnChatStart: TButton
    Left = 21
    Top = 140
    Width = 75
    Height = 22
    Caption = 'Start Chat'
    TabOrder = 1
    OnClick = btnChatStartClick
  end
  object btnChatStop: TButton
    Left = 98
    Top = 140
    Width = 75
    Height = 22
    Caption = 'Stop Chat'
    TabOrder = 3
    OnClick = btnChatStopClick
  end
  object butExit: TButton
    Left = 318
    Top = 140
    Width = 72
    Height = 22
    Caption = 'Exit'
    TabOrder = 4
    OnClick = butExitClick
  end
  object GroupBox2: TGroupBox
    Left = 192
    Top = 11
    Width = 198
    Height = 106
    Caption = 'Device Options'
    TabOrder = 2
    TabStop = True
    object rbtCapture: TRadioButton
      Left = 10
      Top = 26
      Width = 153
      Height = 13
      Caption = 'Use default input device'
      TabOrder = 0
      TabStop = True
      OnClick = rbtCaptureClick
    end
    object rbtRender: TRadioButton
      Left = 10
      Top = 51
      Width = 153
      Height = 14
      Caption = 'Use default output device'
      TabOrder = 1
      TabStop = True
      OnClick = rbtCaptureClick
    end
    object chkHideFromVolumeMixer: TCheckBox
      Left = 28
      Top = 73
      Width = 165
      Height = 21
      Caption = 'Hide chat from volume mixer'
      TabOrder = 2
    end
  end
  object GroupBox1: TGroupBox
    Left = 11
    Top = 11
    Width = 172
    Height = 65
    Caption = 'API'
    TabOrder = 0
    TabStop = True
    object cbxChatTransport: TComboBox
      Left = 10
      Top = 22
      Width = 152
      Height = 21
      DropDownCount = 2
      Font.Charset = ANSI_CHARSET
      Font.Color = clDefault
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = cbxChatTransportChange
    end
  end
end
