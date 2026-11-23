object CastRenameDialog: TCastRenameDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Rename Cast receiver'
  ClientHeight = 410
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnCloseQuery = QueryClose
  PixelsPerInch = 96
  TextHeight = 15
  object lblDevice: TLabel
    Left = 16
    Top = 16
    Width = 480
    Height = 18
    AutoSize = False
    Caption = 'Selected Cast receiver'
  end
  object lblNewName: TLabel
    Left = 16
    Top = 46
    Width = 94
    Height = 15
    Caption = 'New device name'
    FocusControl = edtName
  end
  object lblStatus: TLabel
    Left = 16
    Top = 282
    Width = 480
    Height = 70
    AutoSize = False
    Caption = 
      'Choose a name. Some receivers require a local authorization toke' +
      'n.'
    WordWrap = True
  end
  object edtName: TEdit
    Left = 16
    Top = 68
    Width = 480
    Height = 25
    TabOrder = 0
    OnChange = InputChanged
  end
  object grpConnection: TGroupBox
    Left = 16
    Top = 110
    Width = 480
    Height = 158
    Caption = 'Receiver connection'
    TabOrder = 1
    object lblPort: TLabel
      Left = 140
      Top = 27
      Width = 22
      Height = 15
      Caption = 'Port'
      FocusControl = edtPort
    end
    object lblToken: TLabel
      Left = 12
      Top = 88
      Width = 225
      Height = 15
      Caption = 'Local authorization token (only if required)'
      FocusControl = edtToken
    end
    object chkHttps: TCheckBox
      Left = 12
      Top = 24
      Width = 110
      Height = 22
      Caption = 'Use HTTPS'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = SchemeChanged
    end
    object edtPort: TEdit
      Left = 182
      Top = 23
      Width = 75
      Height = 25
      TabOrder = 1
      Text = '8443'
      OnChange = InputChanged
    end
    object chkTrustCertificate: TCheckBox
      Left = 12
      Top = 55
      Width = 450
      Height = 23
      Caption = 'Trust this receiver'#39's local device certificate'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object edtToken: TEdit
      Left = 12
      Top = 111
      Width = 450
      Height = 25
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object btnRename: TButton
    Left = 16
    Top = 365
    Width = 110
    Height = 29
    Caption = 'Rename'
    Default = True
    TabOrder = 2
    OnClick = RenameClick
  end
  object btnReadName: TButton
    Left = 138
    Top = 365
    Width = 150
    Height = 29
    Caption = 'Read current name'
    TabOrder = 3
    OnClick = ReadClick
  end
  object btnClose: TButton
    Left = 386
    Top = 365
    Width = 110
    Height = 29
    Cancel = True
    Caption = 'Close'
    TabOrder = 4
    OnClick = CloseClick
  end
  object tmrPoll: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Poll
    Left = 448
    Top = 16
  end
end
