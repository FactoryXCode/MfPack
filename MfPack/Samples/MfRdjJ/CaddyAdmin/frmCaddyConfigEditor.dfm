object frmCaddyConfigEditor: TfrmCaddyConfigEditor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Caddy Configuration'
  ClientHeight = 227
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object Bevel1: TBevel
    Left = 2
    Top = 4
    Width = 534
    Height = 177
  end
  object lblConfigFile: TLabel
    Left = 46
    Top = 17
    Width = 58
    Height = 15
    Caption = 'Config file:'
  end
  object lblSiteAddress: TLabel
    Left = 39
    Top = 49
    Width = 65
    Height = 15
    Caption = 'Site address:'
  end
  object lblCaddyRoot: TLabel
    Left = 15
    Top = 81
    Width = 89
    Height = 15
    Caption = 'Caddy root path:'
  end
  object lblLogFile: TLabel
    Left = 7
    Top = 113
    Width = 97
    Height = 15
    Caption = 'Log file (optional):'
  end
  object lblProxyHost: TLabel
    Left = 9
    Top = 146
    Width = 95
    Height = 15
    Caption = 'Upstream host/IP:'
  end
  object lblProxyPort: TLabel
    Left = 383
    Top = 146
    Width = 25
    Height = 15
    Caption = 'Port:'
  end
  object edtConfigFile: TEdit
    Left = 110
    Top = 14
    Width = 422
    Height = 23
    Hint = 'Caddy config filename. You can'#39't change this value.'
    ReadOnly = True
    TabOrder = 0
    Text = 'caddy.cff'
  end
  object edtSiteAddress: TEdit
    Left = 110
    Top = 46
    Width = 422
    Height = 23
    Hint = 
      'Site address, the first Caddy site label, such as `factoryxradio' +
      '.asuscomm.com`. This can be multiple URL'#39's or sites.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TextHint = 'factoryxradio.asuscomm.com, https://192.168.x.x, etc.'
  end
  object edtCaddyRoot: TEdit
    Left = 109
    Top = 78
    Width = 422
    Height = 23
    Hint = 'Caddy root path, written to every `root * ...` line.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    TextHint = 'C:\Caddy'
  end
  object edtLogFile: TEdit
    Left = 110
    Top = 110
    Width = 422
    Height = 23
    Hint = 
      'Log file path, written to the `output file` line. The editor man' +
      'ages  the file-writer braces automatically; they are Caddy synta' +
      'x and are not part of the path shown in the editor.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    TextHint = 'C:\Caddy\cast-access.log'
  end
  object edtProxyHost: TEdit
    Left = 110
    Top = 142
    Width = 185
    Height = 23
    Hint = 
      'Upstream host/IP and port, written to every `reverse_proxy ...` ' +
      'line.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    TextHint = '127.0.0.1'
  end
  object edtProxyPort: TEdit
    Left = 414
    Top = 142
    Width = 80
    Height = 23
    Hint = 
      'Upstream host/IP and port, written to every `reverse_proxy ...` ' +
      'line.'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    TextHint = '8000'
  end
  object btnFindProxyHost: TButton
    Left = 301
    Top = 141
    Width = 70
    Height = 25
    Hint = 
      'Discover active devices on private IPv4 LANs and use the selecte' +
      'd address where Caddy is installed.'
    Caption = 'Find...'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnFindProxyHostClick
  end
  object btnSave: TButton
    Left = 375
    Top = 191
    Width = 75
    Height = 28
    Caption = 'Save'
    Default = True
    TabOrder = 7
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 456
    Top = 191
    Width = 75
    Height = 28
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
end
