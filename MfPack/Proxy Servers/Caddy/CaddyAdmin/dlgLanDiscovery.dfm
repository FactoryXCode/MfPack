object LanDiscoveryDialog: TLanDiscoveryDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Find LAN device'
  ClientHeight = 360
  ClientWidth = 920
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
  object lblStatus: TLabel
    Left = 12
    Top = 12
    Width = 155
    Height = 15
    Caption = 'Searching the local network...'
  end
  object pbDiscovery: TProgressBar
    Left = 12
    Top = 34
    Width = 896
    Height = 17
    TabOrder = 0
  end
  object lvDevices: TListView
    Left = 12
    Top = 61
    Width = 896
    Height = 244
    Columns = <
      item
        Caption = 'Computer/device name'
        Width = 190
      end
      item
        Caption = 'Alias / model'
        Width = 300
      end
      item
        Caption = 'IPv4 address'
        Width = 120
      end
      item
        Caption = 'MAC address'
        Width = 140
      end
      item
        Caption = 'Manufacturer'
        Width = 130
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvDevicesDblClick
    OnSelectItem = lvDevicesSelectItem
  end
  object btnRefresh: TButton
    Left = 12
    Top = 317
    Width = 90
    Height = 30
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = btnRefreshClick
  end
  object btnSaveList: TButton
    Left = 109
    Top = 317
    Width = 90
    Height = 30
    Caption = 'Save list...'
    Enabled = False
    TabOrder = 3
    OnClick = btnSaveListClick
  end
  object btnOpenList: TButton
    Left = 206
    Top = 317
    Width = 100
    Height = 30
    Caption = 'Open saved list'
    Enabled = False
    TabOrder = 4
    OnClick = btnOpenListClick
  end
  object btnUseIP: TButton
    Left = 737
    Top = 317
    Width = 82
    Height = 30
    Caption = 'Use IP'
    Default = True
    Enabled = False
    TabOrder = 5
    OnClick = btnUseIPClick
  end
  object btnCancel: TButton
    Left = 826
    Top = 317
    Width = 82
    Height = 30
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object tmrDiscovery: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrDiscoveryTimer
    Left = 136
    Top = 317
  end
end
