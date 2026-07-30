object CastDevicesDlg: TCastDevicesDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ChromeCast devices'
  ClientHeight = 275
  ClientWidth = 544
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBtns: TPanel
    Left = 0
    Top = 237
    Width = 544
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    Color = clMedGray
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object btnCancel: TButton
      Left = 446
      Top = 6
      Width = 91
      Height = 27
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object chkAutoRefresh: TCheckBox
      Left = 14
      Top = 12
      Width = 151
      Height = 15
      Caption = 'Auto Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkAutoRefreshClick
    end
    object btnCast: TButton
      Left = 353
      Top = 6
      Width = 91
      Height = 27
      Caption = 'Cast'
      Default = True
      Enabled = False
      TabOrder = 2
      OnClick = btnCastClick
    end
    object btnRefresh: TButton
      Left = 260
      Top = 6
      Width = 91
      Height = 27
      Caption = 'Refresh'
      Enabled = False
      TabOrder = 3
      OnClick = btnRefreshClick
    end
  end
  object lvCastDevices: TListView
    Left = 0
    Top = 0
    Width = 544
    Height = 213
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBlack
    Columns = <
      item
        Caption = 'Name'
        MaxWidth = 550
        MinWidth = 20
        Width = 240
      end
      item
        Caption = 'Device'
        MaxWidth = 550
        MinWidth = 20
        Width = 300
      end>
    ColumnClick = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvCastDevicesDblClick
    OnSelectItem = lvCastDevicesSelectItem
    ExplicitHeight = 151
  end
  object lblConnectionStatus: TEdit
    Left = 0
    Top = 213
    Width = 544
    Height = 24
    Align = alBottom
    AutoSelect = False
    AutoSize = False
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
end
