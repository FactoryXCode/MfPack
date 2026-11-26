object MfCameraControlsDialog: TMfCameraControlsDialog
  Caption = 'Media Foundation camera controls'
  ClientHeight = 520
  ClientWidth = 900
  Constraints.MinHeight = 560
  Constraints.MinWidth = 920
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 17
  object Pages: TPageControl
    Align = alClient
    ActivePage = tsLive
    TabOrder = 0
    object tsLive: TTabSheet
      Caption = 'Live controls'
      object lstControls: TListBox
        Align = alLeft
        Width = 250
        ItemHeight = 17
        TabOrder = 0
        OnClick = SelectionChanged
      end
      object pnlValue: TPanel
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object lblName: TLabel
          Left = 20
          Top = 20
          Caption = 'Select a live control'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblCurrent: TLabel
          Left = 20
          Top = 58
          Width = 600
          Height = 22
          AutoSize = False
        end
        object lblRange: TLabel
          Left = 20
          Top = 91
          Width = 600
          Height = 22
          AutoSize = False
        end
        object lblCapabilities: TLabel
          Left = 20
          Top = 124
          Width = 600
          Height = 22
          AutoSize = False
        end
        object lblValue: TLabel
          Left = 20
          Top = 173
          Caption = 'Value (device units)'
        end
        object edValue: TEdit
          Left = 20
          Top = 199
          Width = 160
          Height = 25
          TabOrder = 0
        end
        object chkAuto: TCheckBox
          Left = 204
          Top = 201
          Width = 160
          Height = 22
          Caption = 'Automatic mode'
          TabOrder = 1
        end
        object btnApply: TButton
          Left = 20
          Top = 250
          Width = 155
          Height = 32
          Caption = 'Apply live value'
          TabOrder = 2
          OnClick = ApplyClick
        end
        object btnReset: TButton
          Left = 192
          Top = 250
          Width = 160
          Height = 32
          Caption = 'Apply driver default'
          TabOrder = 3
          OnClick = ResetClick
        end
        object btnRefresh: TButton
          Left = 369
          Top = 250
          Width = 120
          Height = 32
          Caption = 'Refresh'
          TabOrder = 4
          OnClick = RefreshClick
        end
      end
    end
    object tsExtended: TTabSheet
      Caption = 'Extended MF controls'
      object lblExtended: TLabel
        Left = 20
        Top = 20
        Caption = 'Extended camera feature'
      end
      object cbxExtended: TComboBox
        Left = 20
        Top = 47
        Width = 520
        Height = 25
        Style = csDropDownList
        TabOrder = 0
        OnChange = ExtendedChanged
      end
      object lblExtendedInfo: TLabel
        Left = 20
        Top = 92
        Width = 850
        Height = 25
        AutoSize = False
      end
      object cbxMode: TComboBox
        Left = 20
        Top = 140
        Width = 300
        Height = 25
        Style = csDropDownList
        TabOrder = 1
      end
      object btnApplyExtended: TButton
        Left = 340
        Top = 137
        Width = 180
        Height = 32
        Caption = 'Apply extended mode'
        TabOrder = 2
        OnClick = ApplyExtendedClick
      end
      object lblExtendedNote: TLabel
        Left = 20
        Top = 200
        Width = 850
        Height = 100
        AutoSize = False
        WordWrap = True
      end
    end
    object tsDefaults: TTabSheet
      Caption = 'Saved camera settings'
      object lblStorage: TLabel
        Left = 20
        Top = 20
        Width = 70
        Height = 17
        Caption = 'Storage:'
      end
      object cbxStorage: TComboBox
        Left = 100
        Top = 16
        Width = 608
        Height = 25
        Style = csDropDownList
        TabOrder = 4
        OnChange = StorageChanged
        Items.Strings = (
          'INI file (this sample)'
          'Windows camera defaults (companion app)')
      end
      object btnApplyIni: TButton
        Left = 724
        Top = 310
        Width = 146
        Height = 32
        Caption = 'Apply saved INI'
        TabOrder = 5
        OnClick = ApplyIniClick
      end
      object lblDefaults: TLabel
        Left = 20
        Top = 50
        Width = 850
        Height = 45
        AutoSize = False
        WordWrap = True
        Caption = 'Select a standard live control on the first tab. Saved defaults affect future camera opens for this Windows user.'
      end
      object lstDefaults: TListBox
        Left = 20
        Top = 110
        Width = 850
        Height = 182
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 17
        TabOrder = 0
      end
      object btnReadDefaults: TButton
        Left = 20
        Top = 310
        Width = 150
        Height = 32
        Caption = 'Reload saved defaults'
        TabOrder = 1
        OnClick = ReadDefaultsClick
      end
      object btnSaveDefault: TButton
        Left = 184
        Top = 310
        Width = 260
        Height = 32
        Caption = 'Save selected live control as default'
        TabOrder = 2
        OnClick = SaveDefaultClick
      end
      object btnRemoveDefault: TButton
        Left = 458
        Top = 310
        Width = 250
        Height = 32
        Caption = 'Remove selected saved default'
        TabOrder = 3
        OnClick = RemoveDefaultClick
      end
      object lblDefaultsStatus: TLabel
        Left = 20
        Top = 365
        Width = 850
        Height = 45
        AutoSize = False
        WordWrap = True
      end
    end
  end
  object pnlStatus: TPanel
    Align = alBottom
    Height = 54
    BevelOuter = bvNone
    TabOrder = 1
    object lblResult: TLabel
      Left = 16
      Top = 4
      Width = 740
      Height = 20
      AutoSize = False
    end
    object lblMonitor: TLabel
      Left = 16
      Top = 29
      Width = 740
      Height = 20
      AutoSize = False
    end
    object btnClose: TButton
      Left = 790
      Top = 12
      Width = 90
      Height = 32
      Caption = 'Close'
      TabOrder = 0
      OnClick = CloseClick
    end
  end
end
