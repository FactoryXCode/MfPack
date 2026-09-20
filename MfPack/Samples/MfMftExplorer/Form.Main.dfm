object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfMftExplorer'
  ClientHeight = 461
  ClientWidth = 1054
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 1070
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splDetails: TSplitter
    Left = 761
    Top = 67
    Width = 7
    Height = 354
    ExplicitLeft = 843
    ExplicitTop = 61
    ExplicitHeight = 360
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 1054
    Height = 67
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 1148
    object lblCategory: TLabel
      Left = 12
      Top = 13
      Width = 49
      Height = 13
      Caption = 'Category:'
    end
    object lblScope: TLabel
      Left = 288
      Top = 13
      Width = 33
      Height = 13
      Caption = 'Scope:'
    end
    object cbCategory: TComboBox
      Left = 12
      Top = 32
      Width = 270
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object cbScope: TComboBox
      Left = 288
      Top = 32
      Width = 270
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object chkSortAndFilter: TCheckBox
      Left = 573
      Top = 5
      Width = 131
      Height = 17
      Caption = 'System sort and filter'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object btnEnumerate: TButton
      Left = 573
      Top = 28
      Width = 130
      Height = 30
      Caption = 'Enumerate MFTs'
      Default = True
      TabOrder = 3
      OnClick = btnEnumerateClick
    end
    object btnInspect: TButton
      Left = 775
      Top = 28
      Width = 130
      Height = 30
      Caption = 'Activate and inspect'
      Enabled = False
      TabOrder = 4
      OnClick = btnInspectClick
    end
    object btnCapabilityProbe: TButton
      Left = 911
      Top = 28
      Width = 130
      Height = 30
      Caption = 'Probe selected category'
      TabOrder = 5
      OnClick = btnCapabilityProbeClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 421
    Width = 1054
    Height = 40
    Align = alBottom
    TabOrder = 3
    ExplicitTop = 423
    DesignSize = (
      1054
      40)
    object lblStatus: TLabel
      Left = 12
      Top = 13
      Width = 35
      Height = 13
      Caption = 'Ready.'
    end
    object btnCopyDetails: TButton
      Left = 775
      Top = 6
      Width = 120
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Copy details'
      TabOrder = 0
      OnClick = btnCopyDetailsClick
    end
  end
  object lvTransforms: TListView
    Left = 0
    Top = 67
    Width = 761
    Height = 354
    Align = alLeft
    Columns = <
      item
        Caption = 'Friendly name'
        Width = 250
      end
      item
        Caption = 'CLSID'
        Width = 245
      end
      item
        Caption = 'DXGI adapter'
        Width = 190
      end
      item
        Caption = 'HW URL'
        Width = 55
      end
      item
        Alignment = taRightJustify
        Caption = 'In'
        Width = 35
      end
      item
        Alignment = taRightJustify
        Caption = 'Out'
        Width = 40
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = lvTransformsSelectItem
    ExplicitTop = 61
    ExplicitHeight = 360
  end
  object memDetails: TMemo
    Left = 768
    Top = 67
    Width = 286
    Height = 354
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitLeft = 844
    ExplicitTop = 61
    ExplicitWidth = 304
    ExplicitHeight = 360
  end
end
