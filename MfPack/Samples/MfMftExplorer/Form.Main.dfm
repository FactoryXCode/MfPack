object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MfMftExplorer'
  ClientHeight = 590
  ClientWidth = 1185
  Color = clBtnFace
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
    Left = 850
    Top = 72
    Width = 5
    Height = 478
    ExplicitLeft = 520
    ExplicitHeight = 450
  end
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 1185
    Height = 72
    Align = alTop
    TabOrder = 0
    object lblCategory: TLabel
      Left = 12
      Top = 12
      Width = 49
      Height = 13
      Caption = 'Category:'
    end
    object lblScope: TLabel
      Left = 300
      Top = 12
      Width = 33
      Height = 13
      Caption = 'Scope:'
    end
    object cbCategory: TComboBox
      Left = 12
      Top = 31
      Width = 270
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object cbScope: TComboBox
      Left = 300
      Top = 31
      Width = 250
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object chkSortAndFilter: TCheckBox
      Left = 570
      Top = 33
      Width = 160
      Height = 17
      Caption = 'System sort and filter'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object btnEnumerate: TButton
      Left = 712
      Top = 27
      Width = 130
      Height = 30
      Caption = 'Enumerate MFTs'
      Default = True
      TabOrder = 3
      OnClick = btnEnumerateClick
    end
    object btnInspect: TButton
      Left = 844
      Top = 27
      Width = 130
      Height = 30
      Caption = 'Activate and inspect'
      Enabled = False
      TabOrder = 4
      OnClick = btnInspectClick
    end
    object btnCapabilityProbe: TButton
      Left = 976
      Top = 27
      Width = 130
      Height = 30
      Caption = 'Probe selected category'
      TabOrder = 5
      OnClick = btnCapabilityProbeClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 550
    Width = 1185
    Height = 40
    Align = alBottom
    TabOrder = 3
    DesignSize = (
      1185
      40)
    object lblStatus: TLabel
      Left = 12
      Top = 13
      Width = 35
      Height = 13
      Caption = 'Ready.'
    end
    object btnCopyDetails: TButton
      Left = 1053
      Top = 7
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
    Top = 72
    Width = 850
    Height = 478
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
  end
  object memDetails: TMemo
    Left = 855
    Top = 72
    Width = 330
    Height = 478
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
