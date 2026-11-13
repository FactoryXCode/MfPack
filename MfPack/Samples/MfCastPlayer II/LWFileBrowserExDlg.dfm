object LWFileBrowserExDlg: TLWFileBrowserExDlg
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Select an audio file'
  ClientHeight = 640
  ClientWidth = 940
  Font.Charset = DEFAULT_CHARSET
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 17
  Color = clBtnFace
  Font.Color = clWindowText
  object Splitter1: TSplitter
    Left = 270
    Top = 111
    Width = 6
    Height = 450
    ExplicitTop = 86
    ExplicitHeight = 475
    Color = clBtnFace
    ParentColor = True
  end
  object SplitterPreview: TSplitter
    Left = 704
    Top = 111
    Width = 6
    Height = 450
    Align = alRight
    ExplicitTop = 86
    ExplicitHeight = 475
    Color = clBtnFace
    ParentColor = True
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 940
    Height = 111
    Align = alTop
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    Color = clBtnFace
    ParentBackground = True
    object lblLocation: TLabel
      Left = 10
      Top = 7
      Width = 53
      Height = 17
      Caption = 'Location'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Font.Color = clWindowText
    end
    object lblIPv4Address: TLabel
      Left = 716
      Top = 7
      Width = 77
      Height = 17
      Caption = 'IPv4 address'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Font.Color = clWindowText
    end
    object cbxLocations: TComboBox
      Left = 4
      Top = 28
      Width = 266
      Height = 25
      Style = csOwnerDrawFixed
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 19
      ParentFont = False
      TabOrder = 0
      OnDrawItem = cbxLocationsDrawItem
      OnSelect = cbxLocationsSelect
      Color = clWindow
      Font.Color = clWindowText
    end
    object edtPath: TEdit
      Left = 290
      Top = 28
      Width = 337
      Height = 25
      Hint = 'Find a local map or network station. (like: \\MyServer)'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnKeyDown = edtPathKeyDown
      Color = clWindow
      Font.Color = clWindowText
    end
    object btnGo: TButton
      Left = 636
      Top = 20
      Width = 68
      Height = 43
      Caption = 'Find'
      OnClick = btnGoClick
      ParentFont = True
      WordWrap = True
    end
    object edtIPv4Address: TEdit
      Left = 716
      Top = 28
      Width = 214
      Height = 25
      Hint = 
        'IPv4 address of the computer for the selected location. Select t' +
        'he text to copy it.'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 2
      Color = clWindow
      Font.Color = clWindowText
    end
    object cbxFileFilter: TFilterComboBox
      Left = 4
      Top = 80
      Width = 703
      Height = 25
      FileList = flbFiles
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Color = clWindow
      Font.Color = clWindowText
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 561
    Width = 940
    Height = 79
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    Color = clBtnFace
    ParentBackground = True
    object lblSelectedFile: TLabel
      Left = 14
      Top = 15
      Width = 79
      Height = 17
      Caption = 'Selected file:'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Font.Color = clWindowText
    end
    object lblDuration: TLabel
      Left = 14
      Top = 44
      Width = 113
      Height = 17
      Caption = 'Duration: 00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Font.Color = clWindowText
    end
    object btnOk: TButton
      Left = 708
      Top = 31
      Width = 107
      Height = 40
      Caption = 'Ok'
      Enabled = False
      OnClick = btnOkClick
      ParentFont = True
      WordWrap = True
      Default = True
    end
    object btnCancel: TButton
      Left = 821
      Top = 31
      Width = 107
      Height = 40
      Caption = 'Cancel'
      OnClick = btnCancelClick
      ParentFont = True
      WordWrap = True
      Cancel = True
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 111
    Width = 270
    Height = 450
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    Color = clBtnFace
    ParentBackground = True
    object lbFolders: TListBox
      Left = 0
      Top = 0
      Width = 270
      Height = 450
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemHeight = 21
      ParentFont = False
      TabOrder = 0
      OnDblClick = lbFoldersDblClick
      OnDrawItem = lbFoldersDrawItem
      Color = clWindow
      Font.Color = clWindowText
    end
  end
  object flbFiles: TFileListBox
    Left = 276
    Top = 111
    Width = 428
    Height = 450
    Align = alClient
    ExtendedSelect = False
    Font.Charset = DEFAULT_CHARSET
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ItemHeight = 17
    ParentFont = False
    ShowGlyphs = True
    TabOrder = 3
    OnChange = flbFilesChange
    OnDblClick = flbFilesDblClick
    Color = clWindow
    Font.Color = clWindowText
    ParentColor = False
  end
  object pnlPreview: TPanel
    Left = 710
    Top = 111
    Width = 230
    Height = 450
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
    Color = clBtnFace
    ParentBackground = True
    object lblPreview: TLabel
      Left = 0
      Top = 0
      Width = 230
      Height = 17
      Align = alTop
      Alignment = taCenter
      Caption = 'Preview'
      Font.Charset = DEFAULT_CHARSET
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 48
      Font.Color = clWindowText
    end
    object imgPreview: TImage
      Left = 0
      Top = 17
      Width = 230
      Height = 433
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
      ExplicitTop = 23
      ExplicitHeight = 429
    end
  end
end
