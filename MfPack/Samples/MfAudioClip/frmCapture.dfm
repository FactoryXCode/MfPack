object frmCapture: TfrmCapture
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'GPU Desktop Capture Sample'
  ClientHeight = 1200
  ClientWidth = 1950
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 28
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1950
    Height = 300
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblAudioDevice: TLabel
      Left = 1110
      Top = 30
      Width = 118
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Audio device:'
    end
    object lblVU: TLabel
      Left = 1110
      Top = 90
      Width = 102
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Audio level:'
    end
    object lvMonitors: TListView
      Left = 12
      Top = 12
      Width = 630
      Height = 276
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Columns = <>
      HideSelection = False
      LargeImages = ilMonitors
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      OnSelectItem = lvMonitorsSelectItem
    end
    object grpMode: TGroupBox
      Left = 660
      Top = 12
      Width = 420
      Height = 180
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Capture mode'
      TabOrder = 1
      object rbVideoAudio: TRadioButton
        Left = 24
        Top = 36
        Width = 360
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Video + Audio (MP4)'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbModeClick
      end
      object rbVideoOnly: TRadioButton
        Left = 24
        Top = 72
        Width = 360
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Video only (MP4)'
        TabOrder = 1
        OnClick = rbModeClick
      end
      object rbAudioOnly: TRadioButton
        Left = 24
        Top = 108
        Width = 360
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Audio only (WAV)'
        TabOrder = 2
        OnClick = rbModeClick
      end
    end
    object btnPreviewToggle: TButton
      Left = 660
      Top = 210
      Width = 210
      Height = 48
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Preview: ON'
      TabOrder = 2
      OnClick = btnPreviewToggleClick
    end
    object cbAudioDevice: TComboBox
      Left = 1248
      Top = 24
      Width = 480
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      TabOrder = 3
    end
    object pbAudioLevel: TProgressBar
      Left = 1248
      Top = 84
      Width = 480
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Smooth = True
      TabOrder = 4
    end
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 300
    Width = 1950
    Height = 600
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvLowered
    Caption = 'Preview area'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 900
    Width = 1950
    Height = 300
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lblOutput: TLabel
      Left = 12
      Top = 12
      Width = 67
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Output:'
    end
    object edtOutput: TEdit
      Left = 96
      Top = 6
      Width = 720
      Height = 36
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      Text = 'capture_output.mp4'
    end
    object btnBrowse: TButton
      Left = 828
      Top = 6
      Width = 60
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object btnStart: TButton
      Left = 900
      Top = 6
      Width = 150
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Start'
      TabOrder = 2
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 1062
      Top = 6
      Width = 150
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Stop'
      TabOrder = 3
      OnClick = btnStopClick
    end
    object mmoLog: TMemo
      Left = 0
      Top = 72
      Width = 1950
      Height = 228
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alBottom
      ScrollBars = ssVertical
      TabOrder = 4
    end
  end
  object ilMonitors: TImageList
    Height = 90
    Width = 160
    Left = 1107
    Top = 150
  end
  object SaveDialog1: TSaveDialog
    Filter = 'MP4 Video (*.mp4)|*.mp4|WAV Audio (*.wav)|*.wav'
    Options = [ofOverwritePrompt]
    Left = 1221
    Top = 150
  end
end
