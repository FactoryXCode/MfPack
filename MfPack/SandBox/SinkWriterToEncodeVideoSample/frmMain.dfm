object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 180
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 0
    Top = 94
    Width = 431
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Click '#39'Execute'#39' to run the sample..'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 452
    Height = 17
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Choose the output format'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 6
    ExplicitWidth = 431
  end
  object butExecute: TButton
    Left = 164
    Top = 128
    Width = 95
    Height = 31
    Caption = 'Execute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = butExecuteClick
  end
  object cbxOutputFormat: TComboBox
    Left = 152
    Top = 38
    Width = 147
    Height = 21
    TabOrder = 1
    Text = 'MP4'
    OnCloseUp = cbxOutputFormatCloseUp
    Items.Strings = (
      'MP4'
      'WMF'
      'AVI')
  end
end
