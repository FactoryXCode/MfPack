object FfrmAdvanced: TFfrmAdvanced
  Left = 0
  Top = 0
  Caption = 'Advanced video options'
  ClientHeight = 204
  ClientWidth = 319
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Position = poMainFormCenter
  DesignSize = (
    319
    204)
  TextHeight = 15
  object Label13: TLabel
    Left = 16
    Top = 136
    Width = 122
    Height = 15
    Hint = 'Values higher than processorcount div 2 have no effect.'
    Caption = 'Resizing threads limit:'
    ParentShowHint = False
    ShowHint = True
  end
  object cbxDisableHardwareTransforms: TCheckBox
    Left = 16
    Top = 18
    Width = 217
    Height = 17
    Caption = 'Disable hardware encoding'
    TabOrder = 0
  end
  object cbxDisableThrottling: TCheckBox
    Left = 16
    Top = 46
    Width = 129
    Height = 17
    Caption = 'Disable throttling'
    TabOrder = 1
  end
  object Button3: TButton
    Left = 102
    Top = 170
    Width = 92
    Height = 26
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 2
    ExplicitTop = 196
  end
  object cbxDisableGOPSize: TCheckBox
    Left = 16
    Top = 104
    Width = 228
    Height = 17
    Caption = 'Disable GOP-size and threads limit'
    TabOrder = 3
  end
  object cbxDisableQualityBasedEncoding: TCheckBox
    Left = 16
    Top = 74
    Width = 195
    Height = 17
    Caption = 'Disable quality based encoding'
    TabOrder = 4
  end
  object spedThreadLimit: TSpinEdit
    Left = 144
    Top = 133
    Width = 63
    Height = 24
    MaxValue = 16
    MinValue = 2
    TabOrder = 5
    Value = 4
  end
end
