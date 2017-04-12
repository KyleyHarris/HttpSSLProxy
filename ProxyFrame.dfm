object frameProxy: TframeProxy
  Left = 0
  Top = 0
  Width = 360
  Height = 165
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 162
    Height = 13
    Caption = 'Connection Point On This Machine'
  end
  object Label2: TLabel
    Left = 16
    Top = 58
    Width = 323
    Height = 13
    Caption = 
      'Data Server (HTTP ONLY, Normally on Local Host, but not required' +
      ')'
  end
  object incomingHost: TEdit
    Left = 16
    Top = 27
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 0
    Text = 'localhost'
  end
  object incomingPort: TEdit
    Left = 144
    Top = 27
    Width = 81
    Height = 21
    TabOrder = 1
    Text = '55111'
  end
  object outgoingHost: TEdit
    Left = 16
    Top = 77
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'localhost'
  end
  object outgoingPort: TEdit
    Left = 144
    Top = 77
    Width = 81
    Height = 21
    TabOrder = 3
    Text = '55111'
  end
  object btnGo: TButton
    Left = 16
    Top = 104
    Width = 75
    Height = 25
    Action = actGo
    TabOrder = 4
  end
  object cbRemoveCompression: TCheckBox
    Left = 16
    Top = 135
    Width = 255
    Height = 17
    Caption = 'Intercept and Remove Compression Request'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = cbRemoveCompressionClick
  end
  object Actions: TActionList
    Left = 264
    Top = 184
    object actGo: TAction
      Caption = 'Go'
      OnExecute = actGoExecute
      OnUpdate = actGoUpdate
    end
  end
end
