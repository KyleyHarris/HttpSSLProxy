object frameProxy: TframeProxy
  Left = 0
  Top = 0
  Width = 692
  Height = 531
  TabOrder = 0
  DesignSize = (
    692
    531)
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
    Left = 248
    Top = 73
    Width = 75
    Height = 25
    Action = actGo
    TabOrder = 4
  end
  object Memo1: TMemo
    Left = 16
    Top = 104
    Width = 657
    Height = 409
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 5
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 336
    Top = 248
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
