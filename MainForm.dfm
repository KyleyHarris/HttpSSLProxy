object ProxyMainForm: TProxyMainForm
  Left = 0
  Top = 0
  Caption = 'SSL Proxy'
  ClientHeight = 570
  ClientWidth = 1238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcProxy: TPageControl
    Left = 0
    Top = 0
    Width = 912
    Height = 570
    Align = alClient
    TabOrder = 0
    OnChange = pcProxyChange
    ExplicitWidth = 592
  end
  object Panel1: TPanel
    Left = 912
    Top = 0
    Width = 326
    Height = 570
    Align = alRight
    TabOrder = 1
    ExplicitLeft = 592
    object Label1: TLabel
      Left = 24
      Top = 73
      Width = 110
      Height = 13
      Caption = 'Rename Current Proxy'
    end
    object Button1: TButton
      Left = 24
      Top = 8
      Width = 137
      Height = 25
      Action = actStartAll
      TabOrder = 0
    end
    object Button2: TButton
      Left = 167
      Top = 8
      Width = 137
      Height = 25
      Action = actStopAll
      TabOrder = 1
    end
    object Button3: TButton
      Left = 24
      Top = 39
      Width = 137
      Height = 25
      Action = actDelete
      TabOrder = 2
    end
    object Button4: TButton
      Left = 167
      Top = 39
      Width = 137
      Height = 25
      Action = actAdd
      TabOrder = 3
    end
    object edtName: TEdit
      Left = 167
      Top = 70
      Width = 138
      Height = 21
      TabOrder = 4
      OnChange = edtNameChange
    end
    object Button5: TButton
      Left = 24
      Top = 112
      Width = 137
      Height = 25
      Action = actSave
      TabOrder = 5
    end
  end
  object ActionList1: TActionList
    Left = 448
    Top = 288
    object actStartAll: TAction
      Caption = 'Start All'
      OnExecute = actStartAllExecute
    end
    object actStopAll: TAction
      Caption = 'Stop All'
      OnExecute = actStopAllExecute
    end
    object actDelete: TAction
      Caption = 'Delete Current'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actAdd: TAction
      Caption = 'Add Proxy'
      OnExecute = actAddExecute
    end
    object actSave: TAction
      Caption = 'Save Config'
      OnExecute = actSaveExecute
    end
  end
end
