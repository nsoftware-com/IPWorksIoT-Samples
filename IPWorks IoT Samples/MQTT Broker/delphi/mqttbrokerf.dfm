object FormMQTTBroker: TFormMQTTBroker
  Left = 0
  Top = 0
  Caption = 'FormMQTTBroker'
  ClientHeight = 462
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 457
    Height = 105
    Caption = 'Start'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 25
      Height = 15
      Caption = 'Port:'
    end
    object Label2: TLabel
      Left = 218
      Top = 24
      Width = 82
      Height = 15
      Caption = 'Cert Store Path:'
    end
    object Label3: TLabel
      Left = 247
      Top = 68
      Width = 53
      Height = 15
      Caption = 'Password:'
    end
    object txtPort: TEdit
      Left = 47
      Top = 21
      Width = 50
      Height = 23
      TabOrder = 0
    end
    object txtCert: TEdit
      Left = 306
      Top = 21
      Width = 137
      Height = 23
      TabOrder = 1
    end
    object btnStart: TButton
      Left = 16
      Top = 64
      Width = 81
      Height = 25
      Caption = 'Start'
      TabOrder = 2
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 112
      Top = 64
      Width = 81
      Height = 25
      Caption = 'Stop'
      TabOrder = 3
      OnClick = btnStopClick
    end
    object txtPass: TEdit
      Left = 306
      Top = 65
      Width = 137
      Height = 23
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 392
    Width = 457
    Height = 57
    Caption = 'Actions'
    TabOrder = 1
    object btnDisconnect: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 0
      OnClick = btnDisconnectClick
    end
    object cbConnList: TComboBox
      Left = 112
      Top = 24
      Width = 145
      Height = 23
      TabOrder = 1
    end
  end
  object lbLog: TListBox
    Left = 8
    Top = 128
    Width = 457
    Height = 258
    ItemHeight = 15
    TabOrder = 2
  end
  object iotMQTTBroker1: TiotMQTTBroker
    OnConnected = OnConnected
    OnConnectionRequest = OnConnectionRequest
    OnDisconnected = OnDisconnected
    OnMessageReceived = OnMsgRecived
    OnMessageSent = OnMsgSent
    OnSessionRemoved = OnSessionRemoved
    OnSessionRequest = OnSessionRequest
    OnSSLClientAuthentication = OnSSLAuth
    OnSubscribe = OnSubscribe
    OnUnsubscribe = OnUnsubscribe
    Left = 288
    Top = 288
  end
end


