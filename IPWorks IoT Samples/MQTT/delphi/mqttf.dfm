object FormMQTT: TFormMQTT
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'MQTT Demo'
  ClientHeight = 329
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label13: TLabel
    Left = 16
    Top = 8
    Width = 559
    Height = 13
    Caption = 
      'An example to demonstrate subscribing to topics and publishing m' +
      'essages with the IPWorks IoT MQTT component.'
  end
  object tConnectBox: TGroupBox
    Left = 8
    Top = 24
    Width = 593
    Height = 97
    Caption = 'Connection Settings'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 26
      Height = 13
      Alignment = taRightJustify
      Caption = 'Host:'
    end
    object Label2: TLabel
      Left = 18
      Top = 40
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Port:'
    end
    object Label3: TLabel
      Left = 224
      Top = 16
      Width = 26
      Height = 13
      Alignment = taRightJustify
      Caption = 'User:'
    end
    object Label4: TLabel
      Left = 200
      Top = 40
      Width = 50
      Height = 13
      Alignment = taRightJustify
      Caption = 'Password:'
    end
    object Label12: TLabel
      Left = 413
      Top = 16
      Width = 45
      Height = 13
      Alignment = taRightJustify
      Caption = 'Client ID:'
    end
    object tHost: TEdit
      Left = 48
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'test.mosquitto.org'
    end
    object tPort: TEdit
      Left = 48
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '1883'
    end
    object tUser: TEdit
      Left = 256
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object tPassword: TEdit
      Left = 256
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object tConnectButton: TButton
      Left = 47
      Top = 64
      Width = 65
      Height = 25
      Caption = '&Connect'
      TabOrder = 7
      OnClick = tConnectButtonClick
    end
    object tClientID: TEdit
      Left = 464
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 4
    end
    object tSSL: TCheckBox
      Left = 522
      Top = 40
      Width = 63
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Use SSL:'
      TabOrder = 6
      OnClick = tSSLClick
    end
    object tCleanSession: TCheckBox
      Left = 387
      Top = 40
      Width = 90
      Height = 17
      Alignment = taLeftJustify
      BiDiMode = bdLeftToRight
      Caption = 'Clean Session:'
      Checked = True
      ParentBiDiMode = False
      State = cbChecked
      TabOrder = 5
    end
  end
  object tSubBox: TGroupBox
    Left = 8
    Top = 128
    Width = 297
    Height = 81
    Caption = 'Subscription Settings'
    Enabled = False
    TabOrder = 1
    object Label7: TLabel
      Left = 28
      Top = 24
      Width = 29
      Height = 13
      Alignment = taRightJustify
      Caption = 'Topic:'
      Enabled = False
    end
    object Label8: TLabel
      Left = 33
      Top = 48
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'QoS:'
      Enabled = False
    end
    object tSubscribeTopic: TEdit
      Left = 64
      Top = 24
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 0
      Text = 'nsoftwaretest'
    end
    object tSubscribeQoS: TComboBox
      Left = 64
      Top = 48
      Width = 121
      Height = 21
      Enabled = False
      ItemIndex = 0
      TabOrder = 1
      Text = '0 - At most once'
      Items.Strings = (
        '0 - At most once'
        '1 - At least once'
        '2 - Exactly once')
    end
    object tSubscribeButton: TButton
      Left = 206
      Top = 20
      Width = 75
      Height = 25
      Caption = '&Subscribe'
      Enabled = False
      TabOrder = 2
      OnClick = tSubscribeButtonClick
    end
    object tUnsubscribeButton: TButton
      Left = 206
      Top = 48
      Width = 75
      Height = 25
      Caption = '&Unsubscribe'
      Enabled = False
      TabOrder = 3
      OnClick = tUnsubscribeButtonClick
    end
  end
  object tPubBox: TGroupBox
    Left = 8
    Top = 216
    Width = 297
    Height = 105
    Caption = 'Publish Settings'
    Enabled = False
    TabOrder = 2
    object Label9: TLabel
      Left = 28
      Top = 24
      Width = 29
      Height = 13
      Alignment = taRightJustify
      Caption = 'Topic:'
      Enabled = False
    end
    object Label10: TLabel
      Left = 33
      Top = 48
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'QoS:'
      Enabled = False
    end
    object Label11: TLabel
      Left = 11
      Top = 72
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = 'Message:'
      Enabled = False
    end
    object tPublishTopic: TEdit
      Left = 64
      Top = 24
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 0
      Text = 'nsoftwaretest'
    end
    object tPublishButton: TButton
      Left = 206
      Top = 72
      Width = 75
      Height = 25
      Caption = '&Publish'
      Enabled = False
      TabOrder = 3
      OnClick = tPublishButtonClick
    end
    object tPublishMessage: TEdit
      Left = 64
      Top = 72
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 2
      Text = 'Hello MQTT!'
    end
    object tPublishQoS: TComboBox
      Left = 64
      Top = 48
      Width = 121
      Height = 21
      Enabled = False
      ItemIndex = 0
      TabOrder = 1
      Text = '0 - At most once'
      Items.Strings = (
        '0 - At most once'
        '1 - At least once'
        '2 - Exactly once')
    end
  end
  object tLogBox: TGroupBox
    Left = 312
    Top = 128
    Width = 289
    Height = 193
    Caption = 'Log'
    Enabled = False
    TabOrder = 3
    object tLog: TMemo
      Left = 8
      Top = 16
      Width = 273
      Height = 169
      TabOrder = 0
    end
  end
  object iotMQTT1: TiotMQTT
    SSLCertStore = 'MY'
    OnConnected = iotMQTT1Connected
    OnConnectionStatus = iotMQTT1ConnectionStatus
    OnDisconnected = iotMQTT1Disconnected
    OnError = iotMQTT1Error
    OnMessageIn = iotMQTT1MessageIn
    OnSSLServerAuthentication = iotMQTT1SSLServerAuthentication
    OnSubscribed = iotMQTT1Subscribed
    OnUnsubscribed = iotMQTT1Unsubscribed
    Left = 240
    Top = 232
  end
end


