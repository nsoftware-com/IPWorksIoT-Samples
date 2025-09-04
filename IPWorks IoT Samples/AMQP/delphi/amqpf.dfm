object FormAMQP: TFormAMQP
  Left = 0
  Top = 0
  Caption = 'AMQP Demo'
  ClientHeight = 569
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 16
    Top = 8
    Width = 439
    Height = 39
    Caption = 
      'This is a demo for the IPWorks IoT AMQP component. It connects' +
      ' to an AMQP server, creates one sender and one receiver link wit' +
      'h the same target, and then allows the user to send and receive ' +
      'messages.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object gConnect: TGroupBox
    Left = 8
    Top = 48
    Width = 449
    Height = 177
    Caption = 'Connection'
    TabOrder = 0
    object Label1: TLabel
      Left = 31
      Top = 28
      Width = 64
      Height = 13
      Alignment = taRightJustify
      Caption = 'Container Id:'
    end
    object Label2: TLabel
      Left = 25
      Top = 52
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = 'Session Name:'
    end
    object Label3: TLabel
      Left = 59
      Top = 76
      Width = 36
      Height = 13
      Alignment = taRightJustify
      Caption = 'Target:'
    end
    object Label4: TLabel
      Left = 27
      Top = 100
      Width = 68
      Height = 13
      Alignment = taRightJustify
      Caption = 'Sender Name:'
    end
    object Label5: TLabel
      Left = 19
      Top = 124
      Width = 76
      Height = 13
      Alignment = taRightJustify
      Caption = 'Receiver Name:'
    end
    object Label6: TLabel
      Left = 237
      Top = 28
      Width = 66
      Height = 13
      Alignment = taRightJustify
      Caption = 'Remote Host:'
    end
    object Label7: TLabel
      Left = 239
      Top = 52
      Width = 64
      Height = 13
      Alignment = taRightJustify
      Caption = 'Remote Port:'
    end
    object Label8: TLabel
      Left = 277
      Top = 76
      Width = 26
      Height = 13
      Alignment = taRightJustify
      Caption = 'User:'
    end
    object Label9: TLabel
      Left = 253
      Top = 100
      Width = 50
      Height = 13
      Alignment = taRightJustify
      Caption = 'Password:'
    end
    object Label11: TLabel
      Left = 24
      Top = 148
      Width = 71
      Height = 13
      Alignment = taRightJustify
      Caption = 'Receive Mode:'
    end
    object tContainer: TEdit
      Left = 104
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'DemoContainer'
    end
    object tSession: TEdit
      Left = 104
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'DemoSession'
    end
    object tTarget: TEdit
      Left = 104
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'DemoTarget'
    end
    object tSender: TEdit
      Left = 104
      Top = 96
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'DemoSenderLink'
    end
    object tReceiver: TEdit
      Left = 104
      Top = 120
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'DemoReceiverLink'
    end
    object tHost: TEdit
      Left = 312
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 7
    end
    object tPort: TEdit
      Left = 312
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 8
      Text = '5672'
    end
    object tUser: TEdit
      Left = 312
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 9
    end
    object tPassword: TEdit
      Left = 312
      Top = 96
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 10
    end
    object bConnect: TButton
      Left = 312
      Top = 144
      Width = 75
      Height = 25
      Caption = '&Connect'
      TabOrder = 12
      OnClick = bConnectClick
    end
    object cSSL: TCheckBox
      Left = 312
      Top = 120
      Width = 97
      Height = 17
      Caption = 'Use SSL'
      TabOrder = 11
      OnClick = cSSLClick
    end
    object rRetrieve: TRadioButton
      Left = 104
      Top = 144
      Width = 57
      Height = 25
      Caption = 'Retrieve'
      TabOrder = 5
    end
    object rAutomatic: TRadioButton
      Left = 160
      Top = 144
      Width = 65
      Height = 25
      Caption = 'Automatic'
      Checked = True
      TabOrder = 6
      TabStop = True
    end
  end
  object gSendReceive: TGroupBox
    Left = 8
    Top = 232
    Width = 449
    Height = 57
    Caption = 'Send and Receive'
    TabOrder = 1
    object Label10: TLabel
      Left = 17
      Top = 28
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = 'Message:'
      Enabled = False
    end
    object tMessage: TEdit
      Left = 72
      Top = 24
      Width = 153
      Height = 21
      Enabled = False
      TabOrder = 0
      Text = 'Hello AMQP!'
    end
    object bSend: TButton
      Left = 296
      Top = 20
      Width = 51
      Height = 25
      Caption = '&Send'
      Enabled = False
      TabOrder = 2
      OnClick = bSendClick
    end
    object bRetrieve: TButton
      Left = 360
      Top = 20
      Width = 83
      Height = 25
      Caption = '&Retrieve Message'
      Enabled = False
      TabOrder = 3
      OnClick = bRetrieveClick
    end
    object cSettled: TCheckBox
      Left = 232
      Top = 26
      Width = 57
      Height = 17
      Caption = 'Settled:'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 1
    end
  end
  object gLog: TGroupBox
    Left = 8
    Top = 296
    Width = 449
    Height = 265
    Caption = 'Log'
    TabOrder = 2
    object mLog: TMemo
      Left = 8
      Top = 16
      Width = 433
      Height = 241
      ReadOnly = True
      TabOrder = 0
    end
  end
  object iotAMQP1: TiotAMQP
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    Timeout = 5
    OnConnectionStatus = iotAMQP1ConnectionStatus
    OnDisconnected = iotAMQP1Disconnected
    OnMessageIn = iotAMQP1MessageIn
    OnMessageOutcome = iotAMQP1MessageOutcome
    OnSSLServerAuthentication = iotAMQP1SSLServerAuthentication
    Left = 256
    Top = 176
  end
end


