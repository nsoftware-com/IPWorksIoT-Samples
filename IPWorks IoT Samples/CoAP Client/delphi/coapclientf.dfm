object FormCoapclient: TFormCoapclient
  Left = 0
  Top = 0
  Caption = 'Coap Client Demo'
  ClientHeight = 638
  ClientWidth = 730
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 697
    Height = 39
    Caption = 
      'This demo shows how to use the CoAP component in client mode. To' +
      ' get started, specify a CoAP server host and port. Then, enter t' +
      'he resource URI path (and, optionally, query string) you wish to' +
      ' make a request against. Finally, click the desired button to se' +
      'nd the request. The "Start Observing" button can be used if you ' +
      'wish to be notified of changes to the resource at the specified ' +
      'URI.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 66
    Width = 161
    Height = 96
    Caption = 'Server Information'
    TabOrder = 0
    object Label2: TLabel
      Left = 3
      Top = 20
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object Label5: TLabel
      Left = 3
      Top = 48
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object etHost: TEdit
      Left = 35
      Top = 16
      Width = 110
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object etPort: TEdit
      Left = 35
      Top = 43
      Width = 110
      Height = 21
      TabOrder = 1
      Text = '5683'
    end
  end
  object GroupBox2: TGroupBox
    Left = 175
    Top = 66
    Width = 547
    Height = 96
    Caption = 'Client Controls'
    TabOrder = 1
    object Label3: TLabel
      Left = 10
      Top = 20
      Width = 22
      Height = 13
      Caption = 'URI:'
    end
    object Label4: TLabel
      Left = 10
      Top = 48
      Width = 156
      Height = 13
      Caption = 'Request Body (POST/PUT Only):'
    end
    object etURI: TEdit
      Left = 38
      Top = 16
      Width = 506
      Height = 21
      TabOrder = 0
      Text = 'animals/domestic/cat'
    end
    object etRequestBody: TEdit
      Left = 172
      Top = 43
      Width = 372
      Height = 21
      TabOrder = 1
    end
    object bGet: TButton
      Left = 3
      Top = 67
      Width = 75
      Height = 25
      Caption = 'GET'
      TabOrder = 2
      OnClick = bGetClick
    end
    object bPost: TButton
      Left = 84
      Top = 67
      Width = 75
      Height = 25
      Caption = 'POST'
      TabOrder = 3
      OnClick = bPostClick
    end
    object bPut: TButton
      Left = 165
      Top = 67
      Width = 75
      Height = 25
      Caption = 'PUT'
      TabOrder = 4
      OnClick = bPutClick
    end
    object bDelete: TButton
      Left = 246
      Top = 67
      Width = 75
      Height = 25
      Caption = 'DELETE'
      TabOrder = 5
      OnClick = bDeleteClick
    end
    object bStopObserving: TButton
      Left = 456
      Top = 67
      Width = 88
      Height = 25
      Caption = 'Stop Observing'
      TabOrder = 6
      OnClick = bStopObservingClick
    end
    object bStartObservation: TButton
      Left = 362
      Top = 67
      Width = 88
      Height = 25
      Caption = 'Start Observing'
      TabOrder = 7
      OnClick = bStartObservationClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 168
    Width = 714
    Height = 467
    Caption = 'Log'
    TabOrder = 2
    object ListBox1: TListBox
      Left = 3
      Top = 16
      Width = 708
      Height = 448
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object iotCoAP1: TiotCoAP
    OnError = iotCoAP1Error
    OnLog = iotCoAP1Log
    OnNotification = iotCoAP1Notification
    OnRequestComplete = iotCoAP1RequestComplete
    Left = 688
    Top = 40
  end
end


