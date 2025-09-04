object FormCoapserver: TFormCoapserver
  Left = 0
  Top = 0
  Caption = 'Coap Server Demo'
  ClientHeight = 638
  ClientWidth = 730
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 714
    Height = 52
    Caption = 
      'This demo shows how to use the CoAP component in server mode. To' +
      ' get started, choose a listening port, then click'#31' Start Listeni' +
      'ng. CoAP clients will then be able to make GET requests (and obs' +
      'erve) any node in the tree. Clients can also send POST/PUT reque' +
      'sts to create or update nodes, and DELETE requests to delete nod' +
      'es. However, the root node cannot be deleted.'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 66
    Width = 306
    Height = 74
    Caption = 'Server Controls'
    TabOrder = 0
    object Label2: TLabel
      Left = 10
      Top = 20
      Width = 55
      Height = 13
      Caption = 'Listen Port:'
    end
    object Edit1: TEdit
      Left = 71
      Top = 16
      Width = 232
      Height = 21
      TabOrder = 0
      Text = '5683'
    end
    object Button1: TButton
      Left = 10
      Top = 43
      Width = 293
      Height = 21
      Caption = 'Start Listening'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 320
    Top = 66
    Width = 402
    Height = 74
    Caption = 'Node Controls'
    TabOrder = 1
    object Label3: TLabel
      Left = 10
      Top = 20
      Width = 54
      Height = 13
      Caption = 'New Child: '
    end
    object Label4: TLabel
      Left = 10
      Top = 48
      Width = 58
      Height = 13
      Caption = 'Node Data: '
    end
    object etNewChild: TEdit
      Left = 70
      Top = 16
      Width = 167
      Height = 21
      TabOrder = 0
      Text = 'etNewChild'
    end
    object etNodeData: TEdit
      Left = 70
      Top = 43
      Width = 248
      Height = 21
      TabOrder = 1
      Text = 'etNodeData'
    end
    object bDeleteNode: TButton
      Left = 324
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Delete Node'
      TabOrder = 2
      OnClick = bDeleteNodeClick
    end
    object bAddNode: TButton
      Left = 243
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Add Node'
      TabOrder = 3
      OnClick = bAddNodeClick
    end
    object bSave: TButton
      Left = 324
      Top = 43
      Width = 75
      Height = 21
      Caption = 'Save'
      TabOrder = 4
      OnClick = bSaveClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 146
    Width = 714
    Height = 343
    Caption = 
      'Data Nodes (use '#39'/'#39' between parents and children when sending re' +
      'quests)'
    TabOrder = 2
    object TreeView1: TTreeView
      Left = 3
      Top = 16
      Width = 708
      Height = 313
      Indent = 19
      TabOrder = 0
      OnClick = TreeView1Click
      Items.NodeData = {
        03010000002A0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        000100000001063C0072006F006F0074003E002C0000000000000000000000FF
        FFFFFFFFFFFFFF000000000000000003000000010761006E0069006D0061006C
        0073002E0000000000000000000000FFFFFFFFFFFFFFFF000000000000000003
        000000010864006F006D00650073007400690063002400000000000000000000
        00FFFFFFFFFFFFFFFF000000000000000000000000010364006F006700240000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000010363
        00610074002E0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010870006100720061006B00650065007400260000000000000000
        000000FFFFFFFFFFFFFFFF00000000000000000300000001046600610072006D
        00240000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        00010363006F007700240000000000000000000000FFFFFFFFFFFFFFFF000000
        0000000000000000000103700069006700280000000000000000000000FFFFFF
        FFFFFFFFFF000000000000000000000000010573006800650065007000260000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000003000000010477
        0069006C006400260000000000000000000000FFFFFFFFFFFFFFFF0000000000
        0000000000000001046C0069006F006E00280000000000000000000000FFFFFF
        FFFFFFFFFF000000000000000000000000010574006900670065007200260000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000010462
        00650061007200}
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 495
    Width = 714
    Height = 140
    Caption = 'Log'
    TabOrder = 3
    object ListBox1: TListBox
      Left = 3
      Top = 16
      Width = 708
      Height = 113
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object iotCoAP1: TiotCoAP
    OnError = iotCoAP1Error
    OnLog = iotCoAP1Log
    OnRegister = iotCoAP1Register
    OnRequest = iotCoAP1Request
    OnResponseComplete = iotCoAP1ResponseComplete
    OnUnregistered = iotCoAP1Unregistered
    Left = 512
    Top = 64
  end
end




