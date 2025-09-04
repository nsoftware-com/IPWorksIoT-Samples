(*
 * IPWorks IoT 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks IoT in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksiot
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit mqttbrokerf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, iotcore,
  iottypes, System.Threading, iotmqttbroker;

type
  TFormMQTTBroker = class(TForm)
    iotMQTTBroker1: TiotMQTTBroker;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    txtPort: TEdit;
    Label2: TLabel;
    txtCert: TEdit;
    btnStart: TButton;
    btnStop: TButton;
    GroupBox2: TGroupBox;
    btnDisconnect: TButton;
    Label3: TLabel;
    txtPass: TEdit;
    cbConnList: TComboBox;
    lbLog: TListBox;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure OnConnected(Sender: TObject; ConnectionId, StatusCode: Integer;
      const Description: string);
    procedure OnConnectionRequest(Sender: TObject; const Address: string;
      Port: Integer; var Accept: Boolean);
    procedure OnDisconnected(Sender: TObject; ConnectionId, StatusCode: Integer;
      const Description: string);
    procedure OnSessionRequest(Sender: TObject; ConnectionId: Integer;
      const ClientId, UserName, Password: string; IsNew: Boolean;
      var Accept: Boolean);
    procedure OnSessionRemoved(Sender: TObject; const ClientId: string);
    procedure OnSubscribe(Sender: TObject; ConnectionId: Integer;
      const ClientId, TopicFilter: string; RequestedQoS: Integer;
      var ReturnCode: Integer);
    procedure OnUnsubscribe(Sender: TObject; ConnectionId: Integer;
      const ClientId, TopicFilter: string);
    procedure OnSSLAuth(Sender: TObject; ConnectionId: Integer;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure OnMsgRecived(Sender: TObject; ConnectionId: Integer;
      const ClientId: string; MessageId: Int64);
    procedure OnMsgSent(Sender: TObject; ConnectionId: Integer;
      const ClientId: string; MessageId: Int64);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMQTTBroker: TFormMQTTBroker;
  listenersAdded: bool;

implementation

{$R *.dfm}

procedure TFormMQTTBroker.btnDisconnectClick(Sender: TObject);
begin
  try
    iotMQTTBroker1.Disconnect(Integer.Parse(cbConnList.Text));
  except on ex: Exception do
  begin
    lbLog.Items.Add('Error: ' + ex.Message);
  end;
  end;
end;

procedure TFormMQTTBroker.btnStartClick(Sender: TObject);
var
  port: Integer;
  task: ITask;
begin
  if Length(txtPort.Text) <> 0 then
    port := Integer.Parse(txtPort.Text)
  else if Length(txtCert.Text) <> 0 then
    port := 8883
  else
    port := 1883;
  
  try
    if not listenersAdded then
    begin
      try
        if (Length(txtCert.Text) <> 0) and (Length(txtPass.Text) <> 0) then
          iotMQTTBroker1.AddBrokerListener('127.0.0.1', port, 1, 99, TEncoding.UTF8.GetBytes(txtCert.Text), txtPass.Text, '*')
        else
          iotMQTTBroker1.AddBrokerListener('127.0.0.1', port, 0, 0, TEncoding.UTF8.GetBytes('') , '' , '');
      except on ex: Exception do
      begin
        lbLog.Items.Add('Error: ' + ex.Message);
        Exit;
      end;
      end;
    end;

   task := TTask.Create(
   procedure
   begin
     while True do iotMQTTBroker1.DoEvents;
   end);

    iotMQTTBroker1.StartListening;
    task.Start;

    lbLog.Items.Add('MQTT Broker started on port ' + port.ToString);
  except on ex: Exception do
  begin
    lbLog.Items.Add('Error: ' + ex.Message);

    iotMQTTBroker1.StopListening;
  end;
  end;

  listenersAdded := true;
end;

procedure TFormMQTTBroker.btnStopClick(Sender: TObject);
begin
  if iotMQTTBroker1.ListenerCount > 0 then
  begin
    iotMQTTBroker1.Shutdown;
    lbLog.Items.Add('Broker is Shutdown!');
  end
  else
  begin
    lbLog.Items.Add('No Listeners Active!');
  end;
end;

procedure TFormMQTTBroker.OnConnected(Sender: TObject; ConnectionId, StatusCode: Integer;
  const Description: string);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has connected with status: ' + StatusCode.ToString + '/' + Description);
  cbConnList.Items.Add(ConnectionId.ToString);
end;

procedure TFormMQTTBroker.OnConnectionRequest(Sender: TObject; const Address: string;
  Port: Integer; var Accept: Boolean);
begin
  lbLog.Items.Add('Client [' + Address + ':' + Port.ToString + '] has requested a connection!');
  //here you can decide if you want to establish a connection with the client
  Accept := true;
end;

procedure TFormMQTTBroker.OnDisconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has disconnected with status: ' + StatusCode.ToString + '/' + Description);
  cbConnList.Items.Delete(cbConnList.Items.IndexOf(ConnectionId.ToString));
end;

procedure TFormMQTTBroker.OnMsgRecived(Sender: TObject; ConnectionId: Integer;
  const ClientId: string; MessageId: Int64);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has sent a message!');
end;

procedure TFormMQTTBroker.OnMsgSent(Sender: TObject; ConnectionId: Integer;
  const ClientId: string; MessageId: Int64);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has recived a message!');
end;

procedure TFormMQTTBroker.OnSessionRemoved(Sender: TObject; const ClientId: string);
begin
  lbLog.Items.Add('Client with ID ' + ClientId + ' has removed the session!');
end;

procedure TFormMQTTBroker.OnSessionRequest(Sender: TObject; ConnectionId: Integer;
  const ClientId, UserName, Password: string; IsNew: Boolean;
  var Accept: Boolean);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has requested a session!');
  lbLog.Items.Add('Client ID is: ' + ClientId);
  //here you can decide if you want to grant the client a session
  Accept := true;
end;

procedure TFormMQTTBroker.OnSSLAuth(Sender: TObject; ConnectionId: Integer;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  //here you can decide if you want to authenticate the client via TLS/SSL
  Accept := true;
end;

procedure TFormMQTTBroker.OnSubscribe(Sender: TObject; ConnectionId: Integer;
  const ClientId, TopicFilter: string; RequestedQoS: Integer;
  var ReturnCode: Integer);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has subscribed to topic: ' + TopicFilter + '/QoS: ' + RequestedQoS.ToString);
end;

procedure TFormMQTTBroker.OnUnsubscribe(Sender: TObject; ConnectionId: Integer;
  const ClientId, TopicFilter: string);
begin
  lbLog.Items.Add('Client [' + ConnectionId.ToString + '] has unsubscribed to the ' + TopicFilter + 'topic!');
end;

end.

