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
unit amqpf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, iotcore, iottypes, iotamqp;

type
  TFormAMQP = class(TForm)
    gConnect: TGroupBox;
    tContainer: TEdit;
    tSession: TEdit;
    tTarget: TEdit;
    tSender: TEdit;
    tReceiver: TEdit;
    tHost: TEdit;
    tPort: TEdit;
    tUser: TEdit;
    tPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    gSendReceive: TGroupBox;
    tMessage: TEdit;
    bConnect: TButton;
    bSend: TButton;
    bRetrieve: TButton;
    cSSL: TCheckBox;
    rRetrieve: TRadioButton;
    rAutomatic: TRadioButton;
    Label11: TLabel;
    cSettled: TCheckBox;
    Label10: TLabel;
    gLog: TGroupBox;
    mLog: TMemo;
    Label12: TLabel;
    iotAMQP1: TiotAMQP;
    procedure bConnectClick(Sender: TObject);
    procedure Connect();
    procedure ResetForm();
    procedure Log(s: String);
    procedure SetGroupEnabled(Group: TGroupBox; Enabled: Boolean);
    procedure iotAMQP1ConnectionStatus(Sender: TObject;
      const ConnectionEvent: string; StatusCode: Integer;
      const Description: string);
    procedure iotAMQP1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: string);
    procedure iotAMQP1MessageIn(Sender: TObject; const SessionName: string;
      SessionIndex: Integer; const LinkName: string; LinkIndex: Integer;
      const MessageId: string; var State: Integer);
    procedure bSendClick(Sender: TObject);
    procedure bRetrieveClick(Sender: TObject);
    procedure iotAMQP1MessageOutcome(Sender: TObject; const SessionName: string;
      SessionIndex: Integer; const LinkName: string; LinkIndex: Integer;
      const MessageId: string; Direction, State: Integer);
    procedure cSSLClick(Sender: TObject);
    procedure iotAMQP1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAMQP: TFormAMQP;

implementation

{$R *.dfm}

// Connect or disconnect
procedure TFormAMQP.bConnectClick(Sender: TObject);
begin
  if iotAMQP1.Connected then
    begin
      iotAMQP1.Disconnect();
    end
  else
    begin
      Connect();
    end;
end;

// Retrieve a message
procedure TFormAMQP.bRetrieveClick(Sender: TObject);
begin
  iotAMQP1.RetrieveTimeout := 5;
  Log('Retrieveing message...');
  try
    iotAMQP1.RetrieveMessage(tReceiver.Text)
  except
    on E: Exception do
    begin
      Log(E.Message)
    end;
  end;
end;

// Build and send the current message
procedure TFormAMQP.bSendClick(Sender: TObject);
begin
  iotAMQP1.ResetMessage();
  iotAMQP1.MessageValueType := TiotamqpMessageValueTypes.mvtString;
  iotAMQP1.MessageValue := tMessage.Text;
  iotAMQP1.MessageSettled := cSettled.Checked;
  Log('Sending message...');
  iotAMQP1.SendMessage(tSender.Text);
  if iotAMQP1.MessageSettled then
    Log('Message sent.');
end;

procedure TFormAMQP.Connect();
begin
  // Make sure everything is reset after an error
  iotAMQP1.Reset();
  iotAMQP1.Timeout := 10;
  // Make sure host and port are present
  if tHost.Text = '' then
    begin
      Log('Remote Host is required to connect.');
      Exit;
    end;
  if tPort.Text = '' then
    begin
      Log('Remote Port is required to connect.');
      Exit;
    end;
  // Detect authentication info and set Authscheme if present
  if (tUser.Text <> '') AND (tPassword.Text <> '') then
    begin
      iotAMQP1.User := tUser.Text;
      iotAMQP1.Password := tPassword.Text;
      iotAMQP1.AuthScheme := TiotamqpAuthSchemes.smSASLPlain;
      Log('Logging in as ' + iotAMQP1.User);
    end
  // Make sure that User and Password are both set if either is set
  else if (tUser.Text <> '') OR (tPassword.Text <> '') then
    begin
      Log('If User or Password is set, both must be set');
      Exit;
    end;
  iotAMQP1.ContainerId := tContainer.Text;
  iotAMQP1.SSLEnabled := cSSL.Checked;
  // Disable connection controls while connected
  SetGroupEnabled(gConnect, False);
  // Connect and set up links
  try
    iotAMQP1.ConnectTo(tHost.Text, StrToInt(tPort.Text));
    // Connection Successful, do post-connection setup
    iotAMQP1.CreateSession(tSession.Text);
    Log('Created session: ' + tSession.Text);
    // Create sender link
    iotAMQP1.CreateSenderLink(tSession.Text, tSender.Text, tTarget.Text);
    Log('Created sender link: ' + tSender.Text);
    // Create receiver link
    if rRetrieve.Checked then
      iotAMQP1.ReceiveMode := TiotamqpReceiveModes.rmRetrieve;
    iotAMQP1.CreateReceiverLink(tSession.Text, tReceiver.Text, tTarget.Text);
    Log('Created receiver link: ' + tReceiver.Text);
    // Enable form components that work while connected
    bConnect.Caption := '&Disconnect';
    bConnect.Enabled := True;
    SetGroupEnabled(gSendReceive, True);
    bRetrieve.Enabled := rRetrieve.Checked;
  except
    on E: Exception do
    begin
      Log(E.Message);
      ResetForm();
    end;
  end;
end;

// Automatically adjust port when SSLEnabled changes
procedure TFormAMQP.cSSLClick(Sender: TObject);
begin
  if cSSL.Checked and (tPort.Text = '5672') then
    begin
      tPort.Text := '5671';
    end
  else if (not cSSL.Checked) and (tPort.Text = '5671') then
    begin
      tPort.Text := '5672';
    end;
end;

// Log changes in connection status
procedure TFormAMQP.iotAMQP1ConnectionStatus(Sender: TObject;
  const ConnectionEvent: string; StatusCode: Integer;
  const Description: string);
begin
  Log('Connection Status: ' + ConnectionEvent);
end;

// Reset the form in the event of an unexpected disconnection
procedure TFormAMQP.iotAMQP1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: string);
begin
  ResetForm();
end;

// Log received messages
procedure TFormAMQP.iotAMQP1MessageIn(Sender: TObject; const SessionName: string;
  SessionIndex: Integer; const LinkName: string; LinkIndex: Integer;
  const MessageId: string; var State: Integer);
begin
  Log('Message received: ' + iotAMQP1.ReceivedMessageValue);
end;

// Log the outcome of sent messages
procedure TFormAMQP.iotAMQP1MessageOutcome(Sender: TObject;
  const SessionName: string; SessionIndex: Integer; const LinkName: string;
  LinkIndex: Integer; const MessageId: string; Direction, State: Integer);
var
  Outcome: string;
begin
  if Direction = 1 then
  begin
    case State of
      0: Outcome := 'accepted';
      1: Outcome := 'rejected';
      2: Outcome := 'released';
      3: Outcome := 'modified';
      else Outcome := '(Error: invalid outcome)';
    end;
    Log('Sent message was ' + Outcome + '.');
  end;
end;

procedure TFormAMQP.iotAMQP1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept := True;
end;

// Reset the application
procedure TFormAMQP.ResetForm();
begin
  bConnect.Caption := '&Connect';
  SetGroupEnabled(gConnect, True);
  SetGroupEnabled(gSendReceive, False);
end;

// Helper function for logging
procedure TFormAMQP.Log(s: string);
begin
  if mLog.Lines.Count >= 15 then
    mLog.Lines.Delete(0);
  mLog.Lines.Append(s);
end;

// Enable or disable all the controls in a group
procedure TFormAMQP.SetGroupEnabled(Group: TGroupBox; Enabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to Group.ControlCount - 1 do
  begin
    Group.Controls[I].Enabled := Enabled;
  end;
end;

end.

