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
unit xmppf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, iotcore, iottypes, iotxmpp;

type
  TFormXmpp = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    tbUser: TEdit;
    tbPass: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    cbContacts: TComboBox;
    bConnect: TButton;
    bSend: TButton;
    tbHistory: TMemo;
    tbInput: TMemo;
    XMPP1: TiotXMPP;
    tbServer: TEdit;
    Label5: TLabel;
    procedure bConnectClick(Sender: TObject);
    procedure bSendClick(Sender: TObject);
    procedure XMPP1Connected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure XMPP1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure XMPP1Presence(Sender: TObject; const User, Domain,
      Resource: String; Availability: Integer; const Status: String);
    procedure XMPP1MessageIn(Sender: TObject; const MessageId, From,
      Domain, Resource: String; MessageType: Integer; const Subject,
      MessageThread, MessageText, MessageHTML, Other: String);
    procedure XMPP1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
   
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormXmpp: TFormXmpp;

implementation

{$R *.DFM}

procedure TFormXmpp.bConnectClick(Sender: TObject);
begin
        Try
            Screen.Cursor := crHourGlass;
            If bConnect.Caption = 'Connect' Then begin
                XMPP1.IMServer := tbServer.Text;
				XMPP1.SSLStartMode:=sslExplicit;
                XMPP1.ConnectTo(tbUser.Text, tbPass.Text);
                end
            else begin
                XMPP1.Disconnect();
            end;
        except on E:Exception do begin
            ShowMessage(E.Message);
            Screen.Cursor := crDefault;
            end;
        end;
end;

procedure TFormXmpp.bSendClick(Sender: TObject);
begin
        tbHistory.Lines.Append(tbUser.Text + ': ' + tbInput.Text);
        XMPP1.MessageText := tbInput.Text;
        XMPP1.SendMessage(cbContacts.Text);
end;

procedure TFormXmpp.XMPP1Connected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
        If (StatusCode = 0) Then begin
            bConnect.Caption := 'Disconnect';
            end
        else begin
        ShowMessage('Connection Error (' + IntToStr(StatusCode) + '): ' + Description);
        end;
        FormXmpp.Caption := 'Now logged on as "' + XMPP1.User + '"';
        Screen.Cursor := crDefault;
end;

procedure TFormXmpp.XMPP1Disconnected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
        If Not (StatusCode = 0) Then begin
            ShowMessage('Disconnected (' + IntToStr(StatusCode) + '): ' + Description);
        end;
        bConnect.Caption := 'Connect';
        FormXmpp.Caption := 'Not yet logged in';
        Screen.Cursor := crDefault;
end;

procedure TFormXmpp.XMPP1Presence(Sender: TObject; const User, Domain,
  Resource: String; Availability: Integer; const Status: String);
begin
        If Not (Availability = 0) Then begin //if not offline
            if cbContacts.Items.IndexOf(User) < 0 then
               cbContacts.Items.Add(User + '@' + Domain);
               cbContacts.Text := User + '@' + Domain;
        end;
end;

procedure TFormXmpp.XMPP1SSLServerAuthentication(Sender: TObject;
  const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=True;
end;

procedure TFormXmpp.XMPP1MessageIn(Sender: TObject; const MessageId, From,
  Domain, Resource: String; MessageType: Integer; const Subject,
  MessageThread, MessageText, MessageHTML, Other: String);
begin
  tbHistory.Lines.Append(From + ': ' + MessageText);
end;

end.


