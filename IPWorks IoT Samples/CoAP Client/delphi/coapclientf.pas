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
unit coapclientf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, iotcore,
  iottypes, iotcoap, System.Generics.Collections;

type
  TFormCoapclient = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    etHost: TEdit;
    Label2: TLabel;
    iotCoAP1: TiotCoAP;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox4: TGroupBox;
    ListBox1: TListBox;
    etPort: TEdit;
    Label5: TLabel;
    etURI: TEdit;
    etRequestBody: TEdit;
    bGet: TButton;
    bPost: TButton;
    bPut: TButton;
    bDelete: TButton;
    bStopObserving: TButton;
    bStartObservation: TButton;
    procedure Log(Line: string);
    procedure iotCoAP1Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
    procedure iotCoAP1Log(Sender: TObject; LogLevel: Integer;
      const Message: string);
    procedure iotCoAP1Notification(Sender: TObject; const URI: string;
      IsLatest: Boolean; var StopObserving: Boolean);
    procedure iotCoAP1RequestComplete(Sender: TObject; Method: Integer;
      const URI, RequestId: string; ErrorCode: Integer;
      const ErrorDescription: string);
    procedure bGetClick(Sender: TObject);
    procedure bPostClick(Sender: TObject);
    procedure bPutClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bStartObservationClick(Sender: TObject);
    procedure bStopObservingClick(Sender: TObject);
  private
    { Private declarations }
    function RequestMethodToString(Method: integer) : string;
    function BuildURI() : string;
  public
    { Public declarations }
  end;

var
  FormCoapclient: TFormCoapclient;

implementation

{$R *.dfm}

procedure TFormCoapclient.iotCoAP1Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin
  Log('OnError: [' + ErrorCode.ToString() + '] ' + Description);
end;

procedure TFormCoapclient.iotCoAP1Log(Sender: TObject; LogLevel: Integer;
  const Message: string);
begin
  Log('OnLog: [' + LogLevel.ToString() + '] ' + Message);
end;

procedure TFormCoapclient.iotCoAP1Notification(Sender: TObject;
  const URI: string; IsLatest: Boolean; var StopObserving: Boolean);
begin
  if IsLatest then
    Log('OnNotification: Received notification about resource at ' + URI + ': [' +
    iotCoAP1.ResponseCode + '] ' + iotCoAP1.ResponseData)
  else
    Log('OnNotification: Received out-of-date notification about resource at ' + URI + '; ignoring.');
end;

procedure TFormCoapclient.iotCoAP1RequestComplete(Sender: TObject;
  Method: Integer; const URI, RequestId: string; ErrorCode: Integer;
  const ErrorDescription: string);
begin
  if ErrorCode = 0 then
    Log('OnRequestComplete: Request ' + RequestMethodToString(Method) + ' request for ' + URI + ' completed successfully: [' +
          iotCoAP1.ResponseCode + '] ' + iotCoAP1.ResponseData)
  else if ErrorCode = 709 then  // Indicates non-2.xx response code returned by server.
    Log('OnRequestComplete: Request ' + RequestMethodToString(Method) + ' request for ' + URI + ' completed unsuccessfully: [' +
          iotCoAP1.ResponseCode + '] ' + iotCoAP1.ResponseData)
  else
    Log('OnRequestComplete: ' + RequestMethodToString(Method) + ' request for ' + URI + ' failed with the following error: (' +
          ErrorCode.ToString() + ') ' + ErrorDescription);
end;

procedure TFormCoapclient.Log(Line: string);
begin
   ListBox1.items.Add(Line);
end;

procedure TFormCoapclient.bDeleteClick(Sender: TObject);
begin
  Log('Sending DELETE request for ' + BuildURI() + '...');
  try
    iotCoAP1.Delete(BuildURI())
  Except
    on E : Exception do
      ShowMessage(E.Message)
  end;
end;

procedure TFormCoapclient.bGetClick(Sender: TObject);
begin
  Log('Sending GET request for ' + BuildURI() + '...');
  try
    iotCoAP1.Get(BuildURI())
  Except
    on E : Exception do
      ShowMessage(E.Message)
  end;
end;

procedure TFormCoapclient.bPostClick(Sender: TObject);
begin
  iotCoAP1.RequestContentFormat := 0; // "text/plain; charset=utf-8".
  iotCoAP1.RequestData := etRequestBody.Text;
  Log('Sending POST request for ' + BuildURI() + '...');
  try
    iotCoAP1.Post(BuildURI())
  Except
    on E : Exception do
      ShowMessage(E.Message)
  end;
end;

procedure TFormCoapclient.bPutClick(Sender: TObject);
begin
  iotCoAP1.RequestContentFormat := 0; // "text/plain; charset=utf-8".
  iotCoAP1.RequestData := etRequestBody.Text;
  Log('Sending PUT request for ' + BuildURI() + '...');
  try
    iotCoAP1.Put(BuildURI())
  Except
    on E : Exception do
      ShowMessage(E.Message)
  end;
end;

procedure TFormCoapclient.bStartObservationClick(Sender: TObject);
begin
  Log('Attempting to start observation of ' + BuildURI() + '...');
  try
    iotCoAP1.StartObserving(BuildURI())
  Except
    on E : Exception do
      ShowMessage(E.Message)
  end;
end;

procedure TFormCoapclient.bStopObservingClick(Sender: TObject);
begin
  Log('Attempting to stop observation of ' + BuildURI() + '...');
  try
    iotCoAP1.StopObserving(BuildURI())
  Except
    on E : Exception do
      ShowMessage(E.Message)
  end;
end;

function TFormCoapclient.BuildURI() : string;
begin
   Result := 'coap://' + etHost.Text + ':' + etPort.Text + '/' + etURI.Text;
end;

function TFormCoapclient.RequestMethodToString(Method: integer) : string;
begin
  case Method of
    1: Result := 'GET';
    2: Result := 'POST';
    3: Result := 'PUT';
    4: Result := 'DELETE';
    else Result := '<other>';
  end;
end;

end.


