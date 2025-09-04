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
unit coapserverf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, iotcore,
  iottypes, iotcoap, System.Generics.Collections;

type
  TFormCoapserver = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    TreeView1: TTreeView;
    Edit1: TEdit;
    Label2: TLabel;
    Button1: TButton;
    iotCoAP1: TiotCoAP;
    Label3: TLabel;
    Label4: TLabel;
    etNewChild: TEdit;
    etNodeData: TEdit;
    bDeleteNode: TButton;
    bAddNode: TButton;
    bSave: TButton;
    GroupBox4: TGroupBox;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure SetData(node: TTreeNode; str: String);
    procedure SetDefaultData();
    procedure Log(Line : string);
    procedure TreeView1Click(Sender: TObject);
    procedure iotCoAP1Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
    procedure iotCoAP1Log(Sender: TObject; LogLevel: Integer;
      const Message: string);
    procedure Button1Click(Sender: TObject);
    procedure DeleteNode(Node: TTreeNode);
    procedure iotCoAP1ResponseComplete(Sender: TObject; const RequestId: string;
      ErrorCode: Integer; const ErrorDescription: string);
    procedure NotifyIfNodeObserved(Node: TTreeNode; Deleted: boolean);
    procedure NotifyIfChildrenObserved(Node: TTreeNode);
    procedure SendNotification(uris: tstringlist);
    procedure bAddNodeClick(Sender: TObject);
    procedure bDeleteNodeClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure iotCoAP1Register(Sender: TObject; const RemoteHost: string;
      RemotePort: Integer; const URI, URIHost: string; URIPort: Integer;
      const URIPath, URIQuery, Token: string; const TokenB: TBytes;
      var Accept: Boolean);
    procedure iotCoAP1Request(Sender: TObject; const RemoteHost: string;
      RemotePort, Method: Integer; const URIHost: string; URIPort: Integer;
      const URIPath, URIQuery, Token: string; const TokenB: TBytes;
      const RequestId: string; var SendResponse: Boolean);
    procedure iotCoAP1Unregistered(Sender: TObject; const RemoteHost: string;
      RemotePort: Integer; const URI, URIHost: string; URIPort: Integer;
      const URIPath, URIQuery: string);

  private
    { Private declarations }
    observedNodes: TDictionary<string, tstringlist>;
    function FindNode(NodePath : string; Root: TTreeNode) : TTreeNode;
    function URIPathToNodePath(URIPath: string) : string;
    function IsObservableNode(NodePath: string) : boolean;
    function GetNodeData(NodePath: string) : string;
    function RequestMethodToString(Method: integer) : string;
    function FindParentNode(NodePath : string) : TTreeNode;
    function GetNodePath(Node: TTreeNode) : string;
  public
    { Public declarations }
  end;

var
  FormCoapserver: TFormcoapserver;

implementation

{$R *.dfm}


procedure TFormCoapserver.FormCreate(Sender: TObject);
var
item : TTreeNode;
begin
  SetDefaultData();

  observedNodes := TDictionary<string, tstringlist>.create();
end;

procedure TFormCoapserver.iotCoAP1Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin
  Log('OnError: [' + ErrorCode.ToString() + '] ' + Description);
end;

procedure TFormCoapserver.iotCoAP1Log(Sender: TObject; LogLevel: Integer;
  const Message: string);
begin
  Log('OnLog: [' + LogLevel.ToString() + '] ' + Message);
end;


procedure TFormCoapserver.iotCoAP1Register(Sender: TObject;
  const RemoteHost: string; RemotePort: Integer; const URI, URIHost: string;
  URIPort: Integer; const URIPath, URIQuery, Token: string;
  const TokenB: TBytes; var Accept: Boolean);
var
Node: TTreeNode;
NodePath: string;
obsURIs: tstringlist;
begin
  Log('OnRegister: Client ' + RemoteHost + ':' + RemotePort.ToString()
        + ' attempting to register as an observer for ' + URI + '...');
  NodePath := Copy(URI, URI.IndexOf('://') + 4, URI.Length);
  NodePath := Copy(NodePath, NodePath.IndexOf('/')+2, NodePath.Length);
  NodePath := URIPathToNodePath(NodePath);
  Node := FindNode(NodePath, TreeView1.Items[0]);
  if Node = nil then
  begin
    iotCoAP1.ResponseContentFormat := -1;
    iotCoAP1.ResponseData := '';
    iotCoAP1.ResponseCode := '4.04'; // "Not Found".
    Log('OnRegister: No such node found.');
  end
  else
  begin
    iotCoAP1.ResponseContentFormat := 0; // "text/plain; charset=utf-8"
    iotCoAP1.ResponseData := GetNodeData(NodePath);
    iotCoAP1.ResponseCode := '2.05'; // "Content".
    if (IsObservableNode(nodePath)) then
    begin
      if observedNodes.ContainsKey(NodePath) then
      begin
        obsURIs := observedNodes[NodePath];
      end
      else
      begin
        obsURIs := tstringlist.Create();
      end;
      obsURIs.Add(URI);
      if observedNodes.ContainsKey(NodePath) then
      begin
        observedNodes[NodePath] := obsURIs;
      end
      else
      begin
        observedNodes.Add(NodePath, obsURIs);
      end;
      Accept := true;
      Log('OnRegister: Node found, registration accepted.');
    end
    else
    begin
      // It's handled just like a GET request, e.Accept is false by default.
      Log('OnRegister: Node found, but observation not allowed, handling like normal GET request.');
    end;
  end;
end;


procedure TFormCoapserver.iotCoAP1Request(Sender: TObject;
  const RemoteHost: string; RemotePort, Method: Integer; const URIHost: string;
  URIPort: Integer; const URIPath, URIQuery, Token: string;
  const TokenB: TBytes; const RequestId: string; var SendResponse: Boolean);
var
MethodStr, NodePath, NewNodeName: string;
Node, Parent: TTreeNode;
begin
  MethodStr := RequestMethodToString(Method);
  Log('OnRequest: ' + MethodStr + ' request received from ' + RemoteHost + ':' +
  RemotePort.ToString() + ' (URIHost=' + URIHost + ' URIPort=' + URIPort.ToString() +
  ' URIPath='+ URIPath + ' URIQuery=' + URIQuery + ' Id=' + RequestId + ')...');

  nodePath := URIPathToNodePath(URIPath);
  Node := FindNode(nodePath, TreeView1.Items[0]);
  if  MethodStr = 'GET' then
  begin
    if Node = nil then
    begin
      iotCoAP1.ResponseContentFormat := -1;
      iotCoAP1.ResponseData := '';
      iotCoAP1.ResponseCode := '4.04'; // "Not Found".
      Log('OnRequest: No such node found.');
    end
    else
    begin
    iotCoAP1.ResponseContentFormat := -1;
    iotCoAP1.ResponseData := GetNodeData(nodePath);
    iotCoAP1.ResponseCode := '2.05'; // "Content".
    Log('OnRequest: Node found, returning data.');
    end;
  end
  else if  (MethodStr = 'PUT') or (MethodStr = 'POST') then
  begin
    if iotCoAP1.RequestContentFormat <> 0 then
    begin
      iotCoAP1.RequestContentFormat := -1;
      iotCoAP1.ResponseData := 'Request''s Content-Format must be "text/plain; charset=utf-8".'; // Diagnostic payload.
      iotCoAP1.ResponseCode := '4.00'; // "Bad Request".
      Log('OnRequest: Request''s Content-Format is invalid (must be "text/plain; charset=utf-8").');
    end
    else if Node = nil then
    begin
      Parent := FindParentNode(NodePath);
      if (Parent = nil) then
      begin
        iotCoAP1.ResponseContentFormat := -1;
        iotCoAP1.ResponseData := 'One or more parent nodes are missing.'; // Diagnostic payload.
        iotCoAP1.ResponseCode := '4.00'; // "Bad Request".
        Log('OnRequest: One or more parent nodes are missing.');
      end
      else
      begin
        NewNodeName := Copy(NodePath, NodePath.LastIndexOf('/') + 2, NodePath.Length);
        TreeView1.Items.AddChild(Parent, NewNodeName);
        Node := FindNode(nodePath, TreeView1.Items[0]);
        SetData(Node, iotCoAP1.RequestData);
        iotCoAP1.ResponseContentFormat := -1;
        iotCoAP1.ResponseData := '';
        iotCoAP1.ResponseCode := '2.01'; // "Created".
        Log('OnRequest: Node created at ' + nodePath + '.');
      end;
    end
    else
    begin
      SetData(Node, iotCoAP1.RequestData);
      NotifyIfNodeObserved(Node, False);

      iotCoAP1.ResponseContentFormat := -1;
      iotCoAP1.ResponseData := '';
      iotCoAP1.ResponseCode := '2.04'; // "Changed".
      Log('OnRequest: Node Data changed at ' + nodePath + '.');
    end;
  end
  else if MethodStr = 'DELETE' then
  begin
    if (Node <> nil) then
    begin
      DeleteNode(Node);
    end;
    iotCoAP1.ResponseContentFormat := -1;
    iotCoAP1.ResponseData := '';
    iotCoAP1.ResponseCode := '2.02'; // "Deleted".
    Log('OnRequest: Node deleted (or did not exist in the first place).');
  end
  else // Default Response: Request Method Not Allowed
  begin
    iotCoAP1.ResponseContentFormat := -1;
    iotCoAP1.ResponseData := 'The specified request method is not supported for this resource.'; // Diagnostic payload.
    iotCoAP1.ResponseCode := '4.05'; // "Method not allowed."
    Log('OnRequest: Method not allowed.');
  end;
end;

procedure TFormCoapserver.iotCoAP1ResponseComplete(Sender: TObject;
  const RequestId: string; ErrorCode: Integer; const ErrorDescription: string);
begin
   Log('OnResponseComplete: Response sent for request Id ' + RequestId + '.');
end;


procedure TFormCoapserver.iotCoAP1Unregistered(Sender: TObject;
  const RemoteHost: string; RemotePort: Integer; const URI, URIHost: string;
  URIPort: Integer; const URIPath, URIQuery: string);
var
Node: TTreeNode;
NodePath: string;
begin
  NodePath := Copy(URI, URI.IndexOf('://') + 4, URI.Length);
  NodePath := Copy(NodePath, NodePath.IndexOf('/')+2, NodePath.Length);
  NodePath := URIPathToNodePath(NodePath);
  Log('OnUnregistered: Client ' + RemoteHost + ':' + RemotePort.ToString() + ' is no longer observing ' + URI + '.');
  ObservedNodes.Remove(NodePath);
end;

procedure TFormCoapserver.SetData(node: TTreeNode; str: String);
var
pStr : ^String;
begin
  New(pStr);
  pStr^ := str;
  node.Data := pStr;
end;


procedure TFormCoapserver.bAddNodeClick(Sender: TObject);
var
NewNodeName: string;
Node, Parent: TTreeNode;
begin
  Parent := TreeView1.Selected;
  if (Parent = nil) then
  begin
    Log('CreateNode: One or more parent nodes are missing.');
  end
  else
  begin
    NewNodeName := etNewChild.Text;
    Node := TreeView1.Items.AddChild(Parent, NewNodeName);
    SetData(Node, etNodeData.Text);
    Log('CreateNode: Node created at ' + GetNodePath(Node) + '.');
  end;
end;

procedure TFormCoapserver.bDeleteNodeClick(Sender: TObject);
begin
  DeleteNode(TreeView1.Selected);
end;

procedure TFormCoapserver.bSaveClick(Sender: TObject);
begin
  SetData(TreeView1.Selected, etNodeData.Text);
  NotifyIfNodeObserved(TreeView1.Selected, False);

  Log('OnSave: Node Data changed at ' + GetNodePath(TreeView1.Selected) + '.');
end;

procedure TFormCoapserver.Button1Click(Sender: TObject);
begin
  if iotCoAP1.Listening then
  begin
    Button1.Caption := 'Start Listening';
    iotCoAP1.Listening := false;
  end
  else
  begin
    iotCoAP1.LocalPort := strtoint(Edit1.Text);
    Button1.Caption := 'Stop Listening';
    iotCoAP1.Listening := true;
  end;
end;

procedure TFormCoapserver.DeleteNode(Node: TTreeNode);
begin
  NotifyIfNodeObserved(Node, True);
  if Node <> TreeView1.Items[0] then
  TreeView1.Items.Delete(Node);
end;

procedure TFormCoapserver.NotifyIfNodeObserved(Node: TTreeNode; Deleted: boolean);
begin
  ShowMessage('Notification?');
  if ObservedNodes.ContainsKey(GetNodePath(Node)) then
  begin
    if (not deleted) then
    begin
      ShowMessage('Notification!');
      iotCoAP1.ResponseContentFormat := 0; // "text/plain; charset=utf-8"
      iotCoAP1.ResponseData := GetNodeData(GetNodePath(Node));
      iotCoAP1.ResponseCode := '2.05'; // "Content".
    end
    else
    begin
      iotCoAP1.RequestContentFormat := -1;
      iotCoAP1.ResponseData := '';
      iotCoAP1.ResponseCode := '4.04'; // "Not Found".
    end;

    SendNotification(ObservedNodes[GetNodePath(Node)]);
    if deleted then
      ObservedNodes.Remove(GetNodePath(Node));
  end;

end;

procedure TFormCoapserver.SendNotification(Uris: tstringlist);
var
uri: string;
begin
  for uri in uris do
    iotCoAP1.SendNotification(uri);
end;

// Unlike NotifyIfNodeObserved, this method is only ever called to notify about deletions.
procedure TFormCoapserver.NotifyIfChildrenObserved(Node: TTreeNode);
var Child, Curr : TTreeNode;
begin
  iotCoAP1.RequestContentFormat := -1;
  iotCoAP1.ResponseData := '';
  iotCoAP1.ResponseCode := '4.04'; // "Not Found".
  Curr := Node.getFirstChild();
  while Curr <> nil do
  begin
    if ObservedNodes.ContainsKey(GetNodePath(child)) then
      SendNotification(observedNodes[GetNodePath(child)]);
      observedNodes.Remove(GetNodePath(child));
      NotifyIfChildrenObserved(child);
    Curr := Curr.getNextSibling();
  end;

end;

Function TFormCoapserver.FindNode(NodePath : string; Root: TTreeNode) : TTreeNode;
var
strings : TStrings;
Curr : TTreeNode;
begin
  Result := nil;
  strings := TStringList.Create;
  ExtractStrings(['/'], [], PChar(NodePath), strings);
  if (strings.Count > 0) and (strings[0] = Root.Text) then
  begin;
    if (strings.Count = 1) then
      Result := Root
    else if Root.HasChildren then
    begin
      Curr := Root.getFirstChild;
      repeat
        Result := FindNode(Copy(NodePath, NodePath.IndexOf('/') + 2, NodePath.Length), Curr);
        Curr := Curr.GetNextSibling;
      until (Curr = nil) or (Result <> nil);
    end;

  end;
end;

Function TFormCoapserver.FindParentNode(NodePath : string) : TTreeNode;

begin
  NodePath := Copy(NodePath, 0, NodePath.LastIndexOf('/'));
  Result := FindNode(NodePath, TreeView1.Items[0]);
end;

procedure TFormCoapserver.SetDefaultData();
begin
  SetData(FindNode('<root>', TreeView1.Items[0]), 'Hello World!');
  SetData(FindNode('<root>/animals', TreeView1.Items[0]), 'Root node for animal noises');
  SetData(FindNode('<root>/animals/domestic', TreeView1.Items[0]), 'Domestic animal noises');
  SetData(FindNode('<root>/animals/domestic/dog', TreeView1.Items[0]), 'Woof!');
  SetData(FindNode('<root>/animals/domestic/cat', TreeView1.Items[0]), 'Meow');
  SetData(FindNode('<root>/animals/domestic/parakeet', TreeView1.Items[0]), 'Chirp');
  SetData(FindNode('<root>/animals/farm', TreeView1.Items[0]), 'Farm animal noises');
  SetData(FindNode('<root>/animals/farm/cow', TreeView1.Items[0]), 'Moo');
  SetData(FindNode('<root>/animals/farm/sheep', TreeView1.Items[0]), 'Baaa');
  SetData(FindNode('<root>/animals/farm/pig', TreeView1.Items[0]), 'Oink');
  SetData(FindNode('<root>/animals/wild', TreeView1.Items[0]), 'Wild animal noises');
  SetData(FindNode('<root>/animals/wild/lion', TreeView1.Items[0]), 'Roar');
  SetData(FindNode('<root>/animals/wild/tiger', TreeView1.Items[0]), 'Growl');
  SetData(FindNode('<root>/animals/wild/bear', TreeView1.Items[0]), 'Oh My!');
end;

procedure TFormCoapserver.TreeView1Click(Sender: TObject);
begin
Screen.Cursor := crHourGlass;
   try
      if TreeView1.Selected.Level >= 0 then
      begin;
        etNodeData.Text := string(TreeView1.Selected.Data^)
      end;
   except on E: EIPworksIoT do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

function TFormCoapserver.GetNodePath(Node: TTreeNode) : string;
begin
  Result := Node.Text;
   while Node <> TreeView1.Items[0] do
   begin
     Result := Node.Parent.Text + '/' + Result;
     Node := Node.Parent;
   end;
end;

function TFormCoapserver.GetNodeData(NodePath: string) : string;
var
Node : TTreeNode;
begin
  Node := FindNode(NodePath, TreeView1.Items[0]);
  Result := String(Node.Data^);
end;

procedure TFormCoapserver.Log(Line: string);
begin
   ListBox1.items.Add(Line);
end;

Function TFormCoapserver.URIPathToNodePath(URIPath: string) : string;
begin
  if URIPath = '' then
    Result := '<node>'
  else
    Result := '<root>/' + URIPath
end;

Function TformCoapServer.IsObservableNode(NodePath: string) : boolean;
begin
  if NodePath <> '<root>' then
    Result := True
  else
    Result := False
end;

function TformCoapServer.RequestMethodToString(Method: integer) : string;
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


