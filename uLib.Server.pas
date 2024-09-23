unit uLib.Server;

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Web.WebBroker,

  IdGlobal,
  IdContext,
  IPPeerAPI,
  IdSSLOpenSSL,
  IdSchedulerOfThreadPool,
  IdHTTPWebBrokerBridge;

type

  TServerLocalEvents = class
  public
    SSLCertificate: String;
  private
    procedure OnGetSSLPassword(var APassword: String);
    procedure OnQuerySSLPort(APort: TIdPort; var AUseSSL: Boolean);
    procedure OnParseAuthentication( AContext: TIdContext;
                                     const AAuthType, AAuthData: String;
                                     var VUsername, VPassword: String;
                                     var VHandled: Boolean);
  end;

  TIdHTTPWebBrokerServer = class
  protected
    AIV,
    AKey_Settings: RawByteString;
    ACrypted: Boolean;
    AFile_Settings,
    HostSettings: String;
    LLocalEvents: TServerLocalEvents;
    LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
    SchedulerOfThreadPool: TIdSchedulerOfThreadPool;
  private
    function BindPort(APort: Integer): Boolean;
    function CheckPort(APort: Integer): Integer;
    function LoadConnectionSettings(
                const Key_Settings, IV: RawByteString;
                Crypted: Boolean): string;
  public
    APort: Integer;
    sProtocol,
    LastMessage,
    AppSettings: String;
    FServer: TIdHTTPWebBrokerBridge;
    function DefaultConnectionSettings(): string; virtual;
    procedure SetPort(APort: String);
    procedure StartServer();
    procedure StopServer();
    procedure SetDatabaseSchemas();
    procedure SaveSettings();
    Constructor Create( const File_Settings: String;
                        const Key_Settings,
                        IV: RawByteString;
                        Crypted: Boolean);
    Destructor Destroy();
  end;

resourcestring
  cArrow = '-> ';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandExit = 'exit';

  sPortInUse = '- Error: Port %s already in use';
  sPortSet = '- Port set to %s';
  sServerRunning = '- The Server is already running';
  sStartingServer = '- Starting %s Server on port %d';
  sStoppingServer = '- Stopping Server';
  sServerStopped = '- Server Stopped';
  sServerNotRunning = '- The Server is not running';
  sInvalidCommand = '- Error: Invalid Command';
  sIndyVersion = '- Indy Version: ';
  sActive = '- Active: ';
  sPort = '- Port: ';
  sSessionID = '- Session ID CookieName: ';
  sCommands = 'Enter a Command: ' + slineBreak +
    '   - "start" to start the server'+ slineBreak +
    '   - "stop" to stop the server'+ slineBreak +
    '   - "set port" to change the default port'+ slineBreak +
    '   - "status" for Server status'+ slineBreak +
    '   - "help" to show commands'+ slineBreak +
    '   - "exit" to close the application';


var
   DomainPrefix: Boolean;

implementation

uses
    System.JSON,
    System.StrUtils,
    System.DateUtils,
    System.Generics.Collections,

    Datasnap.DSHTTP,
    Datasnap.DSSession,
    IdIOHandler,
    IdHeaderList,
    IdHTTPHeaderInfo,

(*
    JOSE.Core.JWT,
    JOSE.Core.JWS,
    JOSE.Core.JWK,
    JOSE.Core.Builder,
  //  JOSE.Core.JWA,
  //  JOSE.Types.JSON,
  //  JOSE.Producer,
  //  JOSE.Types.Bytes,
*)
    uLib.Base,
    uLib.Data,
    uLib.DataModule,
    uLib.Common;


var
  App: TDSHTTPApplication;

Type
  TIdHTTPAppRequestHelper = class helper for TIdHTTPAppRequest
  public
    function GetRequestInfo: TIdEntityHeaderInfo;
  end;


function TIdHTTPAppRequestHelper.GetRequestInfo: TIdEntityHeaderInfo;
begin
  Result := FRequestInfo;
end;


procedure TServerLocalEvents.OnGetSSLPassword(var APassword: String);
begin
  APassword := GetStr(SSLCertificate,'password');
end;

function InternalReadLn(AIOHandler: TIdIOHandler): String;
begin
  Result := AIOHandler.ReadLn;
{  if AIOHandler.ReadLnTimedout then begin
    raise ; //EIdReadTimeout.Create(RSReadTimeout);
  end;}
end;

procedure TServerLocalEvents.OnParseAuthentication(
          AContext: TIdContext;
          const AAuthType, AAuthData: String;
          var VUsername, VPassword: String;
          var VHandled: Boolean);
begin
end;

procedure TServerLocalEvents.OnQuerySSLPort(APort: TIdPort; var AUseSSL: Boolean);
begin
  AUseSSL := True;
end;

{ TIdHTTPWebBrokerServer }

procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
     TDSSessionManager.Instance.TerminateAllSessions;
end;

procedure TIdHTTPWebBrokerServer.SetDatabaseSchemas();
Var
  lSchema: String;
begin
  lSchema:=GetStr(ActiveCustomer,'schema');
  lSchema:=Ifthen(lSchema<>'',lSchema+'.','');

  db_MAIN:=(GetStr(ActiveMain,'database')+'.'+lSchema).ToLower;
  db_CUST:=(GetStr(ActiveCustomer,'database')+'.'+lSchema).ToLower;
end;

function TIdHTTPWebBrokerServer.BindPort(APort: Integer): Boolean;
var
  LTestServer: IIPTestServer;
begin
  Result := True;
  try
    LTestServer := PeerFactory.CreatePeer('', IIPTestServer) as IIPTestServer;
    LTestServer.TestOpenPort(APort, nil);
  except
    Result := False;
  end;
end;

function TIdHTTPWebBrokerServer.CheckPort(APort: Integer): Integer;
begin
  if BindPort(APort) then
     Result := APort
  else
     Result := 0;
end;

constructor TIdHTTPWebBrokerServer.Create(
              const File_Settings: String;
              const Key_Settings, IV: RawByteString;
              Crypted: Boolean);
var
  lPathSettings,
  aSSLCertificate: String;
begin
  AFile_Settings:=File_Settings;
  lPathSettings:=ExtractFilePath(File_Settings);

  AKey_Settings:=Key_Settings;
  AIV:=IV;
  ACrypted:=Crypted;

  LLocalEvents := TServerLocalEvents.Create;
  FServer := TIdHTTPWebBrokerBridge.Create(Nil);
  // FServer.OnParseAuthentication := LLocalEvents.OnParseAuthentication;
  App:=TDSHTTPApplication(FServer);
  SchedulerOfThreadPool := TIdSchedulerOfThreadPool.Create(FServer);
  SchedulerOfThreadPool.PoolSize := 50;
  FServer.Scheduler := SchedulerOfThreadPool;
  FServer.MaxConnections := 1000;

  LIOHandleSSL := Nil;
  sProtocol:='http';

  AppSettings:=LoadConnectionSettings(Key_Settings,IV,Crypted);

  HostSettings:=GetStr(AppSettings,'host');
  MailHostSettings:=GetStr(AppSettings,'mail_host');

  DomainPrefix:=GetBool(HostSettings,'domain_prefix');
  FServer.DefaultPort:=GetInt(HostSettings,'http_port');
  aSSLCertificate:=GetStr(HostSettings,'certificates');

  LLocalEvents.SSLCertificate:=aSSLCertificate;
  if FileExists(lPathSettings+GetStr(aSSLCertificate,'cert_file')) then
     begin
       LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(FServer);
       LIOHandleSSL.SSLOptions.CertFile := lPathSettings+GetStr(aSSLCertificate,'cert_file');
       LIOHandleSSL.SSLOptions.KeyFile := lPathSettings+GetStr(aSSLCertificate,'key_file');
       LIOHandleSSL.SSLOptions.RootCertFile := lPathSettings+GetStr(aSSLCertificate,'root_file');

       LIOHandleSSL.SSLOptions.Method:=sslvSSLv23;
       LIOHandleSSL.SSLOptions.Mode:=sslmServer;
       LIOHandleSSL.OnGetPassword := LLocalEvents.OnGetSSLPassword;

       FServer.IOHandler := LIOHandleSSL;
       FServer.OnQuerySSLPort := LLocalEvents.OnQuerySSLPort;
       sProtocol:='https';
     end;
  SetDatabaseSchemas();
  LastMessage:='- '+sProtocol+' Server created.';
end;

function TIdHTTPWebBrokerServer.DefaultConnectionSettings(): String;
Var
  aJSON,
  Settings: String;
begin
  Settings:='';
  aJSON:='';
  SetJSON(aJSON,['uuId','password'],['','']);
  SetJSON(Settings,['security'],[aJSON]);
  Result:=Settings;
end;

destructor TIdHTTPWebBrokerServer.Destroy;
begin
  TerminateThreads();
  if Assigned(LIOHandleSSL)  then
     LIOHandleSSL.Destroy;
  LLocalEvents.Destroy;
  FreeAndNil(SchedulerOfThreadPool);
  FreeAndNil(FServer);
  Inherited;
end;

function TIdHTTPWebBrokerServer.LoadConnectionSettings(
              const Key_Settings, IV: RawByteString;
              Crypted: Boolean): String;
var
  tJSON: TJSONArray;
  aJSON: String;
  lAppSettings: String;
begin
  // -----------------------------------------------
  lAppSettings:=loadFromfile(AFile_Settings, AKEY_SETTINGS,AIV,ACRYPTED);
  if lAppSettings.IsEmpty then
     lAppSettings:=DefaultConnectionSettings;
  // -----------------------------------------------
  aJSON:=GetStr(LAppSettings,'connection');
  RDBMSKind:=GetDriverID(GetStr(AJSON, 'driverId'));
  ADatabaseServer.AddPair('DriverId', GetStr(AJSON, 'driverId'));
  ADatabaseServer.AddPair('Server', GetStr(AJSON, 'server'));
  ADatabaseServer.AddPair('Database', GetStr(AJSON, 'database'));
  ADatabaseServer.AddPair('User_Name', GetStr(AJSON, 'user_name'));
  ADatabaseServer.AddPair('Password', GetStr(AJSON, 'password'));
  ADatabaseServer.AddPair('Port', GetStr(AJSON, 'port'));
  ADatabaseServer.AddPair('OSAuthent', GetStr(AJSON, 'OSAuthent'));
  DefConnection:=ADatabaseServer.Text;

  ActiveMain:='';
  SetJSON(ActiveMain,
         ['driverId','server','database',
          'user_name','password','port','OSAuthent'],
         ['MSSQL',
           GetStr(AJSON, 'server'),
           GetStr(AJSON, 'database'),
           GetStr(AJSON, 'user_name'),
           GetStr(AJSON, 'password'),
           GetStr(AJSON, 'port'),
           GetStr(AJSON, 'OSAuthent')]);

  aJSON := GetStr(aJSON,'provider');
  ADriverVendor.Clear;
  ADriverVendor.AddPair('vendorLib', GetStr(AJSON, 'vendorLib'));
  ADriverVendor.AddPair('vendorHome', GetStr(AJSON, 'vendorHome'));

  aJSON:= GetStr(LAppSettings,'clients');
  tJSON:= CreateTJSONArray(AJSON);
  for var I := 0 to tJSON.count-1 do
    begin
      var sCust:=tJSON.Items[I].ToString;
      if GetBool(sCust,'default') and GetBool(sCust,'active') then
         ActiveCustomer:=sCust;
      if ACustomerList=Nil then
         ACustomerList:=TStringList.Create;
      ACustomerList.add(sCust);
    end;
  tJSON.destroy;
  result:= LAppSettings;
end;

procedure TIdHTTPWebBrokerServer.SaveSettings();
begin
  saveToFile( AFile_Settings, AppSettings, AKEY_SETTINGS,AIV,ACRYPTED);
end;

procedure TIdHTTPWebBrokerServer.SetPort(APort: String);
begin
  if not FServer.Active then
  begin
    APort := APort.Replace(cCommandSetPort, '').Trim;
    if CheckPort(APort.ToInteger) > 0 then
       begin
         FServer.DefaultPort := APort.ToInteger;

         SetJSON(HostSettings,['http_port'],[FServer.DefaultPort]);
         SetJSON(AppSettings, ['host'],[HostSettings]);
         SaveSettings();
         LastMessage:=Format(sPortSet, [APort]);
       end
    else
      LastMessage:=Format(sPortInUse, [APort]);
  end
  else
    LastMessage:=sServerRunning;
end;

procedure TIdHTTPWebBrokerServer.StartServer;
begin
  if not FServer.Active then
  begin
    if CheckPort(FServer.DefaultPort) > 0 then
    begin
      lastmessage:=Format(sStartingServer,[sProtocol, FServer.DefaultPort]);
      FServer.Bindings.Clear;
      FServer.Active := True;
    end
    else
      LastMessage:=Format(sPortInUse, [FServer.DefaultPort.ToString]);
  end
  else
    LastMessage:=sServerRunning+' at port '+sProtocol+' '+FServer.DefaultPort.ToString;
end;

procedure TIdHTTPWebBrokerServer.StopServer;
begin
  if FServer.Active then
  begin
    TerminateThreads;
    FServer.Active := False;
    FServer.Bindings.Clear;
    LastMessage:=sServerStopped;
  end
  else
    LastMessage:=sServerNotRunning;
end;

{ TLIdHTTPWebBrokerBridge }


end.

(*
*)
