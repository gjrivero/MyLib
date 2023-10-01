unit uLib.Server;

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,

  IdGlobal,
  IPPeerAPI,
  IdSSLOpenSSL,
  IdHTTPWebBrokerBridge;

type
  TServerLocalEvents = class
  public
    SSLCertificate: String;
  private
    procedure OnGetSSLPassword(var APassword: String);
    procedure OnQuerySSLPort(APort: TIdPort; var AUseSSL: Boolean);
  end;

  TIdHTTPWebBrokerServer = class
  protected
    AIV,
    AKey_Settings: RawByteString;
    ACrypted: Boolean;
    LAppSettings,
    SettingsFile,
    HostSettings: String;
    LLocalEvents: TServerLocalEvents;
    LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
    FGetAppSettings: String;
    procedure SetGetAppSettings(const Value: String);
  private
    function BindPort(APort: Integer): Boolean;
    function CheckPort(APort: Integer): Integer;
    procedure LoadConnectionSettings(
                const AppFileName: String;
                const Key_Settings, IV: RawByteString;
                Crypted: Boolean;
                LocalDir: Boolean);
  public
    APort: Integer;
    sProtocol,
    LastMessage: String;
    FServer: TIdHTTPWebBrokerBridge;
    property GetAppSettings: String read FGetAppSettings write SetGetAppSettings;
    procedure SetPort(APort: String);
    procedure StartServer();
    procedure StopServer();
    procedure SetDatabaseSchemas();
    procedure SaveSettings(const AppSettings: String);
    procedure DefaultConnectionSettings(Var AppSettings: String); virtual;
    Constructor Create( const AppFileName: String;
                        const Key_Settings,
                        IV: RawByteString;
                        Crypted: Boolean;
                        LocalDir: Boolean=false);
    Destructor Destroy();
  end;

resourcestring
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

const
  cArrow = '->';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandExit = 'exit';
  logsFolder = 'logs';
  settingFolder = 'settings';

  (*
  'uuId',
  'password'
  'security'
  'http_port'
  'host'
  'driverId',
  'server',
  'database',
  'user_name',
  'password',
  'port',
  'OSAuthent'],
    ['database','schema','branch','role','active','default'],
    ['','dbo','','standard',false,false]);
*)

{$SCOPEDENUMS ON}
type
  DSProtocol = (HTTP, HTTPS);
{$SCOPEDENUMS OFF}

var
   DomainPrefix: Boolean;

implementation

uses
    System.JSON,
    System.StrUtils,
    System.Generics.Collections,
    Datasnap.DSSession,

    uLib.Base,
    uLib.Data,
    uLib.DataModule,
    uLib.Common;


procedure TServerLocalEvents.OnGetSSLPassword(var APassword: String);
begin
  APassword := GetStr(SSLCertificate,'password');
end;

procedure TServerLocalEvents.OnQuerySSLPort(APort: TIdPort; var AUseSSL: Boolean);
begin
  AUseSSL := True;
end;

procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
     TDSSessionManager.Instance.TerminateAllSessions;
end;

{ TIdHTTPWebBrokerServer }

procedure TIdHTTPWebBrokerServer.SetDatabaseSchemas();
Var
  lSchema: String;
begin
  lSchema:=GetStr(ActiveCustomer,'schema');
  lSchema:=Ifthen(lSchema<>'',lSchema+'.','').ToLower;

  db_MAIN:=GetStr(ActiveMain,'database')+'.'+lSchema;
  db_CUST:=GetStr(ActiveCustomer,'database')+'.'+lSchema;
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
              const AppFileName: String;
              const Key_Settings, IV: RawByteString;
              Crypted: Boolean;
              LocalDir: Boolean=false);
var
  CertifiedPath,
  aSSLCertificate: String;
  TL: TStringList;
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Nil);
  LLocalEvents := TServerLocalEvents.Create;
  LIOHandleSSL := Nil;
  sProtocol:='http';
  AKey_Settings:=Key_Settings;
  AIV:=IV;
  ACrypted:=Crypted;
  LoadConnectionSettings(AppFileName,Key_Settings,IV,Crypted,LocalDir);
  HostSettings:=GetStr(GetAppSettings,'host');
  DomainPrefix:=GetBool(HostSettings,'domain_prefix');
  FServer.DefaultPort:=GetInt(HostSettings,'http_port');
  aSSLCertificate:=GetStr(HostSettings,'certificates');
  LLocalEvents.SSLCertificate:=aSSLCertificate;
  CertifiedPath:=ApplicationPath+settingFolder+PathDelim;
  if FileExists(CertifiedPath+GetStr(aSSLCertificate,'cert_file')) then
     begin
       LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(FServer);
       LIOHandleSSL.SSLOptions.CertFile := CertifiedPath+GetStr(aSSLCertificate,'cert_file');
       LIOHandleSSL.SSLOptions.KeyFile := CertifiedPath+GetStr(aSSLCertificate,'key_file');
       LIOHandleSSL.SSLOptions.RootCertFile := CertifiedPath+GetStr(aSSLCertificate,'root_file');

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

procedure TIdHTTPWebBrokerServer.DefaultConnectionSettings(Var AppSettings: String);
Var
  aJSON: String;
begin
  AppSettings:='';
  aJSON:='';
  SetJSON(aJSON,['uuId','password'],['','']);
  SetJSON(AppSettings,['security'],[aJSON]);
end;

destructor TIdHTTPWebBrokerServer.Destroy;
begin
  TerminateThreads();
  if Assigned(LIOHandleSSL)  then
     LIOHandleSSL.Destroy;
  LLocalEvents.Destroy;
  FreeAndNil(FServer);
  Inherited;
end;

procedure TIdHTTPWebBrokerServer.LoadConnectionSettings(
              const AppFileName: String;
              const Key_Settings, IV: RawByteString;
              Crypted: Boolean;
              LocalDir: Boolean);
var
  tJSON: TJSONArray;
  aJSON: String;
begin
  ApplicationPath:=GetApplicationPath(LocalDir); // True para usar directorio aplicacion
  // -----------------------------------------------
  if Not DirectoryExists(ApplicationPath+logsFolder) then
     begin
       ForceDirectories(ApplicationPath+logsFolder);
     end;
  ApplicationLogs:=ApplicationPath+logsFolder+PathDelim+ApplicationName+'.log';
  // -----------------------------------------------
  if Not DirectoryExists(ApplicationPath+settingFolder) then
     begin
       ForceDirectories(ApplicationPath+settingFolder);
     end;
  SettingsFile:=ApplicationPath+settingFolder+PathDelim+ApplicationName+'.cnf';
  // -----------------------------------------------
  LAppSettings:= loadFromfile(SettingsFile, Key_Settings,IV, Crypted);
  if LAppSettings='' then
     begin
       DefaultConnectionSettings(LAppSettings);
       saveTofile(SettingsFile,LAppSettings,KEY_SETTINGS,IV, CRYPTED);
     end;
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
  GetAppSettings:= LAppSettings;
end;

procedure TIdHTTPWebBrokerServer.SaveSettings(const AppSettings: String);
begin
  saveToFile( SettingsFile, AppSettings, AKEY_SETTINGS,AIV,ACRYPTED);
end;

procedure TIdHTTPWebBrokerServer.SetGetAppSettings(const Value: String);
begin
  FGetAppSettings := Value;
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
         SetJSON(LAppSettings, ['host'],[HostSettings]);
         SaveSettings(LAppSettings);

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

end.

(*
*)
