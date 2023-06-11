unit uLib.Data;

interface

uses
  System.SysUtils, System.Classes,
  System.JSON, System.UITypes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.ConsoleUI.Wait,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Comp.Client, FireDAC.Comp.UI,
  FireDAC.Comp.DataSet

{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  ,Data.DBXPlatform
  ,Datasnap.DSSession
  ,FireDAC.Phys.ODBCBase
  ,FireDAC.Phys.MSSQL
  ,FireDAC.Phys.MSSQLDef
  ,FireDAC.Phys.MySQL
  ,FireDAC.Phys.MySQLDef
  ,FireDAC.Phys.PG
  ,FireDAC.Phys.PGDef
{$ENDIF}
  ,System.Generics.Collections, Data.DB;

type
  TFDM = class(TDataModule)
    Cnx: TFDConnection;
    WaitCursor: TFDGUIxWaitCursor;
    EventAlerter1: TFDEventAlerter;
    SQLiteDriver: TFDPhysSQLiteDriverLink;
    SQLiteSecurity: TFDSQLiteSecurity;
    Qry: TFDQuery;
    Cmd: TFDCommand;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure CnxBeforeConnect(Sender: TObject);
  private
    FautoCommit: Boolean;
    FHeaderEnabled: boolean;
    { Private declarations }
{$IF DEFINED(LINUX) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
    MySQLDriver: TFDPhysMySQLDriverLink;
    PgDriver: TFDPhysPgDriverLink;
    MSSQLDriver: TFDPhysMSSQLDriverLink;
{$ENDIF}
    procedure SetHeader(aHeader: TStringList; ForAudit: Boolean=false);
    function  ParamsToJSONObject(params: TFDParams): TJSONObject;
    function cmdAdd(Const dbTableName,
                          Context: String;
                          fldsReturn: String=''): TJSONObject;
    function cmdUpd(Const dbTableName,
                          Context: String;
                          Condition: String): TJSONObject;
    function cmdDel( Const dbTableName, sWhere: String): TJSONObject;
    function cmdExecute(sCmd: string; pParams: TFDParams=Nil): Integer; overload;
    function cmdExecute(sCmd: TStringList; pParams: TFDParams=Nil): Integer; overload;

    function GetRecords(Const sQuery: String;
                              pParams: TFDParams=nil): TJSONArray;

    function execTrans( cSQL, sDecl: TStringList;
                        sFields: String;
                        pParams: TFDParams): TJSONObject;
    function SQLiteSetPassword( Const sNewPass, sOldPass: String): Integer;
    function SQLiteSetCrypt(encrypt: Boolean; const sPassword: string): Integer;
    function SQLiteGetCrypt( const sPassword: String; var sCrypted: String): Integer;
    procedure SetautoCommit(const Value: Boolean);
    procedure SetHeaderEnabled(const Value: boolean);
  public
    { Public declarations }
    property HeaderEnabled: boolean read FHeaderEnabled write SetHeaderEnabled;
    property autoCommit: Boolean read FautoCommit write SetautoCommit;
  end;

Const
  DB_SUCCESSFUL       =  0;
  DB_DATABASE_ERROR   = -1;
  DB_BAD_URL_PARAMS   = -2;
  DB_CONNECTION_ERROR = -3;
  DB_DATA_NOT_FOUND   = -4;
  DB_COMMAND_ERROR    = -5;
  DB_WRITE_FILE_ERROR = -6;
  DB_USER_ERROR       = -7;
  DB_PASSWORD_ERROR   = -8;
  DB_RECORD_INACTIVE  = -9;

var
  FDM: TFDM;
  RDBMSKind: TFDRDBMSKind;
  HTTP_PORT: Integer= 8080;
  DBMessageResult,
  ASettingsFileName: String;
  ACustDatabase,
  ADriverVendor,
  ADefConnection,
  ADatabaseServer,
  ASQLiteSecurity: TStringList;


function sqlWhere( pParams: TFDParams ): String; Overload;
function sqlWhere( const fldNames:  Array of String;
                   const fldValues: Array of Const): String;  Overload;

function GetData( const sQuery: String;
                        pParams: TFDParams=Nil): TJSONArray; Overload;
function GetData( sQuery: TStrings;
                  pParams: TFDParams=nil): TJSONArray; Overload;
function GetData( const sQuery: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONArray; Overload;
function GetData( sQuery: TStrings;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONArray; Overload;

function AddData( const dbTableName: String;
                        Context: TJSONValue;
                        fldsReturn: String='' ): TJSONObject; Overload;
function AddData( const dbTableName,
                        Context: String;
                        fldsReturn: String='' ): TJSONObject; Overload;
function AddData( const dbTableName: string;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                        fldsReturn: String='' ): TJSONObject; Overload;

function UpdData( const dbTableName: String;
                        Context: TJSONValue;
                        Condition: String): TJSONObject; Overload;
function UpdData( const dbTableName,
                        Context,
                        Condition: String): TJSONObject; Overload;
function UpdData( const dbTableName: String;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                        Condition: String): TJSONObject; Overload;

function DelData( const dbTableName, Condition: String): TJSONObject;

function ExecCmd( const sCommands: String; pParams: TFDParams=Nil): Integer; Overload;
function ExecCmd( sCommands: TStrings; pParams: TFDParams=Nil): Integer; Overload;
function ExecCmd( const sCommands: String;
                  const fldNames: Array of String;
                  const fldValues: Array of const): Integer; Overload;
function ExecCmd( sCommands: TStrings;
                  const fldNames: Array of String;
                  const fldValues: Array of const): Integer; Overload;

function ExecTransact( cSQL: TStringList;
                       sDecl: TStringList=nil;
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject; Overload;

function SQLChangePass( const newPass, oldPass: String): Integer;
function SQLSetCrypt( const crypt: boolean; const sPassword: string): Integer;
function SQLGetCrypt( const sPassword: String; var sCryptState: String): Integer;

function fieldsString( const fields: array of String; alias: String=''  ): String;
function GetDriverID(CnxDriver: String): TFDRDBMSKind;
function DatabaseExists( const nameDB: String): boolean;
function GetApplicationPath(LocalPath: Boolean): String;
function SetFDParams( const fldNames: Array Of String;
                      const fldValues: Array Of Variant): TFDParams; overload;
function SetFDParams(const fldNames:  Array of String;
                     const fldValues: Array of Const): TFDParams; overload;

procedure SetDataBaseParams( const Context: String);
procedure LoadConnectSettings( const ProgDataPath: String; var ASettingsFileName: String);
procedure SaveConnectSettings();

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    System.Variants,
    System.StrUtils,
    System.IOUtils,
    System.Math,

    uLib.Base,
    uLib.Helpers
    ;


{$REGION '** SQLite definitions **'}
{ TFDSQLiteService }

function TFDM.SQLiteSetPassword( Const sNewPass, sOldPass: String): Integer;
begin
  Cnx.Connected:=false;
  try
    SQLiteSecurity.Password := Cnx.Params.Values['Encrypt']+':'+sOldPass;
    SQLiteSecurity.ToPassword := Cnx.Params.Values['Encrypt']+':'+sNewPass;
    SQLiteSecurity.ChangePassword;
    result:=DB_SUCCESSFUL;
  except
    result:=DB_PASSWORD_ERROR;
  end;
end;

function TFDM.SQLiteSetCrypt(encrypt: Boolean; const sPassword: string): Integer;
begin
  Cnx.Connected:=false;
  try
    SQLiteSecurity.Database:= Cnx.Params.Database;
    //SQLITE_CRYPT_ALGO+':' +SQLITE_PASSWORD;
    SQLiteSecurity.Password:= Cnx.Params.Values['Encrypt']+':'+
                                   sPassword;
    if Encrypt then
       begin
         SQLiteSecurity.SetPassword;
       end
    else
       SQLiteSecurity.RemovePassword;
    Cnx.Connected:=True;
    result:=DB_SUCCESSFUL;
  except
    result:=DB_PASSWORD_ERROR;
  end;
end;

function TFDM.SQLiteGetCrypt( const sPassword: String; var sCrypted: String): Integer; // SqLite
begin
  Cnx.Connected:=false;
  try
    SQLiteSecurity.Database:= Cnx.Params.Database;
    SQLiteSecurity.Password:= Cnx.Params.Values['Encrypt']+':'+sPassword;
    sCrypted := SQLiteSecurity.CheckEncryption;
    Cnx.Connected:=True;
    result:=DB_SUCCESSFUL;
  except
    result:=DB_CONNECTION_ERROR;
  end;
end;

function SQLChangePass(Const newPass, oldPass: String): Integer;
begin
  FDM:=TFDM.Create(Nil);
  try
    Result:=FDM.SQLiteSetPassword(newPass, oldPass);
  finally
    FreeAndNil(FDM);
  end;
end;

function SQLSetCrypt(Const crypt: boolean; const sPassword: string): Integer;
begin
  FDM:=TFDM.Create(Nil);
  try
    Result:=FDM.SQLiteSetCrypt(Crypt, sPassword);
  finally
    FreeAndNil(FDM);
  end;
end;

function SQLGetCrypt( const sPassword: String; var sCryptState: String): Integer;
begin
  FDM:=TFDM.Create(Nil);
  try
    Result:=FDM.SQLiteGetCrypt(sPassword, sCryptState);
  finally
    FreeAndNil(FDM);
  end;
end;
{$ENDREGION}

(*
function SetSchema(schema: String): String;
begin
  if Not Schema.IsEmpty And (Schema[Length(Schema)]<>'.') then
     Schema:=Schema+'.';
  result:=Schema;
end;
*)


procedure  SetFlds( OJSON: TJSONObject;
                   const fields: array of String;
                   const values: array of const); overload;
Var I: Integer;
begin
  for I := Low(values) to High(values) do
   SetStr(OJSON,fields[I],AssignVal(Values[I]).DeQuotedString);
end;

function GetDriverID(CnxDriver: String): TFDRDBMSKind;
var i,t: Integer;
begin
  i:=1;
  T:=TFDRDBMSKinds.Unknown;
  repeat
     case i of
      1: T:=IfThen(CnxDriver.ToUpper='ORA',TFDRDBMSKinds.Oracle,0);
      2: T:=IfThen(CnxDriver.ToUpper='MSSQL',TFDRDBMSKinds.MSSQL,0);
      3: T:=IfThen(CnxDriver.ToUpper='MYSQL',TFDRDBMSKinds.MySQL,0);
      4: T:=IfThen(CnxDriver.ToUpper='PG',TFDRDBMSKinds.PostgreSQL,0);
      5: T:=IfThen(CnxDriver.ToUpper='IB',TFDRDBMSKinds.Interbase,0);
      6: T:=IfThen(CnxDriver.ToUpper='FB',TFDRDBMSKinds.Firebird,0);
      7: T:=IfThen(CnxDriver.ToUpper='SQLITE',TFDRDBMSKinds.SQLite,0);
     end;
     Inc(i);
  until (I>7) Or (T<>TFDRDBMSKinds.Unknown);
  result:=t;
end;

function setQueryPaged(Const dbTable, sFields: String;
                          sWhere: String='';
                          sOrderBy: String='';
                          fromRow:  integer=1;
                          Rows: Integer=0): String;

  function getFieldsAs(sFields: String): String;
  Var St,S: String;
      P: Integer;
  begin
    St:='';
    while sFields<>'' do
     begin
       S:=GetStr(sFields,1,',');
       P:=Pos(',',sFields);
       if P=0 then
          P:=Length(S);
       Delete(sFields,1,P+1);
       if (S.CountChar(' ')>0) then
          begin
            if Pos(' as ',S)>0 then
               S:=GetStr(S,3,' ')
            else
               S:=GetStr(S,2,' ');
            //S:=Trim(Copy(S,Pos(sDiv,S)+Length(sDiv)+1,Length(S)));
          end;
       St:=St+','+S;
     end;
    if Copy(St,1,1)=',' then
       Delete(St,1,1);
    Result:=St;
  end;

var
   sFlds,
   stCmd: String;
begin
  sFlds:=sFields.ToLower;
  if sFlds='' then
     sFlds:='*';
  {Case RDBMSKind of
   TFDRDBMSKinds.PostgreSQL:  ;
   TFDRDBMSKinds.Oracle:      ;
   TFDRDBMSKinds.MSSQL: sCmd.Add('IF (@@ROWCOUNT=0)');
   TFDRDBMSKinds.SQLite: ;
  End;}
  stCmd:=
    ';WITH MyCTE AS '#13+
    '  (SELECT '+sFlds+', ROW_NUMBER() OVER (ORDER BY ';
  if (sOrderBy<>'') then
     stCmd:=stCmd+sOrderBy+')'
  else
     stCmd:=stCmd+'@@ROWCOUNT)';
  stCmd:=stCmd+' AS ROWNUM'#13+
    '     FROM '+dbTable+' WITH (NOLOCK)'#13;
  If (sWhere<>'') Then
     stCmd:=stCmd+'    WHERE '+sWhere;
  sFlds:=getFieldsAs(sFlds);

  stCmd:=stCmd+')'#13+
    ' SELECT '+sFlds+','#13+
    '        (SELECT MAX(ROWNUM)'#13+
    '           FROM myCTE WITH (NOLOCK)) AS TOTALROWS, ROWNUM AS LASTROW'#13+
    '   FROM myCTE WITH (NOLOCK)'#13;
  if Rows>0 then
     stCmd:=stCmd+
    '  WHERE RowNum BETWEEN '+IntToStr(fromRow)+' AND '+
             IntToStr(FromRow+Rows-1);
  result:=stCmd+';';
end;

function sqlWhere( const fldNames:  Array of String;
                   const fldValues: Array of Const): String; overload;
var I: Integer;
    fName: String;
    Str: TStringBuilder;
begin
  Str:=TStringBuilder.Create;
  for I := Low(fldNames) to High(fldNames) do
   begin
     fName:=fldNames[I].ToLower;
     if I>Low(fldNames) then
        Str.Append(' AND ');
     Str.Append('('+fName+'='+AmpFilter(AssignVal(fldValues[I]))+')');
   end;
  result:=Str.ToString;
  Str.Destroy;
end;

function sqlWhere(pParams: TFDParams): String; overload;
Var I: Integer;
    fName: String;
    Str: TStringBuilder;
begin
  Str:=TStringBuilder.Create;
  for I := 0 to pParams.count-1 do
   begin
     fname:=pParams[I].Name;
     if I>0 then
        Str.Append(' AND ');
     Str.Append('('+fName+'='+AmpFilter(AssignVal(pParams[I].Value))+')');
   end;
  result:=Str.ToString;
  Str.Destroy;
end;

function SetFDParams( const fldNames: Array Of String;
                      const fldValues: Array Of Variant): TFDParams; overload;
Var
    I:  Integer;
    fName: String;
Begin
  result:=Nil;
  for I:=Low(fldNames) To High(fldNames) Do
   Begin
     if result=nil then
        TFDParams.Create;
     fName:=fldNames[I].ToLower;
     result.Add(fname,fldValues[I]);
   End;
end;

function SetFDParams( const fldNames:  Array of String;
                      const fldValues: Array of Const): TFDParams; overload;
var I: Integer;
    fName: String;
begin
  result:=Nil;
  for I := Low(fldNames) to High(fldNames) do
   begin
     if result=Nil then
        result:=TFDParams.Create;
     fName:=fldNames[I].ToLower;
     case fldValues[I].VType of
      vtBoolean: result.Add(fName,fldValues[I].VBoolean);
      vtInt64: result.Add(fName,fldValues[I].VInt64^);
      vtInteger: result.Add(fName,fldValues[I].VInteger);
      vtChar: result.Add(fName,fldValues[I].VChar);
      vtWideChar: result.Add(fName,fldValues[I].VWideChar);
      vtExtended,
      vtCurrency: result.Add(fName,fldValues[I].VExtended^);
      vtPChar: result.Add(fName,UnicodeString(fldValues[I].VPChar^));
      vtPWideChar: result.Add(fName,fldValues[I].VPWideChar^);
      vtString: result.Add(fName,fldValues[I].VString^).Size:=MaxInt;
      vtWideString: result.Add(fName,WideString(fldValues[I].VWideString)).Size:=MaxInt;
      vtAnsiString: result.Add(fName,UnicodeString(AnsiString(fldValues[I].VAnsiString))).Size:=MaxInt;
      vtUnicodeString: result.Add(fName,UnicodeString(fldValues[I].VUnicodeString)).Size:=MaxInt;
     end;
   end;
end;

procedure TFDM.SetHeaderEnabled(const Value: boolean);
begin
  FHeaderEnabled := Value;
end;

procedure TFDM.SetautoCommit(const Value: Boolean);
begin
  FautoCommit := Value;
end;

procedure TFDM.DataModuleCreate(Sender: TObject);
begin
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  MSSQLDriver:=TFDPhysMSSQLDriverLink.Create(Nil);
  MySQLDriver:=TFDPhysMySQLDriverLink.Create(Nil);
  PgDriver:=TFDPhysPgDriverLink.Create(Nil);
{$ENDIF}
  HeaderEnabled:=false;
  autoCommit:=false;
  Cnx.Connected:=False;
  //Cmd.UpdateOptions.AssignedValues[rvServerOutput]:=true;
end;

procedure TFDM.DataModuleDestroy(Sender: TObject);
begin
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  MSSQLDriver.Destroy;
  MySQLDriver.Destroy;
  PgDriver.Destroy;
{$ENDIF}
end;

function fnCreateQuery(sQry: String=''; pParams: TFDParams=Nil): TFDQuery;
begin
  result:=TFDQuery.Create(FDM);
  result.Connection:=FDM.Cnx;
  result.SQL.Text:=sQry;
  if pParams<>Nil then
     result.Params:=pParams;
  if (sQry<>'') then
     Try
       result.OpenOrExecute;
     Except
       result.SQL.SaveToFile('qry'+FormatDateTime('mmddhhnnss',Now)+'.txt');
     End;
end;

function fnCreateCommand(sQry: String=''; pParams: TFDParams=Nil): TFDCommand;
begin
  result:=TFDCommand.Create(FDM);
  result.Connection:=FDM.Cnx;
  result.CommandText.Text:=sQry;
  if pParams<>Nil then
     result.Params:=pParams;
  if sQry<>'' then
     Try
       result.OpenOrExecute;
     Except
       result.CommandText.SaveToFile('cmd'+FormatDateTime('mmddhhnnss',Now)+'.txt');
     End;
end;

procedure TFDM.SetHeader(aHeader: TStringList; ForAudit: Boolean=false);
Var
  userId,
  userName: String;
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  Session: TDSSession;
{$ENDIF}
begin
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  if forAudit then
     begin
       Session:= TDSSessionManager.GetThreadSession;
       userId:= Session.GetData('loginID');
       userName:= Trim(Session.GetData('FirstName')+' '+Session.GetData('LastName'));
       Case Cnx.RDBMSKind of
        TFDRDBMSKinds.MYSQL:
          begin
            aHeader.Add('SET @user_id:='+userId+';');
            aHeader.Add('SET @user_name:='+QuotedStr(username)+';');
          end;
        TFDRDBMSKinds.PostgreSQL:
          begin
            //aHeader.Add('SELECT set_config(''usession.id'','+userId+',FALSE);');
            //aHeader.Add('SELECT set_config(''usession.name'','+QuotedStr(username)+',FALSE);');
            aHeader.Add('SET usession.id='+userId+';');
            aHeader.Add('SET usession.name='+QuotedStr(username)+';');
          end;
        TFDRDBMSKinds.SQLite: ;
        TFDRDBMSKinds.MSSQL:
          begin
            aHeader.Add('EXEC sp_set_session_context N''user_id'', '+userId+', 0;');
            aHeader.Add('EXEC sp_set_session_context N''user_name'', '+QuotedStr(username)+', 0;');
          end;
       end;
     end;
{$ENDIF}
  if HeaderEnabled or ForAudit then
     Case Cnx.RDBMSKind of
      TFDRDBMSKinds.MYSQL:
        begin
        end;
      TFDRDBMSKinds.PostgreSQL:
        begin
        end;
      TFDRDBMSKinds.SQLite: ;
      TFDRDBMSKinds.MSSQL:
        begin
          aHeader.Add('SET DATEFORMAT YMD;');
          aHeader.Add('SET NOCOUNT ON;');
           {result.Add('SET DATEFIRST 7;'); //, -- 1 = Monday, 7 = Sunday
           result.Add('SET LANGUAGE US_ENGLISH;');}
        end;
     end;
end;

function valuesString( const values: array of const): String;
Var I: Integer;
    sFields: String;
begin
  sFields:='';
  for I := Low(values) to High(values) do
    sFields:=sFields+IfThen(I=0,'',',')+AmpFilter(AssignVal(Values[I]));
  result:=trim(sFields);
end;

function fieldsString( const fields: array of String; alias: String=''  ): String;
Var I: Integer;
    sFields: String;
begin
  sFields:='';
  for I := Low(fields) to High(fields) do
    sFields:=sFields+IfThen(I=0,'',',')+alias+fields[I];
  result:=trim(sFields);
end;


function fieldSetValues( const fields: array of String;
                         const values: array of const;
                               alias: String=''  ): String; overload;
Var I: Integer;
    sFields: String;
begin
  sFields:='';
  for I := Low(fields) to High(fields) do
    sFields:=sFields+
          IfThen(I=0,'',',')+alias+fields[I]+'='+
          AmpFilter(AssignVal(Values[I]));
  result:=trim(sFields);
end;

function fieldSetValues( const fields: array of String;
                         const values: string;
                               alias: String=''  ): String; overload;
Var I: Integer;
    sFields: String;
begin
  sFields:='';
  for I := Low(fields) to High(fields) do
    sFields:=sFields+
       IfThen(I=0,'',',')+alias+fields[I]+'='+
       AmpFilter(GetStr(Values,succ(I),','));
  result:=trim(sFields);
end;

function fieldSetValues( const fields: String;
                         const values: string;
                               alias: String='' ): String; overload;
Var I: Integer;
    sFields: String;
begin
  sFields:='';
  for I := 1 to getMaxFields(fields,',') do
    sFields:=sFields+
             IfThen(I=1,'',',')+alias+GetStr(fields,I,',')+'='+
             AmpFilter(GetStr(Values,I,','));
  result:=trimS(sFields);
end;

function TFDM.ParamsToJSONObject(params: TFDParams): TJSONObject;
Var I: Integer;
    sJSON: String;
begin
  sJSON:='{';
  for I := 0 to params.count-1 do
    begin
      if I>0 then
         sJSON:=sJSON+',';
      sJSON:=sJSON+'"'+Params[I].Name+'":';
      if VarIsNumeric(Params[I].Value) then
         sJSON:=sJSON+VarToStr(Params[I].Value)
      else
         sJSON:=sJSON+'"'+VarToStr(Params[I].Value)+'"';
    end;
  sJSON:=sJSON+'}';
  result:=TJSONObject.ParseJSONValue(sJSON) as TJSONObject;
end;

function TFDM.cmdExecute(sCmd: TStringList; pParams: TFDParams=Nil): Integer;
Var
   aHeader: TStringList;
begin
  aHeader:=TStringList.Create;

  SetHeader(aHeader);
  aHeader.Add(sCmd.Text);
  Cmd.CommandText.Clear;
  Cmd.CommandText.Assign(aHeader);
  if HeaderEnabled then
     case Cnx.RDBMSKind Of
      TFDRDBMSKinds.MSSQL:;
      TFDRDBMSKinds.MYSQL: ;
      TFDRDBMSKinds.SQLite: ;
      TFDRDBMSKinds.PostgreSQL:
        begin
          //Cmd.CommandText.Add('end$block_task$;');
        end;
     end;
  if Not autoCommit then
     Cnx.StartTransaction;
  //Result:=DB_COMMAND_ERROR;
  Try
    Cmd.Execute();
    if Not autoCommit then
       Cnx.Commit;
    Result:=DB_SUCCESSFUL;
  except
   on E: EFDDBEngineException do  begin
           if Not autoCommit then
              Cnx.Rollback;
           Cmd.CommandText.SaveToFile('cmd'+FormatDateTime('mmddhhnnss',Now)+'.txt');
           raise Exception.Create(E.Message);
         end;
  End;
  aHeader.Destroy;
end;

function TFDM.cmdExecute(sCmd: String; pParams: TFDParams=Nil): Integer;
var
  TS: TStringList;
begin
  TS:=TStringList.Create;
  TS.Text:=sCmd;
  cmdExecute(TS,pParams);
  TS.Destroy;
end;

function TFDM.execTrans( cSQL, sDecl: TStringList;
                         sFields: String;
                         pParams: TFDParams): TJSONObject;
var
   cCmd: TStringList;
begin
  cCmd:=TStringList.Create;
  SetHeader(cCmd,true);
  case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     begin
       cCmd.Add('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;');
       cCmd.Add('DECLARE ');
       if sDecl<>Nil then
          cCmd.Add(sDecl.Text);
       cCmd.Add('  @ERROR INT=0;');
       cCmd.Add('BEGIN TRANSACTION;');
       cCmd.Add('BEGIN TRY');
     end;
   TFDRDBMSKinds.MySQL:
     begin
       cCmd.Add('SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE;');
       cCmd.Add('DECLARE @Error INT DEFAULT 0;');
       if sDecl<>Nil then
          cCmd.Add(sDecl.Text);
       cCmd.Add('START TRANSACTION;');
     end;
   TFDRDBMSKinds.PostgreSQL:
     begin
       cCmd.Add('DECLARE');
       if sDecl<>Nil then
          cCmd.Add(sDecl.Text);
       cCmd.Add('  _error INTEGER DEFAULT 0;');
       cCmd.Add('BEGIN');
       cCmd.Add('  BEGIN;');
     end;
   TFDRDBMSKinds.SQLite: ;
  end;
  //--------------------------------
  cCmd.Add(cSQL.Text);
  //--------------------------------
  case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     begin
       cCmd.Add('  IF @ERROR>0');
       cCmd.Add('  BEGIN');
       cCmd.Add('    SELECT @ERROR error;');
       cCmd.Add('    RAISERROR(''ERROR IN TRASACTION SQL'', @ERROR,1);');
       cCmd.Add('  END;');
       cCmd.Add('  COMMIT TRANSACTION;');
       cCmd.Add('  SELECT @ERROR error'+ifThen(sFields<>'',', '+sFields,'')+';');
       cCmd.Add('END TRY');
       cCmd.Add('BEGIN CATCH');
       cCmd.Add('  IF (@@TRANCOUNT>0) or (@ERROR<>0) ');
       cCmd.Add('     ROLLBACK;');
       cCmd.Add('  DECLARE @ErrMsg nvarchar(4000), @ErrSeverity int;');
       cCmd.Add('  SELECT @ErrMsg = ERROR_MESSAGE(),');
       cCmd.Add('         @ErrSeverity = ERROR_SEVERITY()');
       cCmd.Add('  SELECT -1 error, @ErrMsg errmsg, @errseverity errseverity;');
       cCmd.Add('  RAISERROR(@ErrMsg, @ErrSeverity, 1);');
       cCmd.Add('END CATCH;');
     end;
   TFDRDBMSKinds.MySQL:
     begin
       cCmd.Add('SELECT @Error := @@error;');
       cCmd.Add('IF (@Error <> 0) THEN');
       cCmd.Add('  ROLLBACK;');
       cCmd.Add('  SELECT concat(''Transaction failed with error: '', @Error);');
       cCmd.Add('ELSE');
       cCmd.Add('  COMMIT;');
       cCmd.Add('END IF;');
       cCmd.Add('SELECT @Error error'+ifThen(sFields<>'',', '+sFields,'')+';');
     end;
   TFDRDBMSKinds.PostgreSQL:
     begin
       cCmd.Add('  GET STACKED DIAGNOSTICS _error = RETURNED_SQLSTATE;');
       cCmd.Add('  IF _error <> ''00000'' THEN');
       cCmd.Add('    ROLLBACK;');
       cCmd.Add('    RAISE NOTICE ''Transaction rolled back due to error: %'', _error;');
       cCmd.Add('  ELSE');
       cCmd.Add('    COMMIT;');
       cCmd.Add('  END IF;');
       cCmd.Add('  SELECT _error error'+ifThen(sFields<>'',', '+sFields,'')+';');
       cCmd.Add('END;');
     end;
  end;

  Qry.SQL.Clear;
  Qry.SQL.Assign(cCmd);
  if pParams<>Nil then
     Qry.Params:=pParams;
  Qry.Prepare;
  if Not autoCommit then
     Cnx.StartTransaction;
  Try
    Qry.OpenOrExecute;
    Result:=Qry.AsJSONObject;
    if Not autoCommit then
       Cnx.Commit;
  except
   on E: EFDDBEngineException do  begin
           if Not autoCommit then
              Cnx.Rollback;
           Qry.SQL.SaveToFile('qry'+FormatDateTime('mmddhhnnss',Now)+'.txt');
           raise Exception.Create(E.Message);
         end;
  End;
  cCmd.Destroy;
End;


function TFDM.cmdAdd( const dbTableName,
                            Context: String;
                            fldsReturn: String=''): TJSONObject;

  procedure InsTable( iSQL: TStringList;
                      const DBTABLE: String;
                            pNameFlds: String;
                      const pValFlds: String);
  Var
    sIns,
    seqName: String;
  begin
    sIns:=
     'INSERT INTO '+DBTABLE+' ('+pNameFlds+')'+#13+
     '       VALUES ('+pValFlds+')';
    case cnx.RDBMSKind Of
     TFDRDBMSKinds.MSSQL:
       begin
         seqName:=GetStr(DBTABLE,2,'.');
         if seqName.IsEmpty then
            seqName:=DBTABLE;
         seqName:='seq_'+seqName;
         iSQL.Add(sIns+';');
         iSQL.Add('IF EXISTS(SELECT name FROM sys.sequences WHERE name='+QuotedStr(seqName)+')');
         iSQL.Add('   SELECT current_value id FROM sys.sequences WHERE name='+QuotedStr(seqName)+';');
         iSQL.Add('ELSE');
         iSQL.Add('   SELECT IDENT_CURRENT(' + QuotedStr(DBTABLE) + ') AS id;');
       end;
     TFDRDBMSKinds.MYSQL:
       begin
         iSQL.Add(sIns+';');
         iSQL.Add('SELECT LAST_INSERT_ID() AS id;');
       end;
     TFDRDBMSKinds.SQLite,
     TFDRDBMSKinds.PostgreSQL:
       iSQL.Add(sIns+' RETURNING id;');
    end;
  end;

Var
  sCmd: TStringList;
  AJSON: TJSONArray;
  sFields,
  sValues,
  sSetVal: String;
begin
  sCmd:=TStringList.create;
  try
    GetFieldsvalues(Context,sFields,sValues,sSetVal);
    InsTable(sCmd,dbTableName,sFields,sValues);
    result:=execTrans(sCmd,Nil,fldsReturn,nil);
  finally
    sCmd.Destroy;
  end;
End;

function TFDM.cmdUpd(Const dbTableName, Context: String;
                           Condition: String): TJSONObject;

  procedure UpdTable( iSQL: TStringList;
                      const DBTABLE: String;
                      const setVals: String;
                      const condition: String);
  begin
    iSQL.Add('UPDATE '+DBTABLE+'');
    iSQL.Add('   SET '+setVals);
    iSQL.Add(' WHERE '+condition+';');
  end;

Var
  sCmd: TStringList;
  AJSON: TJSONArray;
  sFields,
  sValues,
  sSetVal: String;
begin
  sCmd:=TStringList.create;
  try
    GetFieldsvalues(Context,sFields,sValues,sSetVal);
    updTable(sCmd,dbTableName,sSetVal,condition);
    result:=execTrans(sCmd,Nil,'',Nil);
  finally
    sCmd.Destroy;
  end;
End;

function TFDM.cmdDel(Const dbTableName, sWhere: String): TJSONObject;
Var
   sCmd: TStringList;
begin
  sCmd:=TStringList.create;
  try
    sCmd.Add('DELETE FROM '+dbTableName);
    sCmd.Add(' WHERE '+sWhere+';');
    result:=execTrans(sCmd,Nil,'',Nil);
  finally
    sCmd.Destroy;
  end;
end;

function TFDM.GetRecords(Const sQuery: String; pParams: TFDParams=Nil): TJSONArray;
Var aHeader: TStringList;
begin
  aHeader:=TStringList.Create;
  SetHeader(aHeader);
  Qry.SQL.Clear;
  Qry.SQL.Assign(aHeader);
  Qry.SQL.Add(sQuery);
  if pParams<>Nil then
     Qry.Params:=pParams;
  Qry.Prepare;
  try
    Qry.Open;
  except
    on E: EFDDBEngineException do begin
            Qry.SQL.SaveToFile('qry'+FormatDateTime('mmddhhnnss',Now)+'.txt');
            raise Exception.Create(E.Message);
          end;
  end;
  aHeader.Destroy;
  Result:=Qry.AsJSONArray();
end;


procedure TFDM.CnxBeforeConnect(Sender: TObject);
begin
  Cnx.Params.Text:=ADatabaseServer.Text;
  Cnx.DriverName:=ADatabaseServer.Values['DriverId'];
  RDBMSKind:=Cnx.RDBMSKind;
  FDM.autoCommit:=False;
  case Cnx.RDBMSKind of
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
    TFDRDBMSKinds.MSSQL:
      ;
    TFDRDBMSKinds.MySQL:
      begin
        MySQLDriver.VendorHome:=ADriverVendor.Values['VendorHome'];
        MySQLDriver.VendorLib:=ADriverVendor.Values['VendorLib'];
      end;
    TFDRDBMSKinds.PostgreSQL:
      begin
        FDM.autoCommit:=True;
        PGDriver.VendorHome:=ADriverVendor.Values['VendorHome'];
        PGDriver.VendorLib:=ADriverVendor.Values['VendorLib'];
      end;
{$ENDIF}
    TFDRDBMSKinds.SQLite:
      begin
        SQLiteSecurity.Database:=ASQLiteSecurity.Values['Database'];
        SQLiteSecurity.Password:=ASQLiteSecurity.Values['Password'];
      end;
  end;
end;

//-------------------------------------------------------
//
//-------------------------------------------------------

function GetData( const sQuery: String;
                   pParams: TFDParams=nil): TJSONArray; overload;
begin
  try
    Result:=FDM.GetRecords(sQuery,pParams);
  finally
  end;
end;

function GetData( const sQuery: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONArray; overload;
Var
   pParams: TFDParams;
begin
  pParams:=SetFDParams(fldNames,fldValues);
  try
    Result:=FDM.GetRecords(sQuery,pParams);
  finally
    pParams.Destroy;
  End;
end;

function GetData( sQuery: TStrings;
                  pParams: TFDParams=nil): TJSONArray;  overload;
begin
  try
    Result:=FDM.GetRecords(sQuery.Text,pParams);
  finally
  end;
end;

function GetData( sQuery: TStrings;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONArray;  overload;
Var
   pParams: TFDParams;
begin
  pParams:=SetFDParams(fldNames,fldValues);
  try
    Result:=FDM.GetRecords(sQuery.Text,pParams);
  finally
    pParams.Destroy;
  End;
end;

function AddData( Const dbTableName: string;
                        Context: TJSONValue;
                        fldsReturn: String='' ): TJSONObject; Overload;
begin
  try
    Result:=FDM.cmdAdd(dbTableName,Context.ToString,fldsReturn);
  finally
  end;
end;

function AddData( Const dbTableName,
                        Context: String;
                        fldsReturn: String='' ): TJSONObject; Overload;
begin
  try
    Result:=FDM.cmdAdd(dbTableName,Context,fldsReturn);
  finally
  end;
end;


function AddData( Const dbTableName: string;
                  const fldNames:  Array of String;
                  const fldValues: Array of const;
                        fldsReturn: String='' ): TJSONObject; Overload;
var Context: String;
begin
  Context:='';
  SetFldsJSON(Context,fldNames,fldValues);
  try
    Result:=FDM.cmdAdd(dbTableName,Context,fldsReturn);
  finally
  end;
end;

function UpdData( const dbTableName: String;
                        Context: TJSONValue;
                        Condition: String): TJSONObject; Overload;
begin
  try
    Result:=FDM.cmdUpd(dbTableName,Context.ToString,Condition);
  finally
  end;
End;

function UpdData( const dbTableName, Context, Condition: String): TJSONObject; Overload;
begin
  try
    Result:=FDM.cmdUpd(dbTableName,Context,Condition);
  finally
  end;
End;

function UpdData( const dbTableName: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const;
                        Condition: String): TJSONObject; Overload;
var Context: string;
begin
  Context:='{}';
  SetFldsJSON(Context,fldNames,fldValues);
  try
    Result:=FDM.cmdUpd(dbTableName,Context,Condition);
  finally
  end;
end;

function DelData( Const dbTableName, condition: String): TJSONObject;
begin
  try
    Result:= FDM.cmdDel(dbTableName,condition);
  finally
  end;
end;

function ExecTransact( cSQL: TStringList;
                       sDecl: TStringList=nil;
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject; Overload;
begin
  result:= FDM.execTrans(cSQL, sDecl, sFields, pParams);
end;

function ExecCmd( const sCommands: String; pParams: TFDParams=Nil): Integer; Overload;
begin
  if sCommands='' then
     Exit(0);
  try
    Result:=FDM.cmdExecute(sCommands,pParams);
  finally
  end;
end;

function ExecCmd( sCommands: TStrings; pParams: TFDParams=Nil): Integer; Overload;
begin
  result:=ExecCmd( sCommands.text,pParams);
end;

function ExecCmd( const sCommands: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): Integer; overload;
var pParams: TFDParams;
begin
  if sCommands='' then
     Exit(0);
  pParams:=setFDParams(fldNames,fldValues);
  Try
    Result:=FDM.cmdExecute(sCommands,pParams);
  finally
    pParams.Destroy;
  end;
end;

function ExecCmd( sCommands: TStrings;
                   const fldNames:  Array of String;
                   const fldValues: Array of const): Integer; Overload;
begin
  result:= ExecCmd( sCommands.Text,fldNames,fldValues);
end;


function DatabaseExists(const nameDB: String): boolean;
Var
  JSON: TJsonObject;
  Sqry: String;
begin
  if RDBMSKind in [TFDRDBMSKinds.SQLite] then
     begin
       result:=true;
       exit;
     end;
  case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     SQry:=
      'SELECT ISNULL(db_id('+QuotedStr(nameDB)+'),0) db_exists;';
   TFDRDBMSKinds.MySQL:
     SQry:=
      'SELECT 1 db_exists'+
      '  FROM INFORMATION_SCHEMA.SCHEMATA'+
      ' WHERE SCHEMA_NAME='+QuotedStr(nameDB)+'';
   TFDRDBMSKinds.PostgreSQL:
     SQry:=
      'SELECT 1 db_exists'+
      '  FROM pg_catalog.pg_database'+
      ' WHERE {lcase(datname)} = {lcase('+QuotedStr(nameDB) +')};';
  end;
  JSON:=GetData(SQry)[0] as TJSONObject;
  result:=false;
  if Assigned(JSON) then
     result:=GetInt(JSON,'db_exists')>0;
  JSON.Destroy;
end;

function GetApplicationPath(LocalPath: Boolean): String;
var
   AppName,
   AppStationName,
   ProgDataPath: String;
begin
  AppStationName := GetEnvironmentVariable('COMPUTERNAME');
  AppName := ChangeFileExt(ExtractFileName(paramstr(0)), ''); // Ohne Endung
{$IF DEFINED (Linux) or DEFINED (MACOS)}
  ProgDataPath := IncludeTrailingPathDelimiter(GetHomePath) + '.config/' +
                   AppName + PathDelim;
  //ProgDataPath := 'Data' + PathDelim;    // Development
{$ENDIF}
{$IFDEF MSWINDOWS}
  ProgDataPath:= IncludeTrailingPathDelimiter(GetHomePath)+AppName+PathDelim;
{$ENDIF}
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
  ProgDataPath := TPath.GetDocumentsPath+PathDelim;
{$ENDIF}
  if LocalPath then
     begin
       ProgDataPath:=ExtractFilePath(paramstr(0));
     end;
  writeln(ProgDataPath);
  if Not DirectoryExists(ProgDataPath) then
     begin
       ForceDirectories(ProgDataPath);
     end;
  result:=ProgDataPath;
end;

procedure SaveConnectSettings();
var
  pJSON,
  aJSON,
  sJSON,
  tJSON: String;
begin
  try
    tJSON:='';
    aJSON:='';
    sJSON:='';
    SetInt(tJSON,'http_port',HTTP_PORT);
    SetJSON(aJSON,'host',tJSON);

    SetStr(sJSON,'driverId',ADatabaseServer.Values['DriverID']);
    SetStr(sJSON,'server',ADatabaseServer.Values['Server']);
    SetStr(sJSON,'database',ADatabaseServer.Values['Database']);
    SetStr(sJSON,'user_Name',ADatabaseServer.Values['User_Name']);
    SetStr(sJSON,'password',ADatabaseServer.Values['Password']);
    SetInt(sJSON,'data_Port',ADatabaseServer.Values['Data_Port']);
    SetStr(sJSON,'OSAuthent',ADatabaseServer.Values['OSAuthent']);
    SetJSON(aJSON,'connection',sJSON);

    pJSON:='';
    SetStr(pJSON,'vendorLib',ADriverVendor.Values['VendorLib']);
    SetStr(pJSON,'vendorHome',ADriverVendor.Values['VendorHome']);
    SetJSON(aJSON,'provider',pJSON);

    pJSON:='';
    SetStr(pJSON,'name',ACustDatabase.Values['name']);
    SetStr(pJSON,'role',ACustDatabase.Values['role']);
    SetJSON(aJSON,'cust_data',pJSON);

    saveTextfile(ASettingsFileName, AJSON, false); // Crypted
  finally
  end;
end;

procedure LoadConnectSettings(Const ProgDataPath: String; var ASettingsFileName: String);
var
   JSON,
   aJSON: String;
begin
  ASettingsFileName:=ProgDataPath+ASettingsFileName;
  if Not DirectoryExists(ProgDataPath) then
     begin
       ForceDirectories(ProgDataPath);
     end;
  if Not FileExists(ASettingsFileName) then
     SaveConnectSettings();
  JSON := loadTextfile(ASettingsFileName, false); // Crypted
  // -----------------------------------------------
  AJSON := GetStr(JSON,'host');
  HTTP_PORT:=GetInt(AJSON,'http_port');
  // -----------------------------------------------
  AJSON := GetStr(JSON,'connection');
  ADatabaseServer.Clear;
  ADatabaseServer.AddPair('DriverId', GetStr(AJSON, 'driverId'));
  ADatabaseServer.AddPair('Server', GetStr(AJSON, 'server'));
  ADatabaseServer.AddPair('Database', GetStr(AJSON, 'database'));
  ADatabaseServer.AddPair('User_Name', GetStr(AJSON, 'user_Name'));
  ADatabaseServer.AddPair('Password', GetStr(AJSON, 'password'));
  ADatabaseServer.AddPair('Data_Port', GetStr(AJSON, 'data_Port'));
  ADatabaseServer.AddPair('OSAuthent', GetStr(AJSON, 'OSAuthent'));
  ADefConnection.Text:=ADatabaseServer.Text;
  // -----------------------------------------------
  AJSON := GetStr(JSON,'provider');
  ADriverVendor.Clear;
  ADriverVendor.AddPair('VendorLib', GetStr(AJSON, 'vendorLib'));
  ADriverVendor.AddPair('VendorHome', GetStr(AJSON, 'vendorHome'));
  // -----------------------------------------------
  AJSON := GetStr(JSON,'license');
  ACustDatabase.Clear;
  ACustDatabase.AddPair('branch', GetStr(AJSON, 'branch'));
  ACustDatabase.AddPair('serial', GetStr(AJSON, 'serial'));
end;

procedure SetDataBaseParams(Const Context: String);
var database,
    cust_data,
    provider: String;
begin
  database:=getStr(Context,'connection');
  ADatabaseServer.Clear;
  ADatabaseServer.AddPair('driverId', GetStr(database,'driverId'));
  ADatabaseServer.AddPair('server',   GetStr(database,'server'));
  ADatabaseServer.AddPair('database', GetStr(database,'database'));
  ADatabaseServer.AddPair('user_Name',GetStr(database,'user_Name'));
  ADatabaseServer.AddPair('password', GetStr(database,'password'));
  ADatabaseServer.AddPair('data_Port',GetStr(database,'data_Port'));
  ADatabaseServer.AddPair('OSAuthent',GetStr(database,'OSAuthent'));

  provider:=getStr(Context,'provider');
  ADriverVendor.Clear;
  ADriverVendor.AddPair('vendorLib',GetStr(provider,'vendorLib'));
  ADriverVendor.AddPair('vendorHome',GetStr(provider,'vendorHome'));

  cust_data:=getStr(Context,'cust_data');
  ACustDatabase.Clear;
  ACustDatabase.AddPair('name',GetStr(cust_data,'name'));
  ACustDatabase.AddPair('role',GetStr(cust_data,'role'));
end;

initialization
  ASettingsFileName:= ChangeFileExt(ExtractFileName(ParamStr(0)), '.conf');
  ADatabaseServer:=TStringList.Create;
  ADefConnection:=TStringList.Create;
  ADriverVendor:=TStringList.Create;
  ASQLiteSecurity:=TStringList.Create;
  ACustDatabase:=TStringList.Create;
finalization
  ADatabaseServer.Destroy;
  ADefConnection.Destroy;
  ADriverVendor.Destroy;
  ASQLiteSecurity.Destroy;
  ACustDatabase.Destroy;
end.


// .ContentAsString(TEncoding.UTF8)
(*


//---------------------------------
//  SQLite
//---------------------------------
{ TFDSQLiteService }

function TFDService.SQLSetPassword( Const sNewPass, sOldPass: String): Integer;
begin
  FDM.Cnx.Connected:=false;
  try
    FDM.SQLiteSecurity1.Password := FDM.Cnx.Params.Values['Encrypt']+':'+sOldPass;
    FDM.SQLiteSecurity1.ToPassword := FDM.Cnx.Params.Values['Encrypt']+':'+sNewPass;
    FDM.SQLiteSecurity1.ChangePassword;
    result:=DB_SUCCESSFUL;
  except
    result:=DB_PASSWORD_ERROR;
  end;
end;

function TFDService.SQLSetCrypt(encrypt: Boolean; const sPassword: string): Integer;
begin
  FDM.Cnx.Connected:=false;
  try
    FDM.SQLiteSecurity1.Database:= FDM.Cnx.Params.Database;
    //SQLITE_CRYPT_ALGO+':' +SQLITE_PASSWORD;
    FDM.SQLiteSecurity1.Password:= FDM.Cnx.Params.Values['Encrypt']+':'+
                                   sPassword;
    if encrypt then
       begin
         FDM.SQLiteSecurity1.SetPassword;
       end
    else
       FDM.SQLiteSecurity1.RemovePassword;
    result:=DB_SUCCESSFUL;
  except
    result:=DB_PASSWORD_ERROR;
  end;
end;

function TFDService.SQLGetCrypt( const sPassword: String; var sCrypted: String): Integer; // SqLite
begin
  FDM.Cnx.Connected:=false;
  try
    FDM.SQLiteSecurity1.Database:= FDM.Cnx.Params.Database;
    FDM.SQLiteSecurity1.Password:= FDM.Cnx.Params.Values['Encrypt']+':'+sPassword;
    sCrypted := FDM.SQLiteSecurity1.CheckEncryption;
    result:=DB_SUCCESSFUL;
  except
    result:=DB_CONNECTION_ERROR;
  end;
end;

function _SQLChangePass(Const newPass, oldPass: String): Integer;
var FDS: TFDService;
begin
  FDS:=TFDService.Create;
  try
    Result:=FDS.SQLSetPassword(newPass, oldPass);
  finally
    FDS.Destroy;
  end;
end;

function _SQLSetCrypt(crypt: boolean; const sPassword: string): Integer;
var FDS: TFDService;
begin
  FDS:=TFDService.Create;
  try
    Result:=FDS.SQLSetCrypt(Crypt, sPassword);
  finally
    FDS.Destroy;
  end;
end;

function _SQLGetCrypt( const sPassword: String; var sCryptState: String): Integer;
var FDS: TFDService;
begin
  FDS:=TFDService.Create;
  try
    Result:=FDS.SQLGetCrypt(sPassword, sCryptState);
  finally
    FDS.Destroy;
  end;
end;


procedure TFDM.Synchronizer(fLastUpdate: string);
var
  fDataSet: TFDDataSet;
  sIns, sUpd: string;
  i, j, k: integer;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        TThread.Synchronize(nil,
          procedure
          begin
            { LayoutStatus.Visible := True;
              FloatAnimationStatus.Enabled := True;
              LabelStatus.Text := 'Loading data...';
              Application.ProcessMessages; }
          end);

        // fDSList := ClientModule1.GetServerMethods1Client.GetParts(fLastUpdate);
        // fDataSet := TFDJSONDataSetsReader.GetListValue(fDSList, 0);

        TThread.Synchronize(nil,
          procedure
          begin
            { LabelStatus.Text := 'Starting sync process...';
              Application.ProcessMessages; }
          end);
        FDCnx.StartTransaction;
        i := 0;
        j := fDataSet.RecordCount;
        while not fDataSet.Eof do
         begin
           { sUpd := 'UPDATE PARTS SET ';
             sUpd := sUpd + ' LASTUPDATE = ' + TSQLUtil.SQLDateTime
             (fDataSet.FieldByName('LASTUPDATE').AsString);
            sUpd := sUpd + ' WHERE PARTNO = ' + fDataSet.FieldByName
            ('PARTNO').AsString;
            sIns := 'INSERT INTO PARTS') + ')'; }

           k := FDCnx.ExecSQL(sUpd);
           if k = 0 then
           begin
             k := FDCnx.ExecSQL(sIns);
           end;

           if (k = 1) then
           begin
             Inc(i);
             TThread.Synchronize(nil,
               procedure
               begin
                 { LabelStatus.Text := 'Synchronizing ' + i.ToString + ' from ' +
                   j.ToString + '...';
                   Application.ProcessMessages; }
               end);
           end;

           fDataSet.Next;
          // Sleep(100); //demo
         end;

        FDCnx.Commit;

        TThread.Synchronize(nil,
          procedure
          begin
            { FloatAnimationStatus.Enabled := True;
              LayoutStatus.Visible := False;
              Application.ProcessMessages;
              TDialogService.ShowMessage('Synchronized records: ' + i.ToString); }
          end);
      except
        on E: Exception do
        begin
          FDCnx.Rollback;
          // add a better log solution for the client side
          raise Exception.Create('LoadParts: ' + E.Message);
        end;
      end;
    end).Start;
end;


  aDataSet.Open;
  aDataSet.LogChanges := False;
  aDataSet.FetchOptions.RecsMax := aRecs;
  aDataSet.ResourceOptions.SilentMode := True;
  aDataSet.UpdateOptions.LockMode := lmNone;
  aDataSet.UpdateOptions.LockPoint := lpDeferred;
  aDataSet.UpdateOptions.FetchGeneratorsPoint := gpImmediate;

  aDataSet.BeginBatch;
  try
    for i := 1 to aRecs do
      with aDataSet do
      begin
        Append;
        Fields[0].AsInteger := i;
        Fields[1].AsString := 'Record ' + Format('%.*d', [4, i]);
        Fields[2].AsDateTime := Now;
        Post;
      end;
  finally
    aDataSet.EndBatch;
  end;
*)



