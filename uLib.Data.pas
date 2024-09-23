unit uLib.Data;

interface

uses
  Data.DB,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.UITypes,
  System.Generics.Collections,
  FireDAC.Stan.Param,
  FireDAC.Stan.Intf
;


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


function SqlWhere( pParams: TFDParams ): String; Overload;
function SqlWhere( const fldNames:  Array of String;
                   const fldValues: Array of Const): String;  Overload;

function GetDataSet( const sQuery: String;
                           pParams: TFDParams=Nil): TDataSet;
function GetData( const sQuery: String;
                           pParams: TFDParams=Nil): TJSONValue; Overload;
function GetData( sQuery: TStrings;
                  pParams: TFDParams=nil): TJSONValue; Overload;
function GetData( const sQuery: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONValue; Overload;
function GetData( sQuery: TStrings;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONValue; Overload;

function GetData( const sTblName: String;
                  sfields: String;
                  sCondition: String='';
                  sOrder: string=''): TJSONValue; overload;

function AddData( const dbTableName: String;
                        Context: TJSONValue): TJSONObject; Overload;
function AddData( const dbTableName,
                        Context: String): TJSONObject; Overload;
function AddData( const dbTableName: string;
                  const fldNames: Array of String;
                  const fldValues: Array of const): TJSONObject; Overload;

function UpdData( const dbTableName: String;
                        Context: TJSONValue;
                        Condition: String=''): TJSONObject; Overload;
function UpdData( const dbTableName,
                        Context: String;
                        Condition: String=''): TJSONObject; Overload;
function UpdData( const dbTableName: String;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                        Condition: String=''): TJSONObject; Overload;

function DelData( const dbTableName, Condition: String): TJSONObject;

function ExecCmd( const sCommands: String; pParams: TFDParams=Nil; aCommit: Boolean=true): TJSONObject; Overload;
function ExecCmd( sCommands: TStrings; pParams: TFDParams=Nil; aCommit: Boolean=true): TJSONObject; Overload;
function ExecCmd( const sCommands: String;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                  aCommit: Boolean=true): TJSONObject; Overload;
function ExecCmd( sCommands: TStrings;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                  aCommit: Boolean=true): TJSONObject; Overload;

function ExecTransact( cSQL: TStringList;
                       sDecl: TStringList=nil;
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject; Overload;
function ExecTransact( cSQL: String;
                       sDecl: String='';
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject; Overload;

function FieldsString( const fields: array of String; alias: String=''  ): String;
function GetDriverID(CnxDriver: String): TFDRDBMSKind;
function DatabaseExists( const nameDB: String): boolean;

Function sqlInsert( const filename: String;
                          aJSON: TJSONObject;
                          fldReturn: String=''): String; overload;

Function sqlInsert( const filename, aJSON: string;
                          fldReturn: String=''): String; overload;

Function sqlUpdate( const filename: String;
                          aJSON: TJSONObject;
                          whereStr: String=''): String; overload;

Function sqlUpdate( const filename, aJSON: string;
                          whereStr: String=''): String; overload;

Function sqlInsertOrUpdate( const filename: String;
                                  aJSON: TJSONObject;
                                  whereStr: String;
                                  fldReturn: String=''): String; overload;

Function sqlInsertOrUpdate( const filename, aJSON: string;
                                  whereStr: String;
                                  fldReturn: String=''): String; overload;

function SetFDParams(const fldNames:  Array of String;
                     const fldValues: Array of Const): TFDParams; overload;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
    System.Variants,
    System.StrUtils,
    System.IOUtils,
    System.Math,
    System.Net.URLClient,
    FireDAC.Comp.Client,
    FireDAC.Stan.Error,
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
    Datasnap.DSSession,
    Data.DBXPlatform,

    uLib.Auth,
{$ENDIF}

    uLib.Base,
    uLib.Helpers,
    uLib.Common,
    uLib.DataModule
    ;


type
  TFDMController = class
  protected
    FDM: TdmMain;
  private
    { Private declarations }
    //function ParamsToJSONObject(params: TFDParams): TJSONObject;
    function cmdAdd(Const dbTableName,
                          Context: String): TJSONValue;
    function cmdUpd(Const dbTableName,
                          Context: String;
                          Condition: String=''): TJSONValue;
    function cmdDel( Const dbTableName, sWhere: String): TJSONObject;
    function cmdExecute(sCmd: string; pParams: TFDParams=Nil; aCommit: Boolean=false): Integer; overload;
    function cmdExecute(sCmd: TStringList; pParams: TFDParams=Nil; aCommit: Boolean=false): Integer; overload;

    function GetRecords(Const sQuery: String;
                              pParams: TFDParams=nil): TDataSet;

    function execTrans( cSQL, sDecl: TStringList;
                         sFields: String;
                         pParams: TFDParams;
                         var iError: integer): TJSONArray;

    procedure SetHeader(aHeader: TStringList; ForAudit: Boolean=false);
  public
    { Public declarations }
    //HeadEnabled: boolean;
    constructor Create(AdmMain: TdmMain);
    destructor Destroy; override;
  end;

//-------------------------------------------------------
//  TFDTable(Result).CopyDataSet(dsArticles, [coStructure, coRestart, coAppend]);
//-------------------------------------------------------

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
      5: T:=IfThen(CnxDriver.ToUpper='SQLITE',TFDRDBMSKinds.SQLite,0);
     end;
     Inc(i);
  until (I>5) Or (T<>TFDRDBMSKinds.Unknown);
  result:=T;
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
     fname:=pParams[I].Name.toLower;
     if I>0 then
        Str.Append(' AND ');
     Str.Append('('+fName+'='+AmpFilter(AssignVal(pParams[I].Value))+')');
   end;
  result:=Str.ToString;
  Str.Destroy;
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
    sFields:=sFields+IfThen(I=0,'',',')+alias+fields[I].ToLower;
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
          IfThen(I=0,'',',')+alias+fields[I].ToLower+'='+
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
       IfThen(I=0,'',',')+(alias+fields[I]).ToLower+'='+
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
             IfThen(I=1,'',',')+(alias+GetStr(fields,I,',')).ToLower+'='+
             AmpFilter(GetStr(Values,I,','));
  result:=trimS(sFields);
end;

Function sqlInsert( const filename: String;
                          aJSON: TJSONObject;
                          fldReturn: String=''): String; overload;
Var
  sFields,
  sValues,
  sSetVal: String;
  TS: TStringList;
Begin
  GetFieldsValues( aJSON,sFields, sValues, sSetVal);
  TS:=TStringList.Create;
  TS.Add('INSERT INTO '+fileName+' ('+sFields+')');
  TS.Add('       VALUES ('+sValues+');');
  if Not fldReturn.IsEmpty then
     case RDBMSKind of
      TFDRDBMSKinds.MSSQL:
        begin
          TS.Insert(1,'       OUTPUT inserted.id,'+fldReturn.QuotedString+
                                    ',inserted.'+fldReturn);
          TS.Insert(2,'         INTO @TempTable');
        end;
     TFDRDBMSKinds.MYSQL:
       begin
         TS.Add('SELECT LAST_INSERT_ID() INTO InsertedId;');
         TS.Add('INSERT INTO #TempTable (id,field,value)');
         TS.Add('       (@InsertedID,'+GetStr(sFields,1,',').QuotedString+','+
                           GetStr(sValues,1,',').QuotedString+');');
       end;
     TFDRDBMSKinds.PostgreSQL:
       begin
         TS.Add('RETURNING id INTO InsertedId;');
         TS.Add('INSERT INTO #TempTable (id,field,value)');
         TS.Add('       (@InsertedID,'+GetStr(sFields,1,',').QuotedString+','+
                                      GetStr(sValues,1,',').QuotedString+');');
       end;
     end;
  result:=TS.Text;
  TS.Free;
End;

Function sqlInsert( const filename, aJSON: string;
                          fldReturn: String=''): String; overload;
begin
  result:=sqlInsert(filename,CreateTJSONObject(aJSON),fldReturn);
end;

Function sqlUpdate( const filename: String;
                          aJSON: TJSONObject;
                          whereStr: String=''): String; overload;
Var
    sCmd,
    sFields,
    sValues,
    sSetVal: String;
Begin
  GetFieldsValues(aJSON,sFields, sValues, sSetVal);
  sCmd:='UPDATE '+filename+' SET '+sSetVal;
  if (WhereStr<>'') then
     sCmd:=sCmd+WhereStr;
  sCmd:=sCmd+';';
  case RDBMSKind of
   TFDRDBMSKinds.MYSQL:
     ;
   TFDRDBMSKinds.MSSQL:
     ;
  end;
  result:=sCmd;
end;

Function sqlUpdate( const filename, aJSON: string;
                          whereStr: String=''): String; overload;
begin
  result:=sqlUpdate(filename, CreateTJSONObject(aJSON),whereStr);
end;

Function sqlInsertOrUpdate( const filename: String;
                                  aJSON: TJSONObject;
                                  whereStr: String;
                                  fldReturn: String=''): String; overload;
Var st: String;
Begin
  st:='';
  If (WhereStr<>'') Then
     Begin
       st:=sqlUpdate(filename, aJSON, whereStr);
       case RDBMSKind of
        TFDRDBMSKinds.MYSQL:
          ;
        TFDRDBMSKinds.MSSQL:
         st:=st+#13' IF @@ROWCOUNT=0'#13;
       end;
     End;
  st:=st+sqlInsert(filename,aJSON,fldReturn);
  Result:=St;
End;

Function sqlInsertOrUpdate( const filename, aJSON: string;
                                  whereStr: String;
                                  fldReturn: String=''): String; overload;
begin
  result:=sqlInsertOrUpdate( filename, CreateTJSONObject(aJSON),whereStr,fldReturn);
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

      vtString: result.Add( fName,fldValues[I].VString^).Size:=
                            Length(fldValues[I].VString^);
      vtWideString: result.Add( fName,WideString(fldValues[I].VWideString)).Size:=
                                Length(WideString(fldValues[I].VWideString));
      vtAnsiString: result.Add(fName, AnsiString(fldValues[I].VAnsiString)).Size:=
                                      Length(AnsiString(fldValues[I].VAnsiString));
      vtUnicodeString: result.Add( fName,UnicodeString(fldValues[I].VUnicodeString)).Size:=
                                   Length(UnicodeString(fldValues[I].VUnicodeString));
     end;
   end;
end;

//-------------------------------------------------------
//
//-------------------------------------------------------

function fnCreateQuery(sQry: String=''; pParams: TFDParams=Nil): TFDQuery;
var
   DMC: TFDMController;
begin
  if (Trim(sQry)='') then
     Exit(Nil);
  DMC:=TFDMController.Create(dmMain);
  result:=TFDQuery.Create(DMC.FDM);
  result.Connection:=DMC.FDM.Cnx;
  result.SQL.Text:=sQry;
  if pParams<>Nil then
     begin
       result.Params:=pParams;
       result.Prepare;
     end;
  Try
    result.OpenOrExecute;
  Except
    SaveLogFile(result.SQL.Text);
  End;
  DMC.Free;
end;

function fnCreateCommand(sQry: String=''; pParams: TFDParams=Nil): TFDCommand;
var
   DMC: TFDMController;
begin
  if (Trim(sQry)='') then
     Exit(Nil);
  DMC:=TFDMController.Create(dmMain);
  result:=TFDCommand.Create(DMC.FDM);
  result.Connection:=DMC.FDM.Cnx;
  result.CommandText.Text:=sQry;
  if pParams<>Nil then
     begin
       result.Params:=pParams;
       result.Prepare;
     end;
  Try
    result.OpenOrExecute;
  Except
     SaveLogFile(result.CommandText.Text);
  End;
  DMC.Free;
end;

constructor TFDMController.Create(AdmMain: TdmMain);
begin
  inherited Create;
  if not Assigned(FDM) then
     FDM := TdmMain.Create(nil)
  else
     FDM := AdmMain;
end;

destructor TFDMController.Destroy;
begin
  FDM.Free;
  inherited;
end;

procedure TFDMController.SetHeader(aHeader: TStringList; ForAudit: Boolean=false);
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
Var
  userId,
  userName: String;
  Session: TDSSession;
{$ENDIF}
begin
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  if forAudit then
     begin
       Session:= TDSSessionManager.GetThreadSession;
       userId:= Session.GetData(SS_LOGINID);
       userName:= Trim(Session.GetData(SS_FIRSTNAME)+' '+
                       Session.GetData(ss_LASTNAME));
       if (StrToInteger(userId)>0) then
       Case RDBMSKind of
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
        TFDRDBMSKinds.MSSQL:
          begin
            aHeader.Add('EXEC sp_set_session_context N''user_id'', '+userId+', 0;');
            aHeader.Add('EXEC sp_set_session_context N''user_name'', '+QuotedStr(username)+', 0;');
          end;
       end;
     end;
{$ENDIF}
  if ForAudit then
     Case RDBMSKind of
      TFDRDBMSKinds.MYSQL:
        begin
        end;
      TFDRDBMSKinds.PostgreSQL:
        begin
        end;
      TFDRDBMSKinds.MSSQL:
        begin
          aHeader.Add('SET DATEFORMAT YMD;');
          //aHeader.Add('SET NOCOUNT ON;');
           {result.Add('SET DATEFIRST 7;'); //, -- 1 = Monday, 7 = Sunday
           result.Add('SET LANGUAGE US_ENGLISH;');}
        end;
     end;
end;


(*
function TFDMController.ParamsToJSONObject(params: TFDParams): TJSONObject;
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
*)

function TFDMController.cmdExecute(sCmd: TStringList; pParams: TFDParams=Nil; aCommit: Boolean=false): Integer;
Var
   aHeader: TStringList;
begin
  aHeader:=TStringList.Create;
  SetHeader(aHeader);
  aHeader.Add(sCmd.Text);

  FDM.Cmd.CommandText.Clear;
  FDM.Cmd.CommandText.Assign(aHeader);
  if pParams<>Nil then
     begin
       FDM.Cmd.Params:=pParams;
       FDM.Cmd.Prepare;
     end;
  if Not aCommit then
     FDM.Cnx.StartTransaction;
  Try
    FDM.Cmd.Execute();
    if Not aCommit then
        FDM.Cnx.Commit;
    Result:=DB_SUCCESSFUL;
  except
   on E: EFDDBEngineException do  begin
           if Not aCommit then
               FDM.Cnx.Rollback;
           SaveLogFile(FDM.Cmd.CommandText.text);
           raise Exception.Create(E.Message);
         end;
  End;
  aHeader.Destroy;
end;

function TFDMController.cmdExecute(sCmd: String; pParams: TFDParams=Nil; aCommit: Boolean=false): Integer;
var
  TS: TStringList;
begin
  TS:=TStringList.Create;
  TS.Text:=sCmd;
  result:=cmdExecute(TS,pParams,aCommit);
  TS.Destroy;
end;

function TFDMController.execTrans( cSQL, sDecl: TStringList;
                         sFields: String;
                         pParams: TFDParams;
                         var iError: integer): TJSONArray;
var
   cCmd: TStringList;
   I: Integer;
   OkArray: Boolean;
begin
  OkArray:=False;
  If ContainsText(sFields,'SELECT ') then
     OkArray:=ContainsText(sFields,' FROM ');
  cCmd:=TStringList.Create;
  SetHeader(cCmd,true);
  Case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     begin
       cCmd.Add('SET TRANSACTION ISOLATION LEVEL READ COMMITTED;');
       cCmd.Add('DECLARE ');
       cCmd.Add('   @ErrMsg        NVARCHAR(4000)');
       cCmd.Add('  ,@ErrorSeverity INT');
       cCmd.Add('  ,@ErrorState    INT');
       cCmd.Add('  ,@ErrorNumber   INT');
       cCmd.Add('  ,@ErrorLine     INT'+IfThen(sDecl=Nil,';',''));
       if sDecl<>Nil then
          begin
            var sText:=sDecl.Text;
            if (AnsiLastChar(sText)<>';') then
               sText:=sText+';';
            cCmd.AddStrings(sText);
          end;
       cCmd.Add('BEGIN TRANSACTION;');
       cCmd.Add('BEGIN TRY');
     end;
   TFDRDBMSKinds.MySQL:
     begin
       cCmd.Add('SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED;');
       if sDecl<>Nil then
          cCmd.AddStrings(sDecl);
       cCmd.Add('START TRANSACTION;');
     end;
   TFDRDBMSKinds.PostgreSQL:
     begin
       cCmd.Add('DECLARE');
       if sDecl<>Nil then
          cCmd.AddStrings(sDecl);
       cCmd.Add('BEGIN');
       cCmd.Add('  BEGIN;');
     end;
  end;
  //--------------------------------
  //cCmd.AddStrings(cSQL);
  for I := 0 to cSQL.Count-1 do
   cCmd.Add('  '+TrimRight(cSQL[i]));
  //--------------------------------
  Case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     begin
       cCmd.Add('  COMMIT TRANSACTION;');
       if OkArray then
          cCmd.Add('  '+sFields)
       else
          if sFields<>'' then
             cCmd.Add('  SELECT '+sFields+';');
       cCmd.Add('END TRY');
       cCmd.Add('BEGIN CATCH');
       cCmd.Add('  IF (@@TRANCOUNT>0)');
       cCmd.Add('     ROLLBACK;');
       cCmd.Add('  SELECT');
       cCmd.Add('     @ErrMsg = ERROR_MESSAGE(),');
       cCmd.Add('     @ErrorSeverity = ERROR_SEVERITY(),');
       cCmd.Add('     @ErrorState = ERROR_STATE(),');
       cCmd.Add('     @ErrorNumber = ERROR_NUMBER(),');
       cCmd.Add('     @ErrorLine = ERROR_LINE();');
       cCmd.Add('  SET @ErrMsg = @ErrMsg+Char(13)+');
       cCmd.Add('      ''Line: ''+Cast(@ErrorLine As Varchar(10));');
       cCmd.Add('  SELECT @ErrMsg errmsg, @ErrorNumber errornumber,');
       cCmd.Add('         @ErrorLine errorline, @ErrorSeverity errseverity;');
       cCmd.Add('END CATCH;');
     end;
   TFDRDBMSKinds.MySQL:
     begin
       cCmd.Add('SELECT @Error := @@error;');
       cCmd.Add('IF (@Error <> 0) THEN');
       cCmd.Add('   ROLLBACK;');
       cCmd.Add('   SELECT concat(''Transaction failed with error: '', @Error);');
       cCmd.Add('ELSE');
       cCmd.Add('   COMMIT;');
       cCmd.Add('END IF;');
       cCmd.Add('SELECT '+sFields+';');
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
       cCmd.Add('  SELECT '+sFields+';');
       cCmd.Add('END;');
     end;
  end;
{$IFDEF DEBUG}
  cCmd.SaveToFile('trans.txt');
{$ENDIF}
  FDM.Qry.SQL.Clear;
  FDM.Qry.SQL.Assign(cCmd);
  if pParams<>Nil then
     begin
       FDM.Qry.Params:=pParams;
       FDM.Qry.Prepare;
     end;
  //if Not aCommit then
  FDM.Cnx.StartTransaction;
  Try
    FDM.Qry.OpenOrExecute;
    if OkArray then
       Result:=FDM.Qry.AsJSONArray
    else
       begin
         Result:=TJSONArray.Create;
         Result.Add(FDM.Qry.AsJSONObject);
       end;
    //if Not aCommit then
    FDM.Cnx.Commit;
    iError:=0;
  except
   on E: EFDException do begin
         iError:=-1;
         //if Not aCommit then
         FDM.Cnx.Rollback;
         Result:=CreateTJSONArray('[{"error":"'+E.Message+'"}]');
         SaveLogFile(FDM.Qry.SQL.Text);
   end;
  End;
  cCmd.Destroy;
End;

function TFDMController.cmdAdd( const dbTableName, Context: String): TJSONValue;
Var
  sCmd,
  Dcl: TStringList;
  AJSON: TJSONArray;
  AValue: TJSONValue;
  fldsReturn: String;
  I,error: Integer;
begin
  sCmd:=TStringList.create;
  Dcl:=TStringList.create;
  Case RDBMSKind Of
    TFDRDBMSKinds.MSSQL:
        begin
          Dcl.Add(';');
          Dcl.Add('DECLARE ');
          Dcl.Add('  @TempTable TABLE (');
          Dcl.Add('    id INT,');
          Dcl.Add('    field VARCHAR(30),');
          Dcl.Add('    value NVARCHAR(MAX)');
          Dcl.Add('  )');
          fldsReturn:='(SELECT id, field, value FROM @TempTable) insertedrows';
        end;
  end;
  try
    if Context.StartsWith('[') then
       begin
         AJSON:=CreateTJSONArray(Context);
         for I := 0 to AJSON.Count-1 do
           begin
             var JSON:=AJSON.items[i].ToJSON;
             sCmd.Add(sqlInsert(dbTableName,jSON,fldsReturn));
           end;
         AJSON.Free;
       end
    else
       begin
         sCmd.Add(sqlInsert(dbTableName,Context,fldsReturn));
       end;
    AValue:=execTrans(sCmd,Dcl,fldsReturn,nil,error);
    TJSONObject(result).AddPair('status',Error);
    TJSONObject(result).AddPair('response',AValue);
  finally
    sCmd.Destroy;
    Dcl.Destroy;
  end;
End;

function TFDMController.cmdUpd(Const dbTableName, Context: String;
                           Condition: String): TJSONValue;
Var
  sCmd: TStringList;
  AJSON: TJSONArray;
  oJSON: TJSOnObject;
  AValue,
  lValue: TJSONValue;
  sWhere,
  fldsReturn: String;
  I,error: Integer;
begin
  sCmd:=TStringList.create;
  try
    Case RDBMSKind Of
      TFDRDBMSKinds.MSSQL:
         fldsReturn:= '(SELECT @@ROWCOUNT) updatedrows';
    End;
    if Context.StartsWith('[') then
       begin
         AJSON:=CreateTJSONArray(Context);
         for I := 0 to AJSON.count-1 do
          begin
            oJSON:=TJSOnObject(AJSON.Items[I]);

            sWhere:='';
            lValue:=oJSON.FindValue('id');
            If lValue<>Nil Then
               begin
                 sWhere:='(id='+lValue.Value+')';
               end;

            sCmd.Add(sqlUpdate(dbTableName,oJSON,sWhere));
          end;
         AJSON.Free;
       end
    else
       begin
         sCmd.Add(sqlUpdate(dbTableName,Context,Condition));
       end;
    AValue:=execTrans(sCmd,Nil,fldsReturn,Nil,error);
    TJSONObject(result).AddPair('status',Error);
    TJSONObject(result).AddPair('response',AValue);
  finally
    sCmd.Destroy;
  end;
End;

function TFDMController.cmdDel(Const dbTableName, sWhere: String): TJSONObject;
Var
   sCmd: TStringList;
   error: Integer;
   AValue: TJSONValue;
begin
  sCmd:=TStringList.create;
  try
    sCmd.Add('DELETE FROM '+dbTableName);
    sCmd.Add(' WHERE '+sWhere+';');
    AValue:=execTrans(sCmd,Nil,'',Nil,error);
    TJSONObject(result).AddPair('status',Error);
    TJSONObject(result).AddPair('response',AValue);
  finally
    sCmd.Destroy;
  end;
end;

function TFDMController.GetRecords(Const sQuery: String; pParams: TFDParams=Nil): TDataSet;
Var aHeader: TStringList;
begin
  aHeader:=TStringList.Create;
  SetHeader(aHeader);
  FDM.Qry.SQL.Clear;
  FDM.Qry.SQL.Assign(aHeader);
  FDM.Qry.SQL.Add(sQuery);
  if (pParams<>Nil) then
     begin
       FDM.Qry.Params:=pParams;
       FDM.Qry.Prepare;
     end;
  try
    FDM.Qry.Open;
  except
    on E: EFDDBEngineException do begin
            SaveLogFile(FDM.Qry.SQL.Text);
            raise Exception.Create(E.Message);
          end;
  end;
  aHeader.Destroy;
  result:=FDM.Qry;
end;

function setQueryPaged(Const dbTable, sFields, sWhere: String): String;

  function getFieldsAs(sFields: String): String;
  Var
      St,S: String;
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

  procedure setCompare( aExpr: String;
                        var sWhere, sOrderby: String;
                        var pagesize, page, limit: Integer );
  const
      OPERATORS: Array[0..6] Of String=
                  ('>=','<=','!=','>','<','=','|');
  var
     aField,
     aValue: String;
     I: Integer;
  begin
    if aExpr.IsEmpty then
       exit;
    for I := 0 to High(OPERATORS) do
      begin
        var sOp:=OPERATORS[I];
        var P:=Pos(sOp,aExpr);
        if (P>0) then
           begin
             aField:=Trim(Copy(aExpr,1,P-1)).ToLower;
             aValue:=Trim(Copy(aExpr,P+Length(sOp),Length(aExpr)));
             if (aField='limit') then
                limit:=StrToInteger(aValue)
             else
             if (aField='page') then
                page:=StrToInteger(aValue)
             else
             if (aField='pagesize') then
                pageSize:=StrToInteger(aValue)
             else
             if (aField='order') then
                sOrderBy:=aValue
             else
             if (aField='authtoken') then
             else
                begin
                  Var lExpr:='';
                  if (Pos('|',aValue)>0) then
                     begin
                       var lValue:=aValue;
                       var tExpr:='';
                       while lValue<>'' do
                        begin
                          var T:=Pos('|',lValue)-1;
                          if T<=0 then
                             T:=Length(lValue);
                          var sValue:=Copy(lValue,1,T);
                          Delete(lValue,1,Length(sValue)+1);

                          if (sValue.StartsWith('0') or
                             not IsNumeric(sValue)) And
                             not sValue.StartsWith('''') then
                             sValue:=sValue.QuotedString;
                          tExpr:= tExpr+ifThen(tExpr<>'',' OR ',' ')+
                             '('+aField.ToLower+'='+sValue+')';
                        end;
                       lExpr:=trim(tExpr);
                     end
                  else
                     begin
                       if (aValue.StartsWith('0') or
                          not IsNumeric(aValue)) And
                          not aValue.StartsWith('''') then
                          aValue:=aValue.QuotedString;
                       lExpr:=aField.ToLower+sOp+aValue;
                     end;
                  if Not lExpr.IsEmpty then
                     sWhere:= sWhere+ifThen(sWhere<>'',' AND ',' ')+
                             '('+lExpr+')';
                  break;
                end;
           end;
      end;
  end;

  procedure GetQueryParams(var lWhere, sOrderby: string; var pagesize,page,Limit: integer);
  {$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  var
    metaData: TDSInvocationMetadata;
    i: integer;
  {$ENDIF}
  begin
    {$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
    metaData := GetInvocationMetadata;
    for i := 0 to Pred(metaData.QueryParams.Count) do
      begin
        setCompare(metaData.QueryParams[i],lWhere,sOrderby,pagesize,page,Limit);
      end;
    {$ENDIF}
  end;
var
   stCmd: TstringList;
   sFlds,
   lWhere,
   sOrderBy: String;
   Page,
   Limit,
   PageSize: Integer;
begin
  lWhere:=sWhere;
  Page:=0;
  Limit:=0;
  PageSize:=0;
  GetQueryParams(lWhere, sOrderby,pagesize,page,Limit);
  sFlds:=sFields.ToLower;
  if sFlds='' then
     sFlds:='*';
  if Limit>0 then
     Case RDBMSKind Of
       TFDRDBMSKinds.MSSQL:
         begin
           sFlds:='top '+limit.ToString+' '+sFlds;
         end;
     End;
  //sFlds:=getFieldsAs(sFlds);    // Alias
  stCmd:=TstringList.Create;
  if (PageSize>0) and (Page>0) then
     Case RDBMSKind Of
       TFDRDBMSKinds.MSSQL:
         begin
           if (sOrderBy='') then
              sOrderBy:='@@ROWCOUNT';
           stCmd.Add('DECLARE');
           stCmd.Add('  @page INT='+Page.ToString+',');
           stCmd.Add('  @pagesize INT='+PageSize.ToString+';');

           stCmd.Add(';WITH MyCTE AS');
           stCmd.Add('  (SELECT '+sFlds+', ROW_NUMBER() OVER (ORDER BY '+sOrderBy+') row_num');
           stCmd.Add('     FROM '+dbTable+' WITH (NOLOCK)');
           If (lWhere<>'') Then
              stCmd.Add('    WHERE '+lWhere);
           stCmd.Add('  )');
           stCmd.Add('SELECT '+sFlds+'');
           stCmd.Add('  FROM myCTE WITH (NOLOCK)');
           stCmd.Add(' WHERE (row_num>=(@page-1)*@pagesize+1) AND ');
           stCmd.Add('       (row_num<=@page*@pagesize)');
         end;
     end
  else
     Case RDBMSKind Of
       TFDRDBMSKinds.MSSQL:
         begin
           stCmd.Add('SELECT '+sFlds+' ');
           stCmd.Add('  FROM '+dbTable+' WITH (NOLOCK)');
           If (lWhere<>'') Then
              stCmd.Add(' WHERE '+lWhere);
           if (sOrderBy<>'') then
              stCmd.Add(' ORDER BY '+sOrderBy);
         end;
     End;
  result:=stCmd.Text+';';
  stCmd.Destroy;
end;

//-------------------------------------------------------
//
//-------------------------------------------------------

function GetDataSet( const sQuery: String;
                           pParams: TFDParams=nil): TDataSet; overload;
var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.GetRecords(sQuery,pParams);
  finally
    DMC.Destroy;
  end;
end;

function GetData( const sQuery: String;
                  pParams: TFDParams=nil): TJSONValue; overload;
var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    result:=DMC.GetRecords(sQuery,pParams).AsJSONvalue;
  finally
    DMC.Destroy;
  end;
end;

function GetData( sQuery: TStrings;
                  pParams: TFDParams=nil): TJSONValue; overload;
begin
  result:=GetData(sQuery.Text,pParams);
end;

function GetData( const sQuery: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONValue; overload;
Var
   pParams: TFDParams;
begin
  pParams:=SetFDParams(fldNames,fldValues);
  try
    Result:=GetData(sQuery,pParams);
  finally
    pParams.Destroy;
  End;
end;

function GetData( sQuery: TStrings;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONValue; overload;
begin
  result:=GetData(sQuery.Text,fldNames,fldValues);
end;

function GetData( const sTblName: String;
                  sfields: String;
                  sCondition: String='';
                  sOrder: string=''): TJSONValue; overload;
var
  sQry: String;
begin
  if sfields='' then
     sfields:='*';
  sQry:=setQueryPaged(sTblName,sfields,sCondition);
  Result:= GetData(sQry);
end;

function AddData( Const dbTableName: string;
                        Context: TJSONValue ): TJSONObject; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdAdd(dbTableName,Context.ToString) As TJSONObject;
  finally
    DMC.Destroy;
  end;
end;

function AddData( Const dbTableName, Context: String): TJSONObject; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdAdd(dbTableName,Context) As TJSONObject;;
  finally
    DMC.Destroy;
  end;
end;

function AddData( Const dbTableName: string;
                  const fldNames:  Array of String;
                  const fldValues: Array of const ): TJSONObject; Overload;
var
   Context: String;
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  Context:='';
  SetJSON(Context,fldNames,fldValues);
  try
    Result:=DMC.cmdAdd(dbTableName,Context) As TJSONObject;
  finally
    DMC.Destroy;
  end;
end;

function UpdData( const dbTableName: String;
                        Context: TJSONValue;
                        Condition: String=''): TJSONObject; Overload;
Var
   DMC: TFDMController;
   sWhere: String;
begin
  sWhere:=Condition;
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdUpd(dbTableName,Context.ToString,sWhere) As TJSONObject;
  finally
    DMC.Destroy;
  end;
End;

function UpdData( const dbTableName, Context: String;
                        Condition: String=''): TJSONObject; Overload;
Var
   DMC: TFDMController;
   sWhere: String;
begin
  sWhere:=Condition;
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdUpd(dbTableName,Context,sWhere) As TJSONObject;
  finally
    DMC.Destroy;
  end;
End;

function UpdData( const dbTableName: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const;
                        Condition: String=''): TJSONObject; Overload;
var
   Context: string;
   DMC: TFDMController;
   sWhere: String;
begin
  sWhere:=Condition;
  DMC:=TFDMController.Create(dmMain);
  Context:='{}';
  SetJSON(Context,fldNames,fldValues);
  try
    Result:=DMC.cmdUpd(dbTableName,Context,sWhere) As TJSONObject;
  finally
    DMC.Destroy;
  end;
end;

function DelData( Const dbTableName, condition: String): TJSONObject;
Var
   DMC: TFDMController;
   sWhere: String;
begin
  sWhere:=Condition;
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdDel(dbTableName,sWhere) As TJSONObject;
  finally
    DMC.Destroy;
  end;
end;

function ExecTransact( cSQL: String;
                       sDecl: String='';
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject;
Var
   DMC: TFDMController;
   Cmd,
   Dcl: TStringList;
   Error: Integer;
   AValue: TJSONValue;
begin
  DMC:=TFDMController.Create(dmMain);
  Result:=TJSONObject.Create;

  Cmd:=TStringList.Create;
  Cmd.Text:=cSQL;
  Dcl:=Nil;
  if sDecl<>'' then
     begin
       Dcl:=TStringList.Create;
       Dcl.Text:=sDecl;
     end;
  try
    AValue:=DMC.execTrans(Cmd, Dcl, sFields, pParams, Error);
    result.AddPair('status',Error);
    result.AddPair('response',AValue);
  finally
    DMC.Destroy;
    Cmd.Destroy;
    if Dcl<>Nil then
       Dcl.Destroy;
  end;
end;

function ExecTransact( cSQL: TStringList;
                       sDecl: TStringList=nil;
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject;
Var
   DMC: TFDMController;
   AValue: TJSONValue;
   Error: Integer;
begin
  DMC:=TFDMController.Create(dmMain);
  Result:=TJSONObject.Create;
  try
    AValue:=DMC.execTrans(cSQL, sDecl, sFields, pParams, Error);
    result.AddPair('status',Error);
    result.AddPair('response',AValue);
  finally
    DMC.Destroy;
  end;
end;

function ExecCmd( const sCommands: String;
                        pParams: TFDParams=Nil;
                        aCommit: Boolean=true): TJSONObject; Overload;
Var
   DMC: TFDMController;
begin
  if sCommands='' then
     Exit(Nil);
  DMC:=TFDMController.Create(dmMain);
  Result:=TJSONObject.Create;
  try
    result.AddPair('response',DMC.cmdExecute(sCommands,pParams,aCommit));
  finally
    DMC.Destroy;
  end;
end;

function ExecCmd( sCommands: TStrings;
                  pParams: TFDParams=Nil;
                  aCommit: Boolean=true): TJSONObject; Overload;
begin
  result:=ExecCmd( sCommands.text,pParams,aCommit);
end;

function ExecCmd( const sCommands: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const;
                        aCommit: Boolean=true): TJSONObject; overload;
var
   pParams: TFDParams;
   DMC: TFDMController;
begin
  if sCommands='' then
     Exit(Nil);
  DMC:=TFDMController.Create(dmMain);
  pParams:=setFDParams(fldNames,fldValues);
  Result:=TJSONObject.Create;
  try
    result.AddPair('response',DMC.cmdExecute(sCommands,pParams,aCommit));
  finally
    pParams.Destroy;
    DMC.Destroy;
  end;
end;

function ExecCmd( sCommands: TStrings;
                   const fldNames:  Array of String;
                   const fldValues: Array of const;
                   aCommit: Boolean=true): TJSONObject; Overload;
begin
  result:= ExecCmd( sCommands.Text,fldNames,fldValues,aCommit);
end;

function DatabaseExists(const nameDB: String): boolean;
Var
  JSON: TJsonObject;
  Sqry: String;
begin
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
  JSON:=GetData(SQry) As TJSONObject;
  result:=false;
  if (JSON<>Nil) And Assigned(JSON) then
     result:=GetInt(JSON,'db_exists')>0;
  JSON.Destroy;
end;

initialization

finalization

end.

