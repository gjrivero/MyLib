unit uLib.Data;

interface

uses
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
                        Context: TJSONValue): TJSONValue; Overload;
function AddData( const dbTableName,
                        Context: String): TJSONValue; Overload;
function AddData( const dbTableName: string;
                  const fldNames: Array of String;
                  const fldValues: Array of const): TJSONValue; Overload;

function UpdData( const dbTableName: String;
                        Context: TJSONValue;
                        Condition: String=''): TJSONValue; Overload;
function UpdData( const dbTableName,
                        Context: String;
                        Condition: String=''): TJSONValue; Overload;
function UpdData( const dbTableName: String;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                        Condition: String=''): TJSONValue; Overload;

function DelData( const dbTableName, Condition: String): TJSONObject;

function ExecCmd( const sCommands: String; pParams: TFDParams=Nil; aCommit: Boolean=true): Integer; Overload;
function ExecCmd( sCommands: TStrings; pParams: TFDParams=Nil; aCommit: Boolean=true): Integer; Overload;
function ExecCmd( const sCommands: String;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                  aCommit: Boolean=true): Integer; Overload;
function ExecCmd( sCommands: TStrings;
                  const fldNames: Array of String;
                  const fldValues: Array of const;
                  aCommit: Boolean=true): Integer; Overload;

function ExecTransact( cSQL: TStringList;
                       sDecl: TStringList=nil;
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject; Overload;

function GetQueryParams(const sCondition: String): String;
function FieldsString( const fields: array of String; alias: String=''  ): String;
function GetDriverID(CnxDriver: String): TFDRDBMSKind;
function DatabaseExists( const nameDB: String): boolean;
function SetFDParams( const fldNames: Array Of String;
                      const fldValues: Array Of Variant): TFDParams; overload;
function SetFDParams(const fldNames:  Array of String;
                     const fldValues: Array of Const): TFDParams; overload;
procedure SetIdentHeader(TL: TstringList; const dbTable, Id: String);


implementation

uses
    System.Variants,
    System.StrUtils,
    System.IOUtils,
    System.Math,
    Datasnap.DSSession,
    Data.DB,
    Data.DBXPlatform,
    FireDAC.Comp.Client,
    FireDAC.Stan.Error,

    uLib.Auth,
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
                              pParams: TFDParams=nil): TJSONArray;

    function execTrans( cSQL, sDecl: TStringList;
                        sFields: String;
                        pParams: TFDParams): TJSONObject;
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
     end;
     Inc(i);
  until (I>7) Or (T<>TFDRDBMSKinds.Unknown);
  result:=t;
end;

function getQueryParams(const sCondition: String): String;
const
   OPERATORS: Array[0..6] Of String=
      ('>=','<=','!=','>','<','=','#');

  procedure setCompare(aExpr: String; var sWhere: String );
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
             aField:=Trim(Copy(aExpr,1,P-1));
             aValue:=Trim(Copy(aExpr,P+Length(sOp),Length(aExpr)));
             if aValue<>'' then
                begin
                  if (aValue.StartsWith('0') or
                     not IsNumeric(aValue)) And
                     not aValue.StartsWith('''') then
                     aValue:=aValue.QuotedString;
                  sWhere:= sWhere+ifThen(sWhere<>'',' AND ',' ')+
                           '('+aField.ToLower+sOp+aValue+')';
                  break;
                end;
           end;
      end;
  end;

Var
  metaData: TDSInvocationMetadata;
  aExpr,
  sWhere: String;
  I: Integer;
begin
  metaData := GetInvocationMetadata;
  sWhere:=sCondition;
  for i := 0 to Pred(metaData.QueryParams.Count) do
   begin
     aExpr:=metaData.QueryParams[i];
     setCompare(aExpr,sWhere);
   end;
  Result:=Trim(sWhere);
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

//-------------------------------------------------------
//
//-------------------------------------------------------

function fnCreateQuery(sQry: String=''; pParams: TFDParams=Nil): TFDQuery;
var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    result:=TFDQuery.Create(DMC.FDM);
    result.Connection:=DMC.FDM.Cnx;
    result.SQL.Text:=sQry;
    if pParams<>Nil then
       begin
         result.Params:=pParams;
         result.Prepare;
       end;
    if (sQry<>'') then
       Try
         result.OpenOrExecute;
       Except
         SaveLogFile(result.SQL.Text);
       End;
  finally
    DMC.Free;
  end;
end;

function fnCreateCommand(sQry: String=''; pParams: TFDParams=Nil): TFDCommand;
var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    result:=TFDCommand.Create(DMC.FDM);
    result.Connection:=DMC.FDM.Cnx;
    result.CommandText.Text:=sQry;
    if pParams<>Nil then
       begin
         result.Params:=pParams;
         result.Prepare;
       end;
    if sQry<>'' then
       Try
         result.OpenOrExecute;
       Except
         SaveLogFile(result.CommandText.Text);
       End;
  finally
    DMC.Free;
  end;
end;

//-------------------------------------------------------
//
//-------------------------------------------------------

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
  cmdExecute(TS,pParams,aCommit);
  TS.Destroy;
end;

function TFDMController.execTrans( cSQL, sDecl: TStringList;
                         sFields: String;
                         pParams: TFDParams): TJSONObject;
var
   cCmd: TStringList;
begin
  cCmd:=TStringList.Create;
  SetHeader(cCmd,true);
  Case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     begin
       cCmd.Add('SET TRANSACTION ISOLATION LEVEL READ COMMITTED;');
       cCmd.Add('DECLARE ');
       if sDecl<>Nil then
          cCmd.AddStrings(sDecl);
       cCmd.Add('  '+IfThen(sDecl<>Nil,',',' ')+'@Error INT=0');
       cCmd.Add('  ,@ErrSeverity int=0');
       cCmd.Add('  ,@ErrMsg nvarchar(4000);');
       cCmd.Add('BEGIN TRANSACTION;');
       cCmd.Add('BEGIN TRY');
     end;
   TFDRDBMSKinds.MySQL:
     begin
       cCmd.Add('SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED;');
       cCmd.Add('DECLARE @Error INT DEFAULT 0;');
       if sDecl<>Nil then
          cCmd.AddStrings(sDecl);
       cCmd.Add('START TRANSACTION;');
     end;
   TFDRDBMSKinds.PostgreSQL:
     begin
       cCmd.Add('DECLARE');
       if sDecl<>Nil then
          cCmd.AddStrings(sDecl);
       cCmd.Add('  _error INTEGER DEFAULT 0;');
       cCmd.Add('BEGIN');
       cCmd.Add('  BEGIN;');
     end;
  end;
  //--------------------------------
  cCmd.AddStrings(cSQL);
  //--------------------------------
  Case RDBMSKind of
   TFDRDBMSKinds.MSSQL:
     begin
       cCmd.Add('  IF @ERROR>0');
       cCmd.Add('  BEGIN');
       cCmd.Add('    ROLLBACK;');
       cCmd.Add('    SELECT @ErrMsg=''ERROR [''+CAST(@ERROR AS VARCHAR(5))+''] IN TRASACTION'';');
       cCmd.Add('    SELECT @ERROR error, @ErrMsg errmsg;');
       cCmd.Add('    RAISERROR(@ErrMsg, @ERROR,1);');
       cCmd.Add('  END;');
       cCmd.Add('  COMMIT TRANSACTION;');
       cCmd.Add('  SELECT @ERROR error'+ifThen(sFields<>'',', '+sFields,'')+';');
       cCmd.Add('END TRY');
       cCmd.Add('BEGIN CATCH');
       cCmd.Add('  IF (@@TRANCOUNT>0) or (@ERROR<>0) ');
       cCmd.Add('     ROLLBACK;');
       cCmd.Add('  SELECT @ErrMsg = ERROR_MESSAGE(),@ErrSeverity = ERROR_SEVERITY()');
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

  FDM.Qry.SQL.Clear;
  FDM.Qry.SQL.Assign(cCmd);
  //FDM.Qry.SQL.SaveToFile('trans.txt');
  if pParams<>Nil then
     begin
       FDM.Qry.Params:=pParams;
       FDM.Qry.Prepare;
     end;
  //if Not aCommit then
     FDM.Cnx.StartTransaction;
  Try
    FDM.Qry.OpenOrExecute;
    Result:=FDM.Qry.AsJSONObject;
    if GetInt(FDM.Qry,'error')<>0 then
       begin
         //if Not aCommit then
            begin
              SaveLogFile(FDM.Qry.SQL.Text);
              FDM.Cnx.Rollback;
            end;
       end
    else
       //if Not autoCommit then
          FDM.Cnx.Commit;
  except
   on E: EFDDBEngineException do  begin
           //if Not autoCommit then
              FDM.Cnx.Rollback;
           SaveLogFile(FDM.Qry.SQL.Text);
           raise Exception.Create(E.Message);
         end;
  End;
  cCmd.Destroy;
End;

procedure SetIdentHeader(TL: TstringList; const dbTable, Id: String);
var
   seqName,
   SingleName: String;
   c: Integer;
begin
   C:=CountOccurrences('.',dbTable)+1;
   SingleName:=GetStr(dbTable,c,'.');
   if SingleName.IsEmpty then
      SingleName:=dbTable;
   seqName:='seq_'+SingleName;

   TL.Add('IF EXISTS(SELECT name FROM sys.sequences WHERE name='+QuotedStr(seqName)+')');
   TL.Add('   SELECT @'+Id+'=CAST(current_value AS INT) FROM sys.sequences WHERE name='+QuotedStr(seqName));
   TL.Add('ELSE');
   TL.Add('   SELECT @'+Id+'=IDENT_CURRENT(' + QuotedStr(SingleName) + ');');
end;

function TFDMController.cmdAdd( const dbTableName,
                            Context: String): TJSONValue;

  procedure InsTable( iSQL: TStringList;
                      const DBTABLE: String;
                            pNameFlds: String;
                      const pValFlds: String;
                            TempTable: boolean);
  Var
    sIns: String;
  begin
    sIns:=
     'INSERT INTO '+DBTABLE+' ('+pNameFlds+')'+#13+
     '       VALUES ('+pValFlds+')';
    Case RDBMSKind Of
     TFDRDBMSKinds.MSSQL:
       begin
         iSQL.Add(sIns+';');
         SetIdentHeader(iSQL,DBTABLE,'InsertedId');
         if TempTable then
            begin
              iSQL.Add('INSERT INTO #TempTable (id,field,value)');
              iSQL.Add('       SELECT @InsertedId,'+GetStr(pNameFlds,1,',').QuotedString+','+
                                      GetStr(pValFlds,1,',')+';');
            end;
       end;
     TFDRDBMSKinds.MYSQL:
       begin
         iSQL.Add(sIns+';');
         iSQL.Add('SELECT LAST_INSERT_ID() INTO InsertedId;');
         if TempTable then
            begin
              iSQL.Add('INSERT INTO #TempTable (id,field,value)');
              iSQL.Add('       (@InsertedID,'+GetStr(pNameFlds,1,',').QuotedString+','+
                                      GetStr(pValFlds,1,',').QuotedString+');');
            end;
       end;
     TFDRDBMSKinds.PostgreSQL:
       begin
         iSQL.Add(sIns+' RETURNING id INTO InsertedId;');
         if TempTable then
            begin
              iSQL.Add('INSERT INTO #TempTable (id,field,value)');
              iSQL.Add('       (@InsertedID,'+GetStr(pNameFlds,1,',').QuotedString+','+
                                      GetStr(pValFlds,1,',').QuotedString+');');
            end;
       end;
    end;
  end;

Var
  sCmd: TStringList;
  AJSON: TJSONArray;
  sFields,
  sValues,
  sSetVal,
  fldsReturn,
  sCondition: String;
  I: Integer;
begin
  sCmd:=TStringList.create;
  //sDcl:=TStringList.create;
  try
    if Context.StartsWith('[') then
       begin
         Case RDBMSKind Of
          TFDRDBMSKinds.MSSQL:
              begin
                fldsReturn:=
                '(SELECT N''{"id":''+cast(@InsertedId as varchar)+''}'') insertedrows';
                sCmd.Add('CREATE TABLE #TempTable (');
                sCmd.Add('  id INT,');
                sCmd.Add('  field VARCHAR(30),');
                sCmd.Add('  value NVARCHAR(MAX)');
                sCmd.Add(');');
              end;
         end;
         AJSON:=CreateTJSONArray(Context);
         for I := 0 to AJSON.Count-1 do
           begin
             var JSON:=(AJSON[i] as TJSONObject).ToString;
             GetFieldsvalues(JSON,sFields,sValues,sSetVal,sCondition,false);
             InsTable(sCmd,dbTableName,sFields,sValues,True);
           end;
         Case RDBMSKind Of
          TFDRDBMSKinds.MSSQL:
            begin
              fldsReturn:='(SELECT id, field, value FROM #TempTable FOR JSON AUTO) insertedrows';
              //sCmd.Add('DROP #TempTable');
            end;
         end;
       end
    else
       begin
         Case RDBMSKind Of
          TFDRDBMSKinds.MSSQL:
              begin
                sCmd.Add('DECLARE ');
                sCmd.Add('  @InsertedId INT=0;');
              end;
          TFDRDBMSKinds.MYSQL,
          TFDRDBMSKinds.PostgreSQL:
              sCmd.Add('  InsertedId INT=0;');
         End;
         GetFieldsvalues(Context,sFields,sValues,sSetVal,sCondition,false);
         Case RDBMSKind Of
          TFDRDBMSKinds.MSSQL:
              begin
                fldsReturn:=
                '(SELECT N''{"id":''+cast(@InsertedId as varchar)+''}'') insertedrows';
              end;
          TFDRDBMSKinds.MYSQL,
          TFDRDBMSKinds.PostgreSQL:
              sCmd.Add('  InsertedId INT=0;');
         end;
         InsTable(sCmd,dbTableName,sFields,sValues,False);
       end;
    result:= execTrans(sCmd,Nil,fldsReturn,nil);
  finally
    sCmd.Destroy;
    //sDcl.Destroy;
  end;
End;

function TFDMController.cmdUpd(Const dbTableName, Context: String;
                           Condition: String): TJSONValue;

  procedure UpdTable( iSQL: TStringList;
                      const DBTABLE: String;
                      const setVals: String;
                      const aCondition: String);
  begin
    iSQL.Add('UPDATE '+DBTABLE+'');
    iSQL.Add('   SET '+setVals);
    iSQL.Add(' WHERE '+aCondition+';');
  end;

Var
  sCmd: TStringList;
  AJSON: TJSONArray;
  sJSON,
  fldsReturn,
  sFields,
  sValues,
  sSetVal: String;
  I: Integer;
begin
  sCmd:=TStringList.create;
  try
    fldsReturn:=
                '(SELECT @@ROWCOUNT) updatedrows';
    if Context.StartsWith('[') then
       begin
         AJSON:=CreateTJSONArray(Context);
         for I := 0 to AJSON.count-1 do
          begin
            sJSON:=(AJSON.Items[I] As TJSONObject).ToString;
            var lCondition: String;
            GetFieldsvalues(sJSON,sFields,sValues,sSetVal,lCondition,true);
            updTable(sCmd,dbTableName,sSetVal,lCondition);
          end;
       end
    else
       begin
         var pCondition: String;
         GetFieldsvalues(Context,sFields,sValues,sSetVal,pCondition,condition='');
         updTable(sCmd,dbTableName,sSetVal,Condition);
       end;
    result:=execTrans(sCmd,Nil,fldsReturn,Nil);
  finally
    sCmd.Destroy;
  end;
End;

function TFDMController.cmdDel(Const dbTableName, sWhere: String): TJSONObject;
Var
   sCmd: TStringList;
begin
  sCmd:=TStringList.create;
  try
    sCmd.Add('DELETE FROM '+dbTableName);
    sCmd.Add(' WHERE '+sWhere+';');
    result:=execTrans(sCmd,Nil,'',Nil) As TJSONObject;
  finally
    sCmd.Destroy;
  end;
end;

function TFDMController.GetRecords(Const sQuery: String; pParams: TFDParams=Nil): TJSONArray;
Var aHeader: TStringList;
begin
  aHeader:=TStringList.Create;
  SetHeader(aHeader);
  FDM.Qry.SQL.Clear;
  FDM.Qry.SQL.Assign(aHeader);
  FDM.Qry.SQL.Add(sQuery);
  if pParams<>Nil then
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
  if FDM.Qry.IsEmpty then
     Result:=TJSONArray.Create()
  else
     Result:=FDM.Qry.AsJSONArray();
end;

//-------------------------------------------------------
//
//-------------------------------------------------------

function GetData( const sQuery: String;
                   pParams: TFDParams=nil): TJSONArray; overload;
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
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONArray; overload;
Var
   pParams: TFDParams;
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  pParams:=SetFDParams(fldNames,fldValues);
  try
    Result:=DMC.GetRecords(sQuery,pParams);
  finally
    pParams.Destroy;
    DMC.Destroy;
  End;
end;

function GetData( sQuery: TStrings;
                  pParams: TFDParams=nil): TJSONArray;  overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.GetRecords(sQuery.Text,pParams);
  finally
    DMC.Destroy;
  end;
end;

function GetData( sQuery: TStrings;
                  const fldNames:  Array of String;
                  const fldValues: Array of const): TJSONArray;  overload;
Var
   pParams: TFDParams;
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  pParams:=SetFDParams(fldNames,fldValues);
  try
    Result:=DMC.GetRecords(sQuery.Text,pParams);
  finally
    pParams.Destroy;
    DMC.Destroy;
  End;
end;

function AddData( Const dbTableName: string;
                        Context: TJSONValue ): TJSONValue; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdAdd(dbTableName,Context.ToString);
  finally
    DMC.Destroy;
  end;
end;

function AddData( Const dbTableName, Context: String): TJSONValue; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdAdd(dbTableName,Context);
  finally
    DMC.Destroy;
  end;
end;

function AddData( Const dbTableName: string;
                  const fldNames:  Array of String;
                  const fldValues: Array of const ): TJSONValue; Overload;
var
   Context: String;
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  Context:='';
  SetJSON(Context,fldNames,fldValues);
  try
    Result:=DMC.cmdAdd(dbTableName,Context);
  finally
    DMC.Destroy;
  end;
end;

function UpdData( const dbTableName: String;
                        Context: TJSONValue;
                        Condition: String=''): TJSONValue; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdUpd(dbTableName,Context.ToString,Condition);
  finally
    DMC.Destroy;
  end;
End;

function UpdData( const dbTableName, Context: String;
                        Condition: String=''): TJSONValue; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdUpd(dbTableName,Context,Condition);
  finally
    DMC.Destroy;
  end;
End;

function UpdData( const dbTableName: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const;
                        Condition: String=''): TJSONValue; Overload;
var Context: string;
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  Context:='{}';
  SetJSON(Context,fldNames,fldValues);
  try
    Result:=DMC.cmdUpd(dbTableName,Context,Condition);
  finally
    DMC.Destroy;
  end;
end;

function DelData( Const dbTableName, condition: String): TJSONObject;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    Result:= DMC.cmdDel(dbTableName,condition);
  finally
    DMC.Destroy;
  end;
end;

function ExecTransact( cSQL: TStringList;
                       sDecl: TStringList=nil;
                       sFields: String='';
                       pParams: TFDParams=nil): TJSONObject; Overload;
Var
   DMC: TFDMController;
begin
  DMC:=TFDMController.Create(dmMain);
  try
    result:= DMC.execTrans(cSQL, sDecl, sFields, pParams);
  finally
    DMC.Destroy;
  end;
end;

function ExecCmd( const sCommands: String;
                        pParams: TFDParams=Nil;
                        aCommit: Boolean=true): Integer; Overload;
Var
   DMC: TFDMController;
begin
  if sCommands='' then
     Exit(0);
  DMC:=TFDMController.Create(dmMain);
  try
    Result:=DMC.cmdExecute(sCommands,pParams,aCommit);
  finally
    DMC.Destroy;
  end;
end;

function ExecCmd( sCommands: TStrings;
                  pParams: TFDParams=Nil;
                  aCommit: Boolean=true): Integer; Overload;
begin
  result:=ExecCmd( sCommands.text,pParams,aCommit);
end;

function ExecCmd( const sCommands: String;
                  const fldNames:  Array of String;
                  const fldValues: Array of const;
                        aCommit: Boolean=true): Integer; overload;
var pParams: TFDParams;
Var
   DMC: TFDMController;
begin
  if sCommands='' then
     Exit(0);
  DMC:=TFDMController.Create(dmMain);
  pParams:=setFDParams(fldNames,fldValues);
  Try
    Result:=DMC.cmdExecute(sCommands,pParams,aCommit);
  finally
    pParams.Destroy;
    DMC.Destroy;
  end;
end;

function ExecCmd( sCommands: TStrings;
                   const fldNames:  Array of String;
                   const fldValues: Array of const;
                   aCommit: Boolean=true): Integer; Overload;
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
  JSON:=JSONArrayToObject(GetData(SQry));
  result:=false;
  if (JSON<>Nil) And Assigned(JSON) then
     result:=GetInt(JSON,'db_exists')>0;
  JSON.Destroy;
end;


initialization

finalization

end.


