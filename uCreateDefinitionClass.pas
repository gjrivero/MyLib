unit uCreateDefinitionClass;

interface

uses
   Data.DB,
   System.Types,
   System.UITypes,
   System.JSON,
   System.StrUtils,
   System.SysUtils,
   System.Classes,
   System.Variants,
   System.Generics.Collections,
   FireDAC.Stan.param,
   uLib.Base,
   uLib.Data,
   uLib.Helpers;

Type
    TTableAttributes = ( tbNone, tbDict, tbNoAudit,
                         tbAudit, tbOnlyDict, tbAlter);
    TSetTableAttributes = Set Of TTableAttributes;

    TFieldAttributes = ( atNone, atNotNull, atNull, atPrimary,
                         atUnique, atNotDefault, atForeign, atAlter);
    TSetFieldAttributes = Set Of TFieldAttributes;

    TTableBase = class
      private
        countFl,
        countFK,
        countIX: Integer;
        sAFields: String;
        afldNames: TStringList;
        lSQL,
        iSQL: TStringList;
      protected
        dSQL,
        gSQL: TStringList;
        MSchema,
        ASchema,
        rbTables,
        rbFields,
        lTableName: String;
        rbOnly,
        isSequence,
        dictionary,
        onlyDict: Boolean;
        procedure clear;
      public
        procedure addCmd(const sSQL: String);
        procedure addPK(
                    const fields: array of String
                  );
        procedure addIdx(
                    const fields: array of String
                  );
        procedure addFK(
                    const pfields: array of String;
                          targetTable: String
                  );
        procedure addFld(
                    fieldName: String;
                    fieldType: TFieldType;
                    const len: Integer=0;
                    const attributes: TSetFieldAttributes=[];
                    const aDefault: String=''
                  );
        procedure addFldDict(TName, fName, fAlias: String; fType: TFieldType);

        procedure initTable(
                    const tableName: String;
                          tAttr: TSetTableAttributes=[tbDict,tbAudit]
                  ); virtual;
        procedure makeTable; virtual;
        procedure executeSQL; virtual;
        constructor create(const pMSchema,
                                 pASchema,
                                 prbTables,
                                 prbFields: String;
                                 PSequence: Boolean=True); virtual;
        destructor destroy; virtual;
    end;

    TTableDefinition = class(TTableBase)
      private
        auditTable: Boolean;
        procedure auditProcTable;
        procedure lastDateProcTable;

      protected
        fSQL,
        tSQL,
        aSQL: TStringList;
        sqlFileName: String;
      public
        logTable: String;
        procedure dropTable(fTable: String);
        procedure alterTable(const pSQL: TStringList);
        procedure addAlterTables;
        procedure makeTable; override;
        procedure executeSQL; override;
        procedure OthertExecuteSQL;
        procedure createLocalTriggers(const aSchema: String); virtual;
        procedure createCommonTriggers(const aSchema: String); virtual;
        procedure setDefaults(); virtual;
        procedure dropAllTables; virtual;
        procedure initTable(
                    const tableName: String;
                          tAttr: TSetTableAttributes=[tbDict,tbAudit]
                  ); override;
        constructor create(const pMSchema,
                                 pASchema,
                                 prbTables,
                                 prbFields: String;
                                 PSequence: Boolean=True); override;
        destructor destroy; override;
    end;


procedure processSQL(sqlCmd: TStringList; pParams: TFDParams=nil; aSave: boolean=false);

implementation

Uses
    uLib.Common,
    FireDAC.Stan.Intf;

{ TTableDefintion }

Const
    PR_AUDIT_TRACE   = 'aud_trace';
    PR_LAST_UPDATE   = 'aud_date';
    PR_NEXT_VALUE    = 'aud_next';
    PR_NEXT_SEQUENCE = 'aud_seq';
    PR_ENTP_SEQUENCE = 'aud_entseq';


procedure processSQL(sqlCmd: TStringList; pParams: TFDParams=nil; aSave: boolean=false);
Var
   i: Integer;
   sCmd: TStringList;
begin
  sCmd:=TStringList.Create;
  for I:=0 to sqlCmd.Count-1  do
   begin
     if (sqlCmd[I].StartsWith('--GO')) then
        begin
          case RDBMSKind of
           TFDRDBMSKinds.PostgreSQL:
             begin
               sCmd.Insert(0,'do $task$begin');
               sCmd.Add('end$task$;');
             end;
          end;
          if aSave then
             sCmd.SaveToFile('qry'+FormatDateTime('mmddhhnnss',Now)+'.txt');
          ExecCmd(sCmd.Text,pParams);
          //ExecTransact(sCmd);
          sCmd.Clear;
        end
     else
        sCmd.add(sqlCmd[I]);
   end;
  if Not sCmd.Text.IsEmpty then
     begin
       case RDBMSKind of
        TFDRDBMSKinds.PostgreSQL:
          begin
            sCmd.Insert(0,'do $task$begin');
            sCmd.Add('end$task$;');
          end;
       end;
       if aSave then
          sCmd.SaveToFile('qry'+FormatDateTime('mmddhhnnss',Now)+'.txt');
       ExecCmd(sCmd.Text,pParams);
       //ExecTransact(sCmd);
     end;
  sCmd.Destroy;
end;

{ TTableBase }

procedure TTableBase.addCmd(const sSQL: String);
begin
  gSQL.Add(sSQL);
end;

procedure TTableBase.addFK(const pfields: array of String; targetTable: String);
var sFields: String;
begin
  if OnlyDict or rbOnly Or (targetTable='') then
     exit;
  sFields:=fieldsString(pFields).ToLower;
  lSQL.add(' ,FOREIGN KEY ('+sFields+')');
  lSQL.add('   REFERENCES '+targetTable+' (id)');
  lSQL.add('    ON DELETE '+IfThen(countFK=0,'CASCADE','NO ACTION'));
  lSQL.add('    ON UPDATE NO ACTION');
  inc(CountFK);
end;

procedure TTableBase.addFld(fieldName: String; fieldType: TFieldType;
  const len: Integer; const attributes: TSetFieldAttributes;
  const aDefault: String);

const
    TAB_D =26;
var ln: Integer;
    Ch,
    st: String;
begin
  ch:='  ';
  ln:=20;
  fieldName:=fieldName.ToLower;
  sAFields:=sAFields+','+fieldName;
  afldNames.AddPair(lTableName,fieldName+chDiv+Ord(fieldType).ToString);
  if atAlter In attributes then
     begin
       Case RDBMSKind Of
         TFDRDBMSKinds.MSSQL:
           St:='IF NOT EXISTS(SELECT so.name FROM sysobjects so, syscolumns si'#13#10+
               '               WHERE (so.id=si.id) AND'#13#10+
               '                     (si.name='+QuotedStr(fieldName)+') AND'#13#10+
               '                     (so.name='+QuotedStr(lTableName)+') )'#13#10+
               '   ALTER TABLE [dbo].'+lTableName+' WITH NOCHECK ADD';
         TFDRDBMSKinds.MYSQL:
           St:='';
         TFDRDBMSKinds.PostgreSQL:
           St:='';
         TFDRDBMSKinds.SQLite:
           St:='SELECT COUNT(*) AS CNTREC'#13#10+
               '  FROM pragma_table_info('+QuotedStr(lTableName)+')'#13#10+
               ' WHERE name='+QuotedStr(fieldName)+''#13#10+
               '   ALTER TABLE [dbo].'+lTableName+' ADD';
       end;
       lSQL.Add(St);
       Ch:=StringOfChar(' ',10);
       ln:=length(fieldName)+1;
     end
  else
     begin
       if (countFl>0) then
          ch:=' ,';
     end;
  St:=ch+fieldName+StringOfChar(' ',ln-length(fieldName));
  Case fieldType Of
   ftAutoInc:    Case RDBMSKind Of
                  TFDRDBMSKinds.SQLite:
                    St:=St+'INTEGER';
                  TFDRDBMSKinds.MSSQL:
                    begin
                      St:=St+'INT';
                      if Not isSequence then
                         St:=St+' IDENTITY(1,1)';
                    end;
                  TFDRDBMSKinds.MySQL:
                     begin
                       St:=St+'INT UNSIGNED AUTO_INCREMENT';
                     end;
                  TFDRDBMSKinds.PostgreSQL:
                    begin
                      St:=St+'BIGINT';
                      if Not isSequence then
                         St:=St+' GENERATED ALWAYS AS IDENTITY';
                    end;
                 End;
   ftGUId:       Case RDBMSKind Of
                  TFDRDBMSKinds.MySQL:
                    St:=St+'BINARY(16)';
                  TFDRDBMSKinds.MSSQL:
                    St:=St+'UNIQUEIDENTIFIER DEFAULT NEWID()';
                 End;
   ftString:     St:=St+'VARCHAR('+IntToStr(len)+')';
   ftBoolean,
   ftSmallInt:   St:=St+'SMALLINT';
   ftInteger:    Case RDBMSKind Of
                  TFDRDBMSKinds.MySQL:
                    if (atPrimary in attributes) or
                       (atForeign in attributes) then
                       St:=St+'INT UNSIGNED'
                    else
                       St:=St+'INT';
                  TFDRDBMSKinds.PostgreSQL:
                    St:=St+'BIGINT';
                  else
                    St:=St+'INT';
                 End;
   ftFloat,
   ftExtended:   St:=St+'DECIMAL(18,4)';
   ftCurrency:   St:=St+'MONEY';
   ftDate:       St:=St+'DATE';
   ftDateTime,
   ftTimeStamp:  Case RDBMSKind Of
                  TFDRDBMSKinds.PostgreSQL:
                    St:=St+'DATE';
                  else
                    St:=St+'DATETIME';
                 End;
   ftGraphic:    Case RDBMSKind Of
                  TFDRDBMSKinds.MYSQL:
                    St:=St+'BLOB';
                  TFDRDBMSKinds.MSSQL:
                    St:=St+'IMAGE';
                  TFDRDBMSKinds.PostgreSQL:
                    St:=St+'BYTEA';
                 End;
   //---------------
   ftMemo,       //St:=St+'TEXT';
   ftVarBytes:   Case RDBMSKind Of
                  TFDRDBMSKinds.SQLite,
                  TFDRDBMSKinds.PostgreSQL,
                  TFDRDBMSKinds.MySQL:
                    St:=St+'TEXT';
                  TFDRDBMSKinds.MSSQL:
                    St:=St+'NVARCHAR(MAX)';
                 End;
   ftWideString: St:=St+'XML';
   //ftFixedChar:  ; //St:=St+'JSON';
   //---------------
   ftBlob:       St:=St+'BLOB';
   ftBytes:      St:=St+'BINARY ('+IntToStr(len)+')';
   else
                 St:=St+'UNKNOW';
  End;
  if (fieldType in [ftAutoInc,ftGUId]) Or
     (atPrimary In attributes) then
     begin
       St:=St+' PRIMARY KEY';
       if (fieldType In [ftAutoInc]) then
          Case RDBMSKind Of
            TFDRDBMSKinds.PostgreSQL:
              if isSequence  then
                 St:=St+#13+LeftS(' ',TAB_D)+
                     'DEFAULT NEXTVAL('+QuotedStr(aSchema+'seq_'+lTableName)+')';
            TFDRDBMSKinds.MSSQL:
              if isSequence  then
                 St:=St+#13+LeftS(' ',TAB_D)+
                  'DEFAULT (NEXT VALUE FOR '+aSchema+'seq_'+lTableName+')';
            TFDRDBMSKinds.SQLite: St:=St+' AUTOINCREMENT';
            TFDRDBMSKinds.MySQL: ;
          End;
     end
  else
     begin
       if aDefault.IsEmpty And
          Not (fieldType In [ftTimeStamp]) then
          begin
            If (atNotNull In attributes) Or
               (atForeign In attributes) Or
               (fieldType In [ftCurrency, ftInteger,
                              ftSmallInt, ftFloat]) Then
               St:=St+' NOT';
            St:=St+' NULL';
          end;
       if (atUnique In attributes) Then
          St:=St+' UNIQUE';
       If Not (atNotDefault In attributes) Then
          begin
            case fieldType of
             ftInteger:
                  if Not aDefault.IsEmpty then
                     st:=St+' DEFAULT '+aDefault
                  else
                     st:=St+' DEFAULT 0';
             ftFloat,
             ftBoolean,
             ftCurrency,
             ftSmallInt:  if Not aDefault.IsEmpty then
                             st:=St+' DEFAULT '+aDefault
                          else
                             st:=St+' DEFAULT 0';
             ftDate:      If (atNotNull In attributes) Then
                             st:=St+' DEFAULT CURRENT_DATE';
             ftDateTime:  If (atNotNull In attributes) Then
                             st:=St+' DEFAULT CURRENT_TIMESTAMP';
             ftTimeStamp: Case RDBMSKind Of
                            TFDRDBMSKinds.SQLite:
                              St:=St+' DEFAULT (DATETIME(CURRENT_TIMESTAMP, ''localtime''))';
                            else
                              St:=St+' DEFAULT CURRENT_TIMESTAMP';
                          End;
            else
                        if Not aDefault.IsEmpty then
                           st:=St+' DEFAULT '+aDefault;
            end;
          end;
     end;
  if not (onlyDict or rbOnly) then
     begin
       if atAlter In attributes then
          St:=St+';';
       lSQL.add(st);
     end;
  if dictionary then
     AddFldDict(lTableName,fieldname,fieldname,fieldType);
  inc(CountFl);
end;

procedure TTableBase.addFldDict(TName, fName, fAlias: String;
  fType: TFieldType);

  Function DataType: String;
  Var st: String;
  Begin
    st:='dtUnknown';
    Case fType Of
     ftString:   St:='dtString';
     ftBoolean:  St:='dtBoolean';
     ftSmallInt: St:='dtInteger';
     ftAutoInc,
     ftInteger:  St:='dtLongInt';
     ftFloat:    St:='dtDouble';
     ftCurrency: St:='dtCurrency';
     ftDate:     St:='dtDate';
     ftDateTime: St:='dtDateTime';
     ftMemo:     St:='dtMemo';
     ftGraphic:  St:='dtGraphic';
     ftBlob:     St:='dtBlob';
    End;
    Result:=St;
  End;

Var OK: Boolean;
    lSchema: String;
Begin
  lSchema:=ASchema;
  if rbOnly then
     lSchema:=MSchema;
  Ok:=fType In [ ftString, ftInteger, ftFloat, ftBoolean, ftMemo, ftAutoInc,
                 ftCurrency, ftSmallInt, ftExtended, ftDate, ftDateTime,
                 ftTimeStamp, ftVarBytes, ftWideString, ftFixedChar];
  if Ok then
     Begin
       dSQL.add('INSERT INTO '+ASchema+rbFields+' ( ');
       dSQL.add('       table_id,tablename,fieldname,fieldalias,datatype,');
       dSQL.add('       selectable,searchable,sortable,autosearch,mandatory)');
       dSQL.add('       VALUES (');
       dSQL.add('       (SELECT MAX(id) FROM '+ASchema+rbTables+
                        ' WHERE tablename='+QuotedStr(lSchema+TName)+'),');
       dSQL.add('       '+ QuotedStr(lSchema+tName)+','+QuotedStr(FName)+','+
                           QuotedStr(FAlias)+','+QuotedStr(DataType)+',');
       dSQL.add('       ''T'', ''T'', ''T'', ''F'', ''F'');');
     end;
end;

procedure TTableBase.addIdx(const fields: array of String);
var sFields: String;
begin
  if onlyDict or rbOnly then
     exit;
  sFields:=fieldsString(fields).ToLower;
  iSQL.add('CREATE INDEX '+lTableName+'_Idx'+countIx.ToString+' ON ');
  iSQL.add('       '+ASchema+lTableName+' ('+sFields+');');
  inc(CountIX);
end;

procedure TTableBase.addPK(const fields: array of String);
var sFields: String;
begin
  if onlyDict or rbOnly then
     exit;
  sFields:=fieldsString(fields).ToLower;
  lSQL.add(' ,PRIMARY KEY ('+sFields+')');
end;

procedure TTableBase.clear;
begin
  sAFields:='';
  iSQL.Clear;
  lSQL.Clear;
  countFl:=0;
  countFK:=0;
  countIX:=0;
end;

constructor TTableBase.create(const pMSchema, pASchema,
                                    prbTables,
                                    prbFields: String;
                                    PSequence: Boolean=True);
begin
  MSchema:=pMSchema.ToLower;
  ASchema:=pASchema.ToLower;
  rbTables:=prbTables.ToLower;
  rbFields:=prbFields.ToLower;
  IsSequence:=pSequence;
  Case RDBMSKind Of
   TFDRDBMSKinds.SQLite:
     isSequence:=False;
  End;
  OnlyDict:=false;
  afldNames:=TStringList.Create;
  lSQL:=TStringList.Create;
  iSQL:=TStringList.Create;
  dSQL:=TStringList.Create;
  gSQL:=TStringList.Create;
end;

destructor TTableBase.destroy;
begin
  afldNames.Destroy;
  lSQL.Destroy;
  iSQL.Destroy;
  dSQL.Destroy;
  gSQL.Destroy;
end;

procedure TTableBase.executeSQL;
begin
  processSQL(gSQL);
end;

procedure TTableBase.initTable(const tableName: String;
                                     tAttr: TSetTableAttributes=[tbDict,tbAudit]);
var lSchema: String;
    altertbl: Boolean;
begin
  clear;
  lTableName:=tableName.ToLower;
  alterTbl:=tbAlter in tAttr;
  rbOnly:=tbOnlyDict in tAttr;
  dictionary:=(tbDict in tAttr) or onlyDict Or rbOnly;
  lSchema:=ASchema;
  if rbOnly then
     lSchema:=MSchema;
  if Not (onlyDict And alterTbl) and isSequence  then
     begin
       lSQL.add('DROP SEQUENCE IF EXISTS '+aSchema+'seq_'+lTableName+';');
       lSQL.add('--GO');
       lSQL.add('CREATE SEQUENCE '+aSchema+'seq_'+lTableName+'');
       lSQL.add('          START WITH 1 INCREMENT BY 1;');
       lSQL.add('--GO');
     end;
  if Not (onlyDict And alterTbl) then
     begin
       lSQL.add('DROP TABLE IF EXISTS '+aSchema+lTableName+';');
       lSQL.add('--GO');

       lSQL.add('CREATE TABLE '+aSchema+lTableName+' (');
     end;
  if dictionary then
     Begin
       dSQL.Add('INSERT INTO '+ASchema+rbTables+' (');
       dSQL.Add('       tablename,tablealias)');
       dSQL.Add('       VALUES ('+quotedStr(lSchema+lTableName)+','+
                                  QuotedStr(lTableName)+');');
     End;
end;

procedure TTableBase.makeTable;
Var sText: String;
begin
  if onlyDict or rbOnly then
     exit;
  lSQL.Add(');');
  gSQL.AddStrings(lSQL);
  sText:=iSQL.Text;
  if Not sText.IsEmpty then
     Begin
       addCmd('--GO');
       gSQL.AddStrings(iSQL);
       addCmd('--GO');
     End;
end;

{ TTableDefinition }

procedure TTableDefinition.alterTable(const pSQL: TStringList);
begin
  if Not pSQL.Text.IsEmpty then
    aSQL.AddStrings(pSQL);
end;

procedure TTableDefinition.auditProcTable;

  function mysqlFields(pre: String): String;
  var sQry: String;
      i: Integer;
  begin
    sQry:='';
    for i:=0 to afldNames.Count-1 do
      if (afldNames.Names[i]=lTableName) and
         (afldNames.Names[i]<>'updatedat') then
      begin
        Var aLine:=afldNames.ValueFromIndex[i];
        sQry:=sQry+',"'+GetStr(aLine,1)+'",'+
                   ''+pre+'.'+GetStr(aLine,1)+'';
      end;
    delete(sQry,1,1);
    result:='JSON_OBJECT('+sQry+')';
  end;

  function sqliteFields(pre: String): String;
  var sQry: String;
      i: Integer;
  begin
    sQry:='';
    for i:=0 to afldNames.Count-1 do
      if (afldNames.Names[i]=lTableName) and
         (afldNames.Names[i]<>'updatedat') then
      begin
        Var aLine:=afldNames.ValueFromIndex[i];
        var sPrefix:= '''"'+GetStr(aLine,1)+'":';
        case  TFieldType(GetInt(aLine,2)) of
         ftAutoInc,
         ftGUId,
         ftBoolean,
         ftInteger,
         ftFloat,
         ftExtended,
         ftCurrency,
         ftSmallInt:  sQry:=sQry+sPrefix+'''||'+pre+'.'+GetStr(aLine,1)+'||'',''||';
         ftMemo,
         ftDate,
         ftTimeStamp,
         ftDateTime,
         ftString:    sQry:=sQry+sPrefix+'"''||'+pre+'.'+GetStr(aLine,1)+'||''",''||';
        end;
      end;
    delete(sQry,Length(sQry)-3,6);
    result:='''{''||'+sQry+'}''';
  end;

Var
   aAudit: String;
begin
  case RDBMSKind Of
{$REGION ' SQLite Triggers '}
  TFDRDBMSKinds.SQLite:
    Begin
      aAudit:=ASchema+lTableName+'_audins';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('BEFORE INSERT ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('BEGIN');
      tSQL.Add('  INSERT INTO '+ASchema+LogTable+' (');
      tSQL.Add('          eventTime, user_id, username,tablename, operation,');
      tSQL.Add('          beforeValue, afterValue');
      tSQL.Add('         )');
      tSQL.Add('         SELECT DATETIME(CURRENT_TIMESTAMP, ''localtime''), ');
      tSQL.Add('                u.user_id, u.user_name,');
      tSQL.Add('                '+QuotedStr(ASchema+lTableName)+',''INSERT'',');
      tSQL.Add('                NULL,');
      tSQL.Add('                '+sqliteFields('NEW')+'');
      tSQL.Add('           FROM userlogged u;');
      tSQL.Add('END;');
      tSQL.Add('--GO');

      aAudit:=ASchema+lTableName+'_audupd';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('BEFORE UPDATE ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('BEGIN');
      tSQL.Add('  INSERT INTO '+ASchema+LogTable+' (');
      tSQL.Add('          eventTime, user_id, username,tablename, operation, ');
      tSQL.Add('          beforeValue, afterValue');
      tSQL.Add('         )');
      tSQL.Add('         SELECT DATETIME(CURRENT_TIMESTAMP, ''localtime''), ');
      tSQL.Add('                u.user_id,u.user_name,');
      tSQL.Add('                '+QuotedStr(ASchema+lTableName)+',''UPDATE'',');
      tSQL.Add('                '+sqliteFields('OLD')+',');
      tSQL.Add('                '+sqliteFields('NEW')+'');
      tSQL.Add('           FROM userlogged u;');
      tSQL.Add('END;');
      tSQL.Add('--GO');

      aAudit:=ASchema+lTableName+'_auddel';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('BEFORE DELETE ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('BEGIN');
      tSQL.Add('  INSERT INTO '+ASchema+LogTable+' (');
      tSQL.Add('          eventTime, user_id,username,tablename, operation, ');
      tSQL.Add('          beforeValue, afterValue');
      tSQL.Add('         )');
      tSQL.Add('         SELECT DATETIME(CURRENT_TIMESTAMP, ''localtime''), ');
      tSQL.Add('                u.user_id,u.user_name,');
      tSQL.Add('                '+QuotedStr(ASchema+lTableName)+',''DELETE'',');
      tSQL.Add('                NULL,');
      tSQL.Add('                '+sqliteFields('NEW')+'');
      tSQL.Add('           FROM userlogged u;');
      tSQL.Add('END;');
      tSQL.Add('--GO');
    End;
{$ENDREGION}

{$REGION ' MYSQL Triggers '}
  TFDRDBMSKinds.MYSQL:
    Begin
      aAudit:=ASchema+lTableName+'_audins';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('BEFORE INSERT ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('BEGIN');
      tSQL.Add('  CALL '+PR_AUDIT_TRACE+'(''INSERT'','+QuotedStr(ASchema+lTableName)+
                       ',null,'+mysqlFields('NEW')+');');
      tSQL.Add('END;');
      tSQL.Add('--GO');

      aAudit:=ASchema+lTableName+'_audupd';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('BEFORE UPDATE ON  '+ASchema+lTableName+'');
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('BEGIN');
      tSQL.Add('  CALL '+PR_AUDIT_TRACE+'(''UPDATE'','+QuotedStr(ASchema+lTableName)+
                       ','+mysqlFields('OLD')+','+mysqlFields('NEW')+');');
      tSQL.Add('END;');
      tSQL.Add('--GO');

      aAudit:=ASchema+lTableName+'_auddel';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('BEFORE DELETE ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('BEGIN');
      tSQL.Add('  CALL '+PR_AUDIT_TRACE+'(''DELETE'','+QuotedStr(ASchema+lTableName)+
                       ',null,'+mysqlFields('OLD')+');');
      tSQL.Add('END;');
      tSQL.Add('--GO');
    End;
{$ENDREGION}

{$REGION ' POSTGRESQL Triggers '}
  TFDRDBMSKinds.PostgreSQL:
    Begin
      tSQL.Add('CREATE OR REPLACE TRIGGER audit_'+lTableName);
      tSQL.Add(' AFTER INSERT OR UPDATE OR DELETE');
      tSQL.Add('    ON '+ASchema+lTableName);
      tSQL.Add('   FOR EACH ROW');
      tSQL.Add('       EXECUTE PROCEDURE '+PR_AUDIT_TRACE+'();');
      tSQL.Add('--GO');
    End;
{$ENDREGION}

{$REGION ' MSSQL Triggers '}
  TFDRDBMSKinds.MSSQL:
    Begin
      aAudit:=ASchema+lTableName+'_audins';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('    ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR INSERT AS');
      tSQL.Add('SET NOCOUNT ON');
      tSQL.Add('BEGIN');
      tSQL.Add('  DECLARE');
      tSQL.Add('    @NEW VARCHAR(MAX);');
      tSQL.Add('  SET @NEW=(SELECT * FROM inserted FOR JSON PATH, WITHOUT_ARRAY_WRAPPER);');
      tSQL.Add('  EXEC '+PR_AUDIT_TRACE+' ''INSERT'', '''+lTableName+''', NULL, @NEW;');
      tSQL.Add('END;');
      tSQL.Add('--GO');

      aAudit:=ASchema+lTableName+'_audupd';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+';');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('    ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR UPDATE AS');
      tSQL.Add('SET NOCOUNT ON');
      tSQL.Add('BEGIN');
      tSQL.Add('  DECLARE');
      tSQL.Add('    @OLD VARCHAR(MAX),');
      tSQL.Add('    @NEW VARCHAR(MAX);');
      tSQL.Add('  SET @OLD=(SELECT * FROM deleted FOR JSON PATH, WITHOUT_ARRAY_WRAPPER);');
      tSQL.Add('  SET @NEW=(SELECT * FROM inserted FOR JSON PATH, WITHOUT_ARRAY_WRAPPER);');
      tSQL.Add('  EXEC '+PR_AUDIT_TRACE+' ''UPDATE'', '''+lTableName+''', @OLD, @NEW;');
      tSQL.Add('END;');
      tSQL.Add('--GO');

      aAudit:=ASchema+lTableName+'_auddel';
      tSQL.Add('DROP TRIGGER IF EXISTS '+aAudit+'');
      tSQL.Add('--GO');
      tSQL.Add('CREATE TRIGGER '+aAudit);
      tSQL.Add('    ON '+ASchema+lTableName+'');
      tSQL.Add('   FOR DELETE AS');
      tSQL.Add('SET NOCOUNT ON');
      tSQL.Add('BEGIN');
      tSQL.Add('  DECLARE');
      tSQL.Add('    @NEW VARCHAR(MAX);');
      tSQL.Add('  SET @NEW=(SELECT * FROM deleted FOR JSON PATH, WITHOUT_ARRAY_WRAPPER);');
      tSQL.Add('  EXEC '+PR_AUDIT_TRACE+' ''DELETE'', '''+lTableName+''', NULL, @NEW;');
      tSQL.Add('END;');
      tSQL.Add('--GO');
    End;
{$ENDREGION}
  end;
end;

procedure TTableDefinition.lastDateProcTable;
Var
   aDateUpd: String;
begin
  aDateUpd:=ASchema+lTableName+'_upd';

  case RDBMSKind Of
   TFDRDBMSKinds.SQLite:
     Begin
       tSQL.Add('DROP TRIGGER IF EXISTS '+aDateUpd+';');
       tSQL.Add('--GO');
       tSQL.Add('CREATE TRIGGER '+aDateUpd);
       tSQL.Add('AFTER UPDATE ON '+ASchema+lTableName+'');
       tSQL.Add('FOR EACH ROW');
       tSQL.Add('BEGIN');
       tSQL.Add('  UPDATE '+ASchema+lTableName+'');
       tSQL.Add('     SET updatedat = datetime(''now'')');
       tSQL.Add('   WHERE rowid = NEW.rowid;');
       tSQL.Add('END;');
     End;
   TFDRDBMSKinds.MYSQL:
     Begin
       tSQL.Add('DROP TRIGGER IF EXISTS '+aDateUpd+';');
       tSQL.Add('--GO');
       tSQL.Add('CREATE TRIGGER '+aDateUpd);
       tSQL.Add('AFTER UPDATE ON '+ASchema+lTableName+'');
       tSQL.Add('   FOR EACH ROW');
       tSQL.Add('BEGIN');
       tSQL.Add('  SET NEW.updatedat=CURRENT_TIMESTAMP();');
       tSQL.Add('END;');
     End;
   TFDRDBMSKinds.MSSQL:
     Begin
       tSQL.Add('DROP TRIGGER IF EXISTS '+aDateUpd+';');
       tSQL.Add('--GO');
       tSQL.Add('CREATE TRIGGER '+aDateUpd);
       tSQL.Add('    ON '+ASchema+lTableName+'');
       tSQL.Add('   FOR UPDATE AS');
       tSQL.Add('SET NOCOUNT ON;');
       tSQL.Add('UPDATE tbl');
       tSQL.Add('   SET tbl.updatedat=CURRENT_TIMESTAMP');
       tSQL.Add('  FROM '+ASchema+lTableName+' tbl');
       tSQL.Add('       INNER JOIN inserted ins');
       tSQL.Add('       ON (tbl.id=ins.id);');
       tSQL.Add('--GO');
     End;
   TFDRDBMSKinds.PostgreSQL:
     Begin
       aDateUpd:=lTableName+'_upd';
       tSQL.Add('CREATE OR REPLACE TRIGGER '+aDateUpd+'');
       tSQL.Add(' AFTER INSERT OR UPDATE OR DELETE');
       tSQL.Add('    ON '+ASchema+lTableName);
       tSQL.Add('   FOR EACH ROW');
       tSQL.Add('       EXECUTE PROCEDURE '+PR_LAST_UPDATE+'();');
     End;
  end;
  tSQL.Add('--GO');
end;

procedure TTableDefinition.createCommonTriggers(const aSchema: String);
begin
  if onlyDict or rbOnly then
     exit;
  case RDBMSKind of
    TFDRDBMSKinds.PostgreSQL:
      begin
        fSQL.Add('CREATE OR REPLACE FUNCTION '+PR_AUDIT_TRACE+'()');
        fSQL.Add('RETURNS TRIGGER AS $$');
        fSQL.Add('DECLARE');
        fSQL.Add('  user_id int := current_setting(''usession.id'')::int;');
        fSQL.Add('  user_name varchar(60) := current_setting(''usession.name'')::varchar;');
        fSQL.Add('  old_row json := NULL;');
        fSQL.Add('  new_row json := NULL;');
        fSQL.Add('BEGIN');
        fSQL.Add('  IF TG_OP IN (''UPDATE'',''DELETE'') THEN');
        fSQL.Add('     old_row = row_to_json(OLD);');
        fSQL.Add('  ELSIF TG_OP IN (''INSERT'',''UPDATE'') THEN');
        fSQL.Add('        new_row = row_to_json(NEW);');
        fSQL.Add('  END IF;');
        fSQL.Add('  INSERT INTO '+ASchema+LogTable+'');
        fSQL.Add('         (eventtime,user_id,username,tablename,operation,beforevalue,aftervalue)');
        fSQL.Add('         SELECT current_timestamp AT TIME ZONE ''UTC'','+
                           'user_id,user_name,TG_TABLE_NAME,TG_OP,old_row,new_row;');
        fSQL.Add('  RETURN NEW;');
        fSQL.Add('END;');
        fSQL.Add('$$ LANGUAGE plpgsql;');
        fSQL.Add('--GO');

        fSQL.Add('CREATE OR REPLACE FUNCTION '+PR_LAST_UPDATE+'()');
        fSQL.Add('RETURNS TRIGGER AS $$');
        fSQL.Add('BEGIN');
        fSQL.Add('  NEW.updatedat = CURRENT_TIMESTAMP;');
        fSQL.Add('  RETURN NEW;');
        fSQL.Add('END;');
        fSQL.Add('$$ LANGUAGE plpgsql;');
        fSQL.Add('--GO');

        fSQL.Add('CREATE OR REPLACE FUNCTION '+PR_NEXT_SEQUENCE+'()');
        fSQL.Add('RETURNS TRIGGER AS $$');
        fSQL.Add('DECLARE');
        fSQL.Add('  new_id int=0;');
        fSQL.Add('BEGIN');
        fSQL.Add('  ');
        fSQL.Add('  UPDATE '+ASchema+lTableName);
        fSQL.Add('     SET new_id=value, value=value+1');
        fSQL.Add('   WHERE (table_name='+QuotedStr(ASchema+lTableName)+');');
        fSQL.Add('  NEW.id = new_id;');
        fSQL.Add('  RETURN NEW;');
        fSQL.Add('END;');
        fSQL.Add('$$ LANGUAGE plpgsql;');
        fSQL.Add('--GO');

        fSQL.Add('CREATE OR REPLACE FUNCTION '+PR_ENTP_SEQUENCE+'()');
        fSQL.Add('RETURNS TRIGGER AS $$');
        fSQL.Add('DECLARE');
        fSQL.Add('  old_row json := NULL;');
        fSQL.Add('  new_row json := NULL;');
        fSQL.Add('BEGIN');
        fSQL.Add('  RETURN NEW;');
        fSQL.Add('END;');
        fSQL.Add('$$ LANGUAGE plpgsql;');
        fSQL.Add('--GO');
      end;
    TFDRDBMSKinds.MSSQL:
      begin
        fSQL.Add('DROP PROCEDURE IF EXISTS '+PR_AUDIT_TRACE+';');
        fSQL.Add('--GO');
        fSQL.Add('CREATE PROCEDURE '+PR_AUDIT_TRACE+'(');
        fSQL.Add('       @OPERATION VARCHAR(10), @TABLENAME VARCHAR(30),');
        fSQL.Add('       @OLDVALUE VARCHAR(MAX), @NEWVALUE VARCHAR(MAX))');
        fSQL.Add('AS');
        fSQL.Add('SET NOCOUNT ON');
        fSQL.Add('BEGIN');
        fSQL.Add('  DECLARE');
        fSQL.Add('   @USERID INT=');
        fSQL.Add('      COALESCE(CAST(SESSION_CONTEXT(N''user_id'') AS INT),0),');
        fSQL.Add('   @USERNAME VARCHAR(40)=');
        fSQL.Add('      COALESCE(CAST(SESSION_CONTEXT(N''user_name'') AS VARCHAR),''<<DEFAULT>>'');');
        fSQL.Add('  INSERT INTO '+ASchema+LogTable+'');
        fSQL.Add('         (eventtime,user_id,username,tablename,operation,');
        fSQL.Add('          beforevalue, aftervalue)');
        fSQL.Add('         SELECT CURRENT_TIMESTAMP,@USERID,@USERNAME,');
        fSQL.Add('                @TABLENAME,@OPERATION,@OLDVALUE,@NEWVALUE;');
        fSQL.Add('END;');
        fSQL.Add('--GO');
      end;
    TFDRDBMSKinds.MYSQL:
      begin
        fSQL.Add('DROP PROCEDURE IF EXISTS '+PR_AUDIT_TRACE+';');
        fSQL.Add('--GO');
        fSQL.Add('CREATE PROCEDURE '+PR_AUDIT_TRACE+'(');
        fSQL.Add('       IN operation VARCHAR(10), IN tablename VARCHAR(30),');
        fSQL.Add('       IN oldvalue TEXT, IN newvalue TEXT)');
        fSQL.Add('BEGIN');
        fSQL.Add('  INSERT INTO '+ASchema+LogTable+'');
        fSQL.Add('         (eventTime,user_id,username,tablename,operation,');
        fSQL.Add('          beforeValue,afterValue)');
        fSQL.Add('         SELECT CURRENT_TIMESTAMP,');
        fSQL.Add('          COALESCE(@user_id,0),');
        fSQL.Add('          COALESCE(@user_name,''<<DEFAULT>>''),');
        fSQL.Add('          tablename,operation,oldvalue,newvalue;');
        fSQL.Add('END;');
        fSQL.Add('--GO');
      end;
  end;
end;

procedure TTableDefinition.createLocalTriggers(const aSchema: String);
begin
  {}
end;

procedure TTableDefinition.setDefaults( );
begin
  {}
end;

constructor TTableDefinition.create( const pMSchema,
                                           pASchema,
                                           prbTables,
                                           prbFields: String;
                                           PSequence: Boolean=True);
begin
  fSQL:=TStringList.Create; // Triggers and procedures
  tSQL:=TStringList.Create; // Triggers and procedures
  aSQL:=TStringList.Create; // alter tables

  inherited create(pMSchema,pASchema,prbTables,prbFields,PSequence);
end;

destructor TTableDefinition.destroy;
begin
  fSQL.Destroy;
  tSQL.Destroy;
  aSQL.Destroy;
  inherited;
end;

procedure TTableDefinition.makeTable;
Var sSql: String;
begin
  inherited;
  if onlyDict or rbOnly then
     exit;
  sSql:=lSQL.Text;
  if auditTable then
     auditProcTable;
  if ContainsText(sSql,'updatedat') then
     lastDateProcTable;
end;

procedure TTableDefinition.dropAllTables;
begin
end;

procedure TTableDefinition.dropTable(fTable: String);
Var sCmd: String;
begin
  sCmd:='';
  if Not (onlyDict or rbOnly) then
     begin
       sCmd:='DROP TABLE IF EXISTS '+ASchema+fTable+';';
       case RDBMSKind Of
         TFDRDBMSKinds.MSSQL,
         TFDRDBMSKinds.PostgreSQL:
           begin
             ;
             //sCmd:=sCmd+ #13'DROP SEQUENCE IF EXISTS '+aSchema+'seq_'+fTable+';';
           end;
         TFDRDBMSKinds.MySQL:
           ;
       end;
       AddCmd(sCmd);
     end;
end;

procedure TTableDefinition.addAlterTables;
begin
  if onlyDict or rbOnly then
     Exit;
end;

procedure TTableDefinition.executeSQL;
begin
  if not sqlFileName.IsEmpty then
     gSQL.SaveToFile(sqlFileName);
  inherited;
end;

procedure TTableDefinition.OthertExecuteSQL;
Var
   sFileName: String;
begin
  sFileName:=GetStr(sqlFileName,1,'.');
  // Alter tables
  if not aSQL.Text.IsEmpty then
     begin
       if not sFileName.IsEmpty then
          aSQL.SaveToFile(sFileName+'_alt.sql');
       processSQL(aSQL);
     end;
  // Tiggers & procedures
  if not fSQL.Text.IsEmpty then
     begin
       if not sFileName.IsEmpty then
          fSQL.SaveToFile(sFileName+'_ftr.sql');
       processSQL(fSQL);
     end;
  if not tSQL.Text.IsEmpty then
     begin
       if not sFileName.IsEmpty then
          tSQL.SaveToFile(sFileName+'_ttr.sql');
       processSQL(tSQL);
     end;
  // Dictionary
  if not dSQL.Text.IsEmpty then
     begin
       if not sFileName.IsEmpty then
          dSQL.SaveToFile(sFileName+'_dic.sql');
       processSQL(dSQL);
     end;
end;

procedure TTableDefinition.initTable( const tableName: String;
                                      tAttr: TSetTableAttributes=[tbDict,tbAudit]);
begin
  Inherited initTable(tableName,tAttr);
  auditTable:=(tbAudit In tAttr);
  if (tbNoAudit in tAttr) or
     (tbOnlyDict in tAttr) then
     auditTable:=false;
end;

end.
