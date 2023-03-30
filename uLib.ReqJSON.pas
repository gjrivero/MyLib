unit uLib.ReqJSON;

interface

uses
   System.Classes
  ,System.JSON
  ,System.UITypes
  ;

function callGet( sTblName: String;
                  sfields: String='*';
                  sCondition: String='';
                  sOrder: string=''): TJSONValue;

function callAdd( const DBTableName, Context: String): TJSONValue;
function callUpd( const DBTableName, Context, sCondition: String): TJSONValue;
function callDel( const DBTableName, sCondition: String): TJSONValue;

function getQueryCondition(const sCondition: String): String;

implementation

uses
   System.StrUtils
  ,System.SysUtils
  ,System.Generics.Collections
  ,Data.DBXPlatform

  ,uLib.Base
  ,uLib.Data
  ,uLib.Helpers
  ;

function getQueryCondition(const sCondition: String): String;
const
   OPERATORS: Array[0..6] Of String=
      ('>','<','!=','>=','<=','=','#');
Var
  metaData: TDSInvocationMetadata;
  aExpr,
  aField,
  aValue,
  sWhere: String;
  I: Integer;
begin
  metaData := GetInvocationMetadata;
  sWhere:=sCondition;
  for i := 0 to Pred(metaData.QueryParams.Count) do
   begin
     aExpr:=metaData.QueryParams[i];
     if aExpr.Contains(OPERATORS[I]) then
        begin
          aField:=GetStr(aExpr,1,OPERATORS[I]);
          aValue:=GetStr(aExpr,2,OPERATORS[I]);
          if aValue<>'' then
             begin
               if not IsNumeric(aValue) then
                  aValue:=aValue.QuotedString;
               if Not sWhere.IsEmpty Or (i>0) then
                  sWhere:= sWhere+' AND '+
                    '('+aField.ToLower+OPERATORS[I]+aValue+')';
               break;
             end;
        end;
   end;
  Result:=sWhere;
end;

function callUpd( Const DBTableName, Context, sCondition: String): TJSONValue;
Var IOResult: Integer;
    sWhere: String;
begin
  sWhere:=getQueryCondition(sCondition);
  IOResult:= UpdData(DBTableName, Context, sWhere);
  Result:= TJSONObject.Create(TJSONPair.Create('result',TJSONNumber.Create(IOResult)));
end;

function callAdd( Const DBTableName, Context: String): TJSONValue;
Var sResult: String;
begin
  sResult:= AddData(DBTableName, Context);
  Result:= TJSONObject.Create(TJSONPair.Create('result',CreateTJSONvalue(sResult)));
end;

function callDel( Const DBTableName, sCondition: String): TJSONValue;
Var IOResult: Integer;
    sWhere: String;
begin
  //sWhere:=getQueryCondition(sCondition);
  sWhere:=sCondition;
  IOResult:= DelData(DBTableName,sWhere);
  Result:= TJSONObject.Create(TJSONPair.Create('result',TJSONNumber.Create(IOResult)));
end;

function callGet( sTblName: String;
                  sfields: String='*';
                  sCondition: String='';
                  sOrder: string=''): TJSONValue;
var
  sQry,
  sWhere: String;
begin
(*
    TDictionary<string,string>(metadata.QueryParams)
*)
  if sfields='' then
     sfields:='*';
  sWhere:=getQueryCondition(sCondition);
  sQry:=
    'SELECT '+sfields.ToLower+
    '  FROM '+sTblName.ToLower+' {IF MSSQL} WITH (NOLOCK) {fi}'#13;
  if (sWhere<>'') then
     sQry:=sQry+'WHERE '+sWhere+#13;
  if sOrder<>'' then
     sQry:=sQry+' ORDER BY '+sOrder;
  sQry:=sQry+';';
  Result:= GetData(sQry).AsJSONArray;
end;


end.