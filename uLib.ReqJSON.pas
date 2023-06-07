unit uLib.ReqJSON;

interface

uses
   System.Classes
  ,System.JSON
  ,System.UITypes
  ;

function callGet( const sTblName: String;
                        sfields: String='*';
                        sCondition: String='';
                        sOrder: string=''): TJSONArray;
function callGetOne( const sTblName: String;
                        sfields: String;
                        sCondition: String): TJSONObject;

function callAdd( const DBTableName, Context: String): TJSONObject;
function callUpd( const DBTableName, Context, sCondition: String): TJSONObject;
function callDel( const DBTableName, sCondition: String): TJSONObject;

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

  procedure setCompare(var sWhere: String; aExpr: String; Idx: integer);
  var
     aField,
     aValue: String;
  begin
    if aExpr.IsEmpty then
       exit;

    aField:=GetStr(aExpr,1,OPERATORS[Idx]);
    aValue:=GetStr(aExpr,2,OPERATORS[Idx]);
    if aValue<>'' then
       begin
         if not IsNumeric(aValue) then
            aValue:=aValue.QuotedString;
         if Not sWhere.IsEmpty Or (Idx>0) then
            sWhere:= sWhere+' AND '+
                    '('+aField.ToLower+OPERATORS[Idx]+aValue+')';
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
     setCompare(sWhere,aExpr,I);
   end;
  Result:=sWhere;
end;

function callUpd( Const DBTableName, Context, sCondition: String): TJSONObject;
Var IOResult: Integer;
    sWhere: String;
begin
  sWhere:=getQueryCondition(sCondition);
  IOResult:= UpdData(DBTableName, Context, sWhere);
  Result:= TJSONObject.Create(TJSONPair.Create('result',TJSONNumber.Create(IOResult)));
end;

function callAdd( Const DBTableName, Context: String): TJSONObject;
Var sResult: String;
begin
  sResult:= AddData(DBTableName, Context).ToString;
  Result:= TJSONObject.Create(TJSONPair.Create('result',CreateTJSONvalue(sResult)));
end;

function callDel( Const DBTableName, sCondition: String): TJSONObject;
Var IOResult: Integer;
    sWhere: String;
begin
  //sWhere:=getQueryCondition(sCondition);
  sWhere:= sCondition;
  IOResult:= DelData(DBTableName,sWhere);
  Result:= TJSONObject.Create(TJSONPair.Create('result',TJSONNumber.Create(IOResult)));
end;


function callGet( const sTblName: String;
                  sfields: String='*';
                  sCondition: String='';
                  sOrder: string=''): TJSONArray;
var
  sQry,
  sWhere: String;
begin
  if sfields='' then
     sfields:='*';
  sWhere:=getQueryCondition(sCondition);
  sQry:=
    'SELECT '+sfields.ToLower+
    '  FROM '+sTblName.ToLower+' {IF MSSQL} WITH (NOLOCK) {fi} '#13;
  if (sWhere<>'') then
     sQry:=sQry+' WHERE '+sWhere+#13;
  if sOrder<>'' then
     sQry:=sQry+' ORDER BY '+sOrder;
  sQry:=sQry+';';
  Result:= GetData(sQry);
end;

function callGetOne( const sTblName: String;
                           sfields: String;
                           sCondition: String): TJSONObject;
begin
  result:=JSONArrayToObject(CallGet(sTblName,sfields,sCondition));
end;

end.