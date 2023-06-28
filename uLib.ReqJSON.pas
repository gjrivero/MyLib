unit uLib.ReqJSON;

interface

uses
   System.Classes
  ,System.JSON
  ,System.UITypes
  ,FireDAC.Stan.Param
  ;

function callGet( const sTblName: String;
                        sfields: String='*';
                        sCondition: String='';
                        sOrder: string=''): TJSONArray; overload;
function callGetOne( const sTblName: String;
                        sfields: String;
                        sCondition: String): TJSONObject; overload;

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
Var sWhere: String;
    aJSON: TJSONObject;
begin
  sWhere:=getQueryCondition(sCondition);
  aJSON:= UpdData(DBTableName, Context, sWhere);
  if GetInt(aJSON,'error')=0 then
     result:=SetJSONResponse( 0,
                             'Successfully updated!',
                              aJSON)
  else
     result:=SetJSONResponse( -1,
                             GetStr(aJSON,'errormsg'),
                              aJSON);

end;

function callAdd( Const DBTableName, Context: String): TJSONObject;
Var aJSON: TJSONObject;
begin
  aJSON:= AddData(DBTableName, Context);
  if GetInt(aJSON,'error')=0 then
     result:=SetJSONResponse( 0,
                             'Successfully updated!',
                              aJSON)
  else
     result:=SetJSONResponse( -1,
                             GetStr(aJSON,'errormsg'),
                              aJSON);
end;

function callDel( Const DBTableName, sCondition: String): TJSONObject;
Var sWhere: String;
    aJSON: TJSONObject;
begin
  //sWhere:=getQueryCondition(sCondition);
  sWhere:= sCondition;
  aJSON:= DelData(DBTableName,sWhere);
  if GetInt(aJSON,'error')=0 then
     result:=SetJSONResponse( 0,
                             'Successfully deleted!',
                              aJSON)
  else
     result:=SetJSONResponse( -1,
                             GetStr(aJSON,'errormsg'),
                              aJSON);
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