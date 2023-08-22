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