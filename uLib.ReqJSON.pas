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
function callAdd( const DBTableName, Context: String): TJSONObject;
function callUpd( const DBTableName, Context: String; sCondition: String=''): TJSONObject;
function callDel( const DBTableName, sCondition: String): TJSONObject;


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

function callUpd( const DBTableName, Context: String; sCondition: String=''): TJSONObject;
Var sWhere: String;
    aJSON: TJSONObject;
begin
  sWhere:=getQueryParams(sCondition);
  aJSON:= UpdData(DBTableName, Context, sWhere) As TJSONObject;
  if GetInt(aJSON,'error')=0 then
     result:=SetJSONResponse( 0,'Successfully updated!','')
  else
     result:=SetJSONResponse( -1,
                             GetStr(aJSON,'errormsg'),
                              aJSON);

end;

function callAdd( const DBTableName, Context: String): TJSONObject;
Var aJSON: TJSONObject;
begin
  aJSON:= AddData(DBTableName, Context) As TJSONObject;
  if GetInt(aJSON,'error')=0 then
     result:=SetJSONResponse( 0,
                             'Successfully updated!',
                              GetStr(aJSON.ToString,'insertedRows'))
  else
     result:=SetJSONResponse( -1,
                             GetStr(aJSON,'errormsg'),
                              aJSON);
end;

function callDel( const DBTableName, sCondition: String): TJSONObject;
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
  sWhere:=getQueryParams(sCondition);
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