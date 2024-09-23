unit uLib.Base;

interface

uses
    Data.DB
   ,System.Types
   ,System.JSON
   ,System.UITypes
   ,System.Classes
   ,System.SysUtils
   ,System.Variants
   ,System.Generics.Collections
   ,System.Net.HttpClient
   ,System.Net.URLClient
   ,System.Net.HttpClientComponent
   ,XML.XMLIntf

   ,REST.Types
   ,FireDAC.Stan.Param;


Const
   CHDIV  = #1;
   DATE_FORMAT = 'YYYY-MM-DD';
   TIME_FORMAT = 'hh:nn:ss';
   DATE_TIME_FORMAT = 'yyyy-mm-dd hh:nn:ss';
   REGEX_EMAIL =  '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-\p{Cyrillic}]'+
               '+\.[a-zA-Z0-9-.\p{Cyrillic}]*[a-zA-Z0-9\p{Cyrillic}]+$';
   RT_HTML = 23;

Type
   TRandAttributes = (rdAllChar, rdAlpha, rdNumber, rdEspecial );
   TDateTimeMode = (dtNone, dtNow, dtBegin, dtEnd);

var
   App_Name,
   App_Path,
   App_Params,
   App_Version,
   App_FileLogs: String;


function  JSONFilter(sJSON: String): String;
function  FToStrSQL(ValueFloat: Extended; aDec: Integer=6): String; overload;
function  FToStrSQL(const Value: String; aDec: Integer=6): String; overload;
function  StrToReal(const St: String; defDec: Integer=0): Double;
function  StrToInteger(St: String): Integer;
function  DeleteLastChar(const S: String; ch: Char): String;
function  DateStr(Date: TDateTime=-1): String;
function  DateTimeStr(Date: TDateTime=-1; Mode: TDateTimeMode = dtNone): String;
function  StrToDateTimeFmt(sDateTime: String): TDateTime;
function  FormatDef(T: Double; DefDecimals: Integer=2): String; Overload;
function  FormatDef(S: String; DefDecimals: Integer=2): String; Overload;
function  GoodReal(R: Double; defDec: Integer=2): Double; Overload;
function  GoodReal(S: String; defDec: Integer=2): Double; Overload;
function  DivCero(A,B: Double): Double;
function  LeftS(S: String; Len: Integer; Ch: Char=' '): String;

function  GetFloat(OJSON: TJSONObject; const fieldname: String; Default: Double=0): Double; Overload;
function  GetFloat(const sJSON, fieldname: String; Default: Double=0): Double; Overload;
function  GetFloat(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): Double; OverLoad;
function  GetFloat(const St: String; Index: Integer; CDiv: Char=ChDiv): Double; OverLoad;
function  GetFloat(Item: IXMLNode; const AttribName: String; Default: Double=0): Double; Overload;
function  GetFloat(Item: IXMLNode; Default: Double=0): Double; Overload;
function  GetFloat(FT: TDataSet; const FieldName: String): Double; OverLoad;

function  GetInt(OJSON: TJSONObject; const fieldname: String; Default: Integer=0): Integer; Overload;
function  GetInt(const sJSON, fieldname: String; Default: Integer=0): Integer; Overload;
function  GetInt(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): Integer; OverLoad;
function  GetInt(const St: String; Index: Integer; CDiv: Char=ChDiv): Integer; Overload;
function  GetInt(Item: IXMLNode; const AttribName: String; Default: Integer=0): Integer; Overload;
function  GetInt(Item: IXMLNode; Default: Integer=0): Integer; Overload;
function  GetInt(FT: TDataSet; const FieldName: String): Integer; Overload;

function  GetStr(OJSON: TJSONObject; const FieldName: String; Default: String=''): String; Overload;
function  GetStr(const sJSON, fieldname: String; Default: String=''): String; Overload;
function  GetStr(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): String; Overload;
function  GetStr(const St: String; Index: Integer; CDiv: String): String; Overload;
function  GetStr(const St: String; Index: Integer; CDiv: Char=ChDiv): String; Overload;
function  GetStr(Item: IXMLNode): String; Overload;
function  GetStr(Item: IXMLNode; const AttribName: String): String; Overload;
function  GetStr(FT: TDataSet; const FieldName: String): String; Overload;

function  GetDate(OJSON: TJSONObject; const fieldname: String; Default: TDateTime=0): TDateTime; Overload;
function  GetDate(const sJSON, fieldname: String; Default: TDateTime=0): TDateTime; Overload;
function  GetDate(FT: TDataSet; const FieldName: String): TDateTime; Overload;
function  GetDate(const St: String; Index: Integer; CDiv: Char=ChDiv): TDateTime; Overload;
function  GetDate(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): TDateTime; Overload;
function  GetDate(Item: IXMLNode; const AttribName: String; Default: TDateTime=0): TDateTime; Overload;
function  GetDate(Item: IXMLNode; Default: TDateTime=0): TDateTime; Overload;

function  GetBool(OJSON: TJSONObject; const FieldName: string): boolean; Overload;
function  GetBool(const sJSON: String; const FieldName: string): boolean; Overload;

procedure JSONRemove( var sJSON: String; const fields: array of string); overload;
procedure JSONRemove( var sJSON: String; const FieldName: String); overload;
procedure JSONRemove( oJSON: TJSONObject; const fields: array of string); overload;

procedure SetStr(FT: TDataSet; const FieldName, Value: String); Overload;
procedure SetStr(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
procedure SetStr(LB: TStrings; Index, N: Integer; const St: String; CDiv: Char=ChDiv ); Overload;

procedure SetInt(FT: TDataSet; const FieldName: String; Value: Integer); Overload;
procedure SetInt(FT: TDataSet; const FieldName: String; Const Value: String); Overload;
procedure SetInt(Var StSource: String; Index: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
procedure SetInt(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
procedure SetInt(LB: TStrings; Index, N: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
procedure SetInt(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;

procedure SetFloat(FT: TDataSet; const FieldName: String; Value: Double); Overload;
procedure SetFloat(FT: TDataSet; const FieldName: String; const Value: String); Overload;
procedure SetFloat(Var StSource: String; Index: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
procedure SetFloat(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
procedure SetFloat(LB: TStrings; Index, N: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
procedure SetFloat(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;

procedure SetDate(FT: TDataSet; const FieldName: String; Value: TDateTime); Overload;
procedure SetDate(FT: TDataSet; const FieldName: String; const Value: String); Overload;
procedure SetDate(Var StSource: String; Index: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
procedure SetDate(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
procedure SetDate(LB: TStrings; Index, N: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
procedure SetDate(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;

procedure SetFlds( var sLine: String;
                   const fields: array of Integer;
                   const values: array of const;
                   pChDiv: Char=CHDIV);

procedure SetJSON( Var sJSON: String;
                   const fields: array of string;
                   const values: array of const;
                   findExist: boolean=false); overload;
procedure SetJSON( var sJSON: PChar;
                    const fields: array of string;
                    const values: array of const;
                   findExist: boolean=false); overload;
procedure SetJSON( JSON: TJSONObject;
                   const fields: array of string;
                   const values: array of const;
                   findExist: boolean=false); overload;

procedure SetJSONValue( aJSON: TJSONObject;
                        const fields: array of string;
                        const Values: array Of const); overload;
procedure SetJSONValue( var aJSON: string;
                        const fields: array of string;
                        const Values: Array Of const); overload;

procedure SetJSONValue( AJSON: TJSONObject;
                        const AName: String;
                        BJSON: TJSONObject;
                        APath: String=''); overload;
procedure SetJSONValue( AJSON: TJSONObject;
                        const AName: string;
                        const BJSON: string;
                        APath: String=''); overload;

procedure SetJSONValue( var AJSON: String;
                        AName: String;
                        BJSON: TJSONObject;
                        APath: String=''); overload;
procedure SetJSONValue( var AJSON: String;
                        AName: String;
                        const BJSON: string;
                        APath: String=''); overload;


procedure SetJSONValue( aJSON, bJSON: TJSONObject;
                        const fields: array of string); overload;
procedure SetJSONValue( aJSON: TJSONObject;
                        const bJSON: string;
                        const fields: array of string); overload;

procedure SetJSONValue( var aJSON: string;
                        bJSON: TJSONObject;
                        const fields: array of string); overload;
procedure SetJSONValue( var aJSON: string;
                        const bJSON: string;
                        const fields: array of string); overload;

procedure SaveLogFile(const sMessage: String);

function  GetMaxFields(const fields: String; Cdiv: Char=ChDiv): Integer;
function  SetDefaultLine(MaxFields: Integer; Cdiv: Char=ChDiv): String;

procedure DiscountAmount( Var BolPor: String;
                          Var Porcentaje,
                              Descont: Double;
                              TotalF: Double;
                              CheckP: Boolean=false);

function NetHTTPReq( pMethod: TRESTRequestMethod;
                     const BaseURI, Path: String;
                     sJSON: String='';
                     LHeader: TNetHeaders=Nil
                    ): IHTTPResponse; overload;

function NetHTTPReq( pMethod: TRESTRequestMethod;
                     const BaseURI, Path: String;
                     JSON: TJSONValue;
                     LHeader: TNetHeaders=Nil
                    ): IHTTPResponse; Overload;

function IndexOfList( LT:       TStrings;
                      const     StTarget: String;
                      Partial:  Boolean=false): Integer; overload;

function IndexOfList( LB:       TStrings;
                      const     StTarget: String;
                      Column:   Integer=1;
                      Partial:  Boolean=false;
                      CDiv: Char=ChDiv): Integer; overload;

function TrimS(S: String): String;
function AmpFilter(sVal: String): String;
function AssignVal(const AVarRec: TVarRec): String; Overload;
function AssignVal(const AVarRec: Variant): String; Overload;
function IsNumeric(const AString: string): Boolean;
function VersionStr(nuVerMayor, nuVerMenor, nuVerRelease: Integer): String;
function LoadResourceHTML( const RName: string; RType: PCHAR = Nil): String;
function LoadResourceString( const RName: string; RType: PCHAR = RT_RCDATA ): String;
function StringToHex(const S: String): String;
function HexToString(const S: String): String;
function StreamToString(Stream: TStream; Encoding: TEncoding=nil): string;

procedure GetApplicationPath(LocalPath: Boolean);
procedure Compressfile(const filename: String);
procedure DecompressFile(const filename: String);

function CreateTJSONValue(sJSON: String; aEncoding: TEncoding = nil): TJSONValue;
function CreateTJSONObject(sJSON: String; aEncoding: TEncoding = nil): TJSONObject;
function CreateTJSONArray(sJSON: String; aEncoding: TEncoding = nil): TJSONArray;

function SetJSONResponse( iStatus: Integer;
                          const sMessage: string): TJSONObject; overload;
function SetJSONResponse( iStatus: Integer;
                          const sMessage: string;
                          aJSON: String): TJSONObject; overload;
function SetJSONResponse( iStatus: Integer;
                          const sMessage: string;
                          aJSON: TJSONValue): TJSONObject; overload;

procedure GetFieldsValues( JSON: TJSONObject;
                           var sFields, sValues, sSetVal: string);  overload;
procedure GetFieldsValues( sJSON: String;
                           var sFields, sValues, sSetVal: string);   overload;
procedure GetFieldsValues( pParams: TFDParams;
                           var sFields, sValues, sSetVal: string); overload;
function RandString( ALength: Integer;
                     attr: TRandAttributes=rdAllChar): String;

procedure saveTofile(const pFileName: String;
                     const SourceText,
                          CipherKey,
                          IV: RawByteString;
                          Crypted: Boolean); overload;

procedure saveTofile(const pFileName: String;
                     const SourceText: RawByteString); overload;

function loadFromfile(const pFileName: String;
                      const CipherKey, IV: RawByteString;
                            Crypted: Boolean): RawByteString; overload;

function loadFromfile(const pFileName: String): RawByteString; overload;

function ValidateEmail(const emailAddress: string): Boolean;
function IsInternetConnection: Boolean;
procedure SendSSLMail( Connection: TStringList;
                       Recipients: TStringList;
                       CopyRecipients: TStringList=Nil;
                       AttachFiles: TStringList=Nil);

implementation

uses
    System.IOUtils
   ,System.JSON.Types
   ,System.JSON.Writers
   ,System.JSON.Readers
   ,System.Zlib
   ,System.Math
   ,System.RegularExpressions
   ,System.DateUtils
   ,System.StrUtils
   ,System.NetEncoding
   ,FireDAC.Stan.Intf

   ,IdSSLOpenSSL
   ,IdHTTP
   ,IdSMTP
   ,IdText
   ,IdGlobal
   ,IdMessage
   ,IdAttachment
   ,IdEMailAddress
   ,IdAttachmentFile
   ,IdMessageBuilder
   ,IdExplicitTLSClientServerBase

   ,uLib.DataModule
   ,uLib.Common
   ,uLib.Crypt;

(*
const
   JSONIndent = 2;
*)

function VersionStr(nuVerMayor, nuVerMenor, nuVerRelease: Integer): String;
Begin
  Result:=nuVerMayor.ToString+'.'+
          nuVerMenor.ToString+'.'+
          nuVerRelease.ToString;
End;

function RandString(
          ALength: Integer;
          attr: TRandAttributes=rdAllChar): String;

  function CharSetToString(const Chars: TSysCharSet): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to 255 do
      if CharInSet(Chr(I),Chars) then
        Result := Result + Chr(I);
  end;
var
  C1: Integer;
  ACharSequence: TSysCharSet;
  ASequence: String;
begin
  SetLength(result, ALength);
  case Attr of
   rdAlpha: ACharSequence:=[#65..#90]+[#97..#122];
   rdNumber: ACharSequence:=[#48..#57];
   rdEspecial: Begin
                 ACharSequence:=
                   [#33..#47]+
                   [#58..#64]+
                   [#91..#96]+
                   [#123..#126];
               end;
   rdAllChar: ACharSequence:=[#33..#126];
  end;
  ASequence:=CharSetToString(ACharSequence);
  for C1 := 1 to ALength do
    result[C1] := ASequence[Random(Length(ASequence))+1];
end;

function AmpFilter(sVal: String): String;
Var S: String;
begin
  S:=Trim(sVal);
  if (Length(S.DeQuotedString)>1) and
     (S.DeQuotedString[1]='`') then
     begin
       S:=S.DeQuotedString;
       Delete(S,1,1);
       Delete(S,Pos('`',S),1);
     end;
  Result:=S;
end;

function JSONFilter(sJSON: String): String;
begin
  // \b  Backspace (ascii code 08)
  sJSON:=ReplaceStr(sJSON,#08,'\b');
  // \f  Form feed (ascii code 0C)
  sJSON:=ReplaceStr(sJSON,#12,'\f');
  // \n  New line
  sJSON:=ReplaceStr(sJSON,#10,'\n');
  // \r  Carriage return
  sJSON:=ReplaceStr(sJSON,#13,'\r');
  // \t  Tab
  sJSON:=ReplaceStr(sJSON,#09,'\t');
  // \\  Backslash character
  sJSON:=ReplaceStr(sJSON,'\','\\');
  // \"  Double quote
  sJSON:=ReplaceStr(sJSON,'"','\"');
  result:=sJSON;
end;

function AssignVal(const AVarRec: TVarRec): String; Overload;
var Str: String;
begin
  Str:='';
  case AVarRec.VType of
   vtBoolean: Str:=AVarRec.VBoolean.ToString(True);
   vtInt64: Str:=AVarRec.VInt64^.ToString;
   vtInteger: Str:=AVarRec.VInteger.ToString;
   vtObject:  Str:=UnicodeString(AVarRec.VUnicodeString);
   vtExtended,
   vtCurrency: Str:=AVarRec.VExtended^.ToString;

   vtWideChar: Str:=QuotedStr(AVarRec.VWideChar);
   vtWideString: Str:=WideString(AVarRec.VWideString);
   vtChar: Str:=QuotedStr(AVarRec.VWideChar);
   vtPChar: Str:=QuotedStr(UnicodeString(AVarRec.VPChar^));
   vtPWideChar: Str:=QuotedStr(AVarRec.VPWideChar^);
   vtString: Str:=QuotedStr(AVarRec.VString^);
   vtAnsiString: Str:=QuotedStr(UnicodeString(AnsiString(AVarRec.VAnsiString)));
   vtUnicodeString: Str:=QuotedStr(UnicodeString(AVarRec.VUnicodeString));
  end;
  Result:=Str;
end;

function AssignVal(const AVarRec: Variant): String; Overload;
var Str: String;
begin
  Str:='';
  if Not VarIsNull(AVarRec) then
     if VarIsNumeric(AVarRec) then
        Str:=VarToStr(AVarRec)
     else
        Str:=QuotedStr(VarToStr(AVarRec));
  Result:=Str;
end;

{$HINTS OFF}
function IsNumeric(const AString: string): Boolean;
var
  LCode: Integer;
  LVoidR: Double;
begin
  LVoidR:=0;
  Val(AString, LVoidR, LCode);
  Result := (LCode=0);
end;
{$HINTS ON}

function JSONObjectToParams(JSON: TJSONObject): TFDParams;
Var
   field,
   value: String;
   I: Integer;
begin
  result:=Nil;
  for I := 0 to JSON.count-1 do
    begin
      if Result=nil then
         result:=TFDParams.Create;
     field:=JSON.Pairs[I].JsonString.Value.ToLower;
     value:=JSON.Pairs[I].JsonValue.ToString;
     if Value.StartsWith('"') then
        begin
          delete(Value,1,1);
          delete(Value,Length(Value),1);
        end;
      if IsNumeric(value) then
         begin
           if ContainsText(value,'.') then
              result.Add(field,StrToReal(value))
           else
              result.Add(field,StrToInteger(value));
         end
      else
        result.Add(field,value);
    end;
end;

function SetJSONResponse( iStatus: Integer;
                          const sMessage: string): TJSONObject;
begin
  Result:=TJSONObject.Create;
  SetJSON(Result,['status','message'],
                 [iStatus,sMessage]);
end;

function SetJSONResponse( iStatus: Integer;
                          const sMessage: string;
                          aJSON: String): TJSONObject;
begin
  Result:=TJSONObject.Create;
  SetJSON(Result,['status','message'],
                 [iStatus,sMessage]);
  if (aJSON<>'') then
     SetJSON(Result,['response'],[aJSON]);
end;

function SetJSONResponse( iStatus: Integer;
                          const sMessage: string;
                          aJSON: TJSONValue): TJSONObject;
begin
  Result:=TJSONObject.Create;
  SetJSON(Result,['status','message'],
                 [iStatus,sMessage]);
  if (aJSON<>Nil) and Assigned(aJSON) then
     SetJSON(Result,['response'],[aJSON]);
end;

//-------------------------------------------------//
//  JSON: Functions                                //
//-------------------------------------------------//

function CreateTJSONObject(sJSON: String; aEncoding: TEncoding = nil): TJSONObject;
Begin
  if aEncoding=nil then
     aEncoding:=TEncoding.UTF8;
  Result:= TJSONObject.ParseJSONValue(aEncoding.GetBytes(sJSON), 0) as TJSONObject;
End;

function CreateTJSONvalue(sJSON: String; aEncoding: TEncoding = nil): TJSONValue;
Begin
  if aEncoding=nil then
     aEncoding:=TEncoding.UTF8;
  Result:= TJSONObject.ParseJSONValue(aEncoding.GetBytes(sJSON), 0) as TJSONValue;
End;

function CreateTJSONArray(sJSON: String; aEncoding: TEncoding = nil): TJSONArray;
Begin
  if aEncoding=nil then
     aEncoding:=TEncoding.UTF8;
  Result:= TJSONObject.ParseJSONValue(aEncoding.GetBytes(sJSON), 0) as TJSONArray;
End;

function OkVarRec(const AVarRec: TVarRec): Boolean;  overload;
var Ok: Boolean;
Begin
  Ok:=False;
  case AVarRec.VType of
   vtInt64,
   vtInteger:    Ok:=AVarRec.VInteger<>0;
   vtBoolean:    Ok:=AVarRec.VBoolean;
   vtChar:       Ok:=AVarRec.VChar<>'';
   vtExtended:   Ok:=AVarRec.VExtended^<>0;
   vtString:     Ok:=AVarRec.VString^<>'';
   vtPChar:      Ok:=String(AVarRec.VPChar)<>'';
   vtPWideChar:  Ok:=String(AVarRec.VPWideChar)<>'';
   vtAnsiString: Ok:=string(AVarRec.VAnsiString)<>'';
   vtCurrency:   Ok:=AVarRec.VCurrency^<>0;
   vtVariant:    Ok:=TDateTime(AVarRec.VVariant^)<>0;
   vtWideChar:   Ok:=AVarRec.VWideChar<>'';
   vtWideString: Ok:=WideCharToString(AVarRec.VWideString)<>'';
   vtUnicodeString: Ok:=AVarRec.VPWideChar<>'';
  end;
 Result:=Ok;
End;

function OkVarRec(const AVarRec: Variant): Boolean;   overload;
begin
  Result:=Not VarIsNull(AVarRec);
end;

procedure GetFieldsValues( JSON: TJSONObject;
                           var sFields, sValues, sSetVal: string);
Var L: integer;
    field,
    value: String;
begin
  sValues:='';
  sFields:='';
  sSetVal:='';
  for L := 0 to JSON.Count-1 do
   begin
     field:=JSON.Pairs[L].JsonString.Value.ToLower;
     case RDBMSKind of
      TFDRDBMSKinds.MSSQL:
        field:='['+field+']';
     end;
     Value:=JSON.Pairs[L].JsonValue.ToString;
     if Value.StartsWith('"') then
        begin
          delete(Value,1,1);
          delete(Value,Length(Value),1);
          if (Value<>'') And (Value[1]='`') then
             begin
               Delete(Value,1,1);
               Delete(Value,Pos('`',Value),1);
             end
          else
            Value:='N'+Value.QuotedString;
        end
     else
        if Not Value.IsEmpty and (Value[1] In ['[','{']) then
           Case RDBMSKind of
             TFDRDBMSKinds.MSSQL:
               Value:='N'+Value.QuotedString;
             else
               Value:=Value.QuotedString;
           End;
     sFields:=sFields+','+field;
     sValues:=sValues+','+value;
     sSetVal:=sSetVal+','+field+'='+Value;
   End;
  Delete(sValues,1,1);
  delete(sFields,1,1);
  delete(sSetVal,1,1);
end;

procedure GetFieldsValues( sJSON: String;
                           var sFields, sValues, sSetVal: string);
Var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       GetFieldsValues(JSON,sFields,sValues,sSetVal);
       JSON.Destroy;
     end;
end;

procedure GetFieldsValues( pParams: TFDParams;
                           var sFields, sValues, sSetVal: string);
Var
  I: Integer;
  field: string;
begin
  sFields:='';
  sValues:='';
  sSetVal:='';
  for I := 0 to pParams.count-1 do
    begin
      field:=pParams[I].Name;
      case RDBMSKind of
       TFDRDBMSKinds.MSSQL:
         field:='['+field+']';
      end;
      sFields:=sFields+','+field;
      sValues:=sValues+','+pParams[I].Value.AsString;
      sSetVal:=sSetVal+','+field+'='+pParams[I].Value.AsString;
    end;
  System.Delete(sValues,1,1);
  System.delete(sFields,1,1);
  System.delete(sSetVal,1,1);
end;

procedure saveTofile(const pFileName: String;
                     const SourceText,
                          CipherKey,
                          IV: RawByteString;
                          Crypted: Boolean);
var
  Input: RawByteString;
  MS: TStringStream;
begin
  if SourceText='' then
     exit;
  Input:=SourceText;
  if Crypted then
     begin
       Input:=Encrypt(SourceText, CipherKey, IV);
     end;
  MS:=TStringStream.Create(Input);
  try
    MS.SaveToFile(pFileName);
  finally
    MS.Destroy;
  end;
end;

procedure saveTofile(const pFileName: String;
                     const SourceText: RawByteString);
begin
  saveTofile(pFileName,SourceText,'','',false);
end;

function loadFromfile(const pFileName: String;
                      const CipherKey, IV: RawByteString;
                            Crypted: Boolean): RawByteString;
var
  Input: RawByteString;
  MS: TStringStream;
begin
  if not FileExists(pFileName) then
     begin
       result:='';
       exit;
     end;
  MS:=TStringStream.Create;
  try
    MS.LoadFromFile(pfileName);
    Input:=MS.DataString;
    if Crypted then
       begin
         Input:=DeCrypt(MS.DataString, CipherKey, IV);
       end;
    result:=Input;
  finally
    MS.Destroy;
  end;
end;

function loadFromfile(const pFileName: String): RawByteString;
begin
  result:=loadFromfile(pFileName,'','',false);
end;

procedure Compressfile(const filename: String);
var
  LInput, LOutput: TFileStream;
  LZip: TZCompressionStream;
begin
  LInput := TFileStream.Create(filename, fmOpenRead);
  LOutput := TFileStream.Create(ChangeFileExt(fileName, 'zip'), fmCreate);
  LZip := TZCompressionStream.Create(LOutput);
  LZip.CopyFrom(LInput, LInput.Size);
  LZip.Destroy;
  LInput.Destroy;
  LOutput.Destroy
end;

procedure deCompressFile(const filename: String);
var
  LInput, LOutput: TFileStream;
  LUnZip: TZDecompressionStream;
begin
  LInput := TFileStream.Create(fileName, fmOpenRead);
  LOutput := TFileStream.Create(ChangeFileExt(fileName, 'txt'), fmCreate);
  LUnZip := TZDecompressionStream.Create(LInput);
  LOutput.CopyFrom(LUnZip, 0);
  LUnZip.Destroy;
  LInput.Destroy;
  LOutput.Destroy;
end;

(*
function StreamToString(Stream: TStream): string;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    SS.CopyFrom(Stream, 0);  // No need to position at 0 nor provide size
    Result := SS.DataString;
  finally
    SS.Destroy;
  end;
end;
*)

function StreamToString(Stream : TStream; Encoding: TEncoding=Nil) : string;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('',Encoding);
  SS.LoadFromStream(Stream);
  result := SS.DataString;
  SS.Destroy;
end;

function StringToStream(Const Str: String): TStream;
begin
  Result:=TStringStream.Create(Str);
end;

function StringToHex(const S: String): String;
var
  i: integer;
begin
  Result := '';
  for i:= 1 to Length(S) do
    Result:= Result + IntToHex(byte(S[i]),2);
end;

function HexToString(const S: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length( S ) do
   begin
     if ((i mod 2) = 1) then
        Result := Result + Char( StrToInt( '0x' + Copy( S, i, 2 )));
  end;
end;

function FormatDef(T: Double; DefDecimals: Integer=2): String; Overload;
Var
   StC,
   St0,
   sFmt: String;
begin
  StC:='#';
  St0:='0';
  sFmt:=StC+St0;
  if (DefDecimals>0) then
     Begin
       stC:=StC+','+StringOfChar('#',DefDecimals);
       st0:=St0+'.'+StringOfChar('0',DefDecimals);
     End;
  sFmt:=StC+St0;
  Result:=FormatFloat(sFmt,T);
end;

function FormatDef(S: String; DefDecimals: Integer=2): String; Overload;
begin
  Result:=FormatDef(StrToReal(S),DefDecimals);
end;

Function GoodReal(R: Double; defDec: Integer=2): Double; Overload;
Var Fmt: String;
Begin
  fmt:=FormatDef(R,defDec);
  Result:=StrToReal(fmt);
End;

Function GoodReal(S: String; defDec: Integer=2): Double; Overload;
Begin
  Result:=GoodReal(StrToReal(S),defDec);
End;

Function StrToReal(const St: String; defDec: Integer=0): Double;
Var
   I: Integer;
   Valor: Extended;
   okSep: Boolean;
   sNum: String;
Begin
  sNum:='';
  okSep:=False;
  for I := Length(St) downto 1 do
    If CharInSet(St[I],['0'..'9','.',',','-','+','E']) Then
      Case St[I] Of
       '.',
       ',': if not okSep then
               begin
                 sNum:=FormatSettings.DecimalSeparator+sNum;
                 okSep:=True;
               end;
        else
           sNum:=St[I]+sNum;
      end;
  if not sNum.IsEmpty And (sNum[1]=FormatSettings.DecimalSeparator) then
     sNum:='0'+sNum;
  if (sNum='') {or not IsNumeric(sNum)} then
     sNum:='0';
  Try
    Valor := StrToFloat(sNum);
    if defDec>0 then
       Valor:=GoodReal(Valor,defDec);
  Except
    Valor := 0;
  End;
  Result := Valor;
End;

Function StrToInteger(St: String): Integer;
Begin
  Result:=Trunc(StrToReal(St));
End;

Function DivCero(A,B: Double): Double;
Begin
  Result:=0;
  If (B<>0) Then
     Result:=A/B;
End;

Procedure DiscountAmount( Var BolPor: String;
                          Var Porcentaje,
                              Descont: Double;
                              TotalF: Double;
                              CheckP: Boolean=false);
Var
    Valor:  Double;
    TValor: String;
    Porct:  Boolean;
Begin
  { 9.09090909 }
  TValor:=BolPor;
  Porct:=Pos('%',TValor)>0;
  While Pos('%',TValor)>0 Do
   Delete(TValor,Pos('%',TValor),1);
  TValor:=Trim(Tvalor);
  If TValor='' Then
     TValor:='0';
  Valor:=StrToReal(TValor);
  If Porct Then
     Begin
       Porcentaje:=Valor;
       If (Porcentaje>100.0) And CheckP Then
          Begin
            Porcentaje:=0;
            BolPor:='';
            Valor:=TotalF;
          End
       Else
          Valor:=TotalF*Porcentaje/100.0;
     End
  Else
     Porcentaje:=DivCero(Valor*100.0,Totalf);
  Descont:=Valor; //GoodReal(Valor);
  If (Descont<0) Or ((Descont>TotalF) And CheckP) Then
     Begin
       BolPor:='';
       Descont:=0;
       Porcentaje:=0;
     End;
End;

Function GetMaxFields(const fields: String; Cdiv: Char=ChDiv): Integer;
var St,
    sFields: String;
    T,P: Integer;
begin
  T:=0;
  sFields:=fields;
  while (sFields<>'') do
   begin
     P:=Pos(CDiv,sFields);
     St:=Copy(sFields,1,P);
     If (P=0) Then
        St:=Copy(sFields,1,Length(sFields));
     if (TrimS(St)<>'') then
        begin
          Inc(T);
        end;
     Delete(sFields,1,Length(St));
   end;
  Result:=T;
end;

Function SetDefaultLine(MaxFields: Integer; CDiv: Char=ChDiv): String;
Var I: Integer;
    S: String;
Begin
  S:='';
  for I:=1 To  Maxfields do
   S:=S+''+CDiv;
  DeleteLastChar(S,CDiv);
  Result:=S;
End;

procedure SetFlds( var sLine: String;
                   const fields: array of Integer;
                   const values: array of const;
                   pChDiv: Char=CHDIV);
Var I: Integer;
begin
  for I := Low(fields) to High(fields) do
   SetStr(sLine,fields[I], AssignVal(Values[I]).DeQuotedString, pChDiv);
end;

function StrToDateTimeFmt(sDateTime: String): TDateTime;
var
  fs: TFormatSettings;
  dt: TDateTime;
begin
  dt:=-1;
  if not sdateTime.IsEmpty then
     begin
       fs := TFormatSettings.Create;
       fs.DateSeparator := '-';
       fs.ShortDateFormat := 'yyyy-MM-dd';
       fs.TimeSeparator := ':';
       fs.ShortTimeFormat := 'hh:nn';
       fs.LongTimeFormat := 'hh:nn:ss.zzz';
       try
         dt:= StrToDateTime(sDateTime, fs);
       except
         dt:=-1;
       end;
     end;
  result:=dt;
end;

function DeleteLastChar(const S: String; ch: Char): String;
Var St: String;
Begin
  St:=S;
  While Not St.IsEmpty And (St[Length(St)]=Ch) Do
   Delete(St,Length(St),1);
  Result:=St;
End;

{
  unit: WinApi.Windows
  RT_CURSOR       = MakeIntResource(1);
  RT_BITMAP       = MakeIntResource(2);
  RT_ICON         = MakeIntResource(3);
  RT_MENU         = MakeIntResource(4);
  RT_DIALOG       = MakeIntResource(5);
  RT_STRING       = MakeIntResource(6);
  RT_FONTDIR      = MakeIntResource(7);
  RT_FONT         = MakeIntResource(8);
  RT_ACCELERATOR  = MakeIntResource(9);
  RT_RCDATA       = MakeIntResource(10);
  RT_MESSAGETABLE = MakeIntResource(11);
  // NO DEFINIDOS
  RT_ANICURSOR    = MakeIntResource(21)
  RT_ANIICON      = MakeIntResource(22)
  RT_HTML         = MakeIntResource(23);
}

(*

/* standard resource # for HTML */
#define HTML 23

/* HTML resource */
INDEX_PAGE  HTML   "HTML\index.html"   23: Asi tambien funciona
INDEX_PAGE  HTML   "HTML\index.html"
PAGE_1      HTML   "HTML\page1.html"
PAGE_2      HTML   "HTML\page2.html"
LOGO_GIF    HTML   "HTML\logo.gif"
STYLE_CSS   HTML   "HTML\style.css"

/* RCDATA resource */
42          RCDATA "HTML\rcdata.html"

*)


type
  LOWORD = Word;
  {$EXTERNALSYM LOWORD}

function FormatResNameOrType(ResID: PChar): string;

  function HiWord(L: DWORD): Word;
  begin
    Result := L shr 16;
  end;

begin
  if HiWord(LongWord(ResID)) = 0 then
    // high word = 0 => numeric resource id
    // numeric value is stored in low word
    Result := Format('#%d', [LoWord(LongWord(ResID))])
  else
    // high word <> 0 => string value
    // PChar is implicitly converted to string
    Result := ResID;
end;

function MakeResourceURL( const ModuleName: string;
                          const ResName: PChar;
                          const ResType: PChar = nil): string; overload;
begin
  Assert(ModuleName <> '');
  Assert(Assigned(ResName));
  Result := 'res://' + TNetEncoding.URL.Encode(ModuleName);
  if Assigned(ResType) then
     Result := Result + './' + TNetEncoding.URL.Encode(FormatResNameOrType(ResType));
  Result := Result + '/' + TNetEncoding.URL.Encode(FormatResNameOrType(ResName));
end;

function MakeResourceURL( const Module: HMODULE;
                          const ResName: PChar;
                          const ResType: PChar = nil): string; overload;
begin
  Result := MakeResourceURL(GetModuleName(Module), ResName, ResType);
end;

function LoadResourceHTML( const RName: string; RType: PCHAR=Nil): String;
begin
  Result := MakeResourceURL(GetModuleName(HInstance), PCHAR(RName), RType);
end;

function LoadResourceString( const RName: string; RType: PCHAR = RT_RCDATA ): String;
var
  streamResource: TResourceStream;
  sList: TStringList;
begin
  Result := '';
  streamResource := TResourceStream.Create(
                      HInstance,
                      PCHAR(RName),
                      RType);
  try
    sList := TStringList.Create;
    sList.DefaultEncoding := TEncoding.UTF8;
    try
      streamResource.Position := 0;
      sList.Clear;
      sList.LoadFromStream(streamResource,TEncoding.UTF8);
      Result := sList.Text;
    finally
      sList.Destroy;
    end;
  finally
    streamResource.Destroy;
  end;
end;

Function FToStrSQL(ValueFloat: Extended; aDec: Integer=6): String;
Var st: String;
    P:  Byte;
Begin
  P:=aDec;
  St:=FormatFloat('0.'+StringOfChar('0',aDec),ValueFloat);
  While (P>2) And (St[Length(St)]='0') Do
   Begin
     Delete(St,Length(St),1);
     Dec(P);
   End;
  P:=Pos(',',St);
  If P>0 Then
     Begin
       Insert('.',St,P);
       Delete(St,Pos(',',St),1);
     End;
  Result:=St;
End;

Function FToStrSQL(const Value: String; aDec: Integer=6): String;
Begin
  Result:=FToStrSQL(StrToReal(Value),aDec);
End;

Function DateStr(Date: TDateTime=-1): String;
Begin
  if Date=-1 then
     Date:=Now();
  Result:=FormatDateTime(DATE_FORMAT,Date);
End;

Function DateTimeStr(Date: TDateTime=-1; Mode: TDateTimeMode = dtNone): String;
var
  fs: TFormatSettings;
begin
  if Date=-1 then
     Date:=Now();
  fs.TimeSeparator := ':';
  fs.DateSeparator := '-';
  case Mode of
   dtNone: ;
   dtNow:   Date:= Trunc(Date)+Frac(Now());
   dtBegin: Date:= Trunc(Date);
   dtEnd:   Date:= Trunc(Date)+Frac(EndOfTheDay(Date));
  end;
  Result:=FormatDateTime(DATE_TIME_FORMAT,Date,fs);
End;

function TrimS(s: String): String;
Begin
  while Not S.IsEmpty And (S[1]=' ') do
   Delete(S,1,1);
  while Not S.IsEmpty And (S[Length(S)]=' ') do
   Delete(S,Length(S),1);
  Result:=S;
end;

Procedure SetFloat(FT: TDataSet; const FieldName: String; Value: Double); Overload;
Begin
  FT.FieldByName(FieldName).AsFloat:=Value;
End;

Procedure SetFloat(FT: TDataSet; const FieldName: String; const Value: String); Overload;
Begin
  SetFloat(FT,fieldName,StrToreal(Value));
End;

Procedure SetFloat(Var StSource: String; Index: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
Begin
  SetStr(StSource,Index,FloatToStr(Value),cDiv);
End;

Procedure SetFloat(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Begin
  SetFloat(StSource,Index,StrToReal(Value),cDiv);
End;

Procedure SetFloat(LB: TStrings; Index, N: Integer;  Value: Double; CDiv: Char=ChDiv); Overload;
Var
    S: String;
Begin
  If Index<0 Then
     Exit;
  S:=LB[Index];
  SetFloat(S,N,Value,CDiv);
  LB[Index]:=S;
End;

Procedure SetFloat(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
begin
  SetFloat(LB,Index,N,StrToReal(Value),CDiv);
end;

Procedure SetDate(FT: TDataSet; const FieldName: String; Value: TDateTime); Overload;
Begin
  Try
    FT.FieldByName(FieldName).AsDateTime:=Value;
  Except
    FT.FieldByName(FieldName).AsDateTime:=-1;
  End;
End;

Procedure SetDate(FT: TDataSet; const FieldName: String; Const Value: String); Overload;
begin
  SetDate(FT,FieldName,StrToDateTimeFmt(Value));
end;

Procedure SetDate(Var StSource: String; Index: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
Begin
  SetStr(StSource,Index,FormatDateTime(DATE_TIME_FORMAT,Value) ,cDiv);
End;

Procedure SetDate(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
begin
  SetDate(stSource,Index,StrTodateTimeFmt(Value),CDiv);
end;

Procedure SetDate(LB: TStrings; Index, N: Integer;  Value: TDateTime; CDiv: Char=ChDiv); Overload;
Var
    S: String;
Begin
  If Index<0 Then
     Exit;
  S:=LB[Index];
  SetDate(S,N,Value,CDiv);
  LB[Index]:=S;
End;

Procedure SetDate(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
begin
  Setdate(LB,Index,N,StrToDateTimeFmt(Value),CDiv);
end;

Procedure SetInt(FT: TDataSet; const FieldName: String; Value: Integer); Overload;
Begin
  Try
    FT.FieldByName(FieldName).AsInteger:=Value;
  Except
    FT.FieldByName(FieldName).AsInteger:=0;
  End;
End;

Procedure SetInt(FT: TDataSet; const FieldName: String; Const Value: String); Overload;
begin
  SetInt(FT,FieldName,StrToInteger(Value));
end;

Procedure SetInt(Var StSource: String; Index: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
Begin
  SetStr(StSource,Index,IntToStr(Value),cDiv);
End;

Procedure SetInt(Var StSource: String; Index: Integer; Const Value: String; CDiv: Char=ChDiv); Overload;
begin
  SetInt(StSource,Index,StrToInteger(Value),CDiv);
end;

Procedure SetInt(LB: TStrings; Index, N: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
Var
    S: String;
Begin
  If Index<0 Then
     Exit;
  S:=LB[Index];
  Setint(S,N,Value,cDiv);
  LB[Index]:=S;
End;

Procedure SetInt(LB: TStrings; Index, N: Integer; Const Value: String; CDiv: Char=ChDiv); Overload;
begin
  Setint(LB,Index,N,StrToInteger(Value),CDiv);
end;

Procedure SetStr(FT: TDataSet; const FieldName, Value: String); Overload;
Begin
  Try
    FT.FieldByName(FieldName).AsString:=Value;
  Except
    FT.FieldByName(FieldName).AsString:='';
  End;
End;

Procedure SetStr(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Var L,Len,
    X,P: Integer;
    StA: String;
    OkS: Boolean;
    StL: String;
Begin
  StL:=TrimS(AmpFilter(Value));
  L:=0;
  P:=1;
  OkS:=False;
  Repeat
    Inc(L);
    StA:=Copy(StSource,P,Length(StSource));
    X:=Length(StA);
    Len:=Pos(CDiv,StA)-1;
    if (Len<0) then
       Begin
         StSource:=StSource+CDiv;
         Len:=X;
       End;
    If (L=Index) Then
       Begin
         OkS:=True;
         Delete(StSource,P,Len);
         Insert(StL,StSource,P);
       End
    Else
       P:=P+Len+1;
  Until OkS;
End;

Procedure SetStr(LB: TStrings; Index, N: Integer; const St: String; CDiv: Char=ChDiv ); Overload;
Var
    S: String;
Begin
  If Index<0 Then
     Exit;
  S:=LB[Index];
  SetStr(S,N,St,CDiv);
  LB[Index]:=S;
End;

function GetBool(OJSON: TJSONObject; const FieldName: string): boolean; Overload;
Var
    Ok: Boolean;
begin
  ok:=false;
  try
    If oJSON.TryGetValue<boolean>(fieldname,ok) Then
       ;
  except
  end;
  Result:=ok;
end;

function GetBool(const sJSON: String; const FieldName: string): boolean; Overload;
var JSON: TJSONObject;
    Ok: Boolean;
begin
  Ok:=false;
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Ok:=GetBool(JSON,FieldName);
       FreeAndNil(JSON);
     end;
  result:=Ok;
end;

Function GetFloat(FT: TDataSet; const FieldName: String): Double; Overload;
Begin
  Result:=FT.FieldByName(FieldName).AsExtended;
End;

Function GetFloat(OJSON: TJSONObject; const fieldname: String; Default: Double=0): Double; Overload;
var aValue: Double;
begin
  aValue:=StrToReal(GetStr(OJSON,fieldName));
  Result:=ifThen( aValue=0, Default, aValue);
end;

Function GetFloat(const St: String; Index: Integer; CDiv: Char=ChDiv): Double; Overload;
Begin
  Result:=StrToReal(GetStr(St,Index,CDiv));
End;

Function GetFloat(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): Double; OverLoad;
Var St: String;
Begin
  Result:=0;
  If Index<0 Then
     Exit;
  St:=LB[Index];
  Result:=GetFloat(St,N,CDiv);
End;

Function GetFloat(const sJSON, fieldname: String; Default: Double=0): Double;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Result:=GetFloat(JSON,fieldName,Default);
       FreeAndNil(JSON);
     end
  else
     result:=0;
end;

Function GetFloat(Item: IXMLNode; const AttribName: String; Default: Double=0): Double; Overload;
var aValue: Double;
begin
  aValue:=StrToReal(varToStr(Item.Attributes[AttribName]));
  Result:=ifThen(aValue=0, Default, aValue);
End;

Function GetFloat(Item: IXMLNode; Default: Double=0): Double; Overload;
var aValue: Double;
begin
  aValue:=StrToReal(varToStr(Item.NodeValue));
  Result:=ifThen(aValue=0, Default, aValue);
End;

Function GetInt(OJSON: TJSONObject; const fieldname: String; Default: Integer=0): Integer; Overload;
Var sValue: String;
    aValue: Integer;
begin
  sValue:=GetStr(OJSON,fieldName);
  if Not IsNumeric(sValue) then
     if sValue.ToLower='true' then
        sValue:='1'
     else
        sValue:='0';
  aValue:=StrToInteger(sValue);
  Result:=ifThen(aValue=0, Default, aValue);
end;

Function GetInt(FT: TDataSet; const FieldName: String): Integer; Overload;
Begin
  Result:=StrToInteger(FT.FieldByName(FieldName).AsString);
End;

Function GetInt(const St: String; Index: Integer; CDiv: Char=ChDiv): Integer; Overload;
Begin
  Result:=StrToInteger(GetStr(St,Index,CDiv));
End;

Function GetInt(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): Integer; OverLoad;
Var St: String;
Begin
  Result:=0;
  If Index<0 Then
     Exit;
  St:=LB[Index];
  Result:=GetInt(St,N,cDiv);
End;

Function GetInt(const sJSON, fieldname: String; Default: Integer=0): Integer;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Result:=GetInt(JSON,fieldName,Default);
       FreeAndNil(JSON);
     end
  else
     result:=0;
end;

Function GetInt(Item: IXMLNode; const AttribName: String; Default: Integer=0): Integer; Overload;
var aValue: Integer;
Begin
  aValue:=StrToInteger(varToStr(Item.Attributes[AttribName]));
  Result:=ifThen(aValue=0,Default,aValue);
End;

Function GetInt(Item: IXMLNode; Default: Integer=0): Integer; Overload;
var aValue: Integer;
Begin
  aValue:=StrToInteger(varToStr(Item.NodeValue));
  Result:=ifThen(aValue=0,Default,aValue);
End;

Function GetDate(OJSON: TJSONObject; const fieldname: String; Default: TDateTime=0): TDateTime; Overload;
Begin
  Result:=StrToDateTimeFmt(GetStr(OJSON,fieldName));
End;

Function GetDate(FT: TDataSet; const FieldName: String): TDateTime; Overload;
Var D: TDateTime;
Begin
  D:=-1;
  if Not FT.FieldByName(FieldName).IsNull then
     D:=FT.FieldByName(FieldName).AsDateTime;
  Result:=D;
End;

Function GetDate(const St: String; Index: Integer; CDiv: Char=ChDiv ): TDateTime; Overload;
Begin
  Result:=StrToDateTimeFmt(GetStr(St,Index,CDiv));
End;

Function GetDate(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): TDateTime; OverLoad;
Var St: String;
Begin
  Result:=-1;
  If Index<0 Then
     Exit;
  St:=LB[Index];
  Result:=GetDate(St,N,CDiv);
End;

Function GetDate(const sJSON, fieldname: String; Default: TDateTime=0): TDateTime;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Result:=GetDate(JSON,fieldName);
       FreeAndNil(JSON);
     end
  else
     result:=-1;
end;

Function GetDate(Item: IXMLNode; const AttribName: String; Default: TDateTime=0): TDateTime; Overload;
Var St: String;
Begin
  St:=Trim(varToStrDef(Item.Attributes[AttribName],''));
  Result:=StrToDateTimeFmt(St);
End;

Function GetDate(Item: IXMLNode; Default: TDateTime=0): TDateTime; Overload;
Var St: String;
Begin
  St:=Trim(varToStrDef(Item.NodeValue,''));
  Result:=StrToDateTimeFmt(St);
End;

Function GetStr(OJSON: TJSONObject; const fieldname: String; Default: String=''): String;
Var St: String;
    JSONValue: TJSONValue;
begin
  St:=Default;
  try
    JSONValue:=oJSON.GetValue(FieldName);
    if (JSONValue is TJSONArray) or
       (JSONValue is TJSONObject) then
       St:=JSONValue.ToJSON // JSONValue.Format(JSONIndent)
    else
       If oJSON.TryGetValue<string>(fieldname,St) Then
          ;
  except
    St:=Default;
  end;
  Result:=St;
end;

Function GetStr(FT: TDataSet; const FieldName: String): String; Overload;
Begin
  Result:=Trim(FT.FieldByName(FieldName).AsString);
End;

Function GetStr(const St: String; Index: Integer; CDiv: String): String; Overload;
Var S,StL: String;
    L,P:  Integer;
Begin
  S:='';
  StL:=St;
  L:=0;
  While Not StL.IsEmpty Do
   Begin
     Inc(L);
     P:=Pos(CDiv,StL)-1;
     If P<0 Then
        P:=Length(StL);
     If L=Index Then
        Begin
          S:=Copy(StL,1,P);
          Break;
        End;
     Delete(StL,1,P+Length(CDiv));
   End;
  Result:=TrimS(S);
End;

Function GetStr(Const St: String; Index: Integer; CDiv: Char=ChDiv): String; Overload;
Begin
  result:=GetStr(St,Index,String(CDiv));
End;

Function GetStr(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): String; OverLoad;
Var St: String;
Begin
  Result:='';
  If Index<0 Then
     Exit;
  St:=LB[Index];
  Result:=GetStr(St,N,CDiv);
End;

Function  GetStr(const sJSON, fieldname: String; Default: String=''): String;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Result:=GetStr(JSON,fieldName);
       FreeAndNil(JSON);
     end
  else
     result:='';
end;

function GetStr(Item: IXMLNode): String; Overload;
Var St: String;
Begin
  St:='';
  if Not VarIsNull(Item) then
     St:=Trim(VarToStrDef(Item.NodeValue,''));
  Result:=St;
End;

Function GetStr(Item: IXMLNode; const AttribName: String): String; Overload;
Begin
  Result:='';
  if Not VarIsNull(Item) then
     Result:=Trim(VarToStrDef(Item.Attributes[AttribName],''));
End;

procedure SetJSON( JSON: TJSONObject;
                   const fields: array of string;
                   const values: array of const;
                   findExist: boolean=false);
Var I: Integer;
    S,field: String;
    Ok: Boolean;
begin
  if (JSON<>Nil) then
     for I := Low(fields) to High(fields) do
      begin
        field:=fields[I];
        Ok:=True;
        if findExist then
           Ok:=(JSON.FindValue(field)=nil);
        if Ok then
           begin
            JSON.RemovePair(field);
            case Values[I].VType of
             vtBoolean:
               JSON.AddPair(field, TJSONBool.Create(Values[I].VBoolean));
             vtObject:
               JSON.AddPair(field, (Values[I].VObject As TJSONValue));
             vtInt64:
               JSON.AddPair(field, Values[I].VInt64^);
             vtInteger:
               JSON.AddPair(field, Values[I].VInteger);
             vtExtended,
             vtCurrency:
               JSON.AddPair(field, Values[I].VExtended^);
             else
               if (Values[I].VType = vtVariant) then
                  JSON.AddPair(field, TJSONNull.Create)
               else
                  begin
                    S:=AssignVal(Values[I]).DeQuotedString;
                    if (S.StartsWith('{') or S.StartsWith('[')) then
                       JSON.AddPair(field,CreateTJSONValue(S))
                    else
                       JSON.AddPair(field,S);
                  end;
            end;
           end;
      end;
end;

procedure SetJSON( var sJSON: String;
                   const fields: array of string;
                   const values: array of const;
                   findExist: boolean=false);
var
   JSON: TJSONObject;
begin
  if sJSON='' then
     sJSON:='{}';
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       SetJSON(JSON,fields,values,findExist);
       sJSON:=JSON.ToJSON;
       JSON.Destroy;
     end;
end;

procedure SetJSON( var sJSON: PChar;
                   const fields: array of string;
                   const values: array of const;
                   findExist: boolean=false);
var
   JSON: TJSONObject;
begin
  if sJSON='' then
     sJSON:='{}';
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       SetJSON(JSON,fields,values,findExist);
       sJSON:=PChar(JSON.ToJSON);
       JSON.Destroy;
     end;
end;

procedure SetJSONValue( aJSON: TJSONObject;
                        const fields: array of string;
                        const Values: array Of const); overload;
begin
  SetJSON(aJSON,fields,values,true);
end;

procedure SetJSONValue( var aJSON: string;
                        const fields: array of string;
                        const Values: Array Of const); overload;
var
   JSON: TJsonObject;
begin
  if AJSON='' then
     AJSON:='{}';
  JSON:=CreateTJSONObject(aJSON);
  if JSON<>Nil then
     begin
       SetJSON(JSON,fields,values,true);
       aJSON:=JSON.ToJSON;
       JSON.Destroy;
     end;
end;

procedure SetJSONValue( AJSON: TJSONObject;
                        const AName: String;
                        BJSON: TJSONObject;
                        APath: String=''); overload;
var
  value: TJSONValue;
begin
  if APath='' then
     APath:=AName;
  if (AJSON.FindValue(AName)=Nil) then
     begin
       value:=BJSON.FindValue(APath);
       if (value<>nil) then
          AJSON.AddPair(APath,TJSONValue(value.Clone));
     end;
end;

procedure SetJSONValue( AJSON: TJSONObject;
                        const AName: string;
                        const BJSON: string;
                        APath: String=''); overload;
var
   JSON: TJSONObject;
begin
  if bJSON='' then
     exit;
  JSON:=CreateTJSONObject(BJSON);
  if JSON<>Nil then
     begin
       SetJSONValue(AJSON,AName,JSON,APath);
       JSON.Destroy;
     end;
end;

procedure SetJSONValue( var AJSON: String;
                        AName: String;
                        BJSON: TJSONObject;
                        APath: String=''); overload;
var
   JSON: TJSONObject;
begin
  if AJSON='' then
     AJSON:='{}';
  JSON:=CreateTJSONObject(AJSON);
  if JSON<>Nil then
     begin
       SetJSONValue(JSON,AName,BJSON,APath);
       AJSON:=JSON.ToJSON;
       JSON.Destroy;
     end;
end;

procedure SetJSONValue( var AJSON: String;
                        AName: String;
                        const BJSON: string;
                        APath: String=''); overload;
var
   JSON: TJSONObject;
begin
  if AJSON='' then
     AJSON:='{}';
  JSON:=CreateTJSONObject(AJSON);
  if JSON<>Nil then
     begin
       SetJSONValue(JSON,AName,BJSON,APath);
       AJSON:=JSON.ToJSON;
       JSON.Destroy;
     end;
end;

procedure SetJSONValue( aJSON, bJSON: TJSONObject;
                        const fields: array of string); overload;
var
   i: Integer;
   value: TJSONValue;
begin
  for I := Low(fields) to High(fields) do
   begin
     var field:=fields[I];
     if (AJSON.FindValue(field)=Nil) then
        begin
          value:=BJSON.FindValue(field);
          if (value<>nil) then
             AJSON.AddPair(field,TJSONValue(value.Clone));
        end;
   end;
end;

procedure SetJSONValue( aJSON: TJSONObject;
                        const bJSON: string;
                        const fields: array of string); overload;
var
   JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(bJSON);
  if JSON<>Nil then
     begin
       SetJSONValue(aJSON,JSON,fields);
       JSON.Destroy;
     end;
end;

procedure SetJSONValue( var aJSON: string;
                        const bJSON: string;
                        const fields: array of string); overload;
var
   tJSON,
   lJSON: TJSONObject;
begin
  if aJSON='' then
     aJSON:='{}';
  tJSON:=CreateTJSONObject(AJSON);
  if tJSON<>Nil then
     begin
       lJSON:=CreateTJSONObject(bJSON);
       if lJSON<>Nil then
          begin
            SetJSONValue(tJSON,lJSON,fields);
            aJSON:=tJSON.ToJSON;
            lJSON.Destroy;
          end;
       tJSON.Destroy;
     end;
end;

procedure SetJSONValue( var aJSON: string;
                        bJSON: TJSONObject;
                        const fields: array of string); overload;
var
   JSON: TJSONObject;
begin
  if AJSON='' then
     AJSON:='{}';
  JSON:=CreateTJSONObject(AJSON);
  if JSON<>Nil then
     begin
       SetJSONValue(JSON,BJSON,fields);
       AJSON:=JSON.ToJSON;
       JSON.Destroy;
     end;
end;


Procedure JSONRemove( var sJSON: String;
                      const fields: array of string);
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       for var I := Low(fields) to High(fields) do
         JSON.RemovePair(fields[I]);
       sJSON:=JSON.ToJSON;
       FreeAndNil(JSON);
     end;
end;

Procedure JSONRemove(var sJSON: String; const FieldName: String);
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       JSON.RemovePair(fieldName);
       sJSON:=JSON.ToJSON;
       FreeAndNil(JSON);
     end;
end;

Procedure JSONRemove( oJSON: TJSONObject;
                     const fields: array of string);
begin
  for var I := Low(fields) to High(fields) do
    oJSON.RemovePair(fields[I]);
end;

function LeftS(S: String; Len: Integer; Ch: Char=' '): String;
begin
  result:=Copy(S+StringOfChar(Ch,Len),1,Len);
end;

function IndexOfList( LT: TStrings;
                      const StTarget: String;
                      Partial: Boolean=false): Integer; overload;
Var I,P: Integer;
    Ok:  Boolean;
Begin
  P:=-1;
  for I:=0 To LT.Count-1 do
   Begin
     If Partial then
        Ok:=UpperCase(Copy(LT[I],1,Length(StTarget)))=UpperCase(StTarget)
     Else
        Ok:=UpperCase(LT[I])=UpperCase(StTarget);
     If Ok Then
        Begin
          P:=I;
          Break;
        End;
   End;
  Result:=P;
End;

Function IndexOfList( LB: TStrings;
                      const StTarget: String;
                      Column: Integer=1;
                      Partial: Boolean=false;
                      CDiv: Char=ChDiv): Integer;  overload;
Var L,
    Index:    Integer;
    St,
    StSource: String;
    Ok:       Boolean;
Begin
  L:=0;
  Index:=-1;
  While L<LB.Count Do
   Begin
     St:=LB[L];
     StSource:=GetStr(St,Column,CDiv);
     If Partial then
        Ok:=(Copy(UpperCase(StSource),1,Length(StTarget))=TrimS(UpperCase(stTarget)))
     Else
        Ok:=(UpperCase(StSource)=TrimS(UpperCase(stTarget)));
     If Ok Then
        Begin
          Index:=L;
          Break;
        End;
     Inc(L);
   End;
  Result:=Index;
End;

Function GetStringRangeItem( LB: TStrings;
                             Index, FromField, ToField: Integer;
                             CDiv: Char=ChDiv): String; Overload;
Var St,S: String;
    I:    Integer;
Begin
  Result:='';
  If Index<0 Then
     Exit;
  St:=LB[Index];
  S:='';
  For I:=FromField To ToField Do
    S:=S+GetStr(St,I,CDiv)+cDiv;
  Result:=TrimS(S);
End;

function IsInternetConnection: Boolean;
Var
    NetHTTPClient: TNetHTTPClient;
    NetHTTPReq: TNetHTTPRequest;
begin
  NetHTTPClient:=TNetHTTPClient.Create(Nil);
  NetHTTPClient.ProtocolVersion:=THTTPProtocolVersion.HTTP_2_0;  //?
  NetHTTPReq:=TNetHTTPRequest.Create(Nil);
  NetHTTPReq.Client := NetHTTPClient;
  try
    NetHTTPReq.Get('https://www.google.com');
  except
    on E: ENetException do begin
          Exit(False);
    end;
  end;
  NetHTTPClient.Destroy;
  NetHTTPReq.Destroy;
  Result := True;
end;

function NetHTTPReq( pMethod: TRESTRequestMethod;
                     const BaseURI, Path: String;
                     sJSON: String='';
                     LHeader: TNetHeaders=Nil
                    ): IHTTPResponse; overload;
Var
    nClient: TNetHTTPClient;
    nReq: TNetHTTPRequest;
    iResp: IHTTPResponse;
    sStream: TStringStream;
    sURL,sPath: String;
begin
  result:=Nil;
  if BaseURI.IsEmpty then
     exit;
  nClient:=TNetHTTPClient.Create(Nil);
  nClient.ProtocolVersion:=THTTPProtocolVersion.HTTP_2_0;  //?
  nClient.ContentType:='application/json';
  nClient.Accept:='*/*';
  nClient.AcceptEncoding:='gzip, deflate, br';
  nClient.AllowCookies:=True;

  nReq:=TNetHTTPRequest.Create(Nil);
  nReq.Client := nClient;

  sURL:=Trim(BaseURI);
  if (sURL[Length(sURL)]<>'/') then
     sURL:=sURL+'/';
  sPath:=Trim(Path);
  if Not sPath.IsEmpty And (sPath[1]='/') then
     sPath:=Copy(sPath,2,Length(sPath));
  sURL:=sURL+sPath;

  sStream:= Nil;
  if Not sJSON.IsEmpty then
     sStream:= TStringStream.Create(sJSON,TEncoding.UTF8);
  Try
    iResp:=Nil;
    case pMethod of
     rmPUT:    iResp:=nReq.Put(sURL,sStream,nil,LHeader);
     rmPOST:   iResp:=nReq.Post(sURL,sStream,nil,LHeader);
     rmGET:    iResp:=nReq.Get(sURL,nil,LHeader);
     rmDELETE: iResp:=nReq.Delete(sURL,nil,LHeader);
    end;
    result:=iResp;
  except
    on E: ENetException do begin
      if not (E is ENetHTTPClientException) then
         Exit;
    end;
  End;
  if Assigned(sStream) then
     FreeAndNil(sStream);
  FreeAndNil(nClient);
  FreeAndNil(nReq);
end;

function NetHTTPReq( pMethod: TRESTRequestMethod;
                     const BaseURI, Path: String;
                     JSON: TJSONValue;
                     LHeader: TNetHeaders=Nil
                    ): IHTTPResponse; Overload;
begin
  result:=NetHTTPReq( pMethod,BaseURI, Path,
                      JSON.ToJSON, //JSON.Format(JSONIndent),
                      LHeader);
end;

procedure SendEmail( const aFromAddresses,
                           aToAddresses: String;
                     const Subject,
                           Body: String);
var
  Msg: TIdMessage;
  SMTP: TIdSMTP;
begin
  SMTP := TIdSMTP.Create(nil);
  Msg := TIdMessage.Create(nil);
  try
    Msg.From.Text := aFromAddresses;
    Msg.Recipients.EMailAddresses := aToAddresses;
    Msg.Body.Text := Body;
    Msg.Subject := Subject;
    if not SMTP.Connected then
       SMTP.Connect;
    SMTP.Send(Msg);
    try
      SMTP.Disconnect(false);
    except
    end;
  finally
    Msg.Destroy;
    SMTP.Destroy;
  end;
end;

Procedure SendSSLMail( Connection: TStringList;
                       Recipients: TStringList;
                       CopyRecipients: TStringList=Nil;
                       AttachFiles: TStringList=Nil);
var
  Email: TIdMessage;
  SMTP: TIdSMTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  pAddress: TIdEmailAddressItem;
  ImageParts: TStringDynArray;
  I: Integer;
begin
  SMTP:=TIdSMTP.Create(Nil);
  Email:=TIdMessage.Create(nil);
  SSLHandler:=TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
  SSLHandler.port:=25;
  SSLHandler.Destination:=':25';
  SSLHandler.IPVersion:=Id_IPv4;
  SSLHandler.MaxLineAction:=maException;
  //SSLHandler.SSLOptions.Mode:=sslmUnassigned;
  SSLHandler.SSLOptions.Method:=sslvSSLv23;

  try
    SMTP.Host    :=Connection.Values['Host']; // 'smtp.gmail.com';
    SMTP.Port    :=Connection.Values['Port'].ToInteger; // 587;
    SMTP.Username:=Connection.Values['User']; // 'gjrivero@gmail.com';
    SMTP.Password:=Connection.Values['Password']; // 'I_d7492361.';
    SMTP.IOHandler:=SSLHandler;
(*
    0: utNoTLSSupport,
    1: utUseImplicitTLS, // ssl iohandler req, allways tls
    2: utUseRequireTLS, // ssl iohandler req, user command only accepted when in tls
    3: utUseExplicitTLS // < user can choose to use tls
*)
    SMTP.UseTLS := TIdUseTLS(Connection.Values['UseTLS'].ToInteger); // utUseExplicitTLS;

    Email.From.Address := Connection.Values['FromAddress'];
    Email.From.Name:= Connection.Values['FromName'];
    Email.Subject := Connection.Values['Subject'];
    Email.ContentType := 'multipart/related; type="text/html"';

    ImageParts:=Connection.Values['Images'].Split([';']);
    for I := 0 to Recipients.Count-1 do
     begin
       pAddress := Email.Recipients.Add;
       pAddress.Name := GetStr(Recipients[I],1,';');
       pAddress.Address := GetStr(Recipients[I],2,';');
     end;
    if Assigned(CopyRecipients) then
       for I := 0 to CopyRecipients.Count-1 do
        begin
          pAddress:=Email.CCList.Add;
          pAddress.Name := GetStr(CopyRecipients[I],1,';');
          pAddress.Address := GetStr(CopyRecipients[I],2,';');
        end;
   with TIdMessageBuilderHtml.Create do
    try
      PlainText.Text := Connection.Values['PlainText'];
      Html.Text := Connection.Values['Body'];
      if Assigned(ImageParts) then
         for I := Low(ImageParts) to High(ImageParts) do
           begin
             var sCID:=ImageParts[i];

             Delete(sCID,1,Length(ExtractFilePath(sCID)));
             HtmlFiles.Add(ImageParts[i],sCID);
           end;
      if Assigned(AttachFiles) then
        for I := 0 to AttachFiles.Count-1 do
          Attachments.Add(AttachFiles[I]);
      FillMessage(Email);
    finally
      Free;
    end;

    Try
      SMTP.Connect;
      SMTP.Send(Email);
      SMTP.Disconnect;
    Except

    End;
  finally
    SSLHandler.Destroy;
    Email.Destroy;
    SMTP.Destroy;
  end;
end;

function ValidateEmail(const emailAddress: string): Boolean;
var
  RegEx: TRegEx;
begin
  RegEx := TRegex.Create('^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]*[a-zA-Z0-9]+$');
  Result := RegEx.Match(emailAddress).Success;
end;

procedure SaveLogFile(const sMessage: String);
Var
  FLog: TextFile;
  sText: String;
begin
  //-------------------------------------------
  AssignFile(FLog, App_FileLogs);
  if Not FileExists(App_FileLogs) then
    Rewrite(FLog)
  else
    Append(FLog);
  //--------------------------------------------------
  sText:='date: '+DateTimeStr(Now())+' message: '+Trim(sMessage)+'';
  Writeln(FLog,sText);
  CloseFile(FLog);
end;

procedure GetApplicationPath(LocalPath: Boolean);
var
   lPath,
   lStationName: String;
begin
  lStationName := GetEnvironmentVariable('COMPUTERNAME');
  if App_Name='' then
     App_Name := ChangeFileExt(ExtractFileName(paramstr(0)), ''); // Ohne Endung
{$IF DEFINED (Linux) or DEFINED (MACOS)}
  lPath := IncludeTrailingPathDelimiter(GetHomePath) + '.config/' +
                   App_Name + PathDelim;
{$ENDIF}
{$IFDEF MSWINDOWS}
  lPath := IncludeTrailingPathDelimiter(GetHomePath)+App_Name+PathDelim;
{$ENDIF}
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
  lPath := TPath.GetDocumentsPath+PathDelim;
{$ENDIF}
  if LocalPath then
     begin
       lPath :=ExtractFilePath(paramstr(0));
     end;
  App_Path:=lPath;
  App_FileLogs:=lPath+'logs'+PathDelim+'logs.txt';
end;

initialization
  App_Path:='';
  App_Params:='';
  App_FileLogs:='';

  GetApplicationPath(false);
finalization

end.

