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
   ,REST.Types
   ,FireDAC.Stan.Param;


Const
   CHDIV  = #1;
   DATE_FORMAT = 'YYYY-MM-DD';
   TIME_FORMAT = 'hh:nn:ss';
   DATE_TIME_FORMAT = 'yyyy-mm-dd hh:nn:ss';
   REGEX_EMAIL =  '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-\p{Cyrillic}]'+
               '+\.[a-zA-Z0-9-.\p{Cyrillic}]*[a-zA-Z0-9\p{Cyrillic}]+$';

Type
   TRandAttributes= (rdAllChar, rdAlpha, rdNumber, rdEspecial );
   TDateTimeMode = (dtNone, dtBegin, dtEnd);

var
   ApplicationName,
   ApplicationPath,
   ApplicationLogs: String;


function  JSONFilter(sJSON: String): String;
Function  FToStrSQL(Value: Double): String; overload;
Function  FToStrSQL(const Value: String): String; overload;
Function  StrToReal(St: String): Double;
Function  StrToInteger(St: String): Integer;
Function  DeleteLastChar(const S: String; ch: Char): String;
Function  DateStr(Date: TDateTime): String;
Function  DateTimeStr(Date: TDateTime; Mode: TDateTimeMode = dtNone): String;
function  StrToDateTimeFmt(sDateTime: String): TDateTime;
function  FormatDef(T: Double; DefDecimals: Integer=2): String; Overload;
function  FormatDef(S: String; DefDecimals: Integer=2): String; Overload;
Function  GoodReal(R: Double; defDec: Integer=2): Double; Overload;
Function  GoodReal(S: String; defDec: Integer=2): Double; Overload;
Function  DivCero(A,B: Double): Double;
function  LeftS(S: String; Len: Integer; Ch: Char=' '): String;

Function  GetFloat(OJSON: TJSONObject; const fieldname: String): Double; Overload;
Function  GetFloat(FT: TDataSet; const FieldName: String): Double; OverLoad;
Function  GetFloat(const St: String; Index: Integer; CDiv: Char=ChDiv): Double; OverLoad;
Function  GetFloat(const sJSON, fieldname: String): Double; Overload;

Function  GetInt(OJSON: TJSONObject; const fieldname: String): Integer; Overload;
Function  GetInt(OJSON: TJSONObject; const fieldname: String; iDefault: Integer): Integer; Overload;
Function  GetInt(FT: TDataSet; const FieldName: String): Integer; Overload;
Function  GetInt(const St: String; Index: Integer; CDiv: Char=ChDiv): Integer; Overload;
Function  GetInt(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): Integer; OverLoad;
Function  GetInt(const sJSON, fieldname: String): Integer; Overload;

Function  GetStr(OJSON: TJSONObject; const FieldName: String): String; Overload;
Function  GetStr(FT: TDataSet; const FieldName: String): String; Overload;
Function  GetStr(const St: String; Index: Integer; CDiv: String): String; Overload;
Function  GetStr(const St: String; Index: Integer; CDiv: Char=ChDiv): String; Overload;
Function  GetStr(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): String; Overload;
Function  GetStr(const sJSON, fieldname: String): String; Overload;

Function  GetDate(OJSON: TJSONObject; const fieldname: String): TDateTime; Overload;
Function  GetDate(FT: TDataSet; const FieldName: String): TDateTime; Overload;
Function  GetDate(const St: String; Index: Integer; CDiv: Char=ChDiv): TDateTime; Overload;
Function  GetDate(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): TDateTime; Overload;
Function  GetDate(const sJSON, fieldname: String): TDateTime; Overload;
function  GetBool(OJSON: TJSONObject; const FieldName: string): boolean; Overload;
function  GetBool(const sJSON: String; const FieldName: string): boolean; Overload;

function  GetApplicationPath(LocalPath: Boolean): String;

Procedure JSONRemove( var sJSON: String; const fields: array of string); overload;
Procedure JSONRemove( var sJSON: String; const FieldName: String); overload;
Procedure JSONRemove( oJSON: TJSONObject; const fields: array of string); overload;

Procedure SetStr(FT: TDataSet; const FieldName, Value: String); Overload;
Procedure SetStr(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetStr(LB: TStrings; Index, N: Integer; const St: String; CDiv: Char=ChDiv ); Overload;

Procedure SetInt(FT: TDataSet; const FieldName: String; Value: Integer); Overload;
Procedure SetInt(FT: TDataSet; const FieldName: String; Const Value: String); Overload;
Procedure SetInt(Var StSource: String; Index: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
Procedure SetInt(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetInt(LB: TStrings; Index, N: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
Procedure SetInt(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;

Procedure SetFloat(FT: TDataSet; const FieldName: String; Value: Double); Overload;
Procedure SetFloat(FT: TDataSet; const FieldName: String; const Value: String); Overload;
Procedure SetFloat(Var StSource: String; Index: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(LB: TStrings; Index, N: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;

Procedure SetDate(FT: TDataSet; const FieldName: String; Value: TDateTime); Overload;
Procedure SetDate(FT: TDataSet; const FieldName: String; const Value: String); Overload;
Procedure SetDate(Var StSource: String; Index: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
Procedure SetDate(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetDate(LB: TStrings; Index, N: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
Procedure SetDate(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;

procedure SetFlds( var sLine: String;
                   const fields: array of Integer;
                   const values: array of const;
                   pChDiv: Char=CHDIV);

procedure SetJSON( Var sJSON: String;
                   const fields: array of string;
                   const values: array of const); overload;
procedure SetJSON( JSON: TJSONObject;
                   const fields: array of string;
                   const values: array of const); overload;

procedure SaveLogFile(const sMessage: String);

Function  GetMaxFields(const fields: String; Cdiv: Char=ChDiv): Integer;
Function  SetDefaultLine(MaxFields: Integer; Cdiv: Char=ChDiv): String;

Procedure DiscountAmount( Var BolPor:     String;
                     Var Porcentaje,
                         Descont:    Double;
                         TotalF:     Double;
                         CheckP:     Boolean=false);

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

function IndexOfList( LB:       TStrings;
                      const StTarget: String;
                      Column:   Integer=1;
                      Partial:  Boolean=false;
                      CDiv: Char=ChDiv): Integer; overload;

function TrimS(s: String): String;
function AmpFilter(sVal: String): String;
function AssignVal(const AVarRec: TVarRec): String; Overload;
function AssignVal(const AVarRec: Variant): String; Overload;
function IsNumeric(const AString: string): Boolean;
function VersionStr(nuVerMayor, nuVerMenor, nuVerRelease: Integer): String;
function LoadResourceString( const RName: PCHAR; RType: PCHAR = RT_RCDATA ): String;
function LoadResourceHTML( const RName: PCHAR; RType: PCHAR = RT_RCDATA ): String;
function StringToHex(const S: String): String;
function HexToString(const S: String): String;
function StreamToString(Stream: TStream; Encoding: TEncoding=nil): string;
function CountOccurrences(SubStr, S: string): Integer;

procedure Compressfile(const filename: String);
procedure DecompressFile(const filename: String);

function CreateTJSONValue(sJSON: String; aEncoding: TEncoding = nil): TJSONValue;
function CreateTJSONObject(sJSON: String; aEncoding: TEncoding = nil): TJSONObject;
function CreateTJSONArray(sJSON: String; aEncoding: TEncoding = nil): TJSONArray;
function JSONArrayToObject(aJSON: TJSONValue; Index: Integer=0): TJSONObject; overload;
function JSONArrayToObject(aJSON: String; Index: Integer=0): String; overload;

function SetJSONResponse( iStatus: Integer;
                          const sMessage: string): TJSONObject; overload;
function SetJSONResponse( iStatus: Integer;
                          const sMessage: string;
                          aJSON: String): TJSONObject; overload;
function SetJSONResponse( iStatus: Integer;
                          const sMessage: string;
                          aJSON: TJSONValue): TJSONObject; overload;

procedure GetFieldsValues( JSON: TJSONObject;
                           var sFields, sValues, sSetVal, sCondition: string;
                           update: boolean);  overload;
procedure GetFieldsValues( sJSON: String;
                           var sFields, sValues, sSetVal, sCondition: string;
                           update: boolean);   overload;
procedure GetFieldsValues( pParams: TFDParams;
                           var sFields, sValues, sSetVal: string); overload;
function RandString( ALength: Integer;
                     attr: TRandAttributes=rdAllChar): String;

procedure saveTofile(const pFileName: String;
                    const SourceText, CipherKey, IV: RawByteString;
                          Crypted: Boolean);

function loadFromfile(const pFileName: String;
                      const CipherKey, IV: RawByteString;
                            Crypted: Boolean): RawByteString;

function ValidateEmail(const emailAddress: string): Boolean;
function IsInternetConnection: Boolean;
procedure SendSSLMail( Connection: TStringList;
                       Recipients: TStringList;
                       CopyRecipients: TStringList=Nil;
                       AttachFiles: TStringList=Nil);

implementation

uses
    System.IOUtils
   ,System.Zlib
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

function CountOccurrences(SubStr, S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = SubStr then
      Inc(Result);
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

function IsNumeric(const AString: string): Boolean;
var
  LCode: Integer;
  LVoidI: Int64;
  LVoidR: Double;
begin
  Val(AString, LVoidI, LCode);
  if LCode<>0 then
     Val(AString, LVoidR, LCode);
  Result := LCode = 0;
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
  if sJSON.IsEmpty then
     sJSON:='{}';
  if aEncoding=nil then
     aEncoding:=TEncoding.UTF8;
  Result:= TJSONObject.ParseJSONValue(aEncoding.GetBytes(sJSON), 0) as TJSONObject;
End;

function CreateTJSONvalue(sJSON: String; aEncoding: TEncoding = nil): TJSONValue;
Begin
  if sJSON.IsEmpty then
     sJSON:='{}';
  if aEncoding=nil then
     aEncoding:=TEncoding.UTF8;
  Result:= TJSONObject.ParseJSONValue(aEncoding.GetBytes(sJSON), 0) as TJSONValue;
End;

function CreateTJSONArray(sJSON: String; aEncoding: TEncoding = nil): TJSONArray;
Begin
  if sJSON.IsEmpty then
     sJSON:='[]';
  if aEncoding=nil then
     aEncoding:=TEncoding.UTF8;
  Result:= TJSONObject.ParseJSONValue(aEncoding.GetBytes(sJSON), 0) as TJSONArray;
End;

function JSONArrayToObject(aJSON: TJSONValue; Index: Integer=0): TJSONObject;
Begin
  if Assigned(AJSON) Then
     begin
       If (AJSON Is TJSONArray) And (TJSONArray(AJSON).Count>0) then
          Result:=TJSONArray(AJSON)[Index] As TJSONObject
       else
          Result:=TJSONObject.Create;
     end
  else
     Result:=TJSONObject.Create;
End;

function JSONArrayToObject(aJSON: String; Index: Integer=0): String;
var JArray: TJSONArray;
Begin
  JArray:=CreateTJSONArray(aJSON);
  if Assigned(JArray) And (JArray.Count>0) then
     Result:=(JArray[Index] As TJSONObject).ToString
  else
     Result:='{}';
End;

procedure GetFieldsValues( JSON: TJSONObject;
                           var sFields, sValues, sSetVal, sCondition: string;
                           update: boolean);
Var L: integer;
    field,
    value: String;
begin
  sValues:='';
  sFields:='';
  sSetVal:='';
  sCondition:='';
  for L := 0 to JSON.Count-1 do
   begin
     field:=JSON.Pairs[L].JsonString.Value.ToLower;
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
     if update and (CompareText(field,'id')=0) then
        begin
          sCondition:=field+'='+value;
        end
     else
        begin
          sFields:=sFields+','+field;
          sValues:=sValues+','+value;
          sSetVal:=sSetVal+','+field+'='+Value;
        end;
   End;
  Delete(sValues,1,1);
  delete(sFields,1,1);
  delete(sSetVal,1,1);
  if update and sCondition.IsEmpty then
     begin
       sCondition:=getStr(sFields,1,',')+'='+GetStr(sValues,1,',');
     end;
end;

procedure GetFieldsValues( sJSON: String;
                           var sFields, sValues, sSetVal, sCondition: string;
                           update: boolean);
Var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       GetFieldsValues(JSON,sFields,sValues,sSetVal,sCondition,update);
       JSON.Destroy;
     end;
end;

procedure GetFieldsValues( pParams: TFDParams;
                           var sFields, sValues, sSetVal: string);
Var I: Integer;
begin
  sFields:='';
  sValues:='';
  sSetVal:='';
  for I := 0 to pParams.count-1 do
    begin
      sFields:=sFields+','+pParams[I].Name;
      sValues:=sValues+','+pParams[I].Value.AsString;
      sSetVal:=sSetVal+','+pParams[I].Name+'='+pParams[I].Value.AsString;
    end;
  System.Delete(sValues,1,1);
  System.delete(sFields,1,1);
  System.delete(sSetVal,1,1);
end;

procedure saveTofile(const pFileName: String;
                    const SourceText, CipherKey, IV: RawByteString;
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

function  FormatDef(T: Double; DefDecimals: Integer=2): String; Overload;
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
  //  Result:= FormatFloat('#,##0.00',R)
  Result:=FormatFloat(sFmt,T);
end;

function  FormatDef(S: String; DefDecimals: Integer=2): String; Overload;
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

Function StrToReal(St: String): Double;
Var
  S: String;
  Valor: Double;
  lastCh,
  locMilSep,
  locDecSep: Char;
  I,
  Commas,
  Points:    Integer;
Begin
  St:=Trim(St);
  If St='' Then
     St:='0';
  S:='';
  Points:=0;
  Commas:=0;
  lastCh:=#0;
  For I:=1 To Length(St) Do
   If CharInSet(St[I],['0'..'9','.',',','-','+','E']) Then
      Case St[I] Of
      '0'..'9',
      '-','+','E': S:=S+St[I];
        '.':    Begin
                  S:=S+St[I];
                  LastCh:='.';
                  Inc(Points);
                End;
        ',':    Begin
                  S:=S+St[I];
                  LastCh:=',';
                  Inc(Commas);
                End;
      End;
  LocDecSep:='.';
  LocMilSep:=',';
  If ((Points>Commas) And (Commas>0)) Or (LastCh=',') Then
     Begin
       LocDecSep:=',';
       LocMilSep:='.';
     End;
  If (Commas>Points) And (Points=0) Then
     Begin
       I:=1;
       While I<Commas Do
        Begin
          Delete(S,Pos(',',S),1);
          Inc(I);
        End;
     End;
  If (Points>Commas) And (Commas=0) Then
     Begin
       I:=1;
       While I<Points Do
        Begin
          Delete(S,Pos('.',S),1);
          Inc(I);
        End;
     End;
  While Pos(LocMilSep,S)>0 Do
   Delete(S,Pos(LocMilSep,S),1);
  If (FormatSettings.DecimalSeparator<>LocDecSep) Then
     S:=ReplaceStr(S,LocDecSep,FormatSettings.DecimalSeparator);
  if S='' then
     S:='0';
  Try
    Valor:=StrToFloatDef(S,0);
  Except
    Valor:=0;
  End;
  Result:=Valor;
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

Procedure DiscountAmount( Var BolPor:     String;
                     Var Porcentaje,
                         Descont:    Double;
                         TotalF:     Double;
                         CheckP:     Boolean=false);
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

Function  GetMaxFields(const fields: String; Cdiv: Char=ChDiv): Integer;
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

procedure  SetFlds( var sLine: String;
                   const fields: array of Integer;
                   const values: array of const;
                   pChDiv: Char=CHDIV);
Var I: Integer;
begin
  for I := Low(fields) to High(fields) do
   SetStr(sLine,fields[I], AssignVal(Values[I]).DeQuotedString, pChDiv);
end;

procedure SetJSON( JSON: TJSONObject;
                    const fields: array of string;
                    const values: array of const);
Var I: Integer;
    S: String;
begin
  if (JSON<>Nil) then
     for I := Low(fields) to High(fields) do
      begin
        JSON.RemovePair(fields[I]);
        case Values[I].VType of
         vtBoolean:
           JSON.AddPair(fields[I], TJSONBool.Create(Values[I].VBoolean));
         vtObject:
           JSON.AddPair(fields[I], (Values[I].VObject As TJSONValue));
         vtInt64:
           JSON.AddPair(fields[I], Values[I].VInt64^);
         vtInteger:
           JSON.AddPair(fields[I], Values[I].VInteger);
         vtExtended,
         vtCurrency:
           JSON.AddPair(fields[I], Values[I].VExtended^);
         else
           if (Values[I].VType = vtVariant) then
              JSON.AddPair(fields[I], TJSONNull.Create)
           else
              begin
                S:=AssignVal(Values[I]).DeQuotedString;
                if (S.StartsWith('{') or S.StartsWith('[')) then
                   JSON.AddPair(fields[I],CreateTJSONValue(S))
                else
                   JSON.AddPair(fields[I],TJSONString.Create(S));
              end;
        end;
      end;
end;

procedure SetJSON( var sJSON: String;
                    const fields: array of string;
                    const values: array of const);
var
   JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       SetJSON(JSON,fields,values);
       sJSON:=JSON.ToString;
       JSON.Destroy;
     end;
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

Function DeleteLastChar(const S: String; ch: Char): String;
Var St: String;
Begin
  St:=S;
  While Not St.IsEmpty And (St[Length(St)]=Ch) Do
   Delete(St,Length(St),1);
  Result:=St;
End;

{
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
INDEX_PAGE  23     "HTML\index.html"   Asi tambien funciona
INDEX_PAGE  HTML   "HTML\index.html"
PAGE_1      HTML   "HTML\page1.html"
PAGE_2      HTML   "HTML\page2.html"
LOGO_GIF    HTML   "HTML\logo.gif"
STYLE_CSS   HTML   "HTML\style.css"

/* RCDATA resource */
42          RCDATA "HTML\rcdata.html"

*)

function URLEncode(const S: string): string;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    {$IFDEF UNICODE}
    if CharInSet(S[Idx], ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.']) then
    {$ELSE}
    if S[Idx] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.'] then
    {$ENDIF}
      Result := Result + S[Idx]
    else
      Result := Result + '%' + IntToHex(Ord(S[Idx]), 2);
  end;
end;

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
  Result := 'res://' + URLEncode(ModuleName);
  if Assigned(ResType) then
     Result := Result + './' + URLEncode(FormatResNameOrType(ResType));
  Result := Result + '/' + URLEncode(FormatResNameOrType(ResName));
end;

function LoadResourceHTML( const RName: PCHAR; RType: PCHAR = RT_RCDATA ): String;
begin
  Result := MakeResourceURL(GetModuleName(HInstance), RName, RType);
end;

function LoadResourceString( const RName: PCHAR; RType: PCHAR = RT_RCDATA ): String;
var
  streamResource: TResourceStream;
  sList: TStringList;
begin
  Result := '';
  streamResource := TResourceStream.Create(
                      HInstance,
                      RName,
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


Function FToStrSQL(Value: Double): String;
Var st: String;
    P:  Byte;
Begin
  P:=7;
  St:=FormatFloat('0.0000000',Value);
  While (P>1) And (St[Length(St)]='0') Do
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

Function FToStrSQL(const Value: String): String;
Begin
  Result:=FToStrSQL(StrToReal(Value));
End;

Function DateStr(Date: TDateTime): String;
Begin
  Result:=FormatDateTime(DATE_FORMAT,Date);
End;

Function DateTimeStr(Date: TDateTime; Mode: TDateTimeMode = dtNone): String;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  fs.DateSeparator := '-';
  case Mode of
   dtNone: ;
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

Procedure JSONRemove( var sJSON: String;
                      const fields: array of string);
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       for var I := Low(fields) to High(fields) do
         JSON.RemovePair(fields[I]);
       sJSON:=JSON.ToString;
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
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
     end;
end;

Procedure JSONRemove( oJSON: TJSONObject;
                     const fields: array of string);
begin
  if oJSON<>Nil then
     for var I := Low(fields) to High(fields) do
        oJSON.RemovePair(fields[I]);
end;

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

Function GetFloat(OJSON: TJSONObject; const fieldname: String): Double; Overload;
begin
  Result:=StrToReal(GetStr(OJSON,fieldName));
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

Function GetFloat(const sJSON, fieldname: String): Double;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Result:=GetFloat(JSON,fieldName);
       FreeAndNil(JSON);
     end
  else
     result:=0;
end;

Function GetInt(OJSON: TJSONObject; const fieldname: String): Integer; Overload;
Var sValue: String;
begin
  sValue:=GetStr(OJSON,fieldName);
  if Not IsNumeric(sValue) then
     if sValue.ToLower='true' then
        sValue:='1'
     else
        sValue:='0';
  Result:=StrToInteger(sValue);
end;

Function GetInt(OJSON: TJSONObject; const fieldname: String; iDefault: Integer): Integer; Overload;
begin
  if GetInt(OJSON,fieldname)=0 then
     result:=iDefault
  else
     result:=GetInt(OJSON,fieldname);
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

Function GetInt(const sJSON, fieldname: String): Integer;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       Result:=GetInt(JSON,fieldName);
       FreeAndNil(JSON);
     end
  else
     result:=0;
end;

Function GetDate(OJSON: TJSONObject; const fieldname: String): TDateTime; Overload;
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

Function GetDate(const sJSON, fieldname: String): TDateTime;
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

Function GetStr(OJSON: TJSONObject; const fieldname: String): String;
Var St: String;
    JSONValue: TJSONValue;
begin
  St:='';
  try
    JSONValue:=oJSON.GetValue(FieldName);
    if (JSONValue is TJSONArray) or
       (JSONValue is TJSONObject) then
       St:=JSONValue.ToString
    else
       If oJSON.TryGetValue<string>(fieldname,St) Then
          ;
  except
    St:='';
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

Function  GetStr(const sJSON, fieldname: String): String;
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

function LeftS(S: String; Len: Integer; Ch: Char=' '): String;
begin
  result:=Copy(S+StringOfChar(Ch,Len),1,Len);
end;


Function IndexOfList( LB:       TStrings;
                      const StTarget: String;
                      Column:   Integer=1;
                      Partial:  Boolean=false;
                      CDiv:     Char=ChDiv): Integer;
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

Function GetStringRangeItem(LB: TStrings; Index, FromField, ToField: Integer; CDiv: Char=ChDiv): String; Overload;
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
    sStream: TStringStream;
    sURL,sPath: String;
begin
  result:=Nil;
  if BaseURI.IsEmpty then
     exit;
  nClient:=TNetHTTPClient.Create(Nil);
  nClient.Accept := '*/*';
  nClient.ContentType := 'application/json';
  nClient.AcceptCharSet := 'utf-8, *;q=0.8';
  nClient.AllowCookies := true;
  nClient.ProtocolVersion:=THTTPProtocolVersion.HTTP_2_0;  //?

  nReq:=TNetHTTPRequest.Create(Nil);
  nReq.Client := nClient;
  nReq.Accept := '*/*';
  nReq.AcceptCharSet := 'utf-8, *;q=0.8';

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
    case pMethod of
     rmPUT:    Result:=nReq.Put(sURL,sStream,nil,LHeader);
     rmPOST:   Result:=nReq.Post(sURL,sStream,nil,LHeader);
     rmGET:    Result:=nReq.Get(sURL,nil,LHeader);
     rmDELETE: Result:=nReq.Delete(sURL,nil,LHeader);
    end;
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
  result:=NetHTTPReq(pMethod,BaseURI, Path,JSON.ToString,LHeader);
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
    SMTP.UseTLS := TIdUseTLS(Connection.Values['UseTLS'].ToInteger); // utUseExplicitTLS;

    Email.From.Address := Connection.Values['FromAddress'];;
    Email.From.Name:= Connection.Values['FromName'];
    Email.Subject := Connection.Values['Subject'];
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
  sJSON: String;
begin
  //-------------------------------------------
  AssignFile(FLog, ApplicationLogs);
  if Not FileExists(ApplicationLogs) then
    Rewrite(FLog)
  else
    Append(FLog);
  //--------------------------------------------------
  sJSON:='{"date":"'+DateTimeStr(Now())+'"';
  sJSON:=sJSON+',"message":"'+trim(ReplaceText(sMessage,#13#10,' '))+'"}';
  Writeln(FLog,sJSON);
  CloseFile(FLog);
end;

function GetApplicationPath(LocalPath: Boolean): String;
var
   lPath,
   AppName,
   AppStationName: String;
begin
  AppStationName := GetEnvironmentVariable('COMPUTERNAME');
  AppName := ChangeFileExt(ExtractFileName(paramstr(0)), ''); // Ohne Endung
{$IF DEFINED (Linux) or DEFINED (MACOS)}
  lPath := IncludeTrailingPathDelimiter(GetHomePath) + '.config/' +
                   AppName + PathDelim;
{$ENDIF}
{$IFDEF MSWINDOWS}
  lPath := IncludeTrailingPathDelimiter(GetHomePath)+AppName+PathDelim;
{$ENDIF}
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
  lPath := TPath.GetDocumentsPath+PathDelim;
{$ENDIF}
  if LocalPath then
     begin
       lPath :=ExtractFilePath(paramstr(0));
     end;
  result:=lPath;
end;

initialization
  ApplicationPath:=GetApplicationPath(false);
  ApplicationLogs:=ApplicationPath+PathDelim+'logs'+PathDelim;
finalization

end.
