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
   ,Xml.XMLIntf
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
Function  GetFloat(Item: IXMLNode): Double; Overload;
Function  GetFloat(Item: IXMLNode; const AttribName: String): Double; Overload;
Function  GetFloat(const sJSON, fieldname: String): Double; Overload;

Function  GetInt(OJSON: TJSONObject; const fieldname: String): Integer; Overload;
Function  GetInt(FT: TDataSet; const FieldName: String): Integer; Overload;
Function  GetInt(const St: String; Index: Integer; CDiv: Char=ChDiv): Integer; Overload;
Function  GetInt(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): Integer; OverLoad;
Function  GetInt(Item: IXMLNode; const AttribName: String): Integer; Overload;
Function  GetInt(Item: IXMLNode): Integer; Overload;
Function  GetInt(const sJSON, fieldname: String): Integer; Overload;

Function  GetStr(OJSON: TJSONObject; const FieldName: String): String; Overload;
Function  GetStr(FT: TDataSet; const FieldName: String): String; Overload;
Function  GetStr(const St: String; Index: Integer; CDiv: String): String; Overload;
Function  GetStr(const St: String; Index: Integer; CDiv: Char=ChDiv): String; Overload;
Function  GetStr(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): String; Overload;
Function  GetStr(Item: IXMLNode; const AttribName: String): String; Overload;
Function  GetStr(Item: IXMLNode): String; Overload;
Function  GetStr(const sJSON, fieldname: String): String; Overload;

Function  GetDate(OJSON: TJSONObject; const fieldname: String): TDateTime; Overload;
Function  GetDate(FT: TDataSet; const FieldName: String): TDateTime; Overload;
Function  GetDate(const St: String; Index: Integer; CDiv: Char=ChDiv): TDateTime; Overload;
Function  GetDate(LB: TStrings; Index, N: Integer; CDiv: Char=ChDiv): TDateTime; Overload;
Function  GetDate(Item: IXMLNode; const AttribName: String): TDateTime; Overload;
Function  GetDate(Item: IXMLNode): TDateTime; Overload;
Function  GetDate(const sJSON, fieldname: String): TDateTime; Overload;

Procedure SetStr(FT: TDataSet; const FieldName, Value: String); Overload;
Procedure SetStr(OJSON: TJSONObject; const FieldName, Value: String); Overload;
Procedure SetStr(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetStr(LB: TStrings; Index, N: Integer; const St: String; CDiv: Char=ChDiv ); Overload;
Procedure SetStr(var sJSON: String; const FieldName, Value: String); Overload;
Procedure SetStr(var sJSON: String; const FieldName: String; Value: TJSONValue); Overload;

Procedure JSONRemove(var sJSON: String; const FieldName: String);
Procedure SetJSON(JSON: TJSONObject; const FieldName: String; Value: TJSONValue); Overload;
Procedure SetJSON(JSON: TJSONObject; const FieldName, Value: String); Overload;
Procedure SetJSON(var sJSON: String; const FieldName, Value: String); Overload;

Procedure SetInt(FT: TDataSet; const FieldName: String; Value: Integer); Overload;
Procedure SetInt(FT: TDataSet; const FieldName: String; Const Value: String); Overload;
Procedure SetInt(OJSON: TJSONObject; const FieldName: String; Value: Integer); Overload;
Procedure SetInt(OJSON: TJSONObject; const FieldName: String; Const Value: String); Overload;
Procedure SetInt(Var StSource: String; Index: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
Procedure SetInt(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetInt(LB: TStrings; Index, N: Integer; Value: Integer; CDiv: Char=ChDiv); Overload;
Procedure SetInt(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetInt(var sJSON: String; const FieldName, Value: String); Overload;
Procedure SetInt(var sJSON: String; const FieldName: String; Value: Integer); Overload;

Procedure SetFloat(FT: TDataSet; const FieldName: String; Value: Double); Overload;
Procedure SetFloat(FT: TDataSet; const FieldName: String; const Value: String); Overload;
Procedure SetFloat(OJSON: TJSONObject; const FieldName: String; Value: Double); Overload;
Procedure SetFloat(OJSON: TJSONObject; const FieldName: String; const Value: String); Overload;
Procedure SetFloat(Var StSource: String; Index: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(LB: TStrings; Index, N: Integer; Value: Double; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetFloat(var sJSON: String; const FieldName, Value: String); Overload;
Procedure SetFloat(var sJSON: String; const FieldName: String; Value: Double); Overload;

Procedure SetDate(FT: TDataSet; const FieldName: String; Value: TDateTime); Overload;
Procedure SetDate(FT: TDataSet; const FieldName: String; const Value: String); Overload;
Procedure SetDate(OJSON: TJSONObject; const FieldName: String; Value: TDateTime); Overload;
Procedure SetDate(OJSON: TJSONObject; const FieldName: String; const Value: String); Overload;
Procedure SetDate(Var StSource: String; Index: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
Procedure SetDate(Var StSource: String; Index: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetDate(LB: TStrings; Index, N: Integer; Value: TDateTime; CDiv: Char=ChDiv); Overload;
Procedure SetDate(LB: TStrings; Index, N: Integer; const Value: String; CDiv: Char=ChDiv); Overload;
Procedure SetDate(var sJSON: String; const FieldName, Value: String); Overload;
Procedure SetDate(var sJSON: String; const FieldName: String; Value: TDateTime); Overload;

procedure SetFlds( var sLine: String;
                   const fields: array of Integer;
                   const values: array of const;
                   pChDiv: Char=CHDIV);
procedure SetFldsJSON( var sJSON: String;
                   const fields: array of string;
                   const values: array of const);
Function  GetMaxFields(const fields: String; Cdiv: Char=ChDiv): Integer;
Function  SetDefaultLine(MaxFields: Integer; Cdiv: Char=ChDiv): String;

Procedure Descuento( Var BolPor:     String;
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

procedure Compressfile(const filename: String);
procedure DecompressFile(const filename: String);

function CreateTJSONValue(sJSON: String): TJSONValue;
function CreateTJSONObject(sJSON: String): TJSONObject;
function CreateTJSONArray(sJSON: String): TJSONArray;
function JSONArrayToObject(aJSON: TJSONArray; Index: Integer=0): TJSONObject;

procedure GetFieldsValues( JSON: TJSONObject;
                           var sFields, sValues, sSetVal: string); overload;
procedure GetFieldsValues( sJSON: String;
                           var sFields, sValues, sSetVal: string); overload;
procedure GetFieldsValues( pParams: TFDParams;
                           var sFields, sValues, sSetVal: string); overload;
function RandString( ALength: Integer;
                     attr: TRandAttributes=rdAllChar): String;

function saveTextfile(const pFileName: String;
                            JSON: TJSONObject;
                            Crypted: Boolean): Integer; Overload;

function saveTextfile(const pFileName: String;
                            sTEXT: String;
                            Crypted: Boolean): Integer; Overload;
function loadTextfile(const pFileName: String;
                            Crypted: Boolean): String;
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
   ,IdExplicitTLSClientServerBase;



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
  if (Length(S.DeQuotedString)>2) and (S.DeQuotedString[1]='`') then
     begin
       S:=S.DeQuotedString;
       Delete(S,1,1);
       Delete(S,Pos('`',S),1);
     end;
  Result:=S;
end;

function AssignVal(const AVarRec: TVarRec): String; Overload;
var Str: String;
begin
  Str:='';
  case AVarRec.VType of
   vtBoolean: Str:=Ord(AVarRec.VBoolean).ToString;
   vtInt64: Str:=AVarRec.VInt64^.ToString;
   vtInteger: Str:=AVarRec.VInteger.ToString;
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

//-------------------------------------------------//
//  JSON: Functions                                //
//-------------------------------------------------//

function CreateTJSONObject(sJSON: String): TJSONObject;
Begin
  if sJSON.IsEmpty then
     sJSON:='{}';
  Result:= TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(sJSON), 0) as TJSONObject;
End;

function CreateTJSONvalue(sJSON: String): TJSONValue;
Begin
  if sJSON.IsEmpty then
     sJSON:='{}';
  Result:= TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(sJSON), 0) as TJSONValue;
End;

function CreateTJSONArray(sJSON: String): TJSONArray;
Begin
  if sJSON.IsEmpty then
     sJSON:='[]';
  Result:= TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(sJSON), 0) as TJSONArray;
End;

function JSONArrayToObject(aJSON: TJSONArray; Index: Integer=0): TJSONObject;
Begin
  if AJSON<>Nil then
     Result:=aJSON[Index] As TJSONObject
  else
     Result:=TJSONObject.Create;
End;

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
     field:=JSON.Pairs[L].JsonString.Value;
     Value:=JSON.Pairs[L].JsonValue.ToString.Replace('"','''');
     sFields:=sFields+','+field;
     sValues:=sValues+','+value;
     sSetVal:=sSetVal+','+field+'='+Value;
   End;
  System.Delete(sValues,1,1);
  System.delete(sFields,1,1);
  System.delete(sSetVal,1,1);
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

function saveTextfile(const pFileName: String;
                             sTEXT: String;
                             Crypted: Boolean): Integer;
var
  lst: TStringList;
begin
  lst:=TStringList.create;
  try
    if Crypted then
       begin
         sTEXT:=sTEXT;
       end;
    lst.text:=sTEXT; // Aqu� se inserta #$D#$A
    lst.Savetofile(pfileName,TEncoding.UTF8);
    Result:=0;   //DB_SUCCESSFUL;
  except
    Result:=-5; // DB_WRITE_FILE_ERROR;
  end;
  lst.Destroy;
end;

function saveTextfile(const pFileName: String;
                             JSON: TJSONObject;
                             Crypted: Boolean): Integer; Overload;
begin
  result:=saveTextfile(pFileName,JSON.ToString,Crypted);
end;

function loadTextfile(const pFileName: String;
                            Crypted: Boolean): String;
var
  sTEXT: String;
  lst: TStringList;
begin
  lst:=TStringList.create;
  try
    lst.LoadFromFile(pfileName,TEncoding.UTF8);
    sTEXT:=ReplaceStr(lst.TEXT,#$D#$A,''); // Quitar primero #$D#$A
    if Crypted then
       begin
         sTEXT:=sTEXT;
       end;
    result:=sTEXT;
  finally
    lst.Destroy;
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

Procedure Descuento( Var BolPor:     String;
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

Function  getMaxFields(const fields: String; Cdiv: Char=ChDiv): Integer;
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

procedure SetFldsJSON( var sJSON: String;
                   const fields: array of string;
                   const values: array of const);
Var I: Integer;
    JSON: TJSONObject;
    S: String;
begin
  if sJSON='' then
     sJSON:='{}';
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       for I := Low(fields) to High(fields) do
        begin
          JSON.RemovePair(fields[I]);
          S:=AssignVal(Values[I]);
          if IsNumeric(S) then
             begin
               If Pos('.',S)=0 Then
                  JSON.AddPair( fields[I],TJSONNumber.Create(S.ToInteger))
               else
                  JSON.AddPair( fields[I],TJSONNumber.Create(S.ToDouble));
             end
          else
             JSON.AddPair(fields[I],TJSONString.Create(S.DeQuotedString));
        end;
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
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

Procedure SetFloat(OJSON: TJSONObject; const FieldName: String; Value: Double); Overload;
begin
  OJSON.RemovePair(fieldName);
  OJSON.AddPair(fieldName,TJSONNumber.Create(Value));
end;

Procedure SetFloat(OJSON: TJSONObject; const FieldName: String; const Value: String); Overload;
begin
  OJSON.RemovePair(fieldName);
  OJSON.AddPair(fieldName,TJSONNumber.Create(StrToReal(Value)));
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

Procedure SetFloat(var sJSON: String; const FieldName: String; Value: Double); Overload;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);;
  if JSON<>Nil then
     begin
       SetFloat(JSON,fieldName,Value);
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
     end;
end;

Procedure SetFloat(var sJSON: String; const FieldName, Value: String); Overload;
begin
  SetFloat(sJSON,fieldName,StrToReal(Value));
end;

Procedure SetDate(OJSON: TJSONObject; const FieldName: String; Value: TDateTime); Overload;
begin
  OJSON.RemovePair(fieldName);
  OJSON.AddPair(fieldName,TJSONString.Create(DateTimeStr(Value)));
end;

Procedure SetDate(OJSON: TJSONObject; const FieldName: String; Const Value: String); Overload;
begin
  SetDate(OJSON, fieldName, StrToDateTimeFmt(Value));
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

Procedure SetDate(var sJSON: String; const FieldName: String; Value: TDateTime); Overload;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       SetDate(JSON,fieldName,Value);
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
     end;
end;

Procedure SetDate(var sJSON: String; const FieldName, Value: String); Overload;
begin
  SetDate(sJSON,fieldName,StrToDateTimeFmt(Value));
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

Procedure SetInt(OJSON: TJSONObject; const FieldName: String; Value: Integer); Overload;
Begin
  OJSON.RemovePair(fieldName);
  OJSON.AddPair(fieldName,TJSONNumber.Create(Value));
End;

Procedure SetInt(OJSON: TJSONObject; const FieldName: String; Const Value: String); Overload;
begin
  SetInt(OJSON,fieldName,StrToInteger(Value));
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

Procedure SetInt(var sJSON: String; const FieldName: String; Value: Integer); Overload;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       SetInt(JSON,fieldName,Value);
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
     end;
end;

Procedure SetInt(var sJSON: String; const FieldName, Value: String); Overload;
begin
  SetInt(sJSON,fieldName,StrToInteger(Value));
end;

Procedure SetStr(FT: TDataSet; const FieldName, Value: String); Overload;
Begin
  Try
    FT.FieldByName(FieldName).AsString:=Value;
  Except
    FT.FieldByName(FieldName).AsString:='';
  End;
End;

Procedure SetStr(OJSON: TJSONObject; const FieldName, Value: String); Overload;
begin
  OJSON.RemovePair(fieldName);
  OJSON.AddPair(fieldName,TJSONString.Create(AmpFilter(Value)));
end;

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

Procedure SetStr(var sJSON: String; const FieldName, Value: String); Overload;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       SetStr(JSON,fieldName,Value);
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
     end;
end;

Procedure SetStr(var sJSON: String; const FieldName: String; Value: TJSONValue); Overload;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       JSON.RemovePair(fieldName);
       JSON.AddPair(fieldName,Value);
       sJSON:=JSON.ToString;
       FreeAndNil(JSON);
     end;
end;

Procedure SetJSON(JSON: TJSONObject;  const FieldName, Value: String); Overload;
begin
  JSON.RemovePair(fieldName);
  JSON.AddPair(fieldName,TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(Value), 0) as TJSONValue);
end;

Procedure SetJSON(JSON: TJSONObject;  const FieldName: String; Value: TJSONValue); Overload;
Var s: String;
begin
  JSON.RemovePair(fieldName);
  s:=Value.ToString;
  JSON.AddPair(fieldName,Value);
end;

Procedure SetJSON(var sJSON: String; const FieldName, Value: String); Overload;
var JSON: TJSONObject;
begin
  JSON:=CreateTJSONObject(sJSON);
  if JSON<>Nil then
     begin
       JSON.RemovePair(fieldName);
       JSON.AddPair(fieldName,TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(Value), 0) as TJSONValue);
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

Function GetFloat(FT: TDataSet; const FieldName: String): Double; Overload;
Begin
  Result:=FT.FieldByName(FieldName).AsExtended;
End;

Function GetFloat(OJSON: TJSONObject; const fieldname: String): Double; Overload;
begin
  Result:=StrToReal(GetStr(OJSON,fieldName));
end;

Function GetFloat(Item: IXMLNode; const AttribName: String): Double; Overload;
Begin
  Result:=StrToReal(varToStr(Item.Attributes[AttribName]));
End;

Function GetFloat(Item: IXMLNode): Double; Overload;
Begin
  Result:=StrToReal(varToStr(Item.NodeValue));
End;

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

Function GetInt(FT: TDataSet; const FieldName: String): Integer; Overload;
Begin
  Result:=StrToInteger(FT.FieldByName(FieldName).AsString);
End;

Function GetInt(Item: IXMLNode; const AttribName: String): Integer; Overload;
Begin
  Result:=StrToInteger(varToStr(Item.Attributes[AttribName]));
End;

Function GetInt(Item: IXMLNode): Integer; Overload;
Begin
  Result:=StrToInteger(varToStr(Item.NodeValue));
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

Function GetDate(Item: IXMLNode; const AttribName: String): TDateTime; Overload;
Var St: String;
Begin
  St:=Trim(varToStrDef(Item.Attributes[AttribName],''));
  Result:=StrToDateTimeFmt(St);
End;

Function GetDate(Item: IXMLNode): TDateTime; Overload;
Var St: String;
Begin
  St:=Trim(varToStrDef(Item.NodeValue,''));
  Result:=StrToDateTimeFmt(St);
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
       If oJSON.TryGetValue(fieldname,St) Then
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

Function GetStr(Item: IXMLNode; const AttribName: String): String; Overload;
Begin
  Result:='';
  if Not VarIsNull(Item) then
     Result:=Trim(VarToStrDef(Item.Attributes[AttribName],''));
End;

Function GetStr(Item: IXMLNode): String; Overload;
Var St: String;
Begin
  St:='';
  if Not VarIsNull(Item) then
     St:=Trim(VarToStrDef(Item.NodeValue,''));
  Result:=St;
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
     rmPUT:
        Result:=nReq.Put(sURL,sStream,nil,LHeader);
     rmPOST:
        Result:=nReq.Post(sURL,sStream,nil,LHeader);
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
  SSLHandler.SSLOptions.Mode:=sslmUnassigned;
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

end.