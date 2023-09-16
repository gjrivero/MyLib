unit uLib.Crypt;

interface

uses
  IdHMAC,
  DECCipherBase,
  REST.Types,
  System.Classes,
  System.Types,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections;

type
  TIdHMACClass = class of TIdHMAC;


function EqualHashString(const Source, Target: String): boolean;
function EncryptRequest(Const sRequest: RawByteString;
                              CipherKey, IV: TBytes;
                              cmMode: TCipherMode;
                              aEncoding: Boolean=false): RawByteString;
function DeCryptRequest(Const sRequest: RawByteString;
                              CipherKey, IV: TBytes;
                              cmMode: TCipherMode;
                              aEncoding: Boolean=false): RawByteString;
function Encrypt(const SourceText, CipherKey, IV: RawByteString): RawByteString;
function DeCrypt(const SourceText, CipherKey, IV: RawByteString): RawByteString;
procedure EncryptFile( const fileSource, fileTarget: String;
                       const CipherKey, IV: RawByteString);
procedure DeCryptFile( const fileSource, fileTarget: String;
                       const CipherKey, IV: RawByteString);

function PBKDF2( const Password: RawByteString;
                 const Salt: TBytes;
                 const IterationsCount: Integer;
                 const KeyLengthInBytes: Integer;
                 PRFC: TIdHMACClass = nil): TBytes;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

Uses
    System.Hash,
    System.Math,
    System.UITypes,
    System.StrUtils,
    System.Variants,
    System.NetEncoding,

    uLib.Base,
    DECTypes,
    DECCiphers,
    DECCipherModes,

    IdGlobal,
    IdHMACSHA1,
    IdCoderMIME,

    WinApi.Windows;


function EqualHashString(const Source, Target: String): boolean;
var
  StringHash: THashSHA2;
  sHash: String;
begin
  // SHA224, SHA256, SHA384, SHA512, SHA512_224, SHA512_256
  StringHash := THashSHA2.Create(SHA256); // Default = SHA256

  sHash:=StringHash.GetHashString(Source);
  result:=SameText(sHash,Target);
end;

function Encrypt(const SourceText, CipherKey, IV: RawByteString): RawByteString;
var
  Cipher: TCipher_AES128;
  Input,
  Output: TBytes;
begin
  Cipher := TCipher_AES128.Create;
  try
    Cipher.Init(CipherKey, IV);
    Cipher.Mode := cmCBCx;
    Input := System.SysUtils.BytesOf(SourceText);
    Output := Cipher.EncodeBytes(Input);
    result := TNetEncoding.Base64.EncodeBytesToString(OutPut);
    Cipher.Done;
  finally
    Cipher.Free;
  end;
end;

function DeCrypt(const SourceText, CipherKey, IV: RawByteString): RawByteString;
var
  Cipher: TCipher_AES128;
  Input,
  Output: TBytes;
begin
  Cipher := TCipher_AES128.Create;
  try
    Cipher.Init(CipherKey, IV);
    Cipher.Mode := cmCBCx;
    Input:=TNetEncoding.Base64.DecodeStringToBytes(SourceText);
    Output := Cipher.DecodeBytes(Input);
    result:= System.SysUtils.StringOf(Output);

    Cipher.Done;
  finally
    Cipher.Free;
  end;
end;


procedure EncryptFile( const fileSource, fileTarget: String;
                       const CipherKey, IV: RawByteString);
var
  Cipher: TCipher_AES128;
  Input: TBytes;
  i: Integer;
begin
  Cipher := TCipher_AES128.Create;
  try
    Cipher.Init(CipherKey, IV);
    Cipher.Mode := cmCBCx;
    // Encrypt
    Cipher.EncodeFile( fileSource, fileTarget);
    // clean up inside the cipher instance, which also removes the key from RAM
    Cipher.Done;
  finally
    Cipher.Free;
  end;
end;

procedure DeCryptFile( const fileSource, fileTarget: String;
                       const CipherKey, IV: RawByteString);
var
  Cipher: TCipher_AES128;
begin
  Cipher := TCipher_AES128.Create;
  try
      Cipher.Init(CipherKey, IV);
      Cipher.Mode := cmCBCx;
      Cipher.DecodeFile(fileSource, fileTarget);
      // clean up inside the cipher instance, which also removes the key from RAM
      Cipher.Done;
  finally
    Cipher.Free;
  end;
end;


function EncryptRequest(Const sRequest: RawByteString;
                              CipherKey, IV: TBytes;
                              cmMode: TCipherMode;
                              aEncoding: Boolean=false): RawByteString;
Const
   BLOCK_SIZE = 128;
   BSIZE = (BLOCK_SIZE div 8);
var
  aReq,
  Output: TBytes;
  I,l,Pad: Integer;
begin
  //-----------------------------
  result:=sRequest;
  if Trim(sRequest)='' then
     exit;
  if aEncoding then
     begin
       SetLength(aReq,length(sRequest)*2);
       FillChar(aReq[0],length(sRequest)*2,0);
       l:=0;
       for I := 1 to length(sRequest) do
        begin
          aReq[l]:=byte(sRequest[i]);
          inc(l,2);
        end;
     end
  else
     begin
       SetLength(aReq,length(sRequest));
       Move(sRequest[1],aReq[0],length(sRequest));
     end;
  Pad := BSIZE - (Length(aReq) mod BSIZE);
  for i := 1 to Pad do
    aReq := aReq+[Pad];
  //-----------------------------
  Output := [];
  var Cipher:=TCipher_AES.Create;
  try
    try
      Cipher.Mode := cmMode;
      Cipher.Init(CipherKey, IV);
      Output := Cipher.Encodebytes(aReq);
      Cipher.Done;
    except
      on E: Exception do
         Raise Exception.Create(E.ClassName+ ': '+ E.Message);
    end;
  finally
    Cipher.Free;
  end;
  result:= TNetEncoding.Base64.EncodeBytesToString(OutPut);
end;

function DeCryptRequest(Const sRequest: RawByteString;
                              CipherKey, IV: TBytes;
                              cmMode: TCipherMode;
                              aEncoding: Boolean=false): RawByteString;
var
  aReq,
  Output: TBytes;
  sHash: RawByteString;
  I,L,
  Pad,T: Integer;
begin
  result:=sRequest;
  if Trim(sRequest)='' then
     exit;
  //-----------------------------
  aReq:=TNetEncoding.Base64.DecodeStringToBytes(sRequest);
  //-----------------------------
  Output := [];
  var Cipher:=TCipher_AES.Create;
  try
    try
      Cipher.Mode := cmMode;
      Cipher.Init(CipherKey, IV);
      Output := Cipher.DecodeBytes(aReq);
      T:=Length(Output);
      pad:=Output[T-1];
      while (Output[T-1]=pad) do
       begin
         Dec(T);
         SetLength(Output,T);
       end;
      if aEncoding then
         begin
           L:=0;
           SetLength(aReq,T div 2);
           for I := 0 to T-1 do
             if (I mod 2)=0 then
             begin
               aReq[L]:=OutPut[i];
               inc(L);
             end;
           OutPut:=aReq;
         end;
      Cipher.Done;
    except
      on E: Exception do
         Raise Exception.Create(E.ClassName+ ': '+ E.Message);
    end;
  finally
    Cipher.Free;
  end;
  result:= StringOf(OutPut);
end;

function PBKDF2( const Password: RawByteString;
                 const Salt: TBytes;
                 const IterationsCount: Integer;
                 const KeyLengthInBytes: Integer;
                 PRFC: TIdHMACClass = nil): TBytes;
var
  PRF: TIdHMAC;
  D: Integer;
  I: Int32;
  F: TIdBytes;
  U: TIdBytes;
  J: Integer;
  T: TIdBytes;
  lPassword, lSalt: TIdBytes;

  function _ConcatenateBytes(const _B1: TIdBytes; const _B2: TIdBytes): TIdBytes; inline;
  begin
    SetLength(Result, Length(_B1) + Length(_B2));
    if Length(_B1) > 0 then
      Move(_B1[low(_B1)], Result[low(Result)], Length(_B1));
    if Length(_B2) > 0 then
      Move(_B2[low(_B2)], Result[low(Result) + Length(_B1)], Length(_B2));
  end;

  function _INT_32_BE(const _I: Int32): TIdBytes; inline;
  begin
    Result := TIdBytes.Create(_I shr 24, _I shr 16, _I shr 8, _I);
  end;

  procedure _XorBytes(var _B1: TIdBytes; const _B2: TIdBytes); inline;
  var
    _I: Integer;
  begin
    for _I := low(_B1) to high(_B1) do
      _B1[_I] := _B1[_I] xor _B2[_I];
  end;

begin
  if not Assigned(PRFC) then
    PRFC := TIdHMACSHA1;
  PRF := PRFC.Create;
  try
    {
      Conversion TBytes -> TidBytes as Remy Lebeau says
      https://stackoverflow.com/a/18854367/6825479
    }
    SetLength(lPassword,Length(Password));
    move(Password[1],lPassword[0],length(Password));
    lSalt := TIdBytes(Salt);

    D := Ceil(KeyLengthInBytes / PRF.HashSize);
    PRF.Key := lPassword;
    for I := 1 to D do
    begin
      F := PRF.HashValue(_ConcatenateBytes(lSalt, _INT_32_BE(I)));
      U := Copy(F);
      for J := 2 to IterationsCount do
      begin
        U := PRF.HashValue(U);
        _XorBytes(F, U);
      end;
      T := _ConcatenateBytes(T, F);
    end;
    Result := TBytes(Copy(T, low(T), KeyLengthInBytes));
  finally
    PRF.Free;
  end;
end;

end.


