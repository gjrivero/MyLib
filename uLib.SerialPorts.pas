unit uLib.SerialPorts;

interface

uses
   System.UITypes
   ,System.Generics.Collections;

const
  _NUL  = #$00;
  _SOH  = #$01;
  _STX  = #$02;
  _ETX  = #$03;
  _EOT  = #$04;
  _ENQ  = #$05;
  _ACK  = #$06;
  _BEL  = #$07;
  _BS   = #$08;
  _TAB  = #$09;
  _LF   = #$0A;
  _VT   = #$0B;
  _FF   = #$0C;
  _CR   = #$0D;
  _SO   = #$0E;
  _SI   = #$0F;
  _DLE  = #$10;
  _DC1  = #$11;
  _DC2  = #$12;
  _DC3  = #$13;
  _DC4  = #$14;
  _NAK  = #$15;
  _SYN  = #$16;
  _ETB  = #$17;
  _CAN  = #$18;
  _EM   = #$19;
  _SUB  = #$1A;
  _ESC  = #$1B;
  _FS   = #$1C;
  _GS   = #$1D;
  _RS   = #$1E;
  _US   = #$1F;

Type
  TSetOfAnsiChar= Set Of AnsiChar;
  TBaudRate = (brCustom, br110, br300, br600, br1200, br2400,
    br4800, br9600, br14400, br19200, br38400, br56000, br57600,
    br115200, br128000, br256000);
  TStopBits = (sbOneStopBit, sbOne5StopBits, sbTwoStopBits);
  TDataBits = (dbFive, dbSix, dbSeven, dbEight);
  TParityBits = (prNone, prOdd, prEven, prMark, prSpace);


function ASCIITranslate(const sStr: string): string;
function OpenCOMPort( port: Word;
                      bauds: TBaudRate;
                      Parity: TParityBits;
                      DataBits: TDataBits;
                      stopbits: TStopBits): THandle;
function COMSendText( COMFile: THandle; S: AnsiString): Boolean;
function COMSendCmd( COMFile: THandle;
                     const stCmd: AnsiString;
                     const aOut: TSetOfAnsiChar=[_ACK,_NAK];
                     const aResp: AnsiChar=_NUL): String;
function COMSendBuffer( COMFile: THandle; var Buffer; count: Word): Boolean;
function COMReadText( COMFile: THandle): String;
procedure CloseCOMPort( COMFile: THandle);

Implementation

uses  System.SysUtils, Winapi.Windows, Winapi.Messages;

{$REGION COMMDEF}

// This code works for comports 1 thru 9, but not for ports 10 and higher.
// Instead, use code similar to the following
// CommPort := '\\.\COM33';


function OpenCOMPort(port: Word; bauds: TBaudRate;
  Parity: TParityBits; DataBits: TDataBits;
  stopbits: TStopBits): THandle;
const
  CParityBits: array [TParityBits] of Integer = (NOPARITY,
    ODDPARITY, EVENPARITY, MARKPARITY, SPACEPARITY);
  CStopBits: array [TStopBits] of Integer = (ONESTOPBIT,
    ONE5STOPBITS, TWOSTOPBITS);
  CBaudRate: array [TBaudRate] of Integer = (0, CBR_110,
    CBR_300, CBR_600, CBR_1200, CBR_2400, CBR_4800, CBR_9600,
    CBR_14400, CBR_19200, CBR_38400, CBR_56000, CBR_57600,
    CBR_115200, CBR_128000, CBR_256000);
  CDataBits: array [TDataBits] of Integer = (5, 6, 7, 8);
const
  RxBufferSize = 256;
  TxBufferSize = 256;
var
  DCB: TDCB;
  CommTimeouts: TCommTimeouts;
  OkPort: Boolean;
  portName: String;
  COMFile: THandle;
begin
  OkPort := True;
  portName:='COM' + IntToStr(port) + ':';
  if port>9 then
     portName:='\\.\'+portName;
  ComFile := CreateFile( PWideCHAR(portName),
                         GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL, 0);
  if ComFile = INVALID_HANDLE_VALUE then
  begin
    OkPort := False;
  end
  else
  Begin
    if not GetCommState(ComFile, DCB) Then
    Begin
      OkPort := False;
    End;
    DCB.DCBlength := SizeOf(TDCB);
    DCB.Parity := CParityBits[Parity];
    DCB.stopbits := CStopBits[stopbits];
    DCB.BaudRate := CBaudRate[bauds];
    DCB.ByteSize := CDataBits[DataBits];
    if not SetCommState(ComFile, DCB) Then
    Begin
      OkPort := False;
    End;
    if not SetupComm(ComFile, RxBufferSize, TxBufferSize) then
    Begin
      OkPort := False;
    End;
    with CommTimeouts do
    begin
      ReadIntervalTimeout := 0;
      ReadTotalTimeoutConstant := 100;
      ReadTotalTimeoutMultiplier := 0;
      WriteTotalTimeoutMultiplier := 0;
      WriteTotalTimeoutConstant := 1000;
    end;
    if not SetCommTimeouts(ComFile, CommTimeouts) then
    Begin
      OkPort := False;
    End;
  End;
  Result := COMFile;
end;

function COMSendText(COMFile: THandle; S: AnsiString): Boolean;
var
  BytesBuffer, BytesWritten: DWORD;
begin
  BytesWritten := 0;
  BytesBuffer := Length(S);
  Result := WriteFile(ComFile, S[1], BytesBuffer, BytesWritten, nil);
end;

function COMSendBuffer(COMFile: THandle; var Buffer; count: Word): Boolean;
Type
  tBuff = array [1 .. 1] of AnsiChar;
var
  BytesWritten: DWORD;
  mm: ^tBuff;
begin
  mm := @Buffer;
  Result := WriteFile(ComFile, mm^, count, BytesWritten, nil);
end;

function COMReadText(COMFile: THandle): String;
var
  D: Array [1 .. 80] of AnsiChar;
  S: String;
  BytesRead, I: DWORD;
begin
  Result := '';
  fillchar(D, SizeOf(D), 0);

  if not ReadFile(ComFile, D, SizeOf(D), BytesRead, nil) then
  begin
    { Raise an exception }
  end;
  S := '';
  for I := 1 to BytesRead do
    S := S + D[I];
  Result := S;
end;

procedure CloseCOMPort(COMFile: THandle);
begin
  CloseHandle(ComFile);
end;
{$ENDREGION COMMDEF}

function StringToHex(s: String): String;
var
  I: Integer;
  st: String;
begin
  St:='';
  for I:=1 to Length(S)do
   St:=St+IntToHex(ord(S[I]),2)+' ';
  Result:=st;
end;

function ASCIITranslate(const sStr: string):string;
var
   I: integer;
   S: String;
begin
  S:='';
  for I:=1 to length(sStr) do
    case sStr[I] of
      _NUL:  S:=S+'[NUL]';
      _SOH:  S:=S+'[SOH]';
      _STX:  S:=S+'[STX]';
      _ETX:  S:=S+'[ETX]';
      _EOT:  S:=S+'[EOT]';
      _ENQ:  S:=S+'[ENQ]';
      _ACK:  S:=S+'[ACK]';
      _BEL:  S:=S+'[BEL]';
      _BS:  S:=S+'[BS]';
      _TAB:  S:=S+'[TAB]';
      _LF:  S:=S+'[LF]';
      _VT:  S:=S+'[VT]';
      _FF:  S:=S+'[FF]';
      _CR:  S:=S+'[CR]';
      _SO:  S:=S+'[SO]';
      _SI:  S:=S+'[SI]';
      _DLE:  S:=S+'[DLE]';
      _DC1:  S:=S+'[DC1]';
      _DC2:  S:=S+'[DC2]';
      _DC3:  S:=S+'[DC3]';
      _DC4:  S:=S+'[DC4]';
      _NAK:  S:=S+'[NAK]';
      _SYN:  S:=S+'[SYN]';
      _ETB:  S:=S+'[ETB]';
      _CAN:  S:=S+'[CAN]';
      _EM:  S:=S+'[EM]';
      _SUB:  S:=S+'[SUB]';
      _ESC:  S:=S+'[ESC]';
      _FS:  S:=S+'[FS]';
      _GS:  S:=S+'[GS]';
      _RS:  S:=S+'[RS]';
      _US:  S:=S+'[US]';
      #127..#255: S:=S+'['+IntToHex(ord(sStr[I]),2)+']';
     else
         S:=S+sStr[I];
    end;
  Result:=S;
end;

function COMSendCmd( COMFile: THandle;
                     const stCmd: AnsiString;
                     const aOut: TSetOfAnsiChar=[_ACK,_NAK];
                     const aResp: AnsiChar=_NUL): String;
Var sResp: AnsiString;
    count: Integer;
    Ok: Boolean;
    C: AnsiChar;
begin
  sResp:='';
  count:=0;
  if COMSendText(COMFile,StCmd) Then
     if aOut<>[] then
     begin
       Ok:=False;
       Repeat
         sResp := sResp  + COMReadText(COMFile);
         Inc(count);
         //Sleep(5);
         for C In aOut do
          Ok:=(Pos(C, sResp)>0);
       Until Ok Or (Count>10);
       if Ok And (aResp<>_Nul) then
          COMSendCmd(COMfile,aResp,[]);
     end;
  Result:=sResp;
end;

end.

