unit uLib.Auth;

interface

uses
    Web.HTTPApp,
    System.JSON,
    System.SysUtils,
    System.classes;

Const
   METHOD_PING    = 'ping';
   ACT_DEFAULT    = -1;
   ACT_DIAGNOSTIC =  0;
   ACT_SIGNUP     =  1;
   ACT_LOGIN      =  2;
   ACT_VERIFY     =  3;

Type
   TUserWebAuthenticate = class
     function ActionLogin( const sJSON: String): string;  virtual;
     function ActionSignUp( const sJSON: String): string;  virtual;
     function OtherActions( action: Integer; const method: string): string; virtual;
     procedure GetAction(Const mPath, sPath: String; var action: Integer); virtual;
     procedure SetDataSession(var sJSON: String); virtual;
     procedure CommonAuth( const aMainPath, sJSON: string;
                           UserRoles: TStrings;
                           var aResp: String);

     constructor Create( Request: TWebRequest);
     destructor destroy; virtual;
   protected
     AUser,
     APassword,
     Host_Url,
     Base_Url,
     Method,
     JSONBody: string;
     AHeaders: TStringList;
     MethodIndex: Integer;
   private
   public
   end;


implementation

Uses
    System.Hash,
    System.StrUtils,
    System.Generics.Collections,

    IdHTTPHeaderInfo,
    IdCustomHTTPServer,
    IdHTTPWebBrokerBridge,
    Datasnap.DSSession,

    uLib.Base,
    uLib.Data,
    uLib.Common,
    uLib.Helpers
    ;


type
  TIdHTTPAppRequestHelper = class helper for TIdHTTPAppRequest
  public
    function GetRequestInfo: TIdEntityHeaderInfo;
  end;

function TIdHTTPAppRequestHelper.GetRequestInfo: TIdEntityHeaderInfo;
begin
  Result := FRequestInfo;
end;

function TUserWebAuthenticate.OtherActions( action: Integer; const method: string): string;
begin
  result:='{}';
end;

procedure TUserWebAuthenticate.GetAction(Const mPath, sPath: String; var action: Integer);
begin
  action:=ACT_DEFAULT;
  // /api/main/login
  if sPath=METHOD_PING then
     action:=ACT_DIAGNOSTIC
  else
     if (GetStr(Base_Url,MethodIndex-1,'/')=mPath) then
        if sPath='login' then
           action:=ACT_LOGIN
        else
           if sPath='signup' then
              action:=ACT_SIGNUP
           else
              if sPath='verify' then
                 action:=ACT_VERIFY;
end;

procedure TUserWebAuthenticate.SetDataSession(var sJSON: String);
begin
  {}
end;

function TUserWebAuthenticate.ActionSignUp( const sJSON: String): string;
begin
  result:='';
end;

function TUserWebAuthenticate.ActionLogin( const sJSON: String): string;
begin
  result:='';
end;

procedure TUserWebAuthenticate.CommonAuth( const aMainPath, sJSON: string;
                                           UserRoles: TStrings;
                                           var aResp: String);
Var
   aJSON,
   errorMsg: string;
   Action: Integer;
   Valid: Boolean;
begin
  errorMsg:='Invalid invocation!';
  GetAction(aMainPath,Method,Action);
  aResp:='';
  SetJSON(aResp,[SS_VALID],[false]);
  SetJSON(aJSON,[SS_VALID],[false]);
  if action=ACT_DEFAULT then
     Exit;
  //---------------------------------
  AUser:=GetStr(sJSON,SS_USER);
  APassword:=GetStr(sJSON,SS_PASSWORD);
  //---------------------------------
  case Action of
   ACT_DIAGNOSTIC:
     aJSON:='{"valid":true}';
   ACT_SIGNUP,
   ACT_LOGIN:
     Case Action Of
       ACT_SIGNUP:
          aJSON:=ActionSignUp(sJSON);
       ACT_LOGIN:
          aJSON:=ActionLogin(sJSON);
     End;
   else
     aJSON:=OtherActions(Action,Method);
  end;
  valid:=GetBool(aJSON,SS_VALID);
  if Valid then
     begin
       var sRole:=GetStr(aJSON,SS_ROLE);
       if sRole='' then
          sRole:='standard';
       UserRoles.Add(sRole);
       SetDataSession(aJSON);
     end;
  aResp:=aJSON;
end;

constructor TUserWebAuthenticate.Create( Request: TWebRequest);
Var
   aName,
   aValue,
   sHeader: String;
   I: Integer;
begin
  Host_Url:=LowerCase(Request.Host);
  Base_Url:=LowerCase(Request.PathInfo);
  if ContainsText(Host_Url,'api') then
     // '/otp/account/2'
     MethodIndex:=3
  else
     // '/api/otp/account/2'
     MethodIndex:=4;
  if ContainsText(Base_Url,METHOD_PING) then
     Method:=METHOD_PING
  else
     Method:=LowerCase(GetStr(Base_Url,MethodIndex,'/'));

  AHeaders:=TStringList.Create;
  with TIdHTTPAppRequest(Request).GetRequestInfo Do
   for I:= 0 to RawHeaders.Count - 1 do
    begin
      sHeader:=RawHeaders[I];
      aName := GetStr(sHeader,1,':');
      aValue:= GetStr(sHeader,2,':');
      AHeaders.AddPair(aName,aValue );
    end;
  JSONBody:= TIdHTTPAppRequest(Request).Content;
end;

destructor TUserWebAuthenticate.destroy;
begin
  AHeaders.Destroy;
  Inherited;
end;

end.

