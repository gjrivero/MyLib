unit uLib.Auth;

interface

uses
    Web.HTTPApp,
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
    Datasnap.DSSession,
{$ENDIF}
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
     function ActionLogin( const sJSON: String): string; virtual;
     function ActionSignUp( const sJSON: String): string; virtual;
     function ActionDeveloper( const sJSON: String): string; virtual;
     function ActionVerify(Const uURL: String): string; virtual;
     function OtherActions( const uURL: String;
                                  action: Integer;
                            const method: string): string; virtual;
     function GetAction( Const uURL, mPath, sPath: String): Integer; virtual;
     procedure SetDataSession(var sJSON: String); virtual;
     procedure CommonAuth( const aMainPath, sJSON: string;
                           UserRoles: TStrings;
                           var aResp: String);
     procedure SetValue(Session: TDSSession; const Key, Value: String);
     constructor Create(Request: TWebRequest; var Credentials: String);
     destructor destroy; virtual;
   protected
     AUser,
     APassword,
     JSONBody: string;
     MethodIndex: Integer;
     AHeaders: TStringList;
     TokenParam: Boolean;
   private
     Method,
     Base_Url: String;
   public
   end;


implementation

Uses
    System.Hash,
    System.StrUtils,
    System.DateUtils,
    System.NetEncoding,
    System.Generics.Collections,

    IdHTTPHeaderInfo,
    IdHTTPWebBrokerBridge,

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

function TUserWebAuthenticate.OtherActions( const uURL: String;
                                            action: Integer;
                                            const method: string): string;
begin
  result:='{}';
end;

function TUserWebAuthenticate.GetAction( Const uURL, mPath, sPath: String): Integer;
begin
  result:=ACT_DEFAULT;
  // /api/main/login
  if sPath=METHOD_PING then
     result:=ACT_DIAGNOSTIC
  else
     if (GetStr(uURL,MethodIndex-1,'/')=mPath) then
        if sPath='login' then
           result:=ACT_LOGIN
        else
        if sPath='signup' then
           result:=ACT_SIGNUP
        else
        if sPath='verify' then
           result:=ACT_VERIFY;
end;

procedure TUserWebAuthenticate.SetDataSession(var sJSON: String);
begin
   {}
end;

procedure TUserWebAuthenticate.SetValue( Session: TDSSession;
                                         const Key, Value: String);
begin
  if Value.IsEmpty then
     exit;
  Session.PutData(Key,Value);
end;

function TUserWebAuthenticate.ActionSignUp( const sJSON: String): string;
begin
  result:='';
end;

function TUserWebAuthenticate.ActionVerify(Const uURL: String): string;
begin
  result:='';
end;

function TUserWebAuthenticate.ActionLogin( const sJSON: String): string;
begin
  result:='';
end;

function TUserWebAuthenticate.ActionDeveloper( const sJSON: String): string;
begin
  result:='';
end;

procedure TUserWebAuthenticate.CommonAuth( const aMainPath, sJSON: string;
                                           UserRoles: TStrings;
                                           var aResp: String);
var
   Session: TDSSession;
   aJSON,
   errorMsg: string;
   Action: Integer;
   Valid: Boolean;
begin
  errorMsg:='Invalid invocation!';
  Action:=GetAction(Base_Url.ToLower,aMainPath,Method);
  aResp:='';
  SetJSON(aResp,[SS_VALID],[false]);
  SetJSON(aJSON,[SS_VALID],[false]);
  if Not TokenParam And (action=ACT_DEFAULT) then
     Exit;
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
   ACT_VERIFY:
      aJSON:=ActionVerify(Base_Url);
   else
      if TokenParam then
         aJSON:=ActionLogin(sJSON)
      else
         aJSON:=OtherActions(Base_Url.ToLower,Action,Method);
  end;
  valid:=GetBool(aJSON,SS_VALID);
  if Valid then
     begin
       var sRole:=GetStr(aJSON,SS_ROLE);
       if sRole='' then
          sRole:='standard';
       UserRoles.Add(sRole);
       Session := TDSSessionManager.GetThreadSession;
       SetValue(Session,SS_SESSIONID, Session.SessionName);
       SetValue(Session,SS_USER, AUser);
       SetValue(Session,SS_ROLE, sRole);
       SetValue(Session,SS_DEVELOPID, GetStr(aJSON,SS_DEVELOPID));
       SetValue(Session,SS_APPID, GetStr(aJSON,SS_APPID));
       SetValue(Session,SS_APPNAME, GetStr(aJSON,SS_APPNAME));
       SetValue(Session,SS_CUSTID, GetStr(aJSON,SS_CUSTID)); // La tabla debe tener ID
       SetValue(Session,SS_LOGINID, GetStr(aJSON,SS_LOGINID)); // La tabla debe tener ID
       SetValue(Session,SS_FIRSTNAME, GetStr(aJSON,SS_FIRSTNAME));
       SetValue(Session,SS_LASTNAME, GetStr(aJSON,SS_LASTNAME));
       SetValue(Session,SS_EMAIL, GetStr(aJSON,SS_EMAIL));
       SetValue(Session,SS_PHONE, GetStr(aJSON,SS_PHONE));
       SetValue(Session,SS_BRANCH, GetStr(aJSON,SS_BRANCH));
       SetValue(Session,SS_TERMINAL, GetStr(aJSON,SS_TERMINAL));
       //------------------------------------------------
       SetDataSession(aJSON);
     end;
  aResp:=aJSON;
end;

constructor TUserWebAuthenticate.Create( Request: TWebRequest; var Credentials: String);
Var
   sToken,
   aName,
   aValue,
   aQuery,
   lMethod,
   Host,
   sHeader: String;
   I: Integer;
begin
  Host:=LowerCase(Request.Host);
  Base_Url:=Request.PathInfo;
  aQuery:=Request.Query;
  lMethod:=Request.Method;
  if ContainsText(Host.ToLower,'api') then
     // '/v1/otp/account/2'
     MethodIndex:=3
  else
     // '/api/v1/otp/account/2'
     MethodIndex:=4;
  MethodIndex:=MethodIndex+ord(Pos('/v1',Base_Url)>0);
  if ContainsText(Base_Url.ToLower,METHOD_PING) then
     Method:=METHOD_PING
  else
     Method:=GetStr(Base_Url.ToLower,MethodIndex,'/');
  AHeaders:=TStringList.Create;
  with TIdHTTPAppRequest(Request).GetRequestInfo Do
   for I:= 0 to RawHeaders.Count - 1 do
    begin
      sHeader:=RawHeaders[I];
      aName := GetStr(sHeader,1,':');
      aValue:= GetStr(sHeader,2,':');
      AHeaders.AddPair(aName,aValue );
    end;
  sToken:='';
  JSONBody:='';
  if (lMethod='POST') then
     begin
       JSONBody:= TIdHTTPAppRequest(Request).Content;
       if AUser.IsEmpty and
          APassword.IsEmpty and
          ContainsText(JSONBody,'user') then
          SetJSON( Credentials,
                   [ SS_USER, SS_PASSWORD],
                   [ GetStr(JSONBody,'user'),
                     GetStr(JSONBody,'password')]);
     end;
  AUser:=GetStr(Credentials,SS_USER);
  APassword:=GetStr(Credentials,SS_PASSWORD);
  TokenParam:=false;
  if AUser.IsEmpty And APassword.IsEmpty then
     begin
       if sToken.IsEmpty and (Pos('authtoken',aQuery.ToLower)>0) then
          begin
            sToken:=Copy(aQuery,Pos('authtoken',aQuery.ToLower),Length(aQuery));
            Var p:=Pos('&',sToken);
            if P=0 then
               P:=Length(sToken);
            sToken:=Copy(sToken,1,P);
            sToken:=GetStr(sToken,2,'=');
          end;
       if Not sToken.IsEmpty {and (lMethod='GET')} then
          begin
            sToken:=TNetEncoding.Base64.Decode(sToken);
            AUser:=GetStr(sToken,1,':');
            APassword:=GetStr(sToken,2,':');
            SetJSON(Credentials,[SS_USER],[AUser]);
            SetJSON(Credentials,[SS_PASSWORD],[APassword]);
            TokenParam:=True;
          end;
     end;
end;

destructor TUserWebAuthenticate.destroy;
begin
  AHeaders.Destroy;
  Inherited;
end;

end.

