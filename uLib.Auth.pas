unit uLib.Auth;

interface

uses
    Web.HTTPApp,
    System.JSON,
    System.SysUtils,
    System.classes;

Const
   ACT_DEFAULT    = 0;
   ACT_SIGNUP     = 1;
   ACT_LOGIN      = 2;
   ACT_VALIDATE   = 3;

Type
   TUserWebAuthenticate = class
     function ActionLogin( const sJSON: String): string;  virtual;
     function ActionSignUp( const sJSON: String): string;  virtual;
     function OtherActions( action: Integer; const method: string): string; virtual;
     procedure GetAction(Const mPath, sPath: String; var action: Integer); virtual;
     procedure SetDataSession(const sJSON: String); virtual;
     procedure CommonAuth( const aMainPath, sJSON: string;
                           UserRoles: TStrings;
                           var aResp: String);

     constructor Create( Request: TWebRequest;
                         const devKey: string='');
   protected
     AUser,
     APassword,
     Apikey,
     Host_Url,
     Base_Url,
     Method,
     JSONBody,
     DEV_KEY_NAME: string;
     MethodIndex: Integer;
   private
   public
   end;


implementation

Uses
    System.Hash,
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
  if (GetStr(Base_Url,MethodIndex-1,'/')=mPath) then
     if sPath='login' then
        action:=ACT_LOGIN
     else
        if sPath='signup' then
           action:=ACT_SIGNUP
     else
        if sPath='validate' then
           action:=ACT_VALIDATE;
end;

procedure TUserWebAuthenticate.SetDataSession(const sJSON: String);
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

constructor TUserWebAuthenticate.Create( Request: TWebRequest;
                                         const devKey: string='');
Var
   aHeaders: TStringList;
   aName,
   aValue,
   sHeader: String;
   I: Integer;
   TL: TstringList;
begin
  Host_Url:=LowerCase(Request.Host);
  Base_Url:=LowerCase(Request.PathInfo);
  if Host_Url.Contains('api') then
     // '/otp/account/2'
     MethodIndex:=3
  else
     // '/api/otp/account/2'
     MethodIndex:=4;
  Method:=LowerCase(GetStr(Base_Url,MethodIndex,'/'));
  DEV_KEY_NAME:=devKey;
  aHeaders:=TStringList.Create;
  Try
   with TIdHTTPAppRequest(Request).GetRequestInfo Do
    for I:= 0 to RawHeaders.Count - 1 do
      begin
        sHeader:=RawHeaders[I];
        aName := GetStr(sHeader,1,':');
        aValue:= GetStr(sHeader,2,':');
        aHeaders.AddPair(aName,aValue );
      end;
    APIKey:=aHeaders.Values[DEV_KEY_NAME];
    JSONBody:= TIdHTTPAppRequest(Request).Content;
  Finally
    aHeaders.Destroy;
  End;
end;

end.

