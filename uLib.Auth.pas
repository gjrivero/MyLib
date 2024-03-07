unit uLib.Auth;

interface

uses
    Web.HTTPApp,
    Datasnap.DSSession,
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
     function OtherActions( const uURL: String;
                                  action: Integer;
                            const method: string): string; virtual;
     procedure GetAction( Const uURL, mPath, sPath: String;
                          var action: Integer); virtual;
     procedure SetDataSession(var sJSON: String); virtual;
     procedure CommonAuth( const aMainPath, sJSON: string;
                           UserRoles: TStrings;
                           var aResp: String);
     procedure SetValue(Session: TDSSession; const Key, Value: String);
     constructor Create( App_ID: Integer; Request: TWebRequest; var Credentials: String);
     destructor destroy; virtual;
   protected
     AUser,
     APassword,
     Method,
     JSONBody: string;
     TokenParam: Boolean;
     AHeaders: TStringList;
     AppID,
     MethodIndex: Integer;
   private
     Host_Url,
     Base_Url: String;
     //UseToken,
     //procedure BuildToken(const sClaims: String; Var sToken: String);
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
(*
    JOSE.Core.JWT,
    JOSE.Core.JWS,
    JOSE.Core.JWK,
    JOSE.Core.JWA,
    JOSE.Core.Builder,
*)
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

procedure TUserWebAuthenticate.GetAction( Const uURL, mPath, sPath: String;
                                          var action: Integer);
begin
  action:=ACT_DEFAULT;
  // /api/main/login
  if sPath=METHOD_PING then
     action:=ACT_DIAGNOSTIC
  else
     if (GetStr(uURL,MethodIndex-1,'/')=mPath) then
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

function TUserWebAuthenticate.ActionLogin( const sJSON: String): string;
begin
  result:='';
end;

(*
procedure TUserWebAuthenticate.BuildToken(const sClaims: String; Var sToken: String);
var
  LToken: TJWT;
  LAlg: TJOSEAlgorithmId;
begin
  LToken := TJWT.Create;
  try
    // Token claims
    LToken.Claims.Subject := sClaims;
    LToken.Claims.IssuedAt := Now;
    LToken.Claims.Expiration := Now + 15 * OneMinute;
    LToken.Claims.Issuer := 'Saint-Sync-Server';
    // Signing algorithm
    // case cbbAlgorithm.ItemIndex of
    //  0: LAlg := TJOSEAlgorithmId.HS256;
    //  1: LAlg := TJOSEAlgorithmId.HS384;
    //  2: LAlg := TJOSEAlgorithmId.HS512;
    //  else LAlg := TJOSEAlgorithmId.HS256;
    // end;
    LAlg := TJOSEAlgorithmId.HS512;
    // Signing and compact format creation.
    sToken:= TJOSE.SerializeCompact(MY_SECRET, LAlg, LToken);
    // Token in compact representation
    // Header and Claims JSON representation
    // memoJSON.Lines.Add('Header: ' + TJSONUtils.ToJSON(LToken.Header.JSON));
    // memoJSON.Lines.Add('Claims: ' + TJSONUtils.ToJSON(LToken.Claims.JSON));
  finally
    LToken.Free;
  end;
end;
*)

procedure TUserWebAuthenticate.CommonAuth( const aMainPath, sJSON: string;
                                           UserRoles: TStrings;
                                           var aResp: String);
var
   Session: TDSSession;
   Token,
   sData,
   aJSON,
   errorMsg: string;
   Action: Integer;
   Valid: Boolean;
begin
  errorMsg:='Invalid invocation!';
  GetAction(Base_Url.ToLower,aMainPath,Method,Action);
  aResp:='';
  SetJSON(aResp,[SS_VALID],[false]);
  SetJSON(aJSON,[SS_VALID],[false]);
  if Not TokenParam And (action=ACT_DEFAULT) then
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
       Token:=GetStr(aJSON,SS_TOKENID);
       SetValue(Session,SS_SESSIONID, Session.SessionName);
       SetValue(Session,SS_USER, AUser);
       SetValue(Session,SS_ROLE, sRole);
       SetValue(Session,SS_DEVELOPID, GetStr(aJSON,SS_DEVELOPID));
       SetValue(Session,SS_APPID, GetStr(aJSON,SS_APPID));
       SetValue(Session,SS_APPNAME, GetStr(aJSON,SS_APPNAME));
       SetValue(Session,SS_LOGINID, GetStr(aJSON,SS_LOGINID)); // La tabla debe tener ID
       SetValue(Session,SS_FIRSTNAME, GetStr(aJSON,SS_FIRSTNAME));
       SetValue(Session,SS_LASTNAME, GetStr(aJSON,SS_LASTNAME));
       SetValue(Session,SS_EMAIL, GetStr(aJSON,SS_EMAIL));
       SetValue(Session,SS_PHONE, GetStr(aJSON,SS_PHONE));
       SetValue(Session,SS_BRANCH, GetStr(aJSON,SS_BRANCH));
       SetValue(Session,SS_TERMINAL, GetStr(aJSON,SS_TERMINAL));
       //------------------------------------------------
       (*
       if UseToken then
          begin
            sData:='';
            SetJSON(sData,['Id', SS_SESSIONID, SS_LOGINID, SS_USER],
                       [Session.Id,
                        Session.SessionName,
                        GetInt(aJSON,SS_LOGINID),AUser]);
            BuildToken(sData,Token);
          end;
       if not Token.IsEmpty then
          SetValue(Session,SS_TOKENID,Token);
       *)
       //------------------------------------------------
       SetDataSession(aJSON);
     end;
  aResp:=aJSON;
end;

constructor TUserWebAuthenticate.Create( App_ID: Integer; Request: TWebRequest; var Credentials: String);
Var
   aName,
   aValue,
   aQuery,
   lMethod,
   sHeader: String;
   I: Integer;
begin
  AppID:=APP_ID;
  Host_Url:=LowerCase(Request.Host);
  Base_Url:=Request.PathInfo;
  aQuery:=Request.Query;
  lMethod:=Request.Method;
  if ContainsText(Host_Url.ToLower,'api') then
     // '/otp/account/2'
     MethodIndex:=3
  else
     // '/api/otp/account/2'
     MethodIndex:=4;
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
  JSONBody:='';
  if (lMethod='POST') or (lMethod='PUT') or (lMethod='PATCH') then
     JSONBody:= TIdHTTPAppRequest(Request).Content;
  AUser:=GetStr(Credentials,SS_USER);
  APassword:=GetStr(Credentials,SS_PASSWORD);
  //UseToken:=false;
  TokenParam:=false;
  if AUser.IsEmpty And APassword.IsEmpty then
     begin
       Var sToken:=TNetEncoding.Base64.Decode(GetStr(JSONBody,'authtoken'));
       if sToken.IsEmpty then
          begin
            sToken:=Copy(aQuery,Pos('authtoken',aQuery.ToLower),Length(aQuery));
            Var p:=Pos('&',sToken);
            if P=0 then
               P:=Length(sToken);
            sToken:=Copy(sToken,1,P);
            sToken:=GetStr(sToken,2,'=');
           end;
       if Not sToken.IsEmpty and (lMethod='GET') then
          begin
            sToken:=TNetEncoding.Base64.Decode(sToken);
            AUser:=GetStr(sToken,1,':');
            APassword:=GetStr(sToken,2,':');
            SetJSON(Credentials,[SS_USER],[AUser]);
            SetJSON(Credentials,[SS_PASSWORD],[APassword]);
            TokenParam:=tRUE;
          end;
       (*
       UseToken:=true;
       *)
     end;
end;

destructor TUserWebAuthenticate.destroy;
begin
  AHeaders.Destroy;
  Inherited;
end;

end.

