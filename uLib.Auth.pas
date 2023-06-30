unit uLib.Auth;

interface

uses
    Web.HTTPApp,
    System.JSON,
    System.SysUtils,
    System.classes;

Const
   ACT_NO_VALID = 0;
   ACT_SIGNUP   = 1;
   ACT_LOGIN    = 2;
   ACT_VALIDATE = 3;

Type
   TUserWebAuthenticate = class
     function GetUser(const user, shash: String): String; virtual;
     function GetDeveloper(const ApiKey: String): TJSONObject; virtual;
     function OtherActions(const method: string; var aJSON: String): boolean; virtual;

     procedure SetDataSession(aJSON: String); virtual;
     procedure CommonAuth( aActions: TIntegerSet;
                           const aUser, aPassword: string;
                           UserRoles: TStrings;
                           var aResp: String);

     constructor Create( Request: TWebRequest;
                         const devKey: string='');
   protected
     Base_Url,
     JSONBody: String;

   private
     User,
     Method,
     Apikey,
     Dev_Key_Name: string;
     function GetAction(sPath: String): Integer;
     function ValidateDeveloper( var errorMsg: String): Integer;
     function ActionLogin( const sHASH: String;
                            var aJSON: String;
                            var loginID: Integer): boolean;
     function ActionSignUp(const sHASH, sRole: String; var aJSON: String): boolean;
   public
   end;


implementation

Uses
    System.Hash,
    System.Generics.Collections,

    IdHTTPHeaderInfo,
    IdHTTPWebBrokerBridge,
    Datasnap.DSSession,

    uLib.Base,
    uLib.Data,
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

function TUserWebAuthenticate.GetDeveloper(const ApiKey: String): TJSONObject;
begin
  result:=Nil;
end;

function TUserWebAuthenticate.GetUser(const user, shash: String): String;
begin
  result:='';
end;

function TUserWebAuthenticate.OtherActions(const method: string; var aJSON: String): boolean;
begin
  result:=false;
end;

function TUserWebAuthenticate.GetAction(sPath: String): Integer;
begin
  result:=ACT_NO_VALID;
  if sPath='login' then
     result:=ACT_LOGIN
  else
     if sPath='signup' then
        result:=ACT_SIGNUP
  else
     if sPath='validate' then
        result:=ACT_VALIDATE;
end;

procedure TUserWebAuthenticate.SetDataSession(aJSON: String);
var
   Session: TDSSession;
begin
  Session := TDSSessionManager.GetThreadSession;

  Session.PutData('DevelopID', GetStr(aJSON,'developID'));
  Session.PutData('AppID', GetStr(aJSON,'appType'));
  Session.PutData('LoginID', GetStr(aJSON,'userID'));
  Session.PutData('User', GetStr(aJSON,'userlogin'));
  Session.PutData('Passwd', GetStr(aJSON,'passwd'));
  Session.PutData('FirstName', GetStr(aJSON,'firstname'));
  Session.PutData('LastName', GetStr(aJSON,'lastname'));
  Session.PutData('Role', GetStr(aJSON,'role'));
  Session.PutData('CustID', GetStr(aJSON,'custID'));
end;

function TUserWebAuthenticate.ValidateDeveloper( var errorMsg: String): Integer;
var
   aJSON: TJSONObject;
   valid: Boolean;
begin
  result:=0; //-1;
  If (dev_key_name<>'') Then
     begin
       valid:=false;
       aJSON:=GetDeveloper(ApiKey);
       if (aJSON<>Nil) then
          begin
            result:=GetInt(aJSON,'id');
            valid:=(GetInt(aJSON,'id')>0) And
              (GetInt(aJSON,'success')=1) And
              (GetInt(aJSON,'active')=1);
            aJSON.DisposeOf;
          end;
(*
       if Not Valid then
          begin
            errorMsg:='Developer API-Key not active or doesn''t exists!';
            exit(-1);
          end;
*)
     end;
end;

function TUserWebAuthenticate.ActionSignUp( const sHASH, sRole: String;
                                            var aJSON: String): Boolean;
begin
  SetStr(aJSON,'userlogin',user);
  SetStr(aJSON,'passwd',sHash);
  SetStr(aJSON,'role',sRole);
  SetStr(aJSON,'firstname', GetStr(JSONBody,'firstName'));
  SetStr(aJSON,'lastname', GetStr(JSONBody,'lastName'));
  SetStr(aJSON,'email', GetStr(JSONBody,'email'));
  SetStr(aJSON,'phone', GetStr(JSONBody,'phone'));
  result:=true;
end;

function TUserWebAuthenticate.ActionLogin( const sHASH: String;
                                             var aJSON: String;
                                             var loginID: Integer): Boolean;
begin
  result:=false;
  aJSON:=GetUser(User,sHash);
  if Not aJSON.IsEmpty then
     begin
       loginID:=GetInt(aJSON,'userID');
       result:=
          (GetInt(aJSON,'success')=1) And
          //(GetInt(aJSON,'verified')=1) And
          (GetInt(aJSON,'active')=1);
     end;
end;

procedure TUserWebAuthenticate.CommonAuth( aActions: TIntegerSet;
                                           const aUser, aPassword: string;
                                           UserRoles: TStrings;
                                           var aResp: String);
Var
   aJSON,
   sRole,
   sHash,
   errorMsg: string;
   Action,
   loginID,
   developID: Integer;
   StringHash: THashSHA2;
   Valid: Boolean;
begin
  Valid:=False;
  errorMsg:='Invalid invocation!';
  action:=GetAction(Method);
  if action=ACT_NO_VALID then
     Exit;
  Valid:=Action In aActions;
  //---------------------------------
  User:=aUser;
  loginID:=0;
  developID:=0;
  sRole:=GetStr(JSONBody,'role');
  if sRole='' then
     sRole:='standard';
  aJSON:='';
  //---------------------------------
  if (Action In [ACT_SIGNUP,ACT_LOGIN]) then
     developID:=validateDeveloper(errorMsg);
  if (developID>-1) then
     case Action of
       ACT_SIGNUP,
       ACT_LOGIN:
         Begin
           StringHash := THashSHA2.Create();
           sHash:=StringHash.GetHashString(LowerCase(User)+':'+aPassword);
           Case Action Of
            ACT_SIGNUP:
               Valid:=ActionSignUp(sHash,sRole,aJSON);
            ACT_LOGIN:
               begin
                 Valid:=ActionLogin(sHash,aJSON,loginID);
                 if Not Valid then
                    begin
                      errorMsg:='User not active or doesn''t exists!';
                    end;
               end;
           End;
         end;
       else
         valid:=OtherActions(Method,aJSON);
     end;
  if Valid then
     Case Action Of
      ACT_SIGNUP,
      ACT_LOGIN:
         begin
           SetStr(aJSON,'userlogin',user);
           SetInt(aJSON,'userID', loginID);
           SetInt(aJSON,'developID',developID);
           SetInt(aJSON,'appType',GetInt(JSONBody,'appID'));

           SetDataSession(aJSON);

           StrRemove(aJSON,'passwd');
           sRole:=GetStr(aJSON,'role');
           UserRoles.Add(sRole);
         end;
     end;
  aResp:='';
  SetBool(aResp,'valid',Valid);
  SetJSON(aResp,'data',aJSON);
  SetStr(aResp,'message',errorMsg);
end;

constructor TUserWebAuthenticate.Create( Request: TWebRequest;
                                         const devKey: string='');
Var
   aHeaders: TStringList;
   aName,
   aValue,
   sHeader: String;
   I: Integer;
begin
  Method:=LowerCase(GetStr(Request.PathInfo,3,'/'));
  Base_Url:=Request.PathInfo;
  dev_key_name:=devKey;
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

