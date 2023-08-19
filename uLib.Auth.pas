unit uLib.Auth;

interface

uses
    Web.HTTPApp,
    System.JSON,
    System.SysUtils,
    System.classes;

Const
   ACT_DEFAULT  = 0;
   ACT_SIGNUP   = 1;
   ACT_LOGIN    = 2;
   ACT_VALIDATE = 3;

Type
   TUserWebAuthenticate = class
     function GetUser(const sJSON: String): String; virtual;
     function GetDeveloper(const ApiKey: String): TJSONObject; virtual;
     function OtherActions( action: Integer;
                            const method: string;
                            var aJSON: String): boolean; virtual;
     procedure GetAction(Const mPath, sPath: String; var action: Integer); virtual;
     procedure SetDataSession(aJSON: String); virtual;
     procedure CommonAuth( const aMainPath, sJSON: string;
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
     function ValidateDeveloper( var errorMsg: String): Integer;
     function ActionSignUp( const sJSON: String;
                            var aJSON: String): Boolean;
     function ActionLogin( const sJSON: String;
                           var aJSON: String): Boolean;
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

function TUserWebAuthenticate.GetUser(const sJSON: String): String;
begin
  result:='';
end;

function TUserWebAuthenticate.OtherActions( action: Integer;
                                            const method: string;
                                            var aJSON: String): boolean;
begin
  result:=false;
end;

procedure TUserWebAuthenticate.GetAction(Const mPath, sPath: String; var action: Integer);
begin
  action:=ACT_DEFAULT;
  if (GetStr(Base_Url,2,'/')=mPath) then
     if sPath='login' then
        action:=ACT_LOGIN
     else
        if sPath='signup' then
           action:=ACT_SIGNUP
     else
        if sPath='validate' then
           action:=ACT_VALIDATE;
end;

procedure TUserWebAuthenticate.SetDataSession(aJSON: String);
var
   Session: TDSSession;
begin
  Session := TDSSessionManager.GetThreadSession;

  Session.PutData('DevelopID', GetStr(aJSON,'developID'));
  Session.PutData('AppID', GetStr(aJSON,'appID'));
  Session.PutData('LoginID', GetStr(aJSON,'userID'));
  Session.PutData('User', GetStr(aJSON,'user'));
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

function TUserWebAuthenticate.ActionSignUp( const sJSON: String;
                                            var aJSON: String): Boolean;
begin
  SetStr(aJSON,'user',GetStr(sJSON,'user'));
  SetStr(aJSON,'password',GetStr(sJSON,'password'));
  SetStr(aJSON,'role',GetStr(sJSON,'role'));

  SetStr(aJSON,'firstname', GetStr(JSONBody,'firstName'));
  SetStr(aJSON,'lastname', GetStr(JSONBody,'lastName'));
  SetStr(aJSON,'email', GetStr(JSONBody,'email'));
  SetStr(aJSON,'phone', GetStr(JSONBody,'phone'));
  result:=true;
end;

function TUserWebAuthenticate.ActionLogin( const sJSON: String;
                                             var aJSON: String): Boolean;
begin
  aJSON:=GetUser(sJSON);
  result:=(GetInt(aJSON,'success')=1);
end;

procedure TUserWebAuthenticate.CommonAuth( const aMainPath, sJSON: string;
                                           UserRoles: TStrings;
                                           var aResp: String);
Var
   aJSON,
   sRole,
   errorMsg: string;
   Action,
   loginID,
   developID: Integer;
   Valid: Boolean;
begin
  Valid:=False;
  errorMsg:='Invalid invocation!';
  GetAction(aMainPath,Method,Action);
  aResp:='';
  SetInt(aResp,'valid',0);
  if action=ACT_DEFAULT then
     Exit;
  Valid:=true;
  //---------------------------------
  User:=GetStr(sJSON,'user');
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
           Case Action Of
            ACT_SIGNUP:
               Valid:=ActionSignUp(sJSON, aJSON);
            ACT_LOGIN:
               begin
                 Valid:=ActionLogin(sJSON, aJSON);
                 if Not Valid then
                    begin
                      errorMsg:='User not active or doesn''t exists!';
                    end;
               end;
           End;
         end;
       else
         valid:=OtherActions(Action,Method,aJSON);
     end;
  if Valid then
     begin
       Case Action Of
        ACT_SIGNUP,
        ACT_LOGIN:
         begin
           SetInt(aJSON,'userID', loginID);
           StrRemove(aJSON,'password');
         end;
       end;
       SetInt(aJSON,'developID',developID);
       SetInt(aJSON,'appID',GetInt(JSONBody,'appID'));
       sRole:=GetStr(aJSON,'role');
       UserRoles.Add(sRole);
       SetDataSession(aJSON);
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
  Base_Url:=LowerCase(Request.PathInfo);
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

