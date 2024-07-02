unit uLib.Common;

interface

uses
   System.Classes,
   FireDAC.Stan.Intf;

const
   SS_USER        = 'user';
   SS_ROLE        = 'role';
   SS_PASSWORD    = 'password';
   SS_VALID       = 'valid';
   SS_ERROR       = 'error';
   SS_MESSAGE     = 'message';
   SS_CUSTID      = 'cust_id';
   SS_LOGINID     = 'login_id';
   SS_FIRSTNAME   = 'first_name';
   SS_LASTNAME    = 'last_name';
   SS_TERMINAL    = 'terminal';
   SS_APPID       = 'appid';
   SS_APPNAME     = 'appname';
   SS_DEVELOPID   = 'develop_id';
   SS_DEVELOPER   = 'developer';
   SS_BUSINESS    = 'business';
   SS_EMAIL       = 'email';
   SS_PHONE       = 'phone';
   SS_BRANCH      = 'branch';
   SS_TOKENID     = 'tokenId';
   SS_SESSIONID   = 'session_id';

   ROLE_ADMIN     = 'admin';
   ROLE_APPS      = 'apps';
   ROLE_STANDARD  = 'standard';

var
   db_MAIN,
   db_CUST,
   ds_session,

   ActiveMain,
   ActiveCustomer,

   DefConnection,
   MailHostSettings: String;
   ACustomerList: TStringList;
   RDBMSKind: TFDRDBMSKind;


implementation

initialization
  ACustomerList:=TStringList.Create;
finalization
  ACustomerList.Destroy;
end.

