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
   SS_CUSTID      = 'custid';
   SS_LOGINID     = 'loginid';
   SS_FIRSTNAME   = 'firstname';
   SS_LASTNAME    = 'lastname';
   SS_TERMINAL    = 'terminal';
   SS_APPID       = 'appid';
   SS_APPNAME     = 'appname';
   SS_DEVELOPID   = 'developid';
   SS_DEVELOPER   = 'developer';
   SS_EMAIL       = 'email';
   SS_PHONE       = 'phone';
   SS_BRANCH      = 'branch';
   SS_TOKENID     = 'tokenid';
   SS_SESSIONID   = 'sessionid';

   ROLE_ADMIN     = 'admin';
   ROLE_APPS      = 'apps';
   ROLE_STANDARD  = 'standard';

   SECRET_CAPTION = 'Secret (%dbit)';
   MY_SECRET = 'MyS3cr3tP@$$w0rdf0r3nCtpA1xZ12f';

var
   db_MAIN,
   db_CUST,
   ActiveMain,
   ActiveCustomer,
   DefConnection: String;
   ACustomerList: TStringList;
   RDBMSKind: TFDRDBMSKind;


implementation

initialization
  ACustomerList:=TStringList.Create;
finalization
  ACustomerList.Destroy;
end.

