unit uLib.DataModule;

interface

uses
  System.JSON, System.UITypes,
  System.SysUtils, System.Classes,
  Data.DB,
  Data.DBXPlatform,
  Datasnap.DSSession,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Comp.UI,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  ,FireDAC.Phys.MSSQL
  ,FireDAC.Phys.MSSQLDef
  ,FireDAC.Phys.MySQL
  ,FireDAC.Phys.MySQLDef
  ,FireDAC.Phys.PG
  ,FireDAC.Phys.PGDef
{$ENDIF}
  ,System.Generics.Collections, FireDAC.Stan.ExprFuncs;


type
  TdmMain = class(TDataModule)
    Cnx: TFDConnection;
    WaitCursor: TFDGUIxWaitCursor;
    EventAlerter1: TFDEventAlerter;
    Qry: TFDQuery;
    Cmd: TFDCommand;
    SQLiteDriver: TFDPhysSQLiteDriverLink;
    SQLiteValidate: TFDSQLiteValidate;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure CnxBeforeConnect(Sender: TObject);
  private
    { Private declarations }
{$IF DEFINED(LINUX) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
    MySQLDriver: TFDPhysMySQLDriverLink;
    PgDriver: TFDPhysPgDriverLink;
    MSSQLDriver: TFDPhysMSSQLDriverLink;
{$ENDIF}
  public
    { Public declarations }
  end;

var
  dmMain: TdmMain;
  ADriverVendor,
  ASQLiteSecurity,
  ADatabaseServer: TStringList;


implementation

{$R *.dfm}

uses
   uLib.Common;

procedure TdmMain.CnxBeforeConnect(Sender: TObject);
begin
  Cnx.Params.Text:=ADatabaseServer.Text;
  Cnx.DriverName:=ADatabaseServer.Values['DriverId'];
  RDBMSKind:=Cnx.RDBMSKind;
  case Cnx.RDBMSKind of
    TFDRDBMSKinds.MSSQL:
      ;
    TFDRDBMSKinds.SQLite:
      begin
        SQLiteDriver.VendorHome:=ADriverVendor.Values['VendorHome'];
        SQLiteDriver.VendorLib:=ADriverVendor.Values['VendorLib'];
        SQLiteValidate.Database:=ADatabaseServer.Values['Database'];
      end;
    TFDRDBMSKinds.MySQL:
      begin
        MySQLDriver.VendorHome:=ADriverVendor.Values['VendorHome'];
        MySQLDriver.VendorLib:=ADriverVendor.Values['VendorLib'];
      end;
    TFDRDBMSKinds.PostgreSQL:
      begin
        PGDriver.VendorHome:=ADriverVendor.Values['VendorHome'];
        PGDriver.VendorLib:=ADriverVendor.Values['VendorLib'];
      end;
  end;
end;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  MSSQLDriver:=TFDPhysMSSQLDriverLink.Create(Nil);
  MySQLDriver:=TFDPhysMySQLDriverLink.Create(Nil);
  PgDriver:=TFDPhysPgDriverLink.Create(Nil);
{$ENDIF}
  Cnx.Connected:=False;
  //Cmd.UpdateOptions.AssignedValues[rvServerOutput]:=true;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
begin
{$IF DEFINED(Linux) or DEFINED(MACOS) or DEFINED(MSWINDOWS)}
  MSSQLDriver.Destroy;
  MySQLDriver.Destroy;
  PgDriver.Destroy;
{$ENDIF}
end;

initialization
  ADriverVendor:=TStringList.Create;
  ADatabaseServer:=TStringList.Create;
  ASQLiteSecurity:=TStringList.Create;
finalization
  ADriverVendor.Destroy;
  ADatabaseServer.Destroy;
  ASQLiteSecurity.Destroy;
end.


