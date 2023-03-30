object FDM: TFDM
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 303
  Width = 386
  object Cnx: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    LoginPrompt = False
    BeforeConnect = CnxBeforeConnect
    Left = 48
    Top = 24
  end
  object Qry: TFDQuery
    Connection = Cnx
    Left = 96
    Top = 24
  end
  object Cmd: TFDCommand
    Connection = Cnx
    ResourceOptions.AssignedValues = [rvServerOutput]
    Left = 144
    Top = 24
  end
  object WaitCursor: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 232
    Top = 24
  end
  object EventAlerter1: TFDEventAlerter
    Connection = Cnx
    Left = 308
    Top = 24
  end
  object SQLiteDriver: TFDPhysSQLiteDriverLink
    Left = 32
    Top = 184
  end
  object SQLiteSecurity: TFDSQLiteSecurity
    DriverLink = SQLiteDriver
    Left = 120
    Top = 184
  end
end
