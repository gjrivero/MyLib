object dmMain: TdmMain
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 257
  Width = 400
  object Cnx: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    ResourceOptions.AssignedValues = [rvParamCreate, rvMacroCreate, rvMacroExpand, rvParamExpand, rvEscapeExpand, rvKeepConnection]
    LoginPrompt = False
    BeforeConnect = CnxBeforeConnect
    Left = 48
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
  object Qry: TFDQuery
    Connection = Cnx
    ResourceOptions.AssignedValues = [rvMacroCreate, rvMacroExpand, rvEscapeExpand]
    Left = 104
    Top = 24
  end
  object Cmd: TFDCommand
    Connection = Cnx
    ResourceOptions.AssignedValues = [rvMacroCreate, rvMacroExpand, rvEscapeExpand]
    Left = 160
    Top = 24
  end
  object SQLiteDriver: TFDPhysSQLiteDriverLink
    Left = 184
    Top = 136
  end
  object SQLiteValidate: TFDSQLiteValidate
    DriverLink = SQLiteDriver
    Left = 64
    Top = 136
  end
  object SQLiteSecurity: TFDSQLiteSecurity
    DriverLink = SQLiteDriver
    Left = 288
    Top = 136
  end
end
