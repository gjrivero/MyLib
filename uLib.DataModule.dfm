object dmMain: TdmMain
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 204
  Width = 389
  object Cnx: TFDConnection
    Params.Strings = (
      'Pooled=True')
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
    Left = 104
    Top = 24
  end
  object Cmd: TFDCommand
    Connection = Cnx
    Left = 160
    Top = 24
  end
end
