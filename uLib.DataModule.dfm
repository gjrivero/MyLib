object dmMain: TdmMain
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 280
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
  object siLangDispatcher1: TsiLangDispatcher
    ActiveLanguage = 1
    NumOfLanguages = 2
    LangNames.Strings = (
      'English'
      'Spanish')
    Language = 'English'
    GlobalExclusionList.Strings = (
      '!TVirtualImageListItem.CollectionName'
      '!TVirtualImageList.DisabledSuffix')
    Left = 184
    Top = 128
  end
  object siLang_dmMain: TsiLang
    Version = '7.9.6.4'
    StringsTypes.Strings = (
      'TIB_STRINGLIST'
      'TSTRINGLIST'
      'TWIDESTRINGS')
    NumOfLanguages = 2
    LangDispatcher = siLangDispatcher1
    LangDelim = 1
    LangNames.Strings = (
      'English'
      'Spanish')
    Language = 'English'
    ExcludedProperties.Strings = (
      'Category'
      'SecondaryShortCuts'
      'HelpKeyword'
      'InitialDir'
      'HelpKeyword'
      'ActivePage'
      'ImeName'
      'DefaultExt'
      'FileName'
      'FieldName'
      'PickList'
      'DisplayFormat'
      'EditMask'
      'KeyList'
      'LookupDisplayFields'
      'DropDownSpecRow'
      'TableName'
      'DatabaseName'
      'IndexName'
      'MasterFields'
      'SQL'
      'DeleteSQL'
      'UpdateSQL'
      'ModifySQL'
      'KeyFields'
      'LookupKeyFields'
      'LookupResultField'
      'DataField'
      'KeyField'
      'ListField')
    Left = 192
    Top = 136
    TranslationData = {
      73007400430061007000740069006F006E0073005F0055006E00690063006F00
      640065000D000A0073007400480069006E00740073005F0055006E0069006300
      6F00640065000D000A007300740044006900730070006C00610079004C006100
      620065006C0073005F0055006E00690063006F00640065000D000A0073007400
      46006F006E00740073005F0055006E00690063006F00640065000D000A007300
      74004D0075006C00740069004C0069006E00650073005F0055006E0069006300
      6F00640065000D000A007300740044006C006700730043006100700074006900
      6F006E0073005F0055006E00690063006F00640065000D000A00570061007200
      6E0069006E00670001005700610072006E0069006E006700010001000D000A00
      4500720072006F00720001004500720072006F007200010001000D000A004900
      6E0066006F0072006D006100740069006F006E00010049006E0066006F007200
      6D006100740069006F006E00010001000D000A0043006F006E00660069007200
      6D00010043006F006E006600690072006D00010001000D000A00590065007300
      01002600590065007300010001000D000A004E006F00010026004E006F000100
      01000D000A004F004B0001004F004B00010001000D000A00430061006E006300
      65006C000100430061006E00630065006C00010001000D000A00410062006F00
      7200740001002600410062006F0072007400010001000D000A00520065007400
      720079000100260052006500740072007900010001000D000A00490067006E00
      6F007200650001002600490067006E006F0072006500010001000D000A004100
      6C006C000100260041006C006C00010001000D000A004E006F00200054006F00
      200041006C006C0001004E0026006F00200074006F00200041006C006C000100
      01000D000A00590065007300200054006F00200041006C006C00010059006500
      7300200074006F002000260041006C006C00010001000D000A00480065006C00
      700001002600480065006C007000010001000D000A0073007400530074007200
      69006E00670073005F0055006E00690063006F00640065000D000A0073007400
      4F00740068006500720053007400720069006E00670073005F0055006E006900
      63006F00640065000D000A00730074004C006F00630061006C00650073005F00
      55006E00690063006F00640065000D000A007300740043006F006C006C006500
      6300740069006F006E0073005F0055006E00690063006F00640065000D000A00
      73007400430068006100720053006500740073005F0055006E00690063006F00
      640065000D000A00}
  end
end
