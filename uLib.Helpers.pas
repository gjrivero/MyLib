unit uLib.Helpers;

interface

uses
    Data.DB
   ,System.JSON
   ,System.UITypes
   ,System.Classes
   ,System.Generics.Collections
   ;

Type

  TDataSetHelper = class helper for TDataSet
  public
    function AsJSONvalue(AReturnNilIfEOF: boolean = false): TJSONValue;

    function AsJSONObject(AReturnNilIfEOF: boolean = false): TJSONObject;
    function AsJSONObjectString(AReturnNilIfEOF: boolean = false): string;

    function AsJSONArray(AReturnNilIfEOF: boolean = false): TJSONArray;
    function AsJSONArrayString(AReturnNilIfEOF: boolean = false): string;

    function AsStringList(Separator: Char): TStringList;
    procedure LoadFromJSONArray( AJSONArray: TJSONArray); overload;
    procedure LoadFromJSONArray( AJSONArray: TJSONArray;
                                 AIgnoredFields: TArray<string>); overload;
    procedure LoadFromJSONArrayString( AJSONArrayString: string;
                                       AIgnoredFields: TArray<string>); overload;
    procedure LoadFromJSONArrayString( AJSONArrayString: string); overload;
    procedure AppendFromJSONArrayString( AJSONArrayString: string); overload;
    procedure AppendFromJSONArrayString( AJSONArrayString: string;
                                         AIgnoredFields: TArray<string>); overload;
  end;

function ISOTimeToString(ATime: TTime): string;
function ISODateToString(ADate: TDateTime): string;
function ISODateTimeToString(ADateTime: TDateTime): string;
function CheckISOTimeStrSeparator(const TimeAsString: string; const Offset: Word): boolean;
function CheckISODateStrSeparator(const DateAsString: string; const Offset: Word): boolean;
function ISOStrToDateTime(const DateTimeAsString: string): TDateTime;
function ISOStrToTime(const TimeAsString: string): TTime;
function ISOStrToDate(const DateAsString: string): TDate;

implementation

uses
    System.DateUtils
   ,System.SysUtils
   ,System.RTTI
   ,System.NetEncoding
   ,Soap.EncdDecd
   ,Data.SqlTimSt
   ,Data.FmtBcd;


function ContainsFieldName(const FieldName: string;
  var FieldsArray: TArray<string>): boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FieldsArray) - 1 do
  begin
    if SameText(FieldsArray[I], FieldName) then
      Exit(True);
  end;
  Result := false;
end;

function ISOTimeToString(ATime: TTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('hh:nn:ss', ATime, fs);
end;

function ISODateToString(ADate: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', ADate);
end;

function ISODateTimeToString(ADateTime: TDateTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  fs.DateSeparator := '-';
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function CheckISOTimeStrSeparator(const TimeAsString: string; const Offset: Word): boolean;
begin
  Result := (TimeAsString.Chars[Offset + 2] = ':') and
    (TimeAsString.Chars[Offset + 5] = ':');
end;

function CheckISODateStrSeparator(const DateAsString: string; const Offset: Word): boolean;
begin
  Result := (DateAsString.Chars[Offset + 4] = '-') and
    (DateAsString.Chars[Offset + 7] = '-');
end;

function ISOStrToDateTime(const DateTimeAsString: string): TDateTime;
begin
  Result:=-1;
  if DateTimeAsString.IsEmpty then
     Exit;

  if not CheckISODateStrSeparator(DateTimeAsString, 0) then
    raise Exception.Create('Invalid ISO DateTime String');

  if not CheckISOTimeStrSeparator(DateTimeAsString, 11) then
    raise Exception.Create('Invalid ISO DateTime String');

  Result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)),
    StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)),
    StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

function ISOStrToTime(const TimeAsString: string): TTime;
begin
  if not CheckISOTimeStrSeparator(TimeAsString, 0) then
    raise Exception.Create('Invalid ISO Time String');

  Result := EncodeTime(StrToInt(Copy(TimeAsString, 1, 2)),
    StrToInt(Copy(TimeAsString, 4, 2)),
    StrToIntDef(Copy(TimeAsString, 7, 2), 0), 0);
end;

function ISOStrToDate(const DateAsString: string): TDate;
begin
  if not CheckISODateStrSeparator(DateAsString, 0) then
    raise Exception.Create('Invalid ISO Date String');

  Result := EncodeDate(StrToInt(Copy(DateAsString, 1, 4)),
    StrToInt(Copy(DateAsString, 6, 2)), StrToInt(Copy(DateAsString, 9, 2)));
end;

{ TDataSetHelper }

procedure DataSetToJSONObject(
             ADataSet: TDataSet;
             AJSONObject: TJSONObject;
             ADataSetInstanceOwner: boolean);
var
  I: Integer;
  key: string;
  //ts: TSQLTimeStamp;
  MS: TMemoryStream;
  SS: TStringStream;
begin
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    key := ADataSet.Fields[I].FieldName.ToLower;

    if ADataSet.Fields[I].IsNull then
    begin
      AJSONObject.AddPair(key,TJSONNull.Create);
      Continue;
    end;
    case ADataSet.Fields[I].DataType of
      TFieldType.ftInteger, TFieldType.ftLongWord, TFieldType.ftAutoInc, TFieldType.ftSmallint,
        TFieldType.ftShortint:
        AJSONObject.AddPair(key,TJSONNumber.Create(ADataSet.Fields[I].AsInteger));
      TFieldType.ftLargeint:
        begin
          AJSONObject.AddPair(key,TJSONNumber.Create(ADataSet.Fields[I].AsLargeInt));
        end;
      TFieldType.ftSingle,
      TFieldType.ftFloat: AJSONObject.AddPair(key,TJSONNumber.Create(ADataSet.Fields[I].AsFloat));
      ftWideString, ftMemo, ftWideMemo:
        begin
          AJSONObject.AddPair(key,TJSONString.Create(ADataSet.Fields[I].AsWideString));
        end;
      ftString:
        begin
          AJSONObject.AddPair(key,TJSONString.Create(ADataSet.Fields[I].AsString));
        end;
      TFieldType.ftDate,
      TFieldType.ftDateTime:
        AJSONObject.AddPair(key,TJSONString.Create(
           ISODateTimeToString(ADataSet.Fields[I].AsDateTime)));
      TFieldType.ftTimeStamp:
          AJSONObject.AddPair(key,TJSONString.Create(
              ISODateTimeToString(SQLTimeStampToDateTime(ADataSet.Fields[I].AsSQLTimeStamp))));
      TFieldType.ftCurrency:
        AJSONObject.AddPair(key,TJSONNumber.Create(ADataSet.Fields[I].AsCurrency));
      TFieldType.ftBCD, TFieldType.ftFMTBcd:
        AJSONObject.AddPair(key,TJSONNumber.Create(BcdToDouble(ADataSet.Fields[I].AsBcd)));
      TFieldType.ftGraphic,
      TFieldType.ftBlob,
      TFieldType.ftStream:
        begin
          MS := TMemoryStream.Create;
          try
            TBlobField(ADataSet.Fields[I]).SaveToStream(MS);
            MS.Position := 0;
            SS := TStringStream.Create('', TEncoding.ANSI);
            try
              EncodeStream(MS, SS);
              SS.Position := 0;
              AJSONObject.AddPair(key,TJSONString.Create(SS.DataString));
            finally
              SS.Free;
            end;
          finally
            MS.Free;
          end;
        end;
      TFieldType.ftBoolean:
        begin
          AJSONObject.AddPair(key,TJSONNumber.Create(Ord(ADataSet.Fields[I].AsBoolean)));
        end;
    end;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);
end;

procedure DataSetToJSONvalue(
             ADataSet: TDataSet;
             AJSONValue: TJSONValue;
             ADataSetInstanceOwner: boolean);
var AJSON: TJSONObject;
begin
  DataSetToJSONObject(ADataSet,AJSON,ADataSetInstanceOwner);
  AJSONValue:=AJSON.AsType<TJSONValue>;
end;

procedure DataSetToString(
             ADataSet: TDataSet;
             ADataRow: String;
             Separator: Char;
             ADataSetInstanceOwner: boolean);
var
  I: Integer;
  MS: TMemoryStream;
  SS: TStringStream;
begin
  ADataRow:='';
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    if ADataSet.Fields[I].IsNull then
    begin
      ADataRow:=ADataRow+''+Separator;
      Continue;
    end;
    case ADataSet.Fields[I].DataType of
      ftInteger, ftLongWord,
      ftAutoInc, ftSmallint,
      ftShortint, ftLargeint,

      ftSingle, ftFloat:
         ADataRow:=ADataRow+ADataSet.Fields[I].AsString+Separator;

      ftWideString, ftMemo, ftWideMemo, ftString:
         ADataRow:=ADataRow+ADataSet.Fields[I].AsString+Separator;
      ftDate,ftDateTime, ftTimeStamp:
         ADataRow:=ADataRow+ADataSet.Fields[I].AsString+Separator;
      ftCurrency, ftBCD, ftFMTBcd:
        ADataRow:=ADataRow+ADataSet.Fields[I].AsString+Separator;
      ftGraphic,
      ftBlob,
      ftStream:
        begin
          MS := TMemoryStream.Create;
          try
            TBlobField(ADataSet.Fields[I]).SaveToStream(MS);
            MS.Position := 0;
            SS := TStringStream.Create('', TEncoding.ANSI);
            try
              EncodeStream(MS, SS);
              SS.Position := 0;
              ADataRow:=ADataRow+SS.DataString+Separator;
            finally
              SS.Free;
            end;
          finally
            MS.Free;
          end;
        end;
      TFieldType.ftBoolean:
        begin
          ADataRow:=ADataRow+IntToStr(Ord(ADataSet.Fields[I].AsBoolean))+Separator;
        end;
    end;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);
end;

procedure JSONObjectToDataSet(
            AJSONObject: TJSONObject;
            ADataSet: TDataSet;
            AIgnoredFields: TArray<string>;
            AJSONObjectInstanceOwner: boolean);
var
  I: Integer;
  key: string;
  fs: TFormatSettings;
  MS: TMemoryStream;
  SS: TStringStream;
begin
  for I := 0 to ADataSet.FieldCount - 1 do
  begin
    if ContainsFieldName(ADataSet.Fields[I].FieldName, AIgnoredFields) then
      Continue;
    key := ADataSet.Fields[I].FieldName;
    case ADataSet.Fields[I].DataType of
      TFieldType.ftInteger, TFieldType.ftLongWord,
      TFieldType.ftAutoInc, TFieldType.ftSmallint,
      TFieldType.ftShortint:
        begin
          ADataSet.Fields[I].AsInteger := StrToInt(AJSONObject.GetValue(key).Value);
        end;
      TFieldType.ftLargeint:
        begin
          ADataSet.Fields[I].AsLargeInt := StrToInt(AJSONObject.GetValue(key).Value);
        end;
      TFieldType.ftSingle, TFieldType.ftFloat:
        begin
          ADataSet.Fields[I].AsFloat := StrToFloat(AJSONObject.GetValue(key).Value);
        end;
      ftString, ftWideString, ftMemo, ftWideMemo:
        begin
          ADataSet.Fields[I].AsString := AJSONObject.GetValue(key).Value;
        end;
      TFieldType.ftDate:
        begin
          ADataSet.Fields[I].AsDateTime :=ISOStrToDate(AJSONObject.GetValue(key).Value);
        end;
      TFieldType.ftDateTime:
        begin
          ADataSet.Fields[I].AsDateTime :=ISOStrToDateTime(AJSONObject.GetValue(key).Value);
        end;
      TFieldType.ftTimeStamp:
        begin
          ADataSet.Fields[I].AsSQLTimeStamp :=DateTimeToSQLTimeStamp(ISOStrToDateTime(AJSONObject.GetValue(key).Value));
        end;
      TFieldType.ftCurrency:
        begin
          fs.DecimalSeparator := '.';
          { ,$IFNDEF TOJSON }
          // ADataSet.Fields[I].AsCurrency :=
          // StrToCurr((v as TJSONString).Value, fs);
          { .$ELSE } // Delphi XE7 introduces method "ToJSON" to fix some old bugs...
          ADataSet.Fields[I].AsCurrency :=
            StrToCurr( floatToStr(StrToFloat(AJSONObject.GetValue(key).Value)), fs);
          { .$IFEND }
        end;
      TFieldType.ftFMTBcd:
        begin
          ADataSet.Fields[I].AsBcd := DoubleToBcd(StrToFloat(AJSONObject.GetValue(key).Value));
        end;
      TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
        begin
          MS := TMemoryStream.Create;
          try
            SS := TStringStream.Create(AJSONObject.GetValue(key).Value,
              TEncoding.ANSI);
            try
              DecodeStream(SS, MS);
              MS.Position := 0;
              TBlobField(ADataSet.Fields[I]).LoadFromStream(MS);
            finally
              SS.Free;
            end;
          finally
            MS.Free;
          end;
        end;
      TFieldType.ftBoolean:
        begin
{$IFDEF JSONBOOL}
           ADataSet.Fields[I].AsBoolean := AJSONObject.GetValue(key).Value;
{$ELSE}
          if StrToInt(AJSONObject.GetValue(key).Value)=1 then
            ADataSet.Fields[I].AsBoolean := True
          else
            ADataSet.Fields[I].AsBoolean := false;
{$ENDIF}
        end;
      // else
      // raise EMapperException.Create('Cannot find type for field ' + key);
    end;
  end;
  if AJSONObjectInstanceOwner then
    FreeAndNil(AJSONObject);
end;

procedure DataSetToJSONArray(
            ADataSet: TDataSet;
            AJSONArray: TJSONArray;
            ADataSetInstanceOwner: boolean);
var
  Obj: TJSONObject;
begin
  while not ADataSet.Eof do
  begin
    Obj := TJSONObject.Create;
    AJSONArray.Add(Obj);
    DataSetToJSONObject(ADataSet, Obj, false);
    ADataSet.Next;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);
end;

procedure DataSetToStringList(
            ADataSet: TDataSet;
            AList: TStringList;
            Separator: Char;
            ADataSetInstanceOwner: boolean);
var
  sRow: String;
begin
  while not ADataSet.Eof do
  begin
    DataSetToString(ADataSet, sRow, Separator, false);
    AList.Add(sRow);

    ADataSet.Next;
  end;
  if ADataSetInstanceOwner then
    FreeAndNil(ADataSet);
end;

procedure JSONArrayToDataSet(
            AJSONArray: TJSONArray;
            ADataSet: TDataSet;
            AIgnoredFields: TArray<string>;
            AJSONArrayInstanceOwner: boolean); overload;
var
  I: Integer;
begin
  for I := 0 to AJSONArray.Count - 1 do
  begin
    ADataSet.Append;
    JSONObjectToDataSet(TJSONObject(AJSONArray.Items[I]), ADataSet,  AIgnoredFields, false);
    ADataSet.Post;
  end;
  if AJSONArrayInstanceOwner then
    AJSONArray.Free;
end;

procedure JSONArrayToDataSet( AJSONArray: TJSONArray;
                              ADataSet: TDataSet;
                              AJSONArrayInstanceOwner: boolean); overload;
begin
  JSONArrayToDataSet(AJSONArray, ADataSet, TArray<string>.Create(),AJSONArrayInstanceOwner);
end;

function TDataSetHelper.AsJSONObject(AReturnNilIfEOF: boolean = false): TJSONObject;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    DataSetToJSONObject(Self, JObj, false);
    if AReturnNilIfEOF and (JObj.Count = 0) then
      FreeAndNil(JObj);
    Result := JObj;
  except
    FreeAndNil(JObj);
    raise;
  end;
end;

function TDataSetHelper.AsJSONvalue(AReturnNilIfEOF: boolean = false): TJSONValue;
var
  JObj: TJSONObject;
  JArr: TJSONArray;
begin
  if Self.RecordCount>1 then
     begin
       JArr := TJSONArray.Create;
       try
         if not Eof then
            DataSetToJSONArray(Self, JArr, false);
         Result := JArr.AsType<TJSONValue>;
       except
         FreeAndNil(JArr);
         raise;
       end;
     end
  else
     begin
       JObj := TJSONObject.Create;
       try
         DataSetToJSONObject(Self, JObj, false);
         if AReturnNilIfEOF and (JObj.Count = 0) then
            FreeAndNil(JObj);
         Result := JObj.AsType<TJSONValue>;
       except
         FreeAndNil(JObj);
         raise;
       end;
     end;
end;

function TDataSetHelper.AsJSONObjectString(AReturnNilIfEOF: boolean = false): String;
Var JSON: TJSONObject;
begin
  JSON:=AsJSONObject(AReturnNilIfEOF);
  try
    result:=JSON.ToString; // ToJSON;
  finally
    JSON.Free;
  end;
end;

function TDataSetHelper.AsJSONArray(AReturnNilIfEOF: boolean = false): TJSONArray;
var
  JArr: TJSONArray;
begin
  JArr := TJSONArray.Create;
  try
    if not Eof then
       DataSetToJSONArray(Self, JArr, false);
    Result := JArr;
  except
    FreeAndNil(JArr);
    raise;
  end;
end;

function TDataSetHelper.AsJSONArrayString(AReturnNilIfEOF: boolean = false): string;
var
  Arr: TJSONArray;
begin
  Arr := AsJSONArray;
  try
    Result := Arr.ToString; // ToJSON;
  finally
    Arr.Free;
  end;
end;

function TDataSetHelper.AsStringList(Separator: Char): TStringList;
var
  Arr: TStringList;
begin
  Arr:=TStringList.Create;
  try
    if not Eof then
       DataSetToStringList(Self, Arr,Separator,false);
    Result := Arr;
  except
    FreeAndNil(Arr);
    raise;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArray(AJSONArray: TJSONArray);
begin
  Self.DisableControls;
  try
    JSONArrayToDataSet(AJSONArray, Self, TArray<string>.Create(), false);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArray(
             AJSONArray: TJSONArray;
             AIgnoredFields: TArray<string>);
begin
  Self.DisableControls;
  try
    JSONArrayToDataSet(AJSONArray, Self, AIgnoredFields, false);
  finally
    Self.EnableControls;
  end;
end;

procedure TDataSetHelper.LoadFromJSONArrayString(
             AJSONArrayString: string;
             AIgnoredFields: TArray<string>);
begin
  AppendFromJSONArrayString(AJSONArrayString, AIgnoredFields);
end;

procedure TDataSetHelper.LoadFromJSONArrayString(
             AJSONArrayString: string);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<String>.Create());
end;

procedure TDataSetHelper.AppendFromJSONArrayString(
             AJSONArrayString: string;
             AIgnoredFields: TArray<string>);
var
  JV: TJSONArray;
begin
  JV := TJSONArray.Create.ParseJSONValue(
           TEncoding.ANSI.GetBytes(AJSONArrayString),0) as TJSONArray;
  try
    if JV.Count>0 then
      LoadFromJSONArray(JV, AIgnoredFields)
    else
      raise Exception.Create
        ('Expected JSONArray in LoadFromJSONArrayString');
  finally
    JV.Free;
  end;
end;

procedure TDataSetHelper.AppendFromJSONArrayString(AJSONArrayString: string);
begin
  AppendFromJSONArrayString(AJSONArrayString, TArray<string>.Create());
end;

end.