unit uLib.Vcl;

interface

uses
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    RxDBCtrl,
    RxToolEdit;

Type
   TArrayKeys = Array[1..50] Of Word;
   TCoordenada = (cNone, cHorz, cVert);
   TDireccion = (dNone, dLeftRight, dRightLeft, dUpDown, dDownUp);

procedure SetBtnOpt( Sender: TObject; Visible: Boolean; Enabled: Boolean= True);
Procedure SetBtnEnable(     Sender: TObject;
                            Condition: Boolean;
                        Var KeyOn: TArrayKeys;
                        Var MaxKey: Word;
                            AltKey: Boolean;
                            iKey: Integer = 0);
procedure SetBtnCaption(Btn: TPanel; Const sCaption: String; img: TImage); Overload;
procedure SetBtnCaption(Btn: TPanel; Const sCaption: String); Overload;

Procedure SetBtnPos(     Sender: TObject;
                         Condicion: Boolean;
                     Var PosY, PosX:   Integer;
                         Inicio: Integer;
                         Tope: Integer;
                         Coordenada: TCoordenada= cNone;
                         Direccion: TDireccion= dNone);
Function KeyActive(KeyOn: TArrayKeys; MaxKey, Key: Word): Boolean;
procedure FixDBGridColumnsWidth(const DBGrid: TRxDBGrid);

implementation

{%CLASSGROUP 'VCL.Controls.TControl'}

uses
    Vcl.Controls,
    Vcl.ComCtrls,
    Vcl.DBGrids,
    Data.DB;


procedure SetBtnOpt( Sender: TObject; Visible: Boolean; Enabled: Boolean= True);
Var I: Integer;
begin
  TWinControl(Sender).Visible:=Visible;
  TWinControl(Sender).Enabled:=Enabled;
  for I := 0 to TWinControl(Sender).ControlCount-1 do
    if (TWinControl(Sender).Controls[I] Is TLabel) Or
       (TWinControl(Sender).Controls[I] Is TImage) then
    Begin
      TWinControl(Sender).Controls[I].Enabled:=Enabled;
      TWinControl(Sender).Controls[I].Tag:=TWinControl(Sender).Tag;
    End;
end;

Procedure SetBtnEnable(     Sender: TObject;
                            Condition: Boolean;
                        Var KeyOn: TArrayKeys;
                        Var MaxKey: Word;
                            AltKey: Boolean;
                            iKey: Integer = 0);
Begin
  Inc(MaxKey);
  if TWinControl(Sender).Visible then
  Begin
    SetBtnOpt( Sender,True,Condition);
    TButton(Sender).Enabled:=condition;
    if Condition then
    Begin
      TWinControl(Sender).Tag := 100*Ord(AltKey)+MaxKey;
      KeyOn[MaxKey] := iKey;
    End;
  End;
End;

procedure SetBtnCaption(Btn: TPanel; Const sCaption: String);
Var
  I: Integer;
begin
  for I := 0 to Btn.ControlCount - 1 do
    If (Btn.Controls[I] Is TLabel) then
    Begin
      TLabel(Btn.Controls[I]).Caption := sCaption;
    End;
end;

procedure SetBtnCaption(Btn: TPanel; Const sCaption: String; img: TImage );
Var
  I: Integer;
begin
  for I := 0 to Btn.ControlCount - 1 do
   begin
     If (Btn.Controls[I] Is TLabel) then
        Begin
          TLabel(Btn.Controls[I]).Caption := sCaption;
        End
     else
        If (Btn.Controls[I] Is TImage) then
           Begin
             TImage(Btn.Controls[I]).Picture.Assign(img.Picture);
           End;
   end;
end;

Procedure SetBtnPos(     Sender: TObject;
                         Condicion: Boolean;
                     Var PosY, PosX:   Integer;
                         Inicio: Integer;
                         Tope: Integer;
                         Coordenada: TCoordenada= cNone;
                         Direccion: TDireccion= dNone);
Const
  TABX=2;
  TABY=2;

Var
   X,Y,
   iSize,
   Signo: Integer;
Begin
  X:=0;
  Y:=0;
  Signo:=0;
  TWinControl(Sender).Visible:=Condicion;
  TWinControl(Sender).Enabled:=False;
  if TWinControl(Sender).Visible then
     begin
       case Direccion of
        dUpDown,
        dLeftRight: Signo:=1;
        dDownUp,
        dRightLeft: Signo:=-1;
       end;
       case Coordenada of
        cVert: begin
                 iSize:=(TWinControl(Sender).Height+TabY);
                 case Direccion of
                  dUpDown: if (PosY+Signo*iSize>Tope) then
                               begin
                                 PosY:=Inicio;
                                 PosX:=PosX-(TWinControl(Sender).Width+TabX);
                               end;
                  dDownUp: if (PosY+Signo*iSize<Tope) then
                               begin
                                 PosY:=Inicio;
                                 PosX:=PosX-(TWinControl(Sender).Width+TabX);
                               end;
                 end;
                 X:=PosX;
                 Y:=PosY;
                 PosY:=PosY+ Signo*iSize;
               end;
        cHorz: begin
                 iSize:=(TWinControl(Sender).Width+TabX);
                 case Direccion of
                  dLeftRight: if (PosX+Signo*iSize>Tope) then
                                  begin
                                    PosX:=Inicio;
                                    PosY:=PosY+(TWinControl(Sender).Height+TabY);
                                  end;
                  dRightLeft: if (PosX+Signo*iSize<Tope) then
                                  begin
                                    PosX:=Inicio;
                                    PosY:=PosY+(TWinControl(Sender).Height+TabY);
                                  end;
                 End;
                 X:=PosX;
                 Y:=PosY;
                 PosX:=PosX+ Signo*iSize;
               end;
       end;
       TWinControl(Sender).Left:=X;
       TWinControl(Sender).Top:=Y;
       SetBtnOpt(Sender,Condicion,false);
     end;
End;

Function KeyActive(KeyOn: TArrayKeys; MaxKey, Key: Word): Boolean;
Var
  Ok: Boolean;
  I: Integer;
Begin
  Ok := False;
  for I := 1 to MaxKey do
    if Key = KeyOn[I] then
    Begin
      Ok := true;
      Break;
    End;
  Result := Ok;
End;

procedure FixDBGridColumnsWidth(const DBGrid: TRxDBGrid);
var
  i : integer;
  TotWidth : integer;
  VarWidth : integer;
  ResizableColumnCount : integer;
  AColumn : TColumn;
begin
  if Not DBGrid.Visible then
     Exit;
  //total width of all columns before resize
  TotWidth := 0;
  //how many columns need to be auto-resized
  ResizableColumnCount := 0;
  //TRxDBColumn(Grd.Columns[L]).Tag
  for i := 0 to -1 + DBGrid.Columns.Count do
  begin
    TotWidth := TotWidth + DBGrid.Columns[i].Width;
    If Assigned(DBGrid.Columns[I]) then
       Inc(ResizableColumnCount);
  end;

  //add 1px for the column separator line
  if dgColLines in DBGrid.Options then
    TotWidth := TotWidth + TDBGrid(DBGrid).Columns.Count;

  //add indicator column width
  if dgIndicator in DBGrid.Options then
    TotWidth := TotWidth + IndicatorWidth;

  //width vale "left"
  VarWidth :=  DBGrid.ClientWidth - TotWidth;

  if ResizableColumnCount > 0 then
    VarWidth := varWidth div ResizableColumnCount;

  for i := 0 to -1 + DBGrid.Columns.Count do
  begin
    AColumn := DBGrid.Columns[i];
    if Assigned(AColumn) Then
       begin
         AColumn.Width := AColumn.Width + VarWidth;
         {if AColumn.Width<AColumn.Tag then
            AColumn.Width:=AColumn.Tag;}
       end;
  end;
end;

end.
