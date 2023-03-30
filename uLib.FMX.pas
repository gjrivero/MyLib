unit uLib.FMX;

interface

uses
    FMX.Objects,
    FMX.Graphics,
    FMX.StdCtrls,
    FMX.ExtCtrls;

Type
   TArrayKeys = Array[1..50] Of Word;
   TCoordinate = (cNone, cHorz, cVert);
   TDirection = (dNone, dLeftRight, dRightLeft, dUpDown, dDownUp);

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
                         Coordenada: TCoordinate= cNone;
                         Direccion: TDirection= dNone);
Function KeyActive(KeyOn: TArrayKeys; MaxKey, Key: Word): Boolean;
//procedure FixDBGridColumnsWidth(const DBGrid: TRxDBGrid);
Procedure SetLineInfo(Ok: Boolean; Var Y: Integer; objects: TArray<TObject>; X: Integer=4);

implementation

{%CLASSGROUP 'VCL.Controls.TControl'}

uses
    FMX.Forms,
    FMX.Controls,
    FMX.Dialogs,
    System.UITypes,
    System.Types,
    System.Classes,
    System.StrUtils,
    System.SysUtils,
    System.Generics.Collections;


Procedure SetLineInfo(Ok: Boolean; Var Y: Integer; objects: TArray<TObject>; X: Integer=4);
Const MTAB=2;
      HTAB=2;
var
    I,
    H, YT,
    Tab: Integer;
Begin
  H:=0;
  Tab:=MTAB;
  for I:=Low(objects) to High(objects) do
    If Assigned(objects[I]) Then
      if H<TComponent(Objects[I]).Height then
         H:=TComponent(Objects[I]).Height;
  for I:=Low(objects) to High(objects) do
    If Assigned(objects[I]) Then
    Begin
      TwinControl(objects[I]).Visible:=Ok;
      If Ok then
         Begin
           Case I Of
            0..1: X:=TwinControl(objects[I]).Left; // 0: tlabel 1: Tedit.  Generally!!
            2:    if (Objects[I] Is TImage) then
                     X:=X-Tab;
           End;
           YT:=Y;
           if Not (Objects[I] Is TImage) then
              YT:=Y+((H-TwinControl(objects[I]).Height) div 2);
           TwinControl(Objects[I]).Top:=YT;
           TwinControl(Objects[I]).Left:=X;
           Tab:=MTAB;
           if (Objects[I] Is TImage) then
              Tab:=1;
           X:=X+TwinControl(Objects[I]).Width+Tab;
         End;
    End;
  if Ok then
     Begin
       Inc(Y,H+HTAB);
     End
End;


procedure SetBtnOpt( Sender: TObject; Visible: Boolean; Enabled: Boolean= True);
Var I: Integer;
begin
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
                         Coordenada: TCoordinate= cNone;
                         Direccion: TDirection= dNone);
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

(*
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
*)

end.
