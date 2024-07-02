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
Begin
End;


procedure SetBtnOpt( Sender: TObject; Visible: Boolean; Enabled: Boolean= True);
begin
end;

Procedure SetBtnEnable(     Sender: TObject;
                            Condition: Boolean;
                        Var KeyOn: TArrayKeys;
                        Var MaxKey: Word;
                            AltKey: Boolean;
                            iKey: Integer = 0);
Begin
End;

procedure SetBtnCaption(Btn: TPanel; Const sCaption: String);
begin
end;

procedure SetBtnCaption(Btn: TPanel; Const sCaption: String; img: TImage );
begin
end;

Procedure SetBtnPos(     Sender: TObject;
                         Condicion: Boolean;
                     Var PosY, PosX:   Integer;
                         Inicio: Integer;
                         Tope: Integer;
                         Coordenada: TCoordinate= cNone;
                         Direccion: TDirection= dNone);
Begin
End;

Function KeyActive(KeyOn: TArrayKeys; MaxKey, Key: Word): Boolean;
Begin
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
