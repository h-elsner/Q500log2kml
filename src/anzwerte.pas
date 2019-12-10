unit anzwerte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, TATools,
  Forms, Controls, Graphics, Dialogs, Grids, Buttons, Menus, LCLIntf, LCLType,
  StdCtrls, Types;

type

  {TForm2: Detailanzeige kontextabhängig Diagramme oder Statistiken}

  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    Chart1: TChart;
    Chart1ConstantLine1: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    edTime: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    {private declarations}
    procedure DataToMain;
  public
    {public declarations}
    function CleanDN(const s: string): string;     {Ungültige Zeichen entfernen}
    function CleanNum(const s: string): string;    {Ziffern filtern}
    procedure MoveVCursor(x: double; p: integer);  {Time and label position}
    var st: string;
  end;

var
  Form2: TForm2;

const
  InvalidChars: set of char=['\', '/', ':', '*', '?', '"', '<', '>', '|', '&'];

{ Chart1BarSeries1: Series Color:=clFuchsia  (Angle Mode – Purple LED)
  Chart1BarSeries2: Series Color:=clGreen    (für Smart Mode)
  Chart1BarSeries3: Series Color:=clRed      (für RTH)
  Chart1BarSeries4: Series Color:=clMaroon   (Emergency)
  Chart1BarSeries5: Series Color:= $000080FF (Orange)
  Chart1BarSeries7: Series Color:=clBlue     (Sports Mode, Stability)    }

  clOrange=$000080FF;
  clNoGPS=$000080FF;                               {Dark Orange}
  clAngle=clFuchsia;
  clEmergency=clMaroon;
  clSmart=clGreen;
  clRTH=clRed;
  clSport=clBlue;
  clTasks=clMoneyGreen;
  clAttention =$008080F0;                          {Farbe Achtung}
  clVolt2     =$00FF901E;                          {Voltage 2 Farbe}

  csvsep=';';
  spk=4;                                           {Korrekturwert Spaltenbreite}
  ziff=['0'..'9'];                                 {gültige Ziffern}
  zzf='hh:nn:ss';

{.$I q500_dt.inc}
{$I q500_en.inc}

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.MoveVCursor(x: double; p: integer);  {Move vertical cursor}
var lft: integer;
begin
  Chart1ConstantLine1.Position:=x;                 {Red cursor}
  edTime.Text:=FormatDateTime(zzf, x);
  lft:=p*(Chart1.Width-edTime.Width) div 10000+
      (edTime.Width div 2)+Chart1.Left;
  if lft<Chart1.Left then                          {Left/right borders}
    lft:=Chart1.Left;
  if lft+edTime.Width+BitBtn1.Width>Chart1.Width then
    lft:=Chart1.Width-edTime.Width-BitBtn1.Width;
  edTime.Left:=lft;
end;

procedure TForm2.BitBtn1Click(Sender: TObject);
begin
  Close;
  st:='';
end;

procedure TForm2.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart1.ZoomFull;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  MenuItem1.Caption:=rsCopy;
  MenuItem2.Caption:=rsFileSave;
  MenuItem3.Caption:=capGoToVal;
  MenuItem3.Enabled:=false;
  Chart1ConstantLine1.Active:=false;
  StringGrid1.Tag:=0;
  st:='';
end;

procedure TForm2.FormResize(Sender: TObject);      {StringGrid Spalten anpassen}
begin
  if StringGrid1.Visible then begin
    case StringGrid1.ColCount of
      1: StringGrid1.ColWidths[0]:=StringGrid1.Width-spk;
      3: StringGrid1.ColWidths[2]:=StringGrid1.Width-spk-
                                   StringGrid1.ColWidths[0]-
                                   StringGrid1.ColWidths[1];
      6: begin
          StringGrid1.ColWidths[3]:=100;
          StringGrid1.ColWidths[4]:=StringGrid1.ColWidths[3];   {gleiche Breite}
          StringGrid1.ColWidths[5]:=80;
          StringGrid1.ColWidths[2]:=StringGrid1.Width-spk-
                                   StringGrid1.ColWidths[0]-
                                   StringGrid1.ColWidths[1]-
                                   StringGrid1.ColWidths[3]-
                                   StringGrid1.ColWidths[4]-
                                   StringGrid1.ColWidths[5];
         end;
    end;
  end;
end;

procedure TForm2.FormShow(Sender: TObject);        {Doppelklick ID rücksetzen}
begin
  StringGrid1.Tag:=0;
  st:='';
  MenuItem3.Enabled:=false;
  edTime.Visible:=false;
end;

procedure TForm2.MenuItem1Click(Sender: TObject);  {Kopieren}
begin
  If StringGrid1.Visible then
    StringGrid1.CopyToClipboard(false)
  else
    Chart1.CopyToClipboardBitmap;
end;

function TForm2.CleanDN(const s: string): string;  {Ungültige Zeichen entfernen}
var i: integer;
begin
  result:='';
  for i:=1 to length(s) do
    if s[i]=' ' then
      result:=result+'_'
    else
      if not (s[i] in InvalidChars) then
        result:=result+s[i];
end;

function TForm2.CleanNum(const s: string): string;  {Ziffern filtern}
var i: integer;
begin
  result:='';
  for i:=1 to length(s) do
    if s[i] in ziff then
      result:=result+s[i];
end;

procedure TForm2.MenuItem2Click(Sender: TObject);
begin
  SaveDialog1.Title:=rsFileSave;
  if StringGrid1.Visible then begin                {Tabelle als CSV}
    SaveDialog1.FileName:=CleanDN(caption)+'_01.csv';
    if SaveDialog1.Execute then
      StringGrid1.SaveToCSVFile(SaveDialog1.FileName, csvsep, true);
  end else begin                                   {Diagramm}
    SaveDialog1.FileName:=CleanDN(caption)+'_01.png';
    if SaveDialog1.Execute then
      Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
  end;
end;

procedure TForm2.MenuItem3Click(Sender: TObject);  {per Menü Daten übergeben}
begin
  DataToMain;
end;

procedure TForm2.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  try
    if (ACol=0) or (ACol=1) then begin         {Sortieren für Zahlen}
      Result:=StrToInt(StringGrid1.Cells[ACol, ARow])-
              StrToInt(StringGrid1.Cells[BCol, BRow]);
    end else
      result:=CompareText(StringGrid1.Cells[ACol,ARow],
              StringGrid1.Cells[BCol,BRow]);   {als String sortieren}
  except
    result:=CompareText(StringGrid1.Cells[ACol,ARow],
            StringGrid1.Cells[BCol,BRow]);     {als String sortieren}
  end;
  if StringGrid1.SortOrder=soDescending then
    Result:=-Result;                           {Sortierrichtung}
end;

procedure TForm2.DataToMain;          {Daten in st an Hauptformular übergeben}
begin
  if StringGrid1.Visible and
     (StringGrid1.Tag>0) then begin
    if (StringGrid1.ColCount>4) then
      st:=StringGrid1.Cells[3, StringGrid1.Tag]
    else
      st:=StringGrid1.Cells[0, StringGrid1.Tag]
  end;
end;

procedure TForm2.StringGrid1DblClick(Sender: TObject);  {Zeitstempel auslesen}
begin
  DataToMain;                                  {per Doppelklick Daten übergeben}
end;

procedure TForm2.StringGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                         {Werte kopieren}
begin
  if (key=vk_c) and
     (ssCtrl in Shift) then
    StringGrid1.CopyToClipboard(false);
end;

procedure TForm2.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);          {Angeklickte Zeile merken}
var
  Col, Row: Integer;
begin
  if StringGrid1.Visible then begin
    Row:=0;
    StringGrid1.MouseToCell(x, y, Col, Row);
    StringGrid1.Tag:=Row;                      {Zeilennummer in Tag}
    MenuItem3.Enabled:=(Row>0);
  end;
end;

procedure TForm2.StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var e: integer;
begin
  if (aState=[]) and                           {nur wenn Zelle nicht selected}
     (aRow>0) then begin                       {ohne Überschrift}
    if pos('error', StringGrid1.Cells[0, 0])>0 then begin  {Error Flags}
      e:=StrToIntDef(StringGrid1.Cells[0, aRow], 0); {Errorflag suchen}
      if (e>0) and (e<>85) then begin                {Flags vorhanden}
        if (((e and 1)<>0) or ((e and 2)<>0)) and
           (aCol=0) then                       {Voltage warning in erster Spalte}
          StringGrid1.Canvas.Brush.Color:=clSkyBlue;
        if (e shr 2)<>0 then begin             {Compass error überschreibt}
          if aCol=0 then
            StringGrid1.Canvas.Brush.Color:=clOrange;
          if aCol>2 then begin                 {Zeitspalten}
            e:=StrToIntDef(CleanNum(StringGrid1.Cells[5, aRow]), 0); {Dauer}
            if e>5 then                        {Compass error > 5sec}
              StringGrid1.Canvas.Brush.Color:=clRed;
          end;
        end;
      end;
      exit;
    end;
  end;
end;

end.

