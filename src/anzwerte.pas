unit anzwerte;                                     {Additinal chart or table}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, TATools,
  Forms, Controls, Graphics, Dialogs, Grids, Buttons, Menus, LCLIntf, LCLType,
  StdCtrls, q5_common;

type

  {TForm2: Detailanzeige kontextabhängig Diagramme oder Statistiken bei Klick auf Header}

  TForm2 = class(TForm)
    btnOKform2: TBitBtn;
    Chart1: TChart;
    Chart1ConstantLine1: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    edTime: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnGoTo: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    procedure btnOKform2Click(Sender: TObject);
    procedure Chart1DblClick(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure mnGoToClick(Sender: TObject);
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
    procedure DataToMain;

  public
    procedure MoveVCursor(x: double; p: integer);  {Time and label position}

  end;

{$I language.inc}

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.MoveVCursor(x: double; p: integer);  {Move vertical cursor}
var
  lft: integer;

begin
  Chart1ConstantLine1.Position:=x;                 {Red cursor}
  edTime.Text:=FormatDateTime(zzf, x);
  lft:=p*(Chart1.Width-edTime.Width) div 10000+
      (edTime.Width div 2)+Chart1.Left;
  if lft<Chart1.Left then                          {Left/right borders}
    lft:=Chart1.Left;
  if lft+edTime.Width+btnOKform2.Width>Chart1.Width then
    lft:=Chart1.Width-edTime.Width-btnOKform2.Width;
  edTime.Left:=lft;
  Application.ProcessMessages;
end;

procedure TForm2.btnOKform2Click(Sender: TObject);
begin
  timestr:='';
  Close;
end;

procedure TForm2.Chart1DblClick(Sender: TObject);  {Datapoint ID toggle}
begin
  if Chart1.Tag=0 then                             {Only for telemetry}
    Chart1LineSeries1.Pointer.Visible:=not Chart1LineSeries1.Pointer.Visible;
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
  mnGoTo.Caption:=capGoToVal;
  mnGoTo.Enabled:=false;
 {$IFDEF LINUX}                                     {Linux}
    Chart1LineSeries1.LinePen.Color:=clNavy;
  {$ENDIF}
  Chart1ConstantLine1.Active:=false;
  StringGrid1.Tag:=0;
  edTime.Color:=clOrange;                          {Moving label}
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
  Application.ProcessMessages;
end;

procedure TForm2.FormShow(Sender: TObject);        {Doppelklick ID rücksetzen}
begin
  StringGrid1.Tag:=0;
  timestr:='';
  mnGoTo.Enabled:=false;
  edTime.Visible:=false;
end;

procedure TForm2.MenuItem1Click(Sender: TObject);  {Kopieren}
begin
  If StringGrid1.Visible then
    StringGrid1.CopyToClipboard(false)
  else
    Chart1.CopyToClipboardBitmap;
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

procedure TForm2.mnGoToClick(Sender: TObject);     {per Menü Daten übergeben}
begin
  DataToMain;                                      {per Doppelklick Daten übergeben}
end;

procedure TForm2.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  try
    if (ACol=0) or (ACol=1) then begin             {Sortieren für Zahlen}
      Result:=StrToInt(StringGrid1.Cells[ACol, ARow])-
              StrToInt(StringGrid1.Cells[BCol, BRow]);
    end else
      result:=CompareText(StringGrid1.Cells[ACol,ARow],
              StringGrid1.Cells[BCol,BRow]);       {als String sortieren}
  except
    result:=CompareText(StringGrid1.Cells[ACol,ARow],
            StringGrid1.Cells[BCol,BRow]);         {als String sortieren}
  end;
  if StringGrid1.SortOrder=soDescending then
    Result:=-Result;                               {Sortierrichtung}
end;

procedure TForm2.DataToMain;       {Daten in timestr an Hauptformular übergeben}
begin
  if StringGrid1.Visible and
     (StringGrid1.Tag>0) then begin
    if (StringGrid1.ColCount>4) then
      timestr:=StringGrid1.Cells[3, StringGrid1.Tag]
    else
      timestr:=StringGrid1.Cells[0, StringGrid1.Tag];
  end;
end;

procedure TForm2.StringGrid1DblClick(Sender: TObject);  {Zeitstempel auslesen}
begin
  DataToMain;                                      {per Doppelklick Daten übergeben}
end;

procedure TForm2.StringGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                             {Werte kopieren}
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
    mnGoTo.Enabled:=(Row>0);
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
        if ((e and 1)<>0) and
           (aCol=0) then                       {Voltage warning in erster Spalte}
          CellColorSetting(StringGrid1, clVolt1);
        if ((e and 2)<>0) and
           (aCol=0) then                       {Voltage warning in erster Spalte}
          CellColorSetting(StringGrid1, clVolt2);
        if (e shr 2)<>0 then begin             {Compass error überschreibt}
          if aCol=0 then
            CellColorSetting(StringGrid1, clOrange);
          if aCol>2 then begin                 {Zeitspalten}
            e:=StrToIntDef(CleanNum(StringGrid1.Cells[5, aRow]), 0); {Dauer}
            if e>5 then                        {Compass error > 5sec}
              CellColorSetting(StringGrid1, clError);
          end;
        end;
      end;
    end;
    if pos(fmode, StringGrid1.Cells[0, 0])>0 then begin  {Flight Mode}
      e:=StrToIntDef(StringGrid1.Cells[0, aRow], 0); {f_mode as integer}
//      FMcolor(StringGrid1, e, v_type);
    end
  end;
end;

end.

