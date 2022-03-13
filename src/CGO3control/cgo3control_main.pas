          {********************************************************}
          {                                                        }
          {     Auswertung FlightLog Daten von Yuneec Koptern      }
          {                                                        }
          {       Copyright (c) 2015-2020    Helmut Elsner         }
          {                                                        }
          {       Compiler: FPC 3.0.4   /    Lazarus 2.0.8         }
          {                                                        }
          { Pascal programmers tend to plan ahead, they think      }
          { before they type. We type a lot because of Pascal      }
          { verboseness, but usually our code is right from the    }
          { start. We end up typing less because we fix less bugs. }
          {           [Jorge Aldo G. de F. Junior]                 }
          {********************************************************}

(*
  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

================================================================================

Ausgliederung CGO3-Steuerung aus q500log2kml
*)

unit CGO3control_main;

{$mode objfpc}{$H+}
{$modeswitch DuplicateLocals}

interface

uses
  Classes, SysUtils, FileUtil, LCLType,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, EditBtn, XMLPropStorage, Grids, Menus, lclintf, StdCtrls, Spin,
  Clipbrd, indGnouMeter, AdvLed, Sensors, fphttpclient, lazUTF8,
  strutils, dateutils, c3_common, fpeMetaData, fpeExifData, exifstuff;

type

  {TForm1: Main program}

  TForm1 = class(TForm)
    AdvLed1: TAdvLed;
    btnAudio: TBitBtn;
    btnCGO3Reset: TBitBtn;
    btnCGO3Status: TBitBtn;
    btnCGO3Time: TBitBtn;
    btnClose: TBitBtn;
    btnClose1: TBitBtn;
    btnFormatSD: TBitBtn;
    btnFoto: TBitBtn;
    btnScanPic: TBitBtn;
    btnVideoStart: TBitBtn;
    btnVideoStop: TBitBtn;
    btnWiFiSpeedReset: TBitBtn;
    btnWiFiSpeedUp: TBitBtn;
    btnWritePic: TBitBtn;
    cbBackupPic: TCheckBox;
    cbExpoAuto: TCheckBox;
    cbFileList: TCheckBox;
    cbRTSP: TCheckBox;
    cbxCGO3Color: TComboBox;
    cbxCGO3ISO: TComboBox;
    cbxCGO3Shutter: TComboBox;
    cbxCGO3Video: TComboBox;
    cbxCGO3WB: TComboBox;
    cbxPicFolder: TComboBox;
    cbxTelemetry: TComboBox;
    cgpCamera: TCheckGroup;
    edCGOURL: TEdit;
    edReceiveCGO3: TEdit;
    edSendCGO3: TEdit;
    gbCGO3Status: TGroupBox;
    gbVideoRecord: TGroupBox;
    gbVideoSettings: TGroupBox;
    gridCGO3: TStringGrid;
    gridEXIFPic: TStringGrid;
    gridTimeArea: TStringGrid;
    gbSettings: TGroupBox;
    GroupBox9: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    indGnouMeterSDused: TindGnouMeter;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    lblPicFolder: TLabel;
    lblTelemetry: TLabel;
    lblTimeOffset: TLabel;
    N1: TMenuItem;
    mnGeoGMap: TMenuItem;
    mnGeoOSM: TMenuItem;
    mnShowPic: TMenuItem;
    pcMain: TPageControl;
    PopupMenuGeo: TPopupMenu;
    rgPicFormat: TRadioGroup;
    rgVideoFoto: TRadioGroup;
    sbtnPicFolder: TSpeedButton;
    sbtnSendCGO3: TSpeedButton;
    sbtnTelemetry: TSpeedButton;
    speExpo: TFloatSpinEdit;
    speTimeOffset: TSpinEdit;
    StopLightSensor1: TStopLightSensor;
    tabCGO3: TTabSheet;
    tabGeo: TTabSheet;
    TabImages: TImageList;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    tbrSharpness: TTrackBar;
    tmCGOstatus: TTimer;                                {CGO3 Statusabfrage}
    XMLPropStorage1: TXMLPropStorage;

    procedure btnScanPicClick(Sender: TObject);
    procedure btnCGO3StatusClick(Sender: TObject);
    procedure btnVideoStartClick(Sender: TObject);
    procedure btnVideoStopClick(Sender: TObject);
    procedure btnCGO3ResetClick(Sender: TObject);
    procedure btnWiFiSpeedUpClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnWiFiSpeedResetClick(Sender: TObject);
    procedure btnAudioClick(Sender: TObject);
    procedure btnFotoClick(Sender: TObject);
    procedure btnCGO3TimeClick(Sender: TObject);
    procedure btnFormatSDClick(Sender: TObject);
    procedure btnWritePicClick(Sender: TObject);
    procedure cbxPicFolderChange(Sender: TObject);
    procedure cbxPicFolderDblClick(Sender: TObject);
    procedure cbxTelemetryChange(Sender: TObject);
    procedure cbxTelemetryDblClick(Sender: TObject);
    procedure cbExpoAutoChange(Sender: TObject);
    procedure cbxCGO3VideoChange(Sender: TObject);
    procedure cbxCGO3ColorChange(Sender: TObject);
    procedure cbxCGO3WBChange(Sender: TObject);
    procedure cbxCGO3ISOChange(Sender: TObject);
    procedure cbxCGO3ShutterChange(Sender: TObject);
    procedure edSendCGO3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edReceiveCGO3DblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure gridEXIFPicDblClick(Sender: TObject);
    procedure gridEXIFPicPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure gridTimeAreaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnGeoGMapClick(Sender: TObject);
    procedure mnGeoOSMClick(Sender: TObject);
    procedure mnShowPicClick(Sender: TObject);
    procedure sbtnPicFolderClick(Sender: TObject);
    procedure sbtnTelemetryClick(Sender: TObject);
    procedure speExpoChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure rgVideoFotoClick(Sender: TObject);
    procedure rgPicFormatClick(Sender: TObject);
    procedure sbtnSendCGO3Click(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure StatusBar1Hint(Sender: TObject);
    procedure gridCGO3KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tmCGOstatusTimer(Sender: TObject);
    procedure tbrSharpnessClick(Sender: TObject);

  private
    function CGO3run(const CGO3cmd: string; const CGO3act: integer): integer; {CGO3 ansprechen}
    procedure StatusToClipboard;                   {Statuszeile kopieren}
    function ZeitToDT(const s: string): TDateTime;
    function FindTP(wlist: TStringList; tp: TDateTime): integer;
    procedure SendCGOcmd;                          {Command zu CGO3}
    procedure Sharpness;                           {SHARNESS ermitteln}
    procedure GeoShowPic;                          {Menu Show picture}
    procedure ScanPicEnable;                       {Enable picture scanning for geotagging}
    procedure ScanPic;                             {Geotagging: Scan picture folder}

  end;


const
  hpmydat='/pdf/';
  meinname='Helmut Elsner';
  email   ='helmut.elsner@live.com';               {My e-mail address}

  {http://docwiki.embarcadero.com/RADStudio/Seattle/de/Farben_in_der_VCL}
  osmURL='https://www.openstreetmap.org/';         {als Karte}

  numItems=10;                                     {Anzahl Items zu merken in drop-down list}
  gzoom='16';

  wldcd='*';
  us1='_1';
  pngdef=us1+'.png';                               {Dateivorschläge}
  csvdef=us1+fext;
  wexdef=us1+wext;
  sep=',';                                         {Datenseperator im CSV file}
  kma=', ';                                        {Kommaausgabe}
  anzsp=20;                                        {Mindestanzahl Spalten}
  lzyu=21;                                         {Länge Zeitstempel Yuneec}
  fw0=80;                                          {Breite linke Spalte Übersicht default}
  fw1=160;                                         {Breite linke Spalte Übersicht bei alter FW}
  hsw=2;                                           {Höhen-Schwellwert für Analyse in m}
  minlines=10;                                     {Minimum number if lines that makes sense to deal with}
  exID=' ';                                        {ID for values updated by in StringGrid}

var
  Form1: TForm1;

implementation

{$R *.lfm}
                                                   {TForm1: Hauptfenster}

procedure Merkliste(ml: TComboBox; maxAnzahl: integer);
begin                                              {DropDownListe füllen}
  if (ml.Text<>'') and
     (ml.Items.IndexOf(ml.Text)<0) then            {noch nicht in Liste}
    ml.Items.Insert(0, ml.Text);
  if ml.Items.Count>MaxAnzahl then                 {Anzahl in Liste begrenzen}
    ml.Items.Delete(MaxAnzahl);
end;

procedure TForm1.FormCreate(Sender: TObject);      {Anwendung initialisieren und starten}
begin
  Randomize;                                       {Zufallsgenerator initialisieren}
  Caption:=capForm1+tab2+AppVersion;               {Name und Version}
  Hint:=capForm1;
  btnClose.Caption:=capBitBtn1;
  btnClose.Hint:=hntBitBtn1;
  btnCGO3Reset.Caption:=capBitBtn18;
  btnCGO3Reset.Hint:=hntBitBtn18;
  btnCGO3Time.Caption:=capBitBtn23;
  btnCGO3Time.Hint:=hntBitBtn23;
  btnFormatSD.Caption:=capBitBtn24;
  btnFormatSD.Hint:=hntBitBtn24;
  rgVideoFoto.Caption:=rsMode;
  rgVideoFoto.Hint:=rsMode+' ('+rsVdo+'/'+capBitBtn22+')';
  rgVideoFoto.Items[1]:=capBitBtn22;
  rgVideoFoto.ItemIndex:=0;
  rgPicFormat.Caption:=capRadioGroup2;
  rgPicFormat.Hint:=capRadioGroup2;
  StatusBar1.Hint:=hntStatus1;
  tabCGO3.Hint:=hntTabSheet11;
  tbrSharpness.Hint:=hntSharpness;
  Label21.Caption:=rsWB;
  Label21.Hint:=rsWB;
  cbxCGO3WB.Text:=rsWB;
  cbxCGO3WB.Hint:=rsWB;
  Label25.Caption:=rsShutter;
  Label25.Hint:=rsShutter;
  cbxCGO3Shutter.Text:=rsShutter;
  cbxCGO3Shutter.Hint:=rsShutter;
  Label24.Caption:=rsISO;
  Label24.Hint:=rsISO;
  cbxCGO3ISO.Text:=rsISO;
  cbxCGO3ISO.Hint:=rsISO;
  cbRTSP.Hint:=hntCheckBox2;
  cbFileList.Caption:=capCheckBox4;
  cbFileList.Hint:=hntCheckBox4;
  gbCGO3Status.Caption:=rsCGOstat;
  gbCGO3Status.Hint:=rsCGOstat;
  gbVideoRecord.Caption:=capGroupBox7;
  gbVideoRecord.Hint:=capGroupBox7;
  gbVideoSettings.Caption:=capTabSheet12;
  gbVideoSettings.Hint:=capTabSheet12;
  GroupBox9.Caption:=capGroupBox9;
  GroupBox9.Hint:=hntEdit3;
  Tag:=ord(DefaultFormatSettings.DecimalSeparator); {Original zwischenspeichern}
  DefaultFormatSettings.DecimalSeparator:='.';
  AdvLed1.State:=lsDisabled;
  AdvLed1.Hint:=hntRec;
  Image2.Hint:=rsVdo;
  Image3.Hint:=capBitBtn22;
  edCGOURL.Hint:=hntCGO3URL;
  Label19.Hint:=hntCGO3URL;
  Label19.Caption:=rsCGO3URL;
  Label16.Caption:=rsExpo;
  Label16.Hint:=rsExpo;
  speExpo.Hint:=rsExpo;
  cbExpoAuto.Hint:=rsExpo;
  btnCGO3Status.Hint:=hntStatus;
  StopLightSensor1.Hint:=hntWLAN;
  Label18.Hint:=hntWLAN;
  Label22.Caption:=rsVideo;
  Label22.Hint:=rsVideo;
  cbxCGO3Video.Text:=rsVideo;
  cbxCGO3Video.Hint:=rsVideo;
  Label20.Hint:=capRadioGroup2;
  Label23.Caption:=rsFarbFormat;
  Label23.Hint:=rsFarbFormat;
  cbxCGO3Color.Text:=rsFarbFormat;
  cbxCGO3Color.Hint:=rsFarbFormat;
  indGnouMeterSDused.Caption:=capGnou;             {SD card usage}
  indGnouMeterSDused.Hint:=hntGnou;
  gridCGO3.Hint:=hntStringGrid3;
  gridCGO3.Cells[0,0]:='Firmware';
  gridCGO3.Cells[0,1]:=rsWLANSpeed;
  gridCGO3.Cells[0,2]:=rsKStatus;
  gridCGO3.Cells[0,3]:=rsRecTime;
  gridCGO3.Cells[0,4]:='AWB lock';
  gridCGO3.Cells[0,5]:='AE enable';
  gridCGO3.Cells[0,6]:=rsShutter;
  gridCGO3.Cells[0,7]:=rsSharpness;
  btnVideoStart.Hint:=hntBitBtn16;
  btnVideoStop.Hint:=hntBitBtn17;
  btnWiFiSpeedUp.Caption:=capBitBtn19;
  btnWiFiSpeedUp.Hint:=hntBitBtn19;
  btnWiFiSpeedReset.Caption:=capBitBtn20;
  btnWiFiSpeedReset.Hint:=hntBitBtn20;
  btnFoto.Caption:=capBitBtn22;
  btnFoto.Hint:=hntBitBtn22;
  Image1.Hint:=btnAudio.Caption;
  sbtnSendCGO3.Hint:=hntSpeed6;
  edSendCGO3.Hint:=hntEdit3;
  edReceiveCGO3.Hint:=hntEdit4;
  edSendCGO3.TextHint:=hntEdit3;
  edReceiveCGO3.TextHint:=hntEdit4;

{GeoTagging}
  lblPicFolder.Caption:=capPicFolder;
  lblPicFolder.Hint:=hntPicFolder;
  cbxPicFolder.Hint:=hntPicFolder;
  sbtnPicFolder.Hint:=hntPicFolder;
  sbtnPicFolder.Height:=cbxPicFolder.Height+2;
  sbtnPicFolder.Width:=sbtnPicFolder.Height;
  lblTelemetry.Caption:=capTelemetry;
  lblTelemetry.Hint:=hntTelemetry;
  cbxTelemetry.Hint:=hntTelemetry;
  sbtnTelemetry.Hint:=hntTelemetry;
  sbtnTelemetry.Height:=cbxTelemetry.Height+2;
  sbtnTelemetry.Width:=sbtnTelemetry.Height;
  btnScanPic.Caption:=capScanPic;
  btnScanPic.Hint:=hntScanPic;
  btnWritePic.Caption:=capWritePic;
  btnWritePic.Hint:=hntWritePic;
  gridTimeArea.Cells[0, 0]:=capScanPic;
  gridTimeArea.Cells[1, 0]:=rsGridCell2;
  gridTimeArea.Cells[2, 0]:=rsGridCell3;
  gridTimeArea.Cells[0, 1]:=capPicFolder;
  gridTimeArea.Cells[0, 2]:=capTelemetry;
  cbBackupPic.Caption:=capBackupPic;
  cbBackupPic.Hint:=hntBackupPic;
  lblTimeOffset.Caption:=capTimeOffset;
  lblTimeOffset.Hint:=hntTimeOffset;
  speTimeOffset.Hint:=hntTimeOffset;
  gridEXIFPic.Rows[0].Delimiter:=sep;
  gridEXIFPic.Rows[0].StrictDelimiter:=true;
  gridEXIFPic.Rows[0].DelimitedText:=hdrEXIFPic;
  gridEXIFPic.AutoSizeColumns;
  cgpCamera.Caption:=capCams;
  cgpCamera.Hint:=hntCams;
  mnGeoOSM.Caption:=rsToOSM;
  mnShowPic.Caption:=capShowPic;
  mnGeoGMap.Caption:=rsToGMaps;
end;

{Zeitstempel to TDateTime; Format abhängig vom Vehicle Type
 legacy Yuneec: 20151206 11:32:57:234
 }
function TForm1.ZeitToDT(const s: string): TDateTime;
begin
  try
    result:=ScanDateTime('yyyymmdd '+zzf+':zzz', s); {Yuneec legacy format}
  except
    result:=0;
  end;
end;

{Finde einen Zeitstempel in den Dateien mit Datum/Zeit in der 1. Spalte}
function TForm1.FindTP(wlist: TStringList;         {Data list as strings}
                       tp: TDateTime): integer;    {Time to looking for}
var k, pos, len: integer;
    s: string;

begin
  result:=0;                                       {nothing found}
  pos:=1;
  len:=lzyu;                                       {21: length DT Yuneec}
  for k:=pos to wlist.Count-1 do begin
    s:=copy(wlist[k], 1, len);
    if ZeitToDT(s)>=tp then begin
      result:=k;
      break;
    end;
  end;
end;

{http://www.joerg-buchwitz.de/temp/googlemapssyntax.htm
 https://www.google.de/maps?q=48.235367,10.0922553&z=13&om=0

Mit Beschriftung:
https://www.google.de/maps/place/Weihers+17,+88161+Lindenberg/@47.6087465,9.9204697,17z
}

function URLGMap(lati, long: string): string;      {URL für Koordinate in Google Maps}
begin
  result:=gmapURL+'?q='+ChrKoor(lati)+sep+
                        ChrKoor(long)+'&z='+
                        gzoom+'&t=h&om=0';         {&t=k: Sat, &t=h: hybrid}
end;

{ http://wiki.openstreetmap.org/wiki/Browsing
 https://www.openstreetmap.org/?mlat=49.9676&mlon=9.9673#map=10/49.9676/9.9673&layers=Q}

function URLosm(lati, long: string): string;       {URL für Koordinate in OpenStreetMap}
begin
  result:=osmURL+'?mlat='+lati+'&mlon='+long+'#map='+
          gzoom+'/'+lati+'/'+long+'&layers=S';
end;

procedure TForm1.btnCloseClick(Sender: TObject);   {Button beenden}
begin
  Close;
end;


procedure TForm1.cbxPicFolderChange(Sender: TObject); {New directory for pictures}
begin
  ScanPicEnable;
end;

procedure TForm1.ScanPicEnable;
begin
  if DirectoryExists(cbxPicFolder.Text) and
     FileExists(cbxTelemetry.Text) then
    btnScanPic.Enabled:=true
  else
    btnScanPic.Enabled:=false;
end;

procedure TForm1.cbxPicFolderDblClick(Sender: TObject);
begin
  if cbxPicFolder.Text<>'' then
    OpenDocument(IncludeTrailingPathDelimiter(cbxPicFolder.Text));
end;

procedure TForm1.cbxTelemetryChange(Sender: TObject);
begin
  ScanPicEnable;
end;

procedure TForm1.cbxTelemetryDblClick(Sender: TObject);
begin
  if cbxTelemetry.Text<>'' then
    OpenDocument(ExtractFilePath(cbxTelemetry.Text));
end;


function GetCGOStr(w, s: string): string;          {Werte auslesen aus Kamera}
var p, x: integer;
    hs: string;
    f: boolean;

begin
  result:='';                                      {Fehler}
  hs:='';
  f:=false;
  p:=pos(w, s);
  if p>0 then begin
    for x:=p+w.length-1 to s.length do begin
      if s[x]=sep then
        break;                                     {bis zum Datenseperator (,)}
      if f then
        hs:=hs+s[x];
      if s[x]=':' then
        f:=true;
    end;
    hs:=StringReplace(hs, '"', '', [rfReplaceAll]);
    hs:=StringReplace(hs, '}', '', [rfReplaceAll]);
    result:=trim(hs);
  end;
end;

procedure TForm1.Sharpness;                        {SHARNESS ermitteln}
var ff: string;

begin
  CGO3run(getshpn, 0);                             {SHARPNESS abfragen}
  ff:=GetCGOStr('sharpness', edReceiveCGO3.Text);  {Sharpness nur aus GET_SHARPNESS}
  gridCGO3.Cells[1, 7]:=ff;                        {in Tabelle eintragen}
  tbrSharpness.Position:=StrToIntDef(ff, 6);       {Anzeige setzen}
end;

{Ausführen eines CGI-Kommandos für die CGO3(+). Wenn CGO3cmd leer ist, wird
 INDEX_PAGE aufgerufen und alle Anzeigen werden aktualisiert. SHARPNESS muss
 extra abgefragt werden, weil es in CGO3+ nicht mehr in INDEX_PAGE/GET_STATUS
 drin ist - siehe Procedure Sharpness.
 CGO3act: 0..nichts zusätzliches tun
          1..CheckBox4 abfragen und ggf. Dateiliste im Browser anzeigen,
             nur bei Set Speed.
          2..CheckBox5 abfragen und ggf. RTSP-Stream an Browser senden,
             nur bei Start Video.
          3..INDEX_PAGE mit Ausgabe an AppLogHighlighter, nur beim Start Button. }

function TForm1.CGO3run(const CGO3cmd: string; const CGO3act: integer): integer;
Var s, ff: string;
    sdsize, w: integer;

begin
  Image1.Visible:=false;
  Image2.Visible:=false;
  Image3.Visible:=false;
  if not tmCGOstatus.Enabled then Screen.Cursor:=crHourGlass;
  with TFPHttpClient.Create(Nil) do try
    IOTimeout:=6000;                               {Timeout if cam disconnected}
    try
      if CGO3cmd<>'' then begin                    {opt. Kommando ausführen}
        s:=Get(edCGOURL.Text+CGO3cgi+CGO3cmd);
        result:=StrToIntDef(GetCGOStr('rval', s), -1); {Returnwert überschreiben}
        StatusBar1.Panels[2].Text:=s;
        edReceiveCGO3.Text:=s;                     {Ergebnis unten anzeigen}
      end else begin                               {Initialisierung und Werte ausgeben}
        s:=Get(edCGOURL.Text+CGO3cgi+idxpage);
        s:=StringReplace(s, '}', sep, [rfReplaceAll]);
        result:=StrToIntDef(GetCGOStr('rval', s), -1);        {Returnwert Init}

        if result=0 then begin                     {Werte abfragen und anzeigen}
          sdsize:=StrToIntDef(GetCGOStr('sdtotal', s), 0);    {sdtotal abfragen}
          indGnouMeterSDused.ValueMax:=sdsize/1048576;
          indGnouMeterSDused.Value:=(sdsize-StrToIntDef(GetCGOStr('sdfree', s), 0))/1048576;
          cbxCGO3Video.Text:=GetCGOStr('video_mode', s);
          try
            w:=StrToInt(GetCGOStr('iq_type', s));
            cbxCGO3Color.Text:=cbxCGO3Color.Items[w];
          except
            cbxCGO3Color.Text:='';
          end;
          try
            w:=StrToInt(GetCGOStr('white_balance', s));
            case w of
              0: cbxCGO3WB.Text:=cbxCGO3WB.Items[0];
              1: cbxCGO3WB.Text:=cbxCGO3WB.Items[5];
              3: cbxCGO3WB.Text:=cbxCGO3WB.Items[6]; {Sunset}
              4: cbxCGO3WB.Text:=cbxCGO3WB.Items[2]; {Sunny}
              5: cbxCGO3WB.Text:=cbxCGO3WB.Items[3];
              7: cbxCGO3WB.Text:=cbxCGO3WB.Items[4];
             99: cbxCGO3WB.Text:=cbxCGO3WB.Items[1]; {Lock}
            end;
            If cbxCGO3WB.Text='Auto' then
              Label21.Caption:=rsWB+' (AWB)'
            else
              Label21.Caption:=rsWB;
          except
            cbxCGO3WB.Text:='';
          end;
          gridCGO3.Cells[1, 0]:=GetCGOStr('fw_ver', s);
          gridCGO3.Cells[1, 1]:=GetCGOStr('speed_rate', s);
          gridCGO3.Cells[1, 2]:=GetCGOStr('status', s);
          gridCGO3.Cells[1, 3]:=GetCGOStr('record_time', s);
          gridCGO3.Cells[1, 4]:=GetCGOStr('awb_lock', s);;
          gridCGO3.Cells[1, 5]:=GetCGOStr('ae_enable', s);;
          cbxCGO3ISO.Text:=GetCGOStr('iso_value', s);
          ff:=GetCGOStr('shutter_time', s);
          gridCGO3.Cells[1, 6]:='1/'+ff;
          cbxCGO3Shutter.Text:=ff;
          ff:=GetCGOStr('photo_format', s);
          Label20.Caption:=ff;

          w:=-1;
          if (ff='dng') or (ff='raw') then begin
            w:=0;
            tbrSharpness.Enabled:=false;           {Sharpness for jpg}
          end;
          if ff='jpg' then begin
            w:=1;
            tbrSharpness.Enabled:=true;            {Sharpness for jpg}
          end;

          if ff='dng+jpg' then begin               {nur für CGO3+}
            w:=2;
            tbrSharpness.Enabled:=true;            {Sharpness for jpg}
          end;

          rgPicFormat.ItemIndex:=w;
          cbExpoAuto.Checked:=(GetCGOStr('ae_enable', s)='1');
          speExpo.Enabled:=cbExpoAuto.Checked;
          cbxCGO3ISO.Enabled:=not cbExpoAuto.Checked;
          cbxCGO3Shutter.Enabled:=cbxCGO3ISO.Enabled;
          try
            speExpo.Value:=StrToFloat(GetCGOStr('exposure_value', s));
          except
            speExpo.Value:=0;
          end;

          if GetCGOStr('cam_mode', s)='2' then begin
            Image2.Visible:=false;                 {Filmbildchen}
            Image3.Visible:=true;                  {Kamerabildchen}
          end else begin
            Image2.Visible:=true;
            Image3.Visible:=false;
          end;

          btnAudio.Tag:=0;
          if GetCGOStr('audio_sw', s)='0' then begin
            btnAudio.Tag:=1;
            Image1.Visible:=true;
          end else
            Image1.Visible:=false;

          if pos('"record"', s)>0 then begin
            AdvLed1.State:=lsOn;                   {record läuft}
            AdvLed1.Blink:=true;
            tmCGOstatus.Enabled:=true;     {Wartezeit für Stoppen der Aufnahme}
          end else begin
            AdvLed1.State:=lsOff;
            AdvLed1.Blink:=false;
            if tmCGOstatus.Tag=1 then tmCGOstatus.Enabled:=false;
          end;

          btnVideoStart.Enabled:=true; {Kommandos nur nach erfolgreicher Intialisierung}
          btnVideoStop.Enabled:=true;
          btnCGO3Reset.Enabled:=true;
          btnWiFiSpeedUp.Enabled:=true;
          btnWiFiSpeedReset.Enabled:=true;
          btnAudio.Enabled:=true;
          btnFoto.Enabled:=true;
          btnCGO3Time.Enabled:=true;
          btnFormatSD.Enabled:=true;
          rgVideoFoto.Enabled:=true;
          rgPicFormat.Enabled:=true;
          sbtnSendCGO3.Enabled:=true;
          StopLightSensor1.State:=slGREEN;         {WLAN Ampel auf Grün}
          StatusBar1.Panels[2].Text:=rsCGOdone+tab1+edCGOURL.Text;
        end else begin                             {Result <> 0}
          indGnouMeterSDused.ValueMax:=64;
          indGnouMeterSDused.Value:=0;
          StopLightSensor1.State:=slRED;
          StatusBar1.Panels[2].Text:=s;
        end;
      end; {Ende Initalisierung}
      case CGO3act of
        1: if cbFileList.Checked then
             OpenURL(edCGOURL.Text+CGO3dir);       {Datenverzeichnis im Browser öffnen}
        2: If cbRTSP.Checked then                  {Livestream anzeigen}
             OpenURL(StringReplace(edCGOURL.Text, 'http', 'rtsp',[rfIgnoreCase])+'live');
      end;
    except
      StatusBar1.Panels[2].Text:=rsTimeOut;
      result:=-1;                                  {Fehler, Timeout}
      StopLightSensor1.State:=slRED;
    end;
  finally
    Screen.Cursor:=crDefault;
    Free;
  end;
end;

procedure TForm1.btnCGO3StatusClick(Sender: TObject); {Status}
begin
  StatusBar1.Panels[2].Text:=rsCGOwait;
  StatusBar1.Update;
  StopLightSensor1.State:=slYELLOW;
  StopLightSensor1.Update;
  if CGO3run('', 3)=0 then begin                   {CGO3act=3 mit Protokollausgabe}
    AdvLed1.Visible:=true;                         {nur bei Initialisieren}
    CGO3run('GET_FW_VERSION', 0);                  {FW abfragen}
{Sharpness einmal beim Initialisieren abfragen,
 weil der Parameter bei der CGO3+ nicht mehr in INDEX_PAGE steht}
    Sharpness;
  end else
    AdvLed1.Visible:=false;
end;

procedure TForm1.btnVideoStartClick(Sender: TObject);   {Record start}
begin
  CGO3run('START_RECORD', 2);                      {CGO3act=2 RTSP abfragen}
  tmCGOstatus.Enabled:=true;   {laufende Statusabfrage, um Zeit zu aktualisieren}
  tmCGOstatus.Tag:=0;
end;

procedure TForm1.btnVideoStopClick(Sender: TObject);   {Record stop}
begin
  CGO3run('STOP_RECORD', 0);
  tmCGOstatus.Tag:=1;                                   {Stop vorbereiten}
end;

procedure TForm1.btnCGO3ResetClick(Sender: TObject);   {Reset default}
begin
  CGO3run('RESET_DEFAULT', 0);
  CGO3run('', 0);
end;

procedure TForm1.btnWiFiSpeedUpClick(Sender: TObject);   {Set speed}
begin
  CGO3run('SET_WIFI_SPEED&speed_rate=9', 1);       {CGO3act=2 Dateianzeige abfragen}
  CGO3run('', 0);                                  {INDEX_PAGE aufrufen}
end;

procedure TForm1.btnWiFiSpeedResetClick(Sender: TObject);
begin
  CGO3run('SET_WIFI_SPEED&speed_rate=1', 0);       {reset Speed}
  CGO3run('', 0);
end;

procedure TForm1.cbxCGO3VideoChange(Sender: TObject); {Videoformat setzen}
begin
  if btnCGO3Time.Enabled then begin
    CGO3run('SET_VIDEO_MODE&mode='+cbxCGO3Video.Text, 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.cbxCGO3ColorChange(Sender: TObject); {IQ Type}
begin
  if btnCGO3Time.Enabled then begin
    CGO3run('SET_IQ_TYPE&mode='+IntToStr(cbxCGO3Color.ItemIndex), 0);
    CGO3run('', 0);                                {INDEX_PAGE abfragen}
  end;
end;

procedure TForm1.tbrSharpnessClick(Sender: TObject);
begin
  if tbrSharpness.Enabled then begin
    CGO3run('SET_SHARPNESS&value='+IntToStr(tbrSharpness.Position), 0);
    Sharpness;             {SHARPNESS abfragen, da nicht mehr in INDEX_PAGE}
  end;
end;

procedure TForm1.cbxCGO3WBChange(Sender: TObject); {WB}
var s: string;

begin
  if btnCGO3Time.Enabled then begin
    s:='0';
    case cbxCGO3WB.ItemIndex of
      1: s:='99';
      2: s:='4';
      3: s:='5';
      4: s:='7';
      5: s:='1';
      6: s:='3';
    end;
    CGO3run('SET_WHITEBLANCE_MODE&mode='+s, 0);    {seltsames Kommando}
    CGO3run('', 0);
  end;
end;

procedure TForm1.cbxCGO3ISOChange(Sender: TObject); {ISO}
begin
  if btnCGO3Time.Enabled then begin
    CGO3run('SET_SH_TM_ISO&time='+cbxCGO3Shutter.Text+'&value='+cbxCGO3ISO.Text, 0);
    Sleep(500);
    CGO3run('', 0);
  end;
end;

procedure TForm1.cbxCGO3ShutterChange(Sender: TObject); {Shutter}
begin
  if btnCGO3Time.Enabled then begin
    CGO3run('SET_SH_TM_ISO&time='+cbxCGO3Shutter.Text+'&value='+cbxCGO3ISO.Text, 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.edSendCGO3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
                                                   {Kommando absenden bei Enter}
begin
  if sbtnSendCGO3.Enabled and
     (key=VK_RETURN) then
    SendCGOcmd;                                    {Command zu CGO3}
  if key=VK_ESCAPE then
    edSendCGO3.Text:='';                           {Kommando löschen}
end;

procedure TForm1.btnAudioClick(Sender: TObject);   {Audio Switch}
begin
  if btnAudio.Tag=1 then
    CGO3run('SET_AUDIO_SW&mode=1', 0)              {reset Audio}
  else
    CGO3run('SET_AUDIO_SW&mode=0', 0);             {set Audio}
  CGO3run('', 0);
end;

procedure TForm1.btnFotoClick(Sender: TObject);
begin
  CGO3run('TAKE_PHOTO', 0);                        {Photo shot}
end;

procedure TForm1.btnCGO3TimeClick(Sender: TObject);   {Kamera Zeit setzen}
begin
  CGO3run('SET_TIME&time='+FormatDateTime(dzf+'_'+zzf, now), 0);
end;

procedure TForm1.btnFormatSDClick(Sender: TObject);   {SD-Karte formatieren}
begin
  CGO3run('FORMAT_CARD', 0);
  Sleep(2000);
  CGO3run('', 0);
end;


procedure TForm1.rgVideoFotoClick(Sender: TObject); {Cam mode}
begin
  if rgVideoFoto.Enabled then begin
    if rgVideoFoto.ItemIndex=0 then
      CGO3run('SET_CAM_MODE&mode=video', 0)
    else
      CGO3run('SET_CAM_MODE&mode=photo', 0);
    Sleep(800);
    CGO3run('', 0);
  end;
end;

procedure TForm1.cbExpoAutoChange(Sender: TObject); {Autoexposure}
var s: string;
begin
  speExpo.Enabled:=cbExpoAuto.Checked;
  if btnCGO3Time.Enabled then begin
    if cbExpoAuto.Checked then
      s:='1'
    else
      s:='0';
    CGO3run('SET_AE_ENABLE&mode='+s, 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.rgPicFormatClick(Sender: TObject);
begin
  if (rgPicFormat.Enabled) and
     (rgPicFormat.ItemIndex>=0) then begin
    CGO3run('SET_PHOTO_FORMAT&format='+rgPicFormat.Items[rgPicFormat.ItemIndex], 0);
    CGO3run('', 0);
  end;
end;


procedure TForm1.speExpoChange(Sender: TObject);  {Set EV}
begin
  if btnCGO3Time.Enabled then begin
    CGO3run('SET_EXPOSURE_VALUE&mode='+
            FormatFloat(dzfl, speExpo.Value), 0);
    CGO3run('', 0);
  end;
end;

(*function nahe(lat1, lon1, lat2, lon2: double): boolean; {Wert in der Nähe}
begin
  result:=(abs(lat1-lat2)<nhw) and (abs(lon1-lon2)<nhw);
end;*)

(*Begin Geotagging #############################################################

  Needed component: https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/fpexif/
                    plus fpEXIF patch for Yuneec from wp_XYZ in r7965 (2021-01-17)
  See also: https://www.lazarusforum.de/viewtopic.php?f=18&t=13356

  EXIF tags:        https://exiftool.org/TagNames/EXIF.html   *)

procedure TForm1.mnGeoGMapClick(Sender: TObject);  {Menu GeoTagging show in GoogleMaps}
var lat, lon: string;
begin
  lat:=gridEXIFPic.Cells[4, gridEXIFPic.Selection.Top];
  lon:=gridEXIFPic.Cells[5, gridEXIFPic.Selection.Top];
  if (lat<>'') and (lon<>'') then
    OpenURL(URLGMap(lat, lon));
end;

procedure TForm1.mnGeoOSMClick(Sender: TObject);   {Menu GeoTagging show in OSM}
var lat, lon: string;
begin
  lat:=gridEXIFPic.Cells[4, gridEXIFPic.Selection.Top];
  lon:=gridEXIFPic.Cells[5, gridEXIFPic.Selection.Top];
  if (lat<>'') and (lon<>'') then
    OpenURL(URLosm(lat, lon));
end;

procedure TForm1.btnScanPicClick(Sender: TObject); {Geotagging: Button Scan}
begin
  ScanPic;
end;

procedure TForm1.ScanPic;                          {Geotagging: Scan picture folder}
var i, zhl: integer;
    filelist, inlist: TStringlist;
    aImgInfo: TImgInfo;
    picdat, tmin, tmax, ttemp: TDateTime;
    lat, lon, alt: double;
    cam: string;

  procedure FindPicData;                           {Find time stamp in telemetry}
  var pos: integer;
      spl: TStringArray;
  begin
    if inlist.Count>minlines then begin            {Check telemetry file}
      pos:=FindTP(inlist, picdat+(speTimeOffset.Value/24));  {Time point like Q500}
      if pos>1 then begin                          {Found someting inside CSV file}
        gridEXIFPic.Cells[3, i+1]:=IntToStr(pos);  {ID for doing something}
        inc(zhl);
        spl:=inlist[pos].Split(sep);
        gridEXIFPic.Cells[4, i+1]:=spl[5];
        gridEXIFPic.Cells[5, i+1]:=spl[6];
        gridEXIFPic.Cells[6, i+1]:=spl[4];
      end;
    end else
      StatusBar1.Panels[2].Text:=errNoTelemetryFile;
  end;

begin
  if DirectoryExists(cbxPicFolder.Text) then begin
    Screen.Cursor:=crHourGlass;
    filelist:=TStringlist.Create;
    inlist:=TStringlist.Create;
    gridEXIFPic.RowCount:=1;                       {Empty picture list}
    cgpCamera.Items.Clear;                         {Empty camera list}
    aImgInfo:=TImgInfo.Create;
    zhl:=0;
    try
      FindAllFiles(filelist, cbxPicFolder.Text, '*.jpg;*.jpeg', false);
      if filelist.Count>0 then begin
        StatusBar1.Panels[0].Text:=IntToStr(filelist.Count);
        gridEXIFPic.RowCount:=filelist.Count+1;    {Table to list JPG files}
        if FileExists(cbxTelemetry.Text) then
          inlist.LoadFromFile(cbxTelemetry.Text);  {Load related telemetry}
        if inlist.Count>minlines then begin        {Show timing in telemetry}
          tmin:=ZeitToDT(copy(inlist[minlines], 1));
          tmax:=ZeitToDT(copy(inlist[inlist.Count-1], 1));
          gridTimeArea.Cells[1, 2]:=FormatdateTime(vzf, tmin);
          gridTimeArea.Cells[2, 2]:=FormatdateTime(vzf, tmax);
        end;
        tmax:=0;
        tmin:=now;

        for i:=0 to filelist.Count-1 do begin
          picdat:=FileDateToDateTime(FileAge(filelist[i]));  {File date time}

          gridEXIFPic.BeginUpdate;
            gridEXIFPic.Cells[0, i+1]:=ExtractFileName(filelist[i]);
            try
              aImgInfo.LoadFromFile(filelist[i]);
            except                                 {Error message EXIF data structure}
              on e: Exception do begin
                StatusBar1.Panels[2].Text:=e.Message;
                break;
              end;
            end;
            if aImgInfo.HasEXIF then begin         {Read data from EXIF, check what is in}
              try
                ttemp:=GetEXIFtime(aImgInfo);
                if ttemp>0 then                    {EXIF time available, CGO has not}
                  picdat:=ttemp;
                cam:=trim(ReadString(aImgInfo, exModel, ''));
                if (cam<>'') and                   {Fill camera listing}
                   (cgpCamera.Items.IndexOf(cam)<0) then  {Not yet in list}
                  cgpCamera.Items.Add(cam);
                gridEXIFPic.Cells[1, i+1]:=cam;

                lat:=aImgInfo.EXIFdata.GPSLatitude; {Check if coordinates already in}
                lon:=aImgInfo.EXIFdata.GPSLongitude;

                if (lat<>0) or (lon<>0) then begin {Valid coordinates found in EXIF}
                  alt:=aImgInfo.EXIFdata.GPSAltitude;
                  gridEXIFPic.Cells[4, i+1]:=FormatFloat(coordfl6, lat)+exID;
                  gridEXIFPic.Cells[5, i+1]:=FormatFloat(coordfl6, lon)+exID;
                  gridEXIFPic.Cells[6, i+1]:=FormatFloat(dzfl, alt)+exID;
                end else                           {No valid coordinates found in EXIF}
                  FindPicData;                     {Find time stamp in telemetry}

              except                               {Error message EXIF}
                on e: Exception do begin
                  StatusBar1.Panels[2].Text:=e.Message;
                end;
              end;
            end else                               {End hasEXIF, possibly create new EXIF part}
              FindPicData;                         {Find time stamp in telemetry for pics w/o EXIF}
                                                   {File time or overwritten by EXIF time}
            gridEXIFPic.Cells[2, i+1]:=FormatdateTime(vzf, picdat);

            if picdat>tmax then
              tmax:=picdat;
            if picdat<tmin then
              tmin:=picdat;
          gridEXIFPic.EndUpdate;

          gridEXIFPic.AutoSizeColumns;
        end;
                                                   {Show time area for pictures}
        gridTimeArea.Cells[1, 1]:=FormatdateTime(vzf, tmin);
        gridTimeArea.Cells[2, 1]:=FormatdateTime(vzf, tmax);

        for i:=0 to cgpCamera.Items.Count-1 do
          cgpCamera.Checked[i]:=true;
        StatusBar1.Panels[1].Text:=IntToStr(zhl);  {Number if pic files to be updated}
      end else
        StatusBar1.Panels[2].Text:=errNoPictures;
      btnWritePic.Enabled:=(zhl>0);                {Allow write action to pictures}
    finally
      filelist.Free;
      inlist.Free;
      aImgInfo.Free;
      Screen.Cursor:=crDefault;
    end;
  end else
    StatusBar1.Panels[2].Text:=errNoPicFolder;
end;

function CamInList(ca: string; list: TCheckGroup): boolean; {Select camera type}
var i: integer;
begin
  result:=true;                                    {Get all cameras if there is no list}
  if list.Items.Count>0 then begin
    result:=false;
    for i:=0 to list.Items.Count-1 do
      if list.Checked[i] and (ca=list.Items[i]) then begin
        result:=true;
        break;
      end;
  end;
end;

procedure TForm1.btnWritePicClick(Sender: TObject); {Geotagging: Write EXIF data}
var i, zhl: integer;
    fn, cam: string;
    lat, lon, alt: double;
    bg: TDateTime;
    aImgInfo: TImgInfo;
begin
  zhl:=0;
  if gridEXIFPic.RowCount>1 then begin             {If pictures available}
    for i:=1 to gridEXIFPic.RowCount-1 do begin
      bg:=0;
      if gridEXIFPic.Cells[3, i]<>'' then begin    {ID to do something}
        fn:=IncludeTrailingPathDelimiter(cbxPicFolder.Text)+gridEXIFPic.Cells[0, i];
        cam:=gridEXIFPic.Cells[1, i];
        alt:=StrToFloatDef(gridEXIFPic.Cells[6, i], 0); {Altitude relative}
        bg:=ScanDateTime(vzf, gridEXIFPic.Cells[2, i]);
        if cam<>'' then begin                      {Picture has already EXIF data}
          if CamInList(cam, cgpCamera) then begin
            if GetCoords(gridEXIFPic.Cells[4, i], gridEXIFPic.Cells[5, i],
                         lat, lon) then begin
              aImgInfo:=TImgInfo.Create;
              try
                aImgInfo.LoadFromFile(fn);
                if aImgInfo.HasEXIF then begin     {Update EXIF}
                  try
                    WriteEXIFtime(aImgInfo, exTime1, bg, false);
                    WriteEXIFtime(aImgInfo, exTime2, bg, false);
                    WriteCoordinates(aImgInfo, lat, lon, true);
                    WriteAltitude(aImgInfo, alt, true);

                    if cbBackupPic.Checked then    {Make backup file}
                      CopyFile(fn, ChangeFileExt(fn, '.bak'), [cffPreserveTime]);
                    aImgInfo.SaveToFile(fn);
                    gridEXIFPic.Cells[7, i]:=resUpd;
                    inc(zhl);
                  except
                    on e: Exception do begin
                      StatusBar1.Panels[2].Text:=e.Message;
                    end;
                  end;
                end;
              finally
                aImgInfo.Free;
              end;
            end;
          end;
        end else begin                             {Picture had no EXIF data}
          if GetCoords(gridEXIFPic.Cells[4, i], gridEXIFPic.Cells[5, i],
                       lat, lon) then begin
            aImgInfo:=TImgInfo.Create;             {Create EXIF}
            try
              try
                CreateMetadata(aImgInfo, AppName, AppVersion, bg, bg);
                aImgInfo.ExifData.GPSLatitude:=lat;  {With patch r7968 reference is set automatically}
                aImgInfo.ExifData.GPSLongitude:=lon;
                aImgInfo.ExifData.GPSAltitude:=alt;

                if cbBackupPic.Checked then
                  CopyFile(fn, ChangeFileExt(fn, '.bak'), [cffPreserveTime]);
                aImgInfo.SaveToFile(fn);
                gridEXIFPic.Cells[7, i]:=resNew;
                inc(zhl);
              except
                on e: Exception do begin
                  StatusBar1.Panels[2].Text:=e.Message;
                end;
              end;
            finally
              aImgInfo.Free;
            end;
          end;
        end;
      end;
    end;
    if zhl=0 then
      StatusBar1.Panels[2].Text:=errNoUpdates
    else
      Statusbar1.Panels[1].Text:=IntToStr(zhl);
    gridEXIFPic.AutoSizeColumns;
    btnWritePic.Enabled:=false;                    {Close write action}
  end;
end;

procedure TForm1.gridEXIFPicPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);                         {Set colors for EXIF updates}
begin
  if (aRow>0) and                                  {not to header}
     (aState=[]) then begin                        {not if selected}
    case aCol of
      3: if gridEXIFPic.Cells[aCol, aRow]<>'' then
              CellColorSetting(gridEXIFPic, clAttention);
      2: if gridEXIFPic.Cells[3, aRow]<>'' then
              CellColorSetting(gridEXIFPic, clMoneyGreen);
      4..6: if pos(exID, gridEXIFPic.Cells[aCol, aRow])>0 then
              CellColorSetting(gridEXIFPic, clMoneyGreen);
      7: if gridEXIFPic.Cells[aCol, aRow]<>'' then
              CellColorSetting(gridEXIFPic, clMoneyGreen);
    end;
  end;
end;

procedure TForm1.gridTimeAreaMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var sp, zl: integer;
begin
  gridTimeArea.MouseToCell(x, y, sp, zl);          {Zelle unter Maus finden}
  if (sp>0) and (zl>0) then
    gridTimeArea.Hint:=gridTimeArea.Cells[sp, zl]
  else
    gridTimeArea.Hint:='';
end;

procedure TForm1.edReceiveCGO3DblClick(Sender: TObject);  {CGO3 Test Copy to Clipboard}
begin
  If edReceiveCGO3.Text>'' then
    ClipBoard.AsText:=edSendCGO3.Text+LineEnding+edReceiveCGO3.Text;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  ScanPicEnable;
end;

procedure TForm1.gridEXIFPicDblClick(Sender: TObject);
begin
  GeoShowPic;                                      {Show picture}
end;

procedure TForm1.GeoShowPic;                       {Menu Show picture}
var fn: string;
begin
  fn:=gridEXIFPic.Cells[0, gridEXIFPic.Selection.Top];
  if fn<>'' then
    OpenDocument(IncludeTrailingPathDelimiter(cbxPicFolder.Text)+fn);
end;

procedure TForm1.mnShowPicClick(Sender: TObject);  {Menu Show picture}
begin
  GeoShowPic;
end;

procedure TForm1.sbtnPicFolderClick(Sender: TObject); {Geotagging picture folder}
begin
  SelectDirectoryDialog1.Title:=capSelPicFolder;
  if SelectDirectoryDialog1.Execute then begin
    cbxPicFolder.Text:=SelectDirectoryDialog1.FileName;
    Merkliste(cbxPicFolder, numItems);
    ScanPicEnable;
  end;
end;

procedure TForm1.sbtnTelemetryClick(Sender: TObject); {Select telemetry file name}
begin
  OpenDialog1.Title:=capTelemetryFile;
  if OpenDialog1.Execute then begin
    cbxTelemetry.Text:=OpenDialog1.FileName;
    Merkliste(cbxTelemetry, numItems);
    ScanPicEnable;
  end;
end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DefaultFormatSettings.DecimalSeparator:=chr(Tag); {Original wiederherstellen}
end;

procedure TForm1.SendCGOcmd;                       {Command zu CGO3}
begin
  if edSendCGO3.Text>'' then
    if CGO3run(edSendCGO3.Text, 0)=0 then begin
      CGO3run('', 0);
    end else begin
      StatusBar1.Panels[2].Text:=errNoPossbl;
    end;
end;

procedure TForm1.sbtnSendCGO3Click(Sender: TObject);   {Command zu CGO3}
begin
  SendCGOcmd;
end;

procedure TForm1.StatusToClipboard;                {Statuszeile kopieren}
begin
  ClipBoard.AsText:=StatusBar1.Panels[2].Text;
end;

procedure TForm1.StatusBar1DblClick(Sender: TObject);  {Copy bei Doppelclick}
begin
  StatusToClipboard;
end;

{http://www.lazarusforum.de/viewtopic.php?f=10&t=12130}
procedure TForm1.StatusBar1Hint(Sender: TObject);  {Reroute Application hint}
begin
  StatusBar1.Panels[2].Text:=Application.Hint;
end;

procedure TForm1.gridCGO3KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                             {CGO3 Einstellungen kopieren}
begin
  if (key=vk_c) and (ssCtrl in Shift) then gridCGO3.CopyToClipBoard(false);
end;

procedure TForm1.tmCGOstatusTimer(Sender: TObject);     {CGO3 Statusabfrage}
begin
  if tabCGO3.Visible then
    CGO3run('', 0);
end;

end.

