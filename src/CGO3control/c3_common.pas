{           History:

2022-04-02  V1.0 First time stripped down from q500log2kml

}


unit c3_common;                                    {Common definitions and variables}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, lazUTF8, math, Grids;

const
{public constants}
  AppName=   'CGO3control';
  AppVersion='V1.0 02/2022';

  homepage='http://h-elsner.mooo.com';             {My Homepage}

  defVT=5;                                         {Default vehicle YTH, need 5 for Thunderbird}

  InvalidChars: set of char=['\', '/', ':', '*', '?', '"', '<', '>', '|', '&'];
  ziff=['0'..'9'];                                 {gültige Ziffern}
  valchars=[32..126, 128, 166, 167, 169, 177..179, 181, 188..190, 215, 247, 196, 214, 220, 223, 228, 246, 252];tab1=' ';                                        {ein Leerzeichen}
  tab2='  ';
  tab4='    ';
  tab6='      ';
  sckey='&';
  suff=': ';                                       {Suffix zur Datenausgabe}
  emcyID='EMERGENCY';
  idtrue='true';
  idfalse='false';

  csvsep=';';
  spk=4;                                           {Korrekturwert Spaltenbreite}
  zzf='hh:nn:ss';
  mzf='yyyy-mm-dd hh:nn';                          {Datum/Zeit Formate}
  dzf='yyyy-mm-dd';
  vzf=dzf+' '+zzf;
  zzz='.zzz';
  dzfl='0.0';                                      {Formatierung für FormatFloat}
  ctfl='0.00';
  mlfl='0.000';
  coordfl6='0.000000';
  coordfl8='0.00000000';

  fkmh=3.6;                                        {m/s --> km/h}
  fmph=2.2369362920544;                            {m/s --> mph}
  fmile=1.609344;
  fft=0.3048;                                      {Faktor Umrechnung ft --> m}

  fmode='f_mode';                                  {Spalte Flightmode, Position der Spalte in gridDetails.Tag}
  pfmode='fMode';                                  {Spaltenbezeichnung beim YTH Plus}

  clOrange=$000080FF;
  clNoGPS=$000080FF;                               {Dark Orange}

  clAngle=clFuchsia;
  clEmergency=clMaroon;
  clSmart=clGreen;
  clRTH=clRed;
  clSport=clBlue;

  clFairGood=clMoneyGreen;
  clVeryGood=clGreen;
  clTasks=clMoneyGreen;
  clAttention=$008080F0;                           {Farbe Achtung}
  clError=clRed;

  clVolt1=clSkyBlue;
  clVolt2=$00FF901E;                               {Voltage 2 Farbe}
  clErrFlag=$000080FF;                             {Orange}
  clPeaks=clYellow;

  minh=0.01;                                       {Höhengrenzen bei Übernahme}
  maxh=300;
  defh=10;                                         {default Höhe in m}
  maxxh=7999;                                      {Höhe validieren --> testh(a)}
  minnh=-1000;                                     {gefähliche Annahme, tritt aber bei YTH Plus als Fehler auf}
  distmax=1000;                                    {Plasicheck: Koord dist in m}
  Secpd=86400;                                     {Sekunden per Tag}
  tsdelta1=6/864000;                               {Schwellwert für Zeitstempel in 1/10s, default 6=600ms}
  minflt=10/86400;                                 {Mindestflugzeit beim YTH Plus 10s}

  wext='.txt';
  fext='.csv';                                     {alle csv-Dateien}
  bind=' - ';                                      {von/bis ID}
  idxpage='INDEX_PAGE';
  getshpn='GET_SHARPNESS';
  CGO3dir='100MEDIA/';
  CGO3cgi='cgi-bin/cgi?CMD=';


var timestr: string;

{$I cgo3_dt.inc}
{.$I cgo3_en.inc}

{Public functions and procedures}

  function ChrKoor(ko: string): string; inline;    {Korrigiert Format der Koordinaten}
  function KorrBool(const s: string): string;      {true -> 1, Rest -> 0}
  function StatusToByte(const s: string): byte;    {wandelt negative Statusanzeigen um}
  function StrToFloatN(s: string): double; {kapselt StrToFloat, gibt bei Fehler 0 zurück}
  function CleanDN(const s: string): string;       {Ungültige Zeichen entfernen}
  function CleanNum(const s: string): string;      {Ziffern filtern}
  function FormSR(const s: string; const p: integer): string;  {füllt string mit führenden
                                                    Leerzeichen auf Länge p}
  function IntToStrFL(const w, p: integer): string; {Wandelt Zahlen in String mit
                                           Länge p mit führenden Leerzeichen um}
  function GetFNr(const s: string): string;        {filter a float from a string}
  function GetFVal(const s: string): double;       {get a float from a string}
  function tabs(const prefix, suffix: string; const t: integer): string;  {Tabulator + suff}
  function DeltaKoord(const lat1, lon1, lat2, lon2: double): double;      {Entfernung in m}
  procedure CellColorSetting(aGrid: TStringGrid; Farbe: TColor); {Zellen einfärben}

implementation

function KoToStr(const lanlon: double): string; inline;
                                                   {Koordinaten zu String}
begin
  result:=FormatFloat(coordfl8, lanlon);
end;

function ChrKoor(ko: string): string; inline;      {Korrigiert Format der Koordinaten}
var co: double;
    i: integer;
    s: string;

begin
  try
    co:=StrToFloatN(ko);
    if (abs(co)>181) then begin                    {Strange format in ST16S}
      s:='';
      for i:=1 to ko.length do begin
        if ko[i] in ziff then
          s:=s+ko[i];
      end;
      if ko[1]='-' then
        s:='-0.'+s
      else
        s:='0.'+s;
      co:=StrToFloat(s);                           {-6558209.0 --> -0.6558209}
    end;
    result:=KoToStr(co);                           {umwandeln um führende 0 zu bekommen}
  except
    result:=ko;
  end;
end;

function KorrBool(const s: string): string;        {true -> 1, Rest -> 0}
begin
  result:='0';
  if LowerCase(trim(s))=idtrue then
    result:='1';
end;

function StatusToByte(const s: string): byte;      {wandelt negative Statusanzeigen um}
var p: integer;
begin
  try
    p:=StrToInt(s);
  except
    p:=0;
  end;
  result:=(p and 255);                             {positive Zahlen übernehmen}
end;

function StrToFloatN(s: string): double; {kapselt StrToFloat, gibt bei Fehler 0 zurück}
var m: integer;

begin
  result:=0;
  if s.length>0 then begin
    m:=1;                                          {Multiplikator bei Expo}
    if pos('E', s)>0 then begin
      if pos('E7', s)>0 then begin
        s:=StringReplace(s, 'E7','',[rfReplaceAll, rfIgnoreCase]);
      end else
      if pos('E8', s)>0 then begin                 {*10}
        m:=10;
        s:=StringReplace(s, 'E8','',[rfReplaceAll, rfIgnoreCase]);
      end else
      if pos('E9', s)>0 then begin                 {*100}
        m:=100;
        s:=StringReplace(s, 'E9','',[rfReplaceAll, rfIgnoreCase]);
      end;
    end;
    result:=StrToFloatDef(s, 0)*m;
  end;
end;

function CleanDN(const s: string): string;         {Ungültige Zeichen entfernen}
var i: integer;
begin
  result:='';
  for i:=1 to s.length do
    if s[i]=' ' then
      result:=result+'_'
    else
      if not (s[i] in InvalidChars) then
        result:=result+s[i];
end;

function CleanNum(const s: string): string;        {Ziffern filtern}
var i: integer;
begin
  result:='';
  for i:=1 to s.length do
    if s[i] in ziff then
      result:=result+s[i];
end;

function FormSR(const s: string; const p: integer): string;  {füllt string mit führenden
                                                    Leerzeichen auf Länge p}
begin
  result:=s;
  while UTF8length(result)<p do
    result:=tab1+result;
end;

function IntToStrFL(const w, p: integer): string;   {Wandelt Zahlen in String mit
                                         Länge p mit führenden Leerzeichen um}
begin
  try
    result:=FormSR(IntToStr(w), p);
  except
    result:=FormSR('?', p);
  end;
end;

function GetFNr(const s: string): string;          {filter a float from a string}
var x: integer;
    s1: string;
begin
  result:='';
  s1:=trim(s);
  if s1.length>0 then
    for x:=1 to s1.length do
      if (s1[x] in ziff) or
         (s1[x]='-') or
         (s1[x]=DefaultFormatSettings.DecimalSeparator) then
        result:=result+s1[x];
  if result='' then
    result:='0';
end;

function GetFVal(const s: string): double;         {get a float from a string}
begin
  result:=StrToFloatDef(GetFNr(s), 0);
end;

function tabs(const prefix, suffix: string; const t: integer): string;  {Tabulator + suff}
begin                                     {formatiert einen string in Länge t zur Ausgabe}
  result:=prefix;
  while UTF8Length(result)<t do
    result:=result+tab1;
  result:=result+suffix;
end;

{Entfernung zwischen zwei Koordinaten in m
 siehe Haversine formula, Erdradius: 6,371km abh. von Breitengrad
 https://rechneronline.de/erdradius/
 6365.692 optimiert für 50 Breitengrad und 60m Höhe

 D:\Flight_Log_data\_Eigene\YTH\3\FlightLog2019-07-01\Telemetry_00002.csv
 seit Update auf Lazarus 2.0.4 unerwartete Fehlrechnung mit result NaN:
                    lat1     lon1     lat2      lon2
Test-3564 : 0.1 von 48.86739 9.366312 48.86739  9.366313
Test-3565 : Nan von 48.86739 9.366313 48.86739  9.366313
Test-3566 : 0.1 von 48.86739 9.366313 48.86739  9.366315
Test-3567 : Nan von 48.86739 9.366315 48.86739  9.366315
Test-3568 : 0.5 von 48.86739 9.366315 48.867386 9.366317
}
function DeltaKoord(const lat1, lon1, lat2, lon2: double): double;
begin
  result:=0;
  try
    result:=6365692*arccos(sin(lat1*pi/180)*sin(lat2*pi/180)+
            cos(lat1*pi/180)*cos(lat2*pi/180)*cos((lon1-lon2)*pi/180));
    if IsNan(result) or                            {Fehler in Formel ?}
       (result>30000) or   {> 30km --> unplausible Werte identifizieren}
       (result<0.005) then                         {Fehler reduzieren, Glättung}
      result:=0;
  except
  end;
end;

function RadToGrad(r: double): double;
begin
  result:=r*180/pi;                                {rad to ° +/-180}
end;

function GetContrastTextColor(const BackColor: TColor): TColor;
begin                                              {Textfaebe abh. vom Hintergrund}
  if (Red(BackColor) * 0.25+
      Green(BackColor) * 0.625+
      Blue(BackColor) * 0.125) > 90 then
    result := clBlack
  else
    result := clWhite;
end;

procedure CellColorSetting(aGrid: TStringGrid; Farbe: TColor); {Zellen einfärben}
begin
  aGrid.Canvas.Brush.Color:=Farbe;
  aGrid.Canvas.Font.Color:=GetContrastTextColor(Farbe);
end;


end.

