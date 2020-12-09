unit q5_common;                                    {Common definitions and variables}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, lazUTF8, math, Grids;

const
{public constants}
  AppName=   'q500log2kml';
  AppVersion='V4.7 12/2020';

  defaultcol=5;                                    {muss kleiner als die Mindestmenge der Spalten sein}
  defVT=5;                                         {Default vehicle YTH, need 5 for Thunderbird}
  YTHPid=10;                                       {YTH Plus ID}
  MQid=11;                                         {MantisQ ID, PX4 Quadcopter}
  H5id=12;                                         {Yuneec H520, *.tlog files (PX4)}
  MQcsvID=13;                                      {neues CSV Format beim MQ}
  ThBid=14;                                        {Thunderbird, H480 customized firmware on base of PX4}
  brID=90;                                         {Breeze ID}
  H501ID=91;                                       {Hubsan flaretom Rekorder}
  YTHPcols=276;                                    {Anzahl Spalten bei YTH Plus Sensorfiles = ID für Anzeige}

  InvalidChars: set of char=['\', '/', ':', '*', '?', '"', '<', '>', '|', '&'];
  ziff=['0'..'9'];                                 {gültige Ziffern}
  tab1=' ';                                        {ein Leerzeichen}
  tab2='  ';
  tab4='    ';
  tab6='      ';
  sckey='&';

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
  coordfrm='0.000000';

  fkmh=3.6;                                        {m/s --> km/h}
  fmph=2.2369362920544;                            {m/s --> mph}
  fmile=1.609344;
  fft=0.3048;                                      {Faktor Umrechnung ft --> m}

  fmode='f_mode';                                  {Spalte Flightmode, Position der Spalte in gridDetails.Tag}
  pfmode='fMode';                                  {Spaltenbezeichnung beim YTH Plus}
  brfmode='flightMode';                            {FlightMode beim Breeze}

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

var timestr: string;
    v_type: integer;


{Public functions and procedures}

  function BoolToDouble(const s: string): double;  {zum Darstellen von Boolean}
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
  function DeltaKoord(lat1, lon1, lat2, lon2: double): double;   {Entfernung in m}

  procedure CellColorSetting(aGrid: TStringGrid; Farbe: TColor); {Zellen einfärben}
  procedure FMcolor(aGrid: TStringGrid; fm, vt: integer);  {Flight mode coloe r settings}

implementation

function BoolToDouble(const s: string): double;    {zum Darstellen von Boolean}
begin
  result:=0;
  if LowerCase(trim(s))='false' then
    result:=-1;
  if LowerCase(trim(s))='true' then
    result:=1;
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
  if length(s)>0 then begin
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
    try
      result:=StrToFloat(s)*m;
    except
      result:=BoolToDouble(s);
    end;
  end;
end;

function CleanDN(const s: string): string;         {Ungültige Zeichen entfernen}
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

function CleanNum(const s: string): string;        {Ziffern filtern}
var i: integer;
begin
  result:='';
  for i:=1 to length(s) do
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
  if length(s1)>0 then
    for x:=1 to length(s1) do
      if (s1[x] in ziff) or
         (s1[x]='-') or
         (s1[x]=DefaultFormatSettings.DecimalSeparator) then
        result:=result+s1[x];
  if result='' then
    result:='0';
end;

function GetFVal(const s: string): double;         {get a float from a string}
begin
  try
    result:=StrToFloat( GetFNr(s));
  except
    result:=0;
  end;
  if result=nan then
    result:=0;
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
function DeltaKoord(lat1, lon1, lat2, lon2: double): double;
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

procedure CellColorSetting(aGrid: TStringGrid; Farbe: TColor); {Zellen einfärben}
begin
  aGrid.Canvas.Brush.Color:=Farbe;
  aGrid.Canvas.Font.Color:=clBlack;                {Text always black}
end;

procedure BladeCol(aGrid: TStringGrid; fm: integer);
begin
  case fm of                                       {flight modes; wie Chart1BarSeries}
    25:             CellColorSetting(aGrid, clAngle);
    11:             CellColorSetting(aGrid, clSport);
    9, 14, 10, 13:  CellColorSetting(aGrid, clRTH);
    12:             CellColorSetting(aGrid, clSmart);
    8:              CellColorSetting(aGrid, clEmergency);
    5:              CellColorSetting(aGrid, clSilver);        {Motor Starting}
  end;
end;

procedure LegacyCol(aGrid: TStringGrid; fm: integer);
begin
  case fm of                                       {flight modes wie Chart1BarSeries}
    3, 4:                  CellColorSetting(aGrid, clAngle);
    2, 5, 7, 22, 24, 32:   CellColorSetting(aGrid, clNoGPS);
    13, 14, 20:            CellColorSetting(aGrid, clRTH);
    6, 21, 23:             CellColorSetting(aGrid, clSmart);
    9, 10, 11, 12, 17, 18: CellColorSetting(aGrid, clEmergency);
    0, 1:                  CellColorSetting(aGrid, clSport);
    26..29, 33:            CellColorSetting(aGrid, clTasks);  {Tasks}
    8:                     CellColorSetting(aGrid, clSilver); {Motor Starting}
  end;
end;

procedure ThunderCol(aGrid: TStringGrid; fm: integer);
begin
  case fm of                                       {flight modes; wie Chart1BarSeries}
    0:      CellColorSetting(aGrid, clSport);                 {Stabilized blue}
    1:      CellColorSetting(aGrid, clNoGPS);                 {Altitude orange}
    3:      CellColorSetting(aGrid, clAngle);                 {Position purple}
    13, 14: CellColorSetting(aGrid, clAttention);             {RTH other red}
    20:     CellColorSetting(aGrid, clRTH);                   {Rattitude/Rate red}
    33:     CellColorSetting(aGrid, clSmart);                 {Mission green}
    16:     CellColorSetting(aGrid, clSilver);                {Ready silver}
  end;
end;

procedure FMcolor(aGrid: TStringGrid; fm, vt: integer);       {Flight mode coloe r settings}
begin
  case vt of
    1, 2:  LegacyCol(aGrid, fm);
    3:     BladeCol(aGrid, fm);                    {350QX}
    4..6:  LegacyCol(aGrid, fm);
    ThBid: ThunderCol(aGrid, fm);                  {Thunderbird}
  end;
end;

end.

