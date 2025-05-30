          {********************************************************}
          {                                                        }
          {     Auswertung FlightLog Daten von Yuneec Koptern      }
          {                                                        }
          {       Copyright (c) 2015-2025    Helmut Elsner         }
          {                                                        }
          {       Compiler: FPC 3.2.3   /    Lazarus 4.0           }
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

*******************************************************************************)

(*          History:

2015-12-10  V0.1 First try to read telemetry, GUI created, import and data
                 consistence check.
2015-12-21  V1.0 KML or KMZ file creation, Elevation histogram added
2015-12-23  V1.1 Voltage curve as underlay in Elevation histogram
2015-12-25  V1.2 Distance to start place added, histograms improved
                 UAV Pilot Simulation: Indicated by voltage is gray
2015-12-29  V1.3 TabSheet 'Setting' added, menus added
2016-01-01  V1.4 First basic analysis functions added, link Manual added
2016-01-17  V1.5 Colored cells for better visibility of errors flags
2016-01-23  V1.6 Menu item 'Go to error flags' and 'Go to table' added
2016-02-05  V1.7 Update for Blade 350QX
2016-02-10  V1.8 Open Street Map added
2016-02-15  V1.9 Detailed tables and histograms for values in columns added

2016-03-02  V2.0 Update Flight Modes, Vehicle Types, Archive suffix changeable
2016-03-24  V2.1 Histogram distance at Lon/Lat,
                 IMU_status explanations updated (Pressure init supposed)
2016-04-10  V2.2 GPX format
2016-05-04  V2.3 Cursor at histograms added, Support ST10 firmware
                 (only telemetry, no remote files)
2016-05-23  V2.4 Check how frequent telemetry data were sent. Support
                 Typhoon H, H920.
2016-07-18  V2.5 CCC Waypoints editor and conversion (editor later removed).
2016-09-05  V2.6 Improvements for Typhoon H.
2016-10-01  V2.7 Support Log Files from Yuneec Breeze.
2017-04-10  V2.8 KML export updated (Altitude modes). f_mode=32/33 added.
                 Output format for RaceRender, Screenshot,
                 Quick analysis and cut files added.
2017-05-01  V2.9 Main Menu added, source code cleaned, Directory selection
                 with ComboBox that holds last used directories.

2017-05-18  V3.0 Control for CGO3 as test system.
2017-06-15  V3.1 Overview improved, flight record added. Track of RemoteGPS
                 coordinates in KML/KMZ added (black line).
2017-07-02  V3.2 Search in 'Display files' table added. Reverse GeoCoding.
                 Flight record for Breeze. Improvement of tables:
                 Hint per cell at mouse hold over.
2017-11-26  V3.3 Improvement of hints, i.e. Channels.
                 CCC Waypoints editor removed. Only conversion of
                 Telemetrie to  CCC Waypoints remains. BugFix battery capacity
                 in Flight record and VehicleType for 350QX.
2018-01-16  V3.4 Time list for Error flags, Save chart as picture changed
                 for MAC OS X.
                 Filter in data table added (F4) with the same functionality
                 as search (F3). Shortcut Ctrl+n to cut files added.
2018-04-16  V3.5 Menu icon added. Jump to data set by double click on
                 time information (i.e. f_mode). Animated KML/KMZ files.
                 Flexible assignment of columns for quick analysis in settings,
                 drag&drop added.
                 Profiles and popup menu for quick analysis added.
                 Several optimizations and bug fixes.
2018-05-15  V3.6 Flight record moved to tab sheet Scan.
                 Scan: Recursive search in a whole directory.
                       a) Flight record
                       b) Search for known errors or special cases in FlightLogs
                 Display Sensor files (without interpretation).
2018-06-21  V3.7 Basic functions for Typhoon H Plus.
                 Clean up of data telemetry records for YTH Plus added.
                 YTH Plus Sensor loaded as single file in main menu.
2018-07-02       Profiles as "slideshow" (3s).
2018-07-05       Sort table overview by click on column header.
                 Bugfix for Breeze: Support 'strange' coordinates in
                 Breeze telemetry.
2018-07-19       Sensor YTH Plus updated. Message ID and text messages
                 decoded.
                 Bug fix für menu item "Go to datapoint".
2018-07-24  V3.8 AppLogHighlighter added, Text messages from Sensor files listed there.
                 MAV-Link Message IDs updated, Severity added to text messages.
2018-08-15       Typhoon H Plus Smart Mode and Manual Mode added.
2018-08-27	 Updates for Sensor files.
2018-09-14       SHARPNESS for CGO3+ updated, TeamMode at Gimbal Pan Mode.
2018-09-28	 Coordinates from GPS_RAW_INT in AppLogHighlighter, Message names in Sensor
                 file display for Typhoon H Plus.
2018-10-02  V3.9 Mantis Q Support, H Plus sensor files renamed to PX4 sensor files
                 in menu. System health information from PX4 sensor files added
                 to AppLogHighlighter.
2018-10-24       Identification of Mantis Q flylog files improved. Flight path
                 from PX4 sensor files as KML/KMZ or GPX file.
2018-10-28	 Show sensor files faster.
2018-10-29       Reverse geocoding removed, Bugfix: Mantis Q identification,
                 Check only Heartbeat from AUTOPILOT1

2018-11-03  V4.0 H520 *.tlog files support added similar to Mantis Q.
                 Batch conversion, button 'Convert' to KML/KMZ or GPX files
                 works now also for Mantis Q and H520.
                 Quick analysis filled: Voltage, current and SW-load.
                 Elevation histogram filled: Relative elevation and distance to first
                 coordinate.
2018-11-20       Vertical lines in KML/KMZ can be switched on (Extrude). Flight
                 times for Typhoon H Plus improved (Overview and Flight records).
2018-12-07       Added envelope to Elevation histogram.
2018-12-22       Crosshair Tool recreated and streamlined.
2019-01-09  V4.1 PX Sensor data export to CSV file.
                 Some more MAV messages decoded.
2019-01-15       Self-defined CSV format changed - more flexible.
                 RF channels added.
                 Show self-defined PX4 Sensor CSV format.
                 Some important H520 MAV messages added.
2019-01-26       Optimization auto size columns.
2019-02-28       Time since system boot instead of default time format
                 as context hint at telemetry for H Plus.
2019-03-05       Keep default behaviour in selected cells of a table.
2019-03-27       Time label at cursor in additional chart.
2019-04-07       Colors in Elevation chart for PX4 logs.
2019-04-10       Wording updated. Text message added to CSV file.
2019-04-16       MAV Message PARAM_VALUE added.
2019-08-16       Manual Link updated.
2019-09-04  V4.2 Option LiPo remaining capacity added.
2019-11-12       Scan for 'EMERGENCY' in PX4 sensor files
2019-12-10       Update für ST24/H920 alte Firmware (Telemetry ohne Header)
2019-12-31       Setting for Thunderbird (H480 with PX4 firmware)
2020-01-30       Throttle in % added to CSV Header and cell info
2020-02-28  V4.3 Tom's Hubsan Log Recorder added. MAVlink messages
                 selectable for data reduction in PX4 CSV format.
                 Remaining battery capacity now according RC Groups table for
                 voltage vs. capacity.
2020-03-09       Additional placemarks in KML. Colors for Hubsan frames updated.
2020-03-11       Visualize RC gaps in telemetry by double click to additional chart.
2020-04-02       Current unit for H920 updated.
2020-04-25  V4.4 GLOBAL_POSITION_INT and BATTERY_STATUS added.
2020-05-11       Updates for H480 Thunderbird.
2020-05-29       Used capacity for PX4 sensor files instead of SW load.
2020-07-07       Remarks in KML files from PX4 Sensor files
2020-07-30  V4.5 Added Text messages overview for PX4 sensor files
2020-08-21       Clean up KML files from PX4 sensor files
2020-09-20       Message POSITION_TARGET_GLOBAL_INT added (only for AppLogHighlighter)
2020-09-21       Message ALTITUDE (141) added. Tools: List of used MAVlink messages.
2020-09-30       Tools - Hexdump added. Bugfix Motorstatus hexacopter (223).
2020-10-25       Correct strange Coordinates format from ST16S in RemoteGPS*.csv.
2020-10-30  V4.6 Update for Windows High Contrast (own colors removed).
                 AppLog highlighter switchable. Some Colors updated.
                 Open the last (newest) item in the list instead of the first (oldest).
                 Model list window removed. Model drop down list handling improved.
2020-11-01       Context menu for FlightLog list added.
                 Removed double click to refresh, menu item instead.
                 Delete a FlightLog function added.
2020-11-13       MAV link messages reviewed. Missing messages added.
2020-12-09  V4.7 GeoTagging for pictures taken at same time as the drone was flying.
2021-01-12       Time marker in KML tracks
2021-01-16       Query for latest version, GitHub link.
2021-02-02       Enable special evaluation by short key.
                 Current special evaluation: Altitude values from TLOG.
2021-03-28       H Plus column current changed to Remaining LiPo.
2021-04-05  V4.8 Menu Tools: Split TLOG files at time resets.
2021-07-04       Screenshot only from TabControl (the most intresting part).
2022-01-30       Indication for WiFi connection added to fsk_rssi chart.
2022-03-14       CGO3 control and Geotagging removed - independent extra tool
2022-06-22       MAV state values updated.
2022-07-04       GPS_sued in quick analysis chart as value +/-1
2022-09-18       Update Thunderbird FlightMode 12 - Emergency
2022-09-25  V4.9 New function: Combine legacy Yuneec flight logs
2022-11-30       Added function to clean flightlogs with wrong time stamps
2023-02-27       Splitting TLOG files improved
2023-05-16       Sampling regularity for column 0 (Date/Time)
2023-11-23       Added save as CSV to grid Details
2024-04-15  V5.0 AutoCut added: Automatically cut flightlog data to the flight only
2024-06-18       Add special analysis for IMU status
2024-09-14       Sensorfile H480 reworked
2024-10-02  V5.1 Sensor files H480 decoded, displayed as data or as raw hex
2025-04-21       Filter a column like Autofilter in Excel in Details view
2025-05-30       Pan modes updated, Pan mode Angle added
*)


unit q5_common;                                    {Common definitions and variables}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, lazUTF8, math, Grids;

const
{public constants}
  AppName=   'Q500log2kml';
  AppVersion='V5.1 - 05/2025';
  VersValue=511;                                   {Verion number as Integer to compare}
  VersFile='/v';

  homepage='http://h-elsner.mooo.com';             {My Homepage}
  githublink='https://github.com/h-elsner/Q500log2kml';

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
  valchars=[32..126, 128, 166, 167, 169, 177..179, 181, 188..190, 215, 247, 196, 214, 220, 223, 228, 246, 252];
  tab1=' ';                                        {ein Leerzeichen}
  tab2='  ';
  tab4='    ';
  tab6='      ';
  trnrApplog='                      ';
  trenner='--------------------';

  sckey='&';
  suff=': ';                                       {Suffix zur Datenausgabe}
  kma=', ';                                        {Kommaausgabe}
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
  szzz='nn:ss.zzz';                                {Boottime}
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
  brfmode='flightMode';                            {FlightMode beim Breeze}

  clOrange=$000080F8;
  clNoGPS=$000080FF;                               {Dark Orange}
  clAngle=clFuchsia;                               {Angle, Position}
  clEmergency=clMaroon;
  clSmart=clGreen;                                 {Smart, Mission}
  clRTH=$005D00F3;                                 {Dark red}
  clSport=clBlue;                                  {Sport, Stabilized}
  clAcro=clRed;                                    {Rattitude, Acro}
  clTasks=clMoneyGreen;

  clVeryGood=clGreen;
  clFairGood=clMoneyGreen;
  clAttention=$008080F0;                           {Farbe Achtung}
  clError=clRed;

  clVolt1=clSkyBlue;
  clVolt2=$00FF901E;                               {Voltage 2 Farbe}
  clErrFlag=$000080FF;                             {Orange}
  clPeaks=clYellow;

  clTabs=$00F7F7F7;

  minh=0.01;                                       {Höhengrenzen bei Übernahme}
  maxh=300;
  defh=10;                                         {default Höhe in m}
  maxxh=7999;                                      {Höhe validieren --> testh(a)}
  minnh=-1000;                                     {gefähliche Annahme, tritt aber bei YTH Plus als Fehler auf}
  distmax=1000;                                    {Plasicheck: Koord dist in m}
  Secpd=86400;                                     {Sekunden per Tag}
  tsdelta1=6/864000;                               {Schwellwert für Zeitstempel in 1/10s, default 6=600ms}
  tsdelta2=2/86400;                                {> 2sec Telemtrie verloren}
  tsdelta3=5/86400;                                {5 sec für Breeze}
  minflt=10/86400;                                 {Mindestflugzeit beim YTH Plus 10s}


var timestr: string;
    v_type: byte;

{Public functions and procedures}

  function BoolToDouble(const s: string): double;  {zum Darstellen von Boolean in charts}
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
  function DistanceBetweenTwoCoordinates(const lat1, lon1, lat2, lon2: double): double;   {in m}

  procedure CellColorSetting(aGrid: TStringGrid; Farbe: TColor); {Zellen einfärben}
  procedure FMcolor(aGrid: TStringGrid; fm, vt: integer);  {Flight mode coloe r settings}
  function testh(const a: double): boolean; inline;  {Datensätze mit unsinniger Höhe ausblenden}
  function DefaultOuputToAppLog(zhl: integer; time: TDateTime; text: string): string;
  function FilterColumn(var aGrid: TStringGrid;    {Filter a column like autofilter in Excel}
                        const aCol: integer; aText: string): integer;
  procedure ResetAllFilterColumn(var aGrid: TStringGrid);


implementation

function BoolToDouble(const s: string): double;    {zum Darstellen von Boolean in charts}
begin
  result:=0;
  if LowerCase(trim(s))=idfalse then
    result:=-1
  else
    if LowerCase(trim(s))=idtrue then
      result:=1;
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

function CleanDN(const s: string): string;         {Ungültige Zeichen aus Dateiname entfernen}
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

function CleanNum(const s: string): string;        {Ziffern aus String filtern}
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

{Distance between two coordinates in m
 see Haversine formula, earth radius: 6.371km depending on latitude
 https://rechneronline.de/erdradius/
 6365.692 optimized for 50° latitude and 60m altitude }
 
function DistanceBetweenTwoCoordinates(const lat1, lon1, lat2, lon2: double): double;
const
  GeoidRadius=6365692;
// EARTHS_RADIUS_IN_METERS = 6378137;
  
var
  hw: double;

begin
  result:=0;
  try
    if (lat1<>lat2) or (lon1<>lon2) then begin
      hw:=sin(lat1*pi/180)*sin(lat2*pi/180)+
          cos(lat1*pi/180)*cos(lat2*pi/180)*cos((lon1-lon2)*pi/180);
      if not (hw<=1.0) then
        hw:=1.0;                     {must not be >1}
      result:=GeoidRadius*arccos(hw);
      if (result>30000) or           {> 30km --> unplausible Werte identifizieren}
         (result<0.005) then                         {Fehler reduzieren, Glättung}
        result:=0;
    end;
  except
    result:=0;
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

procedure BladeCol(aGrid: TStringGrid; fm: integer); {Flight mode to color assignement}
begin
  case fm of                                       {flight modes; wie Chart1BarSeries}
    25:     CellColorSetting(aGrid, clAngle);
    11:     CellColorSetting(aGrid, clSport);
    9, 14:  CellColorSetting(aGrid, clRTH);
    10, 13: CellColorSetting(aGrid, clAcro);
    12:     CellColorSetting(aGrid, clSmart);
    8:      CellColorSetting(aGrid, clEmergency);
    5:      CellColorSetting(aGrid, clSilver);     {Motor Starting}
  end;
end;

procedure LegacyCol(aGrid: TStringGrid; fm: integer); {Flight mode to color assignement}
begin
  case fm of                                       {flight modes wie Chart1BarSeries}
    3, 4:                  CellColorSetting(aGrid, clAngle);
    2, 5, 7, 22, 24, 32:   CellColorSetting(aGrid, clNoGPS);
    13, 14:                CellColorSetting(aGrid, clRTH);
    20:                    CellColorSetting(aGrid, clAcro);
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
    0:  CellColorSetting(aGrid, clSport);          {Stabilized blue}
    1:  CellColorSetting(aGrid, clNoGPS);          {Altitude orange}
    3:  CellColorSetting(aGrid, clAngle);          {Position purple}
    12: CellColorSetting(aGrid, clEmergency);      {Emergency maroon}
    13: CellColorSetting(aGrid, clRTH);            {RTH other red}
    20: CellColorSetting(aGrid, clAcro);           {Rattitude/Rate red}
    33: CellColorSetting(aGrid, clSmart);          {Mission green}
    16: CellColorSetting(aGrid, clSilver);         {Ready silver}
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

function testh(const a: double): boolean; inline;  {Datensätze mit unsinniger Höhe ausblenden}
begin
  result:=true;
  if (a<minnh) or                                  {tritt bei VTH Plus als Fehler auf}
     (a>maxxh) then result:=false;
end;

function DefaultOuputToAppLog(zhl: integer; time: TDateTime; text: string): string;
begin
  result:=Format('%6d', [zhl])+tab2+FormatDateTime(zzf+zzz, time)+tab2+text;
end;

function FilterColumn(var aGrid: TStringGrid;
                      const aCol: integer; aText: string): integer;
var
  i: integer;

begin
  result:=0;
  aGrid.BeginUpdate;
  try
    for i:=aGrid.FixedRows to aGrid.RowCount-1 do begin
      if aGrid.Cells[aCol, i]=aText then begin
        aGrid.RowHeights[i]:=aGrid.DefaultRowHeight;
        result:=result+1;
      end else
        aGrid.RowHeights[i]:=0;
    end;
  finally
    aGrid.EndUpdate;
  end;
end;

procedure ResetAllFilterColumn(var aGrid: TStringGrid);
var
  i: integer;

begin
  aGrid.BeginUpdate;
  try
    for i:=aGrid.FixedRows to aGrid.RowCount-1 do
      aGrid.RowHeights[i]:=aGrid.DefaultRowHeight;
  finally
    aGrid.EndUpdate;
  end;
end;

end.

