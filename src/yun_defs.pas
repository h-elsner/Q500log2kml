unit yun_defs;                                     {Yuneec definitions and variables}

{$mode objfpc}{$H+}

interface

uses
  sysutils, q5_common;

const
  mndir='FlightLog';
  mndirp='Flight2Log';                             {YTH Plus}
  dkpath='Telemetry';
  kfile=dkpath+'_';
  spath='RemoteGPS';
  sfile=spath+'_';
  fpath='Remote';
  ffile=fpath+'_';
  npath='Sensor';
  mfile='yuneec_';                                 {Variante beim Mantis Q/bext}
  nfile=npath+'_';

  fext='.csv';                                     {alle csv-Dateien}
  bext='.log';                                     {Log files vom Breeze}
  hext='.tlog';                                    {TLOG from H520}
  sext='.bin';                                     {Sensor files}
  wext='.txt';
  skyext='.sky';

  rfm2=[0..7, 9..14, 18, 20..24, 26..29, 31..33];  {Real flight modes, Yuneec legacy}
  rfm3=[8..14, 25];                                {Real flight modes Blade}
  rfmT=[0, 1, 3, 13, 14, 20, 33];                  {Real flight modes Thunderbird}
  rfmP=[4..7, 10, 12, 13, 17];                     {Real flight modes YTH Plus  ???}

  stkang =2184;                                    {switch tilt angle, 10%}
  stkntrl=2048;                                    {neutral; 0%}
  stkdown=683;                                     {-100%}
  stkup  =3412;                                    {+100%}
  stkmax =4095;                                    {+150%}
  stkmin=0;                                        {-150%}
  m45val=1433;                                     {Pan -40% TeamMode}
  p50val=2730;                                     {+50%}
  m50val=1365;                                     {-50%}

  hwphwp ='"way_points';
  hwpyaw ='"yaw';
  hwproll='"roll';
  hwpidx ='"pointerIndex';
  hwpalt ='"altitude';
  hwplon ='"longitude';
  hwplat ='"latitude';
  hwpptch='"pitch';
  hwpgyam='"gimbalYam';
  hwpgpit='"gimbalPitch';

  plfAndr='ndroid';                                {Breeze Platform, all other is iOS}
  brsnid='DroneSN';
  lcol='gps_accH';                                 {letzte Spalte in Überschrift, wegen ST16-FW Fehler}
  bind=' - ';                                      {von/bis ID}
  lipomin=3.3;                                     {Minimum LiPo voltage}
  lipomax=4.2;


{Public functions and procedures}
  function StkToProz(const w: double): integer;    {Stick Position to percent}
  function ProzToStk(const w: double): integer;
  function ChNoToStr(const p: byte): string;       {Bedeutung der Kanäle}
  function ChToStr(const s: string; const p: byte): string; {Channel CHxx umbenennen}
  function vTypeToStr(const v: byte): string;      {vehicle type ausgeben}
  function Mode350(const f: byte): string;         {Blade 350 QX}
  function ModeYTHP(const f: byte): string;        {neu YTH Plus}
  function ModeLegacy(const f: byte): string;      {Q500, YTH and all other legacy}
  function ModeThB(const f: byte): string;         {Thunderbird}
  function fmodeToStr(const fm: byte): string;     {Flight Mode abh. vom Typ ausgeben}
  function BRfmodeToStr(const f: byte): string;    {Flight Mode abh. vom Typ ausgeben}
  function IMUstatusToStr(const u: uint8): string; {imu_status}
  function CGPSToStr(const u: uint8): string;      {CGPS nur bei >H920 und <YTH}
  function BRIMUstatusToStr(const u: byte): string; {imu_status breeze}
  function AutoTakeOffToStr(const u: byte): string; {Breeze}
  function GetPropNum: byte;                       {Number props (4, 6) from vehicle type}
  function MotStatusToStr(const u: byte): string;  {Motor_status}
  function PCGstatusToStr(const u, vt: byte): string; {pressure_compass_status}
  function eflagToStr(const s: string): string;    {Error Flags}
  function LandGearToStr(const s: string): string; {Fahrwerkseinstellung}
  function SwitchToStr(const p, vt: byte; const s: string): string;
  function StickPos(const w: double): string;      {Stickposition in %}
  function StickToStr(const p: byte; const s: string): string;
  function KoToStr(const lanlon: double): string; inline;
                                                   {Koordinaten zu String}
  function ChrKoor(ko: string): string; inline;    {Korrigiert Format der Koordinaten}
  function KorrSigned(const s: string; maske: byte=$FF): string; {String in unsigned String}
  function BrCoordFormat(const c: string): string; {Koordinaten Breeze formatieren}
  function BrCoordToFloat(const c: string): double;     {Koordinaten Breeze in Float}
  function BrGPSfix(const s: string): boolean;     {GPS Fix ID für Breeze}
  function BrTeilen(const s: string; const k: integer): string;
  function H920Amp(const w: double): double; inline;    {Stromsensor H920}
  function TiltToGrad(const w: double): double; inline; {Umrechnung Werte in 0-90 Grad}
  function BrUmrech(const w: double): double; inline;   {Umrechnung unsicher}
  function BrKorrV(const v: string): string;       {Spannung in % beim Breeze}
  function VtoProzY(const vt: byte; const u: double): integer; {vehicle_type, Spannung in %}
  function VtoProzRC(const vt: byte; u: double): integer;
  function brTransformW(const inx: byte; const w: double): double;
  function RadToStr(const s: string): string;      {Camera tilt slider}

{$I language.inc}

implementation

function StkToProz(const w: double): integer;      {Stick Position to percent}
begin
  result:=round(w/stkmax*300)-150;
end;

function ProzToStk(const w: double): integer;
begin
  result:=round(w*stkmax/300)+stkntrl;
end;

function ByteToBin(w: byte): string;               {Byte to Binary string}
var x: integer;

begin
  SetLength(result, 8);
  for x:=1 to 8 do begin
    result[x]:=char(ord('0')+(w shr 7));
    inc(w, w);
  end;
  insert(tab1, result, 5);
end;

function ChNoToStr(const p: byte): string;         {Bedeutung der Kanäle}
begin
  result:='';                                      {default: leer}
  case p of
     1: result:='J1 (thr)';
     2: result:='J4 (roll/ail)';
     3: result:='J3 (pitch/ele)';
     4: result:='J2 (yaw/rud)';
     5: result:=rsFModeSw;
     7: result:=rsK2+' [°]';
     8: result:=rsK1;
     9: result:='Tilt mode';
    10: result:='Pan mode';
    11: result:=rsLandGear;
    12: result:='B2 (Aux)';
  end;
end;

function ChToStr(const s: string; const p: byte): string; {Channel CHxx umbenennen}
begin
  result:=s;                                       {Nehmen, wie es kommt - CHxx}
  {wenn S leer ist, dann Channel wie in Channel settings in ST16:
   Chxx, xx beginnend mit 1 statt 0, wie im FlightLog - Remote}
  if s='' then
    result:='Ch'+IntToStr(p);

  case p of                                        {Liste der Channels zum Umbenennen}
    1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12: result:=result+bind+ChNoToStr(p);
    6: result:=result+' - A02';   {alle unbezeichneten, einstelligen Channels}
    else
      result:=result+' - A'+IntToStr(p-4);         {die restlichen unbezeichneten}
  end;
end;

{see DroneTypeFactory.java
 https://www.rc-drohnen-forum.de/thread/10002}
function vTypeToStr(const v: byte): string;        {vehicle type ausgeben}
begin
  result:='';
  case v of
    0: result:=rsInvalid;
    1: result:='Yuneec H920';
    2: result:='Yuneec Q500';
    3: result:='Blade 350QX';
    4: result:='Blade Chroma (380QX)';
    5: result:='Yuneec Typhoon H';
    6: result:='Yuneec H920+';                     {vermutlich nie genutzt}
	20..29: result:='SR24 car';
	30..39: result:='SR24 boat';
    brID: result:='Yuneec Breeze';                 {selbst bestimmte Typ-IDs, 10..14, 90, 91}
    MQid, MQcsvID: result:='Yuneec MantisQ';       {MantisQ erkannt}
    H5id: result:='Yuneec H520';                   {tlog files from H520}
    YTHPid: result:='Yuneec Typhoon H Plus';       {YTH Plus erkannt}
    ThBid: result:=capThunder;                     {H480 Thunderbird}
    H501ID: result:='Tom''s flight data recorder for Hubsan';  {flaretom Hubsan Recorder}
  end;
end;

function Mode350(const f: byte): string;           {Blade 350 QX}
begin
  result:='';
  case f of
     3: result:=rsWaitRC;
     4: result:=rsInitializing;
     5: result:=rsMotorStarting;
     8: result:=rsEmergency;
     9: result:=fmRTH+tab1+rsLanding;
    10: result:=fmAgility+GPSoff;
    11: result:=fmStability;
    12: result:=fmSmart;
    13: result:=fmAgility;
    14: result:=fmRTH+tab1+rsComing;
    17: result:=rsMagCali+rsCali;
    23: result:=rsBinding;
    25: result:='AP mode';
  end;
end;

function ModeYTHP(const f: byte): string;          {neu YTH Plus}
begin
  result:='';
  case f of
    4: result:=fmManual;                           {ic_drone_flying_mode_m}
    5: result:=fmAngle;                            {ic_drone_flying_mode_a}
    6: result:=fmSmart;                            {ic_drone_flying_mode_smart}
    7: result:=fmSportMode;                        {ic_drone_flying_mode_sport}
    8: result:='Flight mode 8';
   10: result:='Flight mode 10';                   {IPS mode ?, no GPS ?}
   12: result:=fmRTH+'12';
   13: result:=fmRTH+'13';
   17: result:='GPS lost';                         {Really ?}
  end;
end;

function ModeLegacy(const f: byte): string;        {Q500, YTH and all other legacy}
begin
  result:='';
  case f of
     0: result:=fmStability;
     1: result:='Blue flashing'+GPSoff;
     2: result:='Blue'+GPSlost;
     3: result:=fmAngle+' (Purple solid)';
     4: result:=fmAngle+' (Purple flashing)'+GPSoff;
     5: result:=fmAngle+' (Purple solid)'+GPSlost;
     6: result:=fmSmart;
     7: result:=fmSmart+GPSlost;
     8: result:=rsMotorStarting;
     9: result:='Temperature'+rsCali;
    10: result:='Pressure'+rsCali;
    11: result:='Accelerometer bias'+rsCali;
    12: result:=rsEmergency;
    13: result:=fmRTH+tab1+rsComing;
    14: result:=fmRTH+tab1+rsLanding;
    15: result:=rsBinding;
    16: result:=rsInitializing;                    {Ready to start}
    17: result:=rsWaitRC;
    18: result:=rsMagCali+rsCali;
    19: result:=rsUnknown;
    20: result:=fmAgility;                         {Rate}
    21: result:=fmSmart+' - Follow me';
    22: result:=fmSmart+' - Follow me'+GPSlost;
    23: result:=fmSmart+' - Camera tracking';
    24: result:='Camera tracking'+GPSlost;
    26: result:='Task Curve Cable Cam';
    27: result:='Task Journey';
    28: result:='Task Point of Interest';
    29: result:='Task Orbit';
    32: result:='IPS';                             {FMODE_ANGLE_MODE_IPS_ONLY:I = 0x20}
    33: result:='Waypoints';
  end;
end;

function ModeThB(const f: byte): string;           {Thunderbird}
begin
  result:='';
  case f of                                        {Overwrite for Thunderbird}
    0: result:=fmStabilized;
    1: result:=fmAltitude+' or '+fmStabilized;     {??}
    3: result:=fmPosition;
    8: result:='GPS Aquiring';
    13: result:=fmRTH;
    16: result:='Ready';                           {Ready to start}
    20: result:=fmRattitude;
    33: result:=fmMission;
  end;
end;


function fmodeToStr(const fm: byte): string;       {Flight Mode abh. vom Typ ausgeben}
begin
  case v_type of
    3:      result:=Mode350(fm);
    YTHPid: result:=ModeYTHP(fm);
    ThBid:  result:=ModeThB(fm);
  else
    result:=ModeLegacy(fm);
  end;
end;

function BRfmodeToStr(const f: byte): string;      {Flight Mode abh. vom Typ ausgeben}
begin                                              {für Yuneec Breeze}
  case f of
     2: result:='Selfie';
     3: result:='No task selected';
     4: result:='Follow me';
     5: result:='Jouney';
     6: result:='Pilot';
     7: result:='Orbit';
     8: result:='RTH';
     10: result:='Pilot'+GPSoff;
    else result:='Mode '+IntToStr(f);
  end;
end;

{from  DroneStatusParserUtil.java

int imuInt = data[34];
int imuModuleStatus = imuStatus & 1;
int geomagneticModuleStatus = (imuStatus & 2) >> 1;
int geomagneticDataStatus = (imuStatus & 4) >> 2;
int infraredModuleStatus = (imuStatus & 8) >> 3;
int infraredDataStatus = (imuStatus & 16) >> 4;
int ipsModuleStatus = (imuStatus & 32) >> 5;
int ipsDataStatus = (imuStatus & 64) >> 6;
byte baroMagByte = data[35];

}
function IMUstatusToStr(const u: byte): string;   {imu_status}
begin
  result:=rsUndef+tab1+IntToStr(u)+' = '+ByteToBin(u);
  case u of
      1: result:='IMU';
     33: result:='IMU+GPS';
     65: result:='IMU+C-Compass';
     96: result:='GPS+C-Compass+IMU missing';
     97: result:='IMU+GPS+C-Compass';
    101: result:='IMU+GPS+Compass+C-Compass';      {TH}
    193: result:='IMU+C-GPS/Compass';
    225: result:='IMU+GPS+C-GPS/Compass';          {Q500, TH}
    229: result:=rsAllOK;                          {TH}
    241: result:='IMU+GPS+Sonar+C-GPS/Compass on'; {TH}
    245: result:=rsAllOK+'+Sonar';                 {TH}
    231: result:='UAV-Pilot';                      {Simulator}
    255: result:=rsAllSet;
    else begin                                     {fehlende Bits interpretieren}
      result:='';                                  {unbekannt, nichts ausgeben}
      if (u and 1)=0 then
        result:=result+'IMU fail ';
      if (u and 2)=0 then
        result:=result+'Baro fail ';
      if (u and 4)=0 then
        result:=result+'Compass fail ';
      if (u and 8)=0 then
        result:=result+'Compass2 fail ';
      if (u and 16)=0 then
        result:=result+'Sonar off ';               {OK, in FlightModeApp same check}
      if (u and 32)=0 then
        result:=result+'GPS off ';
      if (u and 64)=0 then
        result:=result+'C-Compass off ';
      if (u and 128)=0 then
        result:=result+'C-GPS off';
    end;
  end;
end;

function CGPSToStr(const u: byte): string;         {CGPS nur bei >H920 und <YTH}
begin
  result:='';
  case u of    {die obersten 3 bits vom IMU Status}
    0: result:='C-GPS/Compass off';
    1: result:='C-GPS off';
    2: result:='C-Compass off';
    3: result:='C-GPS/Compass';
  end;
end;

function BRIMUstatusToStr(const u: byte): string;  {imu_status breeze}
begin
  result:=rsUndef+tab1+IntToStr(u)+' = '+ByteToBin(u);
  case u of                                        {Rest ist unbekannt}
    255: result:=rsAllOK;
  end;
end;

function AutoTakeOffToStr(const u: byte): string;  {Breeze}
begin
  result:=IntToStr(u);
  case u of
     0: result:='On the ground';
     1: result:='Take off';
     2: result:='Flying';
    16: result:='Self landing';
    18: result:='Pilot landing';
  end;
end;

{from  DroneStatusParserUtil.java
int baroMagInt = data[35];
int barometerModuleStatus = baroMagByte & 1;
int gpsModuleStatus = (baroMagByte & 2) >> 1;

Für PX4: telemetrie definitionen
https://github.com/PX4/Firmware/blob/master/src/lib/rc/st24.h
}
function PCGstatusToStr(const u, vt: byte): string; {pressure_compass_status}
begin
  result:='';
  case u of
    21: result:='Baro+Compass+GPS';
    63: result:='Baro+Compass+GPS+RealSense';
    81: result:='Baro+GPS+Sonar';
    85: result:='Baro+Compass+GPS+Sonar';
    117: result:='Baro+Compass+GPS+Sonar+RealSense';
    245: result:=rsAllSet;                         {YTH + RS -- bit 7 unbekannt, IPS?}
    else begin                                     {fehlende Bits interpretieren}
      result:='';
      if (u and 1)=0 then
        result:=result+'Baro fail ';
      if (u and 4)=0 then
        result:=result+'Mag fail ';
      if (u and 16)=0 then
        result:=result+'GPS fail ';
      if (vt=5) or
         (vt=YTHPid) then begin                    {nur Typhoon H oder H Plus}
        if (u and 32)=0 then
          result:=result+'RealSense error';
        if (u and 64)=0 then
          result:=result+'Sonar error';
      end;
    end;
  end;
  if result='' then
    result:=rsUndef+tab1+IntToStr(u)+' = '+ByteToBin(u);
end;

function GetPropNum: byte;                         {Number props (4, 6) from vehicle type}
begin
  case v_type of
    2..4, BrID, MQid, MQcsvID, H501ID: result:=4;
  else
    result:=6;
  end;
end;

function MotStatusToStr(const u: byte): string;    {Motor_status}
var
  p: integer;                                      {Number props (4, 6)}

begin
  result:='';
  p:=GetPropNum;
  case u of
    15: if p=4 then
             result:=rsAllOK;
    63, 255: result:=rsAllOK;
    else begin
      if (u and 1) =0 then
        result:=result+'Motor 1 off ';
      if (u and 2) =0 then
        result:=result+'Motor 2 off ';
      if (u and 4) =0 then
        result:=result+'Motor 3 off ';
      if (u and 8) =0 then
        result:=result+'Motor 4 off ';

      if p=6 then begin                            {Hexakopter}
        if (u and 16)=0 then
          result:=result+'Motor 5 off ';
        if (u and 32)=0 then
          result:=result+'Motor 6 off';
      end;
    end;
  end;
end;


{MAX_TIME_COMPASS_THRESHOLD = 3000

from  DroneStatusParserUtil.java
int voltage1stWarning = errorFlagByte & 1;
int voltage2stWarning = (errorFlagByte & 2) >> 1;
int motorErrorWarning = (errorFlagByte & 4) >> 2;
int ultrasonicErrorWarning = (errorFlagByte & 8) >> 3;
int mainboardTemperatureHighErrorWarning = (errorFlagByte & 16) >> 4;
int calibrationErrorWarning = (errorFlagByte & 32) >> 5;
int mainboardTemperatureLowErrorWarning = (errorFlagByte & 64) >> 6;
int noFlyZoneErrorWarning = (errorFlagByte & 128) >> 7;


private float LEVEL1_WARNING = 14.2f;
private float LEVEL2_WARNING = 14.0f;
private float MAX_VOLTAGE = 15.4f;
private float MID_VOLTAGE = 14.9f;
private int droneType = 5;
private String droneTypeName = FModeData.VEHICLE_TYPE_480_NAME;
}
function eflagToStr(const s: string): string;      {Error Flags}
var e: integer;

begin
  result:='';
  e:=StrToIntDef(s, 1024);       {bit nicht belegt, result leer}
  if e>0 then begin
    if (e and 1) >0 then
      result:=result+'Low Voltage Warning 1 ';
    if (e and 2) >0 then
      result:=result+'Low Voltage Warning 2 ';
    if (e and 4) >0 then
      result:=result+'Motor Failsafe Mode ';
    if (e and 8) >0 then
      result:=result+'Complete Motor ESC Failure ';  {Ultrasonic error warning?}
    if (e and 16)>0 then
      result:=result+'Temperature Warning ';       {temp high}
    if (e and 32)>0 then
      result:=result+'Compass Calibration Warning ';
    if (e and 64)>0 then
      result:=result+'Fly-away Checker Warning ';  {temp low}
    if (e and 128)>0 then
      result:=result+'Airport Warning (NFZ) ';
  end else
    result:=rsAllOK;
end;

function LandGearToStr(const s: string): string;   {Fahrwerkseinstellung}
var stk: integer;

begin
  result:='';
  stk:=trunc(StrToFloatN(s));
  if stk=stkmin then
    result:=rsLandGear+tab1+rsUp;
  if (stk=1) or
     (stk=stkmax) then
    result:=rsLandGear+tab1+rsDown;
end;

function SwitchToStr(const p, vt: byte; const s: string): string;
var stk: integer;

begin
  result:='';
  stk:=trunc(StrToFloatN(s));
  case p of
    5: begin                                       {Flight mode switch}
         case stk of                               {default}
           stkup:   result:=fmSmart;
           stkntrl: result:=fmAngle;
           stkdown: result:=fmRTH;
         end;
         case vt of                                {Overwrite}
           ThBid: case stk of
                    0..stkdown: result:=fmRTH;     {-150 .. -100%}
                    m50val:  result:=fmStabilized; {-50%}
                    stkntrl: result:=fmPosition;   {0%}
                    p50val:  result:=fmRattitude;  {+50%}
                    stkup:   result:=fmAltitude;   {+100%}
                  else
                    result:='Unknown flight mode for '+IntToStr(stk)+'%';
                  end;
           YTHPid: if stk=stkup then
                     result:=fmSportSmartMode;
         end;
       end;
    6: begin
         case stk of                               {Default, YTH}
           stkntrl: result:=rsNeutral;
           stkmax:  result:=fmRTH;
         end;
         case vt of                                {abhängig vom vehicle type}
           1: case stk of                          {H920}
                p50val: result:=rsNoGPS;
                stkup:  result:=rsNeutral;
              end;
           2: if stk=stkup then                    {Q500}
                result:=fmRTH;

         end;
       end;
    9: begin                                       {S1 Gimbal Tilt mode}
         if stk=stkup then
           result:='Velocity mode';
         if stk=stkang then
           result:=fmAngle;
         if stk=stkntrl  then
           result:=rsNeutral;
       end;
   10: begin                                       {S2 Gimbal Pan mode}
         if stk=stkup then
           result:='Global mode';
         if stk=stkntrl then
           result:=rsNeutral;
         if stk=m45val then
           result:=rsTeam;
         if stk=1502 then
           result:='Controllable mode';
         if stk=stkdown then
           result:='Follow mode';
       end;
  end;
end;

{Flightmodes:
stk   %      YTH    Thunderbird
-------------------------------
4095 +150%    -         -
3412 +100%   Smart  Altitude
2730  +50%    -     Rattitude        p50val
2048    0%   Angle  Position
1365  -50%    -     Stabilized       m50val
 683 -100%   RTH    RTH
   0 -150%    -     RTH
}

function StickPos(const w: double): string;        {Stickposition in %}
begin
  result:='';
  if round(w)=stkntrl then
    result:=rsNeutral
  else
    result:=IntToStr(StkToProz(w))+'%';
end;

function StickToStr(const p: byte; const s: string): string;
var w: double;

begin
  result:='';
  try
    w:=StrToFloat(s);
  except
    exit;
  end;
  case p of
    1: if round(w)=0 then
         result:='Motor on/off (B3)'
       else
         result:='J1 (thr)'+suff;
    2: result:='J4 (roll)'+suff;
    3: result:='J3 (pitch)'+suff;
    4: result:='J2 (yaw)'+suff;
  end;
  result:=result+StickPos(w);
end;

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

function KorrSigned(const s: string; maske: byte=$FF): string; {String in unsigned String}
var b: byte;                                       {Maske default: FF}

begin
  result:=s;
  b:=StatusToByte(trim(s));
  if b>0 then begin
    if maske>0 then b:=(b and maske);
    result:=IntToStr(b);
  end;
end;

function BrCoordFormat(const c: string): string;   {Koordinaten Breeze formatieren}
begin
  result:=c;
  if c.length>6 then
    insert('.', result, c.length-6);
end;

function BrCoordToFloat(const c: string): double;  {Koordinaten Breeze in Float}
begin
  result:=StrToFloatN(BrCoordFormat(c));
end;

function BrGPSfix(const s: string): boolean;       {GPS Fix ID für Breeze}
var e: integer;

begin
  e:=StrToIntDef(s, 0);
  result:=(e and 128)>0;                           {HWT bit}
end;

function BrTeilen(const s: string; const k: integer): string;
begin                                              {k: number digits after decimal point}
  result:=FloatToStrF(StrToFloatN(trim(s))/100, ffFixed, 8, k);
end;

function H920Amp(const w: double): double; inline; {Stromsensor H920}
begin
  result:=w/2;                                     {uint8_t current; 0.5A resolution?; see st24.h}
end;

function TiltToGrad(const w: double): double; inline; {Umrechnung Werte in 0-90 Grad}
begin
  result:=-(w-683)*90/2729;
end;


{eventuell auch so: uint8_t voltage; 25.4V  voltage = 5 + 255*0.1 = 30.5V, min=5V}
function BrUmrech(const w: double): double; inline; {Umrechnung unsicher}
begin
  result:=w/2.55;                                  {0..255}
end;

function BrKorrV(const v: string): string;         {Spannung in % beim Breeze}
var w: double;

begin
  try
    w:=StrToFloatN(trim(v));
    result:=IntToStr(round(BrUmrech(w)));
  except
    result:=v;
  end;
end;

{aus Yuneec Source code: LiPo Spannung in % Restkapazität}
function VtoProzY(const vt: byte; const u: double): integer; {vehicle_type, Spannung in %}
const s61=23.9;                                    {Schwellwerte 6S}
      s62=21.7;
      s63=21.3;
      s64=21.1;

      s41=14.9;                                    {Schwellwerte 4S}
      s42=14.2;
      s43=14.0;
      s44=13.8;

      s31=10.7;                                    {Schwellwerte 3S}
      s32=10.5;
      s33=10.3;

      s21=7.2;                                     {Schwellwerte 2S   ???}
      s22=7.0;
      s23=6.8;

var   m: double;                                   {Maximale Batteriespannung}

begin
  result:=0;                                       {default Unterspannung=0%}
  case vt of
    1, 6: begin                                    {H920}
         m:=6*lipomax;                             {Maximale LiPo Spannung 6S}
         if  u>=m then
            result:=100;
         if (u>=s61) and
            (u< m)   then
           result:=round((((u-s61)* 5)/(m-  s61))+95);
         if (u> s62) and
            (u< s61) then
           result:=round((((u-s62)*75)/(s61-s62))+20);
         if (u> s63) and
            (u< s62) then
           result:=round((((u-s63)* 5)/(s62-s63))+5);
         if (u> s64) and
            (u<=s63) then
           result:=round( ((u-s64)* 5)/(s63-s64));
       end;
    5, YTHPid, ThBid: begin                        {YTH / YTH Plus / Thunderbird}
         m:=4*lipomax;                             {4S}
         if  u>=m then
           result:=100;
         if (u>=s41) and
            (u< m)   then
           result:=round((((u-s41)*50)/(m-  s41))+50);
         if (u> s42) and
            (u< s41) then
           result:=round((((u-s42)*25)/(s41-s42))+25);
         if (u> s43) and
            (u< s42) then
           result:=round((((u-s43)*20)/(s42-s43))+5);
         if (u> s44) and
            (u<=s43) then
           result:=round( ((u-s44)* 5)/(s43-s44));
       end;
     H501ID: begin
               m:=2*lipomax;                       {2S}
               if  u>=m then
                 result:=100;                      {100%}
               if (u>=s21) and
                  (u< m)   then
                 result:=round((((u-s21)*75)/(m-s21))+25);
               if (u> s22) and
                  (u< s21) then
                 result:=round((((u-s22)*20)/(s21-s22))+5);
               if (u> s23) and
                  (u<=s22) then
                 result:=round( ((u-s23)* 5)/(s22-s23));
             end;
  else begin                                       {alle anderen 3S Kopter}
     m:=3*lipomax;                                 {3S}
     if  u>=m then
       result:=100;                                {100%}
     if (u>=s31) and
        (u< m)   then
       result:=round((((u-s31)*75)/(m-  s31))+25);
     if (u> s32) and
        (u< s31) then
       result:=round((((u-s32)*20)/(s31-s32))+5);
     if (u> s33) and
        (u<=s32) then
       result:=round( ((u-s33)* 5)/(s32-s33));
    end;
  end;
end;

{The relationship of voltage and capacity from RC-Groups:
 https://blog.ampow.com/lipo-voltage-chart/
 https://www.rcgroups.com/forums/showpost.php?p=29431951}

function VtoProzRC(const vt: byte; u: double): integer;
const
  CapTab: array [0..20] of double = (
    100,  95,   90,   85,   80,   75,   70,   65,   60,   55,   50,
    45,   40,   35,   30,   25,   20,   15,   10,   5,    0);
  S1Tab: array [0..20] of double = (
    4.20, 4.15, 4.11, 4.08, 4.02, 3.98, 3.95, 3.91, 3.87, 3.85, 3.84,
    3.82, 3.80, 3.79, 3.77, 3.75, 3.73, 3.71, 3.69, 3.61, 3.27);

var
  i: integer;                                      {index in arrays}
  uz: double;                                      {Voltage down to 1S}

begin
  result:=100;
  case vt of                                       {Check numbers of cells per vehicle type}
    1, 6: uz:=u/6;                                 {6S H920}
    5, YTHPid, ThBid, H5id: uz:=u/4;               {4S driven}
    H501ID: uz:=u/2;                               {2S Hubsan}
  else
    uz:=u/3;                                       {3S all other}
  end;
  if uz<S1Tab[high(S1Tab)] then
    result:=0                                      {all below 3.27 = 0%}
  else
    if uz<s1tab[0] then begin                      {all above 4.2V = 100%}
      for i:=0 to high(CapTab) do begin            {find next threshold}
        if uz>S1Tab[i] then
          break;                                   {Voltage inbetween delta i-1 and i}
      end;
      result:=round(CapTab[i]+((CapTab[i-1]-CapTab[i])/
                              ((S1Tab[i-1]-S1Tab[i])/(uz-S1Tab[i]))));
    end;
end;

function brTransformW(const inx: byte; const w: double): double;
   {inx: Index of column
    w: Value to transform}
begin
  result:=w;                                       {default: in=out}
  case inx of
     3: result:=w/10;
     4: result:=w/10;
     6: result:=w/10;
     7: result:=w/10;
     8: result:=w/10;
     9: result:=w/100;                             {maxSpeed}
    10: result:=w/100;                             {altitude}
    15: result:=w/100;                             {roll}
    16: result:=w/100;                             {pitch}
    17: result:=w/100;                             {yaw}
    20: result:=round(w) and 31;                   {nsat}
  end;
end;

function RadToStr(const s: string): string;        {Camera tilt slider}
var w: double;

begin
  result:='';
  try
    w:=StrToFloat(s);
  except
    exit;
  end;
  result:='K1 (Camera tilt)'+suff+IntToStr(round(TiltToGrad(w)))+'°';
end;

end.
