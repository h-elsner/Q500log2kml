unit q5_common;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, lazUTF8, math;

const
{public constants}
  Version ='V4.5 10/2020';

  InvalidChars: set of char=['\', '/', ':', '*', '?', '"', '<', '>', '|', '&'];
  ziff=['0'..'9'];                                 {gültige Ziffern}
  tab1=' ';                                        {ein Leerzeichen}
  tab2='  ';
  tab4='    ';
  tab6='      ';

  clOrange=$000080FF;
  clNoGPS=$000080FF;                               {Dark Orange}
  clAngle=clFuchsia;
  clEmergency=clMaroon;
  clSmart=clGreen;
  clRTH=clRed;
  clSport=clBlue;
  clTasks=clMoneyGreen;
  clAttention=$008080F0;                           {Farbe Achtung}
  clVolt2    =$00FF901E;                           {Voltage 2 Farbe}


{Public functions and procedures}

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

implementation

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
begin                  {formatiert einen string in Länge t zur Ausgabe}
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

end.

