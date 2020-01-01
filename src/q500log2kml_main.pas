          {********************************************************}
          {                                                        }
          {     Auswertung FlightLog Daten von Yuneec Koptern      }
          {                                                        }
          {       Copyright (c) 2015-2019    Helmut Elsner         }
          {                                                        }
          {       Compiler: FPC 3.0.4   /    Lazarus 1.8.2         }
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

  Auswertung der FlightLogDaten vom Yuneec Q500
  (Q500, H920, Typhoon H, Blade Chroma, Blade 350QX)

Die Daten liegen als *.CSV Dateien in einer fixen Struktur vor,
wenn man in die ST10 eine Speicherkarte einlegt:
---FlightLog
   +---Remote      Remote_00001.csv       Stickbewegungen, einige Schalter
   +---RemoteGPS   RemoteGPS_00001.csv    GPS Daten von der Funke (Controler)
   +---Telemetry   Telemetry_00001.csv    Telemetriedaten vom Kopter
   +---Sensor      Sensor_00001.bin       Sensordaten vom Kopter, Firmwarestände

Datenformat siehe Benutzerhandbuch, Anhang
------------------------------------------
Source ST10+      https://github.com/azvampyre/st10-v01b31c

KML file generation: https://developers.google.com/kml/documentation/
YTH waypoints: http://www.eegalmc2.com/us/typhoonh/

Hilfsvariablen:
Form1.Tag:        Original Dezimal Separator zwischenspeichern (ob das nötig ist?)
BitBtn1.Tag:      1=Breeze telemetry als ft, sonst Meter
BitBtn3.Tag:      Plattform for Breeze: 1: Android, 0: iOS
StringGrid1.Tag:  Spaltennummer, wo f_mode steht.
StringGrid2.Tag:  Merker für zuletzt benutztes Tabsheet, gesetzt beim Umschalten
PageControl1.Tag: Indikator, ob Form2 angezeigt wurde (0 .. nein, >0 .. ja)
SpinEdit3.Tag:    Vehicle Type as Integer (default = 5, YTH Plus = 10,
                                           Breeze = 90 = brID)
Label3.Tag:       Spalte für Suche/Filter in Tabelle merken.
StatusBar1.Tag:   Legt fest, ob StatusPanel 3 und 4 (Zeiten) mit ins
                  Clipboard kopiert wird (>0: ja).
RadioGroup1.Tag:  Indicates if autosize columns is needed. 0 means yes.


Icon: Aus einem Bild, gemacht von der obersten Flugleitung
http://image.online-convert.com/convert-to-ico

Dank für Unterstützung und Erstellung der Mac OS X - Version an:
Frank Kieselbach
info@kieselbach.de
www.kieselbach.de

================================================================================

History:

2015-12-10  V0.1 First try to read telemetry, GUI created, import and data
                 consistence check.
2015-12-21  V1.0 KML or KMZ file creation, Elevation histogram
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
2016-05-04  V2.3 Kursor on histograms added, Support ST10 firmware
                 (only telemetry, no remote files)
2016-05-23  V2.4 Check how frequent telemetry data were sent. Support
                 Typhoon H, H920.
2016-07-18  V2.5 CCC Waypoints editor and conversation (editor later removed).
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
2018-07-02       Profiles as "Diashow" (3s).
2018-07-05       Sort table overview by click on column header.
                 Bugfix for Breeze: Support 'strange' coordinates in
                 Breeze telemetry.
2018-07-19       Sensor YTH Plus updated. Message ID and text messages
                 decoded.
                 Bug fix für menu item "Go to datapoint".
2018-07-24  V3.8 AppLog added, Text messages from Sensor files listed there.
                 MAV-Link Message IDs updated, Severity added to text messages.
2018-08-15       Typhoon H Plus Smart Mode and Manual Mode added.
2018-08-27	 Updates for Sensor files.
2018-09-14       SHARPNESS for CGO3+ updated, TeamMode at Gimbal Pan Mode.
2018-09-28	 Coordinates from GPS_RAW_INT in AppLog, Message names in Sensor
                 file display for Typhoon H Plus.
2018-10-02 V3.9  Mantis Q Support, H Plus sensor files renamed to PX4 sensor files
                 in menu. System health information from PX4 sensor files added
                 to AppLog.
2018-10-24       Identification of Mantis Q flylog files improved. Flight path
                 from PX4 sensor files as KML/KMZ or GPX file.
2018-10-28	 Show sensor files faster.
2018-10-29       Reverse geocoding removed, Bugfix: Mantis Q identification,
                 Check only Heartbeat from AUTOPILOT1
2018-11-03 V4.0  H520 *.tlog files support added similar to Mantis Q.
                 Batch conversion, button 'Convert' to KML/KMZ or GPX files
                 works now also for Mantis Q and H520.
                 Quick analysis filled: Voltage, current and SW-load.
                 Elevation histogram filled: Relative elevation and distance to first
                 coordinate.
2018-11-20       Vertical lines in KML/KMZ can be switched on (Extrude). Flight
                 times for Typhoon H Plus improved (Overview and Flight records).
2018-12-07       Added envelope to Elevation histogram.
2018-12-22       Crosshair Tool recreated and streamlined.
2019-01-09 V4.1	 PX Sensor data export to CSV file.
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
2019-09-04 V4.2  Option LiPo remaining capacity added.
2019-11-12       Scan for 'EMERGENCY' in PX4 sensor files
2019-12-10       Update für ST24/H920 alte Firmware (Telemetry ohne Header)
2019-12-31       Setting for Thunderbird (H480 with PX4 firmware)
*)

unit q500log2kml_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, LCLType,
  TATransformations, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, EditBtn, XMLPropStorage, Grids, Menus, lclintf, StdCtrls, Spin,
  Zipper, math, TAChartUtils, TAFuncSeries, Clipbrd, anzwerte,
  TACustomSeries, TATools, indGnouMeter, AdvLed, Sensors, graphutil,
  fphttpclient, lazUTF8, SynEdit, SynHighlighterMulti, SynHighlighterAny,
  strutils, dateutils, lazsysutils;

{$I q500_dt.inc}
{.$I q500_en.inc}

type
  TarrFW = array[0..7] of string;

  TWayPH = record
    yaw: double;
    roll: double;
    pindex: integer;
    altitude: double;
    lon: double;
    lat: double;
    pitch: double;
    gimbalYam: double;
    gimbalPitch: double;
  end;

  {TForm1: Hauptprogramm}

  TForm1 = class(TForm)
    AdvLed1: TAdvLed;
    BitBtn1: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;                             {Ausschneiden}
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn21: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    BitBtn27: TBitBtn;
    BitBtn28: TBitBtn;
    BitBtn3: TBitBtn;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1BarSeries2: TBarSeries;
    Chart1BarSeries3: TBarSeries;
    Chart1BarSeries4: TBarSeries;
    Chart1BarSeries5: TBarSeries;
    Chart1BarSeries7: TBarSeries;
    Chart1ConstantLine1: TConstantLine;
    Chart1ConstantLine2: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart3: TChart;
    Chart3LineSeries1: TLineSeries;
    Chart4: TChart;
    Chart4LineSeries1: TLineSeries;
    Chart5: TChart;
    Chart5LineSeries1: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    ChartToolset2: TChartToolset;
    ChartToolset2DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset2PanDragTool1: TPanDragTool;
    ChartToolset2ZoomMouseWheelTool1: TZoomMouseWheelTool;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    cbReduced: TCheckBox;
    cbCap: TCheckBox;
    cbThunder: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    ComboBox1: TComboBox;                          {Kopter ID - Hint}
    ComboBox10: TComboBox;                         {Schnellanalyse Profiles}
    ComboBox2: TComboBox;                          {FlightLog Directory}
    ComboBox3: TComboBox;                          {CGO3 Video}
    ComboBox4: TComboBox;                          {CGO3 Farbeinstellung}
    ComboBox5: TComboBox;                          {CGO3 Weißabgleich}
    ComboBox6: TComboBox;                          {CGO3 ISO}
    ComboBox7: TComboBox;                          {CGO3 Shutter}
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;                          {Find input}
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource2: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource3: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource4: TDateTimeIntervalChartSource;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Label9: TLabel;
    MenuItem57: TMenuItem;
    ProgressBar1: TProgressBar;
    RadioGroup10: TRadioGroup;
    RadioGroup8: TRadioGroup;
    RadioGroup9: TRadioGroup;
    SpeedButton5: TSpeedButton;
    StringGrid5: TStringGrid;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    TabImages: TImageList;
    indGnouMeter1: TindGnouMeter;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;            {"Als Bild speichern" im PopUp Menü HDiagr}
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem10: TMenuItem;                         {Menü GoTo Settings}
    MenuItem11: TMenuItem;                         {Menü Default Schnellanalyse}
    MenuItem12: TMenuItem;                         {Menü Profiles - no action}
    MenuItem13: TMenuItem;                         {Menü Profile FlightMode}
    MenuItem42: TMenuItem;                         {Menü Profile Errors}
    MenuItem43: TMenuItem;                         {Menü Profile GPS}
    MenuItem48: TMenuItem;                         {Menü Profile Throttle}
    MenuItem49: TMenuItem;                         {Menü Profile Pitch}
    MenuItem51: TMenuItem;                         {Menü Profile Roll}
    MenuItem52: TMenuItem;                         {Menü Profile Yaw}
    MenuItem53: TMenuItem;                         {Menü Profile 3Axis}
    MenuItem54: TMenuItem;                         {Menü Table Cut}
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;                         {Menü Sensor file from YTH Plus}
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;                        {Popup Menü Schellanalyse}
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    RadioGroup7: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    StaticText1: TStaticText;
    StatusBar1: TStatusBar;
    StopLightSensor1: TStopLightSensor;
    StringGrid1: TStringGrid;
    grdOverview: TStringGrid;
    StringGrid3: TStringGrid;
    StringGrid4: TStringGrid;
    TabSheet1: TTabSheet;                          {Übersicht}
    TabSheet10: TTabSheet;                         {Settings Analyse}
    TabSheet11: TTabSheet;                         {CGO3}
    TabSheet12: TTabSheet;                         {Settings Sonstige}
    TabSheet2: TTabSheet;                          {Höhendiagramm}
    TabSheet3: TTabSheet;                          {Datentabelle}
    TabSheet4: TTabSheet;                          {Settings}
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet8: TTabSheet;                          {Schnellanalyse}
    TabSheet9: TTabSheet;                          {Settings Konvert}
    Timer1: TTimer;                                {CGO3 Statusabfrage}
    Timer2: TTimer;
    Timer3: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TreeView1: TTreeView;
    XMLPropStorage1: TXMLPropStorage;
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);      {Ausschneiden}
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure cbThunderChange(Sender: TObject);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChartToolset2DataPointCrosshairTool1Draw(
      ASender: TDataPointDrawTool);
    procedure CheckBox11Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure ColorButton1Click(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure ComboBox10DblClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox2DblClick(Sender: TObject);
    procedure ComboBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure ComboBox8DblClick(Sender: TObject);
    procedure ComboBox8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit4DblClick(Sender: TObject);
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure Image4Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label7MouseEnter(Sender: TObject);
    procedure Label7MouseLeave(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label8MouseEnter(Sender: TObject);
    procedure Label8MouseLeave(Sender: TObject);
    procedure LabeledEdit1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LabeledEdit1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LabeledEdit2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LabeledEdit2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LabeledEdit3DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LabeledEdit3DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem40Click(Sender: TObject);
    procedure MenuItem41Click(Sender: TObject);
    procedure MenuItem42Click(Sender: TObject);
    procedure MenuItem43Click(Sender: TObject);
    procedure MenuItem45Click(Sender: TObject);
    procedure MenuItem48Click(Sender: TObject);
    procedure MenuItem49Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem50Click(Sender: TObject);
    procedure MenuItem51Click(Sender: TObject);
    procedure MenuItem52Click(Sender: TObject);
    procedure MenuItem53Click(Sender: TObject);
    procedure MenuItem56Click(Sender: TObject);
    procedure MenuItem57Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure StaticText1DblClick(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure StatusBar1Hint(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure StringGrid1Selection(Sender: TObject; aCol, aRow: Integer);
    procedure grdOverviewCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure grdOverviewDblClick(Sender: TObject);
    procedure grdOverviewHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure grdOverviewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure grdOverviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdOverviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdOverviewPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure StringGrid3KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid4HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGrid4KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid5DblClick(Sender: TObject);
    procedure StringGrid5KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid5Resize(Sender: TObject);
    procedure TabSheet8Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Click(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
    function CheckNumTurns(const dn: string): integer;
    procedure AnzeigeCSV(const mode: integer);
    procedure BrAnzeigeCSV(const mode: integer);
    procedure MQAnzeigeCSV(const mode: integer);   {Mantis Q CSV Format anzeigen}
    procedure HDiagramm(fn: string);
    procedure BrHDiagramm(fn: string);
    procedure Anzeige;
    procedure AnzeigeSchnell;
    procedure LoadTree;
    procedure Werte(z: integer);
    procedure ProtoWerte(fn: string;               {Dateiname}
                         olist: TStringList;       {Datensätze}
                     var fln: integer;             {Flugnummer}
                     var gflt: TDateTime;          {Flugzeit}
                     var gstr: double);            {Flugstrecke}
    procedure BrProtoWerte(fn: string;             {Dateiname}
                         olist: TstringList;       {Datensätze}
                     var fln: integer;             {Flugnummer}
                     var gflt: TDateTime;          {Flugzeit}
                     var gstr: double);            {Flugstrecke}
    procedure MacheDASH(z: integer);
    procedure MacheRR(z: integer);
    procedure MacheKMZ(fn: string);
    function TransformW(const fnr, inx: integer; const w: double): double;
    procedure EnSave;                              {Speichern erlauben}
    procedure Analyse;
    procedure GoToZ(a: integer);                   {Gehe zur Zeilennummer mit Abstand}
    procedure ZhLWerte(p: integer);                {Werte zählen}
    procedure DiaWerte(p: integer);                {Werte als Diagramm}
    function GethFromST10(const z: integer; const dt: TDateTime): double; {Höhe aus RemoteGPS_xxxx}
    function vms(const d: TDateTime; const w: double): string;
    procedure SelDirSet;
    procedure SelDirProt;
    procedure SelDirAct(const fn: string);         {Alles neu laden}
    procedure FirmwareLesen;
    procedure PlatformLesen;
    procedure YFlugBuch;
    procedure BrFlugBuch;
    function GetCellInfo(const sp, zl: integer): string; {Hint/Info per Zelle}
    procedure TabSuchen;                           {Suchen in Tabelle}
    procedure TabSelect;                           {Filtern in Tabelle}
    procedure SetStartP;
    procedure SetEndP;
    procedure MacheKML(fn: string; z: integer);
    procedure MacheGPX(fn: string; z: integer);
    procedure MacheCCC(fn: string);
    function ColorToKMLColor(const AColor: TColor): string;
    procedure DoForm2Show(p: integer);             {Anzeige zeigen, Breite mit p}
    procedure AnzeigeAddHist(Index: integer);      {Anzeige zusätzlicher Infos}
    procedure ScreenToBild(fn: string);            {Screenshot}
    function CGO3run(const CGO3cmd: string; const CGO3act: integer): integer; {CGO3 ansprechen}
    function GetFW(var fwout: TarrFW): integer;    {Anzahl FW items}
    procedure KursorAus;                           {Fadenkreuz aus}
    procedure GetDDdata(lab: TLabeledEdit);        {Wert für Schnellanalyse übergeben}
    procedure SetProfile(idx: integer);            {Profile ausgewählt}
    procedure FreigabeCut(a: boolean);             {Freigabe und Anzeige Cut}
    procedure StatusToClipboard;                   {Statuszeile kopieren}
    procedure ReadMAV(const mode: integer);        {Sensor Datei auslesen}
    function fModeFinden(l: TStringList): boolean; {FlightMode Spalte finden}
    function CheckVT(vt, fm: string): boolean;     {Vehicle Type prüfen für YTH Plus}
    function SpeedX(const sp: double): double;     {Geschwindigkeit umrechnen}
    function GetFlightLogDir(fn: string): string;  {FlightLog Verzeichnis finden}
    function vTypeToStr(const v: integer): string; {vehicle type ausgeben}
    function ZeitToDT(const s: string; const vt: integer): TDateTime;
    function FindTP(wlist: TStringList; tp: TDateTime; const vt: integer): integer;
    function fmodeToStr(const f: integer): string; {Flight Mode abh. vom Typ ausgeben}
    function PCGstatusToStr(const u: integer): string; {pressure_compass_status}
    function SwitchToStr(const p: integer; const s: string): string;
    function CheckE7(const s: string): boolean;    {prüft einen string auf Fehler}
    function ShowSensorPlus(fn: string;            {Sensordatei vom YTH Plus}
                            z: integer;            {Index der Datei}
                            gx, tb, ov: boolean): boolean;  {True bei emergency}
    procedure ShowSensorH(const fn: string; mode: integer); {Sensor File YTH}
    function ComplFN(st: string; tp: TDateTime): string;    {Dateinamen mit Nummer ergänzen}
    procedure AppLogTimeStamp(s: string);          {AppLog einteilen}
    procedure SendCGOcmd;                          {Command zu CGO3}
    procedure Sharpness;                           {SHARNESS ermitteln}
    procedure OpenSensorPlus;                      {Sensordatei vom YTH Plus öffnen}
    function IsMantisQ(const fn: string): boolean; {Auf PX4 Quadcopter prüfen}
    procedure ShowMQ;                              {Anzeige SensorFile MQ}
    procedure ShowH520;                            {Anzeige TLOG File H520}
    procedure KMLheader(f: string; dt: TDateTime; klist: TStringList);
    procedure HDiaInit;                            {HöhenDiagramm Anzeige rücksetzen}
    procedure SetSensorEnv;                        {Bedienung für Sensor anpassen}
    procedure ResetSensorEnv;
    procedure AnzeigePX4CSV(fn: string);           {CSV aus eigenem Format anzeigen}
    procedure HeaderST24;                          {Write Header for H920 + ST24}
    function FakeHeader: string;                   {Missing Header for H920+ST24}
    procedure OverWriteVT;                         {Overwrite vehicle type for PX4 Thunderbird}
    procedure GetDefVT;                            {Fill defVT depending on settings}

  public
                                                   {public declarations}
    const 
      Version ='V4.2 12/2019';
  end;

const
  homepage='http://h-elsner.mooo.com';             {meine Homepage}
  hpmydat='/pdf/';
  meinname='Helmut Elsner';
  email   ='helmut.elsner@live.com';               {meine e-mail Adresse}
  lazURL  ='http://www.lazarus-ide.org/';          {Werbung}
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
  fix='Fix';
  wldcd='*';
  fext='.csv';                                     {alle csv-Dateien}
  bext='.log';                                     {Log files vom Breeze}
  hext='.tlog';                                    {TLOG from H520}
  sext='.bin';                                     {Sensor files}
  wext='.txt';
  skyext='.sky';
  us1='_1';
  sextP=wldcd+wext;                                {Suche Sensor files YTH Plus}
  pngdef=us1+'.png';                               {Dateivorschläge}
  csvdef=us1+fext;
  wexdef=us1+wext;
  plfAndr='ndroid';                                {Breeze Platform, all other is iOS}
  idtrue='true';
  sep=',';                                         {Datenseperator im CSV file}
  suff=': ';                                       {Suffix zur Datenausgabe}
  kma=', ';                                        {Kommaausgabe}
  bind=' - ';                                      {von/bis ID}
  ziff=['0'..'9'];                                 {gültige Ziffern}
  anzsp=20;                                        {Mindestanzahl Spalten}
  lzbr=19;                                         {Länge Zeitstempel Breeze}
  lzyu=21;                                         {Länge Zeitstempel Yuneec}
  fw0=80;                {Breite linke Spalte Übersicht default}
  fw1=160;               {Breite linke Spalte Übersicht bei alter FW}
  maxxh=7999;                                      {Höhe validieren --> testh(a)}
  minnh=-1000;           {gefähliche Annahme, tritt aber bei YTH Plus als Fehler auf}
  distmax=1000;                                    {Plasicheck: Koord dist in m}
  fmode='f_mode';        {Spalte Flightmode, Position der Spalte in StringGrid1.Tag}
  pfmode='fMode';        {Spaltenbezeichnung beim YTH Plus}
  brfmode='flightMode';  {FlightMode beim Breeze}
  brsnid='DroneSN';
  lcol='gps_accH';       {letzte Spalte in Überschrift, wegen ST16-FW Fehler}
  hsw=2;                 {Höhen-Schwellwert für Analyse in m}
  Secpd=86400;                                     {Sekunden per Tag}
  tsdelta1=6/864000;     {Schwellwert für Zeitstempel in Zehntelsekunden,
                          default 6=600ms}
  tsdelta2=2/86400;                                {> 2sec Telemtrie verloren}
  tsdelta3=5/86400;                                {5 sec für Breeze}
  minflt=10/86400;                                 {Mindestflugzeit beim YTH Plus 10s}
  rfm2=[0..7, 9..14, 18, 20..24, 26..29, 31..33];  {Real flight modes, Yuneec legacy}
  rfm3=[8..14, 25];                                {Real flight modes Blade}
  rfmP=[4..7, 10, 12, 13, 17];                     {Real flight modes YTH Plus  ???}
  rfmT=[3, 16];                                    {Real flight modes YTH Thunderbird}
  fkmh=3.6;                                        {m/s --> km/h}
  fmph=2.2369362920544;                            {m/s --> mph}
  fmile=1.609344;
  fft=0.3048;                                      {Faktor Umrechnung ft --> m}
  lipomin=3.3;          {minimale Zellenspannung von LiPos,
                         die nicht unterschritten werden sollte}
  lipomax=4.2;

  emcyID='EMERGENCY';

{http://docwiki.embarcadero.com/RADStudio/Seattle/de/Farben_in_der_VCL}
  osmURL='http://www.openstreetmap.org/';          {als Karte}
  gzoom='16';
  starticon='http://maps.google.com/mapfiles/dir_0.png';       {blau}
  stopicon='http://maps.google.com/mapfiles/dir_walk_60.png';  {grau}
  aircrafticon='http://earth.google.com/images/kml-icons/track-directional/track-0.png';
{Mapicons: https://sites.google.com/site/gmapicons/home/
           https://sites.google.com/site/gmapsdevelopment/
 Alternativen, aber oben gefällt mir besser}
//  starticon='http://maps.google.com/mapfiles/north.png';
//  stopicon='http://maps.google.com/mapfiles/south.png';
//  stopicon='http://maps.google.com/mapfiles/dir_60.png';     {blau}
//  aircrafticon='http://maps.google.com/mapfiles/kml/shapes/airports.png';

  idxpage='INDEX_PAGE';
  getshpn='GET_SHARPNESS';

  xmlvers='<?xml version="1.0" encoding="UTF-8"?>'; {ID XML/GPX header}
  kmlvers='<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">';
  gpxvers='<gpx xmlns="http://www.topografix.com/GPX/1/1" version="1.1"';
  GPXlat='lat="';
  GPXlon='" lon="';
  GPXele='<ele>';
  GPXet1='</wpt>';
  GPXet3='</trkpt>';
  KMLwhen='when>';
  amtag='altitudeMode>';
  pmtag='Placemark>';
  doctag='Document>';
  cotag='coordinates>';
  extru='<extrude>1</extrude>';
  tr='"'+suff;
  wpem='},';                                       {Waypoint Endemarke}
  tab1=' ';                                        {ein Leerzeichen}
  tab2='  ';
  tab4='    ';
  tab6='      ';

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

  minh=0.01;                                       {Höhengrenzen bei Übernahme}
  maxh=300;
  defh=10;                                         {default Höhe in m}
  stkntrl=2048;
  stkdown=683;
  stkup  =3412;
  stkmax=4095;
  stkmin=0;
  m45val=1433;                                     {Pan -45% TeamMode}
  spt=10;                    {zusätzliche Spaltenbreite  für Telemetrie in pix}
  rrk='# ';                  {RaceRender Kommentar}
  defaultcol=5;              {muss kleiner als die Mindestmenge der Spalten sein}
  brID=90;                   {Breeze ID}
  defVT=5;                   {Default vehicle YTH}
  YTHPid=10;                 {YTH Plus ID}
  MQid=11;                   {MantisQ ID, PX4 Quadcopter}
  H5id=12;                   {Yuneec H520, *.tlog files (PX4)}
  ThBid=15;                  {YTH Thunderbird on base of PX4}

  MQcsvID=13;                {neues CSV Format beim MQ}
  YTHPcols=276;              {Anzahl Spalten bei YTH Plus Sensorfiles = ID für Anzeige}
  AnzDirs=12;                {Anzahl der letzen Verzeichnisse}
  FWsz=18;                   {Mindestgröße Datei für FW}
  CGO3dir='100MEDIA/';
  CGO3cgi='cgi-bin/cgi?CMD=';
  trenner='--------------------';

  mzf='yyyy-mm-dd hh:nn';                          {Datum/Zeit Formate}
  dzf='yyyy-mm-dd';
  vzf=dzf+' '+zzf;
  zzz='.zzz';

  dzfl='0.0';                {Formatierung für FormatFloat}
  ctfl='0.00';
  mlfl='0.000';

  tabu=19;
  dstr=';';                  {Datentrenner für Flugprotokoll}
  lblsize=35;                {LabelSize zum Ausrichten der Y-Achsen bei Schnellanalyse}
                                                   {Sensordateien: }
  dsID=$BC;                                        {ID für einen Datensatz}
  lenfix=8;                                        {Länge Fixpart}
  dsIDP=$FD;                                       {ID für einen PX Datensatz}
  lenfixP=20;                                      {Länge Fixpart YTH Plus}
  HBlen=9;                                         {Länge Payload Heartbeat}
  csvanz=80;                                       {Anzahl Spalten bei PX4 CSV Datei}
  posChan=60;                                      {Startposition Spalten RC-Channels}
  deltayaw=15;                                     {Change of direction [°] for Waypoints}

var
  Form1: TForm1;
  tend, tpos, cutb, cute: TDateTime;        {Ende-Zeit, Zeit-Position in Tabellen}
  kpath: string;
  topp: array of array[0..6] of integer;    {Positionen Topzeile der Tabellen
  i..Index der Datei mit Null beginnend i, 0  Pos TopRow Telemetrie
                                        i, 1  Pos TopRow ST10
                                        i, 2  Pos TopRow Funk
                                        i, 3  Pos TopRow Sensor
                                        0, 4  Pos TopRow Übersicht
                                        1, 4  date of a time stamp (RdTime)
                                        2, 4  time of a time stamp
                                        i, 5  Pointer Suche Error Flag > 1
                                        i, 6  8 bit Error flags
                                              256..Emergency
                                              512..  frei
                                              1024.. usw}
  nhw: double;                              {Delta für Koordinaten in der Nähe}
  farbskala: array [0..2, 0..63] of TColor; {Farbeverlauf entspreched Werte}

implementation

{$R *.lfm}
                                                   {TForm1: Hauptfenster}

function GetExePath: string; {Gibt den Pfad zur exe-Datei mit Slash am Ende zurück}
begin
  result:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

function TForm1.GetFlightLogDir(fn: string): string; {FlightLog Verzeichnis finden}
var i, k: integer;
    splitlist: TStringList;
begin
  result:=fn;                                      {default: Ganzes Verzeichnis}
  splitlist:=TStringList.Create;
  try                                              {letzten Pfadnamen finden}
    splitlist.Delimiter:=PathDelim;
    splitlist.StrictDelimiter:=True;
    splitlist.DelimitedText:=fn;                   {dazu Pfad splitten}
    if splitlist.Count>1 then begin

      for i:=splitlist.Count-2 downto 0 do         {YTH Plus probieren}
        if pos(mndirp, splitlist[i])=1 then begin
          result:='';                              {Pfad zusammenbauen}
          SpinEdit3.Tag:=YTHPid;
          for k:=0 to i do
            result:=result+splitlist[k]+Pathdelim;
          exit;
        end;

      for i:=splitlist.Count-2 downto 0 do         {FlightLog von hinten suchen}
        if pos(mndir, splitlist[i])=1 then begin
          result:='';                              {Pfad zusammenbauen}
          for k:=0 to i do
            result:=result+splitlist[k]+Pathdelim;
          exit;
        end;

    end;
    if ExtractFileExt(fn)=bext then begin
      result:=ExtractFilePath(fn);                 {Breeze}
      SpinEdit3.Tag:=BRid;
    end;
  finally
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);      {Anwendung initialisieren und starten}
var i: integer;
begin
  Randomize;                                       {Zufallsgenerator initialisieren}
  nhw:=0.00002;                                    {default Koordinaten in der Nähe}
  cutb:=0;                                         {Zeitstempel zum Ausschneiden}
  cute:=0;                                         {Beginn und Ende löschen}
  topp:=nil;
  SetLength(topp, 4);                              {Topzeilen array}
  kpath:=IncludeTrailingPathDelimiter(dkpath);     {default setzen}
  SynEdit1.Lines.Clear;
  AppLogTimeStamp(Application.ExeName+tab4+version);
  Caption:=capForm1+tab2+Version;                  {Sprachversion einstellen}
  Hint:=capForm1;
  BitBtn1.Caption:=capBitBtn1;
  BitBtn1.Hint:=hntBitBtn1;
  BitBtn2.Caption:=capBitBtn2;
  BitBtn3.Caption:=capBitBtn3;
  BitBtn12.Caption:=capBitBtn12;                   {Schnellanalyse Defaulteinstellung}
  BitBtn12.Hint:=hntBitBtn12;
  BitBtn13.Caption:=rsScreenshot;
  BitBtn13.Hint:=hntBitBtn13;
{$IFDEF DARWIN}                                    {MAC OS X}
  BitBtn13.Visible:=false;                         {Button Screenshot}
  MenuItem25.Visible:=false;                       {Main Menu Screenshot}
  MenuItem3.Visible:=false;                        {PopUp Menu Höhenprofil; geht das nun?}
{$ENDIF}
  BitBtn14.Caption:=capBitBtn14;                   {Ausschneiden / Cut}
  BitBtn14.Hint:=hntBitBtn14;
  BitBtn18.Caption:=capBitBtn18;
  BitBtn18.Hint:=hntBitBtn18;
  BitBtn23.Caption:=capBitBtn23;
  BitBtn23.Hint:=hntBitBtn23;
  BitBtn24.Caption:=capBitBtn24;
  BitBtn24.Hint:=hntBitBtn24;
  BitBtn25.Caption:=capNachweis;                   {Flugprotokoll}
  ListBox1.Hint:=hntListBox1;
  ColorButton1.Hint:=hntColorBtn1;
  StatusBar1.Panels[5].Text:=DefaultStatus;
  ComboBox2.Hint:=DefaultStatus;
  SpeedButton4.Hint:=DefaultStatus;
  SpeedButton5.Hint:=capSelProt;                   {Protokollverzeichnis}
  MenuItem1.Caption:=rsToGMaps;
  MenuItem2.Caption:=rsToClipBoard;
  MenuItem3.Caption:=rsHDiaSave;
  MenuItem4.Caption:=capCrossHairOn;
  MenuItem4.Hint:=hntCrossHair;
  MenuItem5.Caption:=capGoToZNr;
  MenuItem6.Caption:=capTabSheet10;
  MenuItem7.Caption:=capMenuItem7;
  MenuItem8.Caption:=capMenuItem8;
  MenuItem9.Caption:=rsToOSM;
  MenuItem16.Caption:=rsResetCutBE;
  MenuItem17.Caption:=rsStartTpunkt;
  MenuItem18.Caption:=rsEndTPunkt;
  MenuItem20.Caption:=rsResetCutBE;
  MenuItem22.Caption:=capDatei;
  MenuItem24.Caption:=capBitBtn2;
  MenuItem25.Caption:=rsScreenshot;
  MenuItem26.Caption:=capHelp;
  MenuItem27.Caption:=capLabel7;
  MenuItem30.Caption:=capInfo;
  MenuItem32.Caption:=rsResetCutBE;
  MenuItem33.Caption:=rsStartTPunkt;
  MenuItem34.Caption:=rsEndTPunkt;
  MenuItem35.Caption:=capBitBtn14;                 {Ausschneiden / Cut}
  MenuItem54.Caption:=capBitBtn14;                 {Ausschneiden / Cut}
  MenuItem36.Caption:=capBitBtn3;
  MenuItem38.Caption:=capBitBtn1;
  MenuItem40.Caption:=capOpen;
  MenuItem41.Caption:=capSelDir;
  MenuItem44.Caption:=capGroupBox10;
  MenuItem45.Caption:=capSelProt;
  MenuItem47.Caption:=capMI47;
  MenuItem50.Caption:=rsCopy;
  MenuItem11.Caption:=capBitBtn12;                 {Menü Default}
  MenuItem10.Caption:=rsPC1Tab4;                   {Menü Settings}
  MenuItem56.Caption:=capSensorPlus;               {Sensordatei für YTH Plus öffnen}
  MenuItem57.Caption:=capDiashow;                  {alle Profiles anzeigen}
  RadioGroup1.Caption:=capRadioGroup1;
  RadioGroup1.Hint:=hntRadioGroup1;
  RadioGroup1.Items[0]:=dkpath;
  RadioGroup1.Items[1]:=spath;
  RadioGroup1.Items[2]:=fpath;
  RadioGroup1.Items[3]:=npath;
  RadioGroup1.Tag:=0;                              {Autosize columns needed}
  RadioGroup2.Caption:=capRadioGroup2+tab1+capBitBtn2;
  RadioGroup2.Hint:=hntRadioGroup2;
  RadioGroup3.Caption:=capRadioGroup3;
  RadioGroup4.Caption:=capDirSuffix+tab1+capBitBtn3;
  RadioGroup4.Hint:=hntDirSuffix;
  RadioGroup5.Caption:=capGroupBox5;
  RadioGroup5.Hint:=hntGroupBox5;
  RadioGroup6.Caption:=rsMode;
  RadioGroup6.Hint:=rsMode+' ('+rsVdo+'/'+capBitBtn22+')';
  RadioGroup6.Items[1]:=capBitBtn22;
  RadioGroup6.ItemIndex:=0;
  RadioGroup7.Caption:=capRadioGroup2;
  RadioGroup7.Hint:=capRadioGroup2;
  RadioGroup8.Caption:=capRadioGroup2+tab1+capNachweis;
  RadioGroup8.Hint:=hntRadioGroup8;
  StatusBar1.Hint:=hntStatus1;
  TabSheet1.Caption:=rsPC1Tab1;
  TabSheet2.Caption:=rsPC1Tab2;
  TabSheet3.Caption:=rsPC1Tab3;
  TabSheet4.Caption:=rsPC1Tab4;
  TabSheet5.Caption:=capScan;
  TabSheet5.Hint:=hntScan;
  TabSheet6.Hint:=TabSheet6.Caption;
  TabSheet11.Hint:=hntTabSheet11;
  TabSheet8.Caption:=capAnalyse;
  TabSheet9.Caption:=capTabSheet9;
  TabSheet9.Hint:=capTabSheet9;
  TabSheet10.Caption:=capTabSheet10;
  TabSheet10.Hint:=capTabSheet10;
  GroupBox1.Caption:=capGroupBox1;
  TabSheet12.Caption:=capTabSheet12;
  TabSheet12.Hint:=capTabSheet12;
  Memo1.Hint:=hntMemo1;
  Label1.Caption:=capLabel1;
  Label1.Hint:=hntLabel1;
  SpinEdit1.Hint:=hntLabel1;
  Label2.Caption:=capLabel2;
  Label3.Caption:=capLabel3;
  Label4.Caption:=capLabel4;
  Label4.Hint:=hntTrackBar1;
  Label5.Caption:=capLabel5;
  Label6.Caption:=capLabel6;
  Label7.Caption:=capLabel7;
  Label8.Caption:=capLabel8;
  Label12.Caption:=capLabel12;
  Label12.Hint:=hntTrackBar2;
  Label13.Caption:=capLabel13;
  Label13.Hint:=capLabel13;
  Label14.Caption:=capLabel14;
  Label14.Hint:=capLabel14;
  Label15.Caption:=rsDauer;
  Label15.Hint:=rsDauer;
  Label7.Hint:=GetExePath+manual;                  {default}
  if not FileExists(Label7.Hint) then begin
    Label7.Hint:=homepage+hpmydat+manual;          {mit Internet überschreiben}
  end else begin
  {$IFDEF DARWIN}
    Label7.Hint:=manual;                           {für MAC OS X überschreiben}
  {$ENDIF}
  end;
  Label8.Hint:=homepage+downURL;
  StaticText1.Caption:='';
  TrackBar3.Hint:=hntSharpness;
  Label21.Caption:=rsWB;
  Label21.Hint:=rsWB;
  ComboBox5.Text:=rsWB;
  ComboBox5.Hint:=rsWB;
  Label25.Caption:=rsShutter;
  Label25.Hint:=rsShutter;
  ComboBox7.Text:=rsShutter;
  ComboBox7.Hint:=rsShutter;
  ComboBox8.Hint:=capSelProt;
  ComboBox9.Hint:=hntComboBox9;
  Label24.Caption:=rsISO;
  Label24.Hint:=rsISO;
  ComboBox6.Text:=rsISO;
  ComboBox6.Hint:=rsISO;
  TrackBar1.SelEnd:=TrackBar1.Position;
  TrackBar2.SelEnd:=TrackBar2.Position;
  TrackBar2.Hint:=hntTrackBar2;
  ComboBox1.Hint:=hntEdit1;
  CheckBox2.Hint:=hntCheckBox2;
  CheckBox3.Caption:=capCheckBox3;
  CheckBox3.Hint:=hntCheckBox3;
  CheckBox4.Caption:=capCheckBox4;
  CheckBox4.Hint:=hntCheckBox4;
  CheckBox6.Caption:=capCheckBox6;
  CheckBox6.Hint:=hntCheckBox6;
  CheckBox7.Caption:=capCheckBox7;
  CheckBox7.Hint:=hntCheckBox7;
  CheckBox8.Caption:=capCheckBox8;                 {Save Sensor as CSV}
  CheckBox8.Hint:=hntCheckBox8;
  CheckBox9.Caption:=capCheckBox9;                 {YZH Plus Datenbereinigung}
  CheckBox9.Hint:=hntCheckBox9;
  CheckBox10.Caption:=capCheckBox10;               {Flight path from PX4 sensor}
  CheckBox10.Hint:=hntCheckBox10;
  CheckBox11.Hint:=hntCheckBox11;                  {add extrude 1 tag to KML}
  cbCap.Caption:=cbCapCaption;                     {Remaining capacity instead voltage}
  cbCap.Hint:=cbCapHint;
  cbReduced.Caption:=capReduced;                   {Applog reduced, only text msg}
  cbReduced.Hint:=hntReduced;
  cbThunder.Caption:=capThunder;
  cbThunder.Hint:=hntThunder;
  GroupBox2.Caption:=capGroupBox2;
  GroupBox3.Caption:=rsPC1Tab2;
  GroupBox4.Caption:=capAnalyse;
  GroupBox4.Hint:=hntGroupBox4+capAnalyse;
  GroupBox5.Caption:=rsCGOstat;
  GroupBox5.Hint:=rsCGOstat;
  GroupBox7.Caption:=capGroupBox7;
  GroupBox7.Hint:=capGroupBox7;
  GroupBox8.Caption:=capTabSheet12;
  GroupBox8.Hint:=capTabSheet12;
  GroupBox9.Caption:=capGroupBox9;
  GroupBox9.Hint:=hntEdit3;
  GroupBox10.Caption:=capGroupBox10;               {Flugprotokoll}
  GroupBox10.Hint:=hntGroupBox10;
  GroupBox11.Caption:=rsFind;                      {Suche}
  GroupBox11.Hint:=hntComboBox9;
  ColorButton1.Caption:=capColorButton1;
  TrackBar1.Hint:=hntTrackBar1;
  SpinEdit3.Hint:=hntSpinEdit3;
  Label6.Hint:=hntSpinEdit3;
  LabeledEdit1.EditLabel.Caption:=capTopDia;
  LabeledEdit2.EditLabel.Caption:=capMiddleDia;
  LabeledEdit3.EditLabel.Caption:=capBottomDia;
  Tag:=ord(DefaultFormatSettings.DecimalSeparator); {Original zwischenspeichern}
  DefaultFormatSettings.DecimalSeparator:='.';
  tend:=0;
  RadioGroup1.ItemIndex:=0;
  StringGrid1.Tag:=19;                    {default Position bei neuer FW ST10+}
  StringGrid1.ColWidths[0]:=130;
//  StringGrid1.Columns[0].Width:=130;             {Besser, aber dann überall!
//  Und das Indexkonzept ist anders, weil FixedColumns nicht beim Index mitzählt}
  StringGrid1.Hint:=hntGrid1;                      {Default Hint data}
  StringGrid1.ColCount:=defaultcol;
  grdOverview.ColWidths[0]:=fw0;
  grdOverview.Hint:=rsPC1Tab1;                     {Default Hint Overview}
  grdOverview.Cells[1,0]:=rsGridCell1;             {Übersichtstabelle aufbauen}
  grdOverview.Cells[2,0]:=rsGridCell2;
  grdOverview.Cells[3,0]:=rsGridCell3;
  grdOverview.Cells[4,0]:=rsDauer;
  grdOverview.Cells[5,0]:=rsGridCell5;
  grdOverview.Cells[6,0]:=rsGridCell6;
  grdOverview.Cells[7,0]:=rsGridCell7;
  grdOverview.Cells[8,0]:=rsGridCell8;
  grdOverview.Cells[9,0]:=rsGridCell9;
  grdOverview.Cells[10,0]:=rsGridCell10;
  grdOverview.Tag:=2;                              {Used Tab; default: Tabelle}
  StringGrid4.Hint:=hntStringGrid4;
  PageControl1.ActivePageIndex:=0;
  PageControl2.ActivePageIndex:=0;
  PageControl2.Hint:=rsPC1Tab4;                    {Einstellungen}
  AdvLed1.State:=lsDisabled;
  AdvLed1.Hint:=hntRec;
  Image2.Hint:=rsVdo;
  Image3.Hint:=capBitBtn22;
  Image4.Hint:=LazURL;
  Edit5.Hint:=hntCGO3URL;
  Label19.Hint:=hntCGO3URL;
  Label19.Caption:=rsCGO3URL;
  Label16.Caption:=rsExpo;
  Label16.Hint:=rsExpo;
  FloatSpinEdit1.Hint:=rsExpo;
  CheckBox5.Hint:=rsExpo;
  BitBtn15.Hint:=hntStatus;
  StopLightSensor1.Hint:=hntWLAN;
  Label18.Hint:=hntWLAN;
  Label22.Caption:=rsVideo;
  Label22.Hint:=rsVideo;
  ComboBox3.Text:=rsVideo;
  ComboBox3.Hint:=rsVideo;
  Label20.Hint:=capRadioGroup2;
  Label23.Caption:=rsFarbFormat;
  Label23.Hint:=rsFarbFormat;
  ComboBox4.Text:=rsFarbFormat;
  ComboBox4.Hint:=rsFarbFormat;
  indGnouMeter1.Caption:=capGnou;                  {SD card usage}
  indGnouMeter1.Hint:=hntGnou;
  Chart3.AxisList[0].LabelSize:=lblsize;           {y-Achsen ausrichten}
  Chart4.AxisList[0].LabelSize:=lblsize;
  Chart5.AxisList[0].LabelSize:=lblsize;
  StringGrid3.Hint:=hntStringGrid3;
  StringGrid3.Cells[0,0]:='Firmware';
  StringGrid3.Cells[0,1]:=rsWLANSpeed;
  StringGrid3.Cells[0,2]:=rsKStatus;
  StringGrid3.Cells[0,3]:=rsRecTime;
  StringGrid3.Cells[0,4]:='AWB lock';
  StringGrid3.Cells[0,5]:='AE enable';
  StringGrid3.Cells[0,6]:=rsShutter;
  StringGrid3.Cells[0,7]:=rsSharpness;
  StringGrid5.Hint:=rsResult;
  StringGrid5.Cells[0,0]:=rsNum;
  StringGrid5.Cells[1,0]:=rsResult;
  BitBtn16.Hint:=hntBitBtn16;
  BitBtn17.Hint:=hntBitBtn17;
  BitBtn19.Caption:=capBitBtn19;
  BitBtn19.Hint:=hntBitBtn19;
  BitBtn20.Caption:=capBitBtn20;
  BitBtn20.Hint:=hntBitBtn20;
  BitBtn22.Caption:=capBitBtn22;
  BitBtn22.Hint:=hntBitBtn22;
  BitBtn26.Caption:=capScan;
  BitBtn27.Caption:=rsSave;                        {AppLog speichern}
  BitBtn27.Hint:=hntBitBtn27;
  BitBtn28.Caption:=capDel;                        {AppLog löschen}
  BitBtn28.Hint:=hntDel;
  Image1.Hint:=BitBtn21.Caption;
  SpeedButton6.Hint:=hntSpeed6;
  Edit3.Hint:=hntEdit3;
  Edit4.Hint:=hntEdit4;
  Edit3.TextHint:=hntEdit3;
  Edit4.TextHint:=hntEdit4;
  ProgressBar1.Hint:=hntProgrBar1;
  RadioGroup9.Caption:=capProb;
  GroupBox6.Caption:=capProbScan;
  GroupBox6.Hint:=hntProb;
  Chart1BarSeries1.SeriesColor:=clAngle;           {Angle Mode – Purple status LED}
  Chart1BarSeries2.SeriesColor:=clSmart;           {für Smart Mode - Green status LED}
  Chart1BarSeries3.SeriesColor:=clRTH;             {für RTH}
  Chart1BarSeries4.SeriesColor:=clEmergency;       {Emergency}
  Chart1BarSeries5.SeriesColor:=clNoGPS;           {Ohne GPS Orange}
  Chart1BarSeries7.SeriesColor:=clSport;           {Sports Mode, Stability}
{Mit Hue, Luminance und Saturation gearbeitet (gleichbedeutend mit HSV oder TSW)}
  for i:=0 to High(farbskala[0]) do begin {nur einmal aufbauen und dann über Index nutzen}
    farbskala[0, i]:=HLStoColor(160, 255-round(i/High(farbskala[0])*150), 230);  {blau}
    farbskala[1, i]:=HLStoColor(80,  255-round(i/High(farbskala[0])*150), 230);  {grün}
    farbskala[2, i]:=HLStoColor(0,   255-round(i/High(farbskala[0])*150), 230);  {rot}
  end;
  tpos:=0;                                         {keine Position in der Tabelle}
  StatusBar1.Panels[2].Text:=RadioGroup2.Items[RadioGroup2.ItemIndex];
  BitBtn2.Hint:=hntBitBtn2+' (+ '+StatusBar1.Panels[2].Text+')';
  BitBtn3.Hint:=hntBitBtn3+' (+ '+RadioGroup4.Items[RadioGroup4.ItemIndex]+')';
  RadioGroup10.Enabled:=CheckBox1.Checked;
  FreigabeCut(false);                              {ohne Statusausgabe}
end;

function BoolToDouble(const s: string): double;    {zum Darstellen von Boolean}
begin
  result:=0;
  if LowerCase(trim(s))='false' then
    result:=-1;
  if LowerCase(trim(s))='true' then
    result:=1;
end;

function StrToFloatN(s: string): extended; {kapselt StrToFloat, gibt bei Fehler 0 zurück}
var m: integer;
begin
  result:=0;
  if length(s)>0 then begin
    m:=1;                                          {Multiplikator bei Expo}
    if pos('E7', s)>0 then
      s:=StringReplace(s, 'E7','',[rfReplaceAll, rfIgnoreCase]);

    if pos('E8', s)>0 then begin                   {*10}
      m:=10;
      s:=StringReplace(s, 'E8','',[rfReplaceAll, rfIgnoreCase]);
    end;

    if pos('E9', s)>0 then begin                   {*100}
      m:=100;
      s:=StringReplace(s, 'E9','',[rfReplaceAll, rfIgnoreCase]);
    end;

    try
      result:=StrToFloat(s)*m;
    except
      result:=BoolToDouble(s);
    end;
  end;
end;

function testh(const a: double): boolean;         {Datensätze mit unsinniger Höhe ausblenden}
begin
  result:=true;
  if (a<minnh) or                                 {tritt bei VTH Plus als Fehler auf}
     (a>maxxh) then result:=false;
end;

function OpenManual: boolean;                      {Handbuch aufrufen}
begin
  if not FileExists(GetExePath+manual) then
    result:=OpenURL(homepage+hpmydat+manual)
  else
    result:=OpenDocument(GetExePath+manual);
end;

{Zeitstempel to TDateTime; Format abhängig vom Vehicle Type
 legacy Yuneec: 20151206 11:32:57:234
 Mantis Q CSV:  2019-02-28 17:53:44.401
 Breeze:        2015-12-06 11:32:57               }
function TForm1.ZeitToDT(const s: string; const vt: integer): TDateTime;
begin
  try
    case vt of
      brID: begin                                  {Breeze}
              result:=ScanDateTime(vzf, s);
              if BitBtn3.Tag=1 then                {Platform Android}
                result:=result-nowUTC+now;         {UTC to local time}
            end;
      MQcsvID: result:=ScanDateTime('yyyy-mm-dd hh:nn:ss.zzz', s);
    else
      result:=ScanDateTime('yyyymmdd hh:nn:ss:zzz', s); {Yuneec legacy format}
    end;
  except
    result:=0;
  end;
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

{Finde einen Zeitstempel in den Dateien mit Datum/Zeit in der 1. Spalte}
function TForm1.FindTP(wlist: TStringList;        {Data list as strings}
                       tp: TDateTime;             {Time looking for}
                       const vt: integer): integer;   {Vehicle type}
var k, pos, len: integer;
    s: string;
begin
  result:=0;                                       {nothing found}
  case vt of
    brID:    begin                                 {Breeze}
               pos:=9;
               len:=lzbr;                          {19: length DT Breeze}
             end;
    MQcsvID: begin                                 {Mantis Q CSV format}
               pos:=16;
               len:=lzyu+2;                        {23: length DT Mantis Q}
             end;
  else
    pos:=1;
    len:=lzyu;                                     {21: length DT Yuneec}
  end;
  for k:=pos to wlist.Count-1 do begin
    s:=copy(wlist[k], 1, len);
    if ZeitToDT(s, vt)>=tp then begin
      result:=k;
      break;
    end;
  end;
end;

function ChNoToStr(const p: integer): string;      {Bedeutung der Kanäle}
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

function ChToStr(const s: string; const p: integer): string; {Channel CHxx umbenennen}
begin
  result:=s;                                        {Nehmen, wie es kommt - CHxx}
  {wenn S leer ist, dann Channel wie in Channel settings in ST16:
   Chxx, xx beginnend mit 1 statt 0, wie im FlightLog - Remote}
  if s='' then
    result:='Ch'+IntToStr(p);

  case p of                                {Liste der Channels zum Umbenennen}
    1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12: result:=result+bind+ChNoToStr(p);
    6: result:=result+' - A02';   {alle unbezeichneten, einstelligen Channels}
    else
      result:=result+' - A'+IntToStr(p-4);     {die restlichen unbezeichneten}
  end;
end;

{see DroneTypeFactory.java}
function TForm1.vTypeToStr(const v: integer): string; {vehicle type ausgeben}
begin
  result:='';
  case v of
    0: result:=rsInvalid;
    1: result:='Yuneec H920';
    2: result:='Yuneec Q500';
    3: result:='Blade 350QX';
    4: result:='Blade Chroma (380QX)';
    5: result:='Yuneec Typhoon H';
    6: result:='Yuneec H920+';                     {vermutlich ungenutzt}
    brID: result:='Yuneec Breeze';                 {selbst bestimmte Typ-IDs}
    MQid, MQcsvID: result:='Yuneec MantisQ';       {MantisQ erkannt}
    H5id: result:='Yuneec H520';                   {tlog files from H520}
    YTHPid: result:='Yuneec Typhoon H Plus';       {YTH Plus erkannt}
    ThBid: result:=capThunder;                     {H480 Thunderbird}
  else
    SynEdit1.Lines.Add('''Unknown Vehicle type '+IntToStr(v)+' found');
  end;
end;

function TForm1.fmodeToStr(const f: integer): string; {Flight Mode abh. vom Typ ausgeben}
begin
  result:='';
  if SpinEdit3.Tag=3 then begin                    {Blade 350 QX}
    case f of
       3: result:=rsWaitRC;
       4: result:=rsInitializing;
       5: result:=rsMotorStarting;
       8: result:=rsEmergency;
       9: result:=rsRTH+tab1+rsLanding;
      10: result:=rsAgility+GPSoff;
      11: result:=rsStability;
      12: result:=rsSmart;
      13: result:=rsAgility;
      14: result:=rsRTH+tab1+rsComing;
      17: result:=rsMagCali+rsCali;
      23: result:=rsBinding;
      25: result:='AP mode';
    end;

  end else if SpinEdit3.Tag=YTHPid then begin      {neu YTH Plus}
    case f of
      4: result:=rsManual;          {C0409R.drawable.ic_drone_flying_mode_m}
      5: result:=rsAngle;           {C0409R.drawable.ic_drone_flying_mode_a}
      6: result:=rsSmart;           {C0409R.drawable.ic_drone_flying_mode_smart}
      7: result:=rsSportMode;       {C0409R.drawable.ic_drone_flying_mode_sport}
      8: result:='Flight mode 8';
     10: result:='Flight mode 10';  {IPS mode ?, no GPS ?}
     12: result:=rsRTH+'12';
     13: result:=rsRTH+'13';
     17: result:='GPS lost';        {Really ?}
    end;

  end else begin                                   {Q500, YTH and all other legacy}
    case f of
       0: result:=rsStability;
       1: result:='Blue flashing'+GPSoff;
       2: result:='Blue'+GPSlost;
       3: result:=rsAngle+' (Purple solid)';
       4: result:=rsAngle+' (Purple flashing)'+GPSoff;
       5: result:=rsAngle+' (Purple solid)'+GPSlost;
       6: result:=rsSmart;
       7: result:=rsSmart+GPSlost;
       8: result:=rsMotorStarting;
       9: result:='Temperature'+rsCali;
      10: result:='Pressure'+rsCali;
      11: result:='Accelerometer bias'+rsCali;
      12: result:=rsEmergency;
      13: result:=rsRTH+tab1+rsComing;
      14: result:=rsRTH+tab1+rsLanding;
      15: result:=rsBinding;
      16: result:=rsInitializing;                  {Ready to start}
      17: result:=rsWaitRC;
      18: result:=rsMagCali+rsCali;
      19: result:=rsUnknown;
      20: result:=rsAgility;                       {Rate}
      21: result:=rsSmart+' - Follow me';
      22: result:=rsSmart+' - Follow me'+GPSlost;
      23: result:=rsSmart+' - Camera tracking';
      24: result:='Camera tracking'+GPSlost;
      26: result:='Task Curve Cable Cam';
      27: result:='Task Journey';
      28: result:='Task Point of Interest';
      29: result:='Task Orbit';
//      31: result:='seen during WLAN only';       {??}
      32: result:='IPS';                           {FMODE_ANGLE_MODE_IPS_ONLY:I = 0x20}
      33: result:='Waypoints';
    end;
  end;
end;

function BRfmodeToStr(const f: integer): string;   {Flight Mode abh. vom Typ ausgeben}
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

procedure Merkliste(ml: TComboBox; maxAnzahl: integer); {DropDownListe füllen}
begin
  // ml.Text:=ExcludeTrailingPathDelimiter(ml.Text);
  if ml.Items.IndexOf(ml.Text)<0 then              {noch nicht in Liste}
    ml.Items.Insert(0, ml.Text);
  if ml.Items.Count>MaxAnzahl then                 {Anzahl in Liste begrenzen}
    ml.Items.Delete(MaxAnzahl);
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
function IMUstatusToStr(const u: integer): string; {imu_status}
begin
  result:=rsUndef+tab1+IntToStr(u)+' = '+ByteToBin(u);
  case u of
      1: result:='IMU';
     33: result:='IMU+GPS';
     65: result:='IMU+C-Compass';
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
        result:=result+'Sonar off ';
      if (u and 32)=0 then
        result:=result+'GPS off ';
      if (u and 64)=0 then
        result:=result+'C-Compass off ';
      if (u and 128)=0 then
        result:=result+'C-GPS off';
    end;
  end;
end;

function CGPSToStr(const u: integer): string;      {CGPS nur bei >H920 und <YTH}
begin
  result:='';
  case u of    {die obersten 3 bits vom IMU Status}
    0: result:='C-GPS/Compass off';
    1: result:='C-GPS off';
    2: result:='C-Compass off';
    3: result:='C-GPS/Compass';
  end;
end;

function BRIMUstatusToStr(const u: integer): string;  {imu_status breeze}
begin
  result:=rsUndef+tab1+IntToStr(u)+' = '+ByteToBin(u);
  case u of                                           {Rest ist unbekannt}
    255: result:=rsAllOK;
  end;
end;

function AutoTakeOffToStr(const u: integer): string;  {Breeze}
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
function TForm1.PCGstatusToStr(const u: integer): string; {pressure_compass_status}
begin
  result:='';
  case u of
    21: result:='Baro+Compass+GPS';
    63: result:='Baro+Compass+GPS+RealSense';
    81: result:='Baro+GPS+Sonar';
    85: result:='Baro+Compass+GPS+Sonar';
    117: result:='Baro+Compass+GPS+Sonar+RealSense';
    245: result:=rsAllSet;  {YTH + RS -- bit 7 unbekannt, IPS?}
    else begin              {fehlende Bits interpretieren}
      result:='';
      if (u and 1)=0 then
        result:=result+'Baro fail ';
      if (u and 4)=0 then
        result:=result+'Mag fail ';
      if (u and 16)=0 then
        result:=result+'GPS fail ';
      if SpinEdit3.Tag=5 then begin                {nur Typhoon H}
        if (u and 32)=0 then
          result:=result+'RealSense error';
        if (u and 64)=0 then
          result:=result+'Sonar error';
      end;
    end;
  end;
  if result='' then result:=rsUndef+tab1+IntToStr(u)+' = '+ByteToBin(u);
end;

function MotStatusToStr(const u: integer): string; {Motor_status}
begin
  result:='';
  case u of
    15, 63, 255: result:=rsAllOK;
    else begin
      if (u and 1) =0 then
        result:=result+'Motor 1 off ';
      if (u and 2) =0 then
        result:=result+'Motor 2 off ';
      if (u and 4) =0 then
        result:=result+'Motor 3 off ';
      if (u and 8) =0 then
        result:=result+'Motor 4 off ';
      if (Form1.SpinEdit3.Tag=1) or
         (Form1.SpinEdit3.Tag=5) then begin        {Hexakopter}
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
      result:=result+'Complete Motor ESC Failure ';   {Ultrasonic error warning?}
    if (e and 16)>0 then
      result:=result+'Temperature Warning ';          {temp high}
    if (e and 32)>0 then
      result:=result+'Compass Calibration Warning ';
    if (e and 64)>0 then
      result:=result+'Fly-away Checker Warning ';     {temp low}
    if (e and 128)>0 then
      result:=result+'Airport Warning (NFZ) ';
  end else
    result:=rsAllOK;
end;

{Message Struktur:
 https://github.com/mavlink/c_library_v2/tree/master/common
 https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/common.xml
 https://github.com/YUNEEC/MavlinkLib}

function MsgIDtoStr(id: integer): string;
begin
  result:=rsUnknown+' MAV_CMD'+' $'+IntToHex(id, 2)+
          ' ('+IntToStr(id)+')';                   {default}
  case id of
      0:  result:='heartbeat';                     {Supported Msg Länge 9}
      1:  result:='sys_status';                    {Supported Msg Länge 1F}
      2:  result:='system_time';                   {Länge 0B}
      4:  result:='ping';
      5:  result:='change_operator_control';
      6:  result:='change_operator_control_ack';
      7:  result:='auth_key';
     11:  result:='set_mode';
    $14:  result:='param_request_read';
    $15:  result:='param_request_list';
    $16:  result:='param_value';
    $17:  result:='param_set';
    $18:  result:='gps_raw_int';                   {Supported Msg Länge 31/32}
    $19:  result:='gps_status';                    {Länge 1}
    $1A:  result:='scaled_imu';
    $1B:  result:='raw_imu';
    $1C:  result:='raw_pressure';
    $1D:  result:='scaled_pressure';
    $1E:  result:='attitude';                      {Länge 1C}
    $1F:  result:='attitude_quaternion';           {Supported Msg Länge 20}
    $20:  result:='local_position_ned';            {Länge 1C}
    $21:  result:='global_position_int';           {Supported Msg Länge 1C}
    $22:  result:='rc_channels_scaled';
    $23:  result:='rc_channels_raw';
    $24:  result:='servo_output_raw';              {Länge 10 oder 15}
    $25:  result:='mission_request_partial_list';
    $26:  result:='mission_write_partial_list';
    $27:  result:='mission_item';
    $28:  result:='mission_request';
    $29:  result:='mission_set_current';
    $2B:  result:='mission_current';
    $2A:  result:='mission_request_list';
    $2C:  result:='mission_count';                 {Länge 3 oder 5}
    $2D:  result:='mission_clear_all';
    $2E:  result:='mission_item_reached';
    $2F:  result:='mission_ack';
    $30:  result:='set_gps_global_origin';
    $31:  result:='gps_global_origin';
    $32:  result:='param_map_rc';
    $33:  result:='mission_request_int';
    $36:  result:='safety_set_allowed_area';
    $37:  result:='safety_allowed_area';
    $3D:  result:='attitude_quaternion_cov';
    $3E:  result:='nav_controller_output';
    $3F:  result:='global_position_int_cov';
    $40:  result:='local_position_ned_cov';
    $41:  result:='rc_channels';                   {Supported Msg Länge 2A}
    $42:  result:='request_data_stream';
    $43:  result:='data_stream';
    $45:  result:='manual_control';                {Länge 0B}
    $46:  result:='rc_channels_override';          {Länge 11}
    $49:  result:='mission_item_int';
    $4A:  result:='vfr_hud';                       {Länge 11}
    $4B:  result:='command_int';
    $4C:  result:='command_long';                  {Länge 20}
    $4D:  result:='command_ack';
    $4E:  result:='command_int_stamped';           {78: UTC time stamp, Boot time}
    $4F:  result:='command_long_stamped';          {79}
    $51:  result:='manual_setpoint';
    $52:  result:='set_attitude_target';
    $53:  result:='attitude_target';               {Länge 24}
    $54:  result:='set_position_target_local_ned';
    $55:  result:='position_target_local_ned';     {Länge 33}
    $56:  result:='set_position_target_global_int';
    $57:  result:='position_target_global_int';    {Länge 3}
    $59:  result:='local_position_ned_system_global_offset';
    $5A:  result:='hil_state';
    $5B:  result:='hil_controls';
    $5C:  result:='hil_rc_inputs_raw';
    $5D:  result:='hil_actuator_controls';
    $64:  result:='optical_flow';
    $65:  result:='global_vision_position_estimate';
    $66:  result:='vision_position_estimate';
    $67:  result:='vision_speed_estimate';
    $68:  result:='vicon_position_estimate';
    $69:  result:='highres_imu';                   {Länge 3E}
    $6A:  result:='optical_flow_rad';
    $6B:  result:='hil_sensor';
    $6C:  result:='sim_state';
    $6D:  result:='radio_status';
    $6E:  result:='file_transfer_protocol';
    $6F:  result:='timesync';                      {Länge 0D}
    $70:  result:='camera_trigger';
    $71:  result:='hil_gps';
    $72:  result:='hil_optical_flow';
    $73:  result:='hil_state_quaternion';
    $74:  result:='scaled_imu1';
    $75:  result:='log_request_list';
    $76:  result:='log_entry';
    $77:  result:='log_request_data';
    $78:  result:='log_data';
    $79:  result:='log_erase';
    $7A:  result:='log_request_end';
    $7B:  result:='gps_inject_data';
    $7C:  result:='gps2_raw';
    $7D:  result:='power_status';
    $7E:  result:='serial_control';
    $7F:  result:='gps_rtk';
    $80:  result:='gps2_rtk';
    $81:  result:='scaled_imu2';
    $82:  result:='data_transmission_handshake';
    $83:  result:='encapsulated_data';
    $84:  result:='distance_sensor';
    $85:  result:='terrain_request';
    $86:  result:='terrain_data';
    $87:  result:='terrain_check';
    $88:  result:='terrain_report';
    $89:  result:='scaled_pressure2';
    $8A:  result:='att_pos_mocap';
    $8B:  result:='set_actuator_control_target';
    $8C:  result:='actuator_control_target';       {Länge 14}
    $8D:  result:='altitude';                      {Länge 20}
    $8E:  result:='resource_request';
    $8F:  result:='scaled_pressure2';
    $90:  result:='follow_target';
    $92:  result:='control_system_state';
    $93:  result:='battery_status';
    $94:  result:='autopilot_version';             {Länge 34, 48, 4C}
    $95:  result:='landing_target';
{MESSAGE IDs 180 - 229: Space for custom messages in
 individual projectname_messages.xml files -->}
(*  201:  result:='sens_power';                    {not know if used}
    202:  result:='sens_MPTT';
    203:  result:='aslctrl_data';
    204:  result:='aslctrl_debug';
    205:  result:='asluav_status';
    206:  result:='ekf_ext';                       {Wind speed and such stuff}
    207:  result:='asl_obctrl';
    208:  result:='sens_atmos';                    {Atmospheric sensors}
    209:  result:='sens_batmon';                   {Battery monitor}
    210:  result:='fw_soaring_data';               {fixed wing...}
    211:  result:='sensorpod_status';
    212:  result:='sens_power_board';
    213:  result:='gsm_link_status';               {LTE too}       *)
    $E6:  result:='estimator_status';              {Länge 2A}
    $E7:  result:='wind_cov';                      {Länge 20}
    $E8:  result:='gps_input';
    $E9:  result:='gps_rtcm_data';
    $EA:  result:='high_latency';
    $EB:  result:='high_latency2';
    $F1:  result:='vibration';                     {Länge 14}
    $F2:  result:='home_position';                 {Supported Msg Länge 28 oder 3C}
    $F3:  result:='set_home_position';
    $F4:  result:='message_interval';
    $F5:  result:='extended_sys_state';            {Länge 02}
    $F6:  result:='adsb_vehicle';
    $F7:  result:='collision';
    $F9:  result:='memory_vect';
    $F8:  result:='v2_extension';
    $FA:  result:='debug_vect';
    $FB:  result:='named_value_float';
    $FC:  result:='named_value_int';
    $FD:  result:='statustext';                    {Länge variabel}
    $FE:  result:='debug';
    $100: result:='setup_signing';
    $101: result:='button_change';
    $102: result:='play_tune';
    $103: result:='camera_information';
    $104: result:='camera_settings';
    $105: result:='storage_information';
    $106: result:='camera_capture_status';
    $107: result:='camera_image_captured';         {Länge FC}
    $108: result:='flight_information';            {Supported Msg Länge 1B}
    $109: result:='mount_orientation';             {Länge 20}
    $10A: result:='logging_data';
    $10B: result:='logging_data_acked';
    $10C: result:='logging_ack';
    $10D: result:='video_stream_information';
    $10E: result:='set_video_stream_settings';
    $12B: result:='wifi_config_ap';
    $12C: result:='protocol_version';
    $136: result:='uavcan_node_status';
    $137: result:='uavcan_node_info';
    $140: result:='param_ext_request_read';
    $141: result:='param_ext_request_list';
    $142: result:='param_ext_value';               {Länge 95}
    $143: result:='param_ext_set';
    $144: result:='param_ext_ack';                 {Länge 91}
    $14A: result:='obstacle_distance';             {Länge 9E}
    $14B: result:='odometry';
    $14C: result:='trajectory_representation_waypoints';
    $14D: result:='trajectory_representation_bezier';
    340:  result:='UTM_global_position';           {154'h}
    350:  result:='debug_float_array';
    360:  result:='orbit_execution_status';
    //      21: result:='CMD_NAV_LAND';
    //      22: result:='CMD_NAV_TAKEOFF';
    400: result:='CMD_COMPONENT_ARM_DISARM';
    520: result:='CMD_REQUEST_AUTOPILOT_CAPABILITIES';
    528: result:='CMD_REQUEST_FLIGHT_INFORMATION';
    //    4005: result:='';
    //    4007: result:='';
    5000: result:='CMD_NAV_FENCE_RETURN_POINT';

    40001: result:='CMD_RESET_MPPT'; {Mission command to reset Maximum Power Point Tracker (MPPT)}
    40002: result:='CMD_PAYLOAD_CONTROL';    {Mission command to perform a power cycle on payload}
  end;
end;

(*  unused until now
{https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/ASLUAV.xml}
function GSMlinkType(sv: byte): string;
begin
  result:='Link type unknown';
  case sv of
    0: result:='No service';
    2: result:='2G (GSM/GRPS/EDGE) link';
    3: result:='3G link (WCDMA/HSDPA/HSPA)';
    4: result:='4G link (LTE)';
  end;
end;   *)

{ENUMs siehe: https://github.com/mavlink/c_library_v2/blob/master/common/common.h}
function MAVseverity(sv: byte): string;
begin
  result:='';
  case sv of
    0: result:=emcyID;      {System is unusable. This is a "panic" condition}
    1: result:='ALERT';     {Action should be taken immediately. Indicates error
                             in non-critical systems}
    2: result:='CRITICAL';  {Action must be taken immediately. Indicates failure
                             in a primary system}
    3: result:='ERROR';     {Indicates an error in secondary/redundant systems}
    4: result:='WARNING';   {Indicates about a possible future error if this
                             is not resolved within a given timeframe. Example
                             would be a low battery warning}
    5: result:='NOTICE';    {An unusual event has occurred, though not an error
                             condition. This should be investigated for the
                             root cause.}
    6: result:='INFO';      {Normal operational messages. Useful for logging.
                             No action is required for these messages.}
    7: result:='DEBUG';     {Useful non-operational messages that can assist in
                             debugging. These should not occur during normal
                             operation}
  end;
end;

{https://developer.yuneec.com/documentation/125/Supported-mavlink-messages
 https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/common.xml}
function MAVcompID(cid: byte): string;             {MAV_Component_ID}
begin
  result:='';
  case cid of
    0: result:='ALL';
    1: result:='AUTOPILOT 1';
    100: result:='CAMERA';
    101..105: result:='CAMERA '+IntToStr(cid-99);
    140..153: result:='SERVO '+IntToStr(cid-139);
    154: result:='GIMBAL';
    155: result:='LOG';
    156: result:='ADSB';
    157: result:='OSD';
    158: result:='PERIPHERAL';
    159: result:='QX1_GIMBAL';
    160: result:='FLARM';
    180: result:='MAPPER';
    190: result:='MISSIONPLANNER';
    195: result:='PATHPLANNER';
    200: result:='IMU';
    201: result:='IMU 2';
    202: result:='IMU 3';
    220: result:='GPS';
    221: result:='GPS 2';
    240: result:='UDP BRIDGE';
    241: result:='UART BRIDGE';
    250: result:='SYSTEM CONTROL';
  end;
end;

{https://github.com/mavlink/c_library_v2/blob/master/common/common.h

1	MAV_SYS_STATUS_SENSOR_3D_GYRO	0x01 3D gyro
2	MAV_SYS_STATUS_SENSOR_3D_ACCEL	0x02 3D accelerometer
4	MAV_SYS_STATUS_SENSOR_3D_MAG	0x04 3D magnetometer
8	MAV_SYS_STATUS_SENSOR_ABSOLUTE_PRESSURE	0x08 absolute pressure
16	MAV_SYS_STATUS_SENSOR_DIFFERENTIAL_PRESSURE	0x10 differential pressure
32	MAV_SYS_STATUS_SENSOR_GPS	0x20 GPS
64	MAV_SYS_STATUS_SENSOR_OPTICAL_FLOW	0x40 optical flow
128	MAV_SYS_STATUS_SENSOR_VISION_POSITION	0x80 computer vision position
256	MAV_SYS_STATUS_SENSOR_LASER_POSITION	0x100 laser based position
512	MAV_SYS_STATUS_SENSOR_EXTERNAL_GROUND_TRUTH	0x200 external ground truth (Vicon or Leica)
1024	MAV_SYS_STATUS_SENSOR_ANGULAR_RATE_CONTROL	0x400 3D angular rate control
2048	MAV_SYS_STATUS_SENSOR_ATTITUDE_STABILIZATION	0x800 attitude stabilization
4096	MAV_SYS_STATUS_SENSOR_YAW_POSITION	0x1000 yaw position
8192	MAV_SYS_STATUS_SENSOR_Z_ALTITUDE_CONTROL	0x2000 z/altitude control
16384	MAV_SYS_STATUS_SENSOR_XY_POSITION_CONTROL	0x4000 x/y position control
32768	MAV_SYS_STATUS_SENSOR_MOTOR_OUTPUTS	0x8000 motor outputs / control
65536	MAV_SYS_STATUS_SENSOR_RC_RECEIVER	0x10000 rc receiver
131072	MAV_SYS_STATUS_SENSOR_3D_GYRO2	0x20000 2nd 3D gyro
262144	MAV_SYS_STATUS_SENSOR_3D_ACCEL2	0x40000 2nd 3D accelerometer
524288	MAV_SYS_STATUS_SENSOR_3D_MAG2	0x80000 2nd 3D magnetometer
1048576	MAV_SYS_STATUS_GEOFENCE	0x100000 geofence
2097152	MAV_SYS_STATUS_AHRS	0x200000 AHRS subsystem health
4194304	MAV_SYS_STATUS_TERRAIN	0x400000 Terrain subsystem health
8388608	MAV_SYS_STATUS_REVERSE_MOTOR	0x800000 Motors are reversed
16777216	MAV_SYS_STATUS_LOGGING	0x1000000 Logging
33554432	MAV_SYS_STATUS_SENSOR_BATTERY	0x2000000 Battery
67108864	MAV_SYS_STATUS_SENSOR_PROXIMITY	0x4000000 Proximity
134217728	MAV_SYS_STATUS_SENSOR_SATCOM	0x8000000 Satellite Communication}

function MSenStat(m: longword): string;            {uint32 MAV_SYS_STATUS_SENSOR}
begin
  result:='';
  if m>0 then begin
    if (m and 1)>0   then result:=result+'3D_GYRO ';
    if (m and 2)>0   then result:=result+'3D_ACCEL ';
    if (m and 4)>0   then result:=result+'3D_MAG ';
    if (m and 8)>0   then result:=result+'ABSOLUTE_PRESSURE ';
    if (m and 16)>0  then result:=result+'DIFFERENTIAL_PRESSURE ';
    if (m and 32)>0  then result:=result+'GPS ';
    if (m and 64)>0  then result:=result+'OPTICAL_FLOW ';
    if (m and 128)>0 then result:=result+'VISION_POSITION ';
    if (m and 256)>0 then result:=result+'LASER_POSITION ';
    if (m and 512)>0 then result:=result+'EXTERNAL_GROUND_TRUTH ';
    if (m and 1024)>0    then result:=result+'ANGULAR_RATE_CONTROL ';
    if (m and 2048)>0    then result:=result+'ATTITUDE_STABILIZATION ';
    if (m and 4096)>0    then result:=result+'YAW_POSITION ';
    if (m and 8192)>0    then result:=result+'Z_ALTITUDE_CONTROL ';
    if (m and 16384)>0   then result:=result+'XY_POSITION_CONTROL ';
    if (m and 32768)>0   then result:=result+'MOTOR_OUTPUTS ';
    if (m and 65536)>0   then result:=result+'RC_RECEIVER ';
    if (m and 131072)>0  then result:=result+'3D_GYRO2 ';
    if (m and 262144)>0  then result:=result+'3D_ACCEL2 ';
    if (m and 524288)>0  then result:=result+'3D_MAG2 ';
    if (m and 1048576)>0   then result:=result+'GEOFENCE ';
    if (m and 2097152)>0   then result:=result+'AHRS ';
    if (m and 4194304)>0   then result:=result+'TERRAIN ';
    if (m and 8388608)>0   then result:=result+'REVERSE_MOTOR ';
    if (m and 16777216)>0  then result:=result+'LOGGING ';
    if (m and 33554432)>0  then result:=result+'BATTERY ';
    if (m and 67108864)>0  then result:=result+'PROXIMITY ';
    if (m and 134217728)>0 then result:=result+'SATCOM ';
  end else result:='0';
end;

{https://mavlink.io/en/messages/common.html#GPS_FIX_TYPE (nicht aktuell!)
 besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
 siehe heartbeat "Verdrehung"

0	GPS_FIX_TYPE_NO_GPS	No GPS connected
1	GPS_FIX_TYPE_NO_FIX	No position information, GPS is connected
2	GPS_FIX_TYPE_2D_FIX	2D position
3	GPS_FIX_TYPE_3D_FIX	3D position
4	GPS_FIX_TYPE_DGPS	DGPS/SBAS aided 3D position
5	GPS_FIX_TYPE_RTK_FLOAT	RTK float, 3D position
6	GPS_FIX_TYPE_RTK_FIXED	RTK Fixed, 3D position
7	GPS_FIX_TYPE_STATIC	Static fixed, typically used for base stations
8	GPS_FIX_TYPE_PPP	PPP, 3D position.}

function GPSfixType(const s:string): string;   {MAVlink GPS fix type to string}
var w: integer;
begin
  result:=rsUnknown;
  w:=StrToIntDef(s, 0);
  case w of
    0:	Result:='No GPS connected';
    1:	Result:='No position information, GPS is connected';
    2:	Result:='2D position';
    3:	Result:='3D position';
    4:	Result:='DGPS/SBAS aided 3D position';
    5:	Result:='RTK float, 3D position';
    6:	Result:='RTK Fixed, 3D position';
    7:	Result:='Static fixed, typically used for base stations';
    8:	Result:='PPP, 3D position';
  end;
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

function TForm1.SwitchToStr(const p: integer; const s: string): string;
var stk: double;
begin
  result:='';
  stk:=trunc(StrToFloatN(s));
  case p of
    5: begin
         if stk=stkup then
           result:=rsSmart;
         if (stk=stkup) and
            (SpinEdit3.Tag=YTHPid) then
           result:=rsSportSmartMode;
         if stk=stkntrl then
           result:=rsAngle;
         if stk=stkdown then
           result:=rsRTH;
       end;
    6: case SpinEdit3.Tag of                 {abhängig vom vehicle type}
         1: begin                                  {H920}
              if stk=stkmax then
                result:=rsRTH;
              if stk=stkup then
                result:=rsNeutral;
              if stk=2730 then
                result:=rsNoGPS;
//              if s=strdown  then result:='';
            end;
         2: begin                                  {Q500}
              if stk=stkup then
                result:=rsRTH;
              if stk=stkntrl  then
                result:=rsNeutral;
            end;
         5: begin                                  {YTH}
              if stk=stkmax then
                result:=rsRTH;
              if stk=stkntrl then
                result:=rsNeutral;
            end;
       end;
    9: begin                                       {S1 Gimbal Tilt mode}
         if stk=stkup then
           result:='Velocity mode';
         if stk=2184 then
           result:=rsAngle;
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

function StickPos(const w: double): string;        {Stickposition in %}
begin
  result:='';
  if round(w)=stkntrl then
    result:=rsNeutral
  else begin
    if w>stkntrl then
      result:='+'+IntToStr(round((w-stkntrl)*150/stkntrl))+'%';
    if (w<stkntrl) and
       (w>10) then
      result:='-'+IntToStr(round((stkntrl-w)*150/stkntrl))+'%';
  end;
end;

function StickToStr(const p: integer; const s: string): string;
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

{https://mavlink.io/en/messages/common.html#MAV_LANDED_STATE
 (nicht aktuell!) besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
  siehe heartbeat "Verdrehung"
  0	MAV_LANDED_STATE_UNDEFINED	MAV landed state is unknown
  1	MAV_LANDED_STATE_ON_GROUND	MAV is landed (on ground)
  2	MAV_LANDED_STATE_IN_AIR	MAV is in air
  3	MAV_LANDED_STATE_TAKEOFF	MAV currently taking off
  4	MAV_LANDED_STATE_LANDING	MAV currently landing  }

function MLStoStr(const m: byte): string;          {MAV_LANDED_STATE}
begin
  case m of
    1: result:='Landed (on ground)';
    2: result:='In air';
    3: result:='Currently taking off';
    4: result:='Currently landing';
  else
    result:='MAV landed state undef';
  end;
end;

{https://mavlink.io/en/messages/common.html#MAV_STATE
 (nicht aktuell!) besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
 siehe heartbeat "Verdrehung"
0	MAV_STATE_UNINIT	Uninitialized system, state is unknown.
bit 0	MAV_STATE_BOOT	        System is booting up.
bit 1	MAV_STATE_CALIBRATING	System is calibrating and not flight-ready.
bit 2	MAV_STATE_STANDBY	System is grounded and on standby. It can be launched any time.
bit 3	MAV_STATE_ACTIVE	System is active and might be already airborne. Motors are engaged.
bit 4	MAV_STATE_CRITICAL	System is in a non-normal flight mode. It can however still navigate.
bit 5	MAV_STATE_EMERGENCY	System is in a non-normal flight mode. It lost control over parts or over the whole airframe. It is in mayday and going down.
bit 6	MAV_STATE_POWEROFF	System just initialized its power-down sequence, will shut down now.
bit 7	MAV_STATE_FLIGHT_TERMINATION	System is terminating itself.}

function MSTtoStr(const m: byte): string;   {Bitleiste MAV_STATE auswerten}
begin
  result:='MAV_STATE'+suff;
  if m>0 then begin
    if (m and 1)>0   then result:=result+'BOOT ';
    if (m and 2)>0   then result:=result+'CALIBRATING ';
    if (m and 4)>0   then result:=result+'STANDBY ';
    if (m and 8)>0   then result:=result+'ACTIVE ';
    if (m and 16)>0  then result:=result+'CRITICAL ';
    if (m and 32)>0  then result:=result+emcyID+' ';
    if (m and 64)>0  then result:=result+'POWEROFF ';
    if (m and 128)>0 then result:=result+'FLIGHT_TERMINATION';
  end else result:=result+rsUnknown;
end;

{https://mavlink.io/en/messages/common.html      MAV_MODE_FLAG
(nicht aktuell!) besser hier: https://github.com/mavlink/c_library_v2/tree/master/common
 siehe heartbeat "Verdrehung"

 Flags:
 https://github.com/mavlink/c_library_v2/blob/master/common/common.h

These flags encode the MAV mode. In Heartbeat base-mode +6
Value	Field Name	Description
128	MAV_MODE_FLAG_SAFETY_ARMED	0b10000000 MAV safety set to armed. Motors are enabled / running / can start. Ready to fly. Additional note: this flag is to be ignore when sent in the command MAV_CMD_DO_SET_MODE and MAV_CMD_COMPONENT_ARM_DISARM shall be used instead. The flag can still be used to report the armed state.
64	MAV_MODE_FLAG_MANUAL_INPUT_ENABLED	0b01000000 remote control input is enabled.
32	MAV_MODE_FLAG_HIL_ENABLED	0b00100000 hardware in the loop simulation. All motors / actuators are blocked, but internal software is full operational.
16	MAV_MODE_FLAG_STABILIZE_ENABLED	0b00010000 system stabilizes electronically its attitude (and optionally position). It needs however further control inputs to move around.
8	MAV_MODE_FLAG_GUIDED_ENABLED	0b00001000 guided mode enabled, system flies waypoints / mission items.
4	MAV_MODE_FLAG_AUTO_ENABLED	0b00000100 autonomous mode enabled, system finds its own goal positions. Guided flag can be set or not, depends on the actual implementation.
2	MAV_MODE_FLAG_TEST_ENABLED	0b00000010 system has a test mode enabled. This flag is intended for temporary system tests and should not be used for stable implementations.
1	MAV_MODE_FLAG_CUSTOM_MODE_ENABLED	0b00000001 Reserved for future use.}

function MMFtoStr(const m: byte): string;   {Bitleiste MAV_Mode_FLAG auswerten}
begin
  result:='MAV_MODE_FLAG'+suff;
  if m>0 then begin
    if (m and 1)>0   then result:=result+'CUSTOM_MODE_ENABLED ';
    if (m and 2)>0   then result:=result+'TEST_ENABLED ';
    if (m and 4)>0   then result:=result+'AUTO_ENABLED ';
    if (m and 8)>0   then result:=result+'GUIDED_ENABLED ';
    if (m and 16)>0  then result:=result+'STABILIZE_ENABLED ';
    if (m and 32)>0  then result:=result+'HIL_ENABLED ';
    if (m and 64)>0  then result:=result+'MANUAL_INPUT_ENABLED ';
    if (m and 128)>0 then result:=result+'SAFETY_ARMED';
  end else Result:=result+rsUnknown;
end;

function TForm1.CheckE7(const s: string): boolean; {prüft einen string auf Fehler}
begin
  result:=(not CheckBox9.Checked) or               {Prüfung ggf. abschalten}
          (pos('E7', s)=0);                        {E7 darf in Telemetrie nicht vorkommen}
end;

function KoToStr(const lanlon: double): string;    {Koordinaten zu String}
begin
  result:=FloatToStrF(lanlon, ffFixed, 3, 8);
end;

function ChrKoor(ko: string): string;              {Korrigiert Format der Koordinaten}
begin
  try
    result:=KoToStr(StrToFloatN(ko));              {umwandeln um führende 0 zu bekommen}
  except
    result:=ko;
  end;
end;

function KorrBool(const s: string): string;        {true -> 1, Rest -> 0}
begin
  result:='0';
  if LowerCase(trim(s))='true' then
    result:='1';
end;

function KorrSigned(const s: string; const maske: byte): string; {String in unsigned String}
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
  if length(c)>6 then
    insert('.', result, length(c)-6);
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

function BrTeilen(const s: string; const t, k: integer): string;
begin
  result:=FloatToStrF(StrToFloatN(trim(s))/t, ffFixed, 10, k);
end;

function H920Amp(const w: double): double;         {Stromsensor H920}
begin
  result:=w/2;                                     {uint8_t current; 0.5A resolution}
end;

function TiltToGrad(const w: double): double;      {Umrechnung Werte in 0-90 Grad}
begin
  result:=-(w-683)*90/2729;
end;

{eventuell auch so: uint8_t voltage; 25.4V  voltage = 5 + 255*0.1 = 30.5V, min=5V}
function BrUmrech(const w: double): double;        {Umrechnung unsicher}
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
function VtoProz(const vt: integer; const u: double): integer; {vehicle_type, Spannung in %}
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
    5, 7, 10: begin                                {YTH / YTH Plus}
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
   else begin                                      {alle anderen 3S Kopter}
         m:=3*lipomax;                             {3S}
         if  u>=m then
           result:=100;                            {100%}
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

function VPerCell (const vt: integer; const u: double): string;  {Spannung pro Zelle ausgeben}
begin
  case vt of
    1, 6:     result:=FloatToStrF(u/6, ffFixed, 2, 2)+rsPerCell; {H920 6S}
    5, 7, 10: result:=FloatToStrF(u/4, ffFixed, 2, 2)+rsPerCell; {H480/520 4S}
    else      result:=FloatToStrF(u/3, ffFixed, 2, 2)+rsPerCell; {all 3S}
  end;
end;

function RadToStr(const p: integer; const s: string): string;  {Drehrad-Einstellung}
var w: double;
begin
  result:='';
  try
    w:=StrToFloat(s);
  except
    exit;
  end;
  case p of
    7: result:='K1 (Camera tilt)'+suff+IntToStr(round(TiltToGrad(w)))+'°';
  end;
end;

function tabs(const prefix, suffix: string; const t: integer): string;  {Tabulator + suff}
begin                  {formatiert einen string in Länge t zur Ausgabe}
  result:=prefix;
  while UTF8Length(result)<t do
    result:=result+tab1;
  result:=result+suffix;
end;

function FormSR(const s: string; const p: integer): string;  {füllt string mit führenden
                                                       Leerzeichen auf Länge p}
begin
  result:=s;
  while UTF8length(result)<p do
    result:=tab1+result;
end;

function IntToStrFL(const w, p: integer): string; {Wandelt Zahlen in String mit
                                         Länge p mit führenden Leerzeichen um}
begin
  try
    result:=FormSR(IntToStr(w), p);
  except
    result:=FormSR('?', p);
  end;
end;

function GetNr(const s: string): string; {filtert Ziffern aus einem String für Dateiname}
begin
  result:='';
  if s<>'' then begin
    case Form1.SpinEdit3.Tag of
       MQid: begin                                 {MantisQ}
               result:=ChangeFileExt(s, '');
               result:=StringReplace(result, nfile, '', [rfIgnoreCase]);
               result:=StringReplace(result, mfile, '', [rfIgnoreCase]);
             end;
       H5id: result:=ChangeFileExt(s, '');         {H520, ganzer Namensstamm}
       brID: result:=ChangeFileExt(s, '')          {Breeze, new firmware}

       else result:=Form2.CleanNum(s);
    end;
  end;
end;

function GetFNr(const s: string): string;          {filtert Float aus einem String}
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

function ResultDN(const s, ex: string): string;    {erzeuge Ergebnis-Dateinamen}
begin
  result:=ExtractFileDir(s)+GetNr(ExtractFileName(s))+ex; {im übergeordnedten Dir}
end;

function RandomFN(const fn: string; const vt, mode: integer): string; {Dateinamen ermitteln}
var p: integer;                                    {Position zum zufälligen Ändern}
    z: char;
begin
  result:=IncludeTrailingPathDelimiter(ExtractFilePath(fn))+'cut'+us1+bext;
  case vt of
    brID: p:=UTF8length(fn)-4;                     {Breeze}
       4: p:=UTF8length(fn)-7;                     {Chroma}
    else
      p:=UTF8length(fn)-8;                         {legacy Yuneec}
  end;

  if length(fn)>p then begin
    result:=fn;
    if mode=0 then
      repeat
        z:=Chr(random(8)+49)
      until
        fn[p]<>z                                   {Zahl zwischen 1 und 9}
    else
      z:=Chr(mode+48);
    result[p]:=z;
  end;
end;

function NichtLeer(const s: string): boolean; {Identifikation eines leeren Feldes
                                               oder Feld mit 0 Wert}
var w: extended;
begin
  result:=true;
  if trim(s)='' then
    result:=false
  else begin
    try
      w:=StrToFloat(s);
      if w=0 then result:=false;
    except
    end;
  end;
end;

function DateTimeToUNIX(const dt: TDateTime):Integer;
begin
  result:=((Trunc(dt)-25569)*Secpd)+Trunc(Secpd*(dt-Trunc(dt)))-200;
end;

function NumSec(d: TDateTime): string;       {Dauer in Anzahl Sekunden wandeln}
begin
  try
//  result:=IntToStr(round(d*Secpd));
    result:=FloatToStrF(d*Secpd, ffFixed, 12, 1);  {alternativ mit Kommastelle}
  except
    result:='';
  end;
end;

function brTransformW(const inx: integer; const w: double): double;
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

function TForm1.SpeedX(const sp: double): double;  {Geschwindigkeit umrechnen}
begin
  result:=sp;                                      {default m/s}
  case RadioGroup3.ItemIndex of
    1: result:=sp*fkmh;                            {km/h}
    2: result:=sp*fmph;                            {mph}
  end;
end;

function TForm1.TransformW(const fnr, inx: integer; const w: double): double;
   {fnr: 0..Telemetry, 1..RemoteGPS, 2..Remote
    inx: Index of column
    w: Value to transform}
begin
  result:=w;                                       {default: in=out}
  case fnr of
    0: case inx of                                 {Telemetry}
          7: SpeedX(w);                            {tas}
         14: result:=round(w) and 255;             {motor status}
         15: result:=round(w) and 255;             {imustatus}
         16: result:=round(w) and 255;             {sensor status}
       end;
(*    1: case inx of                               {RemoteGPS}
       end;                           eigentlich nix zu tun *)
    2: case inx of                                 {Remote}
         7: result:=TiltToGrad(w);                 {CH6 Kamera neigen}
       end;
  end;
end;

function TForm1.GetCellInfo(const sp, zl: integer): string; {Hint/Info per Zelle}
var e: integer;
    s: string;
    t: double=0;


  function HeaderHnt: string;
  begin
    result:=StringGrid1.Cells[sp, 0]+'=';
  end;

  function DefaultHnt: string;
  begin
    result:=HeaderHnt+StringGrid1.Cells[sp, zl];
  end;

  procedure PayLoad;                               {Payload detailliert anzeigen}
  begin
    if StringGrid1.Cells[sp, zl]='' then           {leere Zellen}
      s:='n/a'
    else begin                                     {gefüllte Zellen}
      try
        e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl])
      except
        e:=0;
      end;
      s:='Payload - '+StringGrid1.Cells[sp, 0]+': $'+StringGrid1.Cells[sp, zl]+
         ' = '+IntToStr(e);
      if (e>31) and
         (e<128) then
        s:=s+' ('+Chr(e)+')';                      {ASCII Zeichen, wenn gültig}
    end;
  end;

  procedure Zeitstempel;                           {Zeitstempel legacy anzeigen}
  begin
    if sp=0 then
    s:=FormatDateTime(vzf+zzz, ZeitToDT(StringGrid1.Cells[0, zl], SpinEdit3.Tag));
  end;

  procedure TabHintYTHP;
  begin
    s:='';
    if (sp=0) or (sp=1) then begin                 {Fix=/1}
      s:=rsRecordNo+IntToStr(zl)+kma+DefaultHnt;
    end;
    if sp=lenfix-6 then begin                      {Sequenz number}
      try
        e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]); {dezimal}
        s:=rsRecordNo+IntToStr(zl)+kma+
           StringGrid1.Cells[sp, 0]+'='+IntToStr(e);
      except
        s:='';                                     {bei Fehler Standardausgabe}
      end;
    end;
    if sp=lenfix-4 then                            {MAV Component ID}
      try
        e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]);
        s:=MAVcompID(e);
      except
        s:=rsError;
      end;
    if (sp=lenfix-1) or
       (sp=lenfix-2) or
       (sp=lenfix-3) then begin                    {Message ID 3 Byte}
      try
        e:=Hex2Dec('$'+StringGrid1.Cells[lenfix-1, zl]+
                       StringGrid1.Cells[lenfix-2, zl]+
                       StringGrid1.Cells[lenfix-3, zl]);
        s:=IntToStr(e)+suff+StringGrid1.Cells[lenfix, zl];
      except
        s:=StringGrid1.Cells[lenfix, zl];
      end;
    end;
    if sp>lenfix-1 then
      Payload;                                     {Payload anzeigen}
    if sp=lenfix then
      s:='mavlink_msg_'+StringGrid1.Cells[sp, zl]; {Message Name}
    if sp=lenfix+1 then
      s:=rsLenPL+StringGrid1.Cells[sp, zl];        {Payloadlänge}
  end;

  procedure TabHintBreeze;
  begin
    case sp of
       2: begin
            e:=StrToIntDef(StringGrid1.Cells[sp, zl], 99);
            s:=BrfmodeToStr(e);
          end;
       3, 4, 6, 7, 8:
          begin
            t:=StrToFloatN(StringGrid1.Cells[sp, zl])/10;
            s:=StringGrid1.Cells[sp, 0]+'='+FloatToStrF(t, ffFixed, 3, 1);
            if BitBtn1.Tag=0 then
              s:=s+'m'
            else
              s:=s+'ft';
          end;
       9: begin
            t:=StrToFloatN(StringGrid1.Cells[sp, zl])/100;
            s:=StringGrid1.Cells[sp, 0]+'='+FloatToStrF(t, ffFixed, 3, 1);
            if BitBtn1.Tag=0 then
              s:=s+'m/s'
            else
              s:=s+'ft/s';
          end;
      10: begin                                    {Altitude}
            t:=StrToFloatN(StringGrid1.Cells[sp, zl])/100;
            s:=StringGrid1.Cells[sp, 0]+'='+FloatToStrF(t, ffFixed, 4, 1);
            if BitBtn1.Tag=0 then
              s:=s+'m'
            else
              s:=s+'ft';
          end;
      11: s:='IMU Status='+KorrSigned(StringGrid1.Cells[sp, zl], 255);
      12: begin
            t:=BrCoordToFloat(StringGrid1.Cells[sp, zl]);
            s:=StringGrid1.Cells[sp, 0]+'='+KoToStr(t);
          end;
      13: begin
            t:=BrCoordToFloat(StringGrid1.Cells[sp, zl]);
            s:=StringGrid1.Cells[sp, 0]+'='+KoToStr(t);
          end;
      14: begin                                    {AutoTakeOff}
            e:=StrToIntDef(StringGrid1.Cells[sp, zl], 99);
            s:=AutoTakeOffToStr(e);
          end;
      15: begin                                    {roll}
            t:=StrToFloatN(StringGrid1.Cells[sp, zl])/100;
            s:=StringGrid1.Cells[sp, 0]+'='+FloatToStrF(t, ffFixed, 4, 2);
          end;
      16: begin                                    {pitch}
            t:=StrToFloatN(StringGrid1.Cells[sp, zl])/100;
            s:=StringGrid1.Cells[sp, 0]+'='+FloatToStrF(t, ffFixed, 4, 2);
          end;
      17: begin                                    {yaw}
            t:=StrToFloatN(StringGrid1.Cells[sp, zl])/100;
            s:=StringGrid1.Cells[sp, 0]+'='+FloatToStrF(t, ffFixed, 4, 2);
          end;
      18: begin                                    {MotorStatus}
            e:=StrToIntDef(StringGrid1.Cells[sp, zl], 0);
            s:=MotStatusToStr(e);
          end;
      19: s:=eflagToStr(StringGrid1.Cells[sp, zl]); {Error Flags}
      20: s:='Number Sats='+KorrSigned(StringGrid1.Cells[sp, zl], 63);
      21: s:=rsRest+' ~'+BrKorrV(StringGrid1.Cells[sp, zl])+'%';
    end;
  end;

  procedure TabHintSensorYTH;
  begin
    s:='';
    if sp=0 then begin                             {Sequenz number}
      try
        e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]); {dezimal}
        s:=rsRecordNo+IntToStr(zl)+kma+
           StringGrid1.Cells[sp, 0]+'='+IntToStr(e);
      except
        s:='';                                     {bei Fehler Standardausgabe}
      end;
    end;
    if (sp=1) or (sp=2) then begin                 {SysID/CompID}
      s:=rsRecordNo+IntToStr(zl)+kma+DefaultHnt;
    end;
    if sp>lenfix-2 then
      PayLoad;                                     {Payload anzeigen}
    if sp=lenfix-2 then
      s:=rsLenPL+StringGrid1.Cells[sp, zl];        {Payloadlänge ohne CRC}
  end;

  procedure TabHltYTHP;                            {Typhoon H Plus}
  begin
    case sp of
      0: s:=rsTimeToBoot+suff+
            FormatDateTime(zzf+zzz, ZeitToDT(StringGrid1.Cells[0, zl], YTHPid)-
                                    ZeitToDT(StringGrid1.Cells[0, 1], YTHPid));
      2: s:=StringGrid1.Cells[sp, zl]+'V, '+
            VperCell(SpinEdit3.Tag,                {V per cell}
                     StrToFloatN(StringGrid1.Cells[sp, zl]));
      3: s:=rsrest+tab1+StringGrid1.Cells[sp, zl]+'%';
      4: begin                                     {Altitude}
           if RadioGroup3.ItemIndex=2 then begin
             try
               t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
             except
               t:=0;
             end;
             s:=FloatToStrF(t/fft, ffFixed, 4, 2)+'ft'
           end else
             s:=StringGrid1.Cells[sp, zl]+'m';
         end;
      7, 24, 25:
         begin                                     {tas, hspeed, hspeed}
           try
             t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
           except
             t:=0;
           end;
           s:=DefaultHnt+'m/s = ';
           if RadioGroup3.ItemIndex=2 then
             s:=s+FloatToStrF(t*fmph, ffFixed, 3, 1)+'mph'
           else
             s:=s+FloatToStrF(t*fkmh, ffFixed, 3, 1)+'km/h';
         end;
       9: s:=GPSfixType(StringGrid1.Cells[sp, zl]);
      19: s:=fmodeToStr(StrToIntDef(StringGrid1.Cells[sp, zl], 99)); {fMode}
      21: s:=vtypeToStr(YTHPid);                   {fix eingestellt}
    end;
  end;

  procedure TabHltLegacy;                          {Q500, YTH und andere Kopter}
  begin
    case sp of                                     {beginnt mit fixen Spalten}
       1: if (StringGrid1.Cells[sp, zl]='0')
            then s:='Dual Band Control Redundancy (5.8G WiFi)';
       2: s:=rsRest+' ~'+IntToStr(VtoProz(SpinEdit3.Tag,  {Voltage in %}
                   StrToFloatN(StringGrid1.Cells[sp, zl])))+'%'+
                   kma+VperCell(SpinEdit3.Tag,    {V per cell}
                   StrToFloatN(StringGrid1.Cells[sp, zl]));
       3: if SpinEdit3.Tag=1 then begin            {H920: Current}
            try
              t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
            except
              t:=0;
            end;
            if t>0 then s:=FloatToStrF(H920Amp(t), ffFixed, 3, 1)+'A';
          end;
       4: begin                                    {Altitude}
            if RadioGroup3.ItemIndex=2 then begin
              try
                t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
              except
                t:=0;
              end;
              s:=HeaderHnt+FloatToStrF(t/fft, ffFixed, 4, 2)+'ft'
            end else
              s:=DefaultHnt+'m';
          end;
       7: begin                                    {tas=true air speed}
            try
              t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
            except
              t:=0;
            end;
            s:=HeaderHnt+FloatToStrF(t, ffFixed, 3, 2)+'m/s = ';
            if RadioGroup3.ItemIndex=2 then
              s:=s+FloatToStrF(t*fmph, ffFixed, 3, 1)+'mph'
            else
              s:=s+FloatToStrF(t*fkmh, ffFixed, 3, 1)+'km/h';
          end;
       9: s:=GPSfixType(StringGrid1.Cells[sp, zl]);
      11,12,13: s:=DefaultHnt+'°';                 {Pitch, roll, yaw}
      14: begin                                    {MotorStatus}
            e:=StatusToByte(StringGrid1.Cells[sp, zl]);
            s:=MotStatusToStr(e);
          end;
      15: if (SpinEdit3.Tag>1) then begin          {imu_status}
            e:=StatusToByte(StringGrid1.Cells[sp, zl]);
            s:=IMUstatusToStr(e);
          end;
      else begin    {case else: für variable Spalten abh. vom Typ}
        if (sp=StringGrid1.Tag-1) and
           (SpinEdit3.Tag>1) then begin            {press_compass_status}
          e:=StatusToByte(StringGrid1.Cells[sp, zl]);
          s:=PCGstatusToStr(e);
        end;
        if (sp=StringGrid1.Tag-2) and              {CGPS}
           (SpinEdit3.Tag=2) then begin            {nur bei Q500 gefüllt}
          e:=StatusToByte(StringGrid1.Cells[sp, zl]);
          s:=CGPSToStr(e);
        end;
        if sp=StringGrid1.Tag then begin           {Flight Mode}
          e:=StrToIntDef(StringGrid1.Cells[sp, zl], 99);
          s:=fmodeToStr(e);
        end;
        if sp=StringGrid1.Tag+2 then begin         {vehicle type}
          s:=vtypeToStr(SpinEdit3.Tag);
        end;
        if sp=StringGrid1.Tag+3 then               {error flags}
          s:=eflagToStr(StringGrid1.Cells[sp, zl]);
      end;
    end;
  end;

  procedure TabHLTelemetry;                        {Hints für Telemetrie}
  begin
    Zeitstempel;                                   {Default, can be overwritten}
    case SpinEdit3.Tag of                          {Vehicle type}
      brID: TabHintBreeze;                         {Breeze}
      YTHPid: TabHltYTHP;                          {YTH Plus}
    else
      TabHltLegacy;                                {Q500, YTH und andere Kopter}
    end;
  end;

  procedure TabHLFunk;
  begin
    Zeitstempel;
    case sp of
      1:       s:=StickToStr(sp, StringGrid1.Cells[sp, zl]);
      2..4, 8: s:=StickToStr(sp, StringGrid1.Cells[sp, zl]);
      7:    s:=RadToStr(sp, StringGrid1.Cells[sp, zl]);
      5, 6, 9, 10: s:=SwitchToStr(sp, StringGrid1.Cells[sp, zl]);
      11:   s:=LandGearToStr(StringGrid1.Cells[sp, zl]);
    end;
  end;

  procedure TabHintRemoteGPS;                      {RemoteGPS aus ST10/16}
  begin
    case sp of
      0: Zeitstempel;
      3: begin                                     {Altitude}
        s:=HeaderHnt;
        if RadioGroup3.ItemIndex=2 then begin
             try
               t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
             except
               t:=0;
             end;
             s:=s+FloatToStrF(t/fft, ffFixed, 4, 1)+'ft'
           end else
             s:=DefaultHnt+'m';
         end;
      5: begin                                     {Speed in cm/s}
           try
             t:=StrToFloatN(StringGrid1.Cells[sp, zl])/100;
           except
             t:=0;
           end;
           s:=HeaderHnt+FloatToStrF(t, ffFixed, 3, 2)+'m/s = ';
           if RadioGroup3.ItemIndex=2 then
             s:=s+FloatToStrF(t*fmph, ffFixed, 3, 1)+'mph'
           else
             s:=s+FloatToStrF(t*fkmh, ffFixed, 3, 1)+'km/h';
         end;
      6: s:=DefaultHnt+'°';
    end;
  end;

  procedure TabHintLegacy;                         {CSV-Dateien (Tm/Rm/Rm-GPS)}
  begin
    case RadioGroup1.ItemIndex of
      0: TabHLTelemetry;                           {Telemetrie Kopter}
      1: TabHintRemoteGPS;                         {RemoteGPS}
      2: TabHLFunk;                                {Rawdata RF Funk}
      3: TabHintSensorYTH;                         {Sensor nur für YTH}
    end;
  end;

  procedure TabHintPX4csv;                         {Info für eigene PX4 CSV}
  begin
    case sp of
      2: s:=DefaultHnt+'V';
      3: s:=DefaultHnt+'A';
      4, 37..40: s:=DefaultHnt+'m';
      7: begin                                    {tas=true air speed}
           try
             t:=StrToFloatN(StringGrid1.Cells[sp, zl]);
           except
             t:=0;
           end;
           s:=DefaultHnt+'m/s = ';
           if RadioGroup3.ItemIndex=2 then
             s:=s+FloatToStrF(t*fmph, ffFixed, 3, 1)+'mph'
           else
             s:=s+FloatToStrF(t*fkmh, ffFixed, 3, 1)+'km/h';
         end;
      8: s:=rsEmpty;                               {currently unused}
      11..13: s:=DefaultHnt+'rad';
      1, 14, 46, 47: s:=DefaultHnt+'%';
      15: begin
            if StringGrid1.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]);
              s:=MSenStat(e);
            end else s:=rsEmpty;
          end;
      17: begin
            if StringGrid1.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]);
              s:=StringGrid1.Cells[sp, 0]+': $'+
                 StringGrid1.Cells[sp, zl]+'='+IntToStr(e);
            end else s:=rsEmpty;
          end;
      18: begin
            if StringGrid1.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]);
              s:=MSTtoStr(e);
            end else s:=rsEmpty;
          end;
      19: begin
            if StringGrid1.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+StringGrid1.Cells[sp, zl]);
              s:=MMFtoStr(e);
            end else s:=rsEmpty;
          end;
      21: s:='Vertical accuracy'+suff+StringGrid1.Cells[sp, zl]+'m';
      22: s:='Horizontal accuracy'+suff+StringGrid1.Cells[sp, zl]+'m';
      25: s:='Ground speed'+suff+StringGrid1.Cells[sp, zl]+'m/s';
      26..28: s:=DefaultHnt+'m/s²';
      29..31: s:=DefaultHnt+'rad/s';
      32..34: s:=DefaultHnt+'gauss';
      35, 36: s:=DefaultHnt+'mbar';
      44: s:='Course over ground'+suff+StringGrid1.Cells[sp, zl]+'°';
      45: s:=DefaultHnt+'°C';
      48, 41..43: s:=DefaultHnt+'m/s';
      posChan-1: begin                             {MAV message ID decimal}
                   e:=StrToIntDef(StringGrid1.Cells[sp, zl], 10000000);
                   s:=MsgIDtoStr(e);
                 end;

    end;
  end;

begin                                              {grundlegende Verzweigungen}
  result:=DefaultHnt;                              {default Hint}
  s:='';                                           {default bei nix gefunden}
  if StringGrid1.ColCount=csvanz then begin
    TabHintPX4csv;
  end else begin
    if StringGrid1.ColCount>=YTHPcols then begin   {Sensor PX4 überschreibt alles}
      TabHintYTHP;
    end else begin
      TabHintLegacy;
    end;
  end;
  if s<>'' then                                    {wenn etwas gefunden wurde}
    result:=s;                                     {default überschreiben}
end;

function TForm1.vms(const d: TDateTime; const w: double): string; {Weg in m --> V in m/s oder km/h}
begin
  try
    case RadioGroup3.ItemIndex of
      1: result:=FloatToStrF(w/(d*Secpd)*fkmh, ffFixed, 3, 2); {in km/h}
      2: result:=FloatToStrF(w/(d*Secpd)*fmph, ffFixed, 3, 2); {in mph}
      else
        result:=FloatToStrF(w/(d*Secpd), ffFixed, 3, 2);       {in m/s, default}
    end;
    result:=result+RadioGroup3.Items[RadioGroup3.ItemIndex];
  except
    result:='';
  end;
end;

function CleanWP(s: string): string;               {Gibt Wert aus Waypoint zurück}
var p: integer;
begin
  result:=trim(s);
  p:=pos(tr, result);                              {tr: Trenner " }
  delete(result,1,p);
  result:=GetFNr(result);
end;

function GetGPXval(GPXpar, s: string): string;     {gibt den Zahlenwert zurück}
var p, x: integer;
begin
  result:='';                                      {leer bei Nichtgefunden}
  p:=pos(GPXpar, s);
  if p>0 then begin                                {Parameter gefunden}
    for x:=p+length(GPXpar) to length(s) do
      if (s[x]='"') or
         (s[x]='<') or
         (s[x]='>') then
        break;
    result:=copy(s, p, x-p);
    result:=GetFNr(result);
  end;
end;

procedure TForm1.OverWriteVT;                      {Overwrite vehicle type for PX4 Thunderbird}
begin
  if cbThunder.Checked then begin                  {Overwrite for PX4 Thunderbird}
    SpinEdit3.Tag:=ThBid;                          {Set to SpinEdit3.Tag}
  end;
end;

procedure TForm1.GetDefVT;                         {Fill defVT depending on settings}
begin
  SpinEdit3.Tag:=defVT;                            {default legacy Yuneec}
  OverWriteVT;                                     {Overwrite vehicle type for PX4 Thunderbird}
end;

procedure TForm1.DoForm2Show(p: integer);          {Detailfenster anzeigen mit Breite p}
begin                                              {p=0 --> Breite Form1}
  Form2.Show;                                      {oberhalb Hauptfesnter anzeigen}
  if p=0 then
    Form2.Width:=PageControl1.Width
  else
    Form2.Width:=p;
  Form2.Left:=Form1.Left+PageControl1.Left;        {linksbündig zu Diagrams anzeigen}
  Form2.Top:=Form1.Top-Form2.Height-20;            {20 Pixel von Fensterleiste Kopf zeigen}
  if Form2.Top<Screen.DesktopTop then
    Form2.Top:=Screen.DesktopTop;
  Timer2.Enabled:=true;                            {Timer für Abfrage Doppelclick Form2}
end;

{http://www.joerg-buchwitz.de/temp/googlemapssyntax.htm
 https://www.google.de/maps?q=48.235367,10.0922553&z=13&om=0 }

function URLGMap(lati, long: string): string; {URL für Koordinate in Google Maps}
begin
  result:=gmapURL+'?q='+ChrKoor(lati)+sep+
                        ChrKoor(long)+'&z='+
                        gzoom+'&t=h&om=0';         {&t=k: Sat, &t=h: hybrid}
end;

{ http://wiki.openstreetmap.org/wiki/Browsing
 http://www.openstreetmap.org/?mlat=49.9676&mlon=9.9673#map=10/49.9676/9.9673&layers=Q}

function URLosm(lati, long: string): string; {URL für Koordinate in OpenStreetMap}
begin
  result:=osmURL+'?mlat='+lati+'&mlon='+long+'#map='+
          gzoom+'/'+lati+'/'+long+'&layers=S';
end;

procedure TForm1.MenuItem9Click(Sender: TObject);  {zeige OSM Karte; lat/lon}
begin
  if ListBox1.Items.Count>0 then begin
    case SpinEdit3.Tag of
      brID: OpenURL(URLosm(BrCoordFormat(StringGrid1.Cells[12,StringGrid1.Selection.Top]),
                           BrCoordFormat(StringGrid1.Cells[13,StringGrid1.Selection.Top])));
      MQcsvID: OpenURL(URLosm(StringGrid1.Cells[9, StringGrid1.Selection.Top],
                              StringGrid1.Cells[10, StringGrid1.Selection.Top]));
    else
      case Radiogroup1.ItemIndex of
        0: OpenURL(URLosm(StringGrid1.Cells[5, StringGrid1.Selection.Top],
                          StringGrid1.Cells[6, StringGrid1.Selection.Top]));
        1: OpenURL(URLosm(KoToStr(StrToFloatN(StringGrid1.Cells[2, StringGrid1.Selection.Top])),
                          KoToStr(StrToFloatN(StringGrid1.Cells[1, StringGrid1.Selection.Top]))));
      end;
    end;
  end;
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

function HTMLEnc(const s: string): string;         {Sonderzeichen escapen}
begin
  result:=StringReplace(s, '&', '&amp;', [rfReplaceAll]);   {zuerst !}
  result:=StringReplace(result, '<', '&lt;',    [rfReplaceAll]);
  result:=StringReplace(result, '>', '&gt;',    [rfReplaceAll]);
  result:=StringReplace(result, '"', '&quot;',  [rfReplaceAll]);
  result:=StringReplace(result, 'Ä', '&Auml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'Ö', '&Ouml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'Ü', '&Uuml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ä', '&auml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ö', '&ouml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ü', '&uuml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ß', '&szlig;', [rfReplaceAll]);
  result:=StringReplace(result, '€', '&euro;',  [rfReplaceAll]);
  result:=StringReplace(result, '§', '&sect;',  [rfReplaceAll]);
  result:=StringReplace(result, '°', '&deg;',   [rfReplaceAll]);
  result:=StringReplace(result, '²', '&sup2;',  [rfReplaceAll]);
  result:=StringReplace(result, '³', '&sup3;',  [rfReplaceAll]);
end;

procedure GPXheader(n, f: string; dt: TDateTime; klist: TStringList);
                   {Name, Datei,  Beginnzeit,    Ausgabeliste}
begin
  klist.Add(xmlvers);
  klist.Add(gpxvers+' creator="'+
            HTMLEnc(ExtractFileName(Application.ExeName))+'">');
  klist.Add('<metadata>');
  klist.Add('  <name>'+HTMLEnc(n)+'</name>');
  klist.Add('  <desc>'+FormatDateTime(mzf, dt)+'h - '+
              ExtractFileName(f)+'</desc>');
  klist.Add('</metadata>');
end;

procedure GPXfooter1(klist: TStringList);
begin
  klist.Add('  </trkseg>');
  klist.Add('</trk>');
end;

procedure GPXfooter2(klist: TStringList);
begin
  klist.Add('</gpx>');
end;

procedure KMLfooter1(s: string; klist: TStringList);
begin
  klist.Add(tab4+'</'+s);
  klist.Add('  </LineString>');
  klist.Add('</'+pmtag);
end;

procedure KMLfooter2(klist: TStringList);
begin
  klist.Add('</'+doctag);
  klist.Add('</kml>');
end;

function ColorToHtmlColor(AColor: TColor): string; {HTML Farbcodierung}
begin
  result:=IntToHex(ColorToRgb(AColor), 6);
  result:='#'+Copy(result, 5, 2)+Copy(result, 3, 2)+Copy(result, 1, 2);
end;

function TForm1.ColorToKMLColor(const AColor: TColor): string; {Google Farbcodierung}
begin
  Result:=IntToHex(TrackBar1.Position, 2)+IntToHex(ColorToRgb(AColor), 6);
end;

function GetRFM(fm: string; vt: integer; infl: boolean): boolean; {reale fModes, in flight}
begin
  case vt of                                       {abhängig vom vehicle type}
    1: result:=StrToIntDef(fm, 999) in rfm2;       {H920}
    2: result:=StrToIntDef(fm, 999) in rfm2;       {Q500}
    3: result:=StrToIntDef(fm, 999) in rfm3;       {Blade 350QX}
    4: result:=StrToIntDef(fm, 999) in rfm2;       {Chroma}
    5: result:=StrToIntDef(fm, 999) in rfm2;       {Typhoon H}
    6: result:=StrToIntDef(fm, 999) in rfm2;       {H920+  ??}
    ThBid: result:=StrToIntDef(fm, 999) in rfmT;   {PX4 Thunderbird}
    YTHPid: result:=infl and                       {InFlight nur bei YTH Plus}
              (StrToIntDef(fm, 999) in rfmP);      {YTH Plus}
    brID: result:=(trim(fm)<>'0'); {Breeze: fm ist splitlist[14] = AutoTakeOFF}
  else
    result:=false;                                 {999: unmöglicher f_mode}
  end;
end;

function InFlight(alt,                             {mit Höhe und Geschwindigkeit}
                  tas1, tas: double): boolean;     {prüfen, ob die Kiste fliegt}
const minAltp=1;                                   {Altitude in Meter}
      minAltm=2;                                   {negative Altitude (meist Müll)}
      deltaTas=2.4;          {Delta tas in m/s zum Ausblenden unsinniger Sprünge}
      minTas=0.5;                                  {minimum tas absolut}
begin                                              {Flug: Mindesthöhe oder tas}
  result:=true;
  if Form1.CheckBox9.Checked then                  {YTH Plus bereinigen}
    result:=((abs(alt)>minAltm) and                {Höhe - oder +, hier für -}
             (alt>minAltp)) or                     {Mindesthöhe +}
            ((abs(tas)>minTas) and                 {Mindest-tas und}
             (abs(tas-tas1)<deltaTas));            {Delta tas}
end;

function IsMStart(fm, vt: string): boolean;        {Startknopf}
begin
  case StrToIntDef(vt, 0) of
    1, 2, 6: result:=trim(fm)='8';                 {H920, Q500}
    3: result:=trim(fm)='5';                       {Blade 350QX}
    4, 5: result:=trim(fm)='8';                    {Chroma, YTH}
  else
    result:=false;
  end;
end;

{****************************************************************************
  CreateDirList: legt eine Liste aller Unterverzeichnisse an. Dabei wird
                 rekursiv gesucht, das heißt ein Unterverzeichnis wird wieder
                 auf weitere Unterverzeichnisse untersucht, bis keins mehr
                 gefunden wird.                    Result: Fehlercode
 ****************************************************************************}

function CreateDirList(Path: string; var DirList: TStringList): Integer;
var SR: TSearchRec;
begin
  path:=IncludeTrailingPathDelimiter(path);
  Result:=FindFirst(Path+wldcd, faDirectory, SR);
  try
    while Result=0 do begin
      if ((SR.Attr and faDirectory)=faDirectory) and    {nur Verzeichnisse}
         (SR.Name<>'.') and
         (SR.Name<>'..') then begin  {die nicht}
        DirList.Add(Path+SR.Name);
        CreateDirList(Path+SR.Name,DirList);  {rekursiver Aufruf, um auch die
                                              Unterverzeichnisse zu durchsuchen}
      end;
      Result:=FindNext(SR);                          {Fehlercode übergeben}
    end;
  finally
    FindClose(SR);
  end;
end;

function SuchFile(path: string; const Mask: string; list: TStringList): integer;
var SR: TSearchRec;
    s: string;
begin
  s:=IncludeTrailingPathDelimiter(path);
  result:=FindFirst(s+Mask, faAnyFile, SR);
  try
    while result=0 do begin                        {solange noch was gefunden wurde}
      Application.ProcessMessages;
      list.Add(s+SR.Name);
      result:=FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;
end;

procedure TForm1.SelDirAct(const fn: string);      {Alles neu laden}
var x:integer;
    fnum: string;
begin
  if (ComboBox2.Text>'') and DirectoryExists(ComboBox2.Text) then begin
    Timer2.Enabled:=false;
    if Form2<>nil then
      Form2.Close;              {zusätzliches Diagramm schließen beim Neuladen}
    RadioGroup1.Enabled:=false;
    BitBtn2.Enabled:=false;
    BitBtn3.Enabled:=true;
    MenuItem24.Enabled:=false;
    StatusBar1.Panels[5].Text:=DefaultStatus;
    RadioGroup1.ItemIndex:=0;                      {default Telemetry}
    GetDefVT;
    try
      if ComboBox2.Items.Count>0 then
        for x:=ComboBox2.Items.Count-1 downto 0 do {Liste putzen}
          if not DirectoryExists(ComboBox2.Items[x]) then
            ComboBox2.Items.Delete(x);
      SelectDirectoryDialog1.InitialDir:=ComboBox2.Text;
      if CheckNumTurns(ComboBox2.Text)>0 then begin
        OpenDialog1.InitialDir:=ComboBox2.Text;
        fnum:=IncludeTrailingPathDelimiter(ComboBox2.Text);
        RadioGroup1.Enabled:=true;
        BitBtn2.Enabled:=true;
        MenuItem24.Enabled:=true;
        StatusBar1.Panels[5].Text:=rsFLDir;
{FlightLog Verzeichnis in Dropdown-Liste eintragen}
        ComboBox2.Text:=ExcludeTrailingPathDelimiter(ComboBox2.Text);
        Merkliste(ComboBox2, AnzDirs);             {DropDownListe füllen}
        if PageControl1.ActivePageIndex>4 then
          PageControl1.ActivePageIndex:=0;
        if fn<>'' then begin                       {Index der Datei, wenn übergeben}
          fnum:=GetNr(ExtractFileName(fn));
          if fnum<>'' then
            ListBox1.ItemIndex:=ListBox1.Items.IndexOf(fnum);
        end;
        if ListBox1.ItemIndex<0 then
          ListBox1.ItemIndex:=0;                   {Garantiert eine Datei auswählen}
        case SpinEdit3.Tag of
          MQid: ShowMQ;                            {Sensor_*.txt vom Mantis Q}
          H5id: ShowH520;                          {*.tlog vom H520}
          else Anzeige;                            {alle anderen herkömmlich}
        end;
      end;
    except
      StatusBar1.Panels[5].Text:=rsError;
      SynEdit1.Lines.Add('''3202'+suff+StatusBar1.Panels[5].Text);
    end;
  end else
    BitBtn3.Enabled:=false;                        {Archive Button ausblenden}
end;

function TForm1.IsMantisQ(const fn: string): boolean; {Auf PX4 Quadcopter prüfen}
var inf: TMemoryStream;
    dsbuf: array[0..HBlen+lenfixP] of byte;
    b: byte;
    len: integer;
begin
  result:=false;
  inf:=TMemoryStream.Create;
  try
    inf.LoadFromFile(fn);
    try
      while inf.Position<(inf.Size-20)  do begin     {bis zum Ende der Datei}
        repeat
          b:=inf.ReadByte;
        until (b=dsIDP) or (inf.Position>=inf.Size-lenfixP);
        len:=inf.ReadByte;                         {Länge Payload mit CRC}
        if len=HBlen then begin                    {Nur Heartbeat lesen}
          inf.ReadBuffer(dsbuf, HBlen+lenfixP-2);  {Länge Rest-Datensatz mit FixPart}
          if (dsbuf[lenfix-4]=1) and               {Component ID Autopilot}
             (dsbuf[lenfix-3]=0) and               {Heartbeat}
             (dsbuf[lenfix-2]=0) and
             (dsbuf[lenfix-1]=0) then begin
            if (dsbuf[lenfix+4]=2) and             {Quadkopter}
               (dsbuf[lenfix+5]=12) then begin     {PX4}
              result:=true;
              exit;                                {nach 1. Heartbeat abbrechen}
            end;
          end;
        end;
      end;
    except
      SynEdit1.Lines.Add(fn+' Check if Mantis Q failed, wrong record format');
    end;
  finally
    inf.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.ShowSensorH(const fn: string; mode: integer);
var dsbuf: array[0..264] of byte;
    i, len, zhl, n3: integer;
    inf: TMemoryStream;
    b: byte;

  procedure AusgabeSensor;
  var i: integer;
  begin
    if StringGrid1.RowCount<(zhl+2) then
      StringGrid1.RowCount:=StringGrid1.RowCount+2000;    {neue Zeilen}
    inc(zhl);                                             {Datensätze zählen}
    if (zhl=10) and                                       {Check till line 10}
       (RadioGroup1.Tag=0) then begin
      StringGrid1.AutoSizeColumns;                        {as soon as possible}
      RadioGroup1.Tag:=1;
    end;
    for i:=0 to lenfix-3 do
      StringGrid1.Cells[i, zhl]:=IntToHex(dsbuf[i], 2);   {Header Hex}
    StringGrid1.Cells[lenfix-2, zhl]:=IntToStr(len-2);    {Payload Länge ohne CRC}
    for i:=lenfix-2 to len+lenfix-5 do
      StringGrid1.Cells[i+1, zhl]:=IntToHex(dsbuf[i], 2); {Payload in Hex}
  end;

begin
  n3:=Label3.Tag;                        {Suchspalte umkopieren, wird verändert}
  zhl:=0;
  if FileSize(fn)>lenfix then begin
    Screen.Cursor:=crHourGlass;
    ComboBox9.Text:=UpCase(trim(ComboBox9.Text));

    StringGrid1.BeginUpdate;
    StringGrid1.RowCount:=1;
    StringGrid1.ColCount:=105;                     {Spalten vorbelegen}
    StringGrid1.Cells[0, 0]:='SeqNo';
    StringGrid1.Cells[1, 0]:='SysID';
    StringGrid1.Cells[2, 0]:='CompID';
    StringGrid1.Cells[3, 0]:='TargetID';
    StringGrid1.Cells[4, 0]:='TgtSubID';
    StringGrid1.Cells[5, 0]:=csvMsgID;
    StringGrid1.Cells[lenfix-2, 0]:='lenPL';       {Länge Payload Header}
    for i:=1 to StringGrid1.ColCount-lenfix+1 do   {Payload Byte Nummern}
      StringGrid1.Cells[i+lenfix-2, 0]:='PL'+IntToStr(i);
    StringGrid1.EndUpdate;

    FillChar(dsbuf, length(dsbuf), 0);             {Datenbuffer löschen}
    inf:=TMemoryStream.Create;
    try
      inf.LoadFromFile(fn);
      SynEdit1.Lines.Add('');
      SynEdit1.Lines.Add(fn);
      StatusBar1.Panels[5].Text:=rsWait;

      StringGrid1.BeginUpdate;
      while inf.Position<(inf.Size-lenfix) do begin
        repeat                                     {RecordID suchen}
          b:=inf.ReadByte;
        until (b=dsID) or (inf.Position>=inf.Size-lenfix);
        len:=inf.ReadByte;                         {Länge Payload mit CRC}
        try
          inf.ReadBuffer(dsbuf, len+lenfix-2);     {Länge Rest-Datensatz mit FixPart}
          if mode=0 then
            AusgabeSensor                          {alles anzeigen, ohne Filter}
          else if mode=1 then begin                {mit Filter}
            try
              case n3 of                           {Spaltennummer}
                0..5: if StrToInt(ComboBox9.Text)=dsbuf[n3] then
                     AusgabeSensor;                {dezimal}
                6: if StrToInt(ComboBox9.Text)=len-2 then
                     AusgabeSensor;                {Länge Payload mit CRC}
                else
                  if (n3<(len+lenfix-3)) and
                        (Hex2Dec('$'+ComboBox9.Text)=dsbuf[n3-1]) then
                    AusgabeSensor;                 {Hexadezimal}
              end;
            except
              AusgabeSensor;                       {Kein Filter, alles ausgeben}
              StatusBar1.Panels[2].Text:=RadioGroup2.Items[RadioGroup2.ItemIndex];
              StatusBar1.Panels[5].Text:=errSelection;
              SynEdit1.Lines.Add('''3326'+suff+StatusBar1.Panels[5].Text);
            end;
          end;
        except
          SynEdit1.Lines.Add(fn+': Abrupt sensor file end');  {Msg too short}
        end;
      end;
      StringGrid1.RowCount:=zhl+1;
      StringGrid1.EndUpdate;

      StatusBar1.Panels[0].Text:=IntToStr(FileSize(fn));
      StatusBar1.Panels[1].Text:=IntToStr(zhl);
      StatusBar1.Panels[5].Text:=IntToStr(FileSize(fn));
      SpinEdit3.MaxValue:=zhl;
      if mode=1 then begin                         {Filterinfo in Statusbar}
        StatusBar1.Panels[2].Text:=rsSelection;
        StatusBar1.Panels[5].Text:=StringGrid1.Cells[n3, 0]+' = "'+
                                   ComboBox9.Text+'"';
        SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsMAVlink+tab1+rsDS);
    finally
      inf.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

{Neu bim YTH Plus/ oder Mantis Q (PX4):
 - Dateiendung *.txt
 - längster Datensatz (bisher) 156 Byte Payload
 - Fixpart 20 byte (einschließlich RecordID, aber nach MsgID mit Daten gefüllt)
 - RecordID = $FD
 - MAVlink mit MessageID 3 Bytes
 - enthält Textmessages

https://developer.yuneec.com/documentation/125/Supported-mavlink-messages
https://github.com/mavlink/c_library_v2/tree/master/common

 Dieses Format wird auch beim MantisQ als FlightLog geschrieben bzw. als
 tlog Datei beim H520.

  For Docu:
- Copy flightlogs to your PC in a seperate directory.
- Open a directory where alle the flightlog files are located.
  Files should look like this "Sensor_2018_MM_dd_hh_mm_ss.txt"
  (Sensor_+Date_Time.txt). It's not really a text file but a binary.
  Do not open with a text editor.
- Go to 'Settings' > 'Data conversion' and set 'Flight path from PX4 sensor'
  to true.
- Double click on a file number in the list on the left side.
  Then this will be analysed.
- Results of analysis (some status information, text messages and coordinates
  as GoogleMaps link) will be listed in the AppLog page.
- If 'Flight path from PX4 sensor' is set to true, a kml or gpx file
  (depending on settings) will be created in the same directory as the sensor
  files with the same name but with different extension (*.kml or *.gpx).
- If needed, an additional CSV file can be created with those and some more
  data from the PX4 sensor files/flightlos Mantis Q or tlog files H520.

- Check AppLog for error messages like motor errors or something like this.
  You can open Google Maps with the last coordinate that the Mantis has sent
  before it disappeared.

 https://github.com/mavlink/c_library_v2/tree/master/common
 }

function TForm1.ShowSensorPlus(fn: string;         {Sensordatei vom YTH Plus}
                               z: integer;
                               gx, tb, ov: boolean): boolean;
const
    homeID='home: ';                               {ID Homeposition bei Textmessages}

var dsbuf: array[0..YTHPcols] of byte;
    i, len, zhl, ele0, elemax: integer;
    msl, hbt, mmf: byte;
    inf: TMemoryStream;
    b: byte;
    maplist, outlist, datlist: TStringList;
    s, homestr: string;                            {GPX Ausgabe, homepoint}
    tstr, skoor: String;
    ftm, bg: TDateTime;
    ismq, isGPS: boolean;
    lat1, lon1: double;                            {erster gültiger Datenpunkt}
    lat2, lon2: double;                            {aktuelle Koordinaten}
    lat3, lon3: double;                            {vorherige Koordinaten}
    distg, distmax: double;
    csvarr: array[1..csvanz] of string;            {Werte für CSV-Ausgabe}
    itemp: string;


  procedure StandardAusgabe;                       {Stringgrid mit Hexwerten füllen}
  var i: integer;
  begin
    if tb then begin
      for i:=lenfix+1 to len+lenfixP-2 do          {Fixpart Teil 2 + Payload}
      StringGrid1.Cells[i+1, zhl]:=IntToHex(dsbuf[i-1], 2); {Payload in Hex}
    end;
    if zhl=10 then
      StringGrid1.AutoSizeColumns;                 {as soon as possible}
  end;

  procedure SenCSVausgabe;                         {Ausgabe Werte aus Sensor}
  var i: integer;
      c: string;
  begin
    c:=FormatDateTime(zzf+zzz, bg);                {aktueller Zeitstempel}
    for i:=1 to csvanz do                          {csv Daten ausgeben}
      c:=c+sep+csvarr[i];
    datlist.Add(c);
  end;

  function GetIntFromBuf(const p, a: integer): uint64; {Position/Anzahl Bytes}
  var i: integer;
  begin
    result:=0;
    for i:=0 to a-1 do begin
      result:=result+dsbuf[lenfix+i+p]*(256**i);
    end;
  end;

{https://www.delphipraxis.net/122021-single-byte-array-konvertieren-und-umgekehrt.html
 http://forum.lazarus-ide.org/index.php?topic=42182.0
 Direkter Typecast mit dem Zieldatentyp oder die Deklaration mittels absolute}
  function GetFloatFromBuf(const p: integer): Single; {Position, Länge immer 4}
  var i: integer;
      wfl: array[0..3] of Byte;
      wx: Single absolute wfl;
  begin
    result:=0;
    for i:=0 to 3 do                               {Endianess prüfen (to/downto)}
      wfl[i]:=dsbuf[lenfix+i+p];                   {4 byte aus Buffer ausschneiden}
    result:=wx;                                    {Typecast mittels absolute}
  end;

  procedure TextAusgabe;                           {Ausgabe als Text in Zeile zl}
  var i: integer;
      st, tm, ch: string;
      splitlist: TStringList;
//      wx: uint64;
  begin
    ch:=csvarr[posChan];                  {ursprünglichen Wert zwischenspeichern}
    st:=Format('%6d', [zhl])+tab2+
        FormatDateTime(zzf, bg)+tab2+
        MAVseverity(dsbuf[lenfix])+suff+'''';
    StringGrid1.Cells[lenfix+2, zhl]:=IntToStr(dsbuf[lenfix]); {Severity dezimal}
    tm:='';
    for i:=lenfix+2 to len+lenfixP-12 do begin     {Fixpart Teil 2 + Payload}
      StringGrid1.Cells[i+1, zhl]:=Char(dsbuf[i-1]); {Rest Payload als Text}
      tm:=tm+Char(dsbuf[i-1]);                     {Textmessage zusammenstellen}
    end;
    csvarr[posChan]:='"'+tm+'"';
    st:=st+tm;
    for i:=len+lenfixP-11 to len+lenfixP-2 do      {8 Byte Sig + 2 CRC}
      StringGrid1.Cells[i+1, zhl]:=IntToHex(dsbuf[i-1], 2);
//    wx:=GetIntFromBuf(len+lenfixP-12-lenfix, 8); {Aufsteigende Nummer hinten dran?}
    if pos(emcyID, st)>0 then begin                {EMERGECY gefunden}
      topp[z, 6]:=topp[z, 6] or 256;
      result:=true;
    end;
    if not ov then begin
      SynEdit1.Lines.Add(st+'''');                 {Textmessage speichern}
      if pos(homeID, tm)>1 then begin              {Homepoint als Link}
        distg:=0;                                  {Entfernungswerte zurücksetzen}
        distmax:=0;
        splitlist:=TStringList.Create;
        try
          tm:=StringReplace(tm, ',', '', [rfReplaceAll]);
          splitlist.DelimitedText:=tm;
          homestr:=Format('%-10s', [homeID])+
                   URLGmap(splitlist[1], splitlist[2]);
        finally
          splitlist.Free;
        end;
      end;
      SenCSVAusgabe;
    end;
    csvarr[posChan]:=ch;
  end;

{https://github.com/mavlink/c_library_v2/blob/master/common/mavlink_msg_gps_raw_int.h
 https://mavlink.io/en/messages/common.html#GPS_RAW_INT}

  procedure GPSAusgabe;                            {GPS_RAW_INT auswerten}
  var tme: uint64;                                 {unsignet Integer}
      lat, lon, ele: integer;                      {int32}
      eph, epv, vel, cog, hacc: uint64;
      distp, dists: double;                        {Entfernung zum "Start"}

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    distp:=0;
    dists:=0;
    lat:=GetIntFromBuf(8, 4);                      {uint32}
    lon:=GetIntFromBuf(12, 4);
    tme:=GetIntFromBuf(0, 8);                      {in mysec}
    bg:=tme/(Secpd*1000000);                       {Zeitstempel überall verfügbar}
    ele:=GetIntFromBuf(16, 4);                     {Höhe}
    tstr:=FormatDateTime(dzf, ftm)+'T'+
          FormatDateTime(zzf, bg)+'Z';             {Zeitstring überall verfügbar}
    if ((lat>0) or (lon>0)) and                    {nur bei gültigen Koordinaten}
       (ele<maxxh*1000) then begin                 {nur sinnvolle Höhe}
      lat2:=lat/10000000;                          {nur einmal rechnen}
      lon2:=lon/10000000;                          {und überall verfügbar}
      csvarr[4]:=FormatFloat(ctfl, ele/1000);      {altitude}
      csvarr[5]:=FloatToStr(lat2);                 {Koordinaten}
      csvarr[6]:=FloatToStr(lon2);
{GPS HDOP horizontal dilution of position (unitless)}
      eph:=GetIntFromBuf(20, 2);
      csvarr[23]:=IntToStr(eph);
{GPS HVOP vertical dilution of position (unitless)}
      epv:=GetIntFromBuf(22, 2);
      csvarr[24]:=IntToStr(epv);
      vel:=GetIntFromBuf(24, 2);                   {[cm/s] GPS ground speed}
      csvarr[25]:=FormatFloat(ctfl, vel/100);      {ground speed in m/s}
{[cdeg] Course over ground (NOT heading, but direction of movement)
 in degrees * 100, 0.0..359.99 degrees}
      cog:=GetIntFromBuf(26, 2);
      csvarr[44]:=FormatFloat(ctfl, cog/100);      {Course over ground in Grad}
      csvarr[9]:=GPSfixType(IntToStr(dsbuf[lenfix+28]));     {GPS fix type}
      csvarr[10]:=IntToStr(dsbuf[lenfix+29]);      {Num sats visible}
      hacc:=GetIntFromBuf(38, 4);                  {Altitude uncertainty in mm}
      csvarr[21]:=FormatFloat(mlfl, hacc/1000);    {Vert. acc in m}
      hacc:=GetIntFromBuf(34, 4);                  {Position uncertainty in mm}
      csvarr[22]:=FormatFloat(mlfl, hacc/1000);    {Horiz. acc in m}

      if not isGPS then begin                      {erster gültiger GPS Datensatz}
        ele0:=ele;                                 {erster Höhenwert für HDiagramm}
        lat1:=lat2;                                {"Start"-Koordinaten}
        lon1:=lon2;
        lat3:=lat2;                                {init vorherige Koordinaten}
        lon3:=lon2;
      end;

      isGPS:=true;                                 {1. Datensatz erledigt}
      if ele>elemax then elemax:=ele;              {Gipfelhöhe}
      distp:=DeltaKoord(lat1, lon1, lat2, lon2);   {Entfernung zum 1. Punkt}
      dists:=DeltaKoord(lat3, lon3, lat2, lon2);   {Entfernung zum vorherigen Punkt}
      if distp>distmax then                        {maximale Entfernung ermitteln}
        distmax:=distp;
      distg:=distg+dists;                          {Länge Route ermitteln}
      lat3:=lat2;                                  {Koordinaten merken}
      lon3:=lon2;

      if gx then begin                             {Ausgabe GPX oder KML}
        if RadioGroup2.ItemIndex=2 then begin      {GPX}
          skoor:=GPXlat+FloatToStr(lat2)+
                 GPXlon+FloatToStr(lon2)+'"> ';
          if s='' then begin                       {Startpunkt erkannt}
            maplist.Add('<wpt '+skoor);            {Startpunkt}
            maplist.Add(tab2+'<time>'+tstr+'</time>');
            maplist.Add(tab2+'<name>Start</name>');
            maplist.Add(GPXet1);
            maplist.Add('<trk>');
            maplist.Add(tab2+'<name>'+ExtractFileName(fn)+'</name>');
            maplist.Add(tab2+'<trkseg>');
          end;
          s:=tab4+'<trkpt '+skoor+GPXele+csvarr[4]+
                  '</ele> <time>'+tstr+'</time></trkpt>';
          maplist.Add(s);

        end else begin                             {KML/KMZ}
          skoor:=FloatToStr(lon2)+tab1+FloatToStr(lat2);
          if s='' then begin                       {Startpunkt erkannt}
            maplist.Add('<'+pmtag);                {Startpunkt}
            maplist.Add('<TimeStamp><'+KMLwhen+tstr+
                        '</'+KMLwhen+'</TimeStamp>');
            maplist.Add('<styleUrl>#starting</styleUrl>');
            maplist.Add('<Point><'+cotag+
                        StringReplace(skoor, tab1, sep, [rfReplaceAll])+
                        '</'+cotag+'</Point>');
            maplist.Add('</'+pmtag);
            maplist.Add('<'+pmtag);
            maplist.Add(tab2+'<name>'+ComboBox1.Text+'</name>');
            maplist.Add(tab2+'<description>'+ExtractFileName(fn)+'</description>');
            maplist.Add(tab2+'<styleUrl>#Flightpath</styleUrl>');
            maplist.Add(tab2+'<gx:Track>');
            maplist.Add(tab2+'<'+amtag+RadioGroup5.Items[RadioGroup5.ItemIndex]+
                             '</'+amtag);
            if CheckBox11.Checked then maplist.Add(tab2+extru);
          end;
          s:=tab4+'<gx:coord>'+skoor+tab1+csvarr[4]+'</gx:coord>';
          outlist.Add(s);
          maplist.Add(tab4+'<'+KMLwhen+tstr+'</'+KMLwhen);
        end;
      end;

      if tb then begin                               {Höhendiagramm füllen}
        case msl of
          2: Chart1BarSeries1.AddXY(bg, (ele-ele0)/1000);   {fuchsia like Angle}
          3: Chart1BarSeries7.AddXY(bg, (ele-ele0)/1000);   {blue}
          4: Chart1BarSeries3.AddXY(bg, (ele-ele0)/1000);   {red}
        else
          Chart1BarSeries2.AddXY(bg, (ele-ele0)/1000); {relative Höhe zum ersten Wert}
        end;
        Chart1LineSeries2.AddXY(bg, (ele-ele0)/1000); {Hüllkurve}
        Chart1LineSeries1.AddXY(bg, distp);
        SenCSVAusgabe;
      end;
    end;
  end;

{dsbuf[0]: vtol_state, dsbuf[1]: landed_state
           vtol_state immer 0}
  procedure ExtAusgabe;                            {Extended_SYS_State 245 ($F5)}
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
                         {MAV_LANDED_STATE nur ausgeben, wenn sich etwas ändert}
    csvarr[20]:=MLStoStr(dsbuf[lenfix+1]);         {MAV landed state}
    if dsbuf[lenfix+1]<>msl then begin
      if not cbReduced.Checked then
        SynEdit1.Lines.Add(Format('%6d', [zhl])+tab2+
                           FormatDateTime(zzf, bg)+tab2+
                           csvMAVland+' ($F5)'+suff+
                           csvarr[20]);
      msl:=dsbuf[lenfix+1];                        {letzten Wert merken}
    end;
    SenCSVAusgabe;
  end;

  procedure Heartbeat;                             {MAV_State aus Heartbeat}
  var cm: uint64;
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      if dsbuf[lenfix+7]<>hbt then begin           {ID 1. Heartbeat}
        if (hbt=0) and                             {nur beim 1. Mal}
           (dsbuf[lenfix+4]=2) and                 {Quadkopter}
           (dsbuf[lenfix+5]=12) then begin         {PX4}
          ismq:=true;
          if not cbReduced.Checked then
            SynEdit1.Lines.Add(''''+Format('%17s', [''])+rsVType+suff+
                               VtypeToStr(MQid));  {4 Rotor = Mantis Q}
        end;
        if not cbReduced.Checked then
          SynEdit1.Lines.Add(Format('%6d', [zhl])+tab2+
                             FormatDateTime(zzf, bg)+tab2+
                             MSTtoStr(dsbuf[lenfix+7]));
        hbt:=dsbuf[lenfix+7];                      {letzten Wert merken}
        cm:=GetIntFromBuf(0, 4);                   {custom mode}

{https://github.com/Dronecode/DronecodeSDK/blob/23b76bcd208ce12159e9bd089451ff7c04e284ab/core/px4_custom_mode.h#L50-L59}

        csvarr[17]:=IntToHex(cm, 2);
        csvarr[18]:=IntToHex(hbt, 2);              {MAV state}
        csvarr[19]:=IntToHex(dsbuf[lenfix+6], 2);  {MAV mode flag}
      end;
      if dsbuf[lenfix+6]<>mmf then begin           {nur ausgeben, wenn sich etwas ändert}
        if not cbReduced.Checked then
          SynEdit1.Lines.Add(Format('%6d', [zhl])+tab2+
                             FormatDateTime(zzf, bg)+tab2+
                             MMFtoStr(dsbuf[lenfix+6]));
        mmf:=dsbuf[lenfix+6];                      {letzten Wert merken}
      end;
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure HighresIMU;                            {Msg HIGHRES_IMU (105)}
  var tme: uint64;
      wrt: float;
      it: string;                                  {aktuelle IMU temp}
      i: integer;
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 8);                    {in mysec, uint64}
      bg:=tme/(Secpd*1000000);                     {Zeitstempel überall verfügbar}
      for i:=0 to 11 do begin
        wrt:=GetFloatFromBuf((i*4)+8);             {12 Werte ab [m/s/s] X acceleration}
        csvarr[i+26]:=FormatFloat(ctfl, wrt);
      end;
      wrt:=GetFloatFromBuf(56);                    {IMU temperature [deg C]}
      it:=FormatFloat(dzfl, wrt);
      csvarr[45]:=it;
      if it<>itemp then begin                      {nur wenn Temp sich ändert}
        if not cbReduced.Checked then
          Synedit1.Lines.Add(Format('%6d', [zhl])+tab2+
                             FormatDateTime(zzf, bg)+tab2+csvIMUtemp+
                             suff+it+'°C');
        itemp:=it;                                 {Vergleichswert merken}
      end;
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure LocalPosNed;                           {Msg LOCAL_POSTION_NED (32)}
  var tme: uint32;
      vx, vy, vz: float;
      i: integer;
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/(Secpd*1000);                        {Zeitstempel überall verfügbar}
      for i:=0 to 2 do begin
        vx:=GetFloatFromBuf((i*4)+4);              {3 Werte ab [m] X Position}
        csvarr[i+38]:=FormatFloat(ctfl, vx);
      end;
      vx:=GetFloatFromBuf(16);                     {[m/s] X Speed}
      vy:=GetFloatFromBuf(20);                     {[m/s] Y Speed}
      vz:=GetFloatFromBuf(24);                     {[m/s] Z Speed}
      csvarr[41]:=FormatFloat(ctfl, vx);
      csvarr[42]:=FormatFloat(ctfl, vy);
      csvarr[43]:=FormatFloat(ctfl, vz);
      csvarr[7]:=FormatFloat(ctfl,                 {Orginal Spalte tas}
        sqrt((vx*vx)+(vy*vy)+(vz*vz)));            {tas in m/s berechnet}
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure vfr_hud;                               {Msg VFR_HUD (74)}
  var wrt: float;
      thr: uint;
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
(* Mehr Fragen als Antworten! Lieber nichts überschreiben.
      wrt:=GetFloatFromBuf(0);                     {[m/s] Current indicated airspeed (IAS)}
      csvarr[7]:=FormatFloat(ctfl, wrt);
      wrt:=GetFloatFromBuf(4);                     {[m/s] Current ground speed}
      csvarr[25]:=FormatFloat(ctfl, wrt);
      wrt:=GetFloatFromBuf(8);                     {[m] Current altitude (MSL)}
      csvarr[4]:=FormatFloat(ctfl, wrt);                                        *)

      wrt:=GetFloatFromBuf(12);                    {[m/s] Current climb rate}
      csvarr[48]:=FormatFloat(ctfl, wrt);
   (* Werte passen nicht zu Maßeinheiten.
      hdg:=GetIntFromBuf(16, 2);                   {[deg] Current heading in
                                                    compass units (0-360, 0=north)}
      csvarr[50]:=IntToStr(hdg);          *)
      thr:=0;                                      {default: 0%}
      case len of                                  {pos depending on lenght ??}
        17: thr:=GetIntFromBuf(18, 2);             {[%] Current throttle setting (0 to 100))}
        18: thr:=GetIntFromBuf(19, 2);
        19: thr:=GetIntFromBuf(20, 2);
      end;
      csvarr[49]:=FormatFloat(ctfl, thr*100/255);
//      csvarr[49]:=IntToStr(thr);
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure Attitude;                              {Msg ATTITUDE (30)}
  var tme: uint32;
      wrt: float;
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/(Secpd*1000);                        {Zeitstempel überall verfügbar}
      wrt:=GetFloatFromBuf(4);                     {[rad] Roll angle (-pi..+pi)}
      csvarr[11]:=FormatFloat(mlfl, wrt);
      wrt:=GetFloatFromBuf(8);                     {[rad] Pitch angle (-pi..+pi)}
      csvarr[13]:=FormatFloat(mlfl, wrt);
      wrt:=GetFloatFromBuf(12);                    {[rad] Yaw angle (-pi..+pi)}
      csvarr[12]:=FormatFloat(mlfl, wrt);
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure SensorStatus;                          {MAV_SYS_STATUS}
  var volt, curr, load: double;
      sst: uint64;                                 {UINT unsigned Integer / bits}
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    volt:=GetIntFromBuf(14, 2)/1000;
    curr:=GetIntFromBuf(16, 2)/100;
{load: Maximum usage in percent of the mainloop time.
 Values: [0-1000] - should always be below 1000 (100%)}
    load:=GetIntFromBuf(12, 2)/10;
    csvarr[47]:=FormatFloat(dzfl, load);           {SW-load in %}
    csvarr[2]:=FormatFloat(dzfl, volt);            {Voltage in V}
    csvarr[3]:=FormatFloat(ctfl, curr);            {Current in A}
    csvarr[46]:=IntToStr(dsbuf[lenfix+30]);        {Battery remaining %}
    if not cbReduced.Checked then
      Synedit1.Lines.Add(Format('%6d', [zhl])+tab2+
                         FormatDateTime(zzf, bg)+tab2+csvVolt+
                         suff+csvarr[2]+'V'+
                         tab4+csvAmp+suff+csvarr[3]+'A'+
                         tab4+csvSWload+suff+csvarr[47]+'%');
    sst:=GetIntFromBuf(0, 4);                      {Sensor present}
    if not cbReduced.Checked then
      Synedit1.Lines.Add('                  onboard_control_sensors_present'+suff+
                         MSenStat(sst));           {MAV_SYS_STATUS_SENSOR}
    sst:=GetIntFromBuf(4, 4);                      {Sensor enabled}
    if (sst and 32)>0  then                        {GPS status}
      csvarr[16]:='true'
    else
      csvarr[16]:='false';
    if not cbReduced.Checked then
      Synedit1.Lines.Add('                  onboard_control_sensors_enabled'+suff+
                         MSenStat(sst));
{[c%] Communication drop rate, (UART, I2C, SPI, CAN), dropped packets on all
 links (packets that were corrupted on reception on the MAV)}
    sst:=GetIntFromBuf(18, 2);                     {Drop rate}
    csvarr[14]:=FormatFloat(ctfl, sst/100);

    sst:=GetIntFromBuf(8, 4);                      {Sensor health}
    csvarr[15]:=IntToHex(sst, 2);
    if not cbReduced.Checked then
      Synedit1.Lines.Add('                  onboard_control_sensors_health '+suff+
                         MSenStat(sst));
    if tb then begin                               {Anzeige in Schnellanalyse}
      Chart3LineSeries1.AddXY(bg, volt);           {Spannung}
      Chart4LineSeries1.AddXY(bg, curr);           {Strom (nicht bei Mantis Q)}
      Chart5LineSeries1.AddXY(bg, load);           {SW-Load in %}
    end;
    SenCSVAusgabe;                                 {CSV Datensatz schreiben}
  end;

  procedure RCchannels;
  var i: integer;
      wrt: uint32;
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      wrt:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=wrt/(Secpd*1000);                        {Zeitstempel überall verfügbar}
      csvarr[posChan]:=IntToStr(dsbuf[lenfix+40]); {Channels used}
      csvarr[1]:=IntToStr(dsbuf[lenfix+41]);       {rssi}
      for i:=0 to 17 do begin
        wrt:=GetIntFromBuf((i*2)+4, 2);            {18 Channels}
        csvarr[i+posChan+1]:=IntToStr(wrt);
        if wrt>65534 then csvarr[i+posChan+1]:=''; {ungenutzte Kanäle}
      end;
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure ParamValue;                            {PARAM_VALUE (22)}
  var i: integer;
      num, idx: uint16;
      paramID, wrt: string;
  begin
    StandardAusgabe;                               {Hex values in CSV table}
    num:=GetIntFromBuf(4, 2);                      {Total number of onboard parameters}
    idx:=GetIntFromBuf(6, 2);                      {Index of this onboard parameter}
    wrt:=FloatToStr(GetFloatFromBuf(0));           {Onboard paramaeter value}
    if (GetIntFromBuf(1, 3)=0) and                 {Extreme small values, masks}
       (dsbuf[lenfix]>0) then
      wrt:='$'+IntTohex(dsbuf[lenfix], 2)+'='+
           IntToBin(dsbuf[lenfix], 8, 4);          {Overwrite ...E-44, ...E-45}
    paramID:='';
    for i:=8 to 23 do begin                        {Onboard parameter ID}
      if dsbuf[lenfix+i]=0 then                    {Terminated by NULL}
        break;
      paramID:=paramID+Char(dsbuf[lenfix+i]);
    end;
    csvarr[57]:=paramID;
    csvarr[58]:=wrt;
    SynEdit1.Lines.Add(Format('%6d', [zhl])+tab2+  {Onboard parameter value}
                       FormatDateTime(zzf, bg)+tab2+
                       Format('%-16s', [paramID])+suff+
                       'Type'+suff+IntToStr(dsbuf[lenfix+24])+kma+
                       'Index'+suff+IntToStr(idx)+kma+
                       'Count'+suff+IntToStr(num)+kma+
                       'Value'+suff+wrt);
    SenCSVAusgabe;                                 {CSV Datensatz schreiben}
  end;

  procedure AusgabeSensor;                         {Datenausgabe abh. von MsgID}
  var i, e: integer;
  begin
    e:=GetIntFromBuf(-3, 3);                       {MsgID 3 Byte als Zahl}
    csvarr[posChan-1]:=IntToStr(e);                {Message ID dezimal hinten}
    if tb then begin                               {Alle Datenanzeigen füllen}
      if StringGrid1.RowCount<(zhl+2) then
        StringGrid1.RowCount:=StringGrid1.RowCount+2000;  {neue Zeilen}
      inc(zhl);                                    {Datensätze zählen}
      case e of                                    {Ausgabe bekannter Messages}
        0:   Heartbeat;                            {HEARTBEAT (0) ohne Zeitstempel}
        1:   SensorStatus;                         {MAV_SYS_STATUS}
        22:  ParamValue;                           {PARAM_VALUE ($16)}
        24:  GPSAusgabe;                           {GPS_RAW_INT ($18) auswerten}
        30:  Attitude;                             {ATTITUDE ($1E)}
        32:  LocalPosNed;                          {LOCAL_POSITION_NED ($1E)}
        65:  RCchannels;                           {RC_CHANNELS ($41)}
        74:  vfr_hud;                              {VFR_HUD ($4A)}
        105: HighresIMU;                           {HIGHRES_IMU ($69)}
        245: ExtAusgabe;                           {Extended_SYS_State ($F5)}
        253: TextAusgabe;                          {Statustext ($FD)}
      else                                         {Standard Ausgabe}
        StandardAusgabe;                           {Hexwerte für alle anderen Msg}
      end;
      for i:=0 to lenfix-1 do begin                {FixPart Teil 1 für alle}
        StringGrid1.Cells[i, zhl]:=IntToHex(dsbuf[i], 2);           {in Hex}
//        StringGrid1.Cells[i, zhl]:=IntToStr(dsbuf[i]);            {in Dec}
      end;
      StringGrid1.Cells[lenfix+1, zhl]:=IntToStr(len);  {Payload Länge eintragen}
      StringGrid1.Cells[lenfix, zhl]:=MsgIDtoStr(e);    {Message Name}
    end else begin
      if ov then begin
        if e=253 then TextAusgabe;
      end else
        if e=24 then GPSAusgabe;
    end;
  end;

begin
  zhl:=0;
  msl:=0;                                          {MAV landed_state undef}
  hbt:=0;                                          {MAV state uninit/unknown}
  mmf:=0;                                          {MAV Mode Flags}
  s:='';                                           {noch keine Ausgabe}
  ele0:=0;                                         {Höhe bei Start}
  distmax:=0;                                      {maximale Entfernung}
  distg:=0;                                        {länge Route}
  elemax:=-1000000;
  tstr:='';
  skoor:='';
  bg:=0;                                           {Zeitstempel allg}
  isGPS:=false;
  itemp:='';
  homestr:='';                                     {URL Homepoint, wenn vorhanden}
  ismq:=false;
  topp[z, 5]:=0;                                   {Pointer für Suche Null setzen}
  result:=false;
  for i:=0 to csvanz do
    csvarr[i]:='';
  if FileSize(fn)>lenfixP then begin
    Screen.Cursor:=crHourGlass;
    if tb then begin
      HDiaInit;                                    {Höhendiagramm initalisieren}
      KursorAus;
      SetSensorEnv;
      Chart1ConstantLine2.Position:=0;
      Chart1ConstantLine2.Active:=false;
      Chart3LineSeries1.Clear;
      Chart4LineSeries1.Clear;
      Chart5LineSeries1.Clear;
      Chart3.ZoomFull;
      Chart4.ZoomFull;
      Chart5.ZoomFull;
      Chart3LineSeries1.SeriesColor:=ColorButton2.ButtonColor;
      Chart4LineSeries1.SeriesColor:=ColorButton3.ButtonColor;
      Chart5LineSeries1.SeriesColor:=ColorButton4.ButtonColor;
      Chart1.AxisList[2].Title.Caption:=rsDistHome+' [m]'; {Entfernung}
      Chart3.AxisList[0].Title.Caption:=csvVolt+' [V]';    {y-Achse top}
      Chart4.AxisList[0].Title.Caption:=csvAmp+' [A]';     {y-Achse middle}
      Chart5.AxisList[0].Title.Caption:=csvSWload+' [%]';  {y-Achse bottom}
      if (PageControl1.ActivePageIndex>3) or
         (PageControl1.ActivePageIndex=0) then
        PageControl1.ActivePageIndex:=1;           {Zur Tabelle springen}

      StringGrid1.BeginUpdate;
      StringGrid1.RowCount:=1;
      StringGrid1.ColCount:=YTHPcols;              {Spaltenanzahl vorbelegen}
      StringGrid1.Cells[0, 0]:=Fix+'1';            {Wirklicher FixPart}
      StringGrid1.Cells[1, 0]:=Fix+'2';
      StringGrid1.Cells[2, 0]:='SeqNo';            {bekannte Spalten}
      StringGrid1.Cells[3, 0]:='SysID';            {The H520 uses the mavlink
                system ID 1. This is currently set fixed and can't be changed}
      StringGrid1.Cells[4, 0]:='CompID';
      StringGrid1.Cells[5, 0]:=csvMsgID+'0';
      StringGrid1.Cells[6, 0]:=csvMsgID+'1';
      StringGrid1.Cells[7, 0]:=csvMsgID+'2';
      StringGrid1.Cells[8, 0]:='MsgName';
      StringGrid1.Cells[lenfix+1, 0]:='lenPL';     {Länge Payload Header als Trenner}

      for i:=lenfix+1 to lenfixP-2 do              {Rest Fixpart, aber mit Daten}
        StringGrid1.Cells[i+1, 0]:=fix+IntToStr(i);
      for i:=lenfixP-1 to StringGrid1.ColCount-2 do        {Payload Byte Nummern}
        StringGrid1.Cells[i+1, 0]:='PL'+IntToStr(i-lenfixP+2);

      StringGrid1.ColWidths[8]:=110;               {Msg Name}
      StringGrid1.EndUpdate;
    end;
    FillChar(dsbuf, length(dsbuf), 0);             {Datenbuffer löschen}
    maplist:=TStringList.Create;
    outlist:=TStringList.Create;
    datlist:=TStringList.Create;                   {Ausgabedatei für csv-Daten}
    inf:=TMemoryStream.Create;
    try
      ftm:=FileDateToDateTime(FileAge(fn));
      inf.LoadFromFile(fn);
      StatusBar1.Panels[5].Text:=rsWait;
      StatusBar1.Update;
      SynEdit1.Lines.Add('');
      SynEdit1.Lines.Add(fn);

      if gx then begin                             {Eventuell KML oder GPX erzeugen}
        if RadioGroup2.ItemIndex=2 then begin
          GPXHeader(ComboBox1.Text, fn, ftm, maplist)
        end else begin
          KMLHeader(fn, ftm, maplist);
        end;
      end;

      if tb then
        StringGrid1.BeginUpdate;
      SynEdit1.BeginUpdate(false);                 {Without UnDo Block}
      while inf.Position<(inf.Size-lenfixP) do begin {bis zum Ende der Datei}
        len:=0;                                    {Reset for error detection}
        try
          repeat
            b:=inf.ReadByte;
          until (b=dsIDP) or (inf.Position>inf.Size-lenfixP);
          len:=inf.ReadByte;                       {Länge Payload mit CRC}
          inf.ReadBuffer(dsbuf, len+lenfixP-2);    {Länge Rest-Datensatz mit
                                    FixPart, aber ohne $FD und Längen-Byte (-2)}
          AusgabeSensor;                           {alles anzeigen, ohne Filter}
        except
          if zhl>0 then
            SynEdit1.Lines.Add('''Broken record No'''+suff+
                               IntToStr(zhl)+', Byte'+suff+IntToHex(b, 2)+
                               ', Payload length'+suff+IntToStr(len));
{Usually the last record in a tlog file is too short compared to payload length,
 thus this exception will be raised for each file at the end.}
        end;
      end;
      SynEdit1.EndUpdate;
      if tb then begin
        StringGrid1.RowCount:=zhl+1;
        StringGrid1.EndUpdate;
      end;

      if gx and
         (skoor<>'') then begin                    {KML oder GPX speichern}
        if RadioGroup2.ItemIndex=2 then begin      {GPX}
          GPXfooter1(maplist);
          maplist.Add('<wpt '+skoor);              {Landepunkt}
          maplist.Add(' <time>'+tstr+'</time>');
          maplist.Add(' <name>Stop</name>');
          maplist.Add(GPXet1);
          GPXfooter2(maplist);
          maplist.SaveToFile(ChangeFileExt(fn,
                             RadioGroup2.Items[RadioGroup2.ItemIndex]));
        end else begin                             {KML/KMZ}
          for i:=0 to outlist.Count-1 do
            maplist.Add(outlist[i]);
          maplist.Add('  </gx:Track>');
          maplist.Add('</'+pmtag);                 {End playable track}
          maplist.Add('<'+pmtag);                  {Landepunkt}
          maplist.Add('<TimeStamp><'+
                      KMLwhen+tstr+'</'+KMLwhen+'</TimeStamp>');
          maplist.Add('<styleUrl>#landing</styleUrl>');
          maplist.Add('<Point><'+cotag+
                      StringReplace(skoor, tab1, sep, [rfReplaceAll])+
                      '</'+cotag+'</Point>');
          maplist.Add('</'+pmtag);

          KMLFooter2(maplist);
          maplist.SaveToFile(ChangeFileExt(fn, RadioGroup2.Items[0]));
          if RadioGroup2.ItemIndex=1 then
            MacheKMZ(ChangeFileExt(fn, RadioGroup2.Items[0]));
        end;
      end;

      if tb then begin
        SpinEdit3.MaxValue:=zhl;
        StatusBar1.Panels[5].Text:=fn;
        StatusBar1.Panels[0].Text:=IntToStr(FileSize(fn));
        StatusBar1.Panels[1].Text:=IntToStr(zhl);
        s:=vTypeToStr(YTHPid)+tab4;                {Default: H Plus}
        if ExtractFileExt(fn)=hext then s:=vTypeToStr(H5id)+tab4;
        if ismq then s:=vTypeToStr(MQid)+tab4;
        Chart1.Title.Text.Add(s+ExtractFileName(fn));
        if ele0<>0 then begin
          SynEdit1.Lines.Add('');
          Synedit1.Lines.add(Format('%-10s', [capLabel13+suff])+
                             URLGmap(FloatToStr(lat1),
                                     FloatToStr(lon1)));
{GPS Datensatz in AppLog als Link mit Google Maps Adresse ablegen}
          if homestr<>'' then
            SynEdit1.Lines.Add(homestr);
          SynEdit1.Lines.add(Format('%-10s', [capLabel14+suff])+
                             URLGmap(FloatToStr(lat2),
                                     FloatToStr(lon2)));
          s:=Format('%-25s', [rsGPSh+rsAbsh+suff])+
             Format('%7.1f', [ele0/1000])+'m';
          Chart1.Title.Text.Add(s);                {absolute Höhe für Null}
          SynEdit1.Lines.Add(s);
          SynEdit1.Lines.Add(Format('%-25s', ['Absolute '+rsGridCell5+suff])+
                             Format('%7.1f', [elemax/1000])+'m');
          SynEdit1.Lines .Add(Format('%-25s', ['Relative '+rsGridCell5+suff])+
                              Format('%7.1f', [(elemax-ele0)/1000])+'m');
          SynEdit1.Lines.Add('');
          SynEdit1.Lines .Add(Format('%-25s', [rsGridCell6+suff])+
                              Format('%7.1f', [distmax])+'m');
          SynEdit1.Lines .Add(Format('%-25s', [rsGridCell7+suff])+
                              Format('%7.1f', [distg])+'m');
        end;
        Chart1.Title.Visible:=true;                {Diagrammtitel anzeigen}
        SynEdit1.Lines.Add('');
        SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsMAVLink+tab1+rsDS);
        SynEdit1.Lines.Add('');
        if (datlist.Count>2) and
           CheckBox8.Checked then begin
           s:=csvTime+sep+csvRSSI+sep+csvVolt+sep+      {CSV Header generieren}
              csvAmp+sep+csvAlt+sep+csvLat+sep+csvLon+sep+
              csvTas+sep+'8'+sep+csvGPSfixT+sep+
              csvNumSats+sep+csvRoll+sep+csvYaw+sep+
              csvPitch+sep+csvDropRate+sep+
              csvSenHealth+sep+csvGPSen+sep+
              csvCustMode+sep+csvMAVstate+sep+csvMAVmode+sep+
              csvMAVland+sep+csvVacc+sep+csvHacc+sep+
              csvHDOP+sep+csvVDOP+sep+csvHSpeed+sep+
              'xAccl'+sep+'yAccl'+sep+'zAccl'+sep+
              'xGyro'+sep+'yGyro'+sep+'zGyro'+sep+
              'xMag'+sep+'yMag'+sep+'zMag'+sep+
              'Abs pressure'+sep+'Diff pressure'+sep+'Pressure alt'+sep+
              'xPosition'+sep+'yPosition'+sep+'zPosition'+sep+
              'xSpeed'+sep+'ySpeed'+sep+'zSpeed'+sep+
              csvCOG+sep+csvIMUtemp+sep+csvCap+sep+
              csvSWload+sep+'Climb rate'+sep+'49'+sep+'50'+sep+'51'+sep+
              '52'+sep+'53'+sep+'54'+sep+'55'+sep+'56'+sep+
              'Onboard paramater name'+sep+'Parameter value'+sep+
              csvMsgID+sep+'CH used';
          for i:=1 to 18 do
            s:=s+sep+'CH'+IntToStr(i);
          datlist.Insert(0, s);
          datlist.SaveToFile(ChangeFileExt(fn, fext));  {als *.csv speichern}
        end;
      end;
    finally
      inf.Free;
      maplist.Free;
      outlist.Free;
      datlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.ReadMAV(const mode: integer);     {Sensor Datei H auslesen}
var fn: string;
begin
  if ListBox1.Items.Count>0 then begin
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+npath+
        PathDelim+nfile+ListBox1.Items[ListBox1.ItemIndex]+sext;  {Sensor}
    ShowSensorH(fn, mode);
  end;
end;

function ReadFW(const fn: string; var fwout: TarrFW): integer;
var i, rb: integer;
    s: string;
    buf: array[0..127] of byte;
begin                                              {Firmwarestände auslesen}
  for i:=low(fwout) to high(fwout) do
    fwout[i]:='';                                  {Array immer löschen}
  result:=0;
  with TFileStream.Create(fn, fmOpenRead) do
  try
    rb:=Read(buf, length(buf));
    if rb>FWsz then begin
      s:='';
      for i:=0 to rb-1 do begin
        if (buf[i]=13) or
           (buf[i]=10) then begin
          fwout[result]:=s;                        {letzten Wert noch ausgeben}
          break;                                   {Schleife abbrechen bei CR/LF}
        end;
        if chr(buf[i])=dstr then begin             {Semicolon als Trenner}
          fwout[result]:=s;                        {FW-Version ausgeben}
          s:='';
          result:=result+1;
          if result>high(fwout) then
            break;                                 {Abbruch bei zu vielen Werten}
        end else
          s:=s+chr(buf[i]);                        {FW-Version zusammenbauen}
      end;
    end;
  finally
    Free;
  end;
end;

function TForm1.fModeFinden(l: TStringList): boolean; {FlightMode Spalte finden}
var i: integer;
begin
  result:=false;
  if l[19]='1' then begin                          {H920 Telemetry w/o header}
    StringGrid1.Tag:=17;
    SpinEdit3.Tag:=1;
    result:=true;
  end else begin
    for i:=0 to l.count-1 do begin
      if l[i]=fmode then begin
        StringGrid1.Tag:=i;                        {Position f_mode merken}
        result:=true;                              {validiert, f_mode gefunden}
        break;
      end;
      if l[i]=pfmode then begin
        StringGrid1.Tag:=i;                        {Position fMode merken}
        result:=true;                              {validiert, fMode gefunden}
        SpinEdit3.Tag:=YTHPid;                     {YTH Plus gefunden}
        break;
      end;
    end;
  end;
end;

function TForm1.GetFW(var fwout: TarrFW): integer; {Firmware auslesen}
var fn: string;
begin
  result:=0;                                       {Zeiger Feld im Ausgabearray}
  if ListBox1.Items.Count>0 then begin
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+npath+
        PathDelim+nfile+ListBox1.Items[ListBox1.ItemIndex]+sext; {Sensor}
    if FileSize(fn)>FWsz then
      result:=ReadFW(fn, fwout);
  end;
end;

procedure TForm1.SelDirSet;                        {FlightLog auswählen}
begin
  StatusBar1.Panels[5].Text:=capSelDir;
  SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
  SelectDirectoryDialog1.Title:=capSelDir;
  if SelectDirectoryDialog1.Execute then begin
    ComboBox2.Text:=SelectDirectoryDialog1.FileName;
    SelDirAct('');
  end;
end;

procedure TForm1.SelDirProt;                       {Protokollverzeichnis auswählen}
begin
  StatusBar1.Panels[5].Text:=capSelProt;
  SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
  SelectDirectoryDialog1.Title:=capSelProt;
  if SelectDirectoryDialog1.Execute then
    ComboBox8.Text:=SelectDirectoryDialog1.FileName;
end;

procedure TForm1.EnSave;                           {Speichern erlauben}
begin
  if ListBox1.Items.Count>0 then begin
    BitBtn2.Enabled:=true;
    MenuItem24.Enabled:=true;
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);    {Button beenden}
begin
  Close;
end;

procedure TForm1.BitBtn13Click(Sender: TObject);   {Take screenshot}
begin
  SaveDialog1.Title:=rsScreenshot;
  if PageControl1.ActivePageIndex>=0 then begin
    SaveDialog1.FileName:=Form2.CleanDN(rsScreenshot+PageControl1.ActivePage.Caption+pngdef);
    if (PageControl1.ActivePageIndex=6) and        {Settings}
       (PageControl2.ActivePageIndex>=0) then
      SaveDialog1.FileName:=Form2.CleanDN(rsScreenshot+PageControl2.ActivePage.Caption+pngdef);
  end else
    SaveDialog1.FileName:=Form2.CleanDN(rsScreenshot+capForm1+pngdef);
  if SaveDialog1.Execute then
    ScreenToBild(SaveDialog1.FileName);
end;

procedure TForm1.BitBtn14Click(Sender: TObject);   {Cut files for analysis}
var inlist, outlist: TStringList;
    fno: string;

  procedure CutBreeze;
  var x: integer;
      fn: string;                                  {file name}
      bg: TDateTime;
  begin
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
        ListBox1.Items[ListBox1.ItemIndex]+bext;   {Breeze Logdateien}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      for x:=0 to 8 do
        outlist.Add(inlist[x]);
      for x:=9 to inlist.count-1 do begin
        bg:=ZeitToDT(copy(inlist[x], 1, lzbr), brID);
        if (bg>=cutb) and (bg<=cute) then outlist.Add(inlist[x]);
      end;
      fno:=RandomFN(fn, brID, 0);
      outlist.SaveToFile(fno);
      SynEdit1.Lines.Add(capBitBtn14+suff+rsGridCell2+tab1+      {von}
                         StatusBar1.Panels[3].Text+tab1+rsGridCell3+tab1+
                         StatusBar1.Panels[4].Text);             {bis}
      StatusBar1.Panels[5].Text:=ExtractFileName(fno)+tab1+rsSaved;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  end;

  procedure CutYLegacy;
  var x: integer;
      fn: string;                                  {file name}
      z:  integer;
      bg: TDateTime;
  begin
    z:=random(8)+1;
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
        kfile+ListBox1.Items[ListBox1.ItemIndex]+fext;         {Telemetry}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      outlist.Add(inlist[0]);                      {take over header}
      for x:=1 to inlist.count-1 do begin
        bg:=ZeitToDT(copy(inlist[x], 1, lzyu), 0);
        if (bg>=cutb) and
           (bg<=cute) then
          outlist.Add(inlist[x]);
      end;
      fno:=RandomFN(fn, 0, z);
      outlist.SaveToFile(fno);
      outlist.Clear;
    end;
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+spath+
        PathDelim+sfile+ListBox1.Items[ListBox1.ItemIndex]+fext; {RemGPS}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      outlist.Add(inlist[0]);                      {take over header}
      for x:=1 to inlist.count-1 do begin
        bg:=ZeitToDT(copy(inlist[x], 1, lzyu), 1);
        if (bg>=cutb) and
           (bg<=cute) then
          outlist.Add(inlist[x]);
      end;
      fno:=RandomFN(fn, 1, z);
      outlist.SaveToFile(fno);
      outlist.Clear;
    end;
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+fpath+
        PathDelim+ffile+ListBox1.Items[ListBox1.ItemIndex]+fext; {Remote}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      outlist.Add(inlist[0]);                      {take over header}
      for x:=1 to inlist.count-1 do begin
        bg:=ZeitToDT(copy(inlist[x], 1, lzyu), 2);
        if (bg>=cutb) and
           (bg<=cute) then
          outlist.Add(inlist[x]);
      end;
      fno:=RandomFN(fn, 2, z);
      outlist.SaveToFile(fno);
    end;
    if fno<>'' then begin
      SynEdit1.Lines.Add(capBitBtn14+suff+rsGridCell2+tab1+      {von}
                         StatusBar1.Panels[3].Text+tab1+rsGridCell3+tab1+
                         StatusBar1.Panels[4].Text);             {bis}
      if Spinedit3.Tag=YTHPid then begin           {YTH Plus}
        StatusBar1.Panels[5].Text:=mndirp+wldcd+GetNr(ExtractFileName(fno))+tab1+rsSaved;
      end else begin
        StatusBar1.Panels[5].Text:=mndir+wldcd+GetNr(ExtractFileName(fno))+tab1+rsSaved;
      end;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      SelDirAct('');
    end;
  end;

begin
  if (cutb>0) and (cute>cutb) then begin           {nur zur Sicherheit}
    inlist:=TStringList.Create;
    outlist:=TStringList.Create;
    fno:='';
    try
      if SpinEdit3.Tag=brID then begin             {Breeze}
        CutBreeze;
      end else begin                               {legacy Yuneec}
        CutYLegacy;
      end;
    finally
      FreeAndNil(inlist);
      FreeAndNil(outlist);
    end;
  end;
end;

procedure TForm1.ScreenToBild(fn: string);         {Screenshot}
var bld: TPortableNetworkGraphic;
    ScreenDC: HDC;
begin
  bld:=TPortableNetworkGraphic.Create;             {create PNG-picture}
  try
    bld.Canvas.Clear; {sicherstellen, dass bld bereits vollständig erzeugt wurde}
    ScreenDC:=Form1.Canvas.Handle;                 {whole application}
    bld.LoadFromDevice(ScreenDC);
//    ReleaseDC(0, ScreenDC);             {if released, second Screenshot failed}
    bld.SaveToFile(fn);
  finally
    FreeAndNil(bld);
  end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);    {Button Konvertieren}
var x: integer;
    fn: string;
begin
  if ListBox1.Items.Count>0 then begin
    for x:=0 to ListBox1.Items.Count-1 do begin
      case SpinEdit3.Tag of
        brID: begin                                {Breeze}
                if RadioGroup2.ItemIndex=2 then    {GPX}
                  MacheGPX(IncludeTrailingPathDelimiter(ComboBox2.Text)+
                           ListBox1.Items[x]+bext, x)
                else                               {ansonsten KML/KMZ}
                  MacheKML(IncludeTrailingPathDelimiter(ComboBox2.Text)+
                           ListBox1.Items[x]+bext, x);
              end;
        MQid: begin
                fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
                    nfile+ListBox1.Items[x]+wext;  {Sensor_*.txt}
                if Fileexists(fn) then begin
                  ShowSensorPlus(fn, x, true, false, false);
                end else begin                     {alternativ yuneec_*.log file}
                  fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
                      mfile+ListBox1.Items[x]+bext;
                  if Fileexists(fn) then begin
                    ShowSensorPlus(fn, x, true, false, false);
                  end;
                end;
                StatusBar1.Panels[5].Text:=fn;
              end;
        H5id: begin
                fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
                    ListBox1.Items[x]+hext;        {*.tlog}
                if Fileexists(fn) then begin
                  ShowSensorPlus(fn, x, true, false, false);
                  StatusBar1.Panels[5].Text:=fn;
                end;
              end;

        else begin
                case RadioGroup2.ItemIndex of
                  0,1: MacheKML(IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
                                kfile+ListBox1.Items[x]+fext, x);
                  2: MacheGPX(IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
                                kfile+ListBox1.Items[x]+fext, x);
                  3: MacheDash(x);
                  4: MacheRR(x);
                  5: MacheCCC(IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
                                kfile+ListBox1.Items[x]+fext);
                end;
        end;
      end;                                         {case Vehicle Type}
    end;
    BitBtn2.Enabled:=false;
    MenuItem24.Enabled:=false;
  end;
end;

function TForm1.ComplFN(st: string; tp: TDateTime): string;  {Namensstamm und Zeit}
                                                   {Dateinamen mit Nummer ergänzen}
begin
  result:=st;
  case RadioGroup4.ItemIndex of
    0: result:=st+FormatDateTime(dzf+'_hhnnss', tp);  {Datum und Zeit}
    1: result:=st+FormatDateTime(dzf, tp);            {nur Datum}
    2: result:=st+IntToStr(DateTimeToUnix(tp));       {UNIX Zeitstempel}
    3: result:=st+IntToStr(random(89999)+10000);      {Random number}
  end;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);    {Archivieren durch Umbenennen}
var newdir: string;
    p: integer;
    splitlist: TStringList;
    rpath: string;
begin
  if ComboBox2.Text<>'' then begin                 {nicht leer}
    ComboBox2.Text:=ExcludeTrailingPathDelimiter(ComboBox2.Text);
    splitlist:=TStringList.Create;
    try                                            {letzten Pfadnamen finden}
      splitlist.Delimiter:=PathDelim;
      splitlist.StrictDelimiter:=True;
      splitlist.DelimitedText:=ComboBox2.Text;     {dazu Pfad splitten}
      newdir:=splitlist[splitlist.count-1];
    finally
      FreeAndNil(splitlist);
    end;
    rpath:=mndir;                                  {default: FlightLog}
    if SpinEdit3.Tag=YTHPid then rpath:=mndirp;    {nur für YTH Plus}
    if (tend>0) and                                {Endezeit}
       (pos(dkpath, ComboBox2.Text)<1) and         {nicht Telemetry}
       (newdir=rpath) then begin
      newdir:=ComplFN(ExtractFileDir(ComboBox2.Text)+PathDelim+rpath, tend);
      if RenameFile(IncludeTrailingPathDelimiter(ComboBox2.Text),
                    IncludeTrailingPathDelimiter(newdir)) then begin
        p:=ComboBox2.Items.IndexOf(ComboBox2.Text);
        if p>=0 then
          ComboBox2.Items[p]:=newdir;
        ComboBox2.Text:=newdir;
      end else begin
        StatusBar1.Panels[5].Text:=rsNotRename;
        SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=rsNoArch;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  end;
end;

procedure TForm1.cbThunderChange(Sender: TObject);
begin
  if cbThunder.Checked then begin
    cbThunder.Tag:=RadioGroup5.ItemIndex;          {save last setting}
    RadioGroup5.ItemIndex:=0;                      {Set to 'Absolute'}
  end else
    RadioGroup5.ItemIndex:=cbThunder.Tag;          {restore setting}
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
    for x:=p+length(w)-1 to length(s) do begin
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
  ff:=GetCGOStr('sharpness', Edit4.Text);          {Sharpness nur aus GET_SHARPNESS}
  StringGrid3.Cells[1, 7]:=ff;                     {in Tabelle eintragen}
  TrackBar3.Position:=StrToIntDef(ff, 6);          {Anzeige setzen}
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
          3..INDEX_PAGE mit Ausgabe an AppLog, nur beim Start Button. }

function TForm1.CGO3run(const CGO3cmd: string; const CGO3act: integer): integer;
Var s, ff: string;
    sdsize, w: integer;
begin
  Image1.Visible:=false;
  Image2.Visible:=false;
  Image3.Visible:=false;
  if not Timer1.Enabled then Screen.Cursor:=crHourGlass;
  with TFPHttpClient.Create(Nil) do try
    IOTimeout:=6000;                               {Timeout if cam disconnected}
    try
      SynEdit1.Lines.Add('');                      {Leerzeile im Protokoll}
      if CGO3cmd<>'' then begin                    {opt. Kommando ausführen}
        s:=Get(Edit5.Text+CGO3cgi+CGO3cmd);
        result:=StrToIntDef(GetCGOStr('rval', s), -1); {Returnwert überschreiben}
        SynEdit1.Lines.Add(CGO3cmd);               {Ins LogFile schreiben}
        SynEdit1.Lines.Add(s);                     {Ergebnis ins Logfile}
        Edit4.Text:=s;                             {Ergebnis unten anzeigen}
      end else begin                               {Initialisierung und Werte ausgeben}
        s:=Get(Edit5.Text+CGO3cgi+idxpage);
        if cgo3act=3 then begin                    {nur beim Button Start}
          SynEdit1.Lines.Add(rsCGOdone+tab1+Edit5.Text);
          SynEdit1.Lines.Add(idxpage);             {Ins LogFile schreiben}
          SynEdit1.Lines.Add(s);                   {Ergebnis ins Logfile}
        end;
        s:=StringReplace(s, '}', sep, [rfReplaceAll]);
        result:=StrToIntDef(GetCGOStr('rval', s), -1);        {Returnwert Init}
        if result=0 then begin                     {Werte abfragen und anzeigen}
          sdsize:=StrToIntDef(GetCGOStr('sdtotal', s), 0);    {sdtotal abfragen}
          indGnouMeter1.ValueMax:=sdsize/1048576;
          indGnouMeter1.Value:=(sdsize-StrToIntDef(GetCGOStr('sdfree', s), 0))/1048576;
          ComboBox3.Text:=GetCGOStr('video_mode', s);
          try
            w:=StrToInt(GetCGOStr('iq_type', s));
            ComboBox4.Text:=ComboBox4.Items[w];
          except
            ComboBox4.Text:='';
          end;
          try
            w:=StrToInt(GetCGOStr('white_balance', s));
            case w of
              0: ComboBox5.Text:=ComboBox5.Items[0];
              1: ComboBox5.Text:=ComboBox5.Items[5];
              3: ComboBox5.Text:=ComboBox5.Items[6]; {Sunset}
              4: ComboBox5.Text:=ComboBox5.Items[2]; {Sunny}
              5: ComboBox5.Text:=ComboBox5.Items[3];
              7: ComboBox5.Text:=ComboBox5.Items[4];
             99: ComboBox5.Text:=ComboBox5.Items[1]; {Lock}
            end;
            If ComboBox5.Text='Auto' then
              Label21.Caption:=rsWB+' (AWB)'
            else
              Label21.Caption:=rsWB;
          except
            ComboBox5.Text:='';
          end;
          StringGrid3.Cells[1, 0]:=GetCGOStr('fw_ver', s);
          StringGrid3.Cells[1, 1]:=GetCGOStr('speed_rate', s);
          StringGrid3.Cells[1, 2]:=GetCGOStr('status', s);
          StringGrid3.Cells[1, 3]:=GetCGOStr('record_time', s);
          StringGrid3.Cells[1, 4]:=GetCGOStr('awb_lock', s);;
          StringGrid3.Cells[1, 5]:=GetCGOStr('ae_enable', s);;
          ComboBox6.Text:=GetCGOStr('iso_value', s);
          ff:=GetCGOStr('shutter_time', s);
          StringGrid3.Cells[1, 6]:='1/'+ff;
          ComboBox7.Text:=ff;
          ff:=GetCGOStr('photo_format', s);
          Label20.Caption:=ff;
          w:=-1;
          if (ff='dng') or (ff='raw') then begin
            w:=0;
            TrackBar3.Enabled:=false;              {Sharpness for jpg}
          end;
          if ff='jpg' then begin
            w:=1;
            TrackBar3.Enabled:=true;               {Sharpness for jpg}
          end;
          if ff='dng+jpg' then begin               {nur für CGO3+}
            w:=2;
            TrackBar3.Enabled:=true;               {Sharpness for jpg}
          end;
          RadioGroup7.ItemIndex:=w;
          CheckBox5.Checked:=(GetCGOStr('ae_enable', s)='1');
          FloatSpinEdit1.Enabled:=CheckBox5.Checked;
          ComboBox6.Enabled:=not CheckBox5.Checked;
          ComboBox7.Enabled:=ComboBox6.Enabled;
          try
            FloatSpinEdit1.Value:=StrToFloat(GetCGOStr('exposure_value', s));
          except
            FloatSpinEdit1.Value:=0;
          end;
          if GetCGOStr('cam_mode', s)='2' then begin
            Image2.Visible:=false;                 {Filmbildchen}
            Image3.Visible:=true;                  {Kamerabildchen}
          end else begin
            Image2.Visible:=true;
            Image3.Visible:=false;
          end;
          BitBtn21.Tag:=0;
          if GetCGOStr('audio_sw', s)='0' then begin
            BitBtn21.Tag:=1;
            Image1.Visible:=true;
          end else
            Image1.Visible:=false;
          if pos('"record"', s)>0 then begin
            AdvLed1.State:=lsOn;                   {record läuft}
            AdvLed1.Blink:=true;
            Timer1.Enabled:=true;     {Wartezeit für Stoppen der Aufnahme}
          end else begin
            AdvLed1.State:=lsOff;
            AdvLed1.Blink:=false;
            if Timer1.Tag=1 then Timer1.Enabled:=false;
          end;
          BitBtn16.Enabled:=true; {Kommandos nur nach erfolgreicher Intialisierung}
          BitBtn17.Enabled:=true;
          BitBtn18.Enabled:=true;
          BitBtn19.Enabled:=true;
          BitBtn20.Enabled:=true;
          BitBtn21.Enabled:=true;
          BitBtn22.Enabled:=true;
          BitBtn23.Enabled:=true;
          BitBtn24.Enabled:=true;
          RadioGroup6.Enabled:=true;
          RadioGroup7.Enabled:=true;
          SpeedButton6.Enabled:=true;
          StopLightSensor1.State:=slGREEN;         {WLAN Ampel auf Grün}
          StatusBar1.Panels[5].Text:=rsCGOdone+tab1+Edit5.Text;
        end else begin                             {Result <> 0}
          indGnouMeter1.ValueMax:=64;
          indGnouMeter1.Value:=0;
          StopLightSensor1.State:=slRED;
          SynEdit1.Lines.Add(s);                   {Fehlerausgabe ins Logfile}
        end;
      end; {Ende Initalisierung}
      case CGO3act of
        1: if CheckBox4.Checked then
             OpenURL(Edit5.Text+CGO3dir);          {Datenverzeichnis im Browser öffnen}
        2: If CheckBox2.Checked then               {Livestream anzeigen}
             OpenURL(StringReplace(Edit5.Text, 'http', 'rtsp',[rfIgnoreCase])+'live');
      end;
    except
      StatusBar1.Panels[5].Text:=rsTimeOut;
      SynEdit1.Lines.Add('''4363'+suff+StatusBar1.Panels[5].Text);
      result:=-1;                                  {Fehler, Timeout}
      StopLightSensor1.State:=slRED;
    end;
  finally
    Screen.Cursor:=crDefault;
    Free;
  end;
end;

procedure TForm1.BitBtn15Click(Sender: TObject);   {Status}
begin
  StatusBar1.Panels[5].Text:=rsCGOwait;
  SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
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

procedure TForm1.BitBtn16Click(Sender: TObject);   {Record start}
begin
  CGO3run('START_RECORD', 2);                      {CGO3act=2 RTSP abfragen}
  Timer1.Enabled:=true;   {laufende Statusabfrage, um Zeit zu aktualisieren}
  Timer1.Tag:=0;
end;

procedure TForm1.BitBtn17Click(Sender: TObject);   {Record stop}
begin
  CGO3run('STOP_RECORD', 0);
  Timer1.Tag:=1;                                   {Stop vorbereiten}
end;

procedure TForm1.BitBtn18Click(Sender: TObject);   {Reset default}
begin
  CGO3run('RESET_DEFAULT', 0);
  CGO3run('', 0);
end;

procedure TForm1.BitBtn19Click(Sender: TObject);   {Set speed}
begin
  CGO3run('SET_WIFI_SPEED&speed_rate=9', 1);       {CGO3act=2 Dateianzeige abfragen}
  CGO3run('', 0);                                  {INDEX_PAGE aufrufen}
end;

procedure TForm1.BitBtn20Click(Sender: TObject);
begin
  CGO3run('SET_WIFI_SPEED&speed_rate=1', 0);       {reset Speed}
  CGO3run('', 0);
end;

procedure TForm1.ComboBox3Change(Sender: TObject); {Videoformat setzen}
begin
  if BitBtn23.Enabled then begin
    CGO3run('SET_VIDEO_MODE&mode='+ComboBox3.Text, 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.ComboBox4Change(Sender: TObject); {IQ Type}
begin
  if BitBtn23.Enabled then begin
    CGO3run('SET_IQ_TYPE&mode='+IntToStr(ComboBox4.ItemIndex), 0);
    CGO3run('', 0);                                {INDEX_PAGE abfragen}
  end;
end;

procedure TForm1.TrackBar3Click(Sender: TObject);
begin
  if TrackBar3.Enabled then begin
    CGO3run('SET_SHARPNESS&value='+IntToStr(TrackBar3.Position), 0);
    Sharpness;             {SHARPNESS abfragen, da nicht mehr in INDEX_PAGE}
  end;
end;

procedure TForm1.ComboBox5Change(Sender: TObject); {WB}
var s: String;
begin
  if BitBtn23.Enabled then begin
    s:='0';
    case ComboBox5.ItemIndex of
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

procedure TForm1.ComboBox6Change(Sender: TObject); {ISO}
begin
  if BitBtn23.Enabled then begin
    CGO3run('SET_SH_TM_ISO&time='+ComboBox7.Text+'&value='+ComboBox6.Text, 0);
    Sleep(500);
    CGO3run('', 0);
  end;
end;

procedure TForm1.ComboBox7Change(Sender: TObject); {Shutter}
begin
  if BitBtn23.Enabled then begin
    CGO3run('SET_SH_TM_ISO&time='+ComboBox7.Text+'&value='+ComboBox6.Text, 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.ComboBox8DblClick(Sender: TObject);
begin
  OpenDocument(IncludeTrailingPathDelimiter(ComboBox8.Text));
end;

procedure TForm1.ComboBox8MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Verzeichnisliste löschen}
begin
  if ssCtrl in Shift then
    ComboBox8.Items.Clear;
end;

procedure TForm1.ComboBox9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Suchliste löschen}
begin
  if ssCtrl in Shift then
    ComboBox9.Items.Clear;
end;

procedure TForm1.Edit3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
                                                   {Kommando absenden bei Enter}
begin
  if SpeedButton6.Enabled and
     (key=VK_RETURN) then SendCGOcmd;              {Command zu CGO3}
  if key=VK_ESCAPE then Edit3.Text:='';            {Kommando löschen}
end;

procedure TForm1.BitBtn21Click(Sender: TObject);   {Audio Switch}
begin
  if BitBtn21.Tag=1 then
    CGO3run('SET_AUDIO_SW&mode=1', 0)              {reset Audio}
  else
    CGO3run('SET_AUDIO_SW&mode=0', 0);             {set Audio}
  CGO3run('', 0);
end;

procedure TForm1.BitBtn22Click(Sender: TObject);
begin
  CGO3run('TAKE_PHOTO', 0);                        {Photo shot}
end;

procedure TForm1.BitBtn23Click(Sender: TObject);   {Kamera Zeit setzen}
begin
  CGO3run('SET_TIME&time='+FormatDateTime(dzf+'_'+zzf, now), 0);
end;

procedure TForm1.BitBtn24Click(Sender: TObject);   {SD-Karte formatieren}
begin
  CGO3run('FORMAT_CARD', 0);
  Sleep(2000);
  CGO3run('', 0);
end;

{Der Typhoon H Plus sendet viele unnötige Datensätze mit unsinnige Werten, welche
 die Auswertung beeinträchtigen. Diese Werte können ausgeblendet werden.}

function TForm1.CheckVT(vt, fm: string): boolean;  {Vehicle Type prüfen für YTH Plus}
var fmode: integer;
begin
  result:=true;                                    {default: Alles geht durch}
  fmode:=StrToIntDef(fm, 0);
  if (CheckBox9.Checked) and                       {nur wenn erlaubt}
     (SpinEdit3.Tag=YTHPid) then                   {nur bei YTH Plus}
    if (StrToIntDef(trim(vt), defVT)<>defVT) or    {alle unsinnigen Vehicle Types}
       (vt='') or                                  {Zeilen mit $00}
       (fmode=0) or                                {unsinnige FlightModes}
       (fmode>19) then                             {fMode 20 ist wahrscheinlich ein Fehler}
      result:=false;                               {fehlerhafte Flight Modes}
end;

procedure TForm1.ProtoWerte(fn: string;            {Dateiname}
                            olist: TStringList;    {Datensatz per Flug/Datei}
                        var fln: integer;          {Flugnummer}
                        var gflt: TDateTime;       {Flugzeit}
                        var gstr: double);         {Flugstrecke}
var x, n, g, fmod: integer;
    inlist, splitlist: TStringList;
    bg, bg1, ed, flt: TDateTime;
    hmax, h, tas, tas1, u, tasmax, hmaxg, tasmaxg, umin: double;
    slat: string;
    dist, ddist, lat1, lat2, lat3, lon1, lon2, lon3, emax, strecke: double;
    vld, nflg, simu, gpsu: boolean;
    lfmode, fxmode, modestr: string;               {letzter Flightmode}

const bgid=999999;
      minlines=10;

begin
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  n:=0;
  g:=0;
  hmax:=0;
  tasmax:=0;
  tasmaxg:=0;
  hmaxg:=0;
  umin:=999;
  bg:=bgid;
  bg1:=bgid;
  slat:='';
  emax:=0;
  strecke:=0;
  vld:=false;                                      {Auswertung gültig}
  nflg:=true;
  simu:=false;
  gpsu:=false;                                     {GPS off erkennen}
  flt:=0;                                          {reale Flugzeit}
  ed:=0;
  modestr:='';
  lfmode:='';
  tas1:=0;                                         {Vergleichsgeschwindigkeit vorher}
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBar1.Position:=ProgressBar1.Position+1;
    Application.ProcessMessages;
    if inlist.count>minlines then begin            {Überschrift und mind. 10 Zeilen}
      try
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        splitlist.DelimitedText:=inlist[0]; {Überschrift einlesen, f_mode ermitteln}
        vld:=fModeFinden(splitlist);        {setzt StringGrid1.Tag mit Spaltenindex}
        if SpinEdit3.Tag<>YTHPid then begin
          splitlist.DelimitedText:=inlist[2];      {2. Datenzeile, v_type ermitteln}
          SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
          OverWriteVT;                             {Overwrite for PX4 Thunderbird}
        end;
        for x:=1 to inlist.Count-1 do
        if CheckE7(inlist[x]) then begin           {Plausicheck für YTH Plus}
          splitlist.DelimitedText:=inlist[x];
          if (splitlist.Count>anzsp) and           {Plausicheck YTH und generell gegen Leerzeilen}
              CheckVT(splitlist[StringGrid1.Tag+2],
                      splitlist[StringGrid1.Tag]) then begin
            h:=StrToFloatN(splitlist[4]);          {Altitude}
            if testh(h) then begin                 {nicht bei unsinnigen Höhenwerten}
              tas:=StrToFloatN(splitlist[7]);      {True Air Speed in m/s}
              u:=StrToFloatN(splitlist[2]);        {LiPo Spannung}
              if (u<umin) and (u>1) then
                umin:=u;
              if NichtLeer(splitlist[3]) and       {Simulatorflug}
                 (splitlist[15]='231') then
                simu:=true;
              fxmode:=splitlist[StringGrid1.Tag];  {FlightMode}
              if GetRFM(fxmode, SpinEdit3.Tag,
                        InFlight(h, tas1, tas)) then begin
                if not nflg then begin
                  if bg1<bgid then
                    flt:=flt+ed-bg1;
                  bg1:=bgid;
                end;
                inc(n);
                nflg:=true;
                if fxmode<>lfmode then begin       {Flight Modes auflisten}
                  if modestr='' then
                    modestr:=fmodeToStr(StrToIntDef(fxmode, 16))
                  else
                    modestr:=modestr+'->'+fmodeToStr(StrToIntDef(fxmode, 16));
                  lfmode:=fxmode;
                end;
                ed:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
                if bg>ed then
                  bg:=ed;                          {Beginnzeit ohne GPS}
                if bg1>ed then
                  bg1:=ed;                         {Teil-Beginnzeit ohne GPS}
                if h>hmax then
                  hmax:=h;
                if tas>tasmax then
                  tasmax:=tas;
                if NichtLeer(splitlist[5]) and NichtLeer(splitlist[6]) then begin
                  inc(g);
                  if lowercase(splitlist[8])=idtrue then gpsu:=true;
                  if slat='' then begin
                    slat:=splitlist[5];            {Homepoint speichern}
                    lat1:=StrToFloatN(slat);
                    lon1:=StrToFloatN(splitlist[6]);
                    lat3:=lat1;
                    lon3:=lon1;
                  end;
                  if h>hmaxg then
                    hmaxg:=h;
                  if tas>tasmaxg then tasmaxg:=tas;
                  if slat<>'' then begin           {Startpunkt mit GPS}
                    lat2:=StrToFloatN(splitlist[5]);
                    lon2:=StrToFloatN(splitlist[6]);
                    dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                    ddist:=DeltaKoord(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                    if dist>emax then emax:=dist;
                    strecke:=strecke+ddist;        {Strecke aufaddieren}
                    lat3:=lat2;                    {letzten Punkt speichern}
                    lon3:=lon2;
                  end;
                end;                               {Ende mit GPS Daten}
              end else
                nflg:=false;                       {Ende realer Flug}
              tas1:=tas;                           {Vergleichsgeschwindigkeit}
            end;
          end else begin
            StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS; {Ende Konsistenz checken}
            SynEdit1.Lines.Add(''''+capLabel6+Format('%6d', [x])+       {Datenpunkt ausgeben}
                               suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Einlesen}
        flt:=flt+ed-bg1;
        splitlist.DelimitedText:=inlist[inlist.count-1];
        tend:=ZeitToDT(splitlist[0], SpinEdit3.Tag); {letzten Zeitstempel merken}
      except
        StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS;
        SynEdit1.Lines.Add('''4690'+suff+StatusBar1.Panels[5].Text);
      end;
    end;

    if vld and
       ((SpinEdit3.Tag<>YTHPid) or                 {Mindestflugzeit nur beim YTH Plus}
        (CheckBox9.Checked=false) or               {wenn Bereinigung eingestellt ist}
        (flt>minflt)) then begin                   {Anzeige gültiger Auswertung}
      splitlist.DelimitedText:=inlist[1];          {1. Datenzeile für fehlenden Beginn/Typ}
      fmod:=StrToIntDef(splitlist[StringGrid1.Tag+2],2);
      if g>3 then begin                            {nur wenn Daten vorhanden sind}
        ProgressBar1.Update;
        olist.Add(rsFlightNr+dstr+IntToStr(fln)+dstr);
        StringGrid5.RowCount:=fln+1;
        StringGrid5.Cells[0, fln]:=IntToStr(fln);
        StringGrid5.Cells[1, fln]:=capNachweis+suff+
                                   FormatDateTime(vzf, bg)+bind+
                                   FormatDateTime(zzf, ed);
        olist.Add(rsVType+dstr+vtypeToStr(fmod)+dstr);
        if not gpsu then olist.Add(dstr+rsNoGPS+dstr);     {GPS off by Pilot}
        olist.Add(rsGridCell1+dstr+FormatDateTime(dzf, bg)+dstr);
        olist.Add(rsGridCell2+dstr+FormatDateTime(zzf, bg)+dstr);
        olist.Add(rsGridCell3+dstr+FormatDateTime(zzf, ed)+dstr);
        olist.Add(rsDauer+dstr+FormatDateTime(zzf, flt)+dstr);   {Flugzeit}
        olist.Add(rsStartpkt+dstr+                 {Startposition in GoogleMaps}
                       URLGMap(KoToStr(lat1), KoToStr(lon1))+dstr);
        if modestr<>'' then olist.Add(rsMode+dstr+modestr+dstr);
        if simu then begin                         {Simulatorzeit}
          if CheckBox6.Checked then
            gflt:=gflt+flt;
        end else
          gflt:=gflt+flt;                          {Gesamtzeit aufaddieren}
        gstr:=gstr+(strecke/1000);                 {Flugstrecke in km}
        fln:=fln+1;                                {nächste Flugnummer}
        flt:=flt*24;                               {Dauer in Stunden}
        if RadioGroup3.ItemIndex=2 then begin
          olist.Add(rsGridCell5+dstr+FloatToStrF(hmaxg/fft, ffFixed, 5, 1)+'ft'+dstr);
          olist.Add(rsGridCell6+dstr+FloatToStrF(emax/fft, ffFixed, 5, 1)+'ft'+dstr);
          olist.Add(rsGridCell7+dstr+FloatToStrF(strecke/fft, ffFixed, 5, 1)+'ft'+dstr);
          olist.Add(rsGridCell8+dstr+FloatToStrF(tasmaxg*fmph, ffFixed, 5, 1)+'mph'+dstr);
          if flt>0 then
            olist.Add(rsAvgSpeed+dstr+
            FloatToStrF(strecke/flt/fmile/1000, ffFixed, 5, 1)+'mph'+dstr);
        end else begin
          olist.Add(rsGridCell5+dstr+FloatToStrF(hmaxg, ffFixed, 5, 1)+'m'+dstr);
          olist.Add(rsGridCell6+dstr+FloatToStrF(emax, ffFixed, 5, 1)+'m'+dstr);
          olist.Add(rsGridCell7+dstr+FloatToStrF(strecke, ffFixed, 5, 1)+'m'+dstr);
          olist.Add(rsGridCell8+dstr+FloatToStrF(tasmaxg*fkmh, ffFixed, 5, 1)+'km/h'+dstr);
          if flt>0 then
            olist.Add(rsAvgSpeed+dstr+
            FloatToStrF(strecke/flt/1000, ffFixed, 5, 1)+'km/h'+dstr);
        end;
        olist.Add(rsRest+dstr+FloatToStrF(umin, ffFixed, 5, 1)+'V = ~'+
        IntToStr(round(VtoProz(SpinEdit3.Tag, umin)))+'%'+dstr);
      end else begin                               {reduzierte Ausgabe}
        if n>3 then begin                          {Ausgabe für Flüge ohne GPS}
          olist.Add(rsFlightNr+dstr+IntToStr(fln)+dstr);
          StringGrid5.RowCount:=fln+1;
          StringGrid5.Cells[0, fln]:=IntToStr(fln);
          StringGrid5.Cells[1, fln]:=capNachweis+suff+
                                     FormatDateTime(vzf, bg)+bind+
                                     FormatDateTime(zzf, ed);
          olist.Add(rsVType+dstr+vtypeToStr(fmod)+dstr);
          if simu then
            olist.Add(dstr+rsSimulator+dstr)
          else
            oList.Add(dstr+rsNoGPS+dstr);
          olist.Add(rsGridCell1+dstr+FormatDateTime(dzf, bg)+dstr);
          olist.Add(rsGridCell2+dstr+FormatDateTime(zzf, bg)+dstr);
          olist.Add(rsGridCell3+dstr+FormatDateTime(zzf, ed)+dstr);
          olist.Add(rsDauer+dstr+FormatDateTime(zzf, flt)+dstr);
          if modestr<>'' then olist.Add(rsMode+dstr+modestr+dstr);
          fln:=fln+1;
          gflt:=gflt+flt;
          if RadioGroup3.ItemIndex=2 then begin
            olist.Add(rsGridCell5+dstr+FloatToStrF(hmax/fft, ffFixed, 5, 1)+'ft'+dstr);
            olist.Add(rsGridCell8+dstr+FloatToStrF(tasmax*fmph, ffFixed, 5, 1)+'mph'+dstr)
          end else begin
            olist.Add(rsGridCell5+dstr+FloatToStrF(hmax, ffFixed, 5, 1)+'m'+dstr);
            olist.Add(rsGridCell8+dstr+FloatToStrF(tasmax*fkmh, ffFixed, 5, 1)+'km/h'+dstr);
          end;
          olist.Add(rsRest+dstr+FloatToStrF(umin, ffFixed, 5, 1)+'V = ~'+
          IntToStr(round(VtoProz(SpinEdit3.Tag, umin)))+'%'+dstr);
        end;
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.BrProtoWerte(fn: string;          {Dateiname}
                            olist: TstringList;    {Datensatz per Flug/Datei}
                        var fln: integer;          {Flugnummer}
                        var gflt: TDateTime;       {Flugzeit}
                        var gstr: double);         {Flugstrecke}
var x, n, g: integer;
    inlist, splitlist: TStringList;
    bg, bg1, ed, flt: TDateTime;
    hmax, h, hmaxg, u, umin: double;
    slat, vehid: string;
    dist, ddist, lat1, lat2, lat3, lon1, lon2, lon3, emax, strecke: double;
    vld, gpsu,nflg: boolean;
    fxmode, lfmode, modestr: string;               {letzter Flightmode}

const bgid=999999;
      minlines=10;

begin
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  n:=0;
  g:=0;
  hmax:=0;
  hmaxg:=0;
  umin:=999;
  bg:=bgid;
  bg1:=bgid;
  slat:='';
  emax:=0;
  strecke:=0;
  vld:=false;                                      {Auswertung gültig}
  gpsu:=false;                                     {GPS off erkennen}
  nflg:=true;
  flt:=0;                                          {reale Flugzeit}
  ed:=0;
  vehid:=vtypeToStr(brID);                         {Breeze vorbelegen}
  modestr:='';
  lfmode:='';
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBar1.Position:=ProgressBar1.Position+1;
    Application.ProcessMessages;
    if inlist.count>minlines+5 then begin          {Überschrift und mind. 15 Zeilen}
      try
        vld:=(pos(brsnid, inlist[5])>0) and (pos(brfmode, inlist[8])>0);
        if vld then vehid:=StringReplace(inlist[5], brsnid,
                                         vtypeToStr(brID),[rfIgnoreCase]);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        for x:=9 to inlist.count-1 do begin        {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin      {Konsistenz checken (Breeze)}
            h:=StrToFloatN(splitlist[10])/100;     {Altitude}
            u:=StrToFloatN(splitlist[21]);         {LiPo in % / 2,55}
            if (u<umin) then umin:=u;              {Minimum merken}
            if testh(h) then begin
              if (splitlist[14]<>'0') then begin
                if not nflg then begin
                  if bg1<bgid then flt:=flt+ed-bg1;
                  bg1:=bgid;                       {bei Lücke bg rücksetzen}
                end;
                inc(n);
                nflg:=true;
                fxmode:=splitlist[2];              {FlightMode}
                if fxmode<>lfmode then begin       {Flight modes auflisten}
                  if modestr='' then
                    modestr:=BrFmodeToStr(StrToIntDef(fxmode, 0))
                  else
                    modestr:=modestr+'->'+BrFmodeToStr(StrToIntDef(fxmode, 0));
                  lfmode:=fxmode;
                end;
                ed:=ZeitToDT(splitlist[0], brID);
                if bg>ed then bg:=ed;              {Beginnzeit ohne GPS}
                if bg1>ed then bg1:=ed;            {ev. neue Beginnzeit}
                if h>hmax then hmax:=h;
                if NichtLeer(splitlist[12]) and NichtLeer(splitlist[13]) then begin
                  inc(g);
                  gpsu:=BrGPSfix(splitlist[20]);
  //              if splitlist[5]='1' then gpsu:=true;    {loseGPSact ??}
                  if slat='' then begin
                    slat:=splitlist[12];           {Homepoint speichern}
                    lat1:=BrCoordToFloat(slat);
                    lon1:=BrCoordToFloat(splitlist[13]);
                    lat3:=lat1;
                    lon3:=lon1;
                  end;
                  if h>hmaxg then hmaxg:=h;
                  if slat<>'' then begin           {Startpunkt mit GPS}
                    lat2:=BrCoordToFloat(splitlist[12]);
                    lon2:=BrCoordToFloat(splitlist[13]);
                    dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                    ddist:=DeltaKoord(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                    if dist>emax then emax:=dist;
                    strecke:=strecke+ddist;        {Strecke aufaddieren}
                    lat3:=lat2;                    {letzten Punkt speichern}
                    lon3:=lon2;
                  end;
                end;                               {Ende mit GPS Daten}
              end else
                nflg:=false;
            end;
          end else begin
            StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS; {Ende Konsistenz checken}
            SynEdit1.Lines.Add('''4891'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Einlesen}
        flt:=flt+ed-bg1;
        splitlist.DelimitedText:=inlist[inlist.count-1];
        tend:=ZeitToDT(splitlist[0], brID); {letzten Zeitstempel merken}
      except
        StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS;
        SynEdit1.Lines.Add('''4899'+suff+StatusBar1.Panels[5].Text);
      end;
    end;

    if vld then begin                              {Anzeige gültiger Auswertung}
      splitlist.DelimitedText:=inlist[1];          {1. Datenzeile für fehlenden Beginn/Typ}
      if g>3 then begin                            {nur wenn Daten vorhanden sind}
        ProgressBar1.Update;
        olist.Add(rsFlightNr+dstr+IntToStr(fln)+dstr);
        StringGrid5.RowCount:=fln+1;
        StringGrid5.Cells[0, fln]:=IntToStr(fln);
        StringGrid5.Cells[1, fln]:=capNachweis+suff+
                                   FormatDateTime(vzf, bg)+bind+
                                   FormatDateTime(zzf, ed);
        olist.Add(rsVType+dstr+vehid+dstr);
        if not gpsu then olist.Add(dstr+rsNoGPS+dstr); {GPS off by Pilot}
        olist.Add(rsGridCell1+dstr+FormatDateTime(dzf, bg)+dstr);
        olist.Add(rsGridCell2+dstr+FormatDateTime(zzf, bg)+dstr);
        olist.Add(rsGridCell3+dstr+FormatDateTime(zzf, ed)+dstr);
        olist.Add(rsDauer+dstr+FormatDateTime(zzf, flt)+dstr);   {Flugzeit}
        olist.Add(rsStartpkt+dstr+                 {Startposition in GoogleMaps}
                                   URLGMap(KoToStr(lat1), KoToStr(lon1))+dstr);
        if modestr<>'' then olist.Add(rsMode+dstr+modestr+dstr);
        gflt:=gflt+flt;                            {Gesamtzeit aufaddieren}
        gstr:=gstr+(strecke/1000);                 {Flugstrecke in km}
        flt:=flt*24;                               {Dauer in Stunden}
        fln:=fln+1;
        if RadioGroup3.ItemIndex=2 then begin
          olist.Add(rsGridCell5+dstr+FloatToStrF(hmaxg/fft, ffFixed, 5, 1)+'ft'+dstr);
          olist.Add(rsGridCell6+dstr+FloatToStrF(emax/fft, ffFixed, 5, 1)+'ft'+dstr);
          olist.Add(rsGridCell7+dstr+FloatToStrF(strecke/fft, ffFixed, 5, 1)+'ft'+dstr);
          if flt>0 then
            olist.Add(rsAvgSpeed+dstr+
            FloatToStrF(strecke/flt/fmile/1000, ffFixed, 5, 1)+'mph'+dstr);
        end else begin
          olist.Add(rsGridCell5+dstr+FloatToStrF(hmaxg, ffFixed, 5, 1)+'m'+dstr);
          olist.Add(rsGridCell6+dstr+FloatToStrF(emax, ffFixed, 5, 1)+'m'+dstr);
          olist.Add(rsGridCell7+dstr+FloatToStrF(strecke, ffFixed, 5, 1)+'m'+dstr);
          if flt>0 then
            olist.Add(rsAvgSpeed+dstr+
            FloatToStrF(strecke/flt/1000, ffFixed, 5, 1)+'km/h'+dstr);
        end;
//        olist.Add(rsRest+dstr+'~'+IntToStr(round(BrUmrech(umin)))+'%'+dstr);
      end else begin                               {reduzierte Ausgabe}
        if n>3 then begin                          {Ausgabe für Flüge ohne GPS}
          olist.Add(rsFlightNr+dstr+IntToStr(fln)+dstr);
          StringGrid5.RowCount:=fln+1;
          StringGrid5.Cells[0, fln]:=IntToStr(fln);
          StringGrid5.Cells[1, fln]:=capNachweis+suff+
                                     FormatDateTime(vzf, bg)+bind+
                                     FormatDateTime(zzf, ed);
          olist.Add(rsVType+dstr+vehid+dstr);
          oList.Add(dstr+rsNoGPS+dstr);
          olist.Add(rsGridCell1+dstr+FormatDateTime(dzf, bg)+dstr);
          olist.Add(rsGridCell2+dstr+FormatDateTime(zzf, bg)+dstr);
          olist.Add(rsGridCell3+dstr+FormatDateTime(zzf, ed)+dstr);
          olist.Add(rsDauer+dstr+FormatDateTime(zzf, flt)+dstr);
          if modestr<>'' then olist.Add(rsMode+dstr+modestr+dstr);
          fln:=fln+1;
          gflt:=gflt+flt;
          if RadioGroup3.ItemIndex=2 then begin
            olist.Add(rsGridCell5+dstr+FloatToStrF(hmax/fft, ffFixed, 5, 1)+'ft'+dstr);
          end else begin
            olist.Add(rsGridCell5+dstr+FloatToStrF(hmax, ffFixed, 5, 1)+'m'+dstr);
          end;
//          olist.Add(rsRest+dstr+'~'+IntToStr(round(BrUmrech(umin)))+'%'+dstr);
        end;
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.BitBtn26Click(Sender: TObject);   {Probleme suchen}
var vlist, flist, inlist, splitlist: TStringList;
    i, zhl, vt: integer;
    vstr: string;

  function GetFMPos: boolean;                      {Position f_mode to StringGrid1.Tag}
  begin                                            {Gibt bei Fehler true zurück}
    try
      splitlist.DelimitedText:=inlist[0];
      fModeFinden(splitlist);                      {hier wird YTH Plus schon erkannt}
      splitlist.DelimitedText:=inlist[3];          {Vehicle type bestimmen}
      vt:=StrToInt(splitlist[StringGrid1.Tag+2]);  {eventuell Abbruch bei Fehler}
      if SpinEdit3.Tag<>YTHPid then                {erkannten H Plus nicht überschreiben}
        SpinEdit3.Tag:=vt;
  {zusätzlicher Filter nach Vehicle Type. Vehicle type 5 ist H oder H Plus}
      result:=CheckBox1.Checked and
              (vt<>RadioGroup10.ItemIndex+1);
    except
      result:=true;                                {Abbruch bei Fehler}
    end;
  end;

  function Emergency12(const fn: string): boolean; {Emergency suchen}
  var k, num: integer;
  begin
    num:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      if GetFMPos then exit;
      case SpinEdit3.Tag of
        3: vstr:='8';                              {8 nur bei 350QX}
//        YTHPid: vstr:='??';                      {unbekannt}
        MQid: vstr:=emcyID;
        H5id: vstr:=emcyID;
      else
        vstr:='12';                                {Yuneec legacy}
      end;
      ComboBox9.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=StringGrid1.Tag then exit; {invalid data sets}
        if trim(splitlist[StringGrid1.Tag])=vstr then inc(num);
        if num>2 then begin
          result:=true;
          break;
        end;
      end;
    end;
  end;

  function CCWnum(const fn: string): boolean;      {CCW Blöcke, Anzahl Flags > 10}
  var k, num, flag: integer;
  begin
    num:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      if GetFMPos then exit;
      vstr:='32';                                  {Compass Cali Warning}
      ComboBox9.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=(StringGrid1.Tag+3) then exit; {zerstörte Datensätze}
        flag:=StrToIntDef(trim(splitlist[StringGrid1.Tag+3]), 0);
        if (flag and 32)>0 then begin              {CCW flag}
          inc(num);
          if num>20 then begin                     {Threshold erreicht}
            result:=true;
            break;
          end;
        end else
          num:=0;                                  {CCW Zähler rücksetzen}
      end;
    end;
  end;

  function CCWtime(const fn: string): boolean;     {CCW Blöcke > 5 sec}
  var k, flag: integer;
      bg, ed: TDateTime;
  begin
    bg:=0;
    ed:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      if GetFMPos then
        exit;
      vstr:='32';                                  {Compass Cali Warning}
      ComboBox9.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=(StringGrid1.Tag+3) then exit; {zerstörte Datensätze}
        flag:=StrToIntDef(trim(splitlist[StringGrid1.Tag+3]), 0);
        if (flag and 32)>0 then begin              {CCW flag}
          ed:=ZeitToDT(splitlist[0], 2);           {Zeitpunkt feststellen}
          if bg=0 then
            bg:=ed;                                {neuen Beginn setzen}
          if (ed-bg)>=tsdelta3 then begin          {Threshold 5s erreicht}
            result:=true;
            break;
          end;
        end else
          bg:=0;                                   {Beginnzeit rückgesetzt}
      end;
    end;
  end;

  function StickCali(const fn: string): boolean;   {Sticks Abweichung Neutral}
  var k, m: integer;
      num: array[1..4] of integer;                 {Anzahl neutral für 4 Sticks}
      w: double;
  begin
    FillChar(num, SizeOf(num), 0);                 {Array löschen}
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      ComboBox9.Text:=IntToStr(stkntrl);           {Suche vordefinieren 2048}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count>high(num) then begin    {keine zerstörte Datensätze}
          for m:=Low(num) to high(num) do begin    {4 Sticks prüfen}
            w:=StrToFloatN(splitlist[m]);
            if (w<stkntrl+2) and (w>stkntrl-2) then inc(num[m]);  {2048 +/-1}
          end;
        end;
      end;
      k:=inlist.Count div 20;                      {5% Neutral ?}
      for m:=Low(num) to high(num) do if num[m]<k then begin
        result:=true;                              {Neutral weniger als 5%}
        break;
      end;
    end;
  end;

  function VoltW2(const fn: string): boolean;     {Voltage warning 2}
  var k, num, flag: integer;
  begin
    num:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      if GetFMPos then
        exit;
      vstr:='2';
      ComboBox9.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=(StringGrid1.Tag+3) then exit; {zerstörte Datensätze}
        flag:=StrToIntDef(trim(splitlist[StringGrid1.Tag+3]), 0);
        if (flag and 2)>0 then begin               {Voltage warn 2 flag}
          inc(num);
          if num>5 then begin                      {Threshold erreicht}
            result:=true;
            break;
          end;
        end else
          num:=0;                                  {Zähler rücksetzen}
      end;
    end;
  end;

  function VoltMin(const fn: string): boolean;     {Voltage below Find}
  var k: integer;
      w, v: double;
  begin
    w:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      if GetFMPos then
        exit;
      try
        w:=StrToFloatN(ComboBox9.Text);            {Suchfeld}
      except
        w:=9.9;                                    {3S minimum V}
        ComboBox9.Text:=FloatToStr(w);
      end;
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if (splitlist.Count>StringGrid1.Tag+2) and
            CheckVT(splitlist[StringGrid1.Tag+2],  {zerstörte Datensätze ausblenden}
                    splitlist[StringGrid1.Tag]) then begin
          try
            v:=StrToFloatN(trim(splitlist[2]));
          except
            v:=30;                                 {mehr als 6S bei zerstörten Daten}
          end;
          if v<w then begin                        {Voltage too low}
            result:=true;
            break;
          end;
        end;
      end;
    end;
  end;

  function TeamMode(const fn: string): boolean;    {TeamMode 1433.0 in Pan mode}
  var i: integer;
  begin
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Remote Datei durchsuchen}
      for i:=2 to inlist.Count-1 do begin          {1. Zeile ignorieren}
        splitlist.DelimitedText:=inlist[i];
        if splitlist.Count>10 then begin           {S2 Gimbal pan mode=1433 suchen}
          if trunc(StrToFloatN(splitlist[10]))=m45val then begin
            result:=true;
            break;
          end;
        end;
      end;
    end;
  end;

  function EditFind(const fn: string): boolean;    {Kombination Suche}
  var k, p: integer;
  begin
    p:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                  {Datei durchsuchen}
      if GetFMPos then exit;
      splitlist.DelimitedText:=inlist[0];          {Spalte suchen}
      for k:=1 to splitlist.Count-1 do
        if splitlist[k]=LabeledEdit1.Text then begin
          p:=k;                                    {Spaltennummer}
          break;
        end;
      if p>0 then for k:=1 to inlist.Count-1 do begin
        splitlist.DelimitedText:=inlist[k];        {Nach Wert suchen}
        if splitlist.Count<=p then exit;           {zerstörte Datensätze}
        if splitlist[p]=ComboBox9.Text then begin  {Wert aus Suchfeld suchen}
          result:=true;
          break;
        end;
      end;
    end;
  end;

  function FindSensorFW(const fn: string): boolean; {Sensor Dateien mit FW finden}
  var az: integer;
      arrFW: TarrFW;
  begin
    result:=false;
    if FileSize(fn)>FWsz then begin                {Mindestanzahl 18 Bytes für FW}
      az:=ReadFW(fn, arrFW);
      if (az>2) then
        result:=true;                              {mindestens 3 Werte}
      if pos(skyext, fn)>1 then
        result:=true;                              {alle *.sky haben H520 FW}
    end;
  end;

  procedure Ausgabe;                               {Liste füllen}
  begin
    inc(zhl);
    StringGrid5.RowCount:=zhl+1;
    StringGrid5.Cells[0, zhl]:=IntToStr(zhl);
    StringGrid5.Cells[1, zhl]:=flist[i];
  end;

begin
  BitBtn26.Tag:=0;                                 {Info Mode used}
  StringGrid5.RowCount:=1;                         {Tabelle löschen}
  StringGrid5.Cells[1, 0]:=capProb+bind+rsFLDir;   {Überschrift}
  ProgressBar1.Position:=0;
  RadioGroup1.ItemIndex:=0;                        {File type: Telemetry}
  zhl:=0;                                          {Trefferzähler}
  StringGrid1.Tag:=19;                     {default Position bei neuer FW ST10+}
  ComboBox9.Text:=trim(StringReplace(ComboBox9.Text, sep, '.', []));
  flist:=TStringList.Create;
  vlist:=TStringList.Create;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;                   {CSV zerlegen}
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  vlist.Add(IncludeTrailingPathDelimiter(ComboBox8.Text));
  Screen.Cursor:=crHourGlass;
  try
    CreateDirList(ComboBox8.Text, vlist);
    case RadioGroup9.ItemIndex of
      5, 6: begin
              RadioGroup1.ItemIndex:=2;            {File type: Remote}
              for i:=0 to vlist.Count-1 do SuchFile(vlist[i], ffile+wldcd+fext, flist);
            end;
      7: begin                                     {Edit/Find abh. von selektierter Spalte}
           RadioGroup1.ItemIndex:=LabeledEdit1.Tag;    {File type übernehmen}
           case LabeledEdit1.Tag of
             1:   for i:=0 to vlist.Count-1 do SuchFile(vlist[i], sfile+wldcd+fext, flist);
             2:   for i:=0 to vlist.Count-1 do SuchFile(vlist[i], ffile+wldcd+fext, flist);
             3:   for i:=0 to vlist.Count-1 do SuchFile(vlist[i], nfile+wldcd+sext, flist);
             else for i:=0 to vlist.Count-1 do SuchFile(vlist[i], kfile+wldcd+fext, flist);
           end;
         end;
      8, 9: for i:=0 to vlist.Count-1 do begin     {Sensor Dateien}
              if SpinEdit3.Tag=YTHPid then
                SuchFile(vlist[i], nfile+sextP, flist)
              else begin
                SuchFile(vlist[i], nfile+wldcd+sext, flist);
                SuchFile(vlist[i], wldcd+skyext, flist);
              end;
            end;
      10: for i:=0 to vlist.Count-1 do begin     {PX4 Sensor Dateien}
            SuchFile(vlist[i], wldcd+hext, flist);             {H520}
            Suchfile(vlist[i], nfile+wldcd+wext, flist);   {Mantis Q}
            Suchfile(vlist[i], mfile+wldcd+bext, flist);   {Mantis Q}
          end;
      else begin
        for i:=0 to vlist.Count-1 do SuchFile(vlist[i], kfile+wldcd+fext, flist);
      end;
    end;                                           {default file type Telemetry}
    StatusBar1.Panels[0].Text:=IntToStr(vlist.Count);  {Anzahl Verzeichnisse}
    StatusBar1.Panels[1].Text:=IntToStr(flist.Count);  {Anzahl Dateien}
    Synedit1.Lines.Add('');
    Synedit1.Lines.Add(RadioGroup9.Items[RadioGroup9.ItemIndex]);
    Synedit1.Lines.Add(ComboBox8.Text);
    SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDateien);
    StringGrid5.BeginUpdate;
    if flist.Count>0 then begin                        {Dateien vorhanden}
      ProgressBar1.Max:=flist.Count;
      for i:=0 to flist.Count-1 do begin
        case RadioGroup9.ItemIndex of
          0: if Emergency12(flist[i]) then Ausgabe;
          1: if CCWNum(flist[i]) then Ausgabe;
          2: if CCWTime(flist[i]) then Ausgabe;
          3: if VoltW2(flist[i]) then Ausgabe;
          4: if VoltMin(flist[i]) then Ausgabe;
          5: if StickCali(flist[i]) then Ausgabe;      {Sticks in Remote}
          6: if TeamMode(flist[i]) then Ausgabe;       {Suche nach 1433.0 in Remote}
          7: if EditFind(flist[i]) then Ausgabe;
          8: if FileSize(flist[i])>FWsz then Ausgabe;  {Sensorfile > 6 Byte}
          9: if FindSensorFW(flist[i]) then Ausgabe;   {Sensorfile mit FW}
          10: if ShowSensorPlus(flist[i], 0, false, false, true) then begin
                Ausgabe;
//                ShowSensorPlus(flist[i], 0, false, true, false); {gleich ausgeben?}
              end;
        end;
        ProgressBar1.Position:=i;
      end;
    end;
    StringGrid5.Cells[0, 0]:=rsNum+' ('+IntToStr(zhl)+')';  {Überschrift}
    StringGrid5.EndUpdate;
    if zhl>0 then begin                                     {Anzahl Treffer}
      StatusBar1.Panels[5].Text:=IntToStr(zhl)+rsSuspect;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      ComboBox8.Text:=ExcludeTrailingPathDelimiter(ComboBox8.Text);
      MerkListe(ComboBox8, AnzDirs);
    end else begin
      StatusBar1.Panels[5].Text:=rsNoFound;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBar1.Position:= ProgressBar1.Max;
  finally
    FreeAndNil(flist);
    FreeAndNil(vlist);
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.BitBtn27Click(Sender: TObject);   {AppLog speichern}
begin
  SaveDialog1.Title:=rsFileSave;
  SaveDialog1.FileName:=ComplFN(TabSheet6.Caption, now)+wext;
  if SaveDialog1.Execute then begin
    SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
    SynEdit1.Lines.Clear;
    StatusBar1.Panels[5].Text:=TabSheet6.Caption+tab1+rsSaved+
                               suff+SaveDialog1.FileName;
  end;
end;

procedure TForm1.BitBtn28Click(Sender: TObject);   {Lösche AppLog}
begin
  SynEdit1.Lines.Clear;
end;

procedure TForm1.YFlugBuch;
var vlist, flist, outlist: TStringList;
    x, flno, gfd, datpos: integer;
    prtext: string;
    gftime: TDateTime;
    gdist: Double;

  procedure TextAusgabe;                           {Flugprotokoll als Textdatei}
  var x: integer;
  begin
    outlist.Add(trenner+tab1+capNachweis+tab1+trenner);
    outlist.Add('');
    outlist.Add(tabs(rsFlightReport, suff, tabu)+ComboBox1.text);  {ggf. Seriennummer}
    outlist.Add(tabs(rsCreat+tab1, suff, tabu)+
                FormatDateTime(mzf, now)+'h  '+tab1+rsBy+tab1+
                ExtractFileName(Application.ExeName)+tab2+version);
    if CheckBox9.Checked then
      outlist.Add(capCheckBox9);
    outlist.Add('');
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then begin            {neuer Datensatz}
        outlist.Add(trenner+FormSR(flist[1], UTF8Length(capNachweis)+1)+
                    tab1+trenner);
      end else begin
        if datpos=0 then outlist.Add(tabs(flist[0], suff, tabu)+flist[1]);
        if datpos>0 then outlist[datpos]:=outlist[datpos]+tab1+flist[0]+tab1+flist[1];
        if flist[0]=rsGridCell1 then datpos:=outlist.count-1; {Datum + Zeit}
        if flist[0]=rsGridCell3 then datpos:=0;
      end;
    end;
    outlist.Add('');
    outlist.Add(trenner+trenner+trenner);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(tabs(rsGFtime, suff, tabu)+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(tabs(rsGFtime, suff, tabu)+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if RadioGroup3.ItemIndex=2 then begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FloatToStrF(gdist/fmile, ffFixed, 10, 1)+'mi');
      end else begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FloatToStrF(gdist, ffFixed, 10, 1)+'km');
      end;
    prtext:=wexdef;                                {speichern als Text}
  end;

  procedure CSVAusgabe;                            {Flugprotokoll als CSV Datei}
  var csvlist: array [0..15] of string;
      x, y: integer;
      s: string;
  begin
    outlist.Add(capNachweis);
    outlist.Add('');
    s:=rsCreat+tab1+dstr+FormatDateTime(mzf, now)+'h'+dstr+
       tab1+rsBy+tab1+ExtractFileName(Application.ExeName)+tab2+version;
    if CheckBox9.Checked then s:=s+dstr+capCheckBox9;
    outlist.Add(s);
    outlist.Add('');
    csvlist[0]:=rsFlightNr;
    csvlist[1]:=rsVType;
    csvlist[2]:='GPS/Sim';
    csvlist[3]:=rsGridCell1;
    csvlist[4]:=rsGridCell2;
    csvlist[5]:=rsGridCell3;
    csvlist[6]:=rsDauer;
    csvlist[7]:=rsStartpkt;
    csvlist[8]:=rsAdresse;
    csvlist[9]:=rsMode;
    csvlist[10]:=rsGridCell5;
    csvlist[11]:=rsGridCell6;
    csvlist[12]:=rsGridCell7;
    csvlist[13]:=rsGridCell8;
    csvlist[14]:=rsAvgSpeed;
    csvlist[15]:=rsRest;
    prtext:=csvlist[0];
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+dstr+csvlist[y];
    outlist.Add(prtext);
    for y:=low(csvlist) to high(csvlist) do csvlist[y]:=''; {Array löschen}
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then begin            {neuer Datensatz}
        if csvlist[1]<>'' then begin               {alle außer 1.}
          prtext:=csvlist[0];
          for y:=low(csvlist)+1 to high(csvlist) do begin
            prtext:=prtext+dstr+csvlist[y];
            csvlist[y]:='';                        {Array löschen}
          end;
          outlist.Add(prtext);
        end;
        csvlist[0]:=flist[1];
      end;
      if flist[0]=rsVType then csvlist[1]:=flist[1];
      if flist[0]='' then csvlist[2]:=flist[1];
      if flist[0]=rsGridCell1 then csvlist[3]:=flist[1];
      if flist[0]=rsGridCell2 then csvlist[4]:=flist[1];
      if flist[0]=rsGridCell3 then csvlist[5]:=flist[1];
      if flist[0]=rsDauer then csvlist[6]:=flist[1];
      if flist[0]=rsStartpkt then csvlist[7]:=flist[1];
      if flist[0]=rsAdresse then csvlist[8]:=flist[1];
      if flist[0]=rsMode then csvlist[9]:=flist[1];
      if flist[0]=rsGridCell5 then csvlist[10]:=flist[1];
      if flist[0]=rsGridCell6 then csvlist[11]:=flist[1];
      if flist[0]=rsGridCell7 then csvlist[12]:=flist[1];
      if flist[0]=rsGridCell8 then csvlist[13]:=flist[1];
      if flist[0]=rsAvgSpeed then csvlist[14]:=flist[1];
      if flist[0]=rsRest then csvlist[15]:=flist[1];
    end;
    prtext:=csvlist[0];                            {letzten DS ausgeben}
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+dstr+csvlist[y];
    outlist.Add(prtext);
    outlist.Add('');
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(rsGFtime+dstr+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(rsGFtime+dstr+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if RadioGroup3.ItemIndex=2 then begin
        outlist.Add(rsGFstr+dstr+FloatToStrF(gdist/fmile, ffFixed, 10, 1)+'mi');
      end else begin
        outlist.Add(rsGFstr+dstr+FloatToStrF(gdist, ffFixed, 10, 1)+'km');
      end;
    prtext:=csvdef;                                {speichern als CSV}
  end;

begin            {ganzes Verzeichnis durchsuchen nach Telemetry_*.csv}
  if ComboBox8.Items.Count>0 then
    for x:=ComboBox8.Items.Count-1 downto 0 do     {Liste putzen}
      if not DirectoryExists(ComboBox8.Items[x]) then ComboBox8.Items.Delete(x);
  if DirectoryExists(ComboBox8.Text) then begin
    ComboBox8.Text:=ExcludeTrailingPathDelimiter(ComboBox8.Text);
    MerkListe(ComboBox8, AnzDirs);
    Screen.Cursor:=crHourGlass;
    vlist:=TStringList.Create;
    flist:=TStringList.Create;
    outlist:=TStringList.Create;
    flno:=1;
    gftime:=0;                                     {Gesamt Flugzeit}
    gdist:=0;                                      {gesamt Strecke, nur bei GPS}
    datpos:=0;
    try
      vlist.Add(IncludeTrailingPathDelimiter(ComboBox8.Text));
      CreateDirList(ComboBox8.Text, vlist);
      for x:=0 to vlist.Count-1 do
        SuchFile(vlist[x], kfile+wldcd+fext, flist);
      if flist.Count>1 then begin                  {genug Dateien?}
        StatusBar1.Panels[0].Text:=IntToStr(vlist.Count);  {Anzahl Verzeichnisse}
        StatusBar1.Panels[1].Text:=IntToStr(flist.Count);  {Anzahl Telemetrie}
        SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+rsTurns);
        StatusBar1.Update;
        vlist.Clear;  {brauchen wir hier nicht mehr, wird csv für Ausgabe benutzt}
        flist.Sort;
        ProgressBar1.Max:=flist.Count;
        StringGrid5.BeginUpdate;
        for x:=0 to flist.Count-1 do               {Dateien abarbeiten}
          ProtoWerte(flist[x], vlist, flno, gftime, gdist);
        StringGrid5.Cells[0,0]:=rsNum+'('+IntToStr(flno-1)+')';
        StringGrid5.EndUpdate;
        gfd:=Trunc(gftime);                        {Anzahl Tage der Flugzeit}
        flist.Clear;  {brauchen wir hier nicht mehr, wird splitlist}
        flist.Delimiter:=dstr;                     {Semicolon als Trenner}
        flist.StrictDelimiter:=True;               {keine Spaces als Trenner}
        case RadioGroup8.ItemIndex of              {Ausgabeformat}
          0: TextAusgabe;
          1: CSVAusgabe;
        end;
        StatusBar1.Panels[5].Text:=rsGFtime+suff;
        if gfd=0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                                     FormatDateTime(zzf, gftime)+'h';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                            IntToStr(gfd)+'d '+FormatDateTime(zzf, gftime)+'h';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        if gdist>0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+' / '+rsGFstr+suff;
          if RadioGroup3.ItemIndex=2 then begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FloatToStrF(gdist/fmile, ffFixed, 10, 1)+'mi';
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
          end else begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FloatToStrF(gdist, ffFixed, 10, 1)+'km';
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
          end;
        end;
        Screen.Cursor:=crDefault;
        SaveDialog1.Title:=rsProtSave;
        SaveDialog1.InitialDir:=ComboBox8.Text;
        SaveDialog1.FileName:=Form2.CleanDN(capNachweis+prtext);
        if SaveDialog1.Execute then begin
          ProgressBar1.Position:=0;
          outlist.SaveToFile(SaveDialog1.FileName);
        end;
      end;                                         {Ende genug Dateien}
    finally
      Screen.Cursor:=crDefault;
      FreeAndNil(vlist);
      FreeAndNil(flist);
      FreeAndNil(outlist);
    end;
  end else begin
    StatusBar1.Panels[5].Text:=rsError;
    SynEdit1.Lines.Add('''5544'+suff+StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.BrFlugBuch;                       {Yuneec breeze}
var vlist, flist, outlist: TStringList;
    x, flno, gfd, datpos: integer;
    prtext: string;
    gftime: TDateTime;
    gdist: Double;

  procedure TextAusgabe;                           {Flugprotokoll als Textdatei}
  var x: integer;
  begin
    outlist.Add(trenner+tab1+capNachweis+tab1+trenner);
    outlist.Add('');
    outlist.Add(tabs(rsFlightReport, suff, tabu)+ComboBox1.text); {ggf. Seriennummer}
    outlist.Add(tabs(rsCreat+tab1, suff, tabu)+
                FormatDateTime(mzf, now)+'h  '+tab1+rsBy+tab1+
                ExtractFileName(Application.ExeName)+tab2+version);
    outlist.Add('');
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then                  {neuer Datensatz}
        outlist.Add(trenner+FormSR(flist[1], UTF8Length(capNachweis)+1)+
                    tab1+trenner)
      else begin
        if datpos=0 then outlist.Add(tabs(flist[0], suff, tabu)+flist[1]);
        if datpos>0 then outlist[datpos]:=outlist[datpos]+tab1+flist[0]+tab1+flist[1];
        if flist[0]=rsGridCell1 then datpos:=outlist.count-1; {Datum + Zeit}
        if flist[0]=rsGridCell3 then datpos:=0;
      end;
    end;
    outlist.Add('');
    outlist.Add(trenner+trenner+trenner);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(tabs(rsGFtime, suff, tabu)+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(tabs(rsGFtime, suff, tabu)+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if RadioGroup3.ItemIndex=2 then begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FloatToStrF(gdist/fmile, ffFixed, 10, 1)+'mi');
      end else begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FloatToStrF(gdist, ffFixed, 10, 1)+'km');
      end;
    prtext:=wexdef;                                {speichern als Text}
  end;

  procedure CSVAusgabe;                            {Flugprotokoll als CSV Datei}
  var csvlist: array [0..15] of string;
      x, y: integer;
  begin
    outlist.Add(capNachweis);
    outlist.Add('');
    outlist.Add(rsCreat+tab1+dstr+FormatDateTime(mzf, now)+'h'+dstr+tab1+rsBy+tab1+
                ExtractFileName(Application.ExeName)+tab2+version);
    outlist.Add('');
    csvlist[0]:=rsFlightNr;
    csvlist[1]:=rsVType;
    csvlist[2]:='GPS';
    csvlist[3]:=rsGridCell1;
    csvlist[4]:=rsGridCell2;
    csvlist[5]:=rsGridCell3;
    csvlist[6]:=rsDauer;
    csvlist[7]:=rsStartpkt;
    csvlist[8]:=rsAdresse;
    csvlist[9]:=rsMode;
    csvlist[10]:=rsGridCell5;
    csvlist[11]:=rsGridCell6;
    csvlist[12]:=rsGridCell7;
    csvlist[13]:='n/a';
    csvlist[14]:=rsAvgSpeed;
    csvlist[15]:=rsRest;
    prtext:=csvlist[0];
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+dstr+csvlist[y];
    outlist.Add(prtext);
    for y:=low(csvlist) to high(csvlist) do csvlist[y]:=''; {Array löschen}
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then begin            {neuer Datensatz}
        if csvlist[1]<>'' then begin               {alle außer 1.}
          prtext:=csvlist[0];
          for y:=low(csvlist)+1 to high(csvlist) do begin
            prtext:=prtext+dstr+csvlist[y];
            csvlist[y]:='';                        {Array löschen}
          end;
          outlist.Add(prtext);
        end;
        csvlist[0]:=flist[1];
      end;
      if flist[0]=rsVType then csvlist[1]:=flist[1];
      if flist[0]='' then csvlist[2]:=flist[1];
      if flist[0]=rsGridCell1 then csvlist[3]:=flist[1];
      if flist[0]=rsGridCell2 then csvlist[4]:=flist[1];
      if flist[0]=rsGridCell3 then csvlist[5]:=flist[1];
      if flist[0]=rsDauer then csvlist[6]:=flist[1];
      if flist[0]=rsStartpkt then csvlist[7]:=flist[1];
      if flist[0]=rsAdresse then csvlist[8]:=flist[1];
      if flist[0]=rsMode then csvlist[9]:=flist[1];
      if flist[0]=rsGridCell5 then csvlist[10]:=flist[1];
      if flist[0]=rsGridCell6 then csvlist[11]:=flist[1];
      if flist[0]=rsGridCell7 then csvlist[12]:=flist[1];
      if flist[0]=rsAvgSpeed then csvlist[14]:=flist[1];
      if flist[0]=rsRest then csvlist[14]:=flist[1];
    end;
    prtext:=csvlist[0];                            {letzten DS ausgeben}
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+dstr+csvlist[y];
    outlist.Add(prtext);
    outlist.Add('');
    outlist.Add('');
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(rsGFtime+dstr+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(rsGFtime+dstr+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if RadioGroup3.ItemIndex=2 then begin
        outlist.Add(rsGFstr+dstr+FloatToStrF(gdist/fmile, ffFixed, 10, 1)+'mi');
      end else begin
        outlist.Add(rsGFstr+dstr+FloatToStrF(gdist, ffFixed, 10, 1)+'km');
      end;
    prtext:=csvdef;                                {speichern als CSV}
  end;

begin            {ganzes Verzeichnis durchsuchen nach Telemetry_*.csv}
  if ComboBox8.Items.Count>0 then
    for x:=ComboBox8.Items.Count-1 downto 0 do     {Liste putzen}
      if not DirectoryExists(ComboBox8.Items[x]) then
        ComboBox8.Items.Delete(x);
  if DirectoryExists(ComboBox8.Text) then begin
    ComboBox8.Text:=ExcludeTrailingPathDelimiter(ComboBox8.Text);
    MerkListe(ComboBox8, AnzDirs);
    Screen.Cursor:=crHourGlass;
    vlist:=TStringList.Create;
    flist:=TStringList.Create;
    outlist:=TStringList.Create;
    flno:=1;
    gftime:=0;                                     {Gesamt Flugzeit}
    gdist:=0;                                      {gesamt Strecke, nur bei GPS}
    datpos:=0;
    try
      vlist.Add(IncludeTrailingPathDelimiter(ComboBox8.Text));
      CreateDirList(ComboBox8.Text, vlist);
      for x:=0 to vlist.Count-1 do
        SuchFile(vlist[x], wldcd+bext, flist);
      if flist.Count>1 then begin                  {genug Dateien?}
        StatusBar1.Panels[1].Text:=IntToStr(flist.Count); {Anzahl BrTelemetrie}
        SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+rsTurns);
        StatusBar1.Panels[0].Text:=IntToStr(vlist.Count); {Anzahl Verzeichnisse}
        StatusBar1.Update;
        vlist.Clear;  {brauchen wir hier nicht mehr, wird csv für Ausgabe}
        flist.Sort;
        ProgressBar1.Max:=flist.Count;
        StringGrid5.BeginUpdate;
        for x:=0 to flist.Count-1 do               {Dateien abarbeiten}
          BrProtoWerte(flist[x], vlist, flno, gftime, gdist);
        StringGrid5.Cells[0,0]:=rsNum+'('+IntToStr(flno-1)+')';
        StringGrid5.EndUpdate;
        gfd:=Trunc(gftime);                        {Anzahl Tage der Flugzeit}
        flist.Clear;  {brauchen wir hier nicht mehr, wird splitlist}
        flist.Delimiter:=dstr;                     {Semicolon als Trenner}
        flist.StrictDelimiter:=True;               {keine Spaces als Trenner}
        case RadioGroup8.ItemIndex of              {Ausgabeformat}
          0: TextAusgabe;
          1: CSVAusgabe;
        end;
        StatusBar1.Panels[5].Text:=rsGFtime+suff;
        if gfd=0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                                     FormatDateTime(zzf, gftime)+'h';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                            IntToStr(gfd)+'d '+FormatDateTime(zzf, gftime)+'h';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        if gdist>0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+' / '+rsGFstr+suff;
          if RadioGroup3.ItemIndex=2 then begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FloatToStrF(gdist/fmile, ffFixed, 10, 1)+'mi';
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
          end else begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FloatToStrF(gdist, ffFixed, 10, 1)+'km';
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
          end;
        end;
        Screen.Cursor:=crDefault;
        SaveDialog1.Title:=rsProtSave;
        SaveDialog1.InitialDir:=ComboBox8.Text;
        SaveDialog1.FileName:=Form2.CleanDN(capNachweis+'YBr'+prtext);
        if SaveDialog1.Execute then begin
          ProgressBar1.Position:=0;
          outlist.SaveToFile(SaveDialog1.FileName);
        end;
      end;                                         {Nichts tun bei <2 Dateien}
    finally
      Screen.Cursor:=crDefault;
      FreeAndNil(vlist);
      FreeAndNil(flist);
      FreeAndNil(outlist);
    end;
  end else begin
    StatusBar1.Panels[5].Text:=rsError;
    SynEdit1.Lines.Add('''5752'+suff+StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.BitBtn25Click(Sender: TObject);   {Flugbuch erstellen}
begin
  StringGrid5.RowCount:=1;                         {Tabelle löschen}
  StringGrid5.Cells[1, 0]:=capNachweis;            {Überschrift}
  ProgressBar1.Position:=0;
  BitBtn26.Tag:=1;
  YFlugBuch;
  BrFlugBuch;                                      {noch Breeze versuchen}
end;

procedure TForm1.RadioGroup6Click(Sender: TObject); {Cam mode}
begin
  if RadioGroup6.Enabled then begin
    if RadioGroup6.ItemIndex=0 then
      CGO3run('SET_CAM_MODE&mode=video', 0)
    else
      CGO3run('SET_CAM_MODE&mode=photo', 0);
    Sleep(800);
    CGO3run('', 0);
  end;
end;

procedure TForm1.CheckBox5Change(Sender: TObject); {Autoexposure}
var s: string;
begin
  FloatSpinEdit1.Enabled:=CheckBox5.Checked;
  if BitBtn23.Enabled then begin
    if CheckBox5.Checked then
      s:='1'
    else
      s:='0';
    CGO3run('SET_AE_ENABLE&mode='+s, 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.CheckBox7Change(Sender: TObject); {Pilotenpfad geändert}
begin
  EnSave;                                          {Speichern erlauben}
end;

procedure TForm1.RadioGroup7Click(Sender: TObject);
begin
  if (RadioGroup7.Enabled) and
     (RadioGroup7.ItemIndex>=0) then begin
    CGO3run('SET_PHOTO_FORMAT&format='+RadioGroup7.Items[RadioGroup7.ItemIndex], 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.PlatformLesen;          {Headerdaten aus Breeze anzeigen}
var fn: string;
    inlist: TStringlist;
    x, k: integer;
    s1, s2: string;
begin
  fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
      ListBox1.Items[ListBox1.ItemIndex]+bext;
  inlist:=TStringList.Create;
  try
    if FileExists(fn)then begin
      inlist.LoadFromFile(fn);
      SynEdit1.Lines.Add('');
      if pos(plfAndr, inlist[2])>0 then
        BitBtn3.Tag:=1
      else
        BitBtn3.Tag:=0;
      for x:=1 to 5 do begin
        s1:='';
        s2:=inlist[x];
        SynEdit1.Lines.Add(s2);
        if length(s2)>3 then begin
          for k:=1 to length(s2) do begin
            if s2[k]<>':' then
              s1:=s1+s2[k]
            else
              break;
          end;
          s2:=copy(s2, k+1, length(s2)-k);
          StringGrid4.Cells[0, x]:=s1;
          StringGrid4.Cells[1, x]:=s2;
        end;
      end;
    end;
  finally
    FreeAndNil(inlist);
  end;
end;

procedure TForm1.FirmwareLesen;          {Firmwarestände aus Sensor anzeigen}
var FWarr: TarrFW;
    i, az: integer;
begin
  if PageControl2.ActivePageIndex=2 then begin
    StringGrid4.Cells[0, 1]:=rsKamera;
    StringGrid4.Cells[0, 2]:=rsGimbal;
    StringGrid4.Cells[0, 3]:=rsAutoP;
    StringGrid4.Cells[0, 4]:='?';
    StringGrid4.Cells[0, 5]:=rsRealSense;
    az:=GetFW(FWarr);
    if az>2 then begin
      SynEdit1.Lines.Add('');
      for i:=0 to az do begin
        StringGrid4.Cells[1, i+1]:=FWarr[i];
        if FWarr[i]<>'' then
          SynEdit1.Lines.Add(StringGrid4.Cells[0, i+1]+suff+FWArr[i]);
      end;
    end;
  end;
end;

procedure TForm1.FloatSpinEdit1Change(Sender: TObject);  {Set EV}
begin
  if BitBtn23.Enabled then begin
    CGO3run('SET_EXPOSURE_VALUE&mode='+
            FloatToStrf(FloatSpinEdit1.Value, ffFixed, 3, 1), 0);
    CGO3run('', 0);
  end;
end;

procedure TForm1.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Zoom rücksetzen}
begin
  if ChartToolset2DataPointCrosshairTool1.Enabled then begin
    if Button=mbLeft then begin
      Analyse;
      if ssCtrl in Shift then begin
        StatusToClipboard;                         {Analyse in Zwischenablage}
        StatusBar1.Panels[5].Text:='';             {Analyseergebnis löschen}
      end;
    end;
  end;
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                      {Klicken mit mittlerer Taste}
    Chart1.ZoomFull;
end;

procedure TForm1.Chart3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then begin                {Klicken mit mittlerer Taste}
       Chart3.ZoomFull;
       Chart4.ZoomFull;
       Chart5.ZoomFull;
  end;
end;

procedure TForm1.Chart4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then begin                {Klicken mit mittlerer Taste}
       Chart3.ZoomFull;
       Chart4.ZoomFull;
       Chart5.ZoomFull;
  end;
end;

procedure TForm1.Chart5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) or                          {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then begin                {Klicken mit mittlerer Taste}
       Chart3.ZoomFull;
       Chart4.ZoomFull;
       Chart5.ZoomFull;
  end;
end;

{http://wiki.lazarus.freepascal.org/TAChart_Tutorial:_Chart_Tools
 http://www.lazarusforum.de/viewtopic.php?f=18&t=11521}

procedure TForm1.ChartToolset2DataPointCrosshairTool1Draw(ASender: TDataPointDrawTool);
var abst, idx: integer;                            {Index bestimmen und Werte anzeigen}

  procedure AnzeigeBreeze;
  begin
    StatusBar1.Panels[3].Text:=FormatDateTime(zzf,
               ZeitToDT(StringGrid1.Cells[0, SpinEdit3.Value], SpinEdit3.Tag));
    StatusBar1.Panels[4].Text:=FormatDateTime(zzf,
               ZeitToDT(StringGrid1.Cells[0, abst], SpinEdit3.Tag));
    if pos(capTabSheet10, StatusBar1.Panels[5].Text)<1 then begin
      StatusBar1.Panels[5].Text:=StringGrid1.Cells[10, SpinEdit3.Value]+'cm - '+
                 fmode+'='+StringGrid1.Cells[2, SpinEdit3.Value]+tab1+
                 brFModeToStr(StrToInt(StringGrid1.Cells[StringGrid1.Tag, SpinEdit3.Value]));
    end;
  end;

  procedure AnzeigeYLegacy;
  begin
    StatusBar1.Panels[3].Text:=FormatDateTime(zzf+zzz,
               ZeitToDT(StringGrid1.Cells[0, SpinEdit3.Value], SpinEdit3.Tag));
    StatusBar1.Panels[4].Text:=FormatDateTime(zzf+zzz,
               ZeitToDT(StringGrid1.Cells[0, abst], SpinEdit3.Tag));
    if pos(capTabSheet10, StatusBar1.Panels[5].Text)<1 then begin
      StatusBar1.Panels[5].Text:=StringGrid1.Cells[2, SpinEdit3.Value]+'V - '+
                 StringGrid1.Cells[4, SpinEdit3.Value]+'m - '+fmode+'='+
                 StringGrid1.Cells[StringGrid1.Tag, SpinEdit3.Value]+tab1+
                 FModeToStr(StrToInt(StringGrid1.Cells[StringGrid1.Tag, SpinEdit3.Value]));
    end;
  end;

begin
  if ChartToolset2DataPointCrosshairTool1.Enabled then begin
    idx:=ASender.PointIndex+1;
    if (idx>1) and                                 {im gültigen Bereich}
       (idx<StringGrid1.RowCount) then
      SpinEdit3.Value:=idx;                        {Datenindex übernehmen}
    abst:=SpinEdit3.Value+SpinEdit2.Value;         {Ende des Intervalls}

    try                                            {Versuchen, zweiten Kursor zu setzen}
      Chart1ConstantLine1.Position:=Chart1LineSeries1.XValue[abst];
      Chart1ConstantLine1.Active:=true;            {Anzeige Ende Intervall}
    except
      Chart1ConstantLine1.Active:=false;
    end;
    if abst>StringGrid1.RowCount-2 then            {Überlauf vermeiden}
      abst:=SpinEdit3.Value;

    if SpinEdit3.Tag=brID then begin               {Breeze}
      AnzeigeBreeze;
    end else begin                                 {legacy Yuneec}
      AnzeigeYLegacy;
    end;
  end;
end;

procedure TForm1.CheckBox11Change(Sender: TObject);  {Extrude für KML geändert}
begin
  if CheckBox11.Checked then
    RadioGroup5.ItemIndex:=1;                      {Set relative to ground}
  EnSave;
end;

procedure TForm1.CheckBox1Change(Sender: TObject); {Filteranzeige}
begin
  RadioGroup10.Enabled:=CheckBox1.Checked;
end;

function nahe(lat1, lon1, lat2, lon2: double): boolean; {Wert in der Nähe}
begin
  result:=(abs(lat1-lat2)<nhw) and (abs(lon1-lon2)<nhw);
end;

procedure TForm1.CheckBox3Change(Sender: TObject); {Dashware geändert}
begin
  EnSave;                                          {Speichern erlauben}
end;

procedure TForm1.ColorButton1Click(Sender: TObject);
begin                     {Erneut Speichern erlauben, wenn Farbe geändert wurde}
  EnSave;
end;

procedure TForm1.BitBtn12Click(Sender: TObject);   {Defaulteinstellung}
begin                  {Reset Schnellanalyse für alle drei Histogramme}
  Timer3.Enabled:=false;
  SetProfile(0);
  ComboBox10.ItemIndex:=0;                         {Profiles rücksetzen}
end;

{Profiles sind von mir häufig benutzte Einstellungen für die Schnellanalyse.
 Sie sind hier hart codiert und vom Benutzer nicht editierbar.}

procedure TForm1.ComboBox10Change(Sender: TObject);
begin                                              {Profile ausgewählt}
  if ComboBox10.ItemIndex>0 then SetProfile(ComboBox10.ItemIndex);
end;

procedure TForm1.SetProfile(idx: integer);         {Profile ausgewählt}

  procedure DefaultCl;
  begin
    ComboBox10.ItemIndex:=0;
    ColorButton2.ButtonColor:=clMaroon;
    ColorButton3.ButtonColor:=clPurple;
    ColorButton4.ButtonColor:=clTeal;
  end;

  procedure ProfileBreeze;                         {Für Breeze nur default}
  begin
    LabeledEdit1.Tag:=brID;
    LabeledEdit2.Tag:=brID;
    LabeledEdit3.Tag:=brID;
    LabeledEdit1.Text:=csvPitch;
    LabeledEdit2.Text:=csvRoll;
    LabeledEdit3.Text:=csvYaw;
    DefaultCl;
  end;

  procedure ProfileYTHPlus;
  begin
    case idx of
      0: begin      {Default Button: Reset Schnellanalyse für alle drei Histogramme}
           LabeledEdit1.Text:='fskRssi';
           LabeledEdit2.Text:=csvTas;
           LabeledEdit3.Text:='gpsAccH';
           DefaultCl;
         end;
  {** Hier Profiles editieren ** :: ** Profiles auch in PopupMenu3 **}
      1: begin                                     {Profile: FlightMode}
           LabeledEdit1.Text:='fMode';
           LabeledEdit2.Text:='CH4';               {Flight Mode Switch}
           LabeledEdit2.Tag:=2;                    {Tag=2: Remote}
           LabeledEdit3.Text:=csvVolt;
         end;
      2: begin                                     {Profile: Errors}
           LabeledEdit1.Text:='errorFlags1';
           LabeledEdit2.Text:='motorStatus';
           LabeledEdit3.Text:='pressCompassStatus';
         end;
      3: begin                                     {Profile: GPS}
           LabeledEdit1.Text:='gpsAccH';
           LabeledEdit2.Text:='gpsUsed';
           LabeledEdit3.Text:='imuStatus';
         end;
      4: begin                                     {Profile: Throttle}
           LabeledEdit1.Text:='CH0';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvAlt;
           LabeledEdit3.Text:=csvVolt;
         end;
      5: begin                                     {Profile: Pitch}
           LabeledEdit1.Text:='CH2';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvPitch;
           LabeledEdit3.Text:=csvTas;
         end;
      6: begin                                     {Profile: Roll}
           LabeledEdit1.Text:='CH1';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvRoll;
           LabeledEdit3.Text:=csvTas;
         end;
      7: begin                                     {Profile: Yaw}
           LabeledEdit1.Text:='CH3';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvYaw;
           LabeledEdit3.Text:='errorFlags1';
         end;
      8: begin                                     {Profile: 3Axis}
           LabeledEdit1.Text:=csvPitch;
           LabeledEdit2.Text:=csvRoll;
           LabeledEdit3.Text:=csvYaw;
         end;
    end;                                           {Ende case profiles}
  end;

  procedure ProfileYLegacy;
  begin
    case idx of
      0: begin  {Default Button: Reset Schnellanalyse für alle drei Histogramme}
           LabeledEdit1.Text:='fsk_rssi';
           LabeledEdit2.Text:=csvTas;
           LabeledEdit3.Text:='gps_accH';
           DefaultCl;
         end;
  {** Hier Profiles editieren ** :: ** Profiles auch in PopupMenu3 **}
      1: begin                                     {Profile: FlightMode}
           LabeledEdit1.Text:='f_mode';
           LabeledEdit2.Text:='CH4';               {Flight Mode Switch}
           LabeledEdit2.Tag:=2;                    {Tag=2: Remote}
           LabeledEdit3.Text:=csvVolt;
         end;
      2: begin                                     {Profile: Errors}
           LabeledEdit1.Text:='error_flags1';
           LabeledEdit2.Text:='motor_status';
           LabeledEdit3.Text:='press_compass_status';
         end;
      3: begin                                     {Profile: GPS}
           LabeledEdit1.Text:='gps_accH';
           LabeledEdit2.Text:='gps_used';
           LabeledEdit3.Text:='imu_status';
         end;
      4: begin                                     {Profile: Throttle}
           LabeledEdit1.Text:='CH0';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvAlt;
           LabeledEdit3.Text:=csvVolt;
         end;
      5: begin                                     {Profile: Pitch}
           LabeledEdit1.Text:='CH2';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvPitch;
           LabeledEdit3.Text:=csvTas;
         end;
      6: begin                                     {Profile: Roll}
           LabeledEdit1.Text:='CH1';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvRoll;
           LabeledEdit3.Text:=csvTas;
         end;
      7: begin                                     {Profile: Yaw}
           LabeledEdit1.Text:='CH3';
           LabeledEdit1.Tag:=2;
           LabeledEdit2.Text:=csvYaw;
           LabeledEdit3.Text:='error_flags1';
         end;
      8: begin                                     {Profile: 3Axis}
           LabeledEdit1.Text:=csvPitch;
           LabeledEdit2.Text:=csvRoll;
           LabeledEdit3.Text:=csvYaw;
         end;
    end;                                           {Ende case profiles}
  end;

begin
  if (SpinEdit3.Tag=MQid) or
     (SpinEdit3.Tag=H5id) then
       exit;
  LabeledEdit1.Tag:=0;                             {Tag=0: Telemetry (Default)}
  LabeledEdit2.Tag:=0;
  LabeledEdit3.Tag:=0;
  case SpinEdit3.Tag of
    brID: ProfileBreeze;
    YTHPid: ProfileYTHPlus;
  else
    ProfileYLegacy;
  end;
  PageControl1.ActivePageIndex:=3;                 {Umschalten auf Schnellanalyse}
  Anzeige;                                         {Schnellanalyse ausführen}
  if Timer3.Enabled and
     (idx>0) then begin
    StatusBar1.Panels[5].Text:=rsProfile+suff+ComboBox10.Items[idx];
    SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.ComboBox10DblClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=3;                 {Umschalten auf Schnellanalyse}
  Anzeige;
end;

procedure TForm1.ComboBox2Change(Sender: TObject); {Pfad aufrufen und auswerten}
begin
  SelDirAct('');                                   {Alles neu laden}
end;

procedure TForm1.ComboBox2DblClick(Sender: TObject);
begin                                              {Verzeichnis öffnen}
  OpenDocument(IncludeTrailingPathDelimiter(ComboBox2.Text));
end;

procedure TForm1.ComboBox2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Liste löschen}
begin
  if ssCtrl in Shift then
    ComboBox2.Items.Clear;
end;

procedure TForm1.Edit4DblClick(Sender: TObject);   {CGO3 Test Copy to Clipboard}
begin
  If Edit4.Text>'' then
    ClipBoard.AsText:=Edit3.Text+LineEnding+Edit4.Text;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DefaultFormatSettings.DecimalSeparator:=chr(Tag); {Original wiederherstellen}
//  topp:=nil;
end;

procedure TForm1.FormDblClick(Sender: TObject);    {AboutBox}
begin
  if MessageDlg(capForm1+sLineBreak+ExtractFileName(Application.ExeName)+tab2+version+
                sLineBreak+sLineBreak+meinname+sLineBreak+homepage+sLineBreak+email,
                mtInformation,[mbHelp, mbOK],0)=mrNone then OpenManual;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin                                              {Drop Directory to App Window}
  Application.BringToFront;
  ComboBox2.Text:=GetFlightLogDir(FileNames[0]);
  SelDirAct(ComboBox2.Text);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if PageControl1.ActivePageIndex=2 then begin     {Höhendiagramm}
    if ssCtrl in Shift then begin
      if ChartToolset2DataPointCrosshairTool1.Enabled then begin
        case key of               {Analyse erweitern/einschränken mit +  oder -}
          107, 187: SpinEdit2.Value:=SpinEdit2.Value+1; {+}
          109, 189: SpinEdit2.Value:=SpinEdit2.Value-1; {-}
        end;
        if key=vk_n then
          BitBtn14Click(self);                     {Ausschneiden}
      end;
      if key=vk_c then
        Chart1.CopyToClipboardBitmap;              {Höhenprofil ins Clipboard}
    end;
    if key=vk_ESCAPE then
      KursorAus;                                   {Fadenkreuz aus}
  end;
  if PageControl1.ActivePageIndex=3 then begin     {Schellanalyse}
    if key=vk_ESCAPE then Timer3.Enabled:=false;   {Diashow Profiles stoppen}
    if (key=vk_F5) and
       (SpinEdit3.Tag<>BrID) then
      Timer3.Enabled:=true;                        {Weiter}
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if trim(ParamstrUTF8(1))<>'' then begin          {versuchen, Datei zu öffnen}
    try
      Application.BringToFront;
      ComboBox2.Text:=GetFlightLogDir(ParamstrUTF8(1));
      SelDirAct(ComboBox2.Text);
    except
      SelDirAct('');
    end;
  end else
    SelDirAct('');                                 {Alles neu laden}
end;

procedure TForm1.FormShowHint(Sender: TObject; HintInfo: PHintInfo);
begin

end;

procedure TForm1.Image4Click(Sender: TObject);
begin
  OpenURL(LazURL);
end;

procedure TForm1.Label7Click(Sender: TObject);     {Hilfe aufrufen}
begin
  if OpenManual then
    Label7.Font.Color:=clPurple;
end;

procedure TForm1.Label7MouseEnter(Sender: TObject); {Link animieren}
begin
  Label7.Font.Style:=Label7.Font.Style+[fsBold];
  Label7.Cursor:=crHandPoint;
end;

procedure TForm1.Label7MouseLeave(Sender: TObject); {Link animieren}
begin
  Label7.Font.Style:=Label7.Font.Style-[fsBold];
end;

procedure TForm1.Label8Click(Sender: TObject);     {Download update}
begin                                              {Farbe ändern}
  if OpenURL(homepage+DownURL) then
    Label8.Font.Color:=clPurple;
end;

procedure TForm1.Label8MouseEnter(Sender: TObject); {Link animieren}
begin
  Label8.Font.Style:=Label8.Font.Style+[fsBold];
  Label8.Cursor:=crHandPoint;
end;

procedure TForm1.Label8MouseLeave(Sender: TObject); {Link animieren}
begin
  Label8.Font.Style:=Label8.Font.Style-[fsBold];
end;

procedure TForm1.GetDDdata(lab: TLabeledEdit); {Wert für Schnellanalyse übergeben}
begin
  if TreeView1.Selected<>nil then begin
    ComboBox10.ItemIndex:=0;
    lab.Text:=TreeView1.Selected.Text;
    if TreeView1.Selected.Parent.Text=mndir  then
      lab.Tag:=brID;
    if TreeView1.Selected.Parent.Text=dkpath then
      lab.Tag:=0;
    if TreeView1.Selected.Parent.Text=spath  then
      lab.Tag:=1;
    if TreeView1.Selected.Parent.Text=fpath  then
      lab.Tag:=2;
  end;
end;

procedure TForm1.LabeledEdit1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin                                              {Spalte per D&D auswählen}
  if Source is TTreeView then
    GetDDdata(LabeledEdit1);
end;

procedure TForm1.LabeledEdit1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);         {Drop aus TreeView1}
begin
  Accept:=true;
end;

procedure TForm1.LabeledEdit2DragDrop(Sender, Source: TObject; X, Y: Integer);
begin                                              {Spalte per D&D auswählen}
  if Source is TTreeView then
    GetDDdata(LabeledEdit2);
end;

procedure TForm1.LabeledEdit2DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);         {Drop aus TreeView1}
begin
  Accept:=true;
end;

procedure TForm1.LabeledEdit3DragDrop(Sender, Source: TObject; X, Y: Integer);
begin                                              {Spalte per D&D auswählen}
  if Source is TTreeView then
    GetDDdata(LabeledEdit3);
end;

procedure TForm1.LabeledEdit3DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);         {Drop aus TreeView1}
begin
  Accept:=True;
end;

Procedure TForm1.LoadTree;                         {TreeView für Spalten}
var inlist, splitlist: TStringList;
    fn, hdstr: string;
    newn: TTreeNode;

  procedure addnodes(s: string; n: TTreeNode);     {Überschriften einlesen}
  var x: integer;
  begin
    splitlist.DelimitedText:=s;
    if splitlist.Count>5 then begin
      for x:=1 to splitlist.count-1 do begin
        if pos(lcol, splitlist[x])>0 then begin
          Treeview1.Items.AddChild(n, lcol);
          break;    {letzte Spalte gefunden}
        end;
        Treeview1.Items.AddChild(n, splitlist[x]);
      end;
    end;
  end;

begin
  TreeView1.Items.Clear;
  hdstr:='';
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
        ListBox1.Items[ListBox1.ItemIndex]+bext;   {Breeze}
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, mndir);
      inlist.LoadFromFile(fn);
      if inlist.count>8 then
        addnodes(inlist[8], newn);
      LabeledEdit1.Tag:=brID;                      {hart einstellen}
      LabeledEdit2.Tag:=brID;
      LabeledEdit3.Tag:=brID;
    end;
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
        kfile+ListBox1.Items[ListBox1.ItemIndex]+fext;
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, dkpath);      {create Tree node for Telemetry}
      if (StringGrid1.Tag=17) and
         (SpinEdit3.Tag=1) then begin
        hdstr:=FakeHeader;
        LabeledEdit1.Tag:=0;
        LabeledEdit2.Tag:=0;
        LabeledEdit3.Tag:=0;
      end else begin
        inlist.LoadFromFile(fn);
        if inlist.count>2 then
          hdstr:=inlist[0];
        if LabeledEdit1.Tag=brID then
          LabeledEdit1.Tag:=0;
        if LabeledEdit2.Tag=brID then
          LabeledEdit2.Tag:=0;
        if LabeledEdit3.Tag=brID then
          LabeledEdit3.Tag:=0;
      end;
      if hdstr<>'' then
        addnodes(hdstr, newn);
    end;
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+spath+
                 PathDelim+sfile+ListBox1.Items[ListBox1.ItemIndex]+fext;
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, spath);
      inlist.LoadFromFile(fn);
      if inlist.count>2 then
        addnodes(inlist[0], newn);
    end;
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+fpath+
                 PathDelim+ffile+ListBox1.Items[ListBox1.ItemIndex]+fext;
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, fpath);
      inlist.LoadFromFile(fn);
      if inlist.count>2 then
        addnodes(inlist[0], newn);
    end;
    TreeView1.FullExpand;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.TreeView1Click(Sender: TObject);  {Enable Buttons -> }
begin
  SpeedButton1.Enabled:=(TreeView1.Selected<>nil) and
                        (TreeView1.Selected.Level=1) and
                        (TreeView1.Selected.Text<>LabeledEdit1.Text) and
                        (TreeView1.Selected.Text<>LabeledEdit2.Text) and
                        (TreeView1.Selected.Text<>LabeledEdit3.Text);
  SpeedButton2.Enabled:=(TreeView1.Selected<>nil) and
                        (TreeView1.Selected.Level=1) and
                        (TreeView1.Selected.Text<>LabeledEdit1.Text) and
                        (TreeView1.Selected.Text<>LabeledEdit2.Text) and
                        (TreeView1.Selected.Text<>LabeledEdit3.Text);
  SpeedButton3.Enabled:=(TreeView1.Selected<>nil) and
                        (TreeView1.Selected.Level=1) and
                        (TreeView1.Selected.Text<>LabeledEdit1.Text) and
                        (TreeView1.Selected.Text<>LabeledEdit2.Text) and
                        (TreeView1.Selected.Text<>LabeledEdit3.Text);
end;

procedure TForm1.TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Drag%Drop für Schnellanalyse}
begin
  if button=mbLeft then
    TreeView1.BeginDrag(true);
end;

procedure TForm1.AppLogTimeStamp(s: string);       {AppLog einteilen}
begin
  SynEdit1.Lines.Add(s);
  SynEdit1.Lines.Add(trenner+tab1+FormatDateTime(vzf+zzz, now)+tab1+trenner);
  SynEdit1.Lines.Add('');
end;

procedure TForm1.SetSensorEnv;    {Bedienoberfläche für Sensor Anzeige anpassen}
begin
  Chart1BarSeries1.Clear;
  Chart1BarSeries2.Clear;
  Chart1BarSeries3.Clear;
  Chart1BarSeries4.Clear;
  Chart1BarSeries5.Clear;
  Chart1BarSeries7.Clear;
  PopUpMenu1.Items[0].Enabled:=false;              {GoogleMaps}
  PopUpMenu1.Items[1].Enabled:=false;              {OSM}
  PopUpMenu1.Items[4].Enabled:=false;              {Datenanalyse}
  PopUpMenu1.Items[5].Enabled:=false;              {GoTo #}
  PopUpMenu1.Items[6].Enabled:=false;              {GoTo Errorflags}
  PopUpMenu1.Items[9].Enabled:=false;              {Start}
  PopUpMenu1.Items[10].Enabled:=false;             {Stop}
  MenuItem4.Enabled:= false;                       {HDiagamm Kursor}
  MenuItem12.Enabled:=false;                       {Profiles}
  MenuItem57.Enabled:=false;                       {Diashow}
end;

procedure TForm1.ResetSensorEnv;
begin
  PopUpMenu1.Items[0].Enabled:=true;               {GoogleMaps}
  PopUpMenu1.Items[1].Enabled:=true;               {OSM}
  PopUpMenu1.Items[4].Enabled:=true;               {Datenanalyse}
  PopUpMenu1.Items[5].Enabled:=true;               {GoTo #}
  PopUpMenu1.Items[6].Enabled:=true;               {GoTo Errorflags}
  PopUpMenu1.Items[9].Enabled:=true;               {Start}
  PopUpMenu1.Items[10].Enabled:=true;              {Stop}
  MenuItem4.Enabled:= true;                        {HDiagamm Kursor}
  MenuItem12.Enabled:=true;                        {Profiles}
  MenuItem57.Enabled:=true;                        {Diashow}
end;

procedure TForm1.Anzeige;                          {neu anzeigen}
var x: integer;

  procedure AnzBreeze;
  begin
    RadioGroup1.ItemIndex:=0;
    RadioGroup1.Enabled:=false;                    {nur Telemetrie}
    case PageControl1.ActivePageIndex of
      0: grdOverview.TopRow:=topp[0, 4];           {0 wird sowieso gefüllt}
      1: BrAnzeigeCSV(0);
      2: BrHDiagramm(IncludeTrailingPathDelimiter(ComboBox2.Text)+
                     ListBox1.Items[ListBox1.ItemIndex]+bext);
      3: AnzeigeSchnell;
      6: PlatformLesen;
    end;
  end;

  procedure AnzYLegacy;
  begin
    RadioGroup1.Enabled:=true;                     {Auswahl Remote möglich}
    case PageControl1.ActivePageIndex of
      0: grdOverview.TopRow:=topp[0, 4];           {0 wird sowieso gefüllt}
      1: AnzeigeCSV(0);
      2: HDiagramm(IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
                   kfile+ListBox1.Items[ListBox1.ItemIndex]+fext);
      3: AnzeigeSchnell;
      6: FirmwareLesen;
    end;
  end;

begin
  if (SpinEdit3.Tag=MQid) or                       {nichts tun für MantisQ}
     (SpinEdit3.Tag=H5id) then                     {nichts tun für H520}
       exit;
  AppLogTimeStamp('');
  DefaultFormatSettings.DecimalSeparator:='.';
  if (PageControl1.Tag>0) and
     (PageControl1.ActivePageIndex<>3) then
    try
      Form2.Chart1ConstantLine1.Active:=false;     {nicht bei Schnellanalyse}
    except
    end;
  for x:=1 to StringGrid4.RowCount-1 do            {FW Tabelle löschen}
    StringGrid4.Cells[1, x]:='';
  if ListBox1.Items.Count>0 then begin             {Dateien erkannt}
    ListBox1.Tag:=ListBox1.ItemIndex;              {Dateinummer merken}
    case RadioGroup1.ItemIndex of                  {Voreinstellungen abh. von Quelle}
      0: ResetSensorEnv;                           {Kopter - Telemetrie}
      1: ResetSensorEnv;                           {ST10 - RemoteGPS}
      2: begin                                     {Funk - Remote}
           ResetSensorEnv;
           PopUpMenu1.Items[0].Enabled:=false;     {GoogleMaps}
           PopUpMenu1.Items[1].Enabled:=false;     {OSM}
         end;
      3: SetSensorEnv;                             {Sensor files}
    end;
    case SpinEdit3.Tag of
      brID: AnzBreeze;                             {Breeze}
    else
      AnzYLegacy;                                  {Rest der Yuneec Welt}
    end;
    ComboBox10.Enabled:=RadioGroup1.Enabled;       {Profiles nicht bei Breeze}
    MenuItem33.Enabled:=RadioGroup1.Enabled;       {Menüs abh. von Type}
    MenuItem34.Enabled:=RadioGroup1.Enabled;
    MenuItem12.Enabled:=RadioGroup1.Enabled;       {Menü Profiles nicht für Breeze}
    StatusBar1.Tag:=0;                             {Zeiten nicht mit kopieren}
    LoadTree;                                      {Spaltenliste aktualisieren}
  end;                                             {Ende keine Dateien}
end;

procedure TForm1.ShowMQ;                           {Anzeige SensorFile Mantis Q}
var fn: string;
begin
  if ListBox1.Tag<>ListBox1.ItemIndex then begin   {nur neue Datei}
    PopUpMenu1.Items[0].Enabled:=false;            {GoogleMaps}
    PopUpMenu1.Items[1].Enabled:=false;            {OSM}
    PopUpMenu1.Items[4].Enabled:=false;            {Datenanalyse}
    PopUpMenu1.Items[5].Enabled:=false;            {GoTo #}
    PopUpMenu1.Items[6].Enabled:=false;            {GoTo Errorflags}
    PopUpMenu1.Items[9].Enabled:=false;            {Start}
    PopUpMenu1.Items[10].Enabled:=false;           {Stop}
    PageControl1.ActivePageIndex:=1;               {Detail}
    RadioGroup1.ItemIndex:=3;                      {Sensor Files}
    RadioGroup1.Enabled:=false;
    ListBox1.Tag:=ListBox1.ItemIndex;              {Dateinummer merken}
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
        nfile+ListBox1.Items[ListBox1.ItemIndex]+wext;
    if Fileexists(fn) then begin
      StatusBar1.Panels[5].Text:=fn;
      ShowSensorPlus(fn, ListBox1.ItemIndex, CheckBox10.Checked, true, false);
    end else begin                                 {alternativ yuneec_*.log file}
      fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
          mfile+ListBox1.Items[ListBox1.ItemIndex]+bext;
      if Fileexists(fn) then begin
        StatusBar1.Panels[5].Text:=fn;
        ShowSensorPlus(fn, ListBox1.ItemIndex, CheckBox10.Checked, true, false);
      end;
    end;
  end;
end;

procedure TForm1.ShowH520;                         {Anzeige TLOG File H520}
var fn: string;
begin
  if ListBox1.Tag<>ListBox1.ItemIndex then begin   {nur neue Datei}
    PopUpMenu1.Items[0].Enabled:=false;            {GoogleMaps}
    PopUpMenu1.Items[1].Enabled:=false;            {OSM}
    PopUpMenu1.Items[4].Enabled:=false;            {Datenanalyse}
    PopUpMenu1.Items[5].Enabled:=false;            {GoTo #}
    PopUpMenu1.Items[6].Enabled:=false;            {GoTo Errorflags}
    PopUpMenu1.Items[9].Enabled:=false;            {Start}
    PopUpMenu1.Items[10].Enabled:=false;           {Stop}
    PageControl1.ActivePageIndex:=1;               {Detail}
    RadioGroup1.ItemIndex:=3;                      {Sensor Files}
    RadioGroup1.Enabled:=false;
    ListBox1.Tag:=ListBox1.ItemIndex;              {Dateinummer Index merken}
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
        ListBox1.Items[ListBox1.ItemIndex]+hext;   {Dateinamenstamm+.tlog}
    if Fileexists(fn) then begin
      StatusBar1.Panels[5].Text:=fn;
      ShowSensorPlus(fn, ListBox1.ItemIndex, CheckBox10.Checked, true, false);
    end;
  end;
end;

procedure TForm1.ListBox1Click(Sender: TObject);   {Anzeige wenn Dateiwechsel}
begin
  if (ListBox1.Items.Count>0) and
     (ListBox1.ItemIndex<>ListBox1.Tag) then begin {nur bei neuer Datei}
    Timer2.Enabled:=false;
    Timer3.Enabled:=false;                         {Stop Diashow Profiles}
    if Form2<>nil then
      Form2.Close;                                 {Detailfenster schließen}
    cutb:=0;                                       {Zeitstempel zum Ausschneiden}
    cute:=0;                                       {löschen}
    tpos:=0;                                       {Position zurücksetzen}
    case SpinEdit3.Tag of
      MQid: ShowMQ;                                {Sensor_*.txt vom Mantis Q}
      H5id: ShowH520;                              {*.tlog vom H520}
      else Anzeige;                                {alle anderen herkömmlich}
    end;
  end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject); {Re-read the Filelist}
begin
  SelDirAct('');
end;

procedure TForm1.Memo1Change(Sender: TObject);     {Änderungen übernehmen}
begin
  ComboBox1.Items.Assign(Memo1.Lines);
end;

procedure TForm1.MenuItem10Click(Sender: TObject); {Menü: GoTo Settings}
begin
  PageControl1.ActivePageIndex:=6;
  PageControl2.ActivePageIndex:=1;
end;

procedure TForm1.MenuItem11Click(Sender: TObject); {Menü: Default}
begin
  Timer3.Enabled:=false;
  if (SpinEdit3.Tag=MQid) or                       {nichts tun für MantisQ}
     (SpinEdit3.Tag=H5id) then                     {nichts tun für H520}
       exit;
  SetProfile(0);
  ComboBox10.ItemIndex:=0;                         {Profiles rücksetzen}
end;

procedure TForm1.MenuItem13Click(Sender: TObject); {Menü Profile FlightModes}
begin
  Timer3.Enabled:=false;
  SetProfile(1);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.FreigabeCut(a: boolean);          {Freigabe und Anzeige Cut}
var ts: TDateTime;
begin
  BitBtn14.Enabled:=false;                         {Default: nicht gültig}
  Label13.Caption:=capLabel13;
  Label14.Caption:=capLabel14;
  Label15.Caption:='';                             {Dauer leer}
  if (cutb>0) and
     (cute>0) and
     (cutb>cute) then begin
    ts:=cute;                                      {Zeitstempel austauschen}
    cute:=cutb;
    cutb:=ts;
  end;
  if cutb>0 then
    Label13.Caption:=FormatDateTime(vzf, cutb);
  if cute>0 then
    Label14.Caption:=FormatDateTime(vzf, cute);
  if (cutb>0) and
     (cute>cutb) then begin                        {Dauer anzeigen}
    BitBtn14.Enabled:=true;
    Label15.Caption:=rsDauer+tab1+FormatDateTime('= nn:ss'+zzz, cute-cutb);
    if a then begin
      StatusBar1.Panels[5].Text:=Label15.Caption;  {Textfeld überschreiben}
      SynEdit1.Lines.Add(capBitBtn14+tab1+StatusBar1.Panels[5].Text);
    end;
  end else
    StatusBar1.Tag:=0;                             {nicht kopieren}
  if a then begin                                  {Ausgabe in Statuszeile erlauben}
    StatusBar1.Panels[3].Text:=Label13.Caption;
    StatusBar1.Panels[4].Text:=Label14.Caption;
  end;
  MenuItem35.Enabled:=BitBtn14.Enabled;
  MenuItem54.Enabled:=BitBtn14.Enabled;
  StatusBar1.Tag:=0;                               {nicht kopieren}
end;

procedure TForm1.MenuItem16Click(Sender: TObject); {Reset Beginn/Ende}
begin
  cutb:=0;
  cute:=0;
  FreigabeCut(true);
end;

procedure TForm1.MenuItem17Click(Sender: TObject); {Startpunkt setzen}
begin
  SetStartP;
end;

procedure TForm1.SetStartP;                        {Startpunkt setzen}
begin
  try
    cutb:=ZeitToDT(StringGrid1.Cells[0, SpinEdit3.Value], SpinEdit3.Tag);
  except
    cutb:=0;
  end;
  FreigabeCut(true);
end;

procedure TForm1.MenuItem18Click(Sender: TObject); {Endpunkt setzen}
begin
  SetEndP;
end;

procedure TForm1.SetEndP;                          {Endpunkt setzen}
begin
  try
    cute:=ZeitToDT(StringGrid1.Cells[0, SpinEdit3.Value], SpinEdit3.Tag);
  except
    cute:=0;
  end;
  FreigabeCut(true);
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin                                              {Tabs umschalten}
  if StringGrid1.ColCount>=csvanz then             {nichts tun bei Anzeige Sensor}
    exit;
  Timer3.Enabled:=false;                           {Diashow Profiles stoppen}
  GroupBox11.Tag:=0;                               {Suche zurücksetzen}
  ProgressBar1.Position:=0;
  ComboBox10.ItemIndex:=0;                         {Profiles zurücksetzen}
  RadioGroup10.Enabled:=CheckBox1.Checked;
  ComboBox9.Enabled:=false;
  case PageControl1.ActivePageIndex of     {alle anderen Fälle ohne Änderung}
    1: begin                                       {Datentabelle}
         ComboBox9.Enabled:=true;                  {nur bei Datentabelle}
         if StringGrid1.ColCount<YTHPcols then begin
           grdOverview.Tag:=1;
           Anzeige;
         end;
       end;
    2: begin
         grdOverview.Tag:=2;                       {Höhendiagramm}
         Anzeige;
       end;
    3: begin
         grdOverview.Tag:=3;                       {Schnellanalyse}
         Anzeige;
       end;
    4: ComboBox9.Enabled:=true;                    {Scan, Suche erlauben}
    6: Anzeige;                                    {FW anzeigen wenn vorhanden}
  end;                                             {Tabs merken}
end;

procedure TForm1.Werte(z: integer); {min/max Werte suchen, z: Index der Datei}
var n: integer=0;                   {Anzahl Werte in richtigen Flugmodi}
    g: integer=0;                   {Anzahl Werte mit GPS}
    e: word;
    inlist, splitlist: TStringList;
    bg, bg1, ed, flt: TDateTime;
    hmax, h, u, umin, umax, tas, tas1, tasmax, hmaxg, uming, umaxg, tasmaxg: double;
    fn, slat: string;
    dist, ddist, lat1, lat2, lat3, lon1, lon2, lon3, emax, strecke: double;
    uw1, uw2, vld, nflg, simu: boolean;

const bgid=999999;

   procedure WerteBreeze;
   var x: integer;
   begin
     fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
         ListBox1.Items[z]+bext;
     try
       inlist.LoadFromFile(fn);
     except
       StatusBar1.Panels[5].Text:=fn+nixda;
       SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
     end;
     vld:=(pos(brsnid, inlist[5])>0) and (pos(brfmode, inlist[8])>0);
     StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
     for x:=9 to inlist.count-1 do begin          {Daten einlesen}
//        for x:=16 to inlist.count-1 do begin          {Daten einlesen MQcsvID}
       splitlist.DelimitedText:=inlist[x];
       if (splitlist.Count>anzsp) then begin      {Konsistenz checken (Breeze)}
         h:=StrToFloatN(splitlist[10])/100;       {Altitude}
         if testh(h) then begin
           u:=BrUmrech(StrToFloatN(splitlist[21])); {Voltage}
           if (u>umax) and
              (u<110) then
             umax:=u;
           e:=StrToIntDef(trim(splitlist[19]), 0);{Errorflag}
           topp[z, 6]:=topp[z, 6] or e;           {ID für Zeile einfärben e-flags}
           if (e and 1)>0 then
             uw1:=true;
           if (e and 2)>0 then
             uw2:=true;
           if (e and 4)>0 then                    {Motor failsave}
             topp[z, 6]:=topp[z, 6] or 256;       {ID für Emergency}
           if (trim(splitlist[14])<>'0') then begin    {reale Flüge}
             if not nflg then begin
               if bg1<bgid then flt:=flt+ed-bg1;  {Rest noch aufaddieren}
               bg1:=bgid;                         {bg zurücksetzen bei Lücken}
             end;
             inc(n);
             nflg:=true;
             ed:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
             if bg>ed then
               bg:=ed;                            {Beginnzeit ohne GPS}
             if bg1>ed then
               bg1:=ed;                           {Teil-Beginnzeit ohne GPS}
             if h>hmax then
               hmax:=h;
             if (u<umin) and
                 (u>0) then
                umin:=u;
             if NichtLeer(splitlist[12]) and
                NichtLeer(splitlist[13]) then begin
               inc(g);
               if (slat='') then begin
                 slat:=splitlist[12];             {Homepoint speichern}
                 lat1:=BrCoordToFloat(slat);
                 lon1:=BrCoordToFloat(splitlist[13]);
                 lat3:=lat1;
                 lon3:=lon1;
               end;
               if h>hmaxg then
                 hmaxg:=h;
               if (u>umaxg) and
                  (u<110) then
                 umaxg:=u;
               if (u<uming) and
                  (u>0) then
                 uming:=u;
               if slat<>'' then begin             {Startpunkt mit GPS}
                 lat2:=BrCoordToFloat(splitlist[12]);
                 lon2:=BrCoordToFloat(splitlist[13]);
                 dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                 if dist>emax then emax:=dist;
                 ddist:=DeltaKoord(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                 strecke:=strecke+ddist;          {Strecke aufaddieren}
                 lat3:=lat2;                      {letzten Punkt speichern}
                 lon3:=lon2;
               end;
             end;                                 {Ende mit GPS Daten}
           end else
             nflg:=false;
         end;                                     {Ende realer Flug}
       end else begin
         StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
         SynEdit1.Lines.Add('''6880'+suff+StatusBar1.Panels[5].Text);
       end;
     end;
     flt:=flt+ed-bg1;
     splitlist.DelimitedText:=inlist[inlist.count-1];
     tend:=ZeitToDT(splitlist[0], SpinEdit3.Tag); {letzten Zeitstempel merken}
   end;

   procedure WerteYLegacy;
   var x: integer;
   begin
     fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
         kfile+ListBox1.Items[z]+fext;
     try
       inlist.LoadFromFile(fn);
     except
       StatusBar1.Panels[5].Text:=fn+nixda;
       SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
     end;
     if inlist.count>2 then begin                 {Überschrift und mind. 2 Zeile}
       StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
       splitlist.DelimitedText:=inlist[0]; {Überschrift einlesen, f_mode ermitteln}
       if (splitlist.Count>anzsp) then begin      {genug Spalten in Überschrift}
         vld:=fModeFinden(splitlist);             {Position f-mode merken}
         if SpinEdit3.Tag<>YTHPid then begin      {YTH Plus nicht überschreiben}
           splitlist.DelimitedText:=inlist[2];    {2. Datenzeile, v_type ermitteln}
           SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
           OverWriteVT;                           {Overwrite for PX4 Thunderbird}
         end;
         for x:=1 to inlist.Count-1 do begin
           if CheckE7(inlist[x]) then begin
             splitlist.DelimitedText:=inlist[x];
             if (splitlist.Count>anzsp) and       {Konsistenz Daten checken}
                 CheckVT(splitlist[StringGrid1.Tag+2],
                         splitlist[StringGrid1.Tag]) then begin   {YTH Plus sinnvoll}
               h:=StrToFloatN(splitlist[4]);      {Altitude}
               if testh(h) then begin
                 tas:=StrToFloatN(splitlist[7]);  {True Air Speed in m/s}
                 if (StrToFloatN(splitlist[3])>0) and               {Simulatorflug}
                    (splitlist[15]='231') then
                   simu:=true;
                 u:=StrToFloatN(splitlist[2]);    {Voltage}
                 if (u>umax) and
                    (u<200) then
                   umax:=u;
                 if GetRFM(splitlist[StringGrid1.Tag],  {Flight Mode}
                           SpinEdit3.Tag,         {Vehicle Type}
                           InFlight(h, tas1, tas)) then begin
                   if not nflg then begin         {nur reale Flüge}
                     if bg1<bgid then
                       flt:=flt+ed-bg1;
                     bg1:=bgid;
                   end;
                   inc(n);                        {Anzahl Datensätze real}
                   nflg:=true;

                   if SpinEdit3.Tag=YTHPid then   {YTH Plus vorerst Error Flag ausblenden}
                     e:=0
                   else
                     e:=StrToIntDef(splitlist[StringGrid1.Tag+3], 0); {Errorflag}

                   topp[z, 6]:=topp[z, 6] or e;   {ID für Zeile einfärben e-flags}
                   if (e and 1)>0 then
                     uw1:=true;
                   if (e and 2)>0 then
                     uw2:=true;
                   if SpinEdit3.Tag=3 then begin  {Blade 350QX}
                     if splitlist[StringGrid1.Tag]='8' then
                       topp[z, 6]:=topp[z, 6] or 256; {ID für Emergency}

                   end else
                     if SpinEdit3.Tag=YTHPid then begin {YTH Plus}
                     //   Emergency unknown for YTH Plus --> nichts tun

                   end else begin                 {Alle anderen sollten 12 haben}
                     if splitlist[StringGrid1.Tag]='12' then
                       topp[z, 6]:=topp[z, 6] or 256; {ID für Emergency}
                   end;
                   ed:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
                   if bg>ed then
                     bg:=ed;                      {Beginnzeit ohne GPS}
                   if bg1>ed then
                     bg1:=ed;                     {Teil-Beginnzeit ohne GPS}
                   if h>hmax then
                     hmax:=h;
                   if tas>tasmax then
                     tasmax:=tas;
                   if (u<umin) and
                      (u>1) then
                     umin:=u;
                   if NichtLeer(splitlist[5]) and
                      NichtLeer(splitlist[6]) then begin  {GPS Koordinaten vorh.}
                     inc(g);
                     if (slat='') and
                        (lowercase(splitlist[8])=idtrue) then begin
                       slat:=splitlist[5];        {Homepoint speichern}
                       lat1:=StrToFloatN(slat);
                       lon1:=StrToFloatN(splitlist[6]);
                       lat3:=lat1;
                       lon3:=lon1;
                     end;
                     if h>hmaxg then
                       hmaxg:=h;
                     if tas>tasmaxg then
                       tasmaxg:=tas;
                     if (u>umaxg) and
                        (u<200) then
                       umaxg:=u;
                     if (u<uming) and
                        (u>1) then
                       uming:=u;
                     if slat<>'' then begin       {Startpunkt mit GPS}
                       lat2:=StrToFloatN(splitlist[5]);
                       lon2:=StrToFloatN(splitlist[6]);
                       dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                       ddist:=DeltaKoord(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                       if dist>emax then          {größte Entfernung zum Start}
                         emax:=dist;
                       strecke:=strecke+ddist;    {Strecke aufaddieren}
                       lat3:=lat2;                {letzten Punkt speichern}
                       lon3:=lon2;
                     end;
                   end;                           {Ende mit GPS Daten}
                 end else
                   nflg:=false;                   {Ende realer Flug}
                 tas1:=tas;                       {letzte tas merken für Glättung}
               end;
             end;                                 {Ende Konsistenz und CheckVT}
           end;                                   {Ende CheckE7}
         end;                                     {Ende Einlesen}
         flt:=flt+ed-bg1;
         splitlist.DelimitedText:=inlist[inlist.count-1];
         tend:=ZeitToDT(splitlist[0], SpinEdit3.Tag); {letzten Zeitstempel merken}
       end else begin
         StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;    {Ende Konsistenz checken}
         SynEdit1.Lines.Add('''7013'+suff+StatusBar1.Panels[5].Text);
       end;
     end;
   end;

begin
  if (SpinEdit3.Tag=H5id) or
     (SpinEdit3.Tag=MQid) then
    exit;                                          {nichts tun bei Anzeige PX4}
  screen.Cursor:=crHourGlass;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  if cbThunder.Checked then
    StaticText1.Caption:=capThunder;               {Type hard wired}
  hmax:=0;
  umax:=0;
  umin:=999;
  tasmax:=0;
  tasmaxg:=0;
  tas1:=0;
  hmaxg:=0;
  umaxg:=0;
  uming:=999;
  bg:=bgid;
  bg1:=bgid;
  slat:='';
  emax:=0;
  strecke:=0;
  uw1:=false;                                      {Unterspannung 1}
  uw2:=false;                                      {Unterspannung 2}
  vld:=false;                                      {Auswertung gültig}
  nflg:=true;
  simu:=false;
  flt:=0;                                          {reale Flugzeit}
  ed:=0;                                           {Zeitstempel -> letzte ist Ende}
  try
    if SpinEdit3.Tag=brID then begin               {Breeze}
      WerteBreeze;
    end else begin                                 {Rest der Yuneec Welt}
      WerteYLegacy;
    end;
     if vld then begin                             {Anzeige gültiger Auswertung}
      grdOverview.BeginUpdate;
        flt:=round(flt*secpd)/secpd;        {Runden um Anzeigefehler zu vermeiden}
        if (g>3) and                {alles ausgeben wenn GPS-Daten vorhanden sind}
           ((SpinEdit3.Tag<>YTHPid) or             {Mindestflugzeit nur beim YTH Plus}
            (CheckBox9.Checked=false) or           {wenn Bereinigung eingestellt ist}
            (flt>minflt)) then begin               {Anzeige gültiger Auswertung}
          BitBtn25.Tag:=BitBtn25.Tag+1;
          grdOverview.Cells[1,z+1]:=FormatDateTime(dzf, bg);
          grdOverview.Cells[2,z+1]:=FormatDateTime(zzf, bg);
          grdOverview.Cells[3,z+1]:=FormatDateTime(zzf, ed);
          grdOverview.Cells[4,z+1]:=FormatDateTime('nn:ss', flt);     {Flugzeit}
          if RadioGroup3.ItemIndex=2 then begin
            grdOverview.Cells[5,z+1]:=FloatToStrF(hmaxg/fft, ffFixed, 4, 1)+'ft';
            grdOverview.Cells[6,z+1]:=FloatToStrF(emax/fft, ffFixed, 5, 1)+'ft';
            grdOverview.Cells[7,z+1]:=FloatToStrF(strecke/fft, ffFixed, 5, 1)+'ft';
            grdOverview.Cells[8,z+1]:=FloatToStrF(tasmaxg*fmph, ffFixed, 4, 1)+'mph'
          end else begin
            grdOverview.Cells[5,z+1]:=FloatToStrF(hmaxg, ffFixed, 4, 1)+'m';
            grdOverview.Cells[6,z+1]:=FloatToStrF(emax, ffFixed, 5, 1)+'m';
            grdOverview.Cells[7,z+1]:=FloatToStrF(strecke, ffFixed, 5, 1)+'m';
            grdOverview.Cells[8,z+1]:=FloatToStrF(tasmaxg*fkmh, ffFixed, 4, 1)+'km/h';
          end;
          if SpinEdit3.Tag=brID then begin
            grdOverview.Cells[8,z+1]:='';          {Breeze keine Speed}
            grdOverview.Cells[9,z+1]:=IntToStr(round(umaxg))+'%';
            grdOverview.Cells[10,z+1]:=IntToStr(round(uming))+'%';
          end else begin
            grdOverview.Cells[9,z+1]:=FloatToStrF(umaxg, ffFixed, 3, 1)+'V';
            grdOverview.Cells[10,z+1]:=FloatToStrF(uming, ffFixed, 3, 1)+'V';
          end;
        end else begin                             {reduzierte Ausgabe}
          if (n>3) and                             {Ausgabe für Flüge ohne GPS}
             ((SpinEdit3.Tag<>YTHPid) or           {Mindestflugzeit nur beim YTH Plus}
              (CheckBox9.Checked=false) or         {wenn Bereinigung eingestellt ist}
              (flt>minflt)) then begin             {Anzeige gültiger Auswertung}
            BitBtn25.Tag:=BitBtn25.Tag+1;
            grdOverview.Cells[1,z+1]:=FormatDateTime(dzf, bg);
            grdOverview.Cells[2,z+1]:=FormatDateTime(zzf, bg);
            grdOverview.Cells[3,z+1]:=FormatDateTime(zzf, ed);
            grdOverview.Cells[4,z+1]:=FormatDateTime('nn:ss', flt);
            if RadioGroup3.ItemIndex=2 then begin
              grdOverview.Cells[5,z+1]:=FloatToStrF(hmax/fft, ffFixed, 4, 1)+'ft';
              grdOverview.Cells[8,z+1]:=FloatToStrF(tasmax*fmph, ffFixed, 4, 1)+'mph'
            end else begin
              grdOverview.Cells[5,z+1]:=FloatToStrF(hmax, ffFixed, 4, 1)+'m';
              grdOverview.Cells[8,z+1]:=FloatToStrF(tasmax*fkmh, ffFixed, 4, 1)+'km/h';
            end;
            if SpinEdit3.Tag=brID then begin
              grdOverview.Cells[8,z+1]:='';        {Breeze keine Speed}
              grdOverview.Cells[9,z+1]:=IntToStr(round(umax))+'%';
              grdOverview.Cells[10,z+1]:=IntToStr(round(umin))+'%';
            end else begin
              grdOverview.Cells[9,z+1]:=FloatToStrF(umax, ffFixed, 3, 1)+'V';
              grdOverview.Cells[10,z+1]:=FloatToStrF(umin, ffFixed, 3, 1)+'V';
            end;
          end else begin                           {Ausgabe sonstige (ohne Flug)}
            if SpinEdit3.Tag=brID then
              splitlist.DelimitedText:=inlist[9]
            else
              splitlist.DelimitedText:=inlist[1];
            bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);   {Fake Beginnzeit}
            grdOverview.Cells[1,z+1]:=FormatDateTime(dzf, bg);
            grdOverview.Cells[2,z+1]:=FormatDateTime(zzf, bg);
            grdOverview.Cells[3,z+1]:=FormatDateTime(zzf, tend);
            grdOverview.Cells[4,z+1]:=FormatDateTime('nn:ss',
                                      round((tend-bg)*secpd)/secpd);
            if SpinEdit3.Tag=brID then
              grdOverview.Cells[9,z+1]:=IntToStr(round(umax))+'%'
            else
              grdOverview.Cells[9,z+1]:=FloatToStrF(umax, ffFixed, 3, 1)+'V';
          end;
        end;
      grdOverview.Cells[1, grdOverview.RowCount-1]:=rsTurns+suff+
                                                    IntToStr(BitBtn25.Tag);
      if simu then
        grdOverview.Cells[6,z+1]:=rsSimulator;
      if uw1 then
        grdOverview.Cells[10,z+1]:=grdOverview.Cells[10,z+1]+' !';
      if uw2 then
        grdOverview.Cells[10,z+1]:=grdOverview.Cells[10,z+1]+'!';

      grdOverview.EndUpdate;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.Analyse;              {Datenanalyse ausgewählter Bereiche}
var inlist, splitlist: TStringList;
    x, y, n: integer;
    s, an: string;
    ha, he: double;
    dur, bg: TDateTime;
    dist, lat1, lat2, lon1, lon2, emax: double;

  procedure AnalyseBreeze;
  var i: integer;
  begin
    ha:=StrToFloatN(splitlist[10])/100;          {Höhe Anfang}
    lat1:=BrCoordToFloat(splitlist[12]);
    lon1:=BrCoordToFloat(splitlist[13]);
    splitlist.DelimitedText:=inlist[inlist.Count-1];
    he:=StrToFloatN(splitlist[10])/100;          {Höhe Ende}
    emax:=0;
    if ha>he+hsw then begin                      {Sinken}
      an:=an+rsDescend+tab1+vms(dur, ha-he)+kma;
    end;
    if he>ha+hsw then begin                      {Steigen}
      an:=an+rsAscend+tab1+vms(dur, he-ha)+kma;
    end;
    for i:=0 to inlist.Count-1 do begin
      splitlist.DelimitedText:=inlist[i];
      lat2:=BrCoordToFloat(splitlist[12]);
      lon2:=BrCoordToFloat(splitlist[13]);
      dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum 1. Punkt}
      if dist>emax then
        emax:=dist;
    end;
  end;

  procedure AnalyseYLegacy;
  var i: integer;
  begin
    ha:=StrToFloatN(splitlist[4]);               {Höhe Anfang}
    lat1:=StrToFloatN(splitlist[5]);
    lon1:=StrToFloatN(splitlist[6]);
    splitlist.DelimitedText:=inlist[inlist.Count-1];
    he:=StrToFloatN(splitlist[4]);               {Höhe Ende}
    emax:=0;
    if ha>he+hsw then begin                      {Sinken}
      an:=an+rsDescend+tab1+vms(dur, ha-he)+kma;
    end;
    if he>ha+hsw then begin                      {Steigen}
      an:=an+rsAscend+tab1+vms(dur, he-ha)+kma;
    end;
    for i:=0 to inlist.Count-1 do begin
      splitlist.DelimitedText:=inlist[i];
      lat2:=StrToFloatN(splitlist[5]);
      lon2:=StrToFloatN(splitlist[6]);
      dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
      if dist>emax then
        emax:=dist;
    end;
  end;

  procedure anatelemetr;   {Telemetrie auswerten, Sinken, Steigen, Geschw.}
  begin
    splitlist.DelimitedText:=inlist[0];
    if SpinEdit3.Tag=brID then begin               {Breeze}
      AnalyseBreeze;
    end else begin                                 {Rest der Yuneec Welt}
      AnalyseYLegacy;
    end;
    an:=an+rsGridCell6+tab1+FloatToStrF(emax, ffFixed, 5, 1)+'m, '+
           rsSpeed+tab1+tab1+vms(dur, emax);
  end;

  procedure anast10;                               {ST10 Daten auswerten}
  var i: integer;
  begin
    splitlist.DelimitedText:=inlist[0];
    lat1:=StrToFloatN(splitlist[2]);
    lon1:=StrToFloatN(splitlist[1]);
    ha:=StrToFloatN(splitlist[3]);                 {Höhe Anfang}
    emax:=0;
    for i:=0 to inlist.Count-1 do begin
      splitlist.DelimitedText:=inlist[i];
      lat2:=StrToFloatN(splitlist[2]);
      lon2:=StrToFloatN(splitlist[1]);
      dist:=DeltaKoord(lat1, lon1, lat2, lon2);    {Entfernung zum 1. Punkt}
      if dist>emax then
        emax:=dist;
    end;
    an:=an+rsGPSh+tab1+FloatToStrF(ha, ffFixed, 5, 1)+'m, '+
           rsGridCell6+tab1+FloatToStrF(emax, ffFixed, 8, 1)+'m, '+
           rsSpeed+tab1+tab1+vms(dur, emax);
  end;

  procedure anafunk;                               {Stickbewegungen}
  var i, k, w, max, min: integer;
  begin
    splitlist.DelimitedText:=inlist[0];            {Flightmode Schalter lesen}
    an:=an+SwitchToStr(5, splitlist[5])+tab2;
    for i:=1 to 4 do begin                         {für jeden Stick}
      max:=0;
      min:=10000;
      for k:=0 to inlist.count-1 do begin          {Ausschnitt durchsuchen}
        splitlist.DelimitedText:=inlist[k];
        try
          w:=Round(StrToFloatN(splitlist[i]));
        except
          w:=stkntrl;
        end;
        if w>max then
          max:=w;
        if w<min then
          min:=w;
      end;
      an:=an+ChToStr('', i)+suff;
      if StickPos(max)=StickPos(min) then
        an:=an+StickPos(max)+tab2
      else
        an:=an+'min='+StickPos(min)+'/'+'max='+StickPos(max)+tab2;
    end;
  end;

begin                                              {Datenanalyse ausgewählter Bereiche}
  try
    inlist:=TStringList.Create;
    splitlist:=TStringList.Create;
    splitlist.Delimiter:=sep;
    splitlist.StrictDelimiter:=True;
    Screen.Cursor:=crHourGlass;
    an:='';
    try
      n:=SpinEdit3.Value+SpinEdit2.Value+1;
      if n>StringGrid1.RowCount-1 then
        n:=StringGrid1.RowCount-1;                 {Ende der Datei}
      SynEdit1.Lines.Add(capTabSheet10+tab1+rsFor+tab1+
                         IntToStr(SpinEdit3.Value)+'-'+      {von}
                         IntToStr(n));             {bis}
      for x:=SpinEdit3.Value to n do begin         {Liste zur Auswertung füllen}
        s:=StringGrid1.Cells[0, x];
        for y:=1 to StringGrid1.ColCount-1 do
          s:=s+sep+StringGrid1.Cells[y, x];
        inlist.add(s);
      end;
      if inlist.Count>2 then begin
        splitlist.DelimitedText:=inlist[0];
        bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag); {Beginnzeitpunkt}
        if (cutb=0) or (bg<cutb) then
          cutb:=bg                                 {Beginn setzen}
        else
          if bg>cutb then cute:=bg;                {Ende setzen}
        FreigabeCut(false);                        {Status nicht überschreiben}
        splitlist.DelimitedText:=inlist[inlist.Count-1];
        dur:=ZeitToDT(splitlist[0], SpinEdit3.Tag)-bg;
        case RadioGroup1.ItemIndex of
          0: anatelemetr;
          1: anast10;
          2: anafunk;
        end;
        StatusBar1.Panels[5].Text:=capTabSheet10+tab1+rsFor+tab1+
                                   NumSec(dur)+'s'+suff+an;
        SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      StatusBar1.Tag:=1;                           {Zeiten zum Kopieren freigeben}
    finally                                        {alles wieder freigeben}
      FreeAndNil(inlist);
      FreeAndNil(splitlist);
      Screen.Cursor:=crDefault;
    end;
  except
    StatusBar1.Panels[5].Text:=capTabSheet10+suff+rsError;
    SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.HeaderST24;                       {Write Header for H920 + ST24}
begin
  StaticText1.Caption:='H920 + ST24';
  StringGrid1.ColCount:=22;
  StringGrid1.Cells[1,0]:=rsHDcell1;
  StringGrid1.Cells[2,0]:=rsHDcell2;
  StringGrid1.Cells[3,0]:=rsHDcell3;
  StringGrid1.Cells[4,0]:=rsHDcell4;
  StringGrid1.Cells[5,0]:=rsHDcell5;
  StringGrid1.Cells[6,0]:=rsHDcell6;
  StringGrid1.Cells[7,0]:=rsHDcell7;
  StringGrid1.Cells[8,0]:=rsHDcell8;
  StringGrid1.Cells[9,0]:=rsHDcell9;
  StringGrid1.Cells[10,0]:=rsHDcell10;
  StringGrid1.Cells[11,0]:=rsHDcell11;
  StringGrid1.Cells[12,0]:=rsHDcell12;
  StringGrid1.Cells[13,0]:=rsHDcell13;
  StringGrid1.Cells[14,0]:=rsHDcell14;
  StringGrid1.Cells[15,0]:=rsHDcell15;
  StringGrid1.Cells[16,0]:=rsHDcell16;
  StringGrid1.Cells[17,0]:=rsHDcell17;
  StringGrid1.Cells[18,0]:=rsHDcell18;
  StringGrid1.Cells[19,0]:=rsHDcell19;
  StringGrid1.Cells[20,0]:=rsHDcell20;
  StringGrid1.Cells[21,0]:=rsHDcell21;
  StringGrid1.AutoSizeColumns;
end;

procedure TForm1.AnzeigeCSV(const mode: integer);  {Dateien als Tabelle anzeigen}
var i, x, p, n, zhl: integer;
    inlist, splitlist: TStringList;
    fn, slat, slon, sstr: string;
    tpos1: TDateTime;
    rs: boolean;

begin
  n:=Label3.Tag;                        {zwischenspeichern, wird sonst zerstört}
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des StringGrid1 überschrieben}
  screen.Cursor:=crHourGlass;
  MenuItem7.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  StringGrid1.ColCount:=0;                         {alles löschen}
  StringGrid1.RowCount:=14;
  StringGrid1.ColCount:=defaultcol;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  rs:=false;
  slat:='';
  zhl:=0;
  try
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text);
    TreeView1.Items.Clear;
    Label3.Tag:=n;                                 {Spalte retten für ReadMAV}
    case RadioGroup1.ItemIndex of                  {Dateityp}
      0: fn:=fn+kpath+kfile;                       {Kopter = telemetry_}
      1: fn:=fn+spath+PathDelim+sfile;             {ST10 = RemoteGPS_}
      2: fn:=fn+fpath+PathDelim+ffile;             {Funk = Remote_}
      3: begin                                     {Sensor_}
           ReadMav(mode);
           exit;
         end;
    end;
    fn:=fn+ListBox1.Items[ListBox1.ItemIndex]+fext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>1 then begin
      StatusBar1.Panels[5].Text:=fn;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end else begin
      StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then begin
      try
        splitlist.DelimitedText:=inlist[0];        {Überschrift einlesen}
        if RadioGroup1.ItemIndex=0 then begin      {nur bei Telemetrie}
          topp[ListBox1.ItemIndex, 5]:=0;          {Pointer Null setzen}
          fModeFinden(splitlist);                  {Position f-mode merken}
          if SpinEdit3.Tag<>YTHPid then begin
            splitlist.DelimitedText:=inlist[2];    {Vehicle Type merken}
            SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2],2);
            OverWriteVT;                           {Overwrite for PX4 Thunderbird}
          end else
            if CheckBox9.Checked then
              SynEdit1.Lines.Add(capCheckBox9);    {Bei YTH Plus anzeigen, ob bereinigt}
          StaticText1.Caption:=vtypeToStr(SpinEdit3.Tag);  {anzeigen}
          splitlist.DelimitedText:=inlist[0];      {Überschrift wiederherstellen}
        end;
        SpinEdit3.MaxValue:=inlist.Count;
        SpinEdit2.MaxValue:=inlist.Count-10;
        SpinEdit3.Hint:=hntSpinEdit3+', max. '+IntToStr(SpinEdit3.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        StringGrid1.RowCount:=inlist.Count;        {vorbelegen}
        StringGrid1.ColCount:=splitlist.Count;
        StringGrid1.Cells[0,0]:=rsHDcell0;
        for i:=1 to splitlist.count-1 do begin
          StringGrid1.Cells[i,0]:=splitlist[i];
          if pos(lcol, splitlist[i])>0 then begin  {letzte Spalte gefunden}
            StringGrid1.Cells[i,0]:=lcol;
            StringGrid1.ColCount:=i+1;
            break;                                 {wegen Fehler beim H, alte FW}
          end;
        end;

        StringGrid1.BeginUpdate;
        p:=1;
        for x:=1 to inlist.count-1 do begin        {Daten einlesen}
          inc(zhl);
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>6 then begin
            if mode=0 then begin                   {Default: alle}
              for i:=0 to splitlist.count-1 do
                StringGrid1.Cells[i, x]:=splitlist[i];
              if (RadioGroup1.ItemIndex=0) then begin {nur bei Telemetrie}
                if (SpinEdit3.Tag=5) and           {RealSense erkennen beim YTH}
                   (splitlist[StringGrid1.Tag-1]='245') then rs:=true;
                if (StrToIntDef(splitlist[StringGrid1.Tag+3], 0) shr 1)>0 then begin
                  MenuItem7.Enabled:=true;
                  if topp[ListBox1.ItemIndex, 5]=0 then
                    topp[ListBox1.ItemIndex, 5]:=x;
                end;
                if (slat='') and                   {noch kein Homepoint}
                   NichtLeer(splitlist[5]) and
                   NichtLeer(splitlist[6]) and
                   GetRFM(splitlist[StringGrid1.Tag], {f_mode, vehicle type}
                          SpinEdit3.Tag, true) then begin
                  slat:=splitlist[5];              {Homepoint speichern}
                  slon:=splitlist[6];              {Homepoint speichern}
                end;
              end;
              if (RadioGroup1.ItemIndex=1) and     {Remote GPS}
                 (slat='') and                     {noch kein Homepoint}
                 (x>6) and                         {nicht grad die 1. Zeile}
                  NichtLeer(splitlist[1]) and
                  NichtLeer(splitlist[2]) then begin
                slat:=splitlist[2];                {RC Position speichern}
                slon:=splitlist[1];                {RC Position speichern}
              end;
              if (RadioGroup1.Tag=0) and           {Sizing columns not yet done}
                 (x=50) then begin                 {Check till line 50}
                StringGrid1.AutoSizeColumns;
                RadioGroup1.Tag:=1;                {Sizing done}
              end;
            end;
            if mode=1 then begin                   {Filtermode}
              try
                sstr:=UpCase(trim(splitlist[n]));
              except
                sstr:=splitlist[0];
              end;
              if (sstr=ComboBox9.Text) or             {kurz -> vollqualifiziert}
                 (((length(sstr)>4) or (pos('.', sstr)>0)) and  {Punkt drin oder lang}
                 (pos(ComboBox9.Text, sstr)>0)) then begin      {teilqualifiziert}
                for i:=0 to splitlist.count-1 do   {selektierte Zeile}
                  StringGrid1.Cells[i, p]:=splitlist[i];
                inc(p);
              end;
            end;                                   {Ende Filtermode}
          end else begin
            StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsEmpty+tab1+
                                       capLabel6+Format('%6d', [x]);
            SynEdit1.Lines.Add('''7408'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}
        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1 );
          StatusBar1.Panels[2].Text:=rsSelection;
          SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
          if p>1 then begin
            StringGrid1.RowCount:=p;
          end else StringGrid1.RowCount:=5;        {leere Tabelle}
          StringGrid1.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=StringGrid1.Cells[n,0]+' = "'+
                                     ComboBox9.Text+'"';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          if rs then
            StaticText1.Caption:=StaticText1.Caption+'+'+rsRealSense;
          StatusBar1.Panels[2].Text:=RadioGroup2.Items[RadioGroup2.ItemIndex];
          StringGrid1.TopRow:=topp[ListBox1.ItemIndex, RadioGroup1.ItemIndex]; {gemerkten Top setzen}
          if tpos1>0 then begin          {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to StringGrid1.RowCount-1 do
              if ZeitToDT(StringGrid1.Cells[0, i], SpinEdit3.Tag)>tpos1 then
                break;                             {Stelle gefunden}
            StringGrid1.TopRow:=i-StringGrid1.VisibleRowCount-1;   {zeitl. Pos setzen}
          end;
        end;
        if (StringGrid1.Tag=17) and
           (SpinEdit3.Tag=1) then
          HeaderST24;
        StringGrid1.AutoSizeColumn(0);
        StringGrid1.EndUpdate;

        Synedit1.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(slat, slon));   {Anzeige Start und Ende}
        Synedit1.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(splitlist[5], splitlist[6]));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [zhl]);
        SynEdit1.Lines.Add('''7450'+suff+StatusBar1.Panels[5].Text);
      end;
      if PageControl1.ActivePageIndex=1 then StringGrid1.SetFocus;
    end else begin                                 {Datei leer}
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
    label3.Tag:=n;
  end;
end;

procedure TForm1.AnzeigePX4CSV(fn: string);        {CSV aus eigenem Format anzeigen}
var n, i: integer;
    inlist: TStringList;
begin
  n:=Label3.Tag;                        {zwischenspeichern, wird sonst zerstört}
  Screen.Cursor:=crHourGlass;
  SetSensorEnv;
  MenuItem1.Enabled:=true;                         {GoogleMaps Link}
  MenuItem9.Enabled:=true;                         {OSM Link}
  inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(fn);

    if inlist.count>1 then begin                   {Laden inklusive Überschrift}
      StringGrid1.BeginUpdate;
      PageControl1.ActivePageIndex:=1;
      StringGrid1.ColCount:=csvanz;                {auch ID für PX4 CSV}
      StringGrid1.RowCount:=inlist.Count;          {nur wenn Daten vorhanden sind}
      for i:=0 to Inlist.Count-1 do begin
        StringGrid1.Rows[i].Delimiter:=sep;
        StringGrid1.Rows[i].StrictDelimiter:=true;
        StringGrid1.Rows[i].DelimitedText:=inlist[i];
        if i=10 then
          StringGrid1.AutoSizeColumns;             {only once}
      end;
      StringGrid1.EndUpdate;
    end;

    StatusBar1.Panels[1].Text:=IntToStr(inlist.Count-1);
    StatusBar1.Panels[5].Text:=fn;
    SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+' PX4 '+rsMAVlink+tab1+rsDS);
    SynEdit1.Lines.Add('');
  finally
    Label3.Tag:=n;
    FreeAndNil(inlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.BrAnzeigeCSV(const mode: integer); {Breeze Dateien als Tabelle anzeigen}
var i, x, p, n: integer;
    inlist, splitlist: TStringList;
    fn, slat, slon: string;
    lat1: double;
    tpos1: TDateTime;
begin
  n:=Label3.Tag;                        {zwischenspeichern, wird sonst zerstört}
  BitBtn1.Tag:=0;                       {Annahme Breeze Telemetrie in Meter}
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des StringGrid1 überschrieben}
  screen.Cursor:=crHourGlass;
  for i:=0 to StringGrid1.ColCount-1 do StringGrid1.Cols[i].Clear;
  MenuItem7.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  StringGrid1.RowCount:=1;
  StringGrid1.ColCount:=0;                         {alles löschen}
  slat:='';
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+ListBox1.Items[ListBox1.ItemIndex]+bext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>10 then begin
      try
        if pos(plfAndr, inlist[2])>0 then
          BitBtn3.Tag:=1                           {Platform is Android}
        else
          BitBtn3.Tag:=0;                          {else iOS}
        if pos(brsnid, inlist[5])=1 then           {Serial number}
          StaticText1.Caption:=StringReplace(inlist[5], brsnid,
                               vtypeToStr(brID),[rfIgnoreCase]);
        splitlist.DelimitedText:=inlist[8];        {Überschrift einlesen}
        RadioGroup1.ItemIndex:=0;
        topp[ListBox1.ItemIndex, 5]:=0;            {Pointer Null setzen}
        StaticText1.Caption:=vtypeToStr(brid);     {Typ anzeigen}
        SpinEdit3.MaxValue:=inlist.Count-9;
        SpinEdit2.MaxValue:=inlist.Count-10;
        SpinEdit3.Hint:=hntSpinEdit3+', max. '+IntToStr(SpinEdit3.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-10);
        SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        if inlist.count>10 then begin
          StatusBar1.Panels[5].Text:=fn;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        StringGrid1.RowCount:=inlist.Count-8;      {vorbelegen}
        StringGrid1.ColCount:=splitlist.Count;
        for i:=0 to splitlist.count-1 do
          StringGrid1.Cells[i,0]:=splitlist[i];

        StringGrid1.BeginUpdate;
        splitlist.DelimitedText:=inlist[10];       {2. Zeile einlesen}
        for i:=3 to 9 do begin                     {Prüfen, ob ft verwendet wird}
          try
            lat1:=StrToFloatN(splitlist[i]);
            case i of                              {Tag=1, wenn Telemetrie in ft}
              3: if lat1>200  then BitBtn1.Tag:=1; {distance:     40..200}
              4: if lat1>20   then BitBtn1.Tag:=1; {hight:         7..20}
              6: if lat1>300  then BitBtn1.Tag:=1; {goHomeHight:  30..300}
              7: if lat1>800  then BitBtn1.Tag:=1; {maxHight:     30..800}
              8: if lat1>1000 then BitBtn1.Tag:=1; {maxDistance: 100..1000}
              9: if lat1>500  then BitBtn1.Tag:=1; {maxSpeed:    100..500}
            end;
          except
            BitBtn1.Tag:=0;                        {Annahme Werte in Meter}
          end;
        end;
        p:=1;
        for x:=9 to inlist.count-1 do begin        {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin      {Konsistenz checken (Breeze)}
            if mode=0 then begin
              for i:=0 to splitlist.count-1 do
                StringGrid1.Cells[i,x-8]:=splitlist[i];
              if (RadioGroup1.ItemIndex=0) and
                 ((StrToIntDef(splitlist[19], 0) shr 1)>0) then begin
                MenuItem7.Enabled:=true;
                if topp[ListBox1.ItemIndex, 5]=0 then
                  topp[ListBox1.ItemIndex, 5]:=x-8;
              end;
              if (slat='') and                     {Noch kein Homepoint}
                 BrGPSfix(splitlist[20]) and
                 NichtLeer(splitlist[12]) and
                 NichtLeer(splitlist[13]) then begin
                slat:=splitlist[12];               {Homepoint Breeze speichern}
                slon:=splitlist[13];
              end;
            end;
            if mode=1 then begin                   {Filtermode}
              try
                slat:=UpCase(trim(splitlist[n]));
              except
                slat:=splitlist[0];
              end;
              if (slat=ComboBox9.Text) or             {kurz -> vollqualifiziert}
                 (((length(slat)>4) or (pos('.', slat)>0)) and   {Punkt drin oder lang}
                 (pos(ComboBox9.Text, slat)>0)) then begin       {teilqualifiziert}
                for i:=0 to splitlist.count-1 do   {selektierte Zeile}
                  StringGrid1.Cells[i, p]:=splitlist[i];
                inc(p);
              end;
            end;
          end else begin
            StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
            SynEdit1.Lines.Add('''7584'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}
        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1);
          StatusBar1.Panels[2].Text:=rsSelection;
         SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
         if p>1 then begin
            StringGrid1.RowCount:=p;
          end else
            StringGrid1.RowCount:=5;               {leere Tabelle}
          StringGrid1.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=StringGrid1.Cells[n,0]+' = "'+
                                     ComboBox9.Text+'"';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[2].Text:=RadioGroup2.Items[RadioGroup2.ItemIndex];
          x:=(inlist.count-9) div 2;               {Daten irgendwo in der Mitte}
          if tpos1>0 then begin     {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to StringGrid1.RowCount-1 do
              if ZeitToDT(StringGrid1.Cells[0, i], brID)>tpos1 then
                break; {Stelle gefunden}
            StringGrid1.TopRow:=i-StringGrid1.VisibleRowCount-1;   {zeitl. Pos setzen}
          end;
        end;
        StringGrid1.EndUpdate;

        Synedit1.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(BrCoordFormat(slat),
                           BrCoordFormat(slon)));  {Anzeige Start und Ende}
        Synedit1.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(BrCoordFormat(splitlist[12]),
                           BrCoordFormat(splitlist[13])));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [x]);
        SynEdit1.Lines.Add('''7627'+suff+StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    StringGrid1.TopRow:=topp[ListBox1.ItemIndex, RadioGroup1.ItemIndex]; {Top setzen}
    if PageControl1.ActivePageIndex=1 then
      StringGrid1.SetFocus;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
    Label3.Tag:=n;
  end;
end;

{ #############################################################################
 ToDo Liste: Dateinamen, Erkennung usw. restliche Features}

procedure TForm1.MQAnzeigeCSV(const mode: integer); {Mantis Q CSV Format anzeigen}
var i, x, p, n: integer;
    inlist, splitlist: TStringList;
    fn, slat, slon: string;
    tpos1: TDateTime;
begin
  n:=Label3.Tag;                        {zwischenspeichern, wird sonst zerstört}
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des StringGrid1 überschrieben}
  screen.Cursor:=crHourGlass;
  MenuItem7.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  StringGrid1.RowCount:=1;
  StringGrid1.ColCount:=0;                         {alles löschen}
  slat:='';
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+ListBox1.Items[ListBox1.ItemIndex]+bext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>15 then begin
      try
        if pos(plfAndr, inlist[2])>0 then
          BitBtn3.Tag:=1                           {Platform is Android}
        else
          BitBtn3.Tag:=0;                          {else iOS}
        if pos(brsnid, inlist[8])=1 then           {Serial number}
          StaticText1.Caption:=StringReplace(inlist[5], brsnid,
                               vtypeToStr(MQcsvID),[rfIgnoreCase]);
        splitlist.DelimitedText:=inlist[15];       {Überschrift einlesen}
        RadioGroup1.ItemIndex:=0;
        topp[ListBox1.ItemIndex, 5]:=0;            {Pointer Null setzen}
        StaticText1.Caption:=vtypeToStr(MQcsvID);  {Typ anzeigen}
        SpinEdit3.MaxValue:=inlist.Count-16;
        SpinEdit2.MaxValue:=inlist.Count-17;
        SpinEdit3.Hint:=hntSpinEdit3+', max. '+IntToStr(SpinEdit3.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-17);
        SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        if inlist.count>18 then begin
          StatusBar1.Panels[5].Text:=fn;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        StringGrid1.RowCount:=inlist.Count-15;     {vorbelegen}
        StringGrid1.ColCount:=splitlist.Count;
        for i:=0 to splitlist.count-1 do           {Überschrift auslesen}
          StringGrid1.Cells[i,0]:=splitlist[i];

        StringGrid1.BeginUpdate;
        p:=1;
        for x:=16 to inlist.count-1 do begin       {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin      {Konsistenz checken (Breeze)}
            if mode=0 then begin
              for i:=0 to splitlist.count-1 do
                StringGrid1.Cells[i, x-15]:=splitlist[i];
              if (RadioGroup1.ItemIndex=0) and
                 ((StrToIntDef(splitlist[19], 0) shr 1)>0) then begin
                MenuItem7.Enabled:=true;
                if topp[ListBox1.ItemIndex, 5]=0 then
                  topp[ListBox1.ItemIndex, 5]:=x-15;
              end;
              if (slat='') and                     {Noch kein Homepoint}
                 BrGPSfix(splitlist[20]) and
                 NichtLeer(splitlist[12]) and
                 NichtLeer(splitlist[13]) then begin
                slat:=splitlist[12];               {Homepoint Breeze speichern}
                slon:=splitlist[13];
              end;
            end;

          end else begin
            StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
            SynEdit1.Lines.Add('''7584'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}
        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1);
          StatusBar1.Panels[2].Text:=rsSelection;
         SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
         if p>1 then begin
            StringGrid1.RowCount:=p;
          end else
            StringGrid1.RowCount:=5;               {leere Tabelle}
          StringGrid1.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=StringGrid1.Cells[n,0]+' = "'+
                                     ComboBox9.Text+'"';
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[2].Text:=RadioGroup2.Items[RadioGroup2.ItemIndex];
          x:=(inlist.count-9) div 2;               {Daten irgendwo in der Mitte}
          if tpos1>0 then begin     {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to StringGrid1.RowCount-1 do
              if ZeitToDT(StringGrid1.Cells[0, i], MQcsvID)>tpos1 then
                break; {Stelle gefunden}
            StringGrid1.TopRow:=i-StringGrid1.VisibleRowCount-1;   {zeitl. Pos setzen}
          end;
        end;
        StringGrid1.EndUpdate;

        Synedit1.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(BrCoordFormat(slat),
                           BrCoordFormat(slon)));  {Anzeige Start und Ende}
        Synedit1.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(BrCoordFormat(splitlist[12]),
                           BrCoordFormat(splitlist[13])));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [x]);
        SynEdit1.Lines.Add('''7627'+suff+StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    StringGrid1.TopRow:=topp[ListBox1.ItemIndex, RadioGroup1.ItemIndex]; {Top setzen}
    if PageControl1.ActivePageIndex=1 then
      StringGrid1.SetFocus;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
    Label3.Tag:=n;
  end;
end;

{siehe auch:
http://www.lazarusforum.de/viewtopic.php?f=18&t=5516
http://www.lazarusforum.de/viewtopic.php?f=18&t=6769
http://www.lazarusforum.de/viewtopic.php?f=9&t=9689

http://wiki.lazarus.freepascal.org/TAChart_Tutorial:_BarSeries
http://wiki.lazarus.freepascal.org/TAChart_Tutorial:_Stacked_BarSeries

Wenn die x-Werte nicht äquidistant sind, werden die Balken unterschiedlich breit.
Falls dich das stört, setze oben barseries.BarWidthPercent := bwPercentMin.

barseries.DisableRedrawing;     verhindert das Neuzeichnen des Chart nach
                                Hinzufügen jedes Datenpunkts --> schneller
barseries.EnableRedrawing;

Betreffs der Farben in Chart1 ist das so gelöst, dass es für jede Farbe
eine eigene Chart1BarSeries gibt. Entsprechend des FlightModes werden die
Altitude-Werte den verschiedenen Bars zugeordnet.
So entsteht (mit etwas Glück) ein farbiges Diagramm, wo man in etwa sehen kann,
in welchem Mode geflogen wird. Die Farben sind weitestgehend den Farben
der Status-LED am Kopter angepasst.
Dies funktioniert nicht beim Typhoon H Plus.

Chart1BarSeries1: Series Color:=clFuchsia  (Angle Mode – Purple LED)
Chart1BarSeries2: Series Color:=clGreen    (für Smart Mode)
Chart1BarSeries3: Series Color:=clRed      (für RTH)
Chart1BarSeries4: Series Color:=clMaroon   (Emergency)
Chart1BarSeries5: Series Color:= $000080FF (Orange)
Chart1BarSeries7: Series Color:=clBlue     (Sports Mode, Stability)
}

procedure TForm1.HDiaInit;
begin
  Chart1.Title.Text.Clear;
  Chart1.ZoomFull;                                 {Zoomen beenden}
  Chart1.Title.Visible:=false;
  Chart1.AxisList[1].Title.Caption:='';            {default: nix anzeigen}
  if cbCap.Checked then
    Chart1.AxisList[2].Title.Caption:='LiPo [%]'   {Capacity}
  else
    Chart1.AxisList[2].Title.Caption:='LiPo [V]';  {Spannung}
  Chart1.AxisList[1].Title.Visible:=false;
  Chart1.AxisList[0].Title.LabelFont.Color:=clDefault;
  Chart1BarSeries1.Clear;
  Chart1BarSeries2.Clear;
  Chart1BarSeries3.Clear;
  Chart1BarSeries4.Clear;
  Chart1BarSeries5.Clear;
  Chart1BarSeries7.Clear;
{$IFDEF LINUX}
  Chart1BarSeries1.Transparency:=0;                {sonst schwarzer Hintergrund}
{$ELSE}
  Chart1BarSeries1.Transparency:=50;               {für alle gleich}
{$ENDIF}
  Chart1BarSeries2.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries3.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries4.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries5.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries7.Transparency:=Chart1BarSeries1.Transparency;
  Chart1LineSeries1.Clear;                         {Spanungskurve}
  Chart1LineSeries2.Clear;                         {Hüllkurve}
  Chart1LineSeries1.SeriesColor:=clBlue;
  if cbCap.Checked then
    Chart1ConstantLine2.Active:=false
  else begin
    case SpinEdit3.Tag of                          {Umin Linie anzeigen}
      1: Chart1ConstantLine2.Position:=lipomin*6;  {H920 6S}
      5: Chart1ConstantLine2.Position:=lipomin*4;  {YTH  4S}
      H5id: Chart1ConstantLine2.Position:=lipomin*4;   {H520 4S}
      YTHPid: Chart1ConstantLine2.Position:=lipomin*4; {H Plus 4S}
    else
      Chart1ConstantLine2.Position:=lipomin*3;   {3S default}
    end;
    Chart1ConstantLine2.Active:=true;
  end;
  Chart1ConstantLine1.Active:=false;
end;

procedure TForm1.HDiagramm(fn: string);            {Höhenprofil anzeigen}
var x: integer;
    inlist, splitlist: TStringList;
    h, u, hmxg: double;
    gps, simu: boolean;
    bg: TDateTime;

  procedure HDiaBlade350;
  begin
    case StrToIntDef(splitlist[StringGrid1.Tag], 25) of
      25: Chart1BarSeries1.AddXY(bg, h);           {Angle (AP mode)}
      11: Chart1BarSeries7.AddXY(bg, h);           {Stability mit/ohne GPS}
      12: Chart1BarSeries2.AddXY(bg, h);           {Smart}
      9, 10, 13, 14: Chart1BarSeries3.AddXY(bg, h); {Agility + RTH}
      8:  Chart1BarSeries4.AddXY(bg, h);           {Emergency}
    end;
  end;

  procedure HDiaYTHPlus;
  begin
    case StrToIntDef(splitlist[StringGrid1.Tag], 5) of
      4: Chart1BarSeries5.AddXY(bg, h);            {ohne GPS}
      5: Chart1BarSeries1.AddXY(bg, h);            {Angle}
      6: Chart1BarSeries2.AddXY(bg, h);            {Smart}
      7: Chart1BarSeries7.AddXY(bg, h);            {Sport mode blau}
//    10, 14, 17, 20: Chart1BarSeries4.AddXY(bg, h); {maroon}
      12, 13: Chart1BarSeries3.AddXY(bg, h);       {RTH rot}
    end;
  end;

  procedure HDiaYLegacy;
  begin
    case StrToIntDef(splitlist[StringGrid1.Tag], 5) of
      3, 4: Chart1BarSeries1.AddXY(bg, h);         {Angle}
      2, 5, 7, 22, 24, 31, 32: Chart1BarSeries5.AddXY(bg, h); {ohne GPS}
      6, 21, 23, 26..29, 33:   Chart1BarSeries2.AddXY(bg, h); {Smart}
      13, 14, 20:              Chart1BarSeries3.AddXY(bg, h); {Agility + RTH}
//    9, 10, 11, 17, 18:   Chart1BarSeries4.AddXY(bg, h); {Error, Cali}
      12:                      Chart1BarSeries4.AddXY(bg, h); {Emergency}
      0, 1:                    Chart1BarSeries7.AddXY(bg, h); {Stability}
    end;
  end;

begin
  screen.Cursor:=crHourGlass;
  RadioGroup1.ItemIndex:=0;                        {Umschalten auf Telemetrie}
  AnzeigeCSV(0);                     {dazu passende Tabelle laden für Analyse}
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  gps:=false;
  simu:=false;                                     {Simulatorflug}
  hmxg:=0;                                         {Höhe gesamt für Anzeige}
  try
    HDiaInit;
    StringGrid1.Tag:=19;                           {default Position bei neuer FW}
    try
      inlist.LoadFromFile(fn);                     {Telemtry Datei laden}
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then begin
      KursorAus;                                   {Fadenkreuz aus bei neuem Flug}
      MenuItem4.Enabled:=true;                     {Analyse erlauben}
      splitlist.DelimitedText:=inlist[0];  {Überschrift einlesen zum f_mode ermitteln}
      fModeFinden(splitlist);                      {Position f-mode merken}
      SpinEdit3.MaxValue:=inlist.Count;
      SpinEdit2.MaxValue:=inlist.Count-10;
      SpinEdit3.Hint:=hntSpinEdit3+', max. '+IntToStr(SpinEdit3.MaxValue);
      if inlist.Count>1500 then
        Chart1BarSeries1.BarPen.Width:=2           {Bar smoothing}
      else
        Chart1BarSeries1.BarPen.Width:=4;          {dyn. anpassen}
      Chart1BarSeries2.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries3.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries4.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries5.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries7.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      StatusBar1.Panels[5].Text:=rsHProfil+fn;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
      SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
      for x:=1 to inlist.count-1 do
      try                                          {erstmal Daten checken}
        splitlist.DelimitedText:=inlist[x];
        if (splitlist.Count>anzsp) and             {Konsistenz checken (YTH)}
            CheckVT(splitlist[StringGrid1.Tag+2],
                    splitlist[StringGrid1.Tag]) then begin  {YTH Plus sinnvoll}
          if (NichtLeer(splitlist[3])) and
             (splitlist[15]='231') then            {ID für Simulator}
            simu:=true;
          if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and
             (Lowercase(splitlist[8])=idtrue) then
            GPS:=true;
          h:=StrToFloatN(splitlist[4]);
          if testh(h) then begin
            if (hmxg<h) then
              hmxg:=h;                             {Höhe für Anzeige}
            h:=abs(h);
          end;
        end;
      except
        StatusBar1.Panels[5].Text:=rsInvalid;
        SynEdit1.Lines.Add('''7795'+capLabel6+Format('%6d', [x])+  {Datenpunkt ausgeben}
                           suff+StatusBar1.Panels[5].Text);
      end;
      if RadioGroup3.ItemIndex=2 then              {Überschrift im Diagramm ausgeben}
        Chart1.Title.Text.Add(rsGridCell5+suff+Format('%f', [hmxg/fft])+'ft')
      else
        Chart1.Title.Text.Add(rsGridCell5+suff+Format('%f', [hmxg])+'m');
      if simu then begin
        Chart1LineSeries1.SeriesColor:=clGray;     {Simulation}
        Chart1.Title.Text.Add(rsSimulator);
      end;
      if not gps then begin
        Chart1LineSeries1.SeriesColor:=clRed;      {ohne GPS}
        Chart1.Title.Text.Add(rsNoGPS);
      end;
      for x:=1 to inlist.count-1 do
      try                                          {Dateneinlesen}
        splitlist.DelimitedText:=inlist[x];
        if (splitlist.Count>anzsp) and             {Anzahl Spalten}
            CheckVT(splitlist[StringGrid1.Tag+2],
                    splitlist[StringGrid1.Tag]) then begin  {V-type beim YTH Plus}
          bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
          h:=StrToFloatN(splitlist[4]);
          u:=StrToFloatN(splitlist[2]);
          if cbCap.Checked then
            Chart1LineSeries1.AddXY(bg, VtoProz(SpinEdit3.Tag, u))
          else
            Chart1LineSeries1.AddXY(bg, u);        {Spannungskurve}
          if testh(h) and
             (bg>0) then begin
            Chart1LineSeries2.AddXY(bg, h);        {Hüllkurve}
            case SpinEdit3.Tag of                  {Vehicle Type}
              3: HDiaBlade350;                     {Blade 350QX}
              YTHPid: HDiaYTHPlus;                 {YTH Plus}
            else                                   {Q500 und andere}
              HdiaYLegacy;
            end;
          end;
        end;
      except
        StatusBar1.Panels[5].Text:=rsInvalid;
        SynEdit1.Lines.Add('''8410'+capLabel6+Format('%6d', [x])+   {Datenpunkt ausgeben}
                           suff+StatusBar1.Panels[5].Text);
      end;
      Chart1.Title.Visible:=true;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.BrHDiagramm(fn: string);          {Höhenprofil Breeze anzeigen}
var x: integer;
    inlist, splitlist: TStringList;
    h, u, hmxg: double;
    gps: boolean;
    bg: TDateTime;

begin
  screen.Cursor:=crHourGlass;
  RadioGroup1.ItemIndex:=0;                        {Umschalten auf Telemetrie}
  BrAnzeigeCSV(0);                   {dazu passende Tabelle laden für Analyse}
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  gps:=false;
  hmxg:=0;
  try
    HDiaInit;
    Chart1ConstantLine2.Position:=0;
    Chart1ConstantLine2.Active:=false;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>10 then begin
      KursorAus;                                   {Fadenkreuz aus bei neuem Flug}
      SpinEdit3.MaxValue:=inlist.Count-9;
      SpinEdit2.MaxValue:=inlist.Count-10;
      SpinEdit3.Hint:=hntSpinEdit3+', max. '+IntToStr(SpinEdit3.MaxValue);
      if inlist.Count>1500 then
        Chart1BarSeries1.BarPen.Width:=2           {Bar smoothing}
      else
        Chart1BarSeries1.BarPen.Width:=4;          {dyn. anpassen}
      Chart1BarSeries2.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries3.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries4.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries5.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries7.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      StatusBar1.Panels[5].Text:=rsHProfil+fn;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      StatusBar1.Panels[1].Text:=IntToStr(inlist.count-10);
      SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
      for x:=9 to inlist.count-1 do                {Dateneinlesen}
      try
        splitlist.DelimitedText:=inlist[x];
        if splitlist.Count>anzsp then begin
          if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
             BrGPSfix(splitlist[20]) then GPS:=true;  {fraglich, was loseGPSact ist}
          bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
          h:=StrToFloatN(splitlist[10])/100;
          if testh(h) then begin
            if hmxg<h then
              hmxg:=h;                             {Gipfelhöhe}
          end;
          u:=BrUmrech(StrToFloatN(splitlist[21]));
          Chart1LineSeries1.AddXY(bg, u);          {Spannungskurve}
          Chart1LineSeries2.AddXY(bg, h);          {Hüllkurve}
          case StrToIntDef(splitlist[14], 0) of
            1, 18: Chart1BarSeries2.AddXY(bg, h);  {Start, Landung: green}
            2:     Chart1BarSeries7.AddXY(bg, h);  {In flight: blue}
            16:    Chart1BarSeries4.AddXY(bg, h);  {Notlandung: maroon}
          end;
        end else begin
          StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        if not gps then
          Chart1LineSeries1.SeriesColor:=clRed;    {ohne GPS: Voltage rot}
      except
        StatusBar1.Panels[5].Text:=rsInvalid;
        SynEdit1.Lines.Add('''8273'+capLabel6+Format('%6d', [x])+  {Datenpunkt ausgeben}
                           suff+StatusBar1.Panels[5].Text);
      end;
    end;
    if RadioGroup3.ItemIndex=2 then                {Überschrift im Diagramm ausgeben}
      Chart1.Title.Text.Add(rsGridCell5+suff+Format('%f', [hmxg/fft])+'ft')
    else
      Chart1.Title.Text.Add(rsGridCell5+suff+Format('%f', [hmxg])+'m');
    Chart1.Title.Visible:=true;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

function TForm1.FakeHeader: string;                {Missing Header for H920+ST24}
begin
  result:=sep+rsHDcell1+sep+
             rsHDcell2+sep+
             rsHDcell3+sep+
             rsHDcell4+sep+
             rsHDcell5+sep+
             rsHDcell6+sep+
             rsHDcell7+sep+
             rsHDcell8+sep+
             rsHDcell9+sep+
             rsHDcell10+sep+
             rsHDcell11+sep+
             rsHDcell12+sep+
             rsHDcell13+sep+
             rsHDcell14+sep+
             rsHDcell15+sep+
             rsHDcell16+sep+
             rsHDcell17+sep+
             rsHDcell18+sep+
             rsHDcell19+sep+
             rsHDcell20+sep+
             rsHDcell21;
end;

procedure TForm1.AnzeigeSchnell;                   {Schnellanalyse}
var inlist0, inlist1, inlist2, splitlist: TStringList;
    fn: string;
    bg: TDateTime;
    w: double;

  procedure MakeSAH(lab: TLabeledEdit; hist: TLineSeries; cht: TChart);
  var x, p: integer;
  begin
    case lab.Tag of
      0: begin                                     {Telemetry}
           if (StringGrid1.Tag=17) and
              (SpinEdit3.Tag=1) then
             inlist0[0]:=FakeHeader;
           splitlist.DelimitedText:=inlist0[0];    {Column header}
           if splitlist.count>30 then              {Firmware error YTH}
             for x:=15 to splitlist.count-1 do
               if pos(lcol, splitlist[x])>0 then begin
                 splitlist[x]:=lcol;               {Zahlen wegwerfen}
                 break;
               end;
           p:=splitlist.IndexOf(lab.Text);         {find index of column}
           if p>0 then begin
             for x:=1 to inlist0.count-1 do begin  {Werte einlesen}
               splitlist.DelimitedText:=inlist0[x];
               if CheckVT(splitlist[StringGrid1.Tag+2],
                          splitlist[StringGrid1.Tag]) then begin
                 w:=TransformW(0, p, StrToFloatN(splitlist[p]));
                 if (p<>4) or
                    ((p=4) and testh(w)) then begin
                   bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
                   hist.AddXY(bg, w);
                 end;
               end;
             end;
           end;
         end;   {Fehler im Hauptteil der Procedur werfen mit Fehlerausgabe}
      1: try                                       {RemoteGPS, kann fehlen}
           splitlist.DelimitedText:=inlist1[0];    {Column header}
           p:=splitlist.IndexOf(lab.Text);         {find index of column}
           if p>0 then begin
             for x:=1 to inlist1.count-1 do begin  {Werte einlesen}
               splitlist.DelimitedText:=inlist1[x];
               bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
               w:=TransformW(1, p, StrToFloatN(splitlist[p]));
               hist.AddXY(bg, w);
             end;
           end;
      except
        {fehlende Datei RemoteGPS ist kein Fehler, muss aber abgefangen werden}
      end;
      2: try                                       {Remote, kann fehlen}
           splitlist.DelimitedText:=inlist2[0];    {Column header}
           p:=splitlist.IndexOf(lab.Text);         {find index of column}
           if p>0 then begin
             cht.AxisList[0].Title.Caption:=       {Bezeichnung überschreiben}
               ChToStr(cht.AxisList[0].Title.Caption, p);
             for x:=1 to inlist2.count-1 do begin  {Werte einlesen}
               splitlist.DelimitedText:=inlist2[x];
               bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
               w:=TransformW(2, p, StrToFloatN(splitlist[p]));
               hist.AddXY(bg, w);
             end;
           end;
         except
           {fehlende Datei Remote ist kein Fehler, muss aber abgefangen werden}
         end;
    end;
  end;

  procedure brMakeSAH(lab: TLabeledEdit; hist: TLineSeries);
  var x, p: integer;
  begin
    splitlist.DelimitedText:=inlist0[8];           {Column header}
    p:=splitlist.IndexOf(lab.Text);                {find index of column}
    if p>0 then begin
      for x:=9 to inlist0.count-1 do begin         {Werte einlesen}
        splitlist.DelimitedText:=inlist0[x];
        bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
        if (pos('lat', lab.Text)=1) or
           (pos('lon', lab.Text)=1) then begin
          w:=BrCoordToFloat(splitlist[p]);
        end else
          w:=brTransformW(p, StrToFloatN(splitlist[p]));
        hist.AddXY(bg, w);
      end;
    end;
  end;

  procedure ChkFileFill(lab1: TLabeledEdit);       {Inlist füllen, aber nur wenn leer}
  begin
    case lab1.Tag of             {Datei laden, wenn noch nicht gefüllt}
      0: if inlist0.Count=0 then begin
           fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+kpath+
               kfile+ListBox1.Items[ListBox1.ItemIndex]+fext; {Telemetry}
           if FileExists(fn) then inlist0.LoadFromFile(fn);
         end;
      1: if inlist1.Count=0 then begin
           fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+spath+
               PathDelim+sfile+ListBox1.Items[ListBox1.ItemIndex]+fext; {RemGPS}
           if FileExists(fn) then inlist1.LoadFromFile(fn);
         end;
      2: if inlist2.Count=0 then begin
           fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+fpath+
               PathDelim+ffile+ListBox1.Items[ListBox1.ItemIndex]+fext; {Rem}
           if FileExists(fn) then inlist2.LoadFromFile(fn);
         end;
      brid: if inlist0.Count=0 then begin
           fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
               ListBox1.Items[ListBox1.ItemIndex]+bext;   {Breeze}
           if FileExists(fn) then inlist0.LoadFromFile(fn);
         end;
    end;

  end;

begin
  GroupBox4.Tag:=1;                                {Anzeige gelaufen}
  screen.Cursor:=crHourGlass;
  inlist0:=TStringList.Create;
  inlist1:=TStringList.Create;
  inlist2:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  Chart3LineSeries1.Clear;
  Chart4LineSeries1.Clear;
  Chart5LineSeries1.Clear;
  Chart3.ZoomFull;
  Chart4.ZoomFull;
  Chart5.ZoomFull;
  Chart3LineSeries1.SeriesColor:=ColorButton2.ButtonColor;
  Chart4LineSeries1.SeriesColor:=ColorButton3.ButtonColor;
  Chart5LineSeries1.SeriesColor:=ColorButton4.ButtonColor;
  Chart3.AxisList[0].Title.Caption:=LabeledEdit1.Text;  {y-Achse top}
  Chart4.AxisList[0].Title.Caption:=LabeledEdit2.Text;  {y-Achse middle}
  Chart5.AxisList[0].Title.Caption:=LabeledEdit3.Text;  {y-Achse bottom}
  try
    try                                            {cover empty files}
      ChkFileFill(LabeledEdit1);
      if LabeledEdit1.Tag=brID then
        brMakeSAH(LabeledEdit1, Chart3LineSeries1) {Breeze}
      else
        MakeSAH(LabeledEdit1, Chart3LineSeries1, Chart3);
      ChkFileFill(LabeledEdit2);
      if LabeledEdit2.Tag=brID then
        brMakeSAH(LabeledEdit2, Chart4LineSeries1) {Breeze}
      else
        MakeSAH(LabeledEdit2, Chart4LineSeries1, Chart4);
      ChkFileFill(LabeledEdit3);
      if LabeledEdit3.Tag=brID then
        brMakeSAH(LabeledEdit3, Chart5LineSeries1) {Breeze}
      else
        MakeSAH(LabeledEdit3, Chart5LineSeries1, Chart5);
    except
      StatusBar1.Panels[5].Text:=rsCheckSettings+capAnalyse;
      SynEdit1.Lines.Add('''8442'+suff+StatusBar1.Panels[5].Text+tab1+rsDS);
    end;
  finally
    FreeAndNil(inlist0);
    FreeAndNil(inlist1);
    FreeAndNil(inlist2);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.KMLheader(f: string; dt: TDateTime; klist: TStringList);
begin
  klist.Add(xmlvers);
  klist.Add(kmlvers);
  klist.Add('<'+doctag);
  klist.Add('<name>'+capForm1+'</name>');
  klist.Add('<description>'+
              FormatDateTime(mzf, dt)+'h - '+
              ExtractFileName(f)+'</description>');
  klist.Add('<Style id="Flightpath">');
  klist.Add(tab2+'<LineStyle>');
  klist.Add(tab4+'<color>'+ColorToKMLColor(ColorButton1.ButtonColor)+'</color>');  {Farbe der Linie}
  klist.Add(tab4+'<width>'+IntToStr(SpinEdit1.Value)+'</width>');
  klist.Add(tab2+'</LineStyle>');
  klist.Add(tab2+'<PolyStyle><color>7f00ff00</color></PolyStyle>'); {for Waypoints}
  klist.Add(tab2+'<IconStyle><Icon><href>'+aircrafticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');
  klist.Add('<Style id="GrndStn">');           {Bewegungen Pilot}
  klist.Add(tab2+'<LineStyle>');
  klist.Add(tab4+'<color>FF000000</color>');    {Farbe der Linie, schwarz}
  klist.Add(tab4+'<width>2</width>');
  klist.Add(tab2+'</LineStyle>');
  klist.Add('</Style>');
  klist.Add('<Style id="starting">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+starticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');
  klist.Add('<Style id="landing">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+stopicon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');
end;

{https://developers.google.com/kml/documentation/kml_tut
 http://www.zonums.com/gmaps/kml_color/
 http://kml4earth.appspot.com/icons.html     (Icons)
 http://googlegeodevelopers.blogspot.de/2010/07/making-tracks-new-kml-extensions-in.html
 http://gps.hillclimb.de/?page_id=504

 <extrude>1</extrude>}

procedure TForm1.MacheKML(fn: string; z: integer);   {Hauptfunktion - konvertieren in KML}
var
  inlist, kmllist, splitlist, outlist, outlist1: TStringList;
  x: integer;
  n: Integer=0;
  dn, skoor, stime, lkoor, ltime: string;
  ts, bg: TDateTime;
  absh: Double;

  procedure kmlLegacy;
  begin
    if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and   {Werte validieren}
       testh(StrToFloatN(splitlist[4])) and
       CheckVT(splitlist[StringGrid1.Tag+2],
               splitlist[StringGrid1.Tag]) and
       GetRFM(splitlist[StringGrid1.Tag],
              SpinEdit3.Tag, true) then begin
      bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
      ts:=bg+nowUTC-now;                 {UTC Zeitstempel errechnen}
      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf+zzz, ts)+'Z';
      lkoor:=ChrKoor(splitlist[6])+tab1+
             ChrKoor(splitlist[5])+tab1+splitlist[4]; {lon + lat + alt}
      if stime='' then begin             {Startpunkt merken}
        stime:=ltime;
        if RadioGroup5.ItemIndex=0 then absh:=GethFromST10(z, bg);
        skoor:=ChrKoor(splitlist[6])+sep+
               ChrKoor(splitlist[5])+sep+'0.0';  {ohne Höhe}
      end;
      inc(n);
      outlist1.Add(ltime);               {Zeitstempel}
      outlist.Add(lkoor);                {Koordinaten}
    end;
  end;

  procedure kmlMQcsv;
  begin
    bg:=ZeitToDT(splitlist[0], MQid);
    if (NichtLeer(splitlist[9]) or
        NichtLeer(splitlist[10])) and
        (bg>0) then begin
//    ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ts:=bg;
      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf, ts)+'Z';
      lkoor:=ChrKoor(splitlist[10])+tab1+
             ChrKoor(splitlist[9])+tab1+
             splitlist[5];                         {lon + lat + alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=ChrKoor(splitlist[10])+sep+
               ChrKoor(splitlist[9])+sep+'0.0';    {ohne Höhe}
      end;
      inc(n);
      outlist1.Add(ltime);                         {Zeitstempel}
      outlist.Add(lkoor);                          {Koordinaten}
    end;
  end;

  procedure kmlBreeze;
  begin
    bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
    if (NichtLeer(splitlist[12]) or
        NichtLeer(splitlist[13])) and
        BrGPSfix(splitlist[20]) and
        (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf, ts)+'Z';
      lkoor:=BrCoordFormat(splitlist[13])+tab1+
             BrCoordFormat(splitlist[12])+tab1+
             BrTeilen(splitlist[10], 100, 1);      {lon + lat + alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=BrCoordFormat(splitlist[13])+sep+
               BrCoordFormat(splitlist[12])+sep+'0.0';  {ohne Höhe}
      end;
      inc(n);
      outlist1.Add(ltime);                         {Zeitstempel}
      outlist.Add(lkoor);                          {Koordinaten}
    end;
  end;

begin
  Screen.Cursor:=crHourGlass;
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  outlist1:=TStringList.Create;
  kmllist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  stime:='';
  absh:=0;
  try
    if fn<>'' then begin
      try
        inlist.LoadFromFile(fn);
      except
        StatusBar1.Panels[5].Text:=fn+nixda;
        SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      if inlist.count>10 then
      try
        splitlist.DelimitedText:=inlist[0]; {Überschrift einlesen, f_mode ermitteln}
        fModeFinden(splitlist);                    {Position f-mode merken}
        splitlist.DelimitedText:=inlist[2];        {2. Datenzeile, Zeit}
        if (SpinEdit3.Tag<>YTHPid) and
           (SpinEdit3.Tag<>brID) then begin        {v_type ermitteln}
          SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
          OverWriteVT;                             {Overwrite for PX4 Thunderbird}
        end;
        ts:=ZeitToDT(splitlist[0], SpinEdit3.Tag)+nowUTC-now;
        KMLheader(fn, ts, kmllist);
        for x:=1 to inlist.Count-1 do
          if CheckE7(inlist[x]) then begin
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin
            case SpinEdit3.Tag of
              brID: kmlBreeze;
              MQcsvID: kmlMQcsv;
            else
              kmlLegacy;
            end;
          end else begin
             SynEdit1.Lines.Add(rsNotUsed+suff+inlist[x]);
          end;
        end;
        kmllist.Add('<'+pmtag);                    {Startpunkt}
        kmllist.Add('<TimeStamp><'+KMLwhen+stime+'</'+KMLwhen+'</TimeStamp>');
        kmllist.Add('<styleUrl>#starting</styleUrl>');
        kmllist.Add('<Point><'+cotag+skoor+'</'+cotag+'</Point>');
        kmllist.Add('</'+pmtag);
        kmllist.Add('<'+pmtag);
        kmllist.Add(tab2+'<name>'+ComboBox1.Text+'</name>');
        kmllist.Add(tab2+'<description>'+ExtractFileName(fn)+'</description>');
        kmllist.Add(tab2+'<styleUrl>#Flightpath</styleUrl>');
        kmllist.Add(tab2+'<gx:Track>');               {Start playable track}
{wenn absolute Höhe aus ST10 kommt und es eingestellt ist}
        if (absh<>0) and
           (RadioGroup5.ItemIndex=0) then begin
          kmllist.Add(tab4+'<gx:altitudeOffset>'+
                      FloatToStrF(absh, ffFixed, 5, 1)+
                      '</gx:altitudeOffset>');
          kmllist.Add(tab4+'<'+amtag+RadioGroup5.Items[0]+'</'+amtag);
        end else begin
          if RadioGroup5.ItemIndex=2 then
            kmllist.Add(tab4+'<'+amtag+RadioGroup5.Items[2]+'</'+amtag)
          else
            kmllist.Add(tab4+'<'+amtag+RadioGroup5.Items[1]+'</'+amtag);
        end;
        if CheckBox11.Checked then kmllist.Add(tab4+extru);
        if n>10 then begin
          for x:=0 to outlist.count-1 do           {Timestamps}
            kmllist.add(tab6+'<'+KMLwhen+outlist1[x]+'</'+KMLwhen);
          for x:=0 to outlist.count-1 do           {Coordinates}
            kmllist.add(tab6+'<gx:coord>'+outlist[x]+'</gx:coord>');
        end;
        kmllist.Add(tab2+'</gx:Track>');
        kmllist.Add('</'+pmtag);                   {End playable track}
        kmllist.Add('<'+pmtag);                    {Landepunkt}
        kmllist.Add('<TimeStamp><'+KMLwhen+ltime+'</'+KMLwhen+'</TimeStamp>');
        kmllist.Add('<styleUrl>#landing</styleUrl>');
        lkoor:=StringReplace(lkoor, tab1, sep, [rfReplaceAll]);
        kmllist.Add('<Point><'+cotag+lkoor+'</'+cotag+'</Point>');
        kmllist.Add('</'+pmtag);

{bei Bedarf den Pfad der ST10 auch einspeichern}
        if (SpinEdit3.Tag<>brID) and CheckBox7.Checked then begin
          dn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+spath+
                             PathDelim+sfile+ListBox1.Items[z]+fext;
          if  FileExists(dn) and (n>10) then begin {RemoteGPS als Pfad}
            try
              inlist.LoadFromFile(dn);
            except
              StatusBar1.Panels[5].Text:=dn+nixda;
              SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
            end;
            if inlist.count>5 then begin
              kmllist.Add('<'+pmtag);
              kmllist.Add(tab2+'<name>'+spath+'</name>');
              kmllist.Add(tab2+'<description>'+ExtractFileName(dn)+'</description>');
              kmllist.Add(tab2+'<styleUrl>#GrndStn</styleUrl>');
              kmllist.Add(tab2+'<LineString>');
              kmllist.Add(tab4+'<'+amtag+'clampToGround</'+amtag);
              kmllist.Add(tab4+'<'+cotag);
              for x:=1 to inlist.Count-1 do begin
                splitlist.DelimitedText:=inlist[x];
                if (splitlist.Count>5) and           {Werte validieren}
                   (NichtLeer(splitlist[1]) or NichtLeer(splitlist[2])) then
                   kmllist.add(tab6+ChrKoor(splitlist[1])+sep+    {lon + lat}
                               ChrKoor(splitlist[2])+sep+'0.0');  {ohne Höhe}
              end;
              KMLfooter1(cotag, kmllist);
            end;
          end;
        end;           {Ende wenn Pilotenpfad angezeigt werden kann/soll}
        KMLfooter2(kmllist);
        if n>10 then begin                         {kleine Dateien ausblenden}
          dn:=ResultDN(fn, RadioGroup2.Items[0]);
          try
            kmllist.SaveToFile(dn);
            StatusBar1.Panels[5].Text:=dn+tab1+rsSaved;
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
            if RadioGroup2.ItemIndex=1 then MacheKMZ(dn);
          except
            StatusBar1.Panels[5].Text:=dn+tab1+rsNotSaved;
            SynEdit1.Lines.Add('''8835'+suff+StatusBar1.Panels[5].Text);
          end;
        end;
      except
        SynEdit1.Lines.Add('''8839'+suff+rsInvalid+tab1+rsDS);
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(outlist);
    FreeAndNil(outlist1);
    FreeAndNil(kmllist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

{http://wiki.freepascal.org/paszlib}
procedure TForm1.MacheKMZ(fn: string);  {zippt die kml-Datei, benennt um in kmz}
var
  KMLZipper: TZipper;
begin
  KMLZipper := TZipper.Create;
  try
    KMLZipper.FileName:=ChangeFileExt(fn, RadioGroup2.Items[1]);
    KMLZipper.Entries.AddFileEntry(fn, ExtractFileName(fn));
    KMLZipper.ZipAllFiles;
    DeleteFile(fn);
  finally
    KMLZipper.Free;
  end;
end;

{Visualisierung: http://www.doarama.com/info
So on that note how about working on a converter to change the CSV files to
the proper GPX or IGC files to work in http://www.doarama.com/.

GPX format:
http://www.topografix.com/gpx.asp
http://www.topografix.com/gpx/1/1/
https://en.wikipedia.org/wiki/GPS_Exchange_Format
http://www.doarama.com/api/0.2/docs
http://publicgpx.blog-me.de/2010/11/13/gpx-dateiformat-1/

https://github.com/mikecarr/yuneec-logconvert-web

GPX oder KMl anzeigen: http://www.atlsoft.de/gpx/
}
procedure TForm1.MacheGPX(fn: string; z: integer); {Hauptfunktion - nach GPX}
var
  inlist, kmllist, splitlist, outlist: TStringList;
  x: integer;
  n: Integer=0;
  dn, skoor, stime, lkoor, ltime, lalt: string;
  ts, bg: TDateTime;
  absh, ch: double;

  procedure gpxBreeze;
  begin
    bg:=ZeitToDT(splitlist[0], brID);
    if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
        BrGPSfix(splitlist[20]) and
       (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=tab2+'<time>'+FormatDateTime(dzf, ts)+'T'+
                      FormatDateTime(zzf, ts)+'Z</time>';
      lkoor:=tab1+GPXlat+BrCoordFormat(splitlist[12])+GPXlon+
                        BrCoordFormat(splitlist[13])+'">';   {lat + lon}
      lalt:=tab1+GPXele+BrTeilen(splitlist[10], 100, 1)+'</ele>';{alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=lkoor;
      end;
      inc(n);
      outlist.Add(lkoor+lalt+ltime);
    end;
  end;

  procedure gpxYLegacy;
  begin
    if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and
        testh(StrToFloatN(splitlist[4])) and       {Werte validieren}
        CheckVT(splitlist[StringGrid1.Tag+2],
                splitlist[StringGrid1.Tag]) and
        GetRFM(splitlist[StringGrid1.Tag],
               SpinEdit3.Tag, true) and
        CheckE7(inlist[x]) then begin
      bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=tab2+'<time>'+FormatDateTime(dzf, ts)+'T'+
                           FormatDateTime(zzf, ts)+'Z</time>';
      lkoor:=tab1+GPXlat+ChrKoor(splitlist[5])+GPXlon+
                        ChrKoor(splitlist[6])+'">'; {lat + lon}
      lalt:=splitlist[4];                          {altitude}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=lkoor;
        absh:=GethFromST10(z, bg);                 {Höhe aus der STxx}
      end;
      try
        ch:=StrToFloatN(lalt)+absh;                {absolute Höhe}
        lalt:=FloatToStrF(ch, ffFixed, 5, 2);
      except                                       {lalt unverändert übernehmen}
      end;
      lalt:=tab1+GPXele+lalt+'</ele>';{alt}
      inc(n);
      outlist.Add(lkoor+lalt+ltime);
    end;
  end;

  procedure gpxMQcsv;    {############# ToDo: testen #############}
  begin
    bg:=ZeitToDT(splitlist[0], MQcsvID);
    if (NichtLeer(splitlist[9]) or NichtLeer(splitlist[10])) and
       (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=tab2+'<time>'+FormatDateTime(dzf, ts)+'T'+
                           FormatDateTime(zzf, ts)+'Z</time>';
      lkoor:=tab1+GPXlat+ChrKoor(splitlist[9])+GPXlon+
                         ChrKoor(splitlist[10])+'">';   {lat + lon}
      lalt:=tab1+GPXele+splitlist[5]+'</ele>';{alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=lkoor;
      end;
      inc(n);
      outlist.Add(lkoor+lalt+ltime);
    end;
  end;

begin
  Screen.Cursor:=crHourGlass;
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  kmllist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  stime:='';
  try
    if fn<>'' then begin
      try
        inlist.LoadFromFile(fn);
      except
        StatusBar1.Panels[5].Text:=fn+nixda;
        SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      if inlist.count>10 then
      try
        splitlist.DelimitedText:=inlist[0]; {Überschrift einlesen, f_mode ermitteln}
        fModeFinden(splitlist);                    {Position f-mode merken}
        splitlist.DelimitedText:=inlist[2];        {2. Datenzeile, Zeit}
        if (SpinEdit3.Tag<>YTHPid) and
           (SpinEdit3.Tag<>brID) then begin        {v_type ermitteln}
          SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
          OverWriteVT;                             {Overwrite for PX4 Thunderbird}
        end;
        ts:=ZeitToDT(splitlist[0], SpinEdit3.Tag)+nowUTC-now;
        GPXheader(ComboBox1.Text, fn, ts, kmllist);
        for x:=1 to inlist.Count-1 do begin        {Startpunkt ermitteln}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin
            case SpinEdit3.Tag of
              brID: gpxBreeze;                     {Breeze}
              MQcsvID: gpxMQcsv;                   {Mantis q CSV format}
            else                                   {Yuneec legacy}
              gpxYLegacy;
            end;
          end else begin
            StatusBar1.Panels[5].Text:=rsInvalid;
            SynEdit1.Lines.Add('''9087'+suff+StatusBar1.Panels[5].Text);
          end;
        end;
        kmllist.Add('<wpt'+skoor);                 {Startpunkt}
        kmllist.Add(tab1+GPXele+'0.0</ele>');
        kmllist.Add(stime);
        kmllist.Add(tab2+'<name>Start</name>');
        kmllist.Add(GPXet1);

        kmllist.Add('<trk>');
        kmllist.Add(tab2+'<name>'+ExtractFileName(fn)+'</name>');
        kmllist.Add(tab2+'<trkseg>');
        if n>10 then for x:=0 to outlist.count-1 do
          kmllist.add(tab4+'<trkpt'+outlist[x]+GPXet3);
        GPXfooter1(kmllist);

        dn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+spath+
                         PathDelim+sfile+ListBox1.Items[z]+fext;
        if CheckBox7.Checked and
           FileExists(dn) and
           (n>10) then begin  {RemoteGPS als Pfad}
          try
            inlist.LoadFromFile(dn);
          except
            StatusBar1.Panels[5].Text:=dn+nixda;
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
          end;
          if inlist.count>5 then begin
            kmllist.Add('<trk>');
            kmllist.Add(tab2+'<name>'+ExtractFileName(dn)+'</name>');
            kmllist.Add(tab2+'<trkseg>');
            for x:=1 to inlist.Count-1 do begin
              splitlist.DelimitedText:=inlist[x];
              if (splitlist.Count>5) and           {Werte validieren}
                 (NichtLeer(splitlist[1]) or NichtLeer(splitlist[2])) then begin
                ts:=ZeitToDT(splitlist[0], SpinEdit3.Tag)+nowUTC-now;  {UTC Zeitstempel errechnen}
                ltime:=tab2+'<time>'+FormatDateTime(dzf, ts)+'T'+
                                 FormatDateTime(zzf, ts)+'Z</time>';
                lkoor:=tab1+GPXlat+ChrKoor(splitlist[2])+
                            GPXlon+ChrKoor(splitlist[1])+'">'; {lat + lon}
                lalt:=tab1+GPXele+splitlist[3]+'</ele>';{alt}
                kmllist.Add(tab4+'<trkpt'+lkoor+lalt+ltime+GPXet3);
              end;
            end;
            GPXfooter1(kmllist);
          end;
        end;                {Ende wenn Pilotenpfad angezeigt werden kann/soll}

        kmllist.Add('<wpt'+lkoor);                 {Landepunkt}
        kmllist.Add(lalt);
        kmllist.Add(ltime);
        kmllist.Add(tab2+'<name>Stop</name>');
        kmllist.Add(GPXet1);
        GPXfooter2(kmllist);
        if n>10 then begin                         {kleine Dateien ausblenden}
          dn:=ResultDN(fn, RadioGroup2.Items[RadioGroup2.ItemIndex]);
          try
            kmllist.SaveToFile(dn);
            StatusBar1.Panels[5].Text:=dn+tab1+rsSaved;
            SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
          except
            StatusBar1.Panels[5].Text:=dn+tab1+rsNotSaved;
            SynEdit1.Lines.Add('''9249'+suff+StatusBar1.Panels[5].Text);
          end;
        end;
      except
        SynEdit1.Lines.Add('''9153'+suff+rsInvalid+tab1+rsDS);
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(outlist);
    FreeAndNil(kmllist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

function TForm1.GethFromST10(const z: integer; const dt: TDateTime): double; {Höhe über NN aus RemoteGPS_xxxx}
             {z: Index der Datei, dt: Zeitpunkt wo Höhe genommen werden soll}
var
  inlist, splitlist: TStringList;
  n: Integer=0;
  x: integer;
  hw, hx: double;
begin
  result:=0;
  hw:=0;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    try
      inlist.LoadFromFile(IncludeTrailingPathDelimiter(ComboBox2.Text)+
                          spath+PathDelim+sfile+ListBox1.Items[z]+fext);
      for x:=1 to inlist.count-1 do begin
        splitlist.DelimitedText:=inlist[x];
        if splitlist.Count>5 then begin
          if ZeitToDT(splitlist[0], SpinEdit3.Tag)>dt then begin {Startpunkt suchen}
            hx:=StrToFloatN(splitlist[3]);
            if (abs(hx)>0.1) and testh(hx) then begin  {nur gültige Höhenwerte zählen}
              hw:=hw+hx;
              inc(n);
            end;
            if n>40 then
              break;                               {Abbrechen nach 40 Werten zum Runden}
          end;
        end;
      end;
      result:=(hw/n)-1;                            {Durchschnittswert}
    except
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.MacheDash(z: integer); {ST2DASH Art, Z: index der Datei
DashWare profile:
http://www.drohnen-forum.de/index.php/Attachment/9815-Yuneec-Q500-Dashware-Profile-v1-5-zip/?s=430b777284562460d8986cbff5c7717c665db155}
var
  inlist, dashlist, splitlist: TStringList;
  x, n: Integer;
  fn, s: string;
  hgrd, dist, lat1, lat2, lon1, lon2: double;
  bg: TDateTime;
begin
  Screen.Cursor:=crHourGlass;
  inlist:=TStringList.Create;
  dashlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  hgrd:=0;
  lat1:=0;
  lon1:=0;
  fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
      kpath+kfile+ListBox1.Items[z]+fext;
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then
    try
      splitlist.DelimitedText:=inlist[0]; {Überschrift einlesen, f_mode ermitteln}
      fModeFinden(splitlist);                      {Position f-mode merken}
      splitlist.DelimitedText:=inlist[2];          {2. Datenzeile, Zeit}
      if SpinEdit3.Tag<>YTHPid then begin          {v_type ermitteln}
        SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
        OverWriteVT;                             {Overwrite for PX4 Thunderbird}
      end;
      s:=Trim(inlist[0]);
      s:=StringReplace(s, 'altitude', 'ascent', [rfIgnoreCase]);
      dashlist.Add('distance_from_start'+sep+'datetime'+sep+'altitude'+s);
      for x:=1 to inlist.Count-1 do
        if CheckE7(inlist[x]) then begin
        splitlist.DelimitedText:=inlist[x];
        if (splitlist.Count>anzsp) and             {Konsistenz checken (YTH)}
            CheckVT(splitlist[StringGrid1.Tag+2],
                    splitlist[StringGrid1.Tag]) and
            testh(StrToFloatN(splitlist[4])) then begin
          try
            if ((NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
               (GetRFM(splitlist[StringGrid1.Tag],
                       SpinEdit3.Tag, true)) or
                CheckBox3.Checked) then begin
              lat2:=StrToFloatN(splitlist[5]);
              lon2:=StrToFloatN(splitlist[6]);
              bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
{Startpunkt extra validieren, oben ist noch ein OR !}
              if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
                 GetRFM(splitlist[StringGrid1.Tag],
                        SpinEdit3.Tag, true) then begin
                if (lat1=0) and (lon1=0) then begin    {Startpunkt festlegen}
                  hgrd:=GethFromST10(z, bg);
                  dist:=0;
                  lon1:=lon2;
                  lat1:=lat2;
                end else begin           {Distanz zum Startpunkt berechnen}
                  dist:=DeltaKoord(lat1, lon1, lat2, lon2);   {Entfernung zum Startpunkt}
                end;
              end else begin
                dist:=0;
                if IsMStart(splitlist[StringGrid1.Tag], splitlist[StringGrid1.Tag+2]) then begin
                  lat1:=0;                         {Startpunkt zurücksetzen}
                  lon1:=0;
                end;
              end;
              s:=FloatToStrF(dist, ffFixed, 12, 1)+sep+     {distance_from_start}
                 FormatDateTime(vzf+zzz, bg)+sep+
                 FloatToStrF(StrToFloatN(splitlist[4])+hgrd, ffFixed, 12, 2); {altitude}
              splitlist[8]:=KorrBool(splitlist[8]);         {gps_used}
              splitlist[14]:=KorrSigned(splitlist[14],255); {Motorstatus}
              splitlist[15]:=KorrSigned(splitlist[15],255); {IMU status}
              splitlist[16]:=KorrSigned(splitlist[16],255); {press_comp_status}
              for n:=1 to splitlist.count-1 do
                s:=s+sep+splitlist[n];
              dashlist.add(s);
            end;
          except
           SynEdit1.Lines.Add(''''+capLabel6+Format('%6d', [x])+   {Datenpunkt ausgeben}
                             suff+StatusBar1.Panels[5].Text);
         end;
        end else begin
          StatusBar1.Panels[5].Text:=rsInvalid;
        end;
      end;
      if dashlist.count>10 then begin
        fn:=ResultDN(fn, RadioGroup2.Items[RadioGroup2.ItemIndex]); {+dashw.csv}
        try
          dashlist.SaveToFile(fn);
          StatusBar1.Panels[5].Text:=fn+tab1+rsSaved;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        except
          StatusBar1.Panels[5].Text:=fn+tab1+rsNotSaved;
          SynEdit1.Lines.Add('''8700'+suff+StatusBar1.Panels[5].Text);
        end;
      end;
    except
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(dashlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

{RaceRender:
 http://racerender.com/                Time: hh:mm:ss.nn
 http://racerender.com/Developer
 http://racerender.com/RR3/docs/HowTo-DataInput.html}
procedure TForm1.MacheRR(z: integer); {RaceRender kompatible Datei}
var
  inlist, dashlist, splitlist: TStringList;
  fn, s: string;
  dist, lat1, lat2, lon1, lon2: double;
  bg: TDateTime;

  procedure RRBreeze;
  var x: integer;
  begin
    for x:=9 to inlist.count-1 do begin
      splitlist.DelimitedText:=inlist[x];
      if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
          BrGPSfix(splitlist[20]) and
         (ZeitToDT(splitlist[0], SpinEdit3.Tag)>0) and
         ((trim(splitlist[14])<>'0') or CheckBox3.Checked) then begin
        bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
        lat2:=BrCoordToFloat(splitlist[12]);
        lon2:=BrCoordToFloat(splitlist[13]);
        if (lat1=0) and (lon1=0) then begin        {Startpunkt festlegen}
          dist:=0;
          lon1:=lon2;
          lat1:=lat2;
        end else begin           {Distanz zum Startpunkt berechnen}
          dist:=DeltaKoord(lat1, lon1, lat2, lon2); {Entfernung zum Startpunkt}
        end;
        s:='0';
        if (StatusToByte(trim(splitlist[20])) and 128)>0 then
          s:='1';                                  {GPS fix}
        dashlist.add(FormatDateTime(zzf+'.zz', bg)+sep+      {Time}
                     BrCoordFormat(splitlist[13])+sep+       {lon}
                     BrCoordFormat(splitlist[12])+sep+       {lat}
                     BrTeilen(splitlist[10], 100, 1)+sep+    {altitude}
                     FloatToStrF(dist, ffFixed, 12, 1)+sep+  {distance_from_start}
                     s+sep+                                  {gps_update}
                     '0'+sep+                                {Speed}
                     BrTeilen(splitlist[17], 100, 2)+sep+    {Heading (yaw)}
                     BrTeilen(splitlist[16], 100, 2)+sep+    {pitch}
                     BrTeilen(splitlist[15], 100, 2)+sep+    {roll}
                     splitlist[2]+sep+                       {f_mode}
                     '0'+sep+                                {RSSI}
                     BrKorrV(splitlist[21])+sep+             {Voltage in %}
                     KorrSigned(splitlist[20], 63)+sep+'0'); {NumSats, Acc}
      end;
    end;                                           {Ende Daten einlesen}
  end;

  procedure RRYLegacy;
  var x: integer;
  begin
    for x:=1 to inlist.Count-1 do if CheckE7(inlist[x]) then begin
      splitlist.DelimitedText:=inlist[x];
      if (splitlist.Count>anzsp) and               {Konsistenz checken (YTH)}
          CheckVT(splitlist[StringGrid1.Tag+2],
                  splitlist[StringGrid1.Tag]) and
          testh(StrToFloatN(splitlist[4])) then begin
        try
          if ((NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
             (GetRFM(splitlist[StringGrid1.Tag],
                     SpinEdit3.Tag, true)) or
              CheckBox3.Checked) then begin
            lat2:=StrToFloatN(splitlist[5]);
            lon2:=StrToFloatN(splitlist[6]);
            bg:=ZeitToDT(splitlist[0], SpinEdit3.Tag);
{Startpunkt extra validieren, oben mit CheckBox3 () relativiert}
            if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
               GetRFM(splitlist[StringGrid1.Tag],
                      Spinedit3.Tag, true) then begin
              if (lat1=0) and
                 (lon1=0) then begin               {Startpunkt festlegen}
                dist:=0;
                lon1:=lon2;
                lat1:=lat2;
              end else begin                       {Distanz zum Startpunkt berechnen}
                dist:=DeltaKoord(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
              end;
            end else begin
              dist:=0;
              if IsMStart(splitlist[StringGrid1.Tag], splitlist[StringGrid1.Tag+2]) then begin
                lat1:=0;                           {Startpunkt zurücksetzen}
                lon1:=0;
              end;
            end;
            dashlist.add(FormatDateTime(zzf+'.zz', bg)+sep+      {Time}
                         ChrKoor(splitlist[6])+sep+              {lon}
                         ChrKoor(splitlist[5])+sep+              {lat}
                         splitlist[4]+sep+                       {altitude}
                         FloatToStrF(dist, ffFixed, 12, 1)+sep+  {distance_from_start}
                         KorrBool(splitlist[8])+sep+             {gps_update}
                         splitlist[7]+sep+                       {Speed}
                         splitlist[12]+sep+                      {Heading (yaw)}
                         splitlist[13]+sep+                      {pitch}
                         splitlist[11]+sep+                      {roll}
                         splitlist[StringGrid1.Tag]+sep+         {f_mode}
                         splitlist[1]+sep+                       {RSSI}
                         splitlist[2]+sep+                       {Voltage}
                         splitlist[10]+sep+                      {NumSats}
                         splitlist[StringGrid1.Tag+4]);          {Accuracy (gps_accH)}
          end;
        except
        end;
      end else begin
        StatusBar1.Panels[5].Text:=rsInvalid;
      end;
    end;
  end;

begin
  Screen.Cursor:=crHourGlass;
  inlist:=TStringList.Create;
  dashlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  lat1:=0;
  lon1:=0;
  case SpinEdit3.Tag of
    brID: fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
              ListBox1.Items[z]+bext;
  else                                             {Yuneec legacy}
    fn:=IncludeTrailingPathDelimiter(ComboBox2.Text)+
        kpath+kfile+ListBox1.Items[z]+fext;
  end;
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then
    try
      splitlist.DelimitedText:=inlist[0]; {Überschrift einlesen, f_mode ermitteln}
      fModeFinden(splitlist);                      {Position f-mode merken}
      splitlist.DelimitedText:=inlist[2];          {2. Datenzeile, Zeit}
      if (SpinEdit3.Tag<>YTHPid) and
         (SpinEdit3.Tag<>brID) then begin          {v_type ermitteln}
        SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
        OverWriteVT;                               {Overwrite for PX4 Thunderbird}
      end;
      dashlist.Add(rrk+'RaceRender Data');
      dashlist.Add(rrk+vtypeToStr(SpinEdit3.Tag));
      if SpinEdit3.Tag=brID then dashlist.Add(rrk+trim(inlist[5]));
      dashlist.Add('Time'+sep+'Longitude'+sep+'Latitude'+sep+'Altitude (m)'+sep+
                   'Distance (m)'+sep+'GPS_Update'+sep+'Speed (m/s)'+sep+
                   'Heading'+sep+'Pitch'+sep+'Roll'+sep+'FlightMode'+sep+
                   'RSSI'+sep+'Voltage'+sep+'NumSats'+sep+'Accuracy');
      if SpinEdit3.Tag=brID then begin             {Breeze}
        RRBreeze;
      end else begin
        RRYLegacy;
      end;                                         {Ende Daten aus Telemetry einlesen}
      if dashlist.count>10 then begin              {Datei speichern}
        fn:=ResultDN(fn, RadioGroup2.Items[RadioGroup2.ItemIndex]); {+_rr.csv}
        try
          dashlist.SaveToFile(fn);
          StatusBar1.Panels[5].Text:=fn+tab1+rsSaved;
          SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
        except
          StatusBar1.Panels[5].Text:=fn+tab1+rsNotSaved;
          SynEdit1.Lines.Add('''9469'+suff+StatusBar1.Panels[5].Text);
        end;
      end;
    except
      SynEdit1.Lines.Add('''9473'+suff+rsInvalid);
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(dashlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.MacheCCC(fn: string);             {Waypoints from Telemetrie}
var inlist, splitlist: TStringList;
    np, x: integer;
    wpele, dir, diralt: double;                    {Elevation and direction}
    dist, lat1, lat2, lon1, lon2: double;
    ahwp: array of TWayPH;                         {Array of waypoints}

begin
  Screen.Cursor:=crHourGlass;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  SetLength(ahwp, 0);                              {Array rücksetzen}
  np:=0;
  try
    inlist.LoadFromFile(fn);                       {Telemetriefile laden}
    if inlist.Count>50 then begin
      splitlist.DelimitedText:=inlist[2];          {2. Datenzeile, Zeit}
      if SpinEdit3.Tag<>YTHPid then begin          {v_type ermitteln}
        SpinEdit3.Tag:=StrToIntDef(splitlist[StringGrid1.Tag+2], 2);
        OverWriteVT;                               {Overwrite for PX4 Thunderbird}
      end;
      lat1:=0;
      lon1:=0;
      np:=0;
      SetLength(ahwp, np+1);
      for x:=1 to inlist.Count-1 do
      if CheckE7(inlist[x]) then
      try
        splitlist.DelimitedText:=inlist[x];
        wpele:=StrToFloatN(splitlist[4]);          {Höhe schon mal nehmen}
        if (splitlist.Count>anzsp) and             {Konsistenz checken (YTH)}
            CheckVT(splitlist[StringGrid1.Tag+2],
                    splitlist[StringGrid1.Tag]) and
           (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and
            testh(wpele) and
            GetRFM(splitlist[StringGrid1.Tag],
                   SpinEdit3.Tag, true) then begin
          lat2:=StrToFloatN(splitlist[5]);
          lon2:=StrToFloatN(splitlist[6]);
          dist:=DeltaKoord(lat1, lon1, lat2, lon2); {Entfernung zum letzten Punkt}
          dir:=StrToFloatN(splitlist[12]);         {direction (yaw)}
          if (dist>TrackBar2.Position) or
             (abs(dir-diralt)>deltayaw) then begin
            lat1:=lat2;
            lon1:=lon2;
            diralt:=dir;                           {save direction}
            if dist<distmax then begin             {Waypoint gefunden}
              ahwp[np].lat:=lat2;
              ahwp[np].lon:=lon2;
              ahwp[np].pindex:=np;
              ahwp[np].altitude:=wpele;
              ahwp[np].roll:=StrToFloatN(splitlist[11]);    {? oder 0}
              ahwp[np].yaw:=dir;
              ahwp[np].pitch:=StrToFloatN(splitlist[13]);
              ahwp[np].gimbalYam:=0;
              ahwp[np].gimbalPitch:=0;
              inc(np);
              SetLength(ahwp, np+1);
            end;
          end;
        end;
      except
        SetLength(ahwp, 0);                        {bei Fehler nichts laden}
        StatusBar1.Panels[5].Text:=rsInvalid;
        SynEdit1.Lines.Add(''''+capLabel6+Format('%6d', [x])+ {Datenpunkt ausgeben}
                           suff+StatusBar1.Panels[5].Text);
      end;
      SetLength(ahwp, High(ahwp));                 {letzten Eintrag löschen}
      if High(ahwp)>1 then begin                   {keinen einzelnen WP speichern}
        inlist.clear;
        inlist.add('{');
        inlist.add(tab4+hwphwp+'": [');
        for x:=0 to High(ahwp) do begin
          inlist.add(tab4+tab4+'{');
          inlist.add(tab4+tab4+tab4+hwpyaw+tr+FloatToStr(ahwp[x].yaw)+sep);
          inlist.add(tab4+tab4+tab4+hwproll+tr+FloatToStr(ahwp[x].roll)+sep);
          inlist.add(tab4+tab4+tab4+hwpidx+tr+IntToStr(x)+sep);
          inlist.add(tab4+tab4+tab4+hwpalt+tr+FloatToStr(ahwp[x].altitude)+sep);
          inlist.add(tab4+tab4+tab4+hwplon+tr+FloatToStr(ahwp[x].lon)+sep);
          inlist.add(tab4+tab4+tab4+hwplat+tr+FloatToStr(ahwp[x].lat)+sep);
          inlist.add(tab4+tab4+tab4+hwpptch+tr+FloatToStr(ahwp[x].pitch)+sep);
          inlist.add(tab4+tab4+tab4+hwpgyam+tr+FloatToStr(ahwp[x].gimbalYam)+sep);
          inlist.add(tab4+tab4+tab4+hwpgpit+tr+FloatToStr(ahwp[x].gimbalPitch)+sep);
          inlist.add(tab4+tab4+wpem);
        end;
        inlist[inlist.count-1]:=tab4+tab4+'}';     {überschreiben mit ohne Komma}
        inlist.add(tab4+']');
        inlist.Add('}');
        fn:=ResultDN(fn, 'CCC');
        inlist.SaveToFile(fn);
      end else begin
        StatusBar1.Panels[5].Text:=rsError;
        SynEdit1.Lines.Add('''8965'+suff+StatusBar1.Panels[5].Text);
      end;
      StatusBar1.Panels[5].Text:=IntToStr(np)+rsNumWP;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  finally
    ahwp:=nil;
    FreeAndNil(splitlist);
    FreeAndNil(inlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);         {Quelle wählen}
begin
  if (SpinEdit3.Tag=MQid) or                       {nichts tun für MantisQ}
     (SpinEdit3.Tag=H5id) then                     {nichts tun für H520}
       exit;
  if (SpinEdit3.Tag=YTHPid) and                    {H Plus Sensor files}
     (RadioGroup1.ItemIndex=3) then begin
       OpenSensorPlus;
  end else begin
    Anzeige;
  end;
  RadioGroup1.Tag:=0;                              {Resize columns needed again}
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);    {Format geändert}
begin
  EnSave;
  StatusBar1.Panels[2].Text:=RadioGroup2.Items[RadioGroup2.ItemIndex];
  BitBtn2.Hint:=hntBitBtn2+' ('+StatusBar1.Panels[2].Text+')';
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);    {MPH updaten}
begin
  SelDirAct('');
end;

procedure TForm1.RadioGroup5Click(Sender: TObject);
begin
  EnSave;                                              {Speichern erlauben}
end;

{Einstellungen für Schnellanalyse, Zuordnung der Spalten zu den Diagrammen}
procedure TForm1.SpeedButton1Click(Sender: TObject);   {1. Spalte zuordnen}
begin
  GetDDdata(LabeledEdit1);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);   {2. Spalte zuordnen}
begin
  GetDDdata(LabeledEdit2);
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);   {3. Spalte zuordnen}
begin
  GetDDdata(LabeledEdit3);
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);   {FlightLog Verzeichnis}
begin
  SelDirSet;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);   {Dir Protokoll}
begin
  SelDirProt;                  {Verzeichnis zum Durchsuchen auswählen}
end;

procedure TForm1.SendCGOcmd;                           {Command zu CGO3}
begin
  if Edit3.Text>'' then
    if CGO3run(Edit3.Text, 0)=0 then begin
      CGO3run('', 0);
    end else begin
      StatusBar1.Panels[5].Text:='Command not possible';
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);   {Command zu CGO3}
begin
  SendCGOcmd;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);     {Liniendicke}
begin
  EnSave;                                              {Speichern erlauben}
end;

procedure TForm1.StaticText1DblClick(Sender: TObject); {In Kopter ID übernehmen}
begin
  if StaticText1.Caption<>'' then
    ComboBox1.Text:=StaticText1.Caption; {KopterID}
end;

procedure TForm1.StatusToClipboard;                    {Statuszeile kopieren}
begin
  if StatusBar1.Tag>0 then                             {Zeiten mit kopieren}
    ClipBoard.AsText:=StatusBar1.Panels[3].Text+bind+
                      StatusBar1.Panels[4].Text+tab2+
                      StatusBar1.Panels[5].Text
  else
    ClipBoard.AsText:=StatusBar1.Panels[5].Text;
end;

procedure TForm1.StatusBar1DblClick(Sender: TObject);  {Copy bei Doppelclick}
begin
  StatusToClipboard;
end;

{http://www.lazarusforum.de/viewtopic.php?f=10&t=12130}
procedure TForm1.StatusBar1Hint(Sender: TObject);  {Reroute Application hint}
begin
  Statusbar1.Panels[5].Text:=Application.Hint;
end;

procedure TForm1.StringGrid1Click(Sender: TObject);{Kursor im Diagramm anzeigen}
var ts, tb, te: TDateTime;
begin
  Label3.Tag:=StringGrid1.Selection.Left;          {selektierte Spalte ermitteln}
  SpinEdit3.Value:=StringGrid1.Selection.Top;      {Zeile übernehmen}
  if (RadioGroup1.ItemIndex<>3) and
     (StringGrid1.ColCount<YTHPcols) then begin    {nicht bei Sensordateien}
    ts:=ZeitToDT(StringGrid1.Cells[0, StringGrid1.Row], SpinEdit3.Tag);
    if ts>0 then begin
      if (PageControl1.Tag>0) and
         (Form2.Chart1.Visible) then begin
        try
          tb:=ZeitToDT(StringGrid1.Cells[0, 1], SpinEdit3.Tag);
          te:=ZeitToDT(StringGrid1.Cells[0, StringGrid1.RowCount-1], SpinEdit3.Tag);
          Form2.MoveVCursor(ts, round((ts-tb)/(te-tb)*10000));
          Form2.edTime.Visible:=true;
          Form2.Chart1ConstantLine1.Active:=true;
        except
          Form2.Close;
        end;
      end;
    end;
  end;
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject); {Hilfe zum Zelleninhalt}
begin
  StatusBar1.Panels[5].Text:=GetCellInfo(StringGrid1.Col, StringGrid1.Row);
  SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);   {in AppLog aufnehmen}
end;

procedure TForm1.StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var dta, npos: double;
const mxw=1365;

  procedure Farblauf(f, w: integer);               {Farbabstufung zeichnen}
  var p: integer;   {f.. Farbe (index im Array), w..Wert}
  begin
    p:=abs(w);
    if p>mxw then
      p:=mxw;
    StringGrid1.Canvas.Brush.Color:=farbskala[f, p*High(farbskala[0])div mxw];
  end;

  procedure TelemBreeze;                           {Telemetrie Breeze}
  var e: integer;
  begin
    case aCol of
       2: begin                                    {Flight Mode}
            e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
            case e of
              2, 4, 5, 7: StringGrid1.Canvas.Brush.Color:=clTasks;
              8:  StringGrid1.Canvas.Brush.Color:=clRTH;
              6:  StringGrid1.Canvas.Brush.Color:=clAngle;  {Pilot}
              10: StringGrid1.Canvas.Brush.Color:=clNoGPS;
            end;
          end;
      14: begin                                    {AutoTakeOff}
            e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
            Case e of
              2:     StringGrid1.Canvas.Brush.Color:=clSkyBlue; {Flug}
              1, 18: StringGrid1.Canvas.Brush.Color:=clMoneyGreen;
              16:    StringGrid1.Canvas.Brush.Color:=clAttention;
            end;
          end;
      18: if (StringGrid1.Cells[aCol, aRow]<>'15') then         {Motorschaden}
            StringGrid1.Canvas.Brush.Color:=clRed;
      19: begin                                    {error Zellen einfärben}
            e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);   {Errorflag}
            if ((e and 1)<>0) or ((e and 2)<>0) then
              StringGrid1.Canvas.Brush.Color:=clSkyBlue;
            if (e shr 2)<>0 then
              StringGrid1.Canvas.Brush.Color:=clOrange;
          end;
      20: if BrGPSfix(StringGrid1.Cells[aCol, aRow]) then
            StringGrid1.Canvas.Brush.Color:=clMoneyGreen;       {Num Sats}
    end;
  end;

  procedure TelemYTHP;                             {Telemetrie YTH Plus}
  var e: integer;
      h, v: double;
  begin
    if CheckVT(StringGrid1.Cells[21, aRow],
               StringGrid1.Cells[19, aRow]) then begin {nur mit gültiger vehicle ID}
      case aCol of
         1: if (StringGrid1.Cells[aCol, aRow]>'') then begin   {RSSI}
              try
                h:=abs(StrToFloatN(StringGrid1.Cells[aCol, aRow]));
              except
                h:=0;                              {Max}
              end;
              if h>85 then begin
                StringGrid1.Canvas.Brush.Color:=clRed;
                exit;
              end;
              if (h<=85) and
                 (h>70) then begin
                StringGrid1.Canvas.Brush.Color:=clAttention;
                exit;
              end;
              if (h<=70) and
                 (h>=55) then begin
                StringGrid1.Canvas.Brush.Color:=clMoneyGreen;
                exit;
              end;
              if (h>0) and
                 (h<55) then StringGrid1.Canvas.Brush.Color:=clGreen;
              exit;
            end;
         4: begin                                  {Höhe}
              try
                if RadioGroup3.ItemIndex=2 then
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow])/fft  {in ft}
                else
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow]);     {in m}
                v:=StrToFloatN(GetFNr(grdOverview.Cells[5, ListBox1.ItemIndex+1]));
              except
                h:=0;
              end;
              if (h>1) and
                 testh(h) and
                 (v>0) and
                 (h+0.1>=v) then
                StringGrid1.Canvas.Brush.Color:=clYellow;           {Gipfelhöhe}
              exit;
            end;
         7: begin                     {True Air Speed, nicht für vSpeed, hSpeed}
              try
                if RadioGroup3.ItemIndex=2 then
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow])*fmph  {in mph}
                else
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow])*fkmh; {in km/h}
                v:=StrToFloatN(GetFNr(grdOverview.Cells[8, ListBox1.ItemIndex+1]));
              except
                h:=0;
              end;
              if (aCol=7) and
                 (h>1) and
                 (v>0) and
                 (h+0.1>=v) then                   {Korrekturwert wegen Runden}
               StringGrid1.Canvas.Brush.Color:=clYellow;             {Topspeed}
              exit;
            end;
        19: begin                                  {Flight mode}
              e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
              case e of                            {flight modes wie Chart1BarSeries}
                4:   StringGrid1.Canvas.Brush.Color:=clNoGPS;  {Manual Mode, no GPS}
                5:   StringGrid1.Canvas.Brush.Color:=clAngle;
                6:   StringGrid1.Canvas.Brush.Color:=clSmart;
                7:   StringGrid1.Canvas.Brush.Color:=clSport;
                12, 13: StringGrid1.Canvas.Brush.Color:=clRTH;
              end;
            end;
      end;
      if aCol=StringGrid1.Tag+4 then begin         {GPS horizontale accuracy}
        try
          h:=StrToFloatN(StringGrid1.Cells[aCol, aRow]);
        except
          h:=10;                                   {Max}
        end;
        if h>2.5 then begin
          StringGrid1.Canvas.Brush.Color:=clRed;
          exit;
        end;
        if (h<=2.5) and (h>1.8) then begin
          StringGrid1.Canvas.Brush.Color:=clAttention;
          exit;
        end;
        if (h<=1.8) and (h>=1) then begin
          StringGrid1.Canvas.Brush.Color:=clMoneyGreen;
          exit;
        end;
        if (h<1) and
           (h>0) then
          StringGrid1.Canvas.Brush.Color:=clGreen;
        exit;
      end;
    end;
  end;

{http://www.metageek.com/training/resources/understanding-rssi.html  80-70-67
 andere Quellen (Veris Aeropond)                                     80-70-55
 ich habe für mich definiert:                                        85-70-55}

  procedure TelemRest;                             {Telemetrie aller anderen Kopter}
  var e: integer;
      h, v: double;
  begin
    if StringGrid1.Cells[StringGrid1.Tag+2, aRow]>'0' then begin {nur mit gültiger vehicle ID}
      case aCol of
         1: if (StringGrid1.Cells[aCol, aRow]>'') then begin   {RSSI}
              try
                h:=abs(StrToFloatN(StringGrid1.Cells[aCol, aRow]));
              except
                h:=0;                              {Max}
              end;
              if h>85 then begin
                StringGrid1.Canvas.Brush.Color:=clRed;
                exit;
              end;
              if (h<=85) and (h>70) then begin
                StringGrid1.Canvas.Brush.Color:=clAttention;
                exit;
              end;
              if (h<=70) and (h>=55) then begin
                StringGrid1.Canvas.Brush.Color:=clMoneyGreen;
                exit;
              end;
              if (h>0) and (h<55) then
                StringGrid1.Canvas.Brush.Color:=clGreen;
              exit;
            end;
         4: begin                                  {Höhe}
              try
                if RadioGroup3.ItemIndex=2 then
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow])/fft  {in ft}
                else
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow]);     {in m}
                v:=StrToFloatN(GetFNr(grdOverview.Cells[5, ListBox1.ItemIndex+1]));
              except
                h:=0;
              end;
              if (h>1) and
                 testh(h) and
                 (v>0) and
                 (h+0.1>=v) then
                StringGrid1.Canvas.Brush.Color:=clYellow;  {Gipfelhöhe}
              exit;
            end;
         7: begin                                  {True Air Speed}
              try
                if RadioGroup3.ItemIndex=2 then
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow])*fmph  {in mph}
                else
                  h:=StrToFloatN(StringGrid1.Cells[aCol, aRow])*fkmh; {in km/h}
                v:=StrToFloatN(GetFNr(grdOverview.Cells[8, ListBox1.ItemIndex+1]));
              except
                h:=0;
              end;
              if (aCol=7) and
                 (h>1) and
                 (v>0) and
                 (h+0.1>=v) then                   {Korrekturwert wegen Runden}
                StringGrid1.Canvas.Brush.Color:=clYellow; {Topspeed}
              exit;
            end;
        14: begin                                  {Motorstatus}
              if (StringGrid1.Cells[aCol, aRow]<>'15') and
                 (StringGrid1.Cells[aCol, aRow]<>'63') and
                 (StringGrid1.Cells[aCol, aRow]<>'-1') and
                 (StringGrid1.Cells[aCol, aRow]<>'255') then
                StringGrid1.Canvas.Brush.Color:=clRed; {Motorschaden}
              exit;
            end;
      end;
      if aCol=(StringGrid1.Tag+3) then begin {error Zellen einfärben}
        e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0); {Errorflag}
        if e>0 then begin
          if ((e and 1)<>0) or ((e and 2)<>0) then
            StringGrid1.Canvas.Brush.Color:=clSkyBlue;
          if (e shr 2)<>0 then
            StringGrid1.Canvas.Brush.Color:=clOrange;
        end;
        exit;
      end;
      if aCol=StringGrid1.Tag then begin           {f_mode Zellen einfärben}
        e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
        if SpinEdit3.Tag=3 then begin              {350QX}
          case e of                                {flight modes; wie Chart1BarSeries}
            25:   StringGrid1.Canvas.Brush.Color:=clAngle;
            11:   StringGrid1.Canvas.Brush.Color:=clSport;
            9, 14, 10, 13:  StringGrid1.Canvas.Brush.Color:=clRTH;
            12:   StringGrid1.Canvas.Brush.Color:=clSmart;
            8:    StringGrid1.Canvas.Brush.Color:=clEmergency;
            5:    StringGrid1.Canvas.Brush.Color:=clSilver;     {Motor Starting}
          end;
        end else begin                             {Q500 and all others}
          case e of                                {flight modes wie Chart1BarSeries}
            3, 4: StringGrid1.Canvas.Brush.Color:=clAngle;
            2, 5, 7, 22, 24, 32: StringGrid1.Canvas.Brush.Color:=clNoGPS;
            13, 14, 20: StringGrid1.Canvas.Brush.Color:=clRTH;
            6, 21, 23: StringGrid1.Canvas.Brush.Color:=clSmart;
            9, 10, 11, 12, 17, 18: StringGrid1.Canvas.Brush.Color:=clEmergency;
            0, 1: StringGrid1.Canvas.Brush.Color:=clSport;
            26..29, 33: StringGrid1.Canvas.Brush.Color:=clTasks;  {Tasks}
            8:          StringGrid1.Canvas.Brush.Color:=clSilver; {Motor Starting}
          end;
        end;
        exit;
      end;
      if aCol=StringGrid1.Tag+4 then begin         {GPS horizontale accuracy}
        try
          h:=StrToFloatN(StringGrid1.Cells[aCol, aRow]);
        except
          h:=10;                                   {Max}
        end;
        if h>2.5 then begin
          StringGrid1.Canvas.Brush.Color:=clRed;
          exit;
        end;
        if (h<=2.5) and (h>1.8) then begin
          StringGrid1.Canvas.Brush.Color:=clAttention;
          exit;
        end;
        if (h<=1.8) and (h>=1) then begin
          StringGrid1.Canvas.Brush.Color:=clMoneyGreen;
          exit;
        end;
        if (h<1) and (h>0) then StringGrid1.Canvas.Brush.Color:=clGreen;
        exit;
      end;
    end;
  end;

  procedure Telemetrie;                            {Farben bei Telemetry je Typ}
  begin
    if StringGrid1.Cells[aCol, aRow]>'' then begin
      case SpinEdit3.Tag of
        brID: TelemBreeze;                         {Breeze}
        YTHPid: TelemYTHP;                         {YTH Plus}
        else
          TelemRest;                               {alle anderen Kopter}
      end;
    end;
  end;

{Anzeige Nummer Satelliten auf der ST16s in Farbe laut
 https://yuneecpilots.com/threads/ccc-mode-with-the-h.13103/page-4}
  procedure RC_GPS;                                {Farben bei RemoteGPS}
  var e: integer;
  begin
    if (SpinEdit3.Tag=YTHPid) and                  {nur bei YTH Plus}
       (aCol=4) then begin                         {SatelliteCount}
      e:=StrToIntDef(StringGrid1.Cells[aCol, aRow], 0);
      if e>0 then begin
        if e>10 then begin                         {11 bis x Sats grün}
          StringGrid1.Canvas.Brush.Color:=clGreen;
          exit;
        end;
        if e<5 then begin                          {1 bis 4 Sats rot}
          StringGrid1.Canvas.Brush.Color:=clRed;
          exit;
        end else begin                             {5 bis 10 Sats gelb}
          StringGrid1.Canvas.Brush.Color:=clYellow;
          exit;
        end;
      end else exit;
    end;
  end;

  procedure RC_Ch;                                 {Farben bei Remote (Channels)}
  var e, p: integer;
  begin
    if StringGrid1.Cells[aCol, aRow]>'' then begin
      e:=round(trunc(StrToFloatN(StringGrid1.Cells[aCol, aRow])));
      case aCol of
        1: begin
             p:=e-stkntrl;
             if p<0 then Farblauf(1, p)            {nach unten}
                    else Farblauf(0, p);           {nach oben}
             if e=0 then                           {rote Taste}
               StringGrid1.Canvas.Brush.Color:=clAttention;
           end;
        2, 3, 4:
           begin {die restlichen Knüppel}
             p:=e-stkntrl;
             if p<0 then Farblauf(1, p)            {nach unten}
                    else Farblauf(0, p);           {nach oben}
           end;
        5: begin                                   {Flight mode switch}
             if e=stkntrl then begin               {Angle}
               StringGrid1.Canvas.Brush.Color:=clAngle;
               exit;
             end;
             if e=stkup then begin                 {SportMode: blau}
               if SpinEdit3.Tag=YTHPid then
                 StringGrid1.Canvas.Brush.Color:=clSport
               else                                {Smart}
                 StringGrid1.Canvas.Brush.Color:=clSmart;
               exit;
             end;
             if e=stkdown then                     {RTH}
               StringGrid1.Canvas.Brush.Color:=clRTH;
          end;
      end;
    end;
  end;

  procedure FarbenSensor;
  begin
    if (aCol<lenfix-3) then
      StringGrid1.Canvas.Brush.Color:=clMoneyGreen;   {die wirklich fixen Bytes}
    if (aCol=lenfix-3) then
      StringGrid1.Canvas.Brush.Color:=clOlive;     {Message ID (0)}
    if StringGrid1.ColCount>=YTHPcols then begin   {YTH Plus}
      if (aCol=lenfix-2) or
          (aCol=lenfix-1) then
        StringGrid1.Canvas.Brush.Color:=clOlive;   {Message ID 1 und 2}
      if (aCol=lenfix) then
       StringGrid1.Canvas.Brush.Color:=clMoneyGreen;    {Message Name YTH Plus}
      if (aCol=lenfix+1) then
        StringGrid1.Canvas.Brush.Color:=clSilver;  {Längenspalte YTH Plus}
    end else begin
      if (aCol=lenfix-2) then
        StringGrid1.Canvas.Brush.Color:=clSilver;  {Längenspalte andere mit Sensor Datei}
    end;
{und was auch immer noch bei Sensor Datei}
  end;

  procedure FarbenLegacy;
  begin
    if (aCol=0) and
       (aRow>1) then begin                         {Frequenz Timestamps}
      tpos:=ZeitToDT(StringGrid1.Cells[0, aRow], SpinEdit3.Tag);
      npos:=ZeitToDT(StringGrid1.Cells[0, aRow-1], SpinEdit3.Tag);
      dta:=tpos-npos;                              {Delta zur vorigen Zeile}
      if SpinEdit3.Tag=brID then begin             {Yuneec Breeze}
        if dta>tsdelta2 then
          StringGrid1.Canvas.Brush.Color:=clAttention;
        if dta>tsdelta3 then
          StringGrid1.Canvas.Brush.Color:=clRed;   {5sec für Breeze}
      end else begin                               {andere Kopter}
        if RadioGroup1.ItemIndex=1 then begin      {RemoteGPS}
          if dta>tsdelta2 then                     {2sec, für YTH > 1 sec}
            StringGrid1.Canvas.Brush.Color:=clAttention;
          if dta>tsdelta3 then                     {5sec}
            StringGrid1.Canvas.Brush.Color:=clRed;
        end else begin                             {Telemetry/Remote}
          if dta>tsdelta1 then                     {600ms}
            StringGrid1.Canvas.Brush.Color:=clAttention;
          if dta>tsdelta2 then
            StringGrid1.Canvas.Brush.Color:=clRed; {2sec}
        end;
      end;
      if npos-tpos>tsdelta1 then                   {Zeitrücksprung}
        StringGrid1.Canvas.Brush.Color:=clLime;
      exit;
    end;
    case RadioGroup1.ItemIndex of                  {abh. von Dateityp}
      0: Telemetrie;
      1: RC_GPS;
      2: RC_Ch;
    end;
  end;

  procedure FarbenPX4csv;                          {Einfärbungen bei PX4 CSV}
  begin
    case aCol of

      posChan-1: StringGrid1.Canvas.Brush.Color:=clOlive;    {Message ID}
    end;
  end;

begin                                              {Main part}
  if (aRow>0) and                                  {nicht in Überschrift malen}
     (aState=[]) then begin                        {nicht, wenn selected}
    if StringGrid1.ColCount=csvanz then begin
      FarbenPX4csv;
    end else begin
      if (StringGrid1.ColCount>=YTHPcols) or
         (RadioGroup1.ItemIndex=3) then begin      {Sensor}
        FarbenSensor;
      end else begin                               {Rest wie gehabt}
        FarbenLegacy;
      end;
    end;
  end;
end;

procedure TForm1.DiaWerte(p: integer);   {Anzeige Diagramm Werte für Spalte p}
var x, b: integer;
    w, lat1, lon1: double;
    bg: TDateTime;
    s: string;

{Die Y-Achsenbezeichnung für die Diagramme entsprechend der Spalten anpassen}
  procedure PrepBreeze;                            {Breeze vorbereiten}
  begin
    case p of                                      {Telemetrie Breeze}
      10: s:=s+' [m]';
      12, 13: s:=rsDistHome+' [m]';
      15, 16, 17: s:=s+' [°]';
      21: s:=s+' [%]';                             {Breeze: Restkapazität}
    end;
  end;

  procedure PrepYTHPlus;                           {YTH Plus vorbereiten}
  begin
    b:=2;                                          {mit 2. Zeile beginnen}
    case RadioGroup1.ItemIndex of
      0: begin                                     {Telemetry}
            case p of
              1: s:=s+' [dBm]';
              2: s:=s+' [V]';
              3: s:=rsRest+' [%]';
              4: s:=s+' [m]';
              5, 6: s:=rsDistHome+' [m]';
              7, 24, 25: s:=s+' ['+RadioGroup3.Items[RadioGroup3.ItemIndex]+']';  {selected speed}
              11, 12, 13: s:=s+' [°]';
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST16}
              1, 2: s:=rsDistHome+' [m]';
              3: s:=s+' [m]';
              5: s:=s+tab1+spath;                   {Accuracy GPS ST16}
//              6: s:=s+' ['+RadioGroup3.Items[RadioGroup3.ItemIndex]+']';
              7: s:=s+' [°]';
            end;
         end;
      2: begin                                     {Remote}
           s:=ChToStr(s, p);                       {RC data, CHx bezeichnen}
         end;
    end;
  end;

  procedure PrepYlegacy;                           {Alle anderen vorbereiten}
  begin
    case RadioGroup1.ItemIndex of
      0: begin                                     {Telemetry}
            case p of
              1: s:=s+' [dBm]';
              2: s:=s+' [V]';
              3: if SpinEdit3.Tag=1 then s:=s+' [A]';  {nur H920}
              4: s:=s+' [m]';
              5, 6: s:=rsDistHome+' [m]';
              7, 24, 25: s:=s+' ['+RadioGroup3.Items[RadioGroup3.ItemIndex]+']';  {selected speed}
              11, 12, 13: s:=s+' [°]';
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST10}
              1, 2: s:=rsDistHome+' [m]';
              3: s:=s+' [m]';
              4: s:=s+tab1+spath;                   {Accuracy GPS ST10}
//              5: s:=s+' ['+RadioGroup3.Items[RadioGroup3.ItemIndex]+']';
              6: s:=s+' [°]';
            end;
         end;
      2: begin                                     {Remote}
           s:=ChToStr(s, p);                       {RC data, CHx bezeichnen}
//           s:=ChToStr('', p); {RC data, Chx wie Channel settings}
         end;
    end;
  end;

  procedure PrepPX4csv;                            {PX4 self-dev CSV  vorbereiten}
  begin
    case p of                                      {PX4 CSV Maßeinheiten}
      1, 14, 46, 47: s:=s+' [%]';
      2: s:=s+' [V]';
      3: s:=s+' [A]';
      4, 21, 22, 37..40: s:=s+' [m]';
      5, 6: s:=rsDistHome+' [m]';
      7, 25, 41..43, 48: s:=s+' [m/s]';
      26..28: s:=s+' [m/s²]';
      11..13: s:=s+' [rad]';
      29..31: s:=s+' [rad/s]';
      32..34: s:=s+' [gauss]';
      35, 36: s:=s+' [mbar]';
      44: s:=s+' [°]';
      45: s:=s+' [°C]';
      60..78: s:=s+' [µs]';
    end;
  end;

{Die Zeitachse und Werte entsprechend der Datenformate in den Spalten
 anpassen und Diagramme anlegen}

  procedure ShowDiaBreeze;                         {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(StringGrid1.Cells[0, x], SpinEdit3.Tag);  {Zeitstempel}
    w:=StrToFloatN(StringGrid1.Cells[p, x]); {default: Wert einfach übernehmen}
    case p of                                      {Telemetrie Breeze}
      12, 13: begin                                {Koordinaten}
                 if ((lat1<>0) or (lon1<>0)) and {Koordinaten vorhanden}
                    BrGPSfix(StringGrid1.Cells[20, x]) then begin {GPS-Fix}
                   w:=DeltaKoord(lat1, lon1, BrCoordToFloat(StringGrid1.Cells[12, x]),
                                             BrCoordToFloat(StringGrid1.Cells[13, x]));
                 end else begin                    {keine gültigen Koordinaten}
                   lat1:=BrCoordToFloat(StringGrid1.Cells[12, x]);
                   lon1:=BrCoordToFloat(StringGrid1.Cells[13, x]);
                   w:=0;
                 end;
              end;
      10, 15, 16, 17: w:=w/100;                    {Altitude, roll, pitch, yaw}
      20: w:=StatusToByte(StringGrid1.Cells[p, x]) and 31;  {Number sats}
      21: w:=BrUmrech(w);                          {Spannung in % beim Breeze}
    end;
  end;

  procedure ShowDiaYTHPlus;                        {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(StringGrid1.Cells[0, x], SpinEdit3.Tag);  {Zeitstempel}
    case RadioGroup1.ItemIndex of                  {Ausgewählte Datei}
      0: begin                                     {Telemetry}
           if CheckVT(StringGrid1.Cells[StringGrid1.Tag+2, x],
                      StringGrid1.Cells[StringGrid1.Tag, x]) then begin
              w:=StrToFloatN(StringGrid1.Cells[p, x]); {default: Wert einfach übernehmen}
              case p of                            {Liste der Spalten für Dia}
                5, 6: begin
                        if x>1 then begin          {1. Zeile ignorieren}
                          if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                            w:=DeltaKoord(lat1, lon1, StrToFloatN(StringGrid1.Cells[5, x]),
                                                      StrToFloatN(StringGrid1.Cells[6, x]));
                          end else begin           {noch kein Startpunkt, Startpunkt setzen}
                            if testh(StrToFloatN(StringGrid1.Cells[4, x])) then begin
                              lat1:=StrToFloatN(StringGrid1.Cells[5, x]);
                              lon1:=StrToFloatN(StringGrid1.Cells[6, x]);
                            end;
                            w:=0;
                          end;
                        end;
                      end;
                7, 24, 25: SpeedX(w);
              end;
              if (p=4) and (not testh(w)) then w:=0; {Korrektur unplausibler Höhe}
           end;                                    {Ende Blödsinn ausblenden}
         end;
      1: begin                                     {RemoteGPS}
            w:=StrToFloatN(StringGrid1.Cells[p, x]); {default: Wert einfach übernehmen}
            case p of                              {ST16}
              1,2: if (lat1<>0) or (lon1<>0) then begin
                     w:=DeltaKoord(lat1, lon1, StrToFloatN(StringGrid1.Cells[2, x]),
                                   StrToFloatN(StringGrid1.Cells[1, x]));
                   end else begin
                     if testh(StrToFloatN(StringGrid1.Cells[3, x])) then begin
                       lat1:=StrToFloatN(StringGrid1.Cells[2, x]);
                       lon1:=StrToFloatN(StringGrid1.Cells[1, x]);
                     end;
                     w:=0;
                   end;
//                    6:   SpeedX(w);              {Maßeinheit Speed unklar}
            end;
            if (p=3) and (not testh(w)) then w:=0; {Korrektur unplausibler Werte (Höhe)}
         end;
      2: begin                                     {Remote: nur Angle}
           w:=StrToFloatN(StringGrid1.Cells[p, x]); {default: Wert einfach übernehmen}
           if p=7 then w:=TiltToGrad(StrToFloatN(StringGrid1.Cells[p, x]));
         end;
    end;
  end;

  procedure ShowDiaYlegacy;                        {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(StringGrid1.Cells[0, x], SpinEdit3.Tag);  {Zeitstempel}
    w:=StrToFloatN(StringGrid1.Cells[p, x]);       {default: Wert einfach übernehmen}
    case RadioGroup1.ItemIndex of                  {Gewählte Datei}
      0: begin                                     {Telemetry}
            case p of                              {Liste der Spalten für Dia}
              3: if SpinEdit3.Tag=1 then
                   w:=H920Amp(w);                  {Stromsensor bei H920}
              4: if not testh(w) then
                   w:=0;                           {Korrektur unplausibler Höhe}
              5, 6: begin
                      if x>1 then begin            {1. Zeile ignorieren}
                        if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                          w:=DeltaKoord(lat1, lon1, StrToFloatN(StringGrid1.Cells[5, x]),
                                                    StrToFloatN(StringGrid1.Cells[6, x]));
                        end else begin             {Startpunkt erst setzen}
                          if testh(StrToFloatN(StringGrid1.Cells[4, x])) then begin
                            lat1:=StrToFloatN(StringGrid1.Cells[5, x]);
                            lon1:=StrToFloatN(StringGrid1.Cells[6, x]);
                          end;
                          w:=0;
                        end;
                      end;
                    end;
              7: SpeedX(w);
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST10}
              1,2: if (lat1<>0) or (lon1<>0) then begin
                     w:=DeltaKoord(lat1, lon1, StrToFloatN(StringGrid1.Cells[2, x]),
                                   StrToFloatN(StringGrid1.Cells[1, x]));
                   end else begin
                     if testh(StrToFloatN(StringGrid1.Cells[3, x])) then begin
                       lat1:=StrToFloatN(StringGrid1.Cells[2, x]);
                       lon1:=StrToFloatN(StringGrid1.Cells[1, x]);
                     end;
                     w:=0;
                   end;
//                  5:  SpeedX(w);                 {Maßeinheit Speed unklar}
            end;
            if (p=3) and (not testh(w)) then w:=0; {Korrektur unplausibler Werte (Höhe)}
         end;
      2: begin                                     {Remote}
           if p=7 then w:=TiltToGrad(StrToFloatN(StringGrid1.Cells[p, x]));
         end;
    end;
  end;

  procedure ShowDiaPX4csv;                         {Zeit + wert ermitteln}
  begin
    bg:=ScanDateTime(zzf+zzz, StringGrid1.Cells[0, x]);
    w:=0;
    if StringGrid1.Cells[p, x]<>'' then begin
      w:=StrToFloatN(StringGrid1.Cells[p, x]);     {default: Wert einfach übernehmen}
      case p of                                    {CSV Datei, Spalte p}
        4: if not testh(w) then
             w:=0;                                 {Korrektur unplausibler Höhe}
        5, 6: begin                                {Entfernung zum Startpunkt}
                if x>1 then begin                  {1. Zeile ignorieren}
                  if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                    w:=DeltaKoord(lat1, lon1, StrToFloatN(StringGrid1.Cells[5, x]),
                                              StrToFloatN(StringGrid1.Cells[6, x]));
                  end else begin                   {Startpunkt erst setzen}
                    if testh(StrToFloatN(StringGrid1.Cells[4, x])) then begin
                      lat1:=StrToFloatN(StringGrid1.Cells[5, x]);
                      lon1:=StrToFloatN(StringGrid1.Cells[6, x]);
                    end;
                    w:=0;
                  end;
                end;
              end;
      end;
    end;
  end;

begin
  if p>0 then begin                                {Spalte Datum/Zeit ausschließen}
    DoForm2Show(0);
    PageControl1.Tag:=1;
    w:=0;
    b:=1;                                          {1. Zeile für Auswertung}
    lat1:=0;
    lon1:=0;
    s:=StringGrid1.Cells[p, 0];
    Form2.Caption:=rsChart+' "'+s+'"';
    if Radiogroup1.ItemIndex=2 then Form2.Caption:=ChToStr(Form2.Caption, p);
    Form2.StringGrid1.Visible:=false;
    if StringGrid1.ColCount=csvanz then begin      {eigenes PX4 CSV Format}
      PrepPX4csv;
    end else begin
      case SpinEdit3.Tag of
        brID: PrepBreeze;                          {Y-Achsen vorbereiten}
        YTHPid: PrepYTHPlus;                       {YTH Plus}
      else
        PrepYlegacy;
      end;
    end;                                           {Ausgabe in s}

    Form2.Chart1.AxisList[0].Title.Caption:=s;     {y-Achse bezeichnen}
    Form2.Chart1.AxisList[0].LabelSize:=lblsize;   {y-Achse ausrichten}
    Form2.Chart1.Visible:=true;
    Form2.Chart1LineSeries1.Clear;
    Form2.Chart1.ZoomFull;
     for x:=b to StringGrid1.RowCount-1 do begin
      try
        if StringGrid1.ColCount=csvanz then begin  {PX4 CSV}
          ShowDiaPX4csv;
        end else begin
          case SpinEdit3.Tag of
            brID: ShowDiaBreeze;                   {Breeze}
            YTHPid: ShowDiaYTHPlus;                {YTH Plus}
          else                                     {andere Kopter}
            ShowDiaYlegacy;
          end;
        end;
      except
        w:=0;
      end;
      try
        Form2.Chart1LineSeries1.AddXY(bg, w);
      except
        Form2.Close;
      end;
    end;
    Form2.Refresh;
  end;                                             {Ende p>0}
end;

procedure TForm1.ZhlWerte(p: integer);             {Anzeige Statistik Werte}
type
  sdat=Record
    Value: string;
    Count: integer;
    Beginn, Ende: TDateTime;
end;

const
      zagf=zzf;                                    {Zeitausgabeformat}
      dagf='nn:ss';                                {Dauerausgabeformat}

var
  a: Array of sdat;                        {schneller mit dyn. Array by Corpsman}
  i, j: integer;
  b: boolean;                                      {kein neuer Wert}
  s:     string;                                   {neuer Wert, alter Wert}
  six:  boolean;                                   {Bedingung für Sechs Spalten}

  procedure AusgWe(ag: sdat);                      {Statistikdaten ausgeben}

    procedure vbd;                                 {Ausgabe von - bis - Dauer}
    var dur: TDateTime;
    begin
      Form2.StringGrid1.Cells[3, Form2.StringGrid1.RowCount-1]:=
        FormatDateTime(zagf, ag.Beginn);
      Form2.StringGrid1.Cells[4, Form2.StringGrid1.RowCount-1]:=
        FormatDateTime(zagf, ag.Ende);
      dur:=round((ag.Ende-ag.Beginn)*secpd)/secpd;
      Form2.StringGrid1.Cells[5, Form2.StringGrid1.RowCount-1]:=
        FormatDateTime(dagf, dur);
    end;

    procedure ZhlBreeze;
    begin
      try                                          {3. Spalte füllen}
        case p of
          2:  begin                          {Breeze Flight Mode}
                Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  BrfmodeToStr(StrToInt(ag.Value));
                vbd;
              end;
          11: Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                brIMUStatusToStr(StatusToByte(ag.Value));  {Breeze IMU Status}
          14: begin                         {Breeze AutoTakeOff}
                Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  AutoTakeOffToStr(StrToInt(ag.Value));
                vbd;
              end;
          18: Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                MotStatusToStr(StatusToByte(ag.Value));  {motor_status}
          19: begin                         {error flag}
                Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  eflagToStr(ag.Value);
                vbd;
              end;
        end;
      except
        SynEdit1.Lines.Add(rsError+' during count of values at Breeze');
      end;
    end;

    procedure ZhlYLegacy;
    begin
      if RadioGroup1.ItemIndex=0 then
      try
        if p=8 then begin                          {GPS used}
          vbd;
        end;
        if p=14 then begin                         {motor_status}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            MotStatusToStr(StatusToByte(ag.Value));
        end;
        if (p=15) and
           (StringGrid1.Tag<>17) then              {imu_status}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            IMUstatusToStr(StatusToByte(ag.Value));
        if (p=StringGrid1.Tag-1) and
           (StringGrid1.Tag<>17) then              {press_compass_status}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            PCGstatusToStr(StatusToByte(ag.Value));
        if p=StringGrid1.Tag then begin            {f_mode}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            fmodeToStr(StrToInt(ag.Value));
          vbd;
        end;
        if p=StringGrid1.Tag+2 then                {vehicle type}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            vtypeToStr(StrToInt(ag.Value));
        if p=StringGrid1.Tag+3 then begin          {error flag}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            eflagToStr(ag.Value);
          vbd;
        end;
      except
        SynEdit1.Lines.Add(rsError+' during count of values at Telemetry');
      end;
      if RadioGroup1.ItemIndex=2 then
      try
        if (p=5) then begin                        {Flight Mode}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            SwitchToStr(p, ag.Value);
          vbd;
        end;
        if (p=6) or (p=9) or (p=10) then           {Rest}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            SwitchToStr(p, ag.Value);
        if p=11 then                               {Landing gear}
          Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
            LandGearToStr(ag.Value);
      except
        SynEdit1.Lines.Add(rsError+' during count of values at Remote');
      end;
    end;

  begin
    Form2.StringGrid1.RowCount:=Form2.StringGrid1.RowCount+1;
    Form2.StringGrid1.Cells[0, Form2.StringGrid1.RowCount-1]:=FormSR(ag.Value, 4);
    Form2.StringGrid1.Cells[1, Form2.StringGrid1.RowCount-1]:=IntToStrFL(ag.Count, 6);
    if StringGrid1.ColCount=csvanz then begin      {eigenes PX4 CSV Format}
      try                                          {3. Spalte füllen}
        if ag.Value<>'' then begin                 {leere Zellen nicht auflösen}
          case p of
            15: Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  MSenStat(Hex2Dec('$'+ag.Value)); {Sensor health}
            18: Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  MSTtoStr(Hex2Dec('$'+ag.Value)); {MAV state}
            19: Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  MMFtoStr(Hex2Dec('$'+ag.Value)); {MAV mode flag}
            59: Form2.StringGrid1.Cells[2, Form2.StringGrid1.RowCount-1]:=
                  MsgIDtoStr(StrToInt(ag.Value));  {MAV message ID}
          end;
        end;
      except
        SynEdit1.Lines.Add(rsError+' during count of PX4 CSV values');
      end;
    end else
    if SpinEdit3.Tag=brID then begin               {Breeze}
      ZhlBreeze;
    end else begin                                 {all others}
      ZhlYLegacy;
    end;
  end;

  procedure DreiSpalten;  {zusätzliche Kommentarspalte in Statistik anlegen}
  begin
    Form2.StringGrid1.ColCount:=3;                 {Kommentarspalte anfügen}
    Form2.OnResize(Form2);                         {Spalten anpassen}
    Form2.StringGrid1.Cells[2, 0]:=rsDescript;     {Beschreibung}
  end;

  procedure SechsSpalten;        {zusätzliche Kommentarspalte + Beginn/Ende}
  begin
    Form2.StringGrid1.ColCount:=6;                 {Drei Spalten anfügen}
    Form2.OnResize(Form2);                         {Spalten anpassen}
    Form2.StringGrid1.Cells[2, 0]:=rsDescript;     {Beschreibung}
    Form2.StringGrid1.Cells[3, 0]:=rsGridCell2;    {Beginn}
    Form2.StringGrid1.Cells[4, 0]:=rsGridCell3;    {Ende}
    Form2.StringGrid1.Cells[5, 0]:=rsDauer;        {Dauer}
  end;

  procedure CountValues;                           {Sechs Spalten}
  begin
    a[high(a)].Ende:=ZeitToDT(StringGrid1.Cells[0, i], SpinEdit3.Tag);
    if s<>a[high(a)].Value then begin
      setlength(a, high(a)+2);                     {neuen Wert anlegen}
      a[high(a)].Value:=s;
      a[high(a)].Count:=1;
      a[high(a)].Beginn:=a[high(a)-1].Ende;
    end else begin                                 {immer noch alter Wert}
      a[high(a)].Count:=a[high(a)].Count+1;        {Zähler erhöhen}
    end;
  end;

begin
  if PageControl1.Tag>0 then
    Form2.Chart1ConstantLine1.Active:=false;
  PageControl1.Tag:=0;
  a:=nil;
  for i:=1 to StringGrid1.RowCount - 1 do begin
    if (CheckVT(StringGrid1.Cells[StringGrid1.Tag+2, i], {testen auf YTH Plus Fehler}
                StringGrid1.Cells[StringGrid1.Tag, i])) or
       (StringGrid1.ColCount=csvanz) or            {eigenes PX4 CSV Format ohne six}
       (Radiogroup1.ItemIndex>0) then begin        {Test aber nur bei telemetry}
      s:=trim(StringGrid1.Cells[p, i]);
      if assigned(a) then begin  {ist array schon einmal initialisiert worden ?}
        six:=false;
        if SpinEdit3.Tag=brID then begin           {Breeze}
          if p=2  then six:=true;                  {FlightMode}
          if p=14 then six:=true;                  {AutoTakeOFF}
          if p=19 then six:=true;                  {Error flags}
        end else if SpinEdit3.Tag=YTHPid then begin      {YTH Plus}
          case Radiogroup1.ItemIndex of
            0: begin                               {Telemetry}
                if p=StringGrid1.Tag then six:=true;     {f_mode}
                if p=8 then six:=true;             {GPS used}
               end;
            2: begin                               {Remote}
                 if p=5 then six:=true;            {CH4 Mode Switch}
               end;
          end;
        end else begin                             {Rest Yuneec}
          case Radiogroup1.ItemIndex of
            0: begin                               {Telemetry}
                 if p=StringGrid1.Tag+3 then six:=true;  {Error flag}
                 if p=StringGrid1.Tag then six:=true;    {f_mode}
                 if p=8 then six:=true;                  {GPS used}
               end;
            2: begin                               {Remote}
                 if p=5 then six:=true;            {CH4 Mode Switch}
               end;
          end;
        end;
        if six then CountValues else begin         {Rest zwei oder drei Spalten}
          b:=false;                                {Suchen}
          for j:=0 to high(a) do begin      {Schauen, ob es den Wert schon gibt}
            if a[j].Value=s then begin
              a[j].Count:=a[j].Count+1;            {gefunden, Zähler erhöhen}
              b:=true;
              break;
            end;
          end;
          if not b then begin                      {neuen Wert, im Array anlegen}
            setlength(a, high(a)+2);
            a[high(a)].Value:=s;
            a[high(a)].Count:=1;
          end;
        end;
      end else begin                 {Array noch nicht angelegt, nicht assigned}
        setlength(a, 1);                           {neuen Wert im Array anlegen}
        a[0].Value:=s;                             {Array Werte initialisieren}
        a[0].Count:=1;
        a[0].Beginn:=ZeitToDT(StringGrid1.Cells[0, i], SpinEdit3.Tag);
        a[0].Ende:=a[0].Beginn;                    {Zeitstempel intialisieren}
      end;
    end;
  end;                                             {Ende Spalte untersuchen}
  DoForm2Show(720);        {Detailfenster anzeigen und mit Statistik füllen}
  Form2.Caption:=rsStatistik+' "'+StringGrid1.Cells[p, 0]+'"';
  if Radiogroup1.ItemIndex=2 then case p of        {Remote}
     5: Form2.Caption:=Form2.Caption+' - S4 Flight Mode Switch';
     6: Form2.Caption:=Form2.Caption+' - Flight Mode Add';
     9: Form2.Caption:=Form2.Caption+' - S1 Gimbal tilt mode';
    10: Form2.Caption:=Form2.Caption+' - S2 Gimbal pan mode';
    11: Form2.Caption:=Form2.Caption+' - S5 '+rsLandgear;
  end;
  Form2.StringGrid1.Visible:=true;
  Form2.Chart1.Visible:=false;
  Form2.edTime.Visible:=false;
  Form2.StringGrid1.BeginUpdate;
  Form2.StringGrid1.RowCount:=1;
  Form2.StringGrid1.ColCount:=2;
  Form2.StringGrid1.Cells[0, 0]:=StringGrid1.Cells[p, 0];
  Form2.StringGrid1.Cells[1, 0]:=rsAnzahl;
  if StringGrid1.ColCount=csvanz then begin        {eigenes PX4 CSV Format}
    case p of                                      {muss zu Formatwandlung passen}
      15, 18, 19, 59: DreiSpalten;
    end;
  end else
  if SpinEdit3.Tag=brID then begin {Liste der kommentierten Statistiken, Breeze}
    case p of                                      {muss zu Formatwandlung passen}
      11, 18: DreiSpalten;
      2, 14, 19: SechsSpalten;
    end;
  end else if SpinEdit3.Tag=YTHPid then begin      {YTH Plus}
    case RadioGroup1.ItemIndex of
      0: begin                                     {Telemetry}
           if p=StringGrid1.Tag+2 then DreiSpalten; {Vehicle Type mit Erklärungen}
           if p=8 then SechsSpalten;               {GPS used mit Zeittabelle}
           if p=StringGrid1.Tag then SechsSpalten; {fMode}
         end;
      2: begin                                     {Remote}
           if p=5 then SechsSpalten;               {Flight mode switch}
         end;
    end;
  end else begin                                   {Legacy Yuneec}
    case RadioGroup1.ItemIndex of
      0: begin                                     {Telemetry}
           if p=8 then SechsSpalten;               {GPS used}
           if p=StringGrid1.Tag then SechsSpalten; {f_mode}
           if p=StringGrid1.Tag+3 then SechsSpalten; {Errorflags}
           if p=14 then DreiSpalten;               {Motor Status}
           if p=15 then DreiSpalten;               {IMU Status}
           if p=StringGrid1.Tag-1 then DreiSpalten;  {PressCompasStatus}
           if p=StringGrid1.Tag+2 then DreiSpalten;  {Vehicle Type mit Erklärungen}
         end;
      2: begin                                     {Remote}
           if (p=6) or (p=9) or (p=10) or (p=11) then DreiSpalten;
           if p=5 then SechsSpalten;
         end;
    end;
  end;
  for i:=0 to high(a) do
    AusgWe(a[i]);                                  {Werte ausgeben}
  Form2.StringGrid1.AutoSizeColumns;
  Form2.StringGrid1.EndUpdate;

  Form2.Invalidate;
  setlength(a,0);                                  {Array aufräumen}
end;

{Hier wird verzweigt, ob ein Diagramm oder eine Liste angezeigt werden soll.
 Dies muss entsprechend des Inhalts der Spalten angepasst werden.}

procedure TForm1.AnzeigeAddHist(Index: integer);   {Index ist Nummer Spalte}
begin
  if StringGrid1.ColCount=csvanz then begin        {PX4 CSV self-def format}
    case index of
      9, 15..20, 59, 60: ZhlWerte(Index);          {Tabelle}
      1..7, 10..14, 21..48, 61..78: DiaWerte(Index);
    end;
  end else                                         {sonstige, wie gehabt}
  case RadioGroup1.ItemIndex of
    0: begin
         if SpinEdit3.Tag=brID then begin          {Yuneec Breeze}
           case Index of                           {Telemetrie Spalten auswerten}
             2..9, 11, 14, 18, 19: ZhlWerte(Index);
             10, 12, 13, 15..17, 20, 21: DiaWerte(Index);
           end;
         end else begin                            {andere Yuneec Kopter}
           case Index of                           {Telemetrie Spalten auswerten}
             8, 9, 14..20: ZhlWerte(Index);
             1..7, 10..13: DiaWerte(Index);
             else begin                            {variable Spalten}
               if index=StringGrid1.Tag+2 then ZhlWerte(Index);
               if index=StringGrid1.Tag+3 then ZhlWerte(Index);
               if index=StringGrid1.Tag+4 then DiaWerte(Index);
               if index=StringGrid1.Tag+5 then DiaWerte(Index);  {YTH Plus vSpeed}
               if index=StringGrid1.Tag+6 then DiaWerte(Index);  {YTH Plus hSpeed}
             end;
           end;
         end;
       end;
    1: DiaWerte(Index);                            {ST10 Spalten auswerten}
    2: case Index of                               {Funk Spalten auswerten}
         1..4, 7..8:  DiaWerte(Index);
         5..6, 9..24: ZhlWerte(Index);
    end;
  end;
end;

procedure TForm1.StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if IsColumn and
      (StringGrid1.ColCount>defaultcol) and        {wenn Daten vorhanden sind}
      (StringGrid1.ColCount<YTHPcols) then begin   {nicht bei YTH Plus Sensor}
    AnzeigeAddHist(Index);
    Label3.Tag:=Index;
  end;
end;

procedure TForm1.StringGrid1KeyUp(Sender: TObject; var Key: Word; {Taste im StringGrid}
  Shift: TShiftState);
begin
  topp[ListBox1.ItemIndex, RadioGroup1.ItemIndex]:=StringGrid1.TopRow; {Top merken}
  if key=vk_F3 then TabSuchen;                     {F3 weitersuchen}
  if key=vk_F4 then TabSelect;                     {F4 Filter setzen}
  if (key=vk_F5) or (key=vk_ESCAPE) then begin     {F5 Filter rücksetzen}
    if SpinEdit3.Tag=brID then BrAnzeigeCSV(0)     {Breeze}
                          else AnzeigeCSV(0);      {andere Yuneec}
  end;
  if ssCtrl in Shift then begin
    if key=vk_c then begin
      if ssAlt in Shift then
        StringGrid1.CopyToClipboard(false)         {alles}
      else StringGrid1.CopyToClipboard(true);      {only Selection}
    end;
    if key=vk_f then TabSuchen;                    {Suchen}
    if key=vk_b then SetStartP;                    {Begin point}
    if key=vk_e then SetEndP;                      {End point}
    if key=vk_s then TabSelect;                    {Filterfunktion analog Suche}
    if key=vk_n then BitBtn14Click(self);          {Ausschneiden}
  end;
end;

procedure TForm1.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  x, y: Integer);                                  {Info per Zelle}
var sp, zl: integer;                               {Spalte und Zeile}
begin
  zl:=0;
  sp:=0;
  StringGrid1.MouseToCell(x, y, sp, zl);           {Zelle unter Maus finden}
  if zl>0 then StringGrid1.Hint:=GetCellInfo(sp, zl)
  else
    if RadioGroup1.ItemIndex=3 then                {Hint bei Sensor ausblenden}
      StringGrid1.Hint:=StringGrid1.Cells[sp, 0]
    else
      StringGrid1.Hint:=hntGrid1;
  if (zl=0) and (RadioGroup1.ItemIndex=2) then     {Kanalzuordnung Remote}
    StringGrid1.Hint:=ChToStr('', sp);             {CH vs Ch --> Channel settings}
end;

{http://www.delphipraxis.net/13528-stringgrid-rechte-mousetaste-reihe-selektieren.html
 Pop-up Menü mit rechter Maustaste aufrufen}
procedure TForm1.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  iCol: integer=1;
  iRow: integer=1;
  grSel: TGridRect;
begin
  StringGrid1.MouseToCell(x, y, iCol, iRow);
  if trim(StringGrid1.Cells[iCol, iRow])<>'' then begin
    if Button=mbRight then begin                   {rechte Maustaste}
      grSel.Top:=iRow;
      grSel.Left:=0;                               {ganze Zeile markieren}
      grSel.Right:=StringGrid1.ColCount;
      grSel.Bottom:=iRow;
      StringGrid1.Selection:=grSel;
      PopUpMenu1.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y); {AutoPopup:=false!}
      SpinEdit3.Value:=iRow;                       {Zeile für Start/Endpunkt}
    end;
    if Button=mbLeft then begin
      SpinEdit3.Value:=iRow;                       {Für Start/Endpunkt}
      if ssCtrl in Shift then ComboBox9.Text:=StringGrid1.Cells[iCol, iRow];
    end;              {Linke Maustase und Shift, dann Zelle ins Suchfeld übernehmen}
    topp[ListBox1.ItemIndex, 5]:=StringGrid1.TopRow+StringGrid1.VisibleRowCount+1;
    topp[ListBox1.ItemIndex, RadioGroup1.ItemIndex]:=StringGrid1.TopRow; {Top merken}
  end;
end;

procedure TForm1.StringGrid1Selection(Sender: TObject; aCol, aRow: Integer);
begin
  if aRow<>ComboBox9.Tag then GroupBox11.Tag:=0;   {andere Spalte-Suche zurücksetzen}
  ComboBox9.Tag:=aRow;
  ComboBox9.Enabled:=true;
end;

{StringGrid Sortierung siehe
 http://wiki.lazarus.freepascal.org/Grids_Reference_Page#Sorting_Columns_or_Rows}
procedure TForm1.grdOverviewCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);      {bestimmt, wie Spalte sortiert wird}
var f1, f2: double;
begin
  if ACol>4 then begin                      {bereinigt als Float sortieren}
    if TryStrToFloat(GetFNr(grdOverview.Cells[ACol,ARow]), f1) and
       TryStrToFloat(GetFNr(grdOverview.Cells[BCol,BRow]), f2)
    then result:=CompareValue(f1, f2);
  end else result:=CompareText(grdOverview.Cells[ACol,ARow],  {als Text}
                               grdOverview.Cells[BCol,BRow]);
  if grdOverview.SortOrder=soDescending then result:=-result; {Sortierrichtung}
end;

procedure TForm1.grdOverviewDblClick(Sender: TObject);  {Doppelclick zu Tabelle}
begin
  if ListBox1.Items.Count>0 then begin
    case SpinEdit3.Tag of
      MQid: ShowMQ;                                {MantisQ}
      H5id: ShowH520;                              {eine TLOG Datei H520 anzeigen}
      else begin
        PageControl1.ActivePageIndex:=grdOverview.Tag;  {Merker für letztes TabSheet}
        Anzeige;                                   {alle andern Typen}
      end;
    end;
  end;
end;

procedure TForm1.grdOverviewPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  topp[0, 4]:=grdOverview.TopRow;                  {Top merken für Übersicht}
  if (aState=[]) and
     (aCol>0) and (aRow>0) then begin              {1. Spalte ausschliessen}
    if aRow<grdOverview.RowCount-1 then begin      {Fußzeile ausschliessen}
      if (topp[aRow-1, 6] and 32)>0 then           {Compass error, ganze Zeile}
        grdOverview.Canvas.Brush.Color:=clOrange;
      if ((topp[aRow-1, 6] and 1)>0) and
         (aCol=10) then                            {Voltage 1}
        grdOverview.Canvas.Brush.Color:=clSkyBlue;
      if ((topp[aRow-1, 6] and 2)>0) and
         (aCol=10) then                            {Voltage 2}
        grdOverview.Canvas.Brush.Color:=clVolt2;
      if ((topp[aRow-1, 6] and 256)>0) and
         (aCol<4) then                             {Emergency}
        grdOverview.Canvas.Brush.Color:=clMaroon;
    end else
      if (SpinEdit3.Tag<>MQid) and
         (SpinEdit3.Tag<>H5id) then
        grdOverview.Canvas.Brush.Color:=clMoneyGreen;   {Summenzeile}
  end;
end;

{StringGrid Sortierung siehe
 http://wiki.lazarus.freepascal.org/Grids_Reference_Page#Sorting_Columns_or_Rows}
procedure TForm1.grdOverviewHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);                                  {Spalten sortieren}
begin
  if IsColumn then begin
    if grdOverview.SortOrder=soDescending then
      grdOverview.SortOrder:=soAscending
    else
      grdOverview.SortOrder:=soDescending;
    grdOverview.SortColRow(true, Index,
                           grdOverview.FixedRows, grdOverview.RowCount-2);
  end;
end;

procedure TForm1.grdOverviewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=vk_c) and (ssCtrl in Shift) then grdOverview.CopyToClipboard(false);
end;

procedure TForm1.grdOverviewMouseMove(Sender: TObject; Shift: TShiftState;
          x, y: Integer);                          {Show hint per cell}
var sp, zl: integer;                               {Spalte und Zeile}
begin
  sp:=0;
  zl:=0;
  grdOverview.MouseToCell(x, y, sp, zl);           {Zelle unter Maus finden}
  if (sp>0) and (zl>0) then begin
    if zl=grdOverview.RowCount-1 then begin
      case sp of
        1: grdOverview.Hint:=hntNumFlt;
        4: grdOverview.Hint:=hntDauer;
        7: grdOverview.Hint:=hntStrecke;
        8: grdOverview.Hint:=hntHGeschw;
        else grdOverview.Hint:=rsPC1Tab1;          {default}
      end;
    end else grdOverview.Hint:=grdOverview.Cells[sp, 0]+'='+
                               grdOverview.Cells[sp, zl];
  end else grdOverview.Hint:=grdOverview.Cells[sp, zl];
end;

procedure TForm1.grdOverviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Datei auswählen}
var
  iCol: integer=1;
  iRow: integer=1;
  grSel: TGridRect;
  i: integer;
begin
  if (ListBox1.Items.Count>0) and (Button=mbLeft) then begin  {linke Maustaste}
    grdOverview.MouseToCell(x, y, iCol, iRow);
    grSel.Top:=iRow;
    grSel.Left:=0;                                 {ganze Zeile markieren}
    grSel.Right:=grdOverview.ColCount;
    grSel.Bottom:=iRow;
    grdOverview.Selection:=grSel;
    if iRow<grdOverview.RowCount-1 then            {Summenspalte ausblenden}
      for i:=0 to ListBox1.Items.Count-1 do        {Datei in ListBox auswählen}
        if ListBox1.Items[i]=grdOverview.Cells[0, iRow] then begin
          ListBox1.ItemIndex:=i;                   {Index finden und auswählen}
          break;                                   {fertig, abbrechen}
    end;
  end;
end;

procedure TForm1.StringGrid3KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                             {CGO3 Einstellungen kopieren}
begin
  if (key=vk_c) and (ssCtrl in Shift) then StringGrid3.CopyToClipBoard(false);
end;

procedure TForm1.StringGrid4HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);                                 {Firmware Anzeige}
begin
  if (SpinEdit3.Tag=brID) and
     (not IsColumn) and
     (Index=5) then Memo1.Lines.Add(StringGrid4.Cells[1, Index]);
end;

procedure TForm1.StringGrid4KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                             {Firmwarestände kopieren}
begin
  if (key=vk_c) and (ssCtrl in Shift) then StringGrid4.CopyToClipBoard(false);
end;

procedure TForm1.StringGrid4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var k: integer;                                    {Firmwaretabelle löschen}
begin
  if ssCtrl in Shift then
    for k:=1 to StringGrid4.RowCount-1
      do StringGrid4.Cells[1, k]:='';
end;

procedure TForm1.StringGrid5DblClick(Sender: TObject); {Ergebniszeile}
var fn: string;
begin
  if BitBtn26.Tag=1 then begin                     {Flugbuch}
    if FileExists(SaveDialog1.FileName) then OpenDocument(SaveDialog1.FileName)
      else OpenDocument(IncludeTrailingPathDelimiter(ComboBox8.Text));
  end else begin                                   {Dateiliste}
    if (StringGrid5.Tag>0) then begin              {Es wurde etwas gefunden}
      if RadioGroup9.ItemIndex=10 then begin
        fn:=StringGrid5.Cells[1, StringGrid5.Tag];
        ShowSensorPlus(fn, 0, false, true, false);
        PageControl1.ActivePageIndex:=7;           {Springe zum AppLog}
      end else
      if (RadioGroup9.ItemIndex<8) or              {Sensor Dateien}
         (SpinEdit3.Tag<>YTHPid) then begin        {H Plus}
        fn:=StringGrid5.Cells[1, StringGrid5.Tag];
        ComboBox2.Text:=GetFlightLogDir(fn);
        SelDirAct(fn);
        ComboBox9.Enabled:=true;                   {Suche erlauben}
        PageControl1.ActivePageIndex:=1;           {Springe zur Dateiansicht}
      end;
    end;
  end;
end;

procedure TForm1.StringGrid5KeyUp(Sender: TObject; var Key: Word; {Strg+C}
  Shift: TShiftState);
begin
  if (key=vk_c) and (ssCtrl in Shift) then StringGrid5.CopyToClipboard(false);
end;

procedure TForm1.StringGrid5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  iCol: integer=1;
  iRow: integer=1;
begin
  StringGrid5.Tag:=0;                              {default: Header}
  if Button=mbLeft then begin                      {linke Maustaste}
    StringGrid5.MouseToCell(x, y, iCol, iRow);
    StringGrid5.Tag:=iRow;                         {Zeile markieren}
  end;
end;

procedure TForm1.StringGrid5Resize(Sender: TObject); {Spalte anpassen}
begin
  StringGrid5.ColWidths[1]:=StringGrid5.Width-StringGrid5.ColWidths[0]-30;
end;

procedure TForm1.TabSheet8Resize(Sender: TObject); {Höhe Diagramme anpassen}
begin
  Chart3.Height:=TabSheet8.Height div 3;
  Chart4.Height:=TabSheet8.Height div 3;
  Chart5.Height:=TabSheet8.Height div 3;
  Chart4.Width:=Chart3.Width;
  Chart4.Top:=Chart3.Top+Chart3.Height;
end;

procedure TForm1.Timer1Timer(Sender: TObject);     {CGO3 Statusabfrage}
begin
  if TabSheet11.Visible then CGO3run('', 0);
end;

procedure TForm1.Timer2Timer(Sender: TObject);     {Abfrage Doppelclick Form2}
var tp: TDateTime;
    i: integer;
begin
  if Form2.st<>'' then begin                       {Tageszeit auslesen}
    try
      tp:=EncodeTime(StrToInt(copy(Form2.st,1,2)),
                     StrToInt(copy(Form2.st,4,2)),
                     StrToInt(copy(Form2.st,7,2)), 0);
    except
      tp:=0;
      ComboBox9.Text:=Form2.st;                    {andere Werte in Spalte suchen}
      TabSuchen;
    end;
    Form2.st:='';                                  {Aktion zurücksetzen}
    Form2.MenuItem3.Enabled:=false;
    if (tp>0) and (StringGrid1.RowCount>10) then begin {nur wenn es sich lohnt}
      for i:=1 to StringGrid1.RowCount-1 do begin      {Zeitstempel suchen}
        if frac(ZeitToDT(StringGrid1.Cells[0, i], SpinEdit3.Tag))>=tp then begin
          SpinEdit3.Value:=i;                      {gefunden und springen}
          GoToZ(1);
          break;
        end;
      end;
    end;
    BringToFront;                                  {Hauptfenster nach vorn}
  end;     {nichts tun, wenn kein Zeitstring durch Doppelclick vorhanden ist}
end;

procedure TForm1.Timer3Timer(Sender: TObject);     {Diashow Profiles}
begin
  if ListBox1.Items.Count>0 then begin
    SetProfile(Timer3.Tag);
    Timer3.Tag:=Timer3.Tag+1;
    if Timer3.Tag>=ComboBox10.Items.Count then Timer3.Tag:=0; {wieder von vorn}
  end else Timer3.Enabled:=false;                  {Ohne Daten nix anzeigen}
end;

procedure TForm1.TrackBar1Change(Sender: TObject); {Saturation verstellt}
begin
  EnSave;
  TrackBar1.SelEnd:=TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject); {Entfernung für Waypoits}
var s: string;
begin
  TrackBar2.SelEnd:=TrackBar2.Position;
  s:=IntToStr(TrackBar2.Position)+'m';
  if RadioGroup3.ItemIndex=2 then s:=IntToStr(Round(TrackBar2.Position/fft))+'ft';
  TrackBar2.Hint:=s;
  Label12.Caption:=capLabel12+tab2+s;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);  {Zeige in GoogleMaps}
begin
  if ListBox1.Items.Count>0 then begin             {Breeze lat/lon Format}
    if SpinEdit3.Tag=brID then begin
      OpenURL(URLGMap(BrCoordFormat(StringGrid1.Cells[12,StringGrid1.Selection.Top]),
                      BrCoordFormat(StringGrid1.Cells[13,StringGrid1.Selection.Top])));
    end else begin                                 {Andere}
      case Radiogroup1.ItemIndex of
        0: OpenURL(URLGMap(StringGrid1.Cells[5,StringGrid1.Selection.Top],
                           StringGrid1.Cells[6,StringGrid1.Selection.Top]));
        1: OpenURL(URLGMap(KoToStr(StrToFloatN(StringGrid1.Cells[2,
                                       StringGrid1.Selection.Top])),
                           KoToStr(StrToFloatN(StringGrid1.Cells[1,
                                       StringGrid1.Selection.Top]))));
      end;
    end;
  end;
  if StringGrid1.ColCount=csvanz then              {Self-dev PX4 CSV format}
    OpenURL(URLGMap(StringGrid1.Cells[5,StringGrid1.Selection.Top],
                    StringGrid1.Cells[6,StringGrid1.Selection.Top]));
end;

procedure TForm1.MenuItem28Click(Sender: TObject); {Homepage aufrufen}
begin
  OpenURL(homepage);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);  {Höhenprofil in Zwischenablage}
begin
  Chart1.CopyToClipboardBitmap;                    {Höhenprofil}
end;

procedure TForm1.MenuItem3Click(Sender: TObject);  {PopUp Höhenprofil als Datei}
begin
  SaveDialog1.Title:=rsHDiaSave;
  SaveDialog1.FileName:=Form2.CleanDN(PageControl1.Page[2].Caption+pngdef);
  if SaveDialog1.Execute then
    Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
end;

procedure TForm1.MenuItem40Click(Sender: TObject); {Arbeitsverzeichnis öffnen}
begin
  OpenDocument(IncludeTrailingPathDelimiter(ComboBox2.Text));
end;

procedure TForm1.MenuItem41Click(Sender: TObject); {Menü SelDir}
begin
  SelDirSet;
end;

procedure TForm1.MenuItem42Click(Sender: TObject); {Menü Profile Errors}
begin
  Timer3.Enabled:=false;
  SetProfile(2);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.MenuItem43Click(Sender: TObject); {Menü Profile GPS}
begin
  Timer3.Enabled:=false;
  SetProfile(3);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.MenuItem45Click(Sender: TObject); {Menü Protokollverzeichnis}
begin
  SelDirProt;
end;

procedure TForm1.MenuItem48Click(Sender: TObject); {Menü Profile Throttle}
begin
  Timer3.Enabled:=false;
  SetProfile(4);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.MenuItem49Click(Sender: TObject); {Menü Profile Pitch}
begin
  Timer3.Enabled:=false;
  SetProfile(5);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.TabSelect;                        {Daten selektieren}
begin
  if (ListBox1.Items.Count>0) and                  {nur wenn Dateien da sind}
     (StringGrid1.ColCount<YTHPcols) then begin    {nicht bei YTH Plus Sensor}
    tpos:=0;                                       {Position zurücksetzen}
    ComboBox9.Text:=UpCase(trim(ComboBox9.Text));
    if ComboBox9.Text<>'' then begin               {mit Filter}
      Merkliste(ComboBox9, AnzDirs);               {Suchwerte DropDownListe füllen}
      ComboBox9.Hint:=rsSelection+' in '+StringGrid1.Cells[Label3.Tag, 0];
      if SpinEdit3.Tag=brID then BrAnzeigeCSV(1)   {Breeze Filtermode}
                            else AnzeigeCSV(1);    {legacy Yuneec Filtermode}
    end else                                       {wenn leer, alles anzeigen}
      if SpinEdit3.Tag=brID then BrAnzeigeCSV(0)   {Breeze}
                            else AnzeigeCSV(0);    {ohne Filter}
  end;
end;

procedure TForm1.TabSuchen;                        {In Tabelle suchen}
var x: integer;
    grSel: TGridRect;
    s: string;
begin
  ComboBox9.Text:=StringReplace(ComboBox9.Text, sep, '.', []);
  ComboBox9.Text:=UpCase(trim(ComboBox9.Text));
  if ComboBox9.Enabled and (ComboBox9.Text<>'') then begin  {Suchen}
    if ComboBox9.Tag<2 then GroupBox11.Tag:=0;     {oben nix gefunden}
    Merkliste(ComboBox9, AnzDirs);                 {DropDownListe füllen}
    ComboBox9.Hint:=hntComboBox9+' in '+StringGrid1.Cells[Label3.Tag, 0];
    for x:=ComboBox9.Tag to StringGrid1.RowCount-1 do begin
      ComboBox9.Tag:=x+1;
      s:=UpCase(trim(StringGrid1.Cells[Label3.Tag, x]));
      if (s=ComboBox9.Text) or                     {kurz -> vollqualifiziert}
         (((length(s)>4) or (pos('.', s)>0)) and   {Punkt drin oder lang}
         (pos(ComboBox9.Text, s)>0)) then begin    {teilqualifiziert}
        GroupBox11.Tag:=GroupBox11.Tag+1;          {gefunden pro Spalte merken}
        SpinEdit3.Value:=x;                        {Datenpunkt anzeigen}
        grSel.Top:=x;
        grSel.Left:=0;                             {ganze Zeile markieren}
        grSel.Right:=StringGrid1.ColCount;
        grSel.Bottom:=x;
        StringGrid1.Selection:=grSel;
        if x>8 then StringGrid1.TopRow:=x-4        {Abstand zu oben}
               else StringGrid1.TopRow:=x;
        break;                                     {gefunden}
      end;
    end;
    s:=' in '+StringGrid1.Cells[Label3.Tag, 0];
    if GroupBox11.Tag>0 then begin
      StatusBar1.Panels[5].Text:='"'+ComboBox9.Text+'"'+s;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end else begin
      StatusBar1.Panels[5].Text:='"'+ComboBox9.Text+'"'+nixda+s;
      SynEdit1.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    StatusBar1.Panels[1].Text:=IntToStr(GroupBox11.Tag);  {Anzahl Treffer}
    SynEdit1.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsFound);
    if ComboBox9.Tag>StringGrid1.RowCount-2 then ComboBox9.Tag:=1;
  end else GroupBox11.Tag:=0;                      {leer - Suche zurücksetzen}
end;

procedure TForm1.KursorAus;                        {Fadenkreuz aus}
begin
  Chart1ConstantLine1.Active:=false;
  ChartToolset2DataPointCrosshairTool1.CrosshairPen.Visible:=false;
  ChartToolset2DataPointCrosshairTool1.Enabled:=false;
  MenuItem4.Caption:=capCrossHairOn;
  Chart1.Cursor:=crDefault;
  if cutb>0 then StatusBar1.Panels[3].Text:=FormatDateTime(vzf, cutb);
  if cute>0 then StatusBar1.Panels[4].Text:=FormatDateTime(vzf, cute);
  if (cutb>0) and
     (cute>cutb) then begin                        {Dauer wieder anzeigen}
    BitBtn14.Enabled:=true;
    StatusBar1.Panels[5].Text:=rsDauer+tab1+FormatDateTime('= nn:ss'+zzz, cute-cutb);
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);  {Menü: Cursor ein}
begin
  if ChartToolset2DataPointCrosshairTool1.Enabled=false then begin {Fadenkreuz ein}
    ChartToolset2DataPointCrosshairTool1.CrosshairPen.Visible:=true;
    ChartToolset2DataPointCrosshairTool1.Enabled:=true;
    MenuItem4.Caption:=capCrossHairOff;
    StatusBar1.Panels[5].Text:='';
    Chart1.Cursor:=crCross;
    StatusBar1.Tag:=1;                             {Zeiten zum Kopieren vorhanden}
  end else KursorAus;                              {Fadenkreuz aus}
end;

procedure TForm1.MenuItem50Click(Sender: TObject); {Tabelle kopieren}
begin
  if RadioGroup1.ItemIndex=3 then StringGrid1.CopyToClipboard(false) {Sensor: Alles}
                             else StringGrid1.CopyToClipboard(true); {nur Zeile}
end;

procedure TForm1.MenuItem51Click(Sender: TObject); {Menü Profile Roll}
begin
  Timer3.Enabled:=false;
  SetProfile(6);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.MenuItem52Click(Sender: TObject); {Menü Profile Yaw}
begin
  Timer3.Enabled:=false;
  SetProfile(7);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.MenuItem53Click(Sender: TObject); {Menü Profile 3Axis}
begin
  Timer3.Enabled:=false;
  SetProfile(8);
  ComboBox10.ItemIndex:=0;
end;

procedure TForm1.OpenSensorPlus;                   {Sensordatei vom YTH Plus öffnen}
var spdir: string;
begin
  spdir:=IncludeTrailingPathDelimiter(ComboBox2.Text)+npath;
  OpenDialog1.Title:=capSensorPlus;
  Opendialog1.DefaultExt:=sextP;
  if DirectoryExists(spdir) then
    OpenDialog1.InitialDir:=spdir;                 {zu Sensor wechseln}
  if Opendialog1.Execute then begin
    if ExtractFileExt(OpenDialog1.FileName)=fext then
      AnzeigePX4CSV(OpenDialog1.FileName)          {PX4 Sensor csv anzeigen}
    else                                           {Sensor Datei auswerten}
      ShowSensorPlus(OpenDialog1.FileName, 0, CheckBox10.Checked, true, false);
  end;
end;

procedure TForm1.MenuItem56Click(Sender: TObject); {Sensordatei vom YTH Plus}
begin
  OpenSensorPlus;
end;

procedure TForm1.MenuItem57Click(Sender: TObject); {Menu Diashow Profiles}
begin
  if (SpinEdit3.Tag=MQid) or                       {nichts tun für MantisQ}
     (SpinEdit3.Tag=H5id) or                       {nichts tun für H520}
     (SpinEdit3.Tag=BrID) then                     {nichts tun für Breeze}
       exit;
  Timer3.Tag:=0;
  Timer3.Enabled:=true;
end;

procedure TForm1.GoToZ(a: integer); {Gehe zu Zeilennummer, a..freie Zeilen oben}
var grSel: TGridRect;
    z: integer=1;
begin
  if StringGrid1.RowCount>1 then begin
    if SpinEdit3.Value<StringGrid1.RowCount then z:=SpinEdit3.Value
                                            else z:=StringGrid1.RowCount-1;
    StringGrid1.Col:=1;
    StringGrid1.Row:=z;
    if z>(a*2) then StringGrid1.TopRow:=z-a        {Abstand zu oben}
               else StringGrid1.TopRow:=z;
    grSel.Top:=z;
    grSel.Left:=0;                                 {ganze Zeile markieren}
    grSel.Right:=StringGrid1.ColCount;
    grSel.Bottom:=grSel.Top;
    StringGrid1.Selection:=grSel;
    SpinEdit3.Value:=z;
  end;
  topp[ListBox1.ItemIndex, RadioGroup1.ItemIndex]:=StringGrid1.TopRow; {Top merken}
end;

procedure TForm1.MenuItem5Click(Sender: TObject);  {zur Zeilennummer scrollen}
begin
  if ListBox1.Items.Count>0 then GoToZ(4);
end;

procedure TForm1.MenuItem6Click(Sender: TObject);  {Datenanalyse im StringGrid1}
begin
  if ListBox1.Items.Count>0 then begin
    GoToZ(0);
    Analyse;
  end;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);  {Gehe zum nächsten Fehler}
var x: integer;
begin
  if RadioGroup1.ItemIndex=0 then begin            {nur bei Telemetrie}
    for x:=topp[ListBox1.ItemIndex, 5]+1 to StringGrid1.RowCount-1 do begin
      if (StrToIntDef(StringGrid1.Cells[StringGrid1.Tag+3,x], 0) shr 1)>0 then begin
        if x<StringGrid1.RowCount-1 then begin
          SpinEdit3.Value:=x-1;
          topp[ListBox1.ItemIndex, 5]:=x+StringGrid1.VisibleRowCount+1;
          GoToZ(4);
          StringGrid1.Col:=StringGrid1.Tag+3;
        end;
        break;
      end;
    end;
  end;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);  {gehe zur Tabelle}
begin
  if ListBox1.Items.Count>0 then begin
    PageControl1.ActivePageIndex:=1;
    GoToZ(4);
  end;
end;

function TForm1.CheckNumTurns(const dn: string): integer; {Anzahl der Dateien feststellen}
var x, p, n: integer;
    sr: TSearchRec;
    mf: file;
    buf: array [0..31] of byte;
    wrt: double;
    gtm: TTime;
    s: string;
begin
  kpath:=IncludeTrailingPathDelimiter(dkpath);     {default setzen}
  GetDefVT;                                        {Overwrite for PX4 Thunderbird}
  ListBox1.ItemIndex:=-1;                          {deselektieren}
  ListBox1.Items.Clear;                            {und löschen}
  tend:=0;
  SetLength(topp, 2);                              {Topzeilen array}
  n:=0;                                            {Anzahl erstmal Null}
  try
    if FindFirst(IncludeTrailingPathDelimiter(dn)+kpath+
                 kfile+wldcd+fext, faAnyFile, sr) = 0 then
    try                                            {1. Versuch, normale FlightLog Struktur}
      repeat
        ListBox1.Items.Add(GetNr(sr.Name));        {Dateiliste neu aufbauen}
        inc(n);                                    {Dateien zählen}
      until FindNext(sr)<>0;
      if n>0 then begin
        grdOverview.ColWidths[0]:=fw0;             {default}
        grdOverview.Update;
      end;
    finally
      FindClose(sr);
    end;

    if n=0 then begin                              {2. Versuch ohne Unterverzeichnisse}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+kfile+wldcd+
                   fext, faAnyFile, sr) = 0 then try
        repeat
          ListBox1.Items.Add(GetNr(sr.Name));      {Dateiliste neu aufbauen}
          inc(n);                                  {Dateien zählen}
        until FindNext(sr)<>0;
        if n>0 then begin
          kpath:='';                {nur, wenn dort wirklich was gefunden wurde}
          grdOverview.ColWidths[0]:=fw1;
          grdOverview.Update;
        end;
      finally
        FindClose(sr);
      end;
    end;

    if n=0 then begin                              {Test auf log files vom Breeze}
      SpinEdit3.Tag:=brID;                         {erstmal ID für Breeze setzen
                                            wegen Dateinamen zerlegen und bauen}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+wldcd+
                   bext, faAnyFile, sr)=0 then try
        repeat
          p:=0;
          buf[0]:=0;                               {erste zwei Byte auf YC prüfen}
          AssignFile(mf, IncludeTrailingPathDelimiter(dn)+sr.Name);
          Reset(mf, 1);
          BlockRead(mf, buf, length(buf), p);
          CloseFile(mf);
          if (p=length(buf)) and (buf[0]=$59) and (buf[1]=$43) then begin
            ListBox1.Items.Add(GetNr(sr.Name));    {Dateiliste neu aufbauen}
            inc(n);                                {Dateien zählen}
          end;
        until FindNext(sr)<>0;
        if n>0 then begin    {Breeze: nur, wenn dort wirklich was gefunden wurde}
          grdOverview.ColWidths[0]:=fw1;
          grdOverview.Update;
          StaticText1.Caption:=VTypeToStr(MQid);
        end else begin                             {doch kein Breeze}
          GetDefVT;                                {Overwrite for PX4 Thunderbird}
        end;
      finally
        FindClose(sr);
      end;
    end;

    if n=0 then begin                              {noch ein Versuch für Mantis}
      SpinEdit3.Tag:=MQid;                         {erstmal ID für MantisQ setzen}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+nfile+wldcd, faAnyFile, sr)=0 then
      try
        repeat
          if IsMantisQ(IncludeTrailingPathDelimiter(dn)+sr.Name) then begin
            ListBox1.Items.Add(GetNr(sr.Name));    {Dateiliste neu aufbauen}
            inc(n);                                {Dateien zählen}
          end;
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;

{Variante beim Mantis Q, aber mit bext (.log)}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+mfile+wldcd, faAnyFile, sr)=0 then
      try
        repeat
          if IsMantisQ(IncludeTrailingPathDelimiter(dn)+sr.Name) then begin
            ListBox1.Items.Add(GetNr(sr.Name));    {Dateiliste aufbauen}
            inc(n);                                {Dateien zählen}
          end;
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;

      if n>0 then begin    {Mantis: nur, wenn dort wirklich was gefunden wurde}
        grdOverview.ColWidths[0]:=fw1;
        StaticText1.Caption:=VTypeToStr(MQid);
        ListBox1.Tag:=-1;                          {noch keine Datei angezeigt}
      end else begin                               {doch kein MantisQ}
        GetDefVT;                                  {Overwrite for PX4 Thunderbird}
      end;
    end;

    if n=0 then begin                              {noch ein Versuch für H520}
      SpinEdit3.Tag:=H5id;                         {erstmal ID für MantisQ setzen}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+wldcd+hext, faAnyFile, sr)=0 then
      try
        repeat                                     {*.tlog files}
          ListBox1.Items.Add(GetNr(sr.Name));      {Dateiliste neu aufbauen}
          inc(n);                                  {Dateien zählen}
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;

      if n>0 then begin     {H520: nur, wenn dort wirklich was gefunden wurde}
        grdOverview.ColWidths[0]:=fw1;
        StaticText1.Caption:=VTypeToStr(H5id);
        ListBox1.Tag:=-1;                          {noch keine Datei angezeigt}
      end else begin                               {doch kein H520}
        GetDefVT;                                  {Overwrite for PX4 Thunderbird}
      end;
    end;

    if n>0 then begin                              {irgendwas von oben wurde gefunden}
      topp:=nil;
      SetLength(topp, n);                          {Topzeilen array festlegen}
      for x:=0 to High(topp) do begin              {Alles auf Null setzen}
        topp[x, 0]:=0;
        topp[x, 1]:=0;
        topp[x, 2]:=0;
        topp[x, 3]:=0;                             {Sensor-Datei}
        topp[x, 4]:=0;
        topp[x, 5]:=0;                             {Suchpointer löschen}
        topp[x, 6]:=0;                             {ErrorID für Datei löschen}
      end;
      grdOverview.RowCount:=ListBox1.Items.Count+1; {Übersichtstabelle ohne Summe}
      grdOverview.Row:=1;
      grdOverview.Col:=1;
      for x:=1 to 10 do                            {Spalten}
        for n:=1 to grdOverview.RowCount-1 do      {Zeilen}
          grdOverview.Cells[x, n]:='';             {Inhalte erstmal löschen}
      grdOverview.Cells[0,0]:='';                  {oben links auch löschen}
      BitBtn25.Tag:=0;
      for x:=0 to ListBox1.Items.Count-1 do
        grdOverview.Cells[0, x+1]:=ListBox1.Items[x];
      if (SpinEdit3.Tag<>MQid) and                 {nicht nur Sensor PX4}
         (SpinEdit3.Tag<>H5id) then begin          {Übersichtstabelle füllen}
        grdOverview.RowCount:=grdOverview.RowCount+1; {Summenzeile}
        for x:=0 to ListBox1.Items.Count-1 do
          Werte(x);                                {Telemetrie durchsuchen für Übersicht}
        gtm:=0;
        wrt:=0;
        grdOverview.Cells[0, grdOverview.RowCount-1]:=rsSumme;
        for x:=1 to grdOverview.RowCount-2 do begin {Summen anzeigen}
          try
            wrt:=wrt+StrToFloatN(GetFNr(grdOverview.Cells[7, x]));  {Entfernung}
          except
          end;
          try
            if trim(grdOverview.Cells[5, x])<>'' then
              gtm:=gtm+StrToTime('00:'+trim(grdOverview.Cells[4, x]));
          except
          end;
        end;
        p:=trunc(gtm);
        if p>0 then
          grdOverview.Cells[4, grdOverview.RowCount-1]:=IntToStr(p)+'d '+
                              FormatDateTime(zzf, gtm)
               else
          grdOverview.Cells[4, grdOverview.RowCount-1]:=
                              FormatDateTime(zzf, gtm);
        gtm:=gtm*24;                               {in Stunden}
        if RadioGroup3.ItemIndex=2 then begin
          wrt:=wrt/5280;
          grdOverview.Cells[7, grdOverview.RowCount-1]:=
                              FloatToStrF(wrt, ffFixed, 6, 2)+'mi';
          if gtm>0 then                            {avarage speed in mph}
            grdOverview.Cells[8, grdOverview.RowCount-1]:='Ø '+
                              FloatToStrF(wrt/gtm, ffFixed, 6, 2)+'mph';
        end else begin
          wrt:=wrt/1000;
          grdOverview.Cells[7, grdOverview.RowCount-1]:=
                              FloatToStrF(wrt, ffFixed, 6, 2)+'km';
          if gtm>0 then                            {avarage speed in km/h}
            grdOverview.Cells[8, grdOverview.RowCount-1]:='Ø '+
                              FloatToStrF(wrt/gtm, ffFixed, 6, 2)+'km/h';
        end;
        grdOverview.Cells[0,0]:=rsDateien+suff+IntToStr(n);
        StatusBar1.Panels[0].Text:=rsDateien+suff+IntToStr(n);
        tpos:=0;                                   {zeitl. Pos im StringGrid1}
        SynEdit1.Lines.Add('{');                   {Tabelle als Comment für
                                                    SynAnySyn-Highlighter}
        for p:=0 to grdOverview.RowCount-1 do begin  {Übersicht ausgeben}
          s:='';                                   {Datenzeile neu}
          for x:=0 to grdOverview.ColCount-1 do begin
            s:=s+Format('%14.13s', [grdOverview.Cells[x, p]]);  {Datenzeile füllen}
          end;
          SynEdit1.Lines.Add(s);                   {Ausgeben in AppLog}
        end;
        SynEdit1.Lines.Add('}');
      end;
      ListBox1.ItemIndex:=0;                       {Default: 1. Zeile auswählen}
    end;                                           {Ende Übersichtstabelle}
  except
    n:=0;                                          {nix gefunden}
    SynEdit1.Lines.Add('''10872'+suff+'File error CheckNumTurns');
  end;
  result:=n;                                       {Anzahl übergeben}
  if n>0 then
    RadioGroup1.Tag:=0;                            {Resize columns needed again}
end;

end.

