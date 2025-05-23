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

(*
  Auswertung der FlightLogDaten vom Yuneec Q500 und weitere
  (Q500, H920, Typhoon H, Blade Chroma, Blade 350QX, Mantis Q)

Die Daten liegen als *.CSV Dateien in einer fixen Struktur vor,
wenn man in die ST10 eine Speicherkarte einlegt:
---FlightLog
   +---Remote      Remote_00001.csv       Stickbewegungen, einige Schalter
   +---RemoteGPS   RemoteGPS_00001.csv    GPS Daten von der Funke (Controler)
   +---Sensor      Sensor_00001.bin       Sensordaten vom Kopter, Firmwarestände
   +---Telemetry   Telemetry_00001.csv    Telemetriedaten vom Kopter

Datenformat siehe Benutzerhandbuch, Anhang
------------------------------------------
Source ST10+      https://github.com/azvampyre/st10-v01b31c

KML file generation: https://developers.google.com/kml/documentation/
YTH waypoints: http://www.eegalmc2.com/us/typhoonh/

Hilfsvariablen:
btnClose.Tag:     1=Breeze telemetry als ft, sonst Meter
btnArchive.Tag:   Plattform for Breeze: 1: Android, 0: iOS
gridDetails.Tag:  Spaltennummer, wo f_mode steht.
StringGrid2.Tag:  Merker für zuletzt benutztes Tabsheet, gesetzt beim Umschalten
pcMain.Tag: Indikator, ob Form2 angezeigt wurde (0 .. nein, >0 .. ja)
v_type:           Vehicle Type as Byte (default = 5, YTH Plus = 10,
                                        Breeze = 90 = brID)
Label3.Tag:       Spalte für Suche/Filter in Tabelle merken.
StatusBar1.Tag:   Legt fest, ob StatusPanel 3 und 4 (Zeiten) mit ins
                  Clipboard kopiert wird (>0: ja).
RadioGroup1.Tag:  Indicates if autosize columns is needed. 0 means yes.


Icon: Aus einem Bild, gemacht von der obersten Flugleitung
http://image.online-convert.com/convert-to-ico

ToDo: Liniendiagramm entspechend FlghtMode einfärben

https://forum.lazarus.freepascal.org/index.php?topic=40024.0
https://www.lazarusforum.de/viewtopic.php?f=18&t=14258

*)

unit q500log2kml_main;

{$mode objfpc}{$H+}
{$modeswitch DuplicateLocals}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAIntervalSources, TASeries, LCLType,
  TATransformations, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, XMLPropStorage, Grids, Menus, lclintf, StdCtrls, Spin,
  Zipper, math, TAChartUtils, TAFuncSeries, Clipbrd, anzwerte, TACustomSeries,
  TATools, graphutil, ActnList, lazUTF8,
  SynEdit, SynHighlighterMulti, SynHighlighterAny, strutils, dateutils,
  lazsysutils, q5_common, Types, mav_defs, yun_defs, Iphttpbroker, IpHtml;

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

  {TForm1: Main program}

  TForm1 = class(TForm)
    actMAVmessageIDlist: TAction;
    ActionList1: TActionList;
    btnMAVmessageIDlist: TBitBtn;
    btnAutoCut: TBitBtn;
    btnDeleteLn: TBitBtn;
    btnCombineLogs: TBitBtn;
    btnClose: TBitBtn;
    btnCut: TBitBtn;
    btnDefaultProfile: TBitBtn;
    btnDelAppLog: TBitBtn;
    btnFlugBuch: TBitBtn;
    btnSaveApplog: TBitBtn;
    btnScanErr: TBitBtn;
    btnConv: TBitBtn;
    btnArchive: TBitBtn;
    btnShowHex: TBitBtn;
    btnSpecial: TButton;
    btnSplit: TBitBtn;
    cbCap: TCheckBox;
    cbCleanHplus: TCheckBox;
    cbDashw: TCheckBox;
    cbExtrude: TCheckBox;
    cbHighLight: TCheckBox;
    cbMarker: TCheckBox;
    cbMAVasCSV: TCheckBox;
    cbPilot: TCheckBox;
    cbReduced: TCheckBox;
    cbSensorKML: TCheckBox;
    cbSimu: TCheckBox;
    cbThunder: TCheckBox;
    cbVehicleType: TCheckBox;
    cbxProfiles: TComboBox;
    cbxScanDir: TComboBox;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1BarSeries2: TBarSeries;
    Chart1BarSeries3: TBarSeries;
    Chart1BarSeries4: TBarSeries;
    Chart1BarSeries5: TBarSeries;
    Chart1BarSeries6: TBarSeries;
    Chart1ConstantLine1: TConstantLine;
    Chart1ConstantLine2: TConstantLine;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart3: TChart;
    Chart3LineSeries1: TLineSeries;
    Chart3LineSeries2: TLineSeries;
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
    cbSensorHasData: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    cbxLogDir: TComboBox;
    edDeleteLn: TEdit;
    gbBatt: TGroupBox;
    gbDiverse: TGroupBox;
    gbStkProz: TGroupBox;
    gridDetails: TStringGrid;
    gridFirmware: TStringGrid;
    gridOverview: TStringGrid;
    gridScanResult: TStringGrid;
    GroupBox10: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox6: TGroupBox;
    Image4: TImage;
    IpHttpDataProvider1: TIpHttpDataProvider;
    cbxText: TComboBox;                            {Drone ID - Hint}
    cbxSearch: TComboBox;                          {Find input}
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource2: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource3: TDateTimeIntervalChartSource;
    DateTimeIntervalChartSource4: TDateTimeIntervalChartSource;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lblBegin: TLabel;
    lblEnde: TLabel;
    lblDuration: TLabel;
    Label17: TLabel;
    lblNumPoints: TLabel;
    Label9: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    lblBaseLoad: TLabel;
    lblDistWP: TLabel;
    lblGitHub: TLabel;
    lblMAVcommon: TLabel;
    lblSaturation: TLabel;
    MAVmsg: TCheckGroup;
    mnMainScreenshot: TMenuItem;
    Separator2: TMenuItem;
    mnAutoCut: TMenuItem;
    mnSaveTab: TMenuItem;
    Separator1: TMenuItem;
    mnSplit: TMenuItem;
    mnDownload: TMenuItem;
    mnReload: TMenuItem;
    mnFlDel: TMenuItem;
    Panel1: TPanel;
    pcMain: TPageControl;
    pcSettings3: TPageControl;
    PopUpMenuFlights: TPopupMenu;
    GroupBox11: TGroupBox;
    mnHexdump: TMenuItem;
    mnMAVlist: TMenuItem;
    mnSlideshow: TMenuItem;
    mnTR2: TMenuItem;
    mnCleanCSV: TMenuItem;
    AppLogHighlighter: TSynAnySyn;
    ProgressBarScan: TProgressBar;
    rgAltitudeType: TRadioGroup;
    rgBlockSize: TRadioGroup;
    rgCSVtext: TRadioGroup;
    rgErrType: TRadioGroup;
    rgOutFormat: TRadioGroup;
    rgSpeedUnit: TRadioGroup;
    rgTimeType: TRadioGroup;
    rgVehicleType: TRadioGroup;
    rgVoltRule: TRadioGroup;
    sbtnScanDir: TSpeedButton;
    speNumPoints: TSpinEdit;
    speBaseLoad: TSpinEdit;
    speBlockNum: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    speItems: TSpinEdit;
    speLinePath: TSpinEdit;
    speProz: TSpinEdit;
    speStk: TSpinEdit;
    AppLog: TSynEdit;
    tabAnalyze3: TTabSheet;
    tabAppLog: TTabSheet;
    tabCommon: TTabSheet;
    tabConvert: TTabSheet;
    tabData: TTabSheet;
    tabDetails: TTabSheet;
    tabHdia: TTabSheet;
    TabImages: TImageList;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    lblManual: TLabel;
    lblUpdate: TLabel;
    lbFlights: TListBox;
    MainMenu1: TMainMenu;
    mnGoogleMap: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    mnDelTab: TMenuItem;
    mnStartTab: TMenuItem;
    mnStopTab: TMenuItem;
    MenuItem19: TMenuItem;
    mnCopyHist: TMenuItem;
    mnDelHist: TMenuItem;
    MenuItem21: TMenuItem;
    mnMainFile: TMenuItem;
    mnMainTools: TMenuItem;
    mnConvert: TMenuItem;
    mnMainHelp: TMenuItem;
    mnManual: TMenuItem;
    mnHomepage: TMenuItem;
    MenuItem29: TMenuItem;
    mnSaveAsHist: TMenuItem;                       {"Als Bild speichern" im PopUp Menü HDiagr}
    mnInfo: TMenuItem;
    mnTR1: TMenuItem;
    mnMainDel: TMenuItem;
    mnMainStart: TMenuItem;
    mnMainStop: TMenuItem;
    mnCut: TMenuItem;
    mnArchive: TMenuItem;
    MenuItem37: TMenuItem;
    mnClose: TMenuItem;
    MenuItem39: TMenuItem;
    mnCursorEin: TMenuItem;
    mnExploreLog: TMenuItem;
    mnSelDirLog: TMenuItem;
    mnMainScanRec: TMenuItem;
    mnSelDirRec: TMenuItem;
    MenuItem46: TMenuItem;
    mnExploreRec: TMenuItem;
    mnGoToTab: TMenuItem;
    mnCopyTab: TMenuItem;
    mnSaveAsQuick: TMenuItem;                      {Menü GoTo Settings}
    mnResetProfile: TMenuItem;                     {Menü Default Schnellanalyse}
    mnProfiles: TMenuItem;                         {Menü Profiles - no action}
    mnProfMode: TMenuItem;                         {Menü Profile FlightMode}
    mnProfErr: TMenuItem;                          {Menü Profile Errors}
    mnProfGPS: TMenuItem;                          {Menü Profile GPS}
    mnProfThr: TMenuItem;                          {Menü Profile Throttle}
    mnProfPitch: TMenuItem;                        {Menü Profile Pitch}
    mnProfRoll: TMenuItem;                         {Menü Profile Roll}
    mnProfYaw: TMenuItem;                          {Menü Profile Yaw}
    mnProf3Axis: TMenuItem;                        {Menü Profile 3Axis}
    mnCutTab: TMenuItem;                           {Menü Table Cut}
    MenuItem55: TMenuItem;
    mnSensorPX4: TMenuItem;                        {Menü Sensor file from YTH Plus}
    mnAnalyseTab: TMenuItem;
    mnGoToErr: TMenuItem;
    mnGoTable: TMenuItem;
    mnOSM: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenuTab: TPopupMenu;
    PopupMenuHist: TPopupMenu;
    PopupMenuProfile: TPopupMenu;                  {Popup Menü Schellanalyse}
    rgQuelle: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    sbtnLogDir: TSpeedButton;
    speDataPoint: TSpinEdit;
    StaticText1: TStaticText;
    StatusBar1: TStatusBar;
    tabOverview: TTabSheet;
    tabScan: TTabSheet;
    tabSettings: TTabSheet;
    tbrDistWP: TTrackBar;
    tbrSaturation: TTrackBar;
    TimerDblClick: TTimer;
    TimerDiashow: TTimer;
    TreeView1: TTreeView;
    XMLPropStorage1: TXMLPropStorage;

    procedure actMAVmessageIDlistExecute(Sender: TObject);
    procedure AppLogMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure AppLogMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure btnAutoCutClick(Sender: TObject);
    procedure btnCombineLogsClick(Sender: TObject);
    procedure btnDefaultProfileClick(Sender: TObject);
    procedure btnDeleteLnClick(Sender: TObject);
    procedure btnCutClick(Sender: TObject);        {Cut flight log}
    procedure btnSplitClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnFlugBuchClick(Sender: TObject);
    procedure btnScanErrClick(Sender: TObject);
    procedure btnSaveApplogClick(Sender: TObject);
    procedure btnDelAppLogClick(Sender: TObject);
    procedure btnConvClick(Sender: TObject);
    procedure btnArchiveClick(Sender: TObject);
    procedure btnShowHexClick(Sender: TObject);
    procedure btnSpecialClick(Sender: TObject);
    procedure cbHighLightChange(Sender: TObject);
    procedure cbMarkerChange(Sender: TObject);
    procedure cbThunderChange(Sender: TObject);
    procedure cbxLogDirChange(Sender: TObject);
    procedure cbxLogDirSelect(Sender: TObject);
    procedure cbxTextMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure cbExtrudeChange(Sender: TObject);
    procedure cbVehicleTypeChange(Sender: TObject);
    procedure cbDashwChange(Sender: TObject);
    procedure cbPilotChange(Sender: TObject);
    procedure ColorButton1Click(Sender: TObject);
    procedure cbxProfilesChange(Sender: TObject);
    procedure cbxProfilesDblClick(Sender: TObject);
    procedure cbxLogDirDblClick(Sender: TObject);
    procedure cbxLogDirMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbxScanDirDblClick(Sender: TObject);
    procedure cbxScanDirMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbxSearchMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure gridDetailsGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure gridOverviewGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure lblGitHubClick(Sender: TObject);
    procedure lblGitHubMouseEnter(Sender: TObject);
    procedure lblGitHubMouseLeave(Sender: TObject);
    procedure mnAutoCutClick(Sender: TObject);
    procedure mnDownloadClick(Sender: TObject);
    procedure mnFlDelClick(Sender: TObject);
    procedure mnReloadClick(Sender: TObject);
    procedure mnSaveTabClick(Sender: TObject);
    procedure mnSplitClick(Sender: TObject);
    procedure pcSettings3Change(Sender: TObject);
    procedure speDataPointEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure lblMAVcommonClick(Sender: TObject);
    procedure lblManualClick(Sender: TObject);
    procedure lblManualMouseEnter(Sender: TObject);
    procedure lblManualMouseLeave(Sender: TObject);
    procedure lblUpdateClick(Sender: TObject);
    procedure lblUpdateMouseEnter(Sender: TObject);
    procedure lblUpdateMouseLeave(Sender: TObject);
    procedure LabeledEdit1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LabeledEdit1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LabeledEdit2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LabeledEdit2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure LabeledEdit3DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LabeledEdit3DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lblMAVcommonMouseEnter(Sender: TObject);
    procedure lblMAVcommonMouseLeave(Sender: TObject);
    procedure lbFlightsClick(Sender: TObject);
    procedure MAVmsgDblClick(Sender: TObject);
    procedure MAVmsgItemClick(Sender: TObject; Index: integer);
    procedure mnSaveAsQuickClick(Sender: TObject);
    procedure mnResetProfileClick(Sender: TObject);
    procedure mnProfModeClick(Sender: TObject);
    procedure mnDelTabClick(Sender: TObject);
    procedure mnStartTabClick(Sender: TObject);
    procedure mnStopTabClick(Sender: TObject);
    procedure mnGoogleMapClick(Sender: TObject);
    procedure mnHomepageClick(Sender: TObject);
    procedure mnCopyHistClick(Sender: TObject);
    procedure mnHexdumpClick(Sender: TObject);
    procedure mnSaveAsHistClick(Sender: TObject);
    procedure mnExploreLogClick(Sender: TObject);
    procedure mnSelDirLogClick(Sender: TObject);
    procedure mnProfErrClick(Sender: TObject);
    procedure mnProfGPSClick(Sender: TObject);
    procedure mnSelDirRecClick(Sender: TObject);
    procedure mnProfThrClick(Sender: TObject);
    procedure mnProfPitchClick(Sender: TObject);
    procedure mnCursorEinClick(Sender: TObject);
    procedure mnCopyTabClick(Sender: TObject);
    procedure mnProfRollClick(Sender: TObject);
    procedure mnProfYawClick(Sender: TObject);
    procedure mnProf3AxisClick(Sender: TObject);
    procedure mnSensorPX4Click(Sender: TObject);
    procedure mnSlideshowClick(Sender: TObject);
    procedure mnGoToTabClick(Sender: TObject);
    procedure mnAnalyseTabClick(Sender: TObject);
    procedure mnGoToErrClick(Sender: TObject);
    procedure mnGoTableClick(Sender: TObject);
    procedure mnOSMClick(Sender: TObject);
    procedure mnCleanCSVClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure rgQuelleClick(Sender: TObject);
    procedure rgOutFormatClick(Sender: TObject);
    procedure rgSpeedUnitClick(Sender: TObject);
    procedure rgAltitudeTypeClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure sbtnLogDirClick(Sender: TObject);
    procedure sbtnScanDirClick(Sender: TObject);
    procedure speLinePathChange(Sender: TObject);
    procedure speProzChange(Sender: TObject);
    procedure speProzEditingDone(Sender: TObject);
    procedure speStkEditingDone(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure StatusBar1Hint(Sender: TObject);
    procedure gridDetailsClick(Sender: TObject);
    procedure gridDetailsDblClick(Sender: TObject);
    procedure gridDetailsHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridDetailsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridDetailsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gridDetailsPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure gridDetailsSelection(Sender: TObject; aCol, aRow: Integer);
    procedure gridOverviewCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure gridOverviewDblClick(Sender: TObject);
    procedure gridOverviewHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridOverviewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridOverviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gridOverviewPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure gridFirmwareKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridFirmwareMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gridScanResultDblClick(Sender: TObject);
    procedure gridScanResultKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridScanResultMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure gridScanResultResize(Sender: TObject);
    procedure tabAnalyze3Resize(Sender: TObject);
    procedure TimerDblClickTimer(Sender: TObject);
    procedure TimerDiashowTimer(Sender: TObject);
    procedure tbrSaturationChange(Sender: TObject);
    procedure tbrDistWPChange(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
    function VtoProz(const vt: integer; u: double): integer;
    function ShowVoltageF(const w: double; vt: byte): string;
    function ShowVoltage(const s: string; vt: byte): string;
    function CheckNumTurns(const dn: string): integer;
    procedure AnzeigeCSV(const mode: integer);
    procedure BrAnzeigeCSV(const mode: integer);
    procedure H501AnzeigeCSV(const mode: integer); {H501 Dateien als Tabelle anzeigen}
    procedure MQAnzeigeCSV(const mode: integer);   {Mantis Q CSV Format anzeigen}
    procedure HDiagramm(fn: string);
    procedure HDiaColor;                           {Colors for Bar series depending on vehicle type}
    procedure BrHDiagramm(fn: string);
    procedure H501HDiagramm(fn: string);           {Höhenprofil H501 anzeigen}
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
    procedure h501ProtoWerte(fn: string;           {Dateiname}
                         olist: TstringList;       {Datensatz per Flug/Datei}
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
    procedure ZhLWerte(p: byte);                   {Werte zählen}
    procedure DiaWerte(p: byte);                   {Werte als Diagramm}
    function GethFromST10(const z: integer; const dt: TDateTime): double; {Höhe aus RemoteGPS_xxxx}
    function vms(const d: TDateTime; const w: double): string;
    procedure SelDirSet;
    procedure SelDirProt;
    procedure SelDirAct(const fn: string);         {Alles neu laden}
    procedure FirmwareLesen;
    procedure PlatformLesen;
    procedure YFlugBuch;
    procedure BrFlugBuch;
    procedure H501FlugBuch;
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
    function ZeitToDT(const s: string; const vt: byte): TDateTime;
    function FindTP(wlist: TStringList; tp: TDateTime; const vt: byte): integer;
    function CheckE7(const s: string): boolean;    {prüft einen string auf Fehler}
    function ShowSensorPlus(fn: string;            {Sensordatei vom YTH Plus}
                            fnz: integer;          {file number, index of file}
                            gx, tb,                {gx: Track for kml, tb: fill table gridDetails}
                            ov10,                  {Overview PX4 Emergency}
                            ov11: boolean): boolean; {PX4 Overview Text}
    procedure AusgabeMessages(const fn: string;
                     var outlist: TStringList);    {Datenausgabe MsgID sortiert}
    function ComplFN(st: string; tp: TDateTime): string;    {Dateinamen mit Nummer ergänzen}
    procedure AppLogTimeStamp(s: string);          {AppLogHighlighter einteilen}
    procedure OpenSensorPlus;                      {Sensordatei vom YTH Plus öffnen}
    function IsMantisQ(const fn: string): boolean; {Auf PX4 Quadcopter prüfen}
    procedure ShowMQ;                              {Anzeige SensorFile MQ}
    procedure ShowH520;                            {Anzeige TLOG File H520}
    procedure KMLheader(f: string; dt: TDateTime; klist: TStringList);
    procedure HDiaInit;                            {HöhenDiagramm Anzeige rücksetzen}
    procedure ShowSensorH(const fn: string; mode: integer); {Sensor File YTH}
    procedure SetSensorEnv;                        {Bedienung für Sensor anpassen}
    procedure ResetSensorEnv;
    procedure AnzeigePX4CSV(fn: string);           {CSV aus eigenem Format anzeigen}
    function FakeHeader: string;                   {Missing Header for H920+ST24}
    procedure OverwriteVT;                         {Overwrite vehicle type for PX4 Thunderbird}
    procedure GetDefVT;                            {Fill defVT depending on settings}
    procedure MAVmsgDefault;                       {Set all messages to true}
    procedure HexAusgabe(const fn: string);        {Tools: Display a binary file as hex print}
    procedure HexHeader(const fn: string);         {Write header for file and take block size}
    procedure CheckVersion;                        {Call version file and check}
    procedure SplitSensorPlus;                     {Split PX4 Sensor file}
    procedure ManualCut;                           {Manuelles Ausschneiden}
    procedure CutLegacy(mode: integer);            {mode = 1: Automatisches Ausschneiden}
    procedure EnableMultiselect;                   {Multiselect flight list to combine files}
    procedure CombineLogs;                         {Combine some legacy Yuneec Flightlog to one log files set}
{Special analysis with (hidden) extra button "Special" on Settings > Common settings}
//    procedure TLOGanalysis(fn: string);            {Special analysis: TLOG altitudes}
//    procedure AuswertungCSVdatei;                {Special analysis: allg. für CSV-Datei}
    procedure IMUstatusCheck;                      {Special analysis: List all IMU stuts during flight}
  end;

{$I language.inc}

const
  hpmydat='/pdf/';
  meinname='Helmut Elsner';
  email   ='helmut.elsner@live.com';               {My e-mail address}
  lazURL  ='https://www.lazarus-ide.org/';         {Advertisement}
  lazForum='https://www.lazarusforum.de/app.php/portal';

  fix='Fix';
  wldcd='*';
  us1='_1';
  h5file='H501_';                                  {Dateinamen von flaretom Recoder}
  sextP=wldcd+wext;                                {Suche Sensor files YTH Plus}
  pngdef=us1+'.png';                               {Dateivorschläge}
  csvdef=us1+fext;
  wexdef=us1+wext;
  sep=',';                                         {Datenseparator im CSV file}
  anzsp=20;                                        {Mindestanzahl Spalten}
  lzbr=19;                                         {Länge Zeitstempel Breeze}
  lzyu=21;                                         {Länge Zeitstempel Yuneec}
  fw0=80;                                          {Breite linke Spalte Übersicht default}
  fw1=160;                                         {Breite linke Spalte Übersicht bei alter FW}
  hsw=2;                                           {Höhen-Schwellwert für Analyse in m}
  minlines=10;                                     {Minimum number if lines that makes sense to deal with}
  exID=' ';                                        {ID for values updated by in StringGrid}
  SpaceAutoCut=12;                                 {Number of rows before start and after end for AutoCut}

{http://docwiki.embarcadero.com/RADStudio/Seattle/de/Farben_in_der_VCL}
  osmURL='https://www.openstreetmap.org/';         {als Karte}
//  osmURL='https://opentopomap.org/';             {als topografische Karte}
//  https://opentopomap.org/#map=17/48.92523/14.26150

  gzoom='16';
  starticon='https://maps.google.com/mapfiles/dir_0.png';       {blau}
  stopicon= 'https://maps.google.com/mapfiles/dir_walk_60.png'; {grau}
  alerticon='https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_red.png';
  infoicon= 'https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_yellow.png';
  aircrafticon='https://earth.google.com/images/kml-icons/track-directional/track-0.png';
  fotoicon='https://maps.google.com/mapfiles/kml/pal4/icon38.png';
//  wpicon='http://maps.google.com/mapfiles/kml/pal4/icon49.png';
  wpicon='https://maps.gstatic.com/mapfiles/ridefinder-images/mm_20_shadow.png';

{Mapicons: https://sites.google.com/site/gmapicons/home/
           https://sites.google.com/site/gmapsdevelopment/
           http://kml4earth.appspot.com/icons.html  }


  xmlvers='<?xml version="1.0" encoding="UTF-8"?>';  {ID XML/GPX header}
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
  nmtag='name';
  doctag='Document>';
  cotag='coordinates>';
  extru='<extrude>1</extrude>';
  itagin='<!-- ';
  itagout=' -->';
  tr='"'+suff;
  wpem='},';                                       {Waypoint Endemarke}

  spt=10;                                          {zusätzliche Spaltenbreite  für Telemetrie in pix}
  rrk='# ';                                        {RaceRender Kommentar}
  FWsz=18;                                         {Mindestgröße Datei für FW}

  tabu=19;
  lblsize=35;                                      {LabelSize zum Ausrichten der Y-Achsen bei Schnellanalyse}

var
  Form1: TForm1;
  InitDone: boolean=false;                         {OnShow kann mehrfach aufgerufen werden}
  tend, tpos, cutb, cute: TDateTime;               {Ende-Zeit, Zeit-Position in Tabellen}
  cutbidx: integer;                                {Index to what file the timestamps belong}
  kpath: string;
  topp: array of array[0..6] of integer;           {Positionen Topzeile der Tabellen
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
  CellFocus: array[0..3, 0..1] of integer;         {source, cell [col, row]}
  nhw: double;                                     {Delta für Koordinaten in der Nähe}
  farbskala: array [0..2, 0..63] of TColor;        {Farbeverlauf entspreched Werte}

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

procedure SizeSpeedBtn(var btn: TSpeedbutton; cbx: TComboBox);  {Anpassen Speedbuttons an Darstellung in verschiedenen Betrienssystemen}
begin
  btn.Height:=cbx.Height;
  btn.Width:=btn.Height;                           {Default: Button quadratisch}
  btn.Top:=cbx.Top;
  btn.Left:=cbx.Left+cbx.Width+4;
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
          v_type:=YTHPid;
          StaticText1.Caption:=vtypeToStr(v_type); {Type hard wired}
          cbxText.Text:=StaticText1.Caption;       {Drone ID}
          Merkliste(cbxText, speItems.Value);
          for k:=0 to i do
            result:=result+splitlist[k]+Pathdelim;
          exit;
        end;

      for i:=splitlist.Count-2 downto 0 do         {FlightLog von hinten suchen}
        if pos(mndir, splitlist[i])=1 then begin   {legacy Yuneec}
          result:='';                              {Pfad zusammenbauen}
          for k:=0 to i do
            result:=result+splitlist[k]+Pathdelim;
          exit;
        end;
    end;
    if ExtractFileExt(fn)=bext then begin
      result:=ExtractFilePath(fn);                 {Breeze}
      v_type:=BRid;
      exit;
    end;
    if (pos(h5file, fn)=1) and
       (ExtractFileExt(fn)=fext) then begin        {Hubsam 501 flaretom Recorder}
      result:=ExtractFilePath(fn);
      v_type:=H501ID;                              {Vehicle ID für <> legacy setzen}
    end;
  finally
    FreeAndNil(splitlist);
  end;
end;

procedure ResetCut;                                {Reset timestamps for Cut}
begin
  cutb:=0;                                         {Zeitstempel zum Ausschneiden}
  cute:=0;                                         {Beginn und Ende löschen}
  cutbidx:=-1;
end;

procedure TForm1.FormCreate(Sender: TObject);      {Anwendung initialisieren und starten}
var i: integer;

begin
  Randomize;                                       {Zufallsgenerator initialisieren}
  v_type:=DefVT;
  nhw:=0.00002;                                    {default Koordinaten in der Nähe}
  ResetCut;                                        {Reset timestamps for Cut}
  topp:=nil;
  SetLength(topp, 4);                              {Topzeilen array}
  kpath:=IncludeTrailingPathDelimiter(dkpath);     {default setzen}
  AppLog.Lines.Clear;
  AppLogTimeStamp(AppName+tab4+AppVersion);
  AppLog.Color:=AppLog.GetColorResolvingParent;
  AppLog.Font.Color:=(not ColorToRGB(AppLog.Color)) and $00FFFFFF;
  AppLog.RightEdgeColor:=AppLog.Color;             {Make it invisible}
  Caption:=capForm1+tab2+AppVersion;               {Name und Version}
  Hint:=capForm1;
  btnClose.Caption:=capBitBtn1;
  btnClose.Hint:=hntBitBtn1;
  btnConv.Caption:=capBitBtn2;
  btnArchive.Caption:=capBitBtn3;
  btnDefaultProfile.Caption:=capBitBtn12;          {Schnellanalyse Defaulteinstellung}
  btnDefaultProfile.Hint:=hntBitBtn12;

{$IFDEF DARWIN}                                    {MAC OS X}
  mnSaveAsHist.Visible:=false;                     {PopUp Menu Höhenprofil; geht das nun?}
{$ENDIF}

{https://forum.lazarus.freepascal.org/index.php?topic=34510.0}
{$IFDEF WINDOWS}
  Application.MainFormOnTaskBar := True;
{$ENDIF}

  SizeSpeedBtn(sbtnLogDir, cbxLogDir);             {Anpassen Speedbuttons an Darstellung in verschiedenen Betrienssystemen}
  SizeSpeedBtn(sbtnScanDir, cbxScanDir);

  btnCut.Caption:=capCut;                          {Ausschneiden / Cut}
  btnCut.Hint:=hntCut;
  btnAutoCut.Caption:=capAutoCut;                  {Ausschneiden / AutoCut}
  btnAutoCut.Hint:=hntAutoCut;
  btnSplit.Caption:=capSplit1;                     {Menu: Split PX4 Sensor file at time resets}
  btnSplit.Hint:=hntSplit;
  btnFlugBuch.Caption:=capNachweis;                {Flugprotokoll}
  btnCombineLogs.Caption:=capCombineLogs;
  btnCombineLogs.Hint:=hntCombineLogs;
  lbFlights.Hint:=hntListBox1;
  ColorButton1.Hint:=hntColorBtn1;
  StatusBar1.Panels[5].Text:=DefaultStatus;
  cbxLogDir.TextHint:=DefaultStatus;
  cbxLogDir.Text:=DefaultStatus;
  sbtnLogDir.Hint:=DefaultStatus;
  sbtnScanDir.Hint:=capSelProt;                    {Protokollverzeichnis}
  mnGoogleMap.Caption:=rsToGMaps;
  mnCopyHist.Caption:=rsToClipBoard;
  mnSaveAsHist.Caption:=rsHDiaSave;
  mnCursorEin.Caption:=capCrossHairOn;
  mnCursorEin.Hint:=hntCrossHair;
  mnGoToTab.Caption:=capGoToZNr;
  mnAnalyseTab.Caption:=capTabSheet10;
  mnGoToErr.Caption:=capMenuItem7;
  mnGoTable.Caption:=capMenuItem8;
  mnOSM.Caption:=rsToOSM;
  mnDelTab.Caption:=rsResetCutBE;
  mnStartTab.Caption:=rsStartTpunkt;
  mnStopTab.Caption:=rsEndTPunkt;
  mnDelHist.Caption:=rsResetCutBE;
  mnMainFile.Caption:=capDatei;
  mnConvert.Caption:=capBitBtn2;
  mnMainHelp.Caption:=capHelp;
  mnManual.Caption:=capLabel7;
  mnInfo.Caption:=capInfo;
  mnDownload.Caption:=capLabel8;
  mnMainDel.Caption:=rsResetCutBE;
  mnMainStart.Caption:=rsStartTPunkt;
  mnMainStop.Caption:=rsEndTPunkt;
  mnCut.Caption:=capCut;                           {Ausschneiden / Cut}
  mnCutTab.Caption:=capCut;                        {Ausschneiden / Cut}
  mnArchive.Caption:=capBitBtn3;
  mnClose.Caption:=capBitBtn1;
  mnExploreLog.Caption:=capOpen;
  mnSelDirLog.Caption:=capSelDir;
  mnMainScanRec.Caption:=capGroupBox10;
  mnSelDirRec.Caption:=capSelProt;
  mnExploreRec.Caption:=capMI47;
  mnCopyTab.Caption:=rsCopy;
  mnResetProfile.Caption:=capBitBtn12;             {Menü Default}
  mnSaveAsQuick.Caption:=rsPC1Tab4;                {Menü Settings}
  mnSensorPX4.Caption:=capSensorPlus;              {Sensordatei für YTH Plus öffnen}
  mnSlideshow.Caption:=capDiashow;                 {alle Profiles anzeigen}
  mnCleanCSV.Caption:=capCleanCSV;
  mnCleanCSV.Hint:=hntCleanCSV;
  actMAVmessageIDlist.Caption:=capMAVlist;         {Tools: List of used MAVlink messages}
  actMAVmessageIDlist.Hint:=hntMAVlist;
  mnFlDel.Caption:=capFlDel;
  mnFlDel.Hint:=hntFlDel;
  mnReload.Caption:=capReload;
  mnReload.Hint:=hntReload;
  mnSplit.Caption:=capSplit;                       {Menu: Split PX4 Sensor file at time resets}
  mnSplit.Hint:=hntSplit;
  mnSaveTab.Caption:=capSaveTab;
  mnAutoCut.Caption:=capAutoCut;
  mnAutoCut.Hint:=hntAutoCut;
  rgQuelle.Caption:=capRadioGroup1;
  rgQuelle.Hint:=hntRadioGroup1;
  rgQuelle.Items[0]:=dkpath;
  rgQuelle.Items[1]:=spath;
  rgQuelle.Items[2]:=fpath;
  rgQuelle.Items[3]:=npath;
  rgOutFormat.Caption:=capRadioGroup2+tab1+capBitBtn2;
  rgOutFormat.Hint:=hntRadioGroup2;
  rgSpeedUnit.Caption:=capRadioGroup3;
  rgTimeType.Caption:=capDirSuffix+tab1+capBitBtn3;
  rgTimeType.Hint:=hntDirSuffix;
  rgAltitudeType.Caption:=capGroupBox5;
  rgAltitudeType.Hint:=hntGroupBox5;
  rgCSVtext.Caption:=capRadioGroup2+tab1+capNachweis;
  rgCSVtext.Hint:=hntRadioGroup8;
  rgVoltRule.Caption:=capVrule;
  rgVoltRule.Hint:=hntVrule;
  StatusBar1.Hint:=hntStatus1;
  tabOverview.Caption:=rsPC1Tab1;
  tabHdia.Caption:=rsPC1Tab2;
  tabDetails.Caption:=rsPC1Tab3;
  tabSettings.Caption:=rsPC1Tab4;
  tabScan.Caption:=capScan;
  tabScan.Hint:=hntScan;
  tabAppLog.Hint:=tabAppLog.Caption;
  tabAnalyze3.Caption:=capAnalyse;
  tabConvert.Caption:=capTabSheet9;
  tabConvert.Hint:=capTabSheet9;
  tabData.Caption:=capTabSheet10;
  tabData.Hint:=capTabSheet10;
  gbDiverse.Caption:=capDiverse;
  tabCommon.Caption:=capTabSheet12;
  tabCommon.Hint:=capTabSheet12;
  Label1.Caption:=capLabel1;
  Label1.Hint:=hntLabel1;
  speLinePath.Hint:=hntLabel1;
  Label2.Caption:=capLabel2;
  Label3.Caption:=capLabel3;
  lblSaturation.Hint:=hntTrackBar1;
  lblNumPoints.Caption:=capLabel5;
  Label6.Caption:=capLabel6;
  lblManual.Caption:=capLabel7;
  lblUpdate.Caption:=capLabel8;
  lblDistWP.Caption:=capLabel12;
  lblDistWP.Hint:=hntTrackBar2;
  lblBegin.Caption:=capLabel13;
  lblBegin.Hint:=capLabel13;
  lblEnde.Caption:=capLabel14;
  lblEnde.Hint:=capLabel14;
  lblDuration.Caption:=rsDauer;
  lblDuration.Hint:=rsDauer;
  Label10.Caption:=capItems;
  Label10.Hint:=hntItems;
  speItems.Hint:=hntItems;
  lblManual.Hint:=Application.Location+manual;        {default}
  if not FileExists(lblManual.Hint) then begin        {if no local file available}
    lblManual.Hint:=homepage+hpmydat+manual;          {overwrite with internet link}
  end else begin
  {$IFDEF DARWIN}
    lblManual.Hint:=manual;                           {für MAC OS X überschreiben}
  {$ENDIF}
  end;
  lblUpdate.Hint:=homepage+downURL;
  lblMAVcommon.Hint:=MAVurl;
  StaticText1.Caption:='';
  cbxScanDir.Hint:=capSelProt;
  cbxSearch.Hint:=hntComboBox9;
  tbrSaturation.SelEnd:=tbrSaturation.Position;
  tbrDistWP.SelEnd:=tbrDistWP.Position;
  tbrDistWP.Hint:=hntTrackBar2;
  cbxText.Hint:=hntEdit1;
  cbDashw.Caption:=capCheckBox3;
  cbDashw.Hint:=hntCheckBox3;
  cbSimu.Caption:=capCheckBox6;
  cbSimu.Hint:=hntCheckBox6;
  cbPilot.Caption:=capCheckBox7;
  cbPilot.Hint:=hntCheckBox7;
  cbMAVasCSV.Caption:=capCheckBox8;                {Save Sensor as CSV}
  cbMAVasCSV.Hint:=hntCheckBox8;
  cbCleanHplus.Caption:=capCheckBox9;              {YZH Plus Datenbereinigung}
  cbCleanHplus.Hint:=hntCheckBox9;
  cbSensorKML.Caption:=capCheckBox10;              {Flight path from PX4 sensor}
  cbSensorKML.Hint:=hntCheckBox10;
  cbExtrude.Hint:=hntCheckBox11;                   {add extrude 1 tag to KML}
  cbCap.Caption:=cbCapCaption;                     {Remaining capacity instead voltage}
  cbCap.Hint:=cbCapHint;
  cbReduced.Caption:=capReduced;                   {AppLogHighlighter reduced, only text msg}
  cbReduced.Hint:=hntReduced;
  cbThunder.Caption:=capThunder;
  cbThunder.Hint:=hntThunder;
  cbHighLight.Caption:=capHighLight;
  cbHighLight.Hint:=hntHighLight;
  cbSensorHasData.Caption:=capSensorHasData;
  cbSensorHasData.Hint:=hntSensorHasData;
  GroupBox2.Caption:=capGroupBox2;
  GroupBox3.Caption:=rsPC1Tab2;
  GroupBox4.Caption:=capAnalyse;
  GroupBox4.Hint:=hntGroupBox4+capAnalyse;
  GroupBox10.Caption:=capGroupBox10;               {Flugprotokoll}
  GroupBox10.Hint:=hntGroupBox10;
  GroupBox11.Caption:=rsFind;                      {Suche}
  GroupBox11.Hint:=hntComboBox9;
  gbBatt.Caption:=capgbBatt;
  gbBatt.Hint:=hntgbBatt;
  lblBaseLoad.Caption:=capBaseLoad;
  lblBaseLoad.Hint:=hntBaseLoad;
  speBaseLoad.Hint:=hntbaseLoad;
  ColorButton1.Caption:=capColorButton1;
  tbrSaturation.Hint:=hntTrackBar1;
  speDataPoint.Hint:=hntSpinEdit3;
  Label6.Hint:=hntSpinEdit3;
  LabeledEdit1.EditLabel.Caption:=capTopDia;
  LabeledEdit2.EditLabel.Caption:=capMiddleDia;
  LabeledEdit3.EditLabel.Caption:=capBottomDia;
  DefaultFormatSettings.DecimalSeparator:='.';
  tend:=0;
  rgQuelle.ItemIndex:=0;
  gridDetails.Tag:=DefaultPosFlightMode;           {default Position bei neuer FW ST10+}
  gridDetails.ColWidths[0]:=130;
  gridDetails.Hint:=hntGrid1;                      {Default Hint data}
  gridDetails.ColCount:=defaultcol;
  gridOverview.ColWidths[0]:=fw0;
  gridOverview.Hint:=rsPC1Tab1;                    {Default Hint Overview}
  gridOverview.Cells[1,0]:=rsGridCell1;            {Übersichtstabelle aufbauen}
  gridOverview.Cells[2,0]:=rsGridCell2;
  gridOverview.Cells[3,0]:=rsGridCell3;
  gridOverview.Cells[4,0]:=rsDauer;
  gridOverview.Cells[5,0]:=rsGridCell5;
  gridOverview.Cells[6,0]:=rsGridCell6;
  gridOverview.Cells[7,0]:=rsGridCell7;
  gridOverview.Cells[8,0]:=rsGridCell8;
  gridOverview.Cells[9,0]:=rsGridCell9;
  gridOverview.Cells[10,0]:=rsGridCell10;
  gridOverview.Tag:=1;                             {Used Tab; default: Tabelle}
  gridFirmware.Hint:=hntStringGrid4;
  pcMain.ActivePage:=tabOverview;
  pcSettings3.ActivePage:=tabConvert;
  pcSettings3.Hint:=rsPC1Tab4;                     {Einstellungen}
  Chart3.AxisList[0].LabelSize:=lblsize;           {y-Achsen ausrichten}
  Chart4.AxisList[0].LabelSize:=lblsize;
  Chart5.AxisList[0].LabelSize:=lblsize;
  Chart3.Hint:='';
  Chart4.Hint:='';
  Chart5.Hint:='';
  gridScanResult.Hint:=rsResult;
  gridScanResult.Cells[0,0]:=rsNum;
  gridScanResult.Cells[1,0]:=rsResult;
  btnScanErr.Caption:=capScan;
  btnSaveApplog.Caption:=sckey+rsSave;             {AppLogH speichern}
  btnSaveApplog.Hint:=hntBitBtn27;
  btnDelAppLog.Caption:=sckey+capDel;              {AppLog löschen}
  btnDelAppLog.Hint:=hntDel;
  btnShowHex.Caption:=sckey+capHexdump;
  btnShowHex.Hint:=hntHexdump;
  rgBlockSize.Caption:=capBlockSize;
  rgBlockSize.Hint:=hntBlockSize;
  speBlockNum.Hint:=hntBlockNum;
  mnHexDump.Hint:=hntHexdump;
  mnHexDump.Caption:=capHexdump;
  ProgressBarScan.Hint:=hntProgrBar1;
  rgErrType.Caption:=capProb;
  GroupBox6.Caption:=capProbScan;
  GroupBox6.Hint:=hntProb;
  gbStkProz.Caption:=capStkProz;                   {Stick to percent converter}
  gbStkProz.Hint:=hntStkProz;
  Label11.Caption:=capStk;
  Label17.Caption:=capProz;
  lblGitHub.Caption:=capGitHub;
  lblGitHub.Hint:=githublink;
  speStk.MaxValue:=stkmax;
  speStk.Value:=stkntrl;
  speProz.Value:=StkToProz(stkntrl);
  Chart1BarSeries1.SeriesColor:=clAngle;           {Angle Mode – Purple status LED}
  Chart1BarSeries2.SeriesColor:=clSmart;           {für Smart Mode - Green status LED}
  Chart1BarSeries3.SeriesColor:=clRTH;             {für RTH}
  Chart1BarSeries4.SeriesColor:=clEmergency;       {Emergency}
  Chart1BarSeries5.SeriesColor:=clNoGPS;           {Ohne GPS Orange}
  Chart1BarSeries6.SeriesColor:=clSport;           {Sports Mode, Stability}
  btnDeleteLn.Caption:=capDeleteLn;
  btnDeleteLn.Hint:=hntDeleteLn;
  edDeleteLn.Hint:=hntDeleteEd;

{GeoTagging}

{Mit Hue, Luminance und Saturation gearbeitet (gleichbedeutend mit HSV oder TSW)}
  for i:=0 to High(farbskala[0]) do begin {nur einmal aufbauen und dann über Index nutzen}
    farbskala[0, i]:=HLStoColor(160, 255-round(i/High(farbskala[0])*150), 230);  {blau}
    farbskala[1, i]:=HLStoColor(80,  255-round(i/High(farbskala[0])*150), 230);  {grün}
    farbskala[2, i]:=HLStoColor(0,   255-round(i/High(farbskala[0])*150), 230);  {rot}
  end;

  for i:=0 to 3 do begin
    CellFocus[i, 0]:=0;                            {Focus default settings: Column}
    CellFocus[i, 1]:=1;                            {Row}
  end;

  tpos:=0;                                         {keine Position in der Tabelle}
  StatusBar1.Panels[2].Text:=rgOutFormat.Items[rgOutFormat.ItemIndex];
  btnConv.Hint:=hntBitBtn2+' (+ '+StatusBar1.Panels[2].Text+')';
  btnArchive.Hint:=hntBitBtn3+' (+ '+rgTimeType.Items[rgTimeType.ItemIndex]+')';
  rgVehicleType.Enabled:=cbVehicleType.Checked;
  MAVmsgDefault;                                   {default: all MAV messages selected}
  MAVmsg.Hint:=hntMAVmsg;
  FreigabeCut(false);                              {ohne Statusausgabe}
end;

procedure TForm1.FormActivate(Sender: TObject);    {Action after initializing}
var
  ctrlkeypressed: TShiftState;

begin
  ctrlkeypressed:=GetKeyShiftState;                {Hold Ctrl key during start-up to get special analysis}
  if ssCtrl in ctrlkeypressed then begin
    btnSpecial.Visible:=true;
    pcMain.ActivePage:=tabSettings;
    pcSettings3.ActivePage:=tabCommon;
    btnSpecial.SetFocus;
  end;
  EnableMultiselect;
end;

procedure TForm1.gridDetailsGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if ARow>0 then begin
    HintText:=GetCellInfo(aCol, aRow)
  end else begin
    if rgQuelle.ItemIndex=3 then                   {Hint bei Sensor ausblenden}
      HintText:=gridDetails.Cells[aCol, 0]
    else
      gridDetails.Hint:=hntGrid1;
  end;
  if (aRow=0) and (rgQuelle.ItemIndex=2) then      {Kanalzuordnung Remote}
    HintText:=ChToStr('', aCol);                   {CH vs Ch --> Channel settings}
end;

procedure TForm1.gridOverviewGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if (aCol>0) and (aRow>0) then begin
    if aRow=gridOverview.RowCount-1 then begin     {Last line: Sumn}
      case aCol of
        1: HintText:=hntNumFlt;
        4: HintText:=hntDauer;
        7: HintText:=hntStrecke;
        8: HintText:=hntHGeschw;
        else HintText:=rsPC1Tab1;                  {default}
      end;
    end else begin
      case aCol of
        9, 10: HintText:=                          {min/max voltage}
               ShowVoltageF(GetFVal(gridOverview.Cells[aCol, aRow]), v_type);
      else
        HintText:=gridOverview.Cells[aCol, 0]+'='+
                           gridOverview.Cells[aCol, aRow];     {Default hint}
      end;
    end;
  end else
    HintText:=gridOverview.Cells[aCol, aRow];      {Header}
end;

procedure TForm1.EnableMultiselect;                {Multiselect flight list to combine files}
begin
  lbFlights.MultiSelect:=(pcMain.ActivePage=tabSettings) and
                         (pcSettings3.ActivePage=tabData);
end;

function OpenManual: boolean;                      {Handbuch aufrufen}
begin
  if not FileExists(Application.Location+manual) then
    result:=OpenURL(homepage+hpmydat+manual)
  else
    result:=OpenDocument(Application.Location+manual);
end;

{Zeitstempel to TDateTime; Format abhängig vom Vehicle Type
 legacy Yuneec: 20151206 11:32:57:234
 Mantis Q CSV:  2019-02-28 17:53:44.401
 Breeze:        2015-12-06 11:32:57               }
function TForm1.ZeitToDT(const s: string; const vt: byte): TDateTime;
begin
  try
    case vt of
      brID: begin                                  {Breeze}
              result:=ScanDateTime(vzf, s);
              if btnArchive.Tag=1 then             {Platform Android}
                result:=result-nowUTC+now;         {UTC to local time}
            end;
      MQcsvID: result:=ScanDateTime(dzf+' '+zzf+zzz, s);
      H501ID: result:=ScanDateTime(zzf, s);        {Time format flaretom}
    else
      result:=ScanDateTime('yyyymmdd '+zzf+':zzz', s); {Yuneec legacy format}
    end;
  except
    result:=0;
  end;
end;

{Finde einen Zeitstempel in den Dateien mit Datum/Zeit in der 1. Spalte}
function TForm1.FindTP(wlist: TStringList;         {Data list as strings}
                       tp: TDateTime;              {Time looking for}
                       const vt: byte): integer;   {Vehicle type}
var k, pos, len: integer;
    s: string;

begin
  result:=0;                                       {nothing found}
  pos:=1;
  len:=lzyu;                                       {21: length DT Yuneec}
  case vt of
    brID:    begin                                 {Breeze}
               pos:=9;
               len:=lzbr;                          {19: length DT Breeze}
             end;
    MQcsvID: begin                                 {Mantis Q CSV format}
               pos:=16;
               len:=lzyu+2;                        {23: length DT Mantis Q}
             end;
    H501ID: len:=8;                                {H501 time format flaretom}
  end;

  for k:=pos to wlist.Count-1 do begin
    s:=copy(wlist[k], 1, len);
    if ZeitToDT(s, vt)>=tp then begin
      result:=k;
      break;
    end;
  end;
end;

{File name part}
function GetDateFromFile(s: string): TDateTime;    {Date from Filename H501}
begin
  try
    result:=ScanDateTime(dzf, copy(s, 1, 10));
  except
    result:=trunc(now);
  end;
end;

function FrameToStr(fr: integer): string;          {For H501, Col 1}
begin
  result:='';
  if fr=0 then
    result:='---'
  else begin
    if (fr and 1)=1 then result:='NavData ';
    fr:=fr shr 1;
    if (fr and 1)=1 then result:=result+'TeleData ';
    fr:=fr shr 1;
    if (fr and 1)=1 then result:=result+'Control';
  end;
end;

function H501alt(h: double): double; inline;       {H501 Altitude}
begin
  result:=h*10;
end;

function H501dist(d: double): double; inline;      {H501 distance}
begin
  result:=d*10;
end;

function H501velo(v: double): double; inline;      {H501 speed}
begin
  result:=v/10;
end;

function FramesToColor(c: integer): TColor;        {Colors for the frame settings}
begin
  case c of
    0: result:=clRed;                              {No frame}
    1, 2, 4: result:=clOrange;                     {One frame}
    3, 5, 6: result:=clBlue;                       {Two frames}
  else
    result:=clGreen;                               {All frames}
  end;
end;

function RandomFN(const fn: string; const vt, mode: integer): string; {Dateinamen ermitteln}
var p: integer;                                    {Position zum zufälligen Ändern}
    z: char;

begin
  result:=IncludeTrailingPathDelimiter(ExtractFilePath(fn))+'cut'+us1+bext;
  case vt of
    brID:   p:=UTF8length(fn)-4;                   {Breeze}
       4:   p:=UTF8length(fn)-7;                   {Chroma}
    H501ID: p:=UTF8length(fn)-6;                   {flaretom file name format}
  else
    p:=UTF8length(fn)-8;                           {legacy Yuneec}
  end;

  if fn.length>p then begin
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

function TForm1.CheckE7(const s: string): boolean; inline;
                                                   {prüft einen string auf Fehler}
begin
  result:=(not cbCleanHplus.Checked) or            {Prüfung ggf. abschalten}
          (pos('E7', s)=0);                        {E7 darf in Telemetrie nicht vorkommen}
end;

procedure RSSIpToColor(aGrid: TStringGrid; r: integer);   {RSSI in Prozent einfärben}
begin
  if r<30 then                                     {below 30%}
    CellColorSetting(aGrid, clAttention);
  if r<15 then                                     {below 15%}
    CellColorSetting(aGrid, clError);
end;

procedure VoltToColor(aGrid: TStringGrid; v: double); {LiPo thresholds for colors}
var w: double;

const
  thr_yellow=3.69;
  thr_attention=3.61;
  thr_red=3.26;
begin
  case v_type of                                   {Volts per cell}
    1, 6:   w:=v/6;                                {H920 6S}
    5, YTHPid, ThBid, H5id:
            w:=v/4;                                {H480/520 4S}
    H501ID: w:=v/2;                                {Hubsan 2S}
  else
    w:=v/3;                                        {all other 3S}
  end;
  if w<thr_red then
    CellColorSetting(aGrid, clError)
  else begin
    if w<thr_yellow then
      CellColorSetting(aGrid, clFairGood);
    if w<thr_attention then
      CellColorSetting(aGrid, clAttention);
  end;
end;

function VPerCell (const vt: integer; const u: double): string;  {Voltage per cell}
begin
  case vt of
    1, 6:     result:=FormatFloat(ctfl, u/6)+rsPerCell; {H920 6S}
    5, YTHPid, ThBid, H5id:
              result:=FormatFloat(ctfl, u/4)+rsPerCell; {H480/520 4S}
    H501ID:   result:=FormatFloat(ctfl, u/2)+rsPerCell; {Hubsan 2S}
  else
    result:=FormatFloat(ctfl, u/3)+rsPerCell;           {all 3S}
  end;
end;

function RSSItoColor(r: double): TColor;           {Color code for RSSI value}
begin
  result:=clError;
  if r<85 then
    result:=clAttention;
  if r<70 then
    result:=clFairGood;
  if r<55 then
    result:=clVeryGood;
end;

function TForm1.VtoProz(const vt: integer; u: double): integer;
begin                                              {Choose rule to convert V to %}
  result:=0;
  case rgVoltRule.ItemIndex of
    0: result:=VtoProzY(vt, u);                    {Yuneec rule}
    1: result:=VtoProzRC(vt, u);                   {RC-Groups}
  end;
end;

function TForm1.ShowVoltageF(const w: double; vt: byte): string;
begin
  result:=rsRest+' ~'+IntToStr(VtoProz(vt, w))+    {Voltage in %}
          '%'+kma+VperCell(vt, w);                 {V per cell}
end;

function TForm1.ShowVoltage(const s: string; vt: byte): string;
var v: double;

begin
  v:=StrToFloatDef(s, 0);
  result:=ShowVoltageF(v, vt);
end;

function GetNr(const s: string): string; {filtert Ziffern aus einem String für Dateiname}
begin
  result:='';
  if s<>'' then begin
    case v_type of
       MQid: begin                                 {MantisQ}
               result:=ChangeFileExt(s, '');
               result:=StringReplace(result, nfile, '', [rfIgnoreCase]);
               result:=StringReplace(result, mfile, '', [rfIgnoreCase]);
             end;
       H5id: result:=ChangeFileExt(s, '');         {H520, ganzer Namensstamm}
       brID: result:=ChangeFileExt(s, '');         {Breeze, new firmware}
       H501ID: result:=Copy(s, 6, 19);             {flaretom file name format}

       else result:=CleanNum(s);
    end;
  end;
end;

function ResultDN(const s, ex: string): string;    {erzeuge Ergebnis-Dateinamen}
begin
  case v_type of
    brID: result:=ExtractFileDir(s)+PathDelim+     {im gleichen Verzeichnis}
                  '#'+GetNr(ExtractFileName(s))+ex;
  else                                             {Yuneec legacy}
    result:=ExtractFileDir(s)+
            GetNr(ExtractFileName(s))+ex;          {im übergeordnedten Dir}
  end;
end;


function NichtLeer(const s: string): boolean; inline;
                    {Identifikation eines leeren Feldes oder Feld mit 0 Wert}
var w: string;

begin
  result:=true;
  w:=trim(s);
  if (w='') or (StrToFloatDef(w, 0)=0) then
    result:=false;
end;

function DateTimeToUNIX(const dt: TDateTime):Integer; inline;
begin
  result:=((Trunc(dt)-25569)*Secpd)+Trunc(Secpd*(dt-Trunc(dt)))-200;
end;

function NumSec(d: TDateTime): string; inline;     {Dauer in Anzahl Sekunden wandeln}
begin
  try
//  result:=IntToStr(round(d*Secpd));
    result:=FormatFloat(dzfl, d*Secpd);            {alternativ mit Kommastelle}
  except
    result:='';
  end;
end;

function H501TransformW(const inx: integer; const w: double): double;
   {inx: Index of column
    w: Value to transform}
begin
  result:=w;                                       {default: in=out}
  case inx of
    4:  result:=H501alt(w);
    5:  result:=H501dist(w);
    19: result:=H501velo(w);
  end;
end;

function TForm1.SpeedX(const sp: double): double; inline;
begin                                              {Geschwindigkeit umrechnen}
  result:=sp;                                      {default m/s}
  case rgSpeedUnit.ItemIndex of
    1: result:=sp*fkmh;                            {km/h}
    2: result:=sp*fmph;                            {mph}
  end;
end;

function TForm1.TransformW(const fnr, inx: integer;
                           const w: double): double;
   {fnr: 0..Telemetry, 1..RemoteGPS, 2..Remote
    inx: Index of column
    w: Value to transform}
begin
  result:=w;                                       {default: in=out}
  case fnr of
    0: case inx of                                 {Telemetry}
          7: result:=SpeedX(w);                    {tas}
         14: result:=round(w) and 255;             {motor status}
         15: result:=round(w) and 255;             {imustatus}
         16: result:=round(w) and 255;             {sensor status}
       end;
//  1: case inx of                                 {RemoteGPS, vorerst nix}
    2: case inx of                                 {Remote}
         7: result:=TiltToGrad(w);                 {CH6 Kamera neigen}
       end;
  end;
end;

function TForm1.GetCellInfo(const sp, zl: integer): string; {Hint/Info per Zelle}
var e: integer;
    s: string;
    t: double=0;

  function Lunit: string; inline;                  {Lenght measurement unit}
  begin
    if btnClose.Tag=0 then
      result:='m'
    else
      result:='ft';
  end;

  function HeaderHnt: string; inline;
  begin
    result:=gridDetails.Cells[sp, 0]+'=';
  end;

  function DefaultHnt: string; inline;
  begin
    result:=HeaderHnt+gridDetails.Cells[sp, zl];
  end;

  procedure PayLoad;                               {Payload detailliert anzeigen}
  begin
    if gridDetails.Cells[sp, zl]='' then           {leere Zellen}
      s:=rsN_A
    else begin                                     {gefüllte Zellen}
      try
        e:=Hex2Dec('$'+gridDetails.Cells[sp, zl])
      except
        e:=0;
      end;
      s:='Payload - '+gridDetails.Cells[sp, 0]+': $'+gridDetails.Cells[sp, zl]+
         ' = '+IntToStr(e);
      if (e>31) and
         (e<128) then
        s:=s+' ('+Chr(e)+')';                      {ASCII Zeichen, wenn gültig}
    end;
  end;

  procedure Zeitstempel;                           {Zeitstempel legacy anzeigen}
  begin
    if sp=0 then begin
      case v_type of
        h501ID: s:=FormatDateTime(vzf, ZeitToDT(gridDetails.Cells[0, zl], H501ID)+
                                  GetDateFromFile(lbFlights.Items[lbFlights.ItemIndex]));
      else
        s:=FormatDateTime(vzf+zzz, ZeitToDT(gridDetails.Cells[0, zl], v_type));
      end;
    end;
  end;

  procedure TabHintYTHP;                           {Hint for H Plus}
  begin
    s:='';
    if (sp=0) or (sp=1) then begin                 {Fix=/1}
      s:=rsRecordNo+IntToStr(zl)+kma+DefaultHnt;
    end;

    if sp=lenfix-6 then begin                      {Sequenz number}
      try
        e:=Hex2Dec('$'+gridDetails.Cells[sp, zl]); {dezimal}
        s:=rsRecordNo+IntToStr(zl)+kma+
           HeaderHnt+IntToStr(e);
      except
        s:='';                                     {bei Fehler Standardausgabe}
      end;
    end;

    if sp=lenfix-4 then                            {MAV Component ID}
      try
        e:=Hex2Dec('$'+gridDetails.Cells[sp, zl]);
        s:=MAVcompID(e);
      except
        s:=rsError;
      end;

    if (sp=lenfix-1) or
       (sp=lenfix-2) or
       (sp=lenfix-3) then begin                    {Message ID 3 Byte}
      try
        e:=Hex2Dec('$'+gridDetails.Cells[lenfix-1, zl]+
                       gridDetails.Cells[lenfix-2, zl]+
                       gridDetails.Cells[lenfix-3, zl]);
        s:=IntToStr(e)+suff+gridDetails.Cells[lenfix, zl];
      except
        s:=gridDetails.Cells[lenfix, zl];
      end;
    end;

    if sp>lenfix-1 then
      Payload;                                     {Payload anzeigen}
    if sp=lenfix then
      s:='mavlink_msg_'+gridDetails.Cells[sp, zl]; {Message Name}
    if sp=lenfix+1 then
      s:=rsLenPL+gridDetails.Cells[sp, zl];        {Payloadlänge}
  end;

  procedure TabHintBreeze;
  begin
    case sp of
       2: begin
            e:=StrToIntDef(gridDetails.Cells[sp, zl], 99);
            s:=BrfmodeToStr(e);
          end;
       3, 4, 6, 7, 8:
          begin
            t:=StrToFloatN(gridDetails.Cells[sp, zl])/10;
            s:=HeaderHnt+FormatFloat(dzfl, t);
            s:=s+Lunit;
          end;
       9: begin                                    {maxSpeed}
            t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
            s:=HeaderHnt+FormatFloat(dzfl, t);
            if btnClose.Tag=0 then
              s:=s+'m/s'
            else
              s:=s+'ft/s';
          end;
      10: begin                                    {Altitude}
            t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
            s:=HeaderHnt+FormatFloat(dzfl, t);
            s:=s+Lunit;
          end;
      11: s:='IMU Status='+KorrSigned(gridDetails.Cells[sp, zl], 255);
      12: begin
            t:=BrCoordToFloat(gridDetails.Cells[sp, zl]);
            s:=HeaderHnt+KoToStr(t);
          end;
      13: begin
            t:=BrCoordToFloat(gridDetails.Cells[sp, zl]);
            s:=HeaderHnt+KoToStr(t);
          end;
      14: begin                                    {AutoTakeOff}
            e:=StrToIntDef(gridDetails.Cells[sp, zl], 99);
            s:=AutoTakeOffToStr(e);
          end;
      15: begin                                    {roll}
            t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
            s:=HeaderHnt+FormatFloat(ctfl, t);
          end;
      16: begin                                    {pitch}
            t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
            s:=HeaderHnt+FormatFloat(ctfl, t);
          end;
      17: begin                                    {yaw}
            t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
            s:=HeaderHnt+FormatFloat(ctfl, t);
          end;
      18: begin                                    {MotorStatus}
            e:=StrToIntDef(gridDetails.Cells[sp, zl], 0);
            s:=MotStatusToStr(e);
          end;
      19: s:=eflagToStr(gridDetails.Cells[sp, zl]); {Error Flags}
      20: s:='Number Sats='+KorrSigned(gridDetails.Cells[sp, zl], 63);
      21: s:=rsRest+' ~'+BrKorrV(gridDetails.Cells[sp, zl])+'%';
    end;
  end;

  procedure TabHintH501;                           {Hubsan H501}
  begin
    case sp of
       1: s:='Frames: '+FrameToStr(StrToInt(gridDetails.Cells[1, zl]));
       4: begin                                    {Elevation}
            t:=H501alt(StrToFloatN(gridDetails.Cells[sp, zl]));
            s:=HeaderHnt+FormatFloat(dzfl, t);
            s:=s+Lunit;
          end;
       5: begin                                    {Distance}
            t:=H501dist(StrToFloatN(gridDetails.Cells[sp, zl]));
            s:=HeaderHnt+FormatFloat(dzfl, t);
            s:=s+Lunit;
          end;
       6, 7, 8: s:=DefaultHnt+'°';
       9: s:=ShowVoltage(gridDetails.Cells[sp, zl], v_type);
       19:
          begin
            t:=H501velo(StrToFloatN(gridDetails.Cells[sp, zl]));   {Velocity}
            s:=HeaderHnt+FormatFloat(dzfl, t)+'m/s = ';
            if rgSpeedUnit.ItemIndex=2 then
              s:=s+FormatFloat(dzfl, t*fmph)+'mph'
            else
              s:=s+FormatFloat(dzfl, t*fkmh)+'km/h';
          end;
    end;
  end;

  procedure TabHintSensorYTH;
  begin
    s:='';
    case sp of
      0: s:='Magic '+gridDetails.Cells[sp, zl];
      1: s:=rsLenPL+gridDetails.Cells[sp, zl];
      5: s:='MAV message '+gridDetails.Cells[sp, zl];
      6: s:=rsTimeToBoot+suff+gridDetails.Cells[sp, zl]+'s';
    end;
    if (not cbSensorhAsdata.Checked) and (sp>6) then
      PayLoad;
  end;

  procedure TabHltYTHP;                            {Typhoon H Plus}
  begin
    case sp of
      0: s:=rsTimeToBoot+suff+
            FormatDateTime(zzf+zzz, ZeitToDT(gridDetails.Cells[0, zl], YTHPid)-
                                    ZeitToDT(gridDetails.Cells[0, 1], YTHPid));
      2: s:=gridDetails.Cells[sp, zl]+'V, '+
            VperCell(v_type,                       {V per cell}
                     StrToFloatN(gridDetails.Cells[sp, zl]));
      3: s:=rsrest+tab1+gridDetails.Cells[sp, zl]+'%';
      4: begin                                     {Altitude}
           if rgSpeedUnit.ItemIndex=2 then begin
             try
               t:=StrToFloatN(gridDetails.Cells[sp, zl]);
             except
               t:=0;
             end;
             s:=FormatFloat(ctfl, t/fft)+'ft'
           end else
             s:=gridDetails.Cells[sp, zl]+'m';
         end;
      7, 24, 25:
         begin                                     {tas, hspeed, hspeed}
           try
             t:=StrToFloatN(gridDetails.Cells[sp, zl]);
           except
             t:=0;
           end;
           s:=DefaultHnt+'m/s = ';
           if rgSpeedUnit.ItemIndex=2 then
             s:=s+FormatFloat(dzfl, t*fmph)+'mph'
           else
             s:=s+FormatFloat(dzfl, t*fkmh)+'km/h';
         end;
       9: s:=GPSfixType(gridDetails.Cells[sp, zl]);
      14: begin                                    {MotorStatus}
            e:=StatusToByte(gridDetails.Cells[sp, zl]);
            s:=MotStatusToStr(e);
          end;
      19: s:=fmodeToStr(StrToIntDef(gridDetails.Cells[sp, zl], 99)); {fMode}
      21: s:=vtypeToStr(YTHPid);                   {fix eingestellt}
    end;
  end;

  procedure TabHltLegacy;                          {Q500, YTH und andere Kopter}
  begin
    case sp of                                     {beginnt mit fixen Spalten}
       1: if (gridDetails.Cells[sp, zl]='0')
            then s:='Dual Band Control Redundancy (5.8G WiFi)';
       2: s:=ShowVoltage(gridDetails.Cells[sp, zl], v_type);
       3: if v_type=1 then begin            {H920: Current}
            try
              t:=StrToFloatN(gridDetails.Cells[sp, zl]);
            except
              t:=0;
            end;
            if t>0 then s:=FormatFloat(dzfl, H920Amp(t))+'A';
          end;
       4: begin                                    {Altitude}
            if rgSpeedUnit.ItemIndex=2 then begin
              try
                t:=StrToFloatN(gridDetails.Cells[sp, zl]);
              except
                t:=0;
              end;
              s:=HeaderHnt+FormatFloat(ctfl, t/fft)+'ft'
            end else
              s:=DefaultHnt+'m';
          end;
       7: begin                                    {tas=true air speed}
            try
              t:=StrToFloatN(gridDetails.Cells[sp, zl]);
            except
              t:=0;
            end;
            s:=HeaderHnt+FormatFloat(ctfl, t)+'m/s = ';
            if rgSpeedUnit.ItemIndex=2 then
              s:=s+FormatFloat(dzfl, t*fmph)+'mph'
            else
              s:=s+FormatFloat(dzfl, t*fkmh)+'km/h';
          end;
       9: s:=GPSfixType(gridDetails.Cells[sp, zl]);
      11,12,13: s:=DefaultHnt+'°';                 {Pitch, roll, yaw}
      14: begin                                    {MotorStatus}
            e:=StatusToByte(gridDetails.Cells[sp, zl]);
            s:=MotStatusToStr(e);
          end;
      15: if (v_type>1) then begin                 {imu_status}
            e:=StatusToByte(gridDetails.Cells[sp, zl]);
            s:=IMUstatusToStr(e);
          end;
      else begin    {case else: für variable Spalten abh. vom Typ}
        if (sp=gridDetails.Tag-1) and
           (v_type>1) then begin                   {press_compass_status}
          e:=StatusToByte(gridDetails.Cells[sp, zl]);
          s:=PCGstatusToStr(e, v_type);
        end;
        if (sp=gridDetails.Tag-2) and              {CGPS}
           (v_type=2) then begin                   {nur bei Q500 gefüllt}
          e:=StatusToByte(gridDetails.Cells[sp, zl]);
          s:=CGPSToStr(e);
        end;
        if sp=gridDetails.Tag then begin           {Flight Mode}
          e:=StrToIntDef(gridDetails.Cells[sp, zl], 99);
          s:=fmodeToStr(e);
        end;
        if sp=gridDetails.Tag+2 then begin         {vehicle type}
          s:=vtypeToStr(v_type);
        end;
        if sp=gridDetails.Tag+3 then               {error flags}
          s:=eflagToStr(gridDetails.Cells[sp, zl]);
      end;
    end;
  end;

  procedure TabHLTelemetry;                        {Hints für Telemetrie}
  begin
    Zeitstempel;                                   {Default, can be overwritten}
    case v_type of                                 {Vehicle type}
      brID: TabHintBreeze;                         {Breeze}
      H501ID: TabHintH501;                         {flaretom Hubsan}
      YTHPid: TabHltYTHP;                          {YTH Plus}
    else
      TabHltLegacy;                                {Q500, YTH und andere Kopter}
    end;
  end;

  procedure TabHLFunk;
  begin
    Zeitstempel;
    case sp of
      1:       s:=StickToStr(sp, gridDetails.Cells[sp, zl]);
      2..4, 8: s:=StickToStr(sp, gridDetails.Cells[sp, zl]);
      7:    s:=RadToStr(gridDetails.Cells[sp, zl]);
      5, 6, 9, 10: s:=SwitchToStr(sp, v_type, gridDetails.Cells[sp, zl]);
      11:   s:=LandGearToStr(gridDetails.Cells[sp, zl]);
    end;
  end;

  procedure TabHintRemoteGPS;                      {RemoteGPS aus ST10/16}
  begin
    if v_type=YTHPid then begin
      case sp of
        0: Zeitstempel;
        1, 2: s:=HeaderHnt+ChrKoor(gridDetails.Cells[sp, zl]);   {lat, lon}
        3: begin                                     {Altitude}
          s:=HeaderHnt;
          if rgSpeedUnit.ItemIndex=2 then begin
               try
                 t:=StrToFloatN(gridDetails.Cells[sp, zl]);
               except
                 t:=0;
               end;
               s:=s+FormatFloat(dzfl, t/fft)+'ft'
             end else
               s:=DefaultHnt+'m';
           end;
        5: s:=DefaultHnt+'dm';
        6: begin                                     {Speed in cm/s}
             try
               t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
             except
               t:=0;
             end;
             s:=HeaderHnt+FormatFloat(ctfl, t)+'m/s = ';
             if rgSpeedUnit.ItemIndex=2 then
               s:=s+FormatFloat(dzfl, t*fmph)+'mph'
             else
               s:=s+FormatFloat(dzfl, t*fkmh)+'km/h';
           end;
        7: s:=DefaultHnt+'°';
      end;
    end else begin
      case sp of
        0: Zeitstempel;
        1, 2: s:=HeaderHnt+ChrKoor(gridDetails.Cells[sp, zl]);   {lat, lon}
        3: begin                                     {Altitude}
          s:=HeaderHnt;
          if rgSpeedUnit.ItemIndex=2 then begin
               try
                 t:=StrToFloatN(gridDetails.Cells[sp, zl]);
               except
                 t:=0;
               end;
               s:=s+FormatFloat(dzfl, t/fft)+'ft'
             end else
               s:=DefaultHnt+'m';
           end;
        4: s:=DefaultHnt+'cm';
        5: begin                                     {Speed in cm/s}
             try
               t:=StrToFloatN(gridDetails.Cells[sp, zl])/100;
             except
               t:=0;
             end;
             s:=HeaderHnt+FormatFloat(ctfl, t)+'m/s = ';
             if rgSpeedUnit.ItemIndex=2 then
               s:=s+FormatFloat(dzfl, t*fmph)+'mph'
             else
               s:=s+FormatFloat(dzfl, t*fkmh)+'km/h';
           end;
        6: s:=DefaultHnt+'°';
      end;
    end;
  end;

  procedure TabHintLegacy;                         {CSV-Dateien (Tm/Rm/Rm-GPS)}
  begin
    case rgQuelle.ItemIndex of
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
             t:=StrToFloatN(gridDetails.Cells[sp, zl]);
           except
             t:=0;
           end;
           s:=DefaultHnt+'m/s = ';
           if rgSpeedUnit.ItemIndex=2 then
             s:=s+FormatFloat(dzfl, t*fmph)+'mph'
           else
             s:=s+FormatFloat(dzfl, t*fkmh)+'km/h';
         end;
      8: s:=rsEmpty;                               {currently unused}
      11..13: s:=DefaultHnt+'rad';
      1, 14, 46, 47, 49: s:=DefaultHnt+'%';
      15: begin
            if gridDetails.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+gridDetails.Cells[sp, zl]);
              s:=MSenStat(e);
            end else s:=rsEmpty;
          end;
      17: begin
            if gridDetails.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+gridDetails.Cells[sp, zl]);
              s:=gridDetails.Cells[sp, 0]+': $'+
                 gridDetails.Cells[sp, zl]+'='+IntToStr(e);
            end else s:=rsEmpty;
          end;
      18: begin
            if gridDetails.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+gridDetails.Cells[sp, zl]);
              s:=MSTtoStr(e);
            end else s:=rsEmpty;
          end;
      19: begin
            if gridDetails.Cells[sp, zl]<>'' then begin
              e:=Hex2Dec('$'+gridDetails.Cells[sp, zl]);
              s:=MMFtoStr(e);
            end else s:=rsEmpty;
          end;
      21: s:='Vertical accuracy'+suff+gridDetails.Cells[sp, zl]+'m';
      22: s:='Horizontal accuracy'+suff+gridDetails.Cells[sp, zl]+'m';
      25: s:='Ground speed'+suff+gridDetails.Cells[sp, zl]+'m/s';
      26..28: s:=DefaultHnt+'m/s²';
      29..31: s:=DefaultHnt+'rad/s';
      32..34: s:=DefaultHnt+'gauss';
      35, 36: s:=DefaultHnt+'mbar';
      44: s:='Course over ground'+suff+gridDetails.Cells[sp, zl]+'°';
      45: s:=DefaultHnt+'°C';
      48, 41..43: s:=DefaultHnt+'m/s';
      posChan-1: begin                             {MAV message ID decimal}
                   e:=StrToIntDef(gridDetails.Cells[sp, zl], 10000000);
                   s:=MsgIDtoStr(e);
                 end;

    end;
  end;

begin                                              {grundlegende Verzweigungen}
  result:='';
  if gridDetails.Cells[sp, zl]<>'' then begin
    result:=DefaultHnt;                            {default Hint}
    s:='';                                         {default bei nix gefunden}
    if gridDetails.ColCount=csvanz then begin
      TabHintPX4csv;
    end else begin
      if gridDetails.ColCount>=YTHPcols then begin {Sensor PX4 überschreibt alles}
        TabHintYTHP;
      end else begin
        TabHintLegacy;
      end;
    end;

    if s<>'' then                                  {wenn etwas gefunden wurde}
      result:=s;                                   {default überschreiben}
  end;
end;

function TForm1.vms(const d: TDateTime; const w: double): string; {Weg in m --> V in m/s oder km/h}
begin
  try
    case rgSpeedUnit.ItemIndex of
      1: result:=FormatFloat(ctfl, w/(d*Secpd)*fkmh);    {in km/h}
      2: result:=FormatFloat(ctfl, w/(d*Secpd)*fmph);    {in mph}
      else
        result:=FormatFloat(ctfl, w/(d*Secpd));          {in m/s, default}
    end;
    result:=result+rgSpeedUnit.Items[rgSpeedUnit.ItemIndex];
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
  x:=0;
  p:=pos(GPXpar, s);
  if p>0 then begin                                {Parameter gefunden}
    for x:=p+GPXpar.length to s.length do
      if (s[x]='"') or
         (s[x]='<') or
         (s[x]='>') then
        break;
    result:=copy(s, p, x-p);
    result:=GetFNr(result);
  end;
end;

procedure TForm1.OverwriteVT;                      {Overwrite vehicle type for PX4 Thunderbird}
begin
  if cbThunder.Checked and
     (v_type=defVT) then begin                     {Overwrite for PX4 Thunderbird}
    v_type:=ThBid;                                 {Set to v_type}
    StaticText1.Caption:=capThunder;               {Type hard wired}
    cbxText.Text:=StaticText1.Caption;             {Drone ID}
    Merkliste(cbxText, speItems.Value);
  end;
end;

procedure TForm1.GetDefVT;                         {Fill defVT depending on settings}
begin
  v_type:=defVT;                                   {default legacy Yuneec}
  OverwriteVT;                                     {Overwrite vehicle type for PX4 Thunderbird}
end;

procedure TForm1.DoForm2Show(p: integer);          {Detailfenster anzeigen mit Breite p}
begin                                              {p=0 --> Breite Form1}
  Form2.Show;                                      {oberhalb Hauptfesnter anzeigen}
  if p=0 then
    Form2.Width:=pcMain.Width
  else
    Form2.Width:=p;
  Form2.Left:=Form1.Left+pcMain.Left;              {linksbündig zu Diagrams anzeigen}
  Form2.Top:=Form1.Top-Form2.Height-20;            {20 Pixel von Fensterleiste Kopf zeigen}
  if Form2.Top<Screen.DesktopTop then
    Form2.Top:=Screen.DesktopTop;
  TimerDblClick.Enabled:=true;                     {Timer für Abfrage Doppelclick Form2}
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

procedure TForm1.mnOSMClick(Sender: TObject);      {zeige OSM Karte; lat/lon}
begin
  if gridDetails.ColCount=csvanz then begin        {Self-dev PX4 CSV format}
    OpenURL(URLosm(gridDetails.Cells[5, gridDetails.Row],
                   gridDetails.Cells[6, gridDetails.Row]));
    exit;
  end;
  if lbFlights.Items.Count>0 then begin
    case v_type of
      brID: OpenURL(URLosm(BrCoordFormat(gridDetails.Cells[12, gridDetails.Row]),
                           BrCoordFormat(gridDetails.Cells[13, gridDetails.Row])));
      H501ID:  OpenURL(URLosm(gridDetails.Cells[2, gridDetails.Row],
                              gridDetails.Cells[3, gridDetails.Row]));
      MQcsvID: OpenURL(URLosm(gridDetails.Cells[9, gridDetails.Row],
                              gridDetails.Cells[10, gridDetails.Row]));
    else
      case rgQuelle.ItemIndex of
        0: OpenURL(URLosm(gridDetails.Cells[5, gridDetails.Row],
                          gridDetails.Cells[6, gridDetails.Row]));
        1: OpenURL(URLosm(KoToStr(StrToFloatN(gridDetails.Cells[2, gridDetails.Row])),
                          KoToStr(StrToFloatN(gridDetails.Cells[1, gridDetails.Row]))));
      end;
    end;
  end;
end;

procedure TForm1.mnCleanCSVClick(Sender: TObject); {5GHz Daten aus Telemetrie entfernen}
var i: integer;
    inlist, outlist: TStringList;
    spl: TStringArray;

begin
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  Screen.Cursor:=crHourGlass;
  try
    OpenDialog1.Title:=capCleanCSV;
    OpenDialog1.InitialDir:=cbxLogDir.Text;
    Application.ProcessMessages;
    if OpenDialog1.Execute then begin
      btnShowhex.Tag:=0;                           {No file selected for Block --> Hex}
      inlist.LoadFromFile(OpenDialog1.FileName);   {Load telemetry file}
      StatusBar1.Panels[0].Text:=IntToStr(inlist.Count-1);
      if inlist.Count>minlines then begin
        inlist.SaveToFile(ChangeFileExt(OpenDialog1.FileName, bakext));

        outlist.Clear;
        outlist.Add(inlist[0]);                    {Header}
        for i:=1 to inlist.Count-1 do begin
          spl:=inlist[i].Split(sep);
          if trim(spl[1])<>'0' then                {fsk_rssi<>0 idicated data from 2.4GHz}
            outlist.Add(inlist[i]);
        end;
        if outlist.Count>1 then begin              {Save results}
          outlist.SaveToFile(OpenDialog1.FileName);
          StatusBar1.Panels[1].Text:=IntToStr(outlist.Count-1);
          StatusBar1.Panels[5].Text:=capCleanCSV+suff+OpenDialog1.FileName;
        end;
      end;
    end;
  finally
    inlist.Free;
    outlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

function HTMLEnc(const s: string): string;         {Sonderzeichen escapen}
begin
  result:=StringReplace(s, '&', '&amp;', [rfReplaceAll]);   {zuerst !}
  result:=StringReplace(result, '<', '&lt;',    [rfReplaceAll]);
  result:=StringReplace(result, '>', '&gt;',    [rfReplaceAll]);
  result:=StringReplace(result, '"', '&quot;',  [rfReplaceAll]);
  result:=StringReplace(result, '''', '&apos;', [rfReplaceAll]);
  result:=StringReplace(result, 'Ä', '&Auml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'Ö', '&Ouml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'Ü', '&Uuml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ä', '&auml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ö', '&ouml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ü', '&uuml;',  [rfReplaceAll]);
  result:=StringReplace(result, 'ß', '&szlig;', [rfReplaceAll]);
end;

function write_nme(nm: string): string; inline;    {Set an "name" line tagged}
begin
  result:=tab2+'<'+nmtag+'>'+nm+'</'+nmtag+'>';
end;

procedure placemark(list: TStringList; styl, nam, lkor, ltim: string); inline;
begin
  list.Add('<'+pmtag);                             {Create a placemark}
  list.Add(tab2+'<TimeStamp><'+KMLwhen+ltim+'</'+KMLwhen+'</TimeStamp>');
  list.Add(tab2+'<styleUrl>'+styl+'</styleUrl>');
  if nam<>'' then
    list.Add(write_nme(nam));
  lkor:=StringReplace(lkor, tab1, sep, [rfReplaceAll]);
  list.Add(tab2+'<Point><'+cotag+lkor+'</'+cotag+'</Point>');
  list.Add('</'+pmtag);
end;

procedure GPXheader(n, f: string; dt: TDateTime; klist: TStringList); inline;
                   {Name, Datei,  Beginnzeit,    Ausgabeliste}
begin
  klist.Add(xmlvers);
  klist.Add(gpxvers+' creator="'+
            HTMLEnc(ExtractFileName(Application.ExeName))+'">');
  klist.Add('<metadata>');
  klist.Add(write_nme(HTMLEnc(n)));
  klist.Add('  <desc>'+FormatDateTime(mzf, dt)+'h - '+
              ExtractFileName(f)+'</desc>');
  klist.Add('</metadata>');
end;

procedure GPXfooter1(klist: TStringList); inline;  {Write final lines to GPX file}
begin
  klist.Add('  </trkseg>');
  klist.Add('</trk>');
end;

procedure GPXfooter2(klist: TStringList); inline;  {Write last final line to GPX file}
begin
  klist.Add('</gpx>');
end;

procedure KMLfooter1(s: string; klist: TStringList); inline;
begin
  klist.Add(tab4+'</'+s);
  klist.Add('  </LineString>');
  klist.Add('</'+pmtag);
end;

procedure KMLfooter2(klist: TStringList); inline;  {Write last final line to KML file}
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
  Result:=IntToHex(tbrSaturation.Position, 2)+IntToHex(ColorToRgb(AColor), 6);
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
    H501ID: result:=true;                          {Hubsan all as flight}
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
  if Form1.cbCleanHplus.Checked then               {YTH Plus bereinigen}
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

function SuchFile(path: string; const Mask: string; list: TStringList): integer;
begin
  result:=0;
  FindAllFiles(list, path, mask, false);
  result:=list.Count;
(*  s:=IncludeTrailingPathDelimiter(path);
  result:=FindFirst(s+Mask, faAnyFile, SR);
  try
    while result=0 do begin                        {solange noch was gefunden wurde}
      list.Add(s+SR.Name);
      result:=FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;     *)
end;

function DoDownload: string;                       {Download new version}
begin
  if OpenURL(homepage+DownURL) then
    result:=rsDownloading
  else
    result:=errDownloading;
end;

procedure TForm1.CheckVersion;                     {Call version file and check}
var strm: TStream;
    inlist: TStringList;
    i: integer;
    ct: string;

begin
  inlist:=TStringList.Create;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  ct:='';
  strm:=nil;
  try
    try
      IpHttpDataProvider1.Reference(AppName);
      if IpHttpDataProvider1.CheckURL(homepage+versfile, ct) then
        strm:=IpHttpDataProvider1.DoGetStream(homepage+versfile);
    except
      on e: Exception do begin
        AppLog.Lines.Add(e.Message);
        if pos('time', e.Message)>0 then
          StatusBar1.Panels[5].Text:=errHomepage
        else
          StatusBar1.Panels[5].Text:=e.Message;
        exit;
      end;
    end;

    if (strm<>nil) and (strm.Size>0) then
      inlist.LoadFromStream(strm);

    if inlist.count>0 then begin
      lblUpdate.Font.Color:=clPurple;
      for i:=0 to inlist.count-1 do begin
        if pos(appname, inlist[i])>0 then begin
          ct:=inlist[i].Split([sep])[1];
          ct:=CleanNum(ct);
          if VersValue>=StrToIntDef(ct, 0) then begin      {Current version vs label in internet}
            MessageDlg(rsLatestVersion+sLineBreak+sLineBreak+
                       capForm1+sLineBreak+AppName+tab2+AppVersion,
                       mtInformation,[mbOK],0);
          end else
            StatusBar1.Panels[5].Text:=DoDownload;
          AppLog.Lines.Add(inlist[i]);
          break;
        end;
      end;
    end else
      StatusBar1.Panels[5].Text:=DoDownload;
    AppLog.Lines.Add(StatusBar1.Panels[5].Text);
  finally
    inlist.Free;
    if strm<>nil then
      strm.free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.SelDirAct(const fn: string);      {Alles neu laden}
var x:integer;
    fnum: string;                                  {file name flight num}

begin
  Application.ProcessMessages;
  if (cbxLogDir.Text>'') and DirectoryExists(cbxLogDir.Text) then begin
    TimerDblClick.Enabled:=false;
    if Form2<>nil then
      Form2.Close;              {zusätzliches Diagramm schließen beim Neuladen}
    rgQuelle.Enabled:=false;
    btnConv.Enabled:=false;
    btnArchive.Enabled:=true;
    mnConvert.Enabled:=false;
    StatusBar1.Panels[5].Text:=DefaultStatus;
    rgQuelle.ItemIndex:=0;                         {default Telemetry}
    GetDefVT;
    try
      if cbxLogDir.Items.Count>0 then
        for x:=cbxLogDir.Items.Count-1 downto 0 do {Liste putzen}
          if not DirectoryExists(cbxLogDir.Items[x]) then
            cbxLogDir.Items.Delete(x);
      SelectDirectoryDialog1.InitialDir:=cbxLogDir.Text;

      if CheckNumTurns(cbxLogDir.Text)>0 then begin
        OpenDialog1.InitialDir:=cbxLogDir.Text;
        fnum:=IncludeTrailingPathDelimiter(cbxLogDir.Text);
        rgQuelle.Enabled:=true;
        btnConv.Enabled:=true;
        mnConvert.Enabled:=true;
        StatusBar1.Panels[5].Text:=rsFLDir;
{FlightLog Verzeichnis in Dropdown-Liste eintragen}
        cbxLogDir.Text:=ExcludeTrailingPathDelimiter(cbxLogDir.Text);
        Merkliste(cbxLogDir, speItems.Value);      {DropDownListe füllen}
        if pcMain.ActivePageIndex>4 then           {Settings and after that}
          pcMain.ActivePage:=tabOverview;
        lbFlights.ItemIndex:=-1;                   {Default none selected}
        if fn<>'' then begin                       {Index der Datei, wenn übergeben}
          fnum:=GetNr(ExtractFileName(fn));
          if fnum<>'' then
            lbFlights.ItemIndex:=lbFlights.Items.IndexOf(fnum);
        end;
        if lbFlights.ItemIndex<0 then              {if none selected or none found}
          lbFlights.ItemIndex:=lbFlights.Items.Count-1;           {select last one}
        case v_type of
          MQid: ShowMQ;                            {Sensor_*.txt vom Mantis Q}
          H5id: ShowH520;                          {*.tlog vom H520}
          else Anzeige;                            {alle anderen herkömmlich}
        end;
      end;
    except
      StatusBar1.Panels[5].Text:=rsError;
      AppLog.Lines.Add('''2758'+suff+StatusBar1.Panels[5].Text);
    end;
  end else
    btnArchive.Enabled:=false;                     {Archive Button ausblenden}
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
      while inf.Position<(inf.Size-20)  do begin   {bis zum Ende der Datei}
        repeat
          b:=inf.ReadByte;
        until (b=MagicMAVlinkV2) or (inf.Position>=inf.Size-lenfixP);
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
      AppLog.Lines.Add(fn+' Check if Mantis Q failed, wrong record format');
    end;
  finally
    inf.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure WriteSensorHeaderToGridDetails(grid: TStringGrid);
var
  i: integer;

begin
  grid.BeginUpdate;
  grid.RowCount:=1;
  grid.ColCount:=200;                     {Spalten vorbelegen}
  grid.Cells[0, 0]:='Magic';
  grid.Cells[1, 0]:='Len';
  grid.Cells[2, 0]:='SequNo';
  grid.Cells[3, 0]:='SysID';
  grid.Cells[4, 0]:='TargetID';
  grid.Cells[5, 0]:='MsgID';
  grid.Cells[6, 0]:='Since boot [s]';
  for i:=1 to grid.ColCount-lenfix+1 do   {Payload Byte Nummern}
    grid.Cells[i+6, 0]:='PL'+IntToStr(i);
  grid.EndUpdate;
end;


{Basically modified/unmodified MAVlink V1 format + some undocumented msg:
 https://github.com/mavlink/c_library_v1/tree/master/common
 https://github.com/mavlink/mavlink/blob/master/message_definitions/v1.0/common.xml
 https://github.com/mavlink/mavlink/blob/master/message_definitions/v1.0/ardupilotmega.xml}

procedure TForm1.ShowSensorH(const fn: string; mode: integer);
var msg: TMAVmessage;
    zhl, n3: integer;
    infn: TMemoryStream;
    b: byte;

  procedure WriteToGrid(msg: TMAVmessage);         {Default output, payload in hex}
  var
    i: integer;

  begin
    if gridDetails.RowCount<(zhl+2) then
      gridDetails.RowCount:=gridDetails.RowCount+2000;    {neue Zeilen}
    inc(zhl);                                      {Datensätze zählen}
    if zhl=20 then                                 {Check till line 20}
      gridDetails.AutoSizeColumns;

    gridDetails.Cells[0, zhl]:='BC';
    for i:=1 to 5 do
      gridDetails.Cells[i, zhl]:=IntToStr(msg.msgbytes[i]);      {Header in dec}
//    for i:=6 to msg.msglength+7 do                             {with CRC}
    for i:=6 to msg.msglength+5 do                               {w/o CRC}
      gridDetails.Cells[i+1, zhl]:=IntToHex(msg.msgbytes[i], 2); {Payload in hex}

    gridDetails.Cells[5, zhl]:=BCMsgTypeToStr(msg.msgbytes[5]);
  end;

  procedure TimeSinceBoot_micros(pos: byte);
  var
    timeboot: uint64;

  begin
    timeboot:=MavGetUInt64(msg, pos);              {Time since boot in µs}
    msg.time:=timeboot/secpd/1000000;
    gridDetails.Cells[6, zhl]:=FormatFloat(mlfl, timeboot/1000000); {in s};
  end;

  procedure TimeSinceBoot_ms(pos: byte);
  var
    timeboot: uint32;

  begin
    timeboot:=MavGetUInt32(msg, pos);
    msg.time:=timeboot/secpd/1000;
    gridDetails.Cells[6, zhl]:=FormatFloat(mlfl, timeboot/1000); {Default: in s}
  end;

{Communication drop rate, (UART, I2C, SPI, CAN)
 Communication errors (UART, I2C, SPI, CAN)
 dropped packets on all links (packets that were corrupted on reception on the MAV)}

  procedure SYS_STATUS(data: boolean);             {MsgID 1}
  var
    batt_voltage, load, droprate: single;
    i, commerror: integer;
    errText: string;

  begin
    errText:='';
    load:=MavGetUInt16(msg, 18)/10;
    batt_voltage:=MavGetUInt16(msg, 20)/1000;
    if batt_voltage<lowbatt4S  then
      errText:='Battery low';
    droprate:=MavGetUInt16(msg, 24)/100;
    commerror:=MavGetUInt16(msg, 26);
    if droprate+commerror>0 then
      errText:='Communication problems';

    if not cbReduced.Checked then begin            {MAV_SYS_STATUS_SENSOR to AppLog, sensors long}
      AppLog.Lines.Add(trnrApplog+'onboard_control_sensors_present'+suff+
                       MSenStat(MavGetUint32(msg, 6)));
      AppLog.Lines.Add(trnrApplog+'onboard_control_sensors_enabled'+suff+
                       MSenStat(MavGetUint32(msg, 10)));
      AppLog.Lines.Add(trnrApplog+'onboard_control_sensors_health '+suff+
                       MSenStat(MavGetUint32(msg, 14)));
    end;

    If data then begin
      for i:=0 to 2 do begin
        gridDetails.Cells[7+i*4, zhl]:='OnboardControlSensors';
//        gridDetails.Cells[9+i*4, zhl]:=MSenStat(MavGetUint32(msg, 6+i*4));
        gridDetails.Cells[9+i*4, zhl]:=IntToHex(MavGetUint32(msg, 6+i*4), 6);    {sensors short}
        gridDetails.Cells[10+i*4, zhl]:='';
      end;
      gridDetails.Cells[8, zhl]:='present: ';
      gridDetails.Cells[12, zhl]:='enabled: ';
      gridDetails.Cells[16, zhl]:='health: ';
      gridDetails.Cells[19, zhl]:='Load=';
      gridDetails.Cells[20, zhl]:=FormatFloat(dzfl, load)+'%';
      gridDetails.Cells[21, zhl]:='Vbatt=';
      gridDetails.Cells[22, zhl]:=FormatFloat(ctfl, batt_voltage)+'V';
      if (msg.msgbytes[23]<>$FF) or (msg.msgbytes[22]<>$FF) then begin
        gridDetails.Cells[23, zhl]:='Current=';
        gridDetails.Cells[24, zhl]:=IntToStr(MavGetUint16(msg, 22));
        // Measurement unit unknown, Current not used at Typhoon H
      end;
      gridDetails.Cells[25, zhl]:='Drop rate=';
      gridDetails.Cells[26, zhl]:=FormatFloat(dzfl, droprate)+'%';
      gridDetails.Cells[27, zhl]:='CommunicationErrors=';
      gridDetails.Cells[28, zhl]:=IntToStr(commerror);
      for i:=0 to 3 do begin                       {for error count 1 to 4}
        gridDetails.Cells[29+i*2, zhl]:='ErrorCount'+IntToStr(i+1)+'=';
        gridDetails.Cells[30+i*2, zhl]:=IntToStr(MavGetUint16(msg, 28+1*2));
      end;
      if msg.msgbytes[36]<>$FF then
        gridDetails.Cells[37, zhl]:='Battery='+IntToStr(msg.msgbytes[36])+'%';
    end;

    if errText<>'' then begin
      Applog.Lines.Add(DefaultOuputToAppLog(zhl, msg.time,
                       'SYS_STATUS: '+errText+' -- Load='+
                       FormatFloat(dzfl, load)+'%, Vbatt='+
                       FormatFloat(ctfl, batt_voltage)+'V, Drop rate='+
                       FormatFloat(ctfl, droprate)+'%, Communication errors='+
                       IntToStr(commerror)));
    end;
  end;

  procedure Sys_Time(data: boolean);                  {Time synchronization MsgID 2}
  var
    ts: TDateTime;
    i: integer;

  begin
    TimeSinceBoot_ms(14);
    ts:=UnixToDateTime(MavGetUInt64(msg, 6) div 1000000);  {us --> s}
    if data then begin
      gridDetails.Cells[7, zhl]:='System time UTC:';
      gridDetails.Cells[8, zhl]:=FormatDateTime(dzf, ts);
      gridDetails.Cells[9, zhl]:=FormatDateTime(zzf, ts);
      for i:=10 to 14 do
        gridDetails.Cells[i, zhl]:='';

      gridDetails.Cells[15, zhl]:='Time since boot:';
      gridDetails.Cells[16, zhl]:=FormatDateTime(szzz, msg.time);
      gridDetails.Cells[17, zhl]:='s';
      gridDetails.Cells[18, zhl]:='';
    end;
    if not cbReduced.Checked then begin
      AppLog.Lines.Add(DefaultOuputToAppLog(zhl, msg.time,
                      'UTC:  '+FormatDateTime(vzf, ts)));
    end;
  end;

{eph:Standard deviation of horizontal position error, (metres)
 epv:Standard deviation of vertical position error, (metres)}

  procedure GPS_RAW_INT;                           {MsgID 24}
  var
    timeboot: uint64;
    lat, lon, alt: single;

  begin
    timeboot:=MavGetUInt64(msg, 6);                {Time since boot in µs}
    if timeboot>0 then begin
      msg.time:=timeboot/secpd/1000000;
      gridDetails.Cells[6, zhl]:=FormatFloat(mlfl, timeboot/1000000); {in s};
    end;

    lat:=MavGetint32(msg,14)/10000000;
    lon:=MavGetint32(msg,18)/10000000;
    alt:=MavGetint32(msg, 22)*0.001;
    if (lat<>0) or (lon<>0) then begin
      gridDetails.Cells[15, zhl]:='Lat:';
      gridDetails.Cells[16, zhl]:=FormatFloat(coordfl8, lat);
      gridDetails.Cells[17, zhl]:='';
      gridDetails.Cells[18, zhl]:='';
      gridDetails.Cells[19, zhl]:='Lon:';
      gridDetails.Cells[20, zhl]:=FormatFloat(coordfl8, lon);
      gridDetails.Cells[21, zhl]:='';
      gridDetails.Cells[22, zhl]:='';
    end;
    gridDetails.Cells[23, zhl]:='Alt_MSL=';
    gridDetails.Cells[24, zhl]:=FormatFloat(ctfl, alt);
    gridDetails.Cells[25, zhl]:='m';
    gridDetails.Cells[26, zhl]:='';
    gridDetails.Cells[27, zhl]:='eph=';
    gridDetails.Cells[28, zhl]:=FormatFloat(ctfl, MavGetUint16(msg, 26)/100)+'m';
    gridDetails.Cells[29, zhl]:='epv=';
    gridDetails.Cells[30, zhl]:=FormatFloat(ctfl, MavGetUint16(msg, 28)/100)+'m';
    gridDetails.Cells[31, zhl]:='Velocity=';
    gridDetails.Cells[32, zhl]:=FormatFloat(ctfl, MavGetUint16(msg, 30)/100)+'m/s';
    gridDetails.Cells[33, zhl]:='Cog=';
    gridDetails.Cells[34, zhl]:=FormatFloat(ctfl, MavGetUint16(msg, 32)/100)+'°';
    gridDetails.Cells[35, zhl]:=GPSfixType(IntToStr(msg.msgbytes[34]));
    gridDetails.Cells[36, zhl]:=IntToStr(msg.msgbytes[35])+' sats';
  end;

  procedure GPS_STATUS;                            {MsgID 25 - Data structure for 20 sats}
  var
    i: integer;

  begin
    gridDetails.Cells[7, zhl]:='Sats_visible='+IntToStr(msg.msgbytes[6]);
    for i:=0 to 19 do begin                        {For all 20 sats}
      gridDetails.Cells[i+8, zhl]:=IntToStr(msg.msgbytes[i+7]);
      if msg.msgbytes[i+27]=1 then begin
        gridDetails.Cells[i+28, zhl]:=IntToStr(msg.msgbytes[i+7])+' in use';
      end;
      gridDetails.Cells[i+48, zhl]:=IntToStr(msg.msgbytes[i+47])+'° ele';
      gridDetails.Cells[i+68, zhl]:=IntToStr(msg.msgbytes[i+67])+'° azi';
      gridDetails.Cells[i+88, zhl]:=IntToStr(msg.msgbytes[i+87])+'dB';
    end;
  end;

  procedure RAW_IMU(data: boolean);                {MsgID 27}
  begin
    TimeSinceBoot_micros(6);
    if data then begin
      gridDetails.Cells[15, zhl]:='xAcc=';
      gridDetails.Cells[16, zhl]:=IntToStr(MavGetint16(msg, 14));
      gridDetails.Cells[17, zhl]:='yAcc=';
      gridDetails.Cells[18, zhl]:=IntToStr(MavGetint16(msg, 16));
      gridDetails.Cells[19, zhl]:='zAcc=';
      gridDetails.Cells[20, zhl]:=IntToStr(MavGetint16(msg, 18));

      gridDetails.Cells[21, zhl]:='xGyro=';
      gridDetails.Cells[22, zhl]:=IntToStr(MavGetint16(msg, 20));
      gridDetails.Cells[23, zhl]:='yGyro=';
      gridDetails.Cells[24, zhl]:=IntToStr(MavGetint16(msg, 22));
      gridDetails.Cells[25, zhl]:='zGyro=';
      gridDetails.Cells[26, zhl]:=IntToStr(MavGetint16(msg, 24));

      gridDetails.Cells[27, zhl]:='xMag=';
      gridDetails.Cells[28, zhl]:=IntToStr(MavGetint16(msg, 26));
      gridDetails.Cells[29, zhl]:='yMag=';
      gridDetails.Cells[30, zhl]:=IntToStr(MavGetint16(msg, 28));
      gridDetails.Cells[31, zhl]:='zMag=';
      gridDetails.Cells[32, zhl]:=IntToStr(MavGetint16(msg, 30));
    end;
  end;

  procedure SCALED_PRESSURE;                       {MsgID 29}
  begin
    TimeSinceBoot_ms(6);
    gridDetails.Cells[11, zhl]:='Pressure_abs=';
    gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='hPa';
    gridDetails.Cells[14, zhl]:='';
    gridDetails.Cells[15, zhl]:='Pressure_diff=';
    gridDetails.Cells[16, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 14));
    gridDetails.Cells[17, zhl]:='hPa';
    gridDetails.Cells[18, zhl]:='';
    gridDetails.Cells[19, zhl]:='Baro_temp=';
    gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, MavGetInt16(msg, 18)/100)+'°C';
  end;

  procedure ATTITUDE;                              {MsgID 30}
  begin
    TimeSinceBoot_ms(6);

    gridDetails.Cells[11, zhl]:='Roll=';
    gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='rad';
    gridDetails.Cells[14, zhl]:='';
    gridDetails.Cells[15, zhl]:='Pitch=';
    gridDetails.Cells[16, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
    gridDetails.Cells[17, zhl]:='rad';
    gridDetails.Cells[18, zhl]:='';
    gridDetails.Cells[19, zhl]:='Yaw=';
    gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 18));
    gridDetails.Cells[21, zhl]:='rad';
    gridDetails.Cells[22, zhl]:='';

    gridDetails.Cells[23, zhl]:='Rollspeed=';
    gridDetails.Cells[24, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 22));
    gridDetails.Cells[25, zhl]:='rad/s';
    gridDetails.Cells[26, zhl]:='';
    gridDetails.Cells[27, zhl]:='Pitchspeed=';
    gridDetails.Cells[28, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 26));
    gridDetails.Cells[29, zhl]:='rad/s';
    gridDetails.Cells[30, zhl]:='';
    gridDetails.Cells[31, zhl]:='Yawspeed=';
    gridDetails.Cells[32, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 30));
    gridDetails.Cells[33, zhl]:='rad/s';
    gridDetails.Cells[34, zhl]:='';
  end;

  procedure LOCAL_POSITION_NED;                    {MsgID 32}
  begin
    TimeSinceBoot_ms(6);
    gridDetails.Cells[11, zhl]:='X_Pos=';
    gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='m';
    gridDetails.Cells[14, zhl]:='';
    gridDetails.Cells[15, zhl]:='Y_Pos=';
    gridDetails.Cells[16, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
    gridDetails.Cells[17, zhl]:='m';
    gridDetails.Cells[18, zhl]:='';
    gridDetails.Cells[19, zhl]:='Z_Pos=';
    gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 18));
    gridDetails.Cells[21, zhl]:='m';
    gridDetails.Cells[22, zhl]:='';

    gridDetails.Cells[23, zhl]:='Vx=';
    gridDetails.Cells[24, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 22));
    gridDetails.Cells[25, zhl]:='m/s';
    gridDetails.Cells[26, zhl]:='';
    gridDetails.Cells[27, zhl]:='Vy=';
    gridDetails.Cells[28, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 26));
    gridDetails.Cells[29, zhl]:='m/s';
    gridDetails.Cells[30, zhl]:='';
    gridDetails.Cells[31, zhl]:='Vz=';
    gridDetails.Cells[32, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 30));
    gridDetails.Cells[33, zhl]:='m/s';
    gridDetails.Cells[34, zhl]:='';
  end;

  procedure GLOBAL_POSITION_INT;                   {MsgID 33}
  var
    lat, lon, alt: single;

  begin
    TimeSinceBoot_ms(6);
    lat:=MavGetint32(msg,10)/10000000;
    lon:=MavGetint32(msg,14)/10000000;
    alt:=MavGetint32(msg, 18)*0.001;
    if (lat<>0) or (lon<>0) then begin
      gridDetails.Cells[11, zhl]:='Lat:';
      gridDetails.Cells[12, zhl]:=FormatFloat(coordfl8, lat);
      gridDetails.Cells[13, zhl]:='';
      gridDetails.Cells[14, zhl]:='';
      gridDetails.Cells[15, zhl]:='Lon:';
      gridDetails.Cells[16, zhl]:=FormatFloat(coordfl8, lon);
      gridDetails.Cells[17, zhl]:='';
      gridDetails.Cells[18, zhl]:='';
    end;
    gridDetails.Cells[19, zhl]:='Alt_MSL=';
    gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, alt);
    gridDetails.Cells[21, zhl]:='m';
    gridDetails.Cells[22, zhl]:='';
    alt:=MavGetint32(msg, 22)*0.001;
    gridDetails.Cells[23, zhl]:='Alt_rel=';
    gridDetails.Cells[24, zhl]:=FormatFloat(ctfl, alt);
    gridDetails.Cells[25, zhl]:='m';
    gridDetails.Cells[26, zhl]:='';

    gridDetails.Cells[27, zhl]:='Vx';
    gridDetails.Cells[28, zhl]:=IntToStr(MavGetint16(msg, 26));
    gridDetails.Cells[29, zhl]:='Vy';
    gridDetails.Cells[30, zhl]:=IntToStr(MavGetint16(msg, 28));
    gridDetails.Cells[31, zhl]:='Vz';
    gridDetails.Cells[32, zhl]:=IntToStr(MavGetint16(msg, 30));

    gridDetails.Cells[33, zhl]:='Hdg';
    gridDetails.Cells[34, zhl]:=FormatFloat(dzfl, MavGetUint16(msg, 32)/100);
  end;

  procedure RC_CHANNELS_RAW;                       {MsgID 35}
  var
    i: integer;

  begin
    TimeSinceBoot_ms(6);

    for i:=1 to 8 do begin
      gridDetails.Cells[i*2+9, zhl]:='Chan'+IntToStr(i)+'=';
      gridDetails.Cells[i*2+10, zhl]:=IntToStr(MavGetUInt16(msg, i*2+8));
    end;
    gridDetails.Cells[27, zhl]:='Port'+IntToStr(msg.msgbytes[26]);
    gridDetails.Cells[28, zhl]:='RSSI='+IntToStr(msg.msgbytes[27]);
  end;

  procedure SERVO_OUTPUT_RAW;
  var
    timeboot: uint32;
    i: integer;

  begin
    timeboot:=MavGetUInt32(msg, 6);                {Strange, but here in µs}
    msg.time:=timeboot/secpd/1000000;
    gridDetails.Cells[6, zhl]:=FormatFloat(mlfl, timeboot/1000000); {Default: in s}

    for i:=1 to 8 do begin
      gridDetails.Cells[i*2+9, zhl]:='Servo'+IntToStr(i)+'=';
      gridDetails.Cells[i*2+10, zhl]:=IntToStr(MavGetUInt16(msg, i*2+8));
    end;
    gridDetails.Cells[27, zhl]:='Port'+IntToStr(msg.msgbytes[26]);
  end;

  procedure MISSION_CURRENT;
  begin
    gridDetails.Cells[7, zhl]:='Sequence=';
    gridDetails.Cells[8, zhl]:=IntToStr(MavGetUint16(msg, 6));
  end;

  procedure MISSION_REQUEST_INT;
  begin
    {Target, Component, Sequence?}
  end;

  procedure Sys_type(data: boolean);
  var
    i: integer;
    systxt: strinG;

  begin
    systxt:='';

    for i:=12 to msg.msglength+5 do begin
      if msg.msgbytes[i]>31 then begin
        if data then
          gridDetails.Cells[i+1, zhl]:=chr(msg.msgbytes[i]);
        systxt:=systxt+chr(msg.msgbytes[i]);
      end;
    end;

    if not cbReduced.Checked then begin
      AppLog.Lines.Add(DefaultOuputToAppLog(zhl, msg.time, systxt));
    end;
  end;

  procedure NAV_CONTROLLER_OUTPUT;
  begin
    gridDetails.Cells[7, zhl]:='NavRoll=';
    gridDetails.Cells[8, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 6));
    gridDetails.Cells[9, zhl]:='deg';
    gridDetails.Cells[10, zhl]:='';
    gridDetails.Cells[11, zhl]:='NavPitch=';
    gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='deg';
    gridDetails.Cells[14, zhl]:='';
    gridDetails.Cells[15, zhl]:='NavBearing=';
    gridDetails.Cells[16, zhl]:=FormatFloat(ctfl, MavGetInt16(msg, 14));
    gridDetails.Cells[17, zhl]:='TargetBearing=';
    gridDetails.Cells[18, zhl]:=FormatFloat(ctfl, MavGetInt16(msg, 16));
    gridDetails.Cells[19, zhl]:='WP_dist';
    gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, MavGetInt16(msg, 18))+'m';

    gridDetails.Cells[21, zhl]:='Alt_Error=';
    gridDetails.Cells[22, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 20));
    gridDetails.Cells[23, zhl]:='m';
    gridDetails.Cells[24, zhl]:='';
    gridDetails.Cells[25, zhl]:='AirSpd_Error=';
    gridDetails.Cells[26, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 24));
    gridDetails.Cells[27, zhl]:='m/s';
    gridDetails.Cells[28, zhl]:='';
    gridDetails.Cells[29, zhl]:='Xtrack_Error=';
    gridDetails.Cells[30, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 28));
    gridDetails.Cells[31, zhl]:='m/s';
    gridDetails.Cells[32, zhl]:='';
  end;

  procedure RC_CHANNELS;                           {MsgID 65}
  var
    i: integer;

  begin
    TimeSinceBoot_ms(6);

    for i:=1 to 18 do begin
      gridDetails.Cells[i*2+9, zhl]:='Chan'+IntToStr(i)+'=';
      gridDetails.Cells[i*2+10, zhl]:=IntToStr(MavGetUInt16(msg, i*2+8));
    end;
    gridDetails.Cells[47, zhl]:='Used:'+IntToStr(msg.msgbytes[46]);
    gridDetails.Cells[48, zhl]:='RSSI='+IntToStr(msg.msgbytes[47]);
  end;

  procedure VRF_HUD;
  begin
    gridDetails.Cells[7, zhl]:='Airspeed=';
    gridDetails.Cells[8, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 6));
    gridDetails.Cells[9, zhl]:='m/s';
    gridDetails.Cells[10, zhl]:='';

    gridDetails.Cells[11, zhl]:='Groundspeed=';
    gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='m/s';
    gridDetails.Cells[14, zhl]:='';

    gridDetails.Cells[15, zhl]:='Alt=';
    gridDetails.Cells[16, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
    gridDetails.Cells[17, zhl]:='m';
    gridDetails.Cells[18, zhl]:='';

    gridDetails.Cells[19, zhl]:='Climb_rate=';
    gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 18));
    gridDetails.Cells[21, zhl]:='m/s';
    gridDetails.Cells[22, zhl]:='';

    gridDetails.Cells[23, zhl]:='Heading=';
    gridDetails.Cells[24, zhl]:=IntToStr(MavGetInt16(msg, 22))+'°';
    gridDetails.Cells[25, zhl]:='Throttle=';
    gridDetails.Cells[26, zhl]:=IntToStr(MavGetInt16(msg, 24))+'%';
  end;

  procedure SENSOR_OFFSETS;
  begin
    gridDetails.Cells[7, zhl]:='Mag_ofs_X';
    gridDetails.Cells[8, zhl]:=FormatFloat(dzfl, MavGetInt16(msg, 6));
    gridDetails.Cells[9, zhl]:='Mag_ofs_Y';
    gridDetails.Cells[10, zhl]:=FormatFloat(dzfl, MavGetInt16(msg, 8));
    gridDetails.Cells[11, zhl]:='Mag_ofs_Z';
    gridDetails.Cells[12, zhl]:=FormatFloat(dzfl, MavGetInt16(msg, 10));
    gridDetails.Cells[13, zhl]:='Mag_declination';
    gridDetails.Cells[14, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 12));
    gridDetails.Cells[15, zhl]:='';
    gridDetails.Cells[16, zhl]:='';

    gridDetails.Cells[17, zhl]:='Raw_pressure';
    gridDetails.Cells[18, zhl]:=IntToStr(MavGetInt32(msg, 16));
    gridDetails.Cells[19, zhl]:='';
    gridDetails.Cells[20, zhl]:='';
    gridDetails.Cells[21, zhl]:='Raw_temp';
    gridDetails.Cells[22, zhl]:=IntToStr(MavGetInt32(msg, 20));
    gridDetails.Cells[23, zhl]:='';
    gridDetails.Cells[24, zhl]:='';

    gridDetails.Cells[25, zhl]:='Gyro_cal_x';
    gridDetails.Cells[26, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 24));
    gridDetails.Cells[27, zhl]:='';
    gridDetails.Cells[28, zhl]:='';
    gridDetails.Cells[29, zhl]:='Gyro_cal_y';
    gridDetails.Cells[30, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 28));
    gridDetails.Cells[31, zhl]:='';
    gridDetails.Cells[32, zhl]:='';
    gridDetails.Cells[33, zhl]:='Gyro_cal_z';
    gridDetails.Cells[34, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 32));
    gridDetails.Cells[35, zhl]:='';
    gridDetails.Cells[36, zhl]:='';

    gridDetails.Cells[37, zhl]:='Acc_cal_x';
    gridDetails.Cells[38, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 36));
    gridDetails.Cells[39, zhl]:='';
    gridDetails.Cells[40, zhl]:='';
    gridDetails.Cells[41, zhl]:='Acc_cal_y';
    gridDetails.Cells[42, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 40));
    gridDetails.Cells[43, zhl]:='';
    gridDetails.Cells[44, zhl]:='';
    gridDetails.Cells[45, zhl]:='Acc_cal_z';
    gridDetails.Cells[46, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 44));
    gridDetails.Cells[47, zhl]:='';
    gridDetails.Cells[48, zhl]:='';
  end;

  procedure AHRS;
  begin
    gridDetails.Cells[7, zhl]:='Gyro_drift_x';
    gridDetails.Cells[8, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 6));
    gridDetails.Cells[9, zhl]:='';
    gridDetails.Cells[10, zhl]:='';
    gridDetails.Cells[11, zhl]:='Gyro_drift_y';
    gridDetails.Cells[12, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='';
    gridDetails.Cells[14, zhl]:='';
    gridDetails.Cells[15, zhl]:='Gyro_driftl_z';
    gridDetails.Cells[16, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 14));
    gridDetails.Cells[17, zhl]:='';
    gridDetails.Cells[18, zhl]:='';

    gridDetails.Cells[19, zhl]:='Acc_weight';
    gridDetails.Cells[20, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 18));
    gridDetails.Cells[21, zhl]:='';
    gridDetails.Cells[22, zhl]:='';
    gridDetails.Cells[23, zhl]:='Renorm_val';
    gridDetails.Cells[24, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 22));
    gridDetails.Cells[25, zhl]:='';
    gridDetails.Cells[26, zhl]:='';

    gridDetails.Cells[27, zhl]:='Error_RP';
    gridDetails.Cells[28, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 26));
    gridDetails.Cells[29, zhl]:='';
    gridDetails.Cells[30, zhl]:='';
    gridDetails.Cells[31, zhl]:='Error_Yaw';
    gridDetails.Cells[32, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 30));
    gridDetails.Cells[33, zhl]:='';
    gridDetails.Cells[34, zhl]:='';
  end;

  procedure HW_STATUS;                             {Example how to decode small messages}
  var
    vcc_cpu: single;

  begin
    vcc_cpu:=MavGetUint16(msg, 6)/1000;
    gridDetails.Cells[7, zhl]:='Vcc FC=';
    gridDetails.Cells[8, zhl]:=FormatFloat(dzfl, vcc_cpu)+'V';
    gridDetails.Cells[9, zhl]:='I²C error: '+IntToStr(msg.msgbytes[8]);
  end;

  procedure DATA96;
  begin
    gridDetails.Cells[7, zhl]:='Data_type='+IntToStr(msg.msgbytes[6]);
    gridDetails.Cells[7, zhl]:='Len='+IntToStr(msg.msgbytes[7]);
    // rest of the data undocumented / unknown
  end;

  procedure RANGEFINDER;
  begin
    gridDetails.Cells[7, zhl]:='Distance=';
    gridDetails.Cells[8, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 6));
    gridDetails.Cells[9, zhl]:='m';
    gridDetails.Cells[10, zhl]:='';
    gridDetails.Cells[11, zhl]:='RawVoltage=';
    gridDetails.Cells[12, zhl]:=FormatFloat(mlfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='V';
    gridDetails.Cells[14, zhl]:='';
  end;

  procedure AHRS2;
  var
    lat, lon: single;

  begin
    gridDetails.Cells[7, zhl]:='Roll';
    gridDetails.Cells[8, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 6));
    gridDetails.Cells[9, zhl]:='';
    gridDetails.Cells[10, zhl]:='';
    gridDetails.Cells[11, zhl]:='Pitch';
    gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
    gridDetails.Cells[13, zhl]:='';
    gridDetails.Cells[14, zhl]:='';
    gridDetails.Cells[15, zhl]:='Yaw';
    gridDetails.Cells[16, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
    gridDetails.Cells[17, zhl]:='';
    gridDetails.Cells[18, zhl]:='';

    gridDetails.Cells[19, zhl]:='Alt_MSL';
    gridDetails.Cells[20, zhl]:=FormatFloat(dzfl, MavGetFloatFromBuf(msg, 18));
    gridDetails.Cells[21, zhl]:='m';
    gridDetails.Cells[22, zhl]:='';

    lat:=MavGetInt32(msg,22)/10000000;
    lon:=MavGetInt32(msg,26)/10000000;
    if (lat<>0) or (lon<>0) then begin
      gridDetails.Cells[23, zhl]:='Lat:';
      gridDetails.Cells[24, zhl]:=FormatFloat(coordfl8, lat);
      gridDetails.Cells[25, zhl]:='';
      gridDetails.Cells[26, zhl]:='';
      gridDetails.Cells[27, zhl]:='Lon:';
      gridDetails.Cells[28, zhl]:=FormatFloat(coordfl8, lon);
      gridDetails.Cells[29, zhl]:='';
      gridDetails.Cells[30, zhl]:='';
    end;
  end;

{See also (alternative):
 https://mavlink.github.io/rust-mavlink/mavlink/ardupilotmega/struct.EKF_STATUS_REPORT_DATA.html}
  procedure EKF_STATUS_REPORT(data: boolean);      {To AppLog in case of problems}
  var
    Velocity_variance: single;
    errText: string;

  begin
    errText:='';
    velocity_variance:=MavGetFloatFromBuf(msg, 6);
    if velocity_variance>=0.8 then begin
      errText:=errIsbad;
    end else begin
      if velocity_variance>=0.5 then begin
        errtext:=errWarningLevel;
      end;
    end;
    if errText<>'' then
      AppLog.Lines.Add(DefaultOuputToAppLog(zhl, msg.time,
                       'EKF_STATUS_REPORT: Velocity_variance='+
                       FormatFloat(mlfl, velocity_variance)+errText));

    if data then begin
      gridDetails.Cells[7, zhl]:='Velocity_variance=';
      gridDetails.Cells[8, zhl]:=FormatFloat(ctfl, Velocity_variance);
      gridDetails.Cells[9, zhl]:='';
      gridDetails.Cells[10, zhl]:='';
      gridDetails.Cells[11, zhl]:='Pos_horiz_variance=';
      gridDetails.Cells[12, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 10));
      gridDetails.Cells[13, zhl]:='';
      gridDetails.Cells[14, zhl]:='';
      gridDetails.Cells[15, zhl]:='Pos_vert_variance=';
      gridDetails.Cells[16, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 14));
      gridDetails.Cells[17, zhl]:='';
      gridDetails.Cells[18, zhl]:='';

      gridDetails.Cells[19, zhl]:='Compass_variance=';
      gridDetails.Cells[20, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 18));
      gridDetails.Cells[21, zhl]:='';
      gridDetails.Cells[22, zhl]:='Flags?:';
      {2 bytes EKF Flags}
      gridDetails.Cells[25, zhl]:='Airspeed_variance?=';
      gridDetails.Cells[26, zhl]:=FormatFloat(ctfl, MavGetFloatFromBuf(msg, 24));
      gridDetails.Cells[27, zhl]:='';
      gridDetails.Cells[28, zhl]:='';
    end;
  end;

{AHRS: Attitude and Heading Reference Systems
 EKF : Extended Kalman Filter}
  procedure StatusText(data: boolean);             {Will be written to AppLog}
  var
    Text: string;
    pos: byte;
    txt: char;

  begin
    text:='';
    pos:=7;
    while (msg.msgbytes[pos]>0) and (pos<msg.msglength+6) do begin
      txt:=chr(msg.msgbytes[pos]);
      text:=text+txt;
      if data then
        gridDetails.Cells[pos+1, zhl]:=txt;
      inc(pos);
    end;
    AppLog.Lines.Add(DefaultOuputToAppLog(zhl, msg.time, MAVseverity(msg.msgbytes[6])+': '+text));
  end;

  procedure DecodeSensorMessages;
  begin
    if cbSensorHasData.Checked then begin
      case msg.msgid of
//      0:  Heartbeat;  {??}
        1:  SYS_STATUS(true);
        2:  Sys_Time(true);
        24: GPS_RAW_INT;
        25: GPS_STATUS;
        27: RAW_IMU(true);
        29: SCALED_PRESSURE;
        30: ATTITUDE;
        32: LOCAL_POSITION_NED;
        33: GLOBAL_POSITION_INT;
        35: RC_CHANNELS_RAW;
        36: SERVO_OUTPUT_RAW;
        42: MISSION_CURRENT;
        51: MISSION_REQUEST_INT;
        52: Sys_type(true);
        62: NAV_CONTROLLER_OUTPUT;
        65: RC_CHANNELS;
        74: VRF_HUD;
        150: SENSOR_OFFSETS;
        163: AHRS;
        165: HW_STATUS;
        172: DATA96;
        173: RANGEFINDER;
        178: AHRS2;
        193: EKF_STATUS_REPORT(true);
        253: StatusText(true);
      end;
    end else begin                           {SensorH as raw (hex bytes)}
      case msg.msgid of                      {Some important info to AppLog}
        1:   SYS_STATUS(false);
        2:   Sys_Time(false);
        27:  RAW_IMU(false);
        29, 30, 32, 33, 35, 65: TimeSinceBoot_ms(6);
//      52:  Sys_type(false);                {Not really important}
        193: EKF_STATUS_REPORT(false);
        253: StatusText(false);
      end;
    end;
  end;

begin
  n3:=Label3.Tag;                                  {Suchspalte umkopieren, wird verändert}
  zhl:=0;
  msg.time:=0;
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  if FileSize(fn)>lenfix then begin
    Screen.Cursor:=crHourGlass;
    infn:=TMemoryStream.Create;
    msg:=ClearMAVmessage;                          {Datenbuffer löschen}

    cbxSearch.Text:=UpCase(trim(cbxSearch.Text));
    WriteSensorHeaderToGridDetails(gridDetails);
    try
      infn.LoadFromFile(fn);
      AppLog.Lines.Add(LineEnding);
      AppLog.Lines.Add(fn);
      StatusBar1.Panels[5].Text:=rsWait;

      gridDetails.BeginUpdate;
        while infn.Position<(infn.Size-lenfix) do begin
          repeat                                     {RecordID suchen $BC}
            b:=infn.ReadByte;
          until (b=MagicBC) or (infn.Position>=infn.Size-lenfix);
          msg.msglength:=infn.ReadByte;              {Länge Payload mit CRC}
          infn.Position:=infn.Position-2;
          infn.ReadBuffer(msg.msgbytes, msg.msglength+lenfix);  {Länge Rest-Datensatz mit FixPart und CRC}
          msg.sysid:=msg.msgbytes[3];
          msg.targetid:=msg.msgbytes[4];
          msg.msgid:=msg.msgbytes[5];
          WriteToGrid(msg);
          DecodeSensorMessages;
        end;
        gridDetails.RowCount:=zhl+1;
      gridDetails.EndUpdate;

      StatusBar1.Panels[0].Text:=IntToStr(FileSize(fn));
      StatusBar1.Panels[1].Text:=IntToStr(zhl);
      StatusBar1.Panels[5].Text:=IntToStr(FileSize(fn));
      speDataPoint.MaxValue:=zhl;
      if mode=1 then begin                         {Filterinfo in Statusbar}
        StatusBar1.Panels[2].Text:=rsSelection;
        StatusBar1.Panels[5].Text:=gridDetails.Cells[n3, 0]+' = "'+
                                   cbxSearch.Text+'"';
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsMAVlink+tab1+rsDS);
    finally
      infn.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.OpenSensorPlus;                   {Sensordatei vom YTH Plus öffnen}
var spdir: string;

begin
  spdir:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+npath;
  OpenDialog1.Title:=capSensorPlus;
  OpenDialog1.DefaultExt:=sextP;
  if DirectoryExists(spdir) then
    OpenDialog1.InitialDir:=spdir;                 {zu Sensor wechseln}
  if OpenDialog1.Execute then begin
    btnShowhex.Tag:=0;                             {Default: not used for Block --> Hex}
    if Form2<>nil then
      Form2.Close;              {zusätzliches Diagramm schließen beim Neuladen}
    if ExtractFileExt(OpenDialog1.FileName)=fext then        {if a CSV file}
      AnzeigePX4CSV(OpenDialog1.FileName)          {PX4 Sensor csv anzeigen}
    else begin                                     {Sensor Datei auswerten}
      ShowSensorPlus(OpenDialog1.FileName, 0, cbSensorKML.Checked, true, false, false);
    end;
  end;
end;

procedure TForm1.actMAVmessageIDlistExecute(Sender: TObject);  {List MAV messages}
var mlist: TStringList;

begin
  mlist:=TStringList.Create;
  try
    OpenDialog1.Title:=capSensorPlus;
    if OpenDialog1.Execute then begin
      btnShowhex.Tag:=0;                           {No file selected for Block --> Hex}
      AusgabeMessages(OpenDialog1.Filename, mlist);
      if mlist.Count>1 then begin
        SaveDialog1.Title:=rsFileSave;
        SaveDialog1.FileName:=ChangeFileExt(ExtractFileName(OpenDialog1.Filename), '')+
                              '_MAVmsgList.csv';
        if SaveDialog1.Execute then begin
          mlist.SaveToFile(SaveDialog1.FileName);
        end;
        pcMain.ActivePage:=tabAppLog;              {Switch to AppLogHighlighter}
        AppLog.TopLine:=AppLog.Lines.Count;
      end;
    end;
  finally
    mlist.Free;
  end;
end;

procedure TForm1.SplitSensorPlus;                  {Split PX4 Sensor file}
var infn, outfn: TMemoryStream;
    b, len: byte;
    zhl, e, fnno: integer;
    tme, tme1: int64;
    dsbuf: array[0..YTHPcols] of byte;

  function GetIntFromBuf(const p, a: integer): uint64; {Position, number bytes}
  var i: integer;

  begin
    result:=0;
    for i:=0 to a-1 do begin
      result:=result+dsbuf[lenfix+i+p]*(256**i);
    end;
  end;

  procedure WriteNewFile(const dnr: integer);
  var fn: string;

  begin
    fn:=ChangeFileExt(OpenDialog1.FileName, '')+'_'+IntToStr(dnr)+hext;
    outfn.SaveToFile(fn);
    outfn.Clear;
    AppLog.Lines.Add(ExtractFileName(fn)+tab1+resNew);
    AppLog.Lines.Add(LineEnding);
    StatusBar1.Panels[0].Text:=IntToStr(zhl);
    StatusBar1.Panels[1].Text:=IntToStr(dnr);
  end;

begin
  OpenDialog1.Title:=capSensorPlus;
  OpenDialog1.DefaultExt:=hext;
  zhl:=0;
  fnno:=0;
  tme1:=0;                                         {Time stamp in ms}
  tme:=0;
  if OpenDialog1.Execute then begin
    btnShowhex.Tag:=0;                             {Default: not used for Block --> Hex}
    if Form2<>nil then
      Form2.Close;                                 {Close additional Chart}
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    infn:=TMemoryStream.Create;
    outfn:=TMemoryStream.Create;
    try
      pcMain.ActivePage:=tabAppLog;                {Show AppLog}
      AppLog.Lines.Add(LineEnding);
      infn.LoadFromFile(OpenDialog1.FileName);
      AppLog.Lines.Add(OpenDialog1.FileName);

      while infn.Position<(infn.Size-lenfixP) do begin {bis zum Ende der Datei}
        len:=0;                                    {Reset for error detection}
        try
          repeat
            b:=infn.ReadByte;
          until (b=MagicMAVlinkV2) or (infn.Position>infn.Size-lenfixP);
          inc(zhl);
          len:=infn.ReadByte;                      {Length payload including CRC}
          infn.ReadBuffer(dsbuf, len+lenfixP-2);   {Whole message, w/o $BC and len (-2)}

          e:=GetIntFromBuf(-3, 3);                 {MsgID 3 byte as integer}
          case e of                                {Messages with time stamps}
            4, 24, 27, 105, 141:
               tme:=GetIntFromBuf(0, 8) div 1000;  {in mysec}
            30..33, 65, 83, 87, 259, 262, 265:
               tme:=GetIntFromBuf(0, 4);           {in ms}
          end;

          if tme<(tme1-30000) then begin           {delta > 30sec back}
            inc(fnno);
//            AppLog.Lines.Add(inttostr(tme1)+' --> '+
//                             inttostr(tme)+' in MAVmsg '+inttostr(e));
            WriteNewFile(fnno);
          end;

          outfn.WriteByte(b);                      {Write MAVlink message}
          outfn.WriteByte(len);
          outfn.WriteBuffer(dsbuf, len+lenfixP-2);
          tme1:=tme;

        except
          {Possibly new file?}
        end;
      end;

      if fnno>0 then begin
        inc(fnno);
        WriteNewFile(fnno);
        StatusBar1.Panels[5].Text:=IntToStr(fnno)+tab1+rsSplits;
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      end else begin
        StatusBar1.Panels[5].Text:=ExtractFileName(OpenDialog1.FileName)+
                                   tab1+rsConsistent;
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        StatusBar1.Panels[1].Text:='';
      end;
      StatusBar1.Panels[0].Text:=IntToStr(zhl);
      AppLog.Lines.Add(LineEnding);
    finally
      infn.Free;
      outfn.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.mnSensorPX4Click(Sender: TObject); {Sensordatei vom YTH Plus}
begin
  OpenSensorPlus;
end;

{If TLOG files contains more than one flight the Boot time may reset.
 Good to split those files in more than one for further analysis.}
procedure TForm1.mnSplitClick(Sender: TObject);    {Menu: Split PX4 Sensor file}
begin
  SplitSensorPlus;
end;

procedure TForm1.pcSettings3Change(Sender: TObject);
begin
  EnableMultiSelect;
end;

procedure TForm1.btnSplitClick(Sender: TObject);   {Button: Split PX4 Sensor file}
begin
  SplitSensorPlus;
end;

procedure TForm1.CombineLogs;                      {Combine some legacy Yuneec Flightlog to one log file set}
var
  z, zhl, numlines: integer;
  csvlist, inlist: TStringList;
  dname: string;
  fis: boolean;                                    {Says if there was a file in the section}

  procedure ForOneSection(fn: string);             {Same for one selected Telemetry, Remote or RemoteGPS}
  var
    i, k: integer;
    fnx: string;
  begin
    for i:=0 to lbFlights.Items.Count-1 do begin
      if lbFlights.Selected[i] then begin          {Load all selected files}
        fnx:=fn+lbFlights.Items[i]+fext;           {Complete filename with flight number}
        if FileExists(fnx) then begin
          inlist.LoadFromFile(fnx);
          if fis then begin                        {A valid file already found before}
            for k:=1 to inlist.Count-1 do          {Add without header}
              csvlist.Add(inlist[k]);
          end else begin
            csvlist.Assign(inlist);                {First file with header}
          end;
          fis:=true;
          inc(zhl);                                {Count number of used files}
        end;
      end;
    end;
    if fis then begin
      fnx:=RandomFN(fnx, 0, z);
      csvlist.SaveToFile(fnx);
      numlines:=numlines+csvlist.Count;            {Add number of lines loaded}
      StatusBar1.Panels[5].Text:=fnx;
      StatusBar1.Panels[0].Text:=IntToStr(zhl);
      StatusBar1.Panels[1].Text:=IntToStr(numlines);
    end;
    fis:=false;
    csvlist.Clear;
  end;

begin
  z:=random(8)+1;
  fis:=false;
  zhl:=0;
  numlines:=0;
  if lbFlights.SelCount>1 then begin               {Only if more than one flight was selected}
    csvlist:=TStringList.Create;
    inlist:=TStringList.Create;
    Screen.Cursor:=crHourGlass;
    try
      dname:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+kfile;           {Telemetry}
      ForOneSection(dname);
      dname:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+spath+PathDelim+sfile; {RemoteGPS}
      ForOneSection(dname);
      dname:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+fpath+PathDelim+ffile; {Remote}
      ForOneSection(dname);
    finally
      Screen.Cursor:=crDefault;
      csvlist.Free;
      inlist.Free;
    end;
    if zhl>3 then
      SelDirAct('');
  end else
    StatusBar1.Panels[5].Text:=errSelect;
end;

procedure TForm1.AppLogMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    AppLog.Font.Size:=AppLog.Font.Size-1;
end;

procedure TForm1.AppLogMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    AppLog.Font.Size:=AppLog.Font.Size+1;
end;

procedure TForm1.btnAutoCutClick(Sender: TObject);     {Automatisches Ausschneiden}
begin
  CutLegacy(1);
end;

procedure TForm1.btnCombineLogsClick(Sender: TObject); {Combine some legacy Yuneec Flightlog to one log files set}
begin
  CombineLogs;
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
- Copy flightlogs to your PC in a separate directory.
- Open a directory where alle the flightlog files are located.
  Files should look like this "Sensor_2018_MM_dd_hh_mm_ss.txt"
  (Sensor_+Date_Time.txt). It's not really a text file but a binary.
  Do not open with a text editor.
- Go to 'Settings' > 'Data conversion' and set 'Flight path from PX4 sensor'
  to true.
- Double click on a file number in the list on the left side.
  Then this will be analysed.
- Results of analysis (some status information, text messages and coordinates
  as GoogleMaps link) will be listed in the AppLogHighlighter page.
- If 'Flight path from PX4 sensor' is set to true, a kml or gpx file
  (depending on settings) will be created in the same directory as the sensor
  files with the same name but with different extension (*.kml or *.gpx).
- If needed, an additional CSV file can be created with those and some more
  data from the PX4 sensor files/flightlos Mantis Q or tlog files H520.

- Check AppLogHighlighter for error messages like motor errors or something like this.
  You can open Google Maps with the last coordinate that the Mantis has sent
  before it disappeared.

 https://github.com/mavlink/c_library_v2/tree/master/common
 }

function TForm1.ShowSensorPlus(fn: string;         {Sensordatei vom YTH Plus}
                               fnz: integer;       {file number}
                               gx, tb,             {gx: Track for kml, tb: fill table gridDetails}
                               ov10,               {Overview PX4 Emergency}
                               ov11: boolean): boolean; {PX4 Overview Text}
const
    homeID='home: ';                               {ID Homeposition bei Textmessages}

var dsbuf: array[0..YTHPcols] of byte;
    i, len, zhl, ele0, elemax, battremain: integer;
    msl, hbt, mmf: byte;
    infn: TMemoryStream;
    b: byte;
    maplist, outlist, datlist: TStringList;
    s, homestr: string;                            {GPX Ausgabe, homepoint}
    tstr, skoor: String;
    ftm, bg, bgl, bglg: TDateTime;
    ismq, isGPS, dzt: boolean;
    lat1, lon1: double;                            {erster gültiger Datenpunkt}
    lat2, lon2: double;                            {aktuelle Koordinaten}
    lat3, lon3: double;                            {vorherige Koordinaten}
    latw1, lonw1: integer;                         {Waypoints from position_target_global_int}
    distg, distmax, ucap: double;
    csvarr: array[1..csvanz] of string;            {Werte für CSV-Ausgabe}
    itemp: string;


  procedure StandardAusgabe; inline;               {Stringgrid mit Hexwerten füllen}
  var i: integer;
  begin
    if tb then begin
      for i:=lenfix+1 to len+lenfixP-2 do          {Fixpart Teil 2 + Payload}
      gridDetails.Cells[i+1, zhl]:=IntToHex(dsbuf[i-1], 2); {Payload in Hex}
    end;
  end;

  procedure SenCSVausgabe;                         {Ausgabe Werte aus Sensor}
  var i: integer;
      c: string;
  begin
    c:=FormatDateTime(zzf+zzz, bg);                {aktueller Zeitstempel}
    for i:=1 to csvanz do                          {csv Daten ausgeben}
      c:=c+sep+csvarr[i];
    datlist.Add(c);
    csvarr[57]:='';
    csvarr[58]:='';
  end;

  function GetIntFromBuf(const p, a: integer): uint64; {Position/Anzahl Bytes}
  var i: integer;
  begin
    result:=0;
    for i:=0 to a-1 do begin
      result:=result+dsbuf[lenfix+i+p]*(256**i);
    end;
  end;

  function GetHexFromBuf(const p, a: integer): string; {Position/Anzahl Bytes}
  var i: integer;
  begin
    result:='';
    for i:=0 to a-1 do begin
      result:=result+'$'+IntToHex(dsbuf[lenfix+i+p], 2)+' ';
    end;
  end;

{https://www.delphipraxis.net/122021-single-byte-array-konvertieren-und-umgekehrt.html
 http://forum.lazarus-ide.org/index.php?topic=42182.0
 https://www.lazarusforum.de/viewtopic.php?f=55&t=14659
 Direkter Typecast mit dem Zieldatentyp oder die Deklaration mittels absolute}
  function GetFloatFromBuf(const p: integer): single; {Position, Länge immer 4}
  var i: integer;
      wfl: packed array[0..3] of Byte;
      wx: Single absolute wfl;

  begin
    result:=0;
    for i:=0 to 3 do                               {Endianess prüfen (to/downto)}
      wfl[i]:=dsbuf[lenfix+i+p];                   {4 byte aus Buffer ausschneiden}
    result:=wx;                                    {Typecast mittels absolute}
  end;

  procedure TextOverview;                          {PX4 Text messages overview}
  var i: integer;
      st, tm: string;

  begin
    tm:='';
    st:=FormatDateTime(zzf, bg)+tab2+
        MAVseverity(dsbuf[lenfix])+suff+'''';
    for i:=lenfix+2 to len+lenfixP-12 do begin     {Fixpart Teil 2 + Payload}
      if dsbuf[i-1]>9 then
        tm:=tm+Char(dsbuf[i-1]);                   {Textmessage zusammenstellen}
    end;
    AppLog.Lines.Add(st+tm);                       {Textmessage speichern}
    result:=true;                                  {Something found in PX4 overview (here all text messages)}
    dzt:=true;                                     {Systemzeit nochmal anzeigen}
  end;

  procedure TextAusgabe;                           {Ausgabe als Text in Zeile zl}
  var i: integer;
      st, tm, ch: string;
      spl: TStringArray;

  begin
    ch:=csvarr[posChan];                           {ursprünglichen Wert zwischenspeichern}
    st:=DefaultOuputToAppLog(zhl, bg, MAVseverity(dsbuf[lenfix])+suff+'''');
    tm:='';
    if tb then
       gridDetails.Cells[lenfix+2, zhl]:=IntToStr(dsbuf[lenfix]); {Severity dezimal}
    for i:=lenfix+2 to len+lenfixP-12 do begin     {Fixpart Teil 2 + Payload}
      if dsbuf[i-1]>9 then begin
        if tb then
          gridDetails.Cells[i+1, zhl]:=Char(dsbuf[i-1]); {Rest Payload als Text}
        tm:=tm+Char(dsbuf[i-1]);                   {Textmessage zusammenstellen}
      end else
        if tb then
          gridDetails.Cells[i+1, zhl]:=IntToHex(dsbuf[i-1], 2);
                                                   {nicht druckbare Zeichen als Hex};
    end;
    if gx then begin
      maplist.Add(itagin+tm+itagout);
      outlist.Add(itagin+tm+itagout);
    end;
    csvarr[posChan]:='"'+tm+'"';
    st:=st+tm;
    for i:=len+lenfixP-11 to len+lenfixP-2 do      {8 Byte Sig + 2 CRC}
      if tb then
        gridDetails.Cells[i+1, zhl]:=IntToHex(dsbuf[i-1], 2);
//    wx:=GetIntFromBuf(len+lenfixP-12-lenfix, 8); {Aufsteigende Nummer}

    if pos(emcyID, st)>0 then begin                {EMERGECY found in text message}
      topp[fnz, 6]:=topp[fnz, 6] or 256;
      result:=true;                                {Something found in PX4 overview}
      if ov10 then
        AppLog.Lines.Add(st+'''');                 {Textmessage in AppLog speichern}
    end;

    if not ov10 then begin                         {no output in case of PX4 emergency search}
      AppLog.Lines.Add(st+'''');                   {Textmessage speichern}
      if pos(homeID, tm)>1 then begin              {Homepoint als Link}
        distg:=0;                                  {Entfernungswerte zurücksetzen}
        distmax:=0;
        tm:=StringReplace(tm, ',', '', [rfReplaceAll]);
        spl:=tm.Split(' ');
        homestr:=Format('%-10s', [homeID])+
                 URLGmap(spl[1], spl[2]);
      end;
      SenCSVAusgabe;
    end;
    csvarr[posChan]:=ch;
  end;

  procedure AusgTrack;                             {KML oder GPX aus GPS data}
  var sinfo: string;

  begin
    if gx and                                      {Ausgabe GPX oder KML}
       (msl<>1) then begin                         {not Landed State}
      if (bg+tsdelta2)<bglg then begin             {Delta > 2s backwards}
        sinfo:=itagin+'time reset'+itagout;        {info about new flight}
        maplist.Add(sinfo);
        outlist.Add(sinfo);
      end;

      bglg:=bg;                                    {Last timestamp for GPS}
      if rgOutFormat.ItemIndex=2 then begin        {GPX}
        skoor:=GPXlat+FloatToStr(lat2)+
               GPXlon+FloatToStr(lon2)+'"> ';
        if s='' then begin                         {Startpunkt erkannt}
          maplist.Add('<wpt '+skoor);              {Startpunkt}
          maplist.Add(tab2+'<time>'+tstr+'</time>');
          maplist.Add(write_nme('Start'));
          maplist.Add(GPXet1);
          maplist.Add('<trk>');
          maplist.Add(write_nme(ExtractFileName(fn)));
          maplist.Add(tab2+'<trkseg>');
        end;
        s:=tab4+'<trkpt '+skoor+GPXele+csvarr[4]+
                '</ele> <time>'+tstr+'</time></trkpt>';
        maplist.Add(s);

      end else begin                               {KML/KMZ}
        skoor:=FloatToStr(lon2)+tab1+FloatToStr(lat2);
        if s='' then begin                         {Startpunkt erkannt}
          placemark(maplist, '#starting', '',
                    StringReplace(skoor, tab1, sep, [rfReplaceAll]), tstr);
          maplist.Add('<'+pmtag);
          maplist.Add(write_nme(cbxText.Text));
          maplist.Add(tab2+'<description>'+ExtractFileName(fn)+'</description>');
          maplist.Add(tab2+'<styleUrl>#Flightpath</styleUrl>');
          maplist.Add(tab2+'<gx:Track>');
          maplist.Add(tab2+'<'+amtag+rgAltitudeType.Items[rgAltitudeType.ItemIndex]+
                           '</'+amtag);
          if cbExtrude.Checked then maplist.Add(tab2+extru);
        end;
        s:=tab4+'<gx:coord>'+skoor+tab1+csvarr[4]+'</gx:coord>';
        outlist.Add(s);
        maplist.Add(tab4+'<'+KMLwhen+tstr+'</'+KMLwhen);
      end;
    end;

  end;

{https://github.com/mavlink/c_library_v2/blob/master/common/mavlink_msg_gps_raw_int.h
 https://mavlink.io/en/messages/common.html#GPS_RAW_INT}

  procedure GPSAusgabe;                            {GPS_RAW_INT auswerten}
  var tme: uint64;                                 {unsigned Integer}
      lat, lon, ele: integer;                      {int32}
      eph, epv, vel, cog, hacc: uint32;
      distp, dists, alta: double;                  {Entfernung zum "Start"}

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    distp:=0;
    dists:=0;
    lat:=GetIntFromBuf(8, 4);                      {uint32}
    lon:=GetIntFromBuf(12, 4);
    tme:=GetIntFromBuf(0, 8);                      {in mysec}
    bg:=tme/Secpd/1000000;                         {Zeitstempel überall verfügbar}
    tstr:=FormatDateTime(dzf, ftm)+'T'+
          FormatDateTime(zzf, bg)+'Z';             {Zeitstring überall verfügbar}
    ele:=GetIntFromBuf(16, 4);                     {Höhe}
    if ((lat<>0) or (lon<>0)) and                  {nur bei gültigen Koordinaten}
       (ele<maxxh*1000) then begin                 {nur sinnvolle Höhe}
      lat2:=lat/10000000;                          {nur einmal rechnen}
      lon2:=lon/10000000;                          {und überall verfügbar}
      csvarr[51]:=FormatFloat(ctfl, ele/1000);     {Altitude MSL}
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
      distp:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);   {Entfernung zum 1. Punkt}
      dists:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2);   {Entfernung zum vorherigen Punkt}
      if distp>distmax then                        {maximale Entfernung ermitteln}
        distmax:=distp;
      distg:=distg+dists;                          {Länge Route ermitteln}
      lat3:=lat2;                                  {Koordinaten merken}
      lon3:=lon2;
      alta:=(ele-ele0)/1000;
      csvarr[4]:=FormatFloat(ctfl, alta);          {altitude relative}

      if msl<>1 then
        AusgTrack;

      if tb then begin                             {Höhendiagramm füllen}
        case msl of
          2: Chart1BarSeries1.AddXY(bg, alta);     {fuchsia like Angle}
          3: Chart1BarSeries6.AddXY(bg, alta);     {blue}
          4: Chart1BarSeries3.AddXY(bg, alta);     {red}
        else
          Chart1BarSeries2.AddXY(bg, alta);        {relative Höhe zum ersten Wert}
        end;
        Chart1LineSeries2.AddXY(bg, alta/1000);    {Hüllkurve}
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
        AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg,
                         csvMAVland+' ($F5)'+suff+csvarr[20]));
      msl:=dsbuf[lenfix+1];                        {letzten Wert merken}
    end;
    SenCSVAusgabe;
  end;

{https://github.com/mavlink/c_library_v2/blob/master/common/mavlink_msg_altitude.h

 8 altitude_monotonic [m] This altitude measure is initialized on system boot
                          and monotonic (it is never reset, but represents the
                          local altitude change).
                          The only guarantee on this field is that it will never
                          be reset and is consistent within a flight.
                          The recommended value for this field is the
                          uncorrected barometric altitude at boot time.
                          This altitude will also drift and vary between flights.
12 altitude_amsl     +[m] This altitude measure is strictly above mean sea level
                          and might be non-monotonic (it might reset on events
                          like GPS lock or when a new QNH value is set).
                          It should be the altitude to which global altitude
                          waypoints are compared to.
                          Note that it is *not* the GPS altitude, however,
                          most GPS modules already output MSL by default and
                          not the WGS84 altitude.
16 altitude_local     [m] This is the local altitude in the local coordinate frame.
                          It is not the altitude above home, but in reference to
                          the coordinate origin (0, 0, 0). It is up-positive.
20 altitude_relative +[m] This is the altitude above the home position.
                          It resets on each change of the current home position.
24 altitude_terrain   [m] This is the altitude above terrain. It might be fed
                          by a terrain database or an altimeter.
                          Values smaller than -1000 should be interpreted as unknown.
28 bottom_clearance   [m] This is not the altitude, but the clear space below
                          the system according to the fused clearance estimate.
                          It generally should max out at the maximum range of
                          e.g. the laser altimeter. It is generally a moving target.
                          A negative value indicates no measurement available.}

  procedure Altitude;                              {MAVLINK_MSG_ID_ALTITUDE 141 ($8D)}
  var tme: uint64;                                 {unsigned Integer}
      fval: double;

  begin
    StandardAusgabe;
    tme:=GetIntFromBuf(0, 8);                      {in mysec}
    bg:=tme/Secpd/1000000;                         {Zeitstempel überall verfügbar}
    tstr:=FormatDateTime(dzf, ftm)+'T'+
          FormatDateTime(zzf, bg)+'Z';             {Zeitstring überall verfügbar}

    fval:=GetFloatFromBuf(12);                     {altitude MSL in m}
    csvarr[51]:=FormatFloat(ctfl, fval);

    fval:=GetFloatFromBuf(20);                     {altitude relative in m}
    csvarr[4]:=FormatFloat(ctfl, fval);

    SenCSVAusgabe;
  end;

{Battery status for 10 cells}
  procedure Batt_Status;                           {BATTERY_STATUS 147 ($93)}
  var i, ival, volts: integer;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    ival:=GetIntFromBuf(30, 2);                    {Current in cA}
    csvarr[3]:=FormatFloat(dzfl, ival/100);        {Current [A]}
    volts:=0;
    for i:=0 to 9 do begin                         {Voltage for 10 cells}
      ival:=GetIntFromBuf(i*2+10, 2);
      if ival<60000 then
        volts:=volts+ival;                         {Zellen aufsummieren}
    end;
    csvarr[2]:=FormatFloat(ctfl, volts/1000);      {Battery voltage}
    ival:=GetIntFromBuf(35, 1);
    if ival>=0 then
      csvarr[46]:=IntToStr(ival);                  {remaining capacity [%]}
    if not cbReduced.Checked then
      AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, csvVolt+suff+csvarr[2]+'V'+
                       tab4+csvAmp+suff+csvarr[3]+'A'));
    SenCSVAusgabe;
  end;

  procedure Heartbeat;                             {MAV_State aus Heartbeat}
  var cm: uint64;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      if ov10 then begin                           {PX4 Emergency Overview}
        if (dsbuf[lenfix+7]=5) or (dsbuf[lenfix+7]=6) then begin   {Critical or emcy}
          AppLog.Lines.Add(DefaultOuputToAppLog(zhl, b, MSTtoStr(dsbuf[lenfix+7])));
          topp[fnz, 6]:=topp[fnz, 6] or 256;
          result:=true;                            {Something found in PX4 overview}
        end;
      end else begin
        if dsbuf[lenfix+7]<>hbt then begin         {ID 1. Heartbeat}
          if (hbt=0) and                           {nur beim 1. Mal}
             (dsbuf[lenfix+4]=2) and               {Quadkopter}
             (dsbuf[lenfix+5]=12) then begin       {PX4}
            ismq:=true;
            if not cbReduced.Checked then
              AppLog.Lines.Add(''''+Format('%17s', [''])+rsVType+suff+
                                 VtypeToStr(MQid)); {4 Rotor = Mantis Q}
          end;
          if not cbReduced.Checked then
            AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, MSTtoStr(dsbuf[lenfix+7])));
          hbt:=dsbuf[lenfix+7];                    {letzten Wert merken}
          cm:=GetIntFromBuf(0, 4);                 {custom mode}

  {https://github.com/Dronecode/DronecodeSDK/blob/23b76bcd208ce12159e9bd089451ff7c04e284ab/core/px4_custom_mode.h#L50-L59}

          csvarr[17]:=IntToHex(cm, 2);
          csvarr[18]:=IntToHex(hbt, 2);            {MAV state}
          csvarr[19]:=IntToHex(dsbuf[lenfix+6], 2); {MAV mode flag}
        end;
        if dsbuf[lenfix+6]<>mmf then begin         {nur ausgeben, wenn sich etwas ändert}
          if not cbReduced.Checked then
            AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, MMFtoStr(dsbuf[lenfix+6])));
          mmf:=dsbuf[lenfix+6];                    {letzten Wert merken}
        end;
        SenCSVAusgabe;                             {CSV Datensatz schreiben}
      end;
    end;
  end;

  procedure HighresIMU;                            {Msg HIGHRES_IMU (105)}
  var tme: uint64;
      wrt: double;
      it: string;                                  {aktuelle IMU temp}
      i: integer;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 8);                    {in mysec, uint64}
      bg:=tme/Secpd/1000000;                       {Zeitstempel überall verfügbar}
      for i:=0 to 11 do begin
        wrt:=GetFloatFromBuf((i*4)+8);             {12 Werte ab [m/s/s] X acceleration}
        csvarr[i+26]:=FormatFloat(ctfl, wrt);
      end;
      wrt:=GetFloatFromBuf(56);                    {IMU temperature [deg C]}
      it:=FormatFloat(dzfl, wrt);
      csvarr[45]:=it;
      if it<>itemp then begin                      {nur wenn Temp sich ändert}
        if not cbReduced.Checked then
          AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, csvIMUtemp+suff+it+'°C'));
        itemp:=it;                                 {Vergleichswert merken}
      end;
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure LocalPosNed;                           {Msg LOCAL_POSTION_NED (32)}
  var tme: uint32;
      vx, vy, vz: double;
      i: integer;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/Secpd/1000;                          {Zeitstempel überall verfügbar}
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

{Only new waypoints for recording in AppLogHighlighter, no CSV}
  procedure PositionTargetGlobal;                  {POSITION_TARGET_GLOBAL_INT 87}
  var tme: uint32;
      lat, lon: integer;
      ele, yawsetp: double;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/secpd/1000;                          {Zeitstempel überall verfügbar}
      tstr:=FormatDateTime(dzf, ftm)+'T'+
            FormatDateTime(zzf, bg)+'Z';           {Zeitstring überall verfügbar}

      lat:=GetIntFromBuf(4, 4);
      lon:=GetIntFromBuf(8, 4);                    {degrees E7  (/10000000)}

      if (lat<>latw1) or (lon<>lonw1) then begin   {new target waypoint}
        ele:=GetFloatFromBuf(12);                  {[m] Altitude (MSL, AGL or relative to home altitude, depending on frame)}
        yawsetp:=GetFloatFromBuf(40);              {Yaw setpoint}
        yawsetp:=RadToGrad(yawsetp);               {Conversion rad (+/- 180°) to °}

        AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, MsgIDtoStr(87)+suff+
                           rsGPSh+suff+FormatFloat(ctfl, ele)+'m'+tab2+
                           CoordFrameToStr(dsbuf[lenfix+50])+tab2+
                           'Yaw setpoint'+suff+FormatFloat(ctfl, yawsetp)+'°'));
        AppLog.Lines.Add(trnrApplog+URLGMap(FloatToStr(lat/10000000), FloatToStr(lon/10000000)));

        latw1:=lat;
        lonw1:=lon;
      end;
    end;
  end;

  procedure GlobalPosInt;                          {Msg Global_POSTION_INT (33)}
  var tme, hdg: uint32;
      vx, vy, vz: double;
      distp, dists: double;                        {Entfernung zum "Start"}
      lat, lon, altr, ele: integer;                {int32}

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/secpd/1000;                          {Zeitstempel überall verfügbar}
      tstr:=FormatDateTime(dzf, ftm)+'T'+
            FormatDateTime(zzf, bg)+'Z';           {Zeitstring überall verfügbar}

      lat:=GetIntFromBuf(4, 4);
      lon:=GetIntFromBuf(8, 4);                    {degrees E7  (/10000000)}
      ele:=GetIntFromBuf(12, 4);                   {altitude MSL [mm]}
      altr:=GetIntFromBuf(16, 4);                  {relative alt [mm]}
      csvarr[51]:=FormatFloat(ctfl, ele/1000);     {altitude MSL}

      if ((lat<>0) or (lon<>0)) and                {nur bei gültigen Koordinaten}
         (altr<maxxh*1000) then begin              {nur sinnvolle Höhe}
        lat2:=lat/10000000;                        {nur einmal rechnen}
        lon2:=lon/10000000;                        {und überall verfügbar}
        csvarr[4]:=FormatFloat(ctfl, altr/1000);   {altitude relative}
        csvarr[5]:=FloatToStr(lat2);               {Koordinaten}
        csvarr[6]:=FloatToStr(lon2);

        if not isGPS then begin                    {erster gültiger GPS Datensatz}
          lat1:=lat2;                              {"Start"-Koordinaten}
          lon1:=lon2;
          lat3:=lat2;                              {init vorherige Koordinaten}
          lon3:=lon2;
          ele0:=ele;                               {Altitude first MSL}
        end;
        isGPS:=true;                               {1. Datensatz erledigt}
        if ele>elemax then elemax:=ele;            {Gipfelhöhe MSL}

        distp:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2); {Entfernung zum 1. Punkt}
        dists:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum vorherigen Punkt}
        if distp>distmax then                      {maximale Entfernung ermitteln}
          distmax:=distp;
        distg:=distg+dists;                        {Länge Route ermitteln}
        lat3:=lat2;                                {Koordinaten merken}
        lon3:=lon2;

        AusgTrack;

        if tb then begin                           {Höhendiagramm füllen}
          case msl of
            2: Chart1BarSeries1.AddXY(bg, (altr)/1000);   {fuchsia like Angle}
            3: Chart1BarSeries6.AddXY(bg, (altr)/1000);   {blue}
            4: Chart1BarSeries3.AddXY(bg, (altr)/1000);   {red}
          else
            Chart1BarSeries2.AddXY(bg, (altr)/1000); {relative Höhe zum ersten Wert}
          end;
          Chart1LineSeries2.AddXY(bg, (altr)/1000);  {Hüllkurve}
          Chart1LineSeries1.AddXY(bg, distp);
        end;

      end;

      vx:=GetIntFromBuf(20, 2)/100;                {[cm/s] X Speed}
      vy:=GetIntFromBuf(22, 2)/100;                {[cm/s] Y Speed}
      vz:=GetIntFromBuf(24, 2)/100;                {[cm/s] Z Speed}
      if vx<100 then
        csvarr[41]:=FormatFloat(ctfl, vx)
      else
        vx:=0;
      if vy<100 then
        csvarr[42]:=FormatFloat(ctfl, vy)
      else
        vy:=0;
      if vz<100 then
        csvarr[43]:=FormatFloat(ctfl, vz)
      else
        vz:=0;
      csvarr[7]:=FormatFloat(ctfl,                 {Orginal Spalte tas}
        sqrt((vx*vx)+(vy*vy)+(vz*vz)));            {tas in m/s berechnet}
      hdg:=GetIntFromBuf(26, 2);
      if hdg<36000 then
        csvarr[50]:=FormatFloat(ctfl, hdg/100);    {Heading [cdeg] 0..359.99}

      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

(*  Format pass nicht zur Beschreibung; Länge immer 11 Byte
00	50	23	E8	07
11      88	71	01	00	00      {6 byte immer gleich}


  procedure GPS_Status;                            {GPS_STATUS (25 = 19'h}
  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    csvarr[10]:=IntToStr(GetIntFromBuf(3, 1));     {Number sats}
  end;    *)

  procedure vfr_hud;                               {Msg VFR_HUD (74)}
  var wrt: double;
      thr: uint;
      hdg: integer;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
(* Mehr Fragen als Antworten! Lieber nichts überschreiben.
      wrt:=GetFloatFromBuf(0);                     {[m/s] Current indicated airspeed (IAS)}
      csvarr[7]:=FormatFloat(ctfl, wrt);
      wrt:=GetFloatFromBuf(4);                     {[m/s] Current ground speed}
      csvarr[25]:=FormatFloat(ctfl, wrt);                         *)
      wrt:=GetFloatFromBuf(8);                     {[m] Current altitude (MSL)}
      csvarr[51]:=FormatFloat(ctfl, wrt);

      wrt:=GetFloatFromBuf(12);                    {[m/s] Current climb rate}
      csvarr[48]:=FormatFloat(ctfl, wrt);
   { Werte passen nicht zu Maßeinheiten.   }
      hdg:=GetIntFromBuf(16, 2);                   {[deg] Current heading in
                                                    compass units (0-360, 0=north)}
      csvarr[50]:=IntToStr(hdg);                   {Heading}
      thr:=0;                                      {default: 0%}
      case len of                                  {pos depending on lenght ??}
        17: thr:=GetIntFromBuf(18, 2);             {[%] Current throttle setting (0 to 100))}
        18: thr:=GetIntFromBuf(19, 2);
        19: thr:=GetIntFromBuf(20, 2);
      end;
      csvarr[49]:=FormatFloat(ctfl, thr*100/255);  {Throttle}
//      csvarr[49]:=IntToStr(thr);
      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

  procedure Attitude;                              {Msg ATTITUDE (30)}
  var tme: uint32;
      wrt: double;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/Secpd/1000;                          {Zeitstempel überall verfügbar}
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
      cca: integer;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    volt:=GetIntFromBuf(14, 2)/1000;
    cca:=GetIntFromBuf(16, 2);                     {Current in cA, -1 means not used}
    curr:=cca/100;                                 {Currency in A}
    if (bgl>0) and (cca>=0) then begin             {Used capacity in mAh}
      cca:=cca+speBaseLoad.Value;                  {Correction value for drained
             current in cA (i.e. for camera, lights, mainboard etc), default 2A}
      ucap:=ucap+((bg-bgl)*cca*240);
    end;
{load: Maximum usage in percent of the mainloop time.
 Values: [0-1000] - should always be below 1000 (100%)}
    load:=GetIntFromBuf(12, 2)/10;
    csvarr[52]:=FormatFloat(dzfl, load);           {SW-load in %}
    csvarr[2]:=FormatFloat(dzfl, volt);            {Voltage in V}
    csvarr[3]:=FormatFloat(ctfl, curr);            {Current in A}
    battremain:=dsbuf[lenfix+30];
    csvarr[46]:=IntToStr(battremain);              {Battery remaining %}
    csvarr[47]:=FormatFloat(ctfl, ucap);          {Battery used mAh}
    if not cbReduced.Checked then
      AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, csvVolt+suff+csvarr[2]+'V'+
                       tab4+csvAmp+suff+csvarr[3]+'A'+
                       tab4+csvUcap+suff+csvarr[47]+'mAh'));
    sst:=GetIntFromBuf(0, 4);                      {Sensor present}
    if not cbReduced.Checked then
      AppLog.Lines.Add(trnrApplog+'onboard_control_sensors_present'+suff+
                         MSenStat(sst));           {MAV_SYS_STATUS_SENSOR}
    sst:=GetIntFromBuf(4, 4);                      {Sensor enabled}
    if (sst and 32)>0  then                        {GPS status}
      csvarr[16]:=idtrue
    else
      csvarr[16]:=idfalse;
    if not cbReduced.Checked then
      AppLog.Lines.Add(trnrApplog+'onboard_control_sensors_enabled'+suff+
                         MSenStat(sst));
{[c%] Communication drop rate, (UART, I2C, SPI, CAN), dropped packets on all
 links (packets that were corrupted on reception on the MAV)}
    sst:=GetIntFromBuf(18, 2);                     {Drop rate}
    csvarr[14]:=FormatFloat(ctfl, sst/100);

    sst:=GetIntFromBuf(8, 4);                      {Sensor health}
    csvarr[15]:=IntToHex(sst, 2);
    if not cbReduced.Checked then
      AppLog.Lines.Add(trnrApplog+'onboard_control_sensors_health '+suff+
                         MSenStat(sst));
    if tb then begin                               {Anzeige in Schnellanalyse}
      Chart3LineSeries1.AddXY(bg, volt);           {Spannung}
      Chart4LineSeries1.AddXY(bg, curr);           {Strom (nicht bei Mantis Q)}
      Chart5LineSeries1.AddXY(bg, ucap);           {Used capacity in mAh}
    end;
    bgl:=bg;
    SenCSVAusgabe;                                 {CSV Datensatz schreiben}
  end;

  procedure RCchannels;
  var i: integer;
      wrt: uint32;

  begin
    StandardAusgabe;                               {hexwerte in StringGrid darstellen}
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      wrt:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=wrt/Secpd/1000;                          {Zeitstempel überall verfügbar}
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
      pvalue: double;

  begin
    StandardAusgabe;                               {Hex values in CSV table}
    num:=GetIntFromBuf(4, 2);                      {Total number of onboard parameters}
    idx:=GetIntFromBuf(6, 2);                      {Index of this onboard parameter}
    pvalue:=GetFloatFromBuf(0);
    if isNan(pvalue) then
      wrt:=GetHexFromBuf(0, 4)
    else
      wrt:=FloatToStr(pvalue);                     {Onboard paramaeter value}
    if (GetIntFromBuf(1, 3)=0) and                 {Extreme small values, masks}
       (dsbuf[lenfix]>0) then
      wrt:='$'+IntToHex(dsbuf[lenfix], 2)+'='+
           IntToBin(dsbuf[lenfix], 8, 4);          {Overwrite ...E-44, ...E-45}
    paramID:='';
    for i:=8 to 23 do begin                        {Onboard parameter ID}
      if dsbuf[lenfix+i]=0 then                    {Terminated by NULL}
        break;
      paramID:=paramID+Char(dsbuf[lenfix+i]);
    end;
    csvarr[57]:=paramID;
    csvarr[58]:=wrt;
    AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, Format('%-16s', [paramID])+suff+
                       'Type'+suff+IntToStr(dsbuf[lenfix+24])+kma+
                       'Index'+suff+IntToStr(idx)+kma+
                       'Count'+suff+IntToStr(num)+kma+
                       'Value'+suff+wrt));
    SenCSVAusgabe;                                 {CSV Datensatz schreiben}
  end;

  procedure Systemzeit;                            {[us] Timestamp (UNIX epoch time)}
  var ts: TDateTime;
  begin
    StandardAusgabe;                               {Hex values in CSV table}
    if (not cbReduced.Checked) or dzt then begin
      ts:=UnixToDateTime(GetIntFromBuf(0, 8) div 1000000);        {us --> s}
      AppLog.Lines.Add(DefaultOuputToAppLog(zhl, bg, 'UTC:  '+FormatDateTime(vzf, ts)));
      dzt:=false;
    end;
  end;

  procedure AusgabeSensor;                         {Datenausgabe abh. von MsgID}
  var i, e: integer;

  begin
    e:=GetIntFromBuf(-3, 3);                       {MsgID 3 Byte als Zahl}
    csvarr[posChan-1]:=IntToStr(e);                {Message ID dezimal hinten}
    if tb then begin                               {Alle Datenanzeigen füllen}
      if gridDetails.RowCount<(zhl+2) then
        gridDetails.RowCount:=gridDetails.RowCount+2000; {neue Zeilen}
      inc(zhl);                                    {Datensätze zählen}
      if zhl=20 then
        gridDetails.AutoSizeColumns;               {As soon as possible and only once}
      case e of                                    {Ausgabe bekannter Messages}
        0:   if MAVmsg.Checked[0] then Heartbeat;  {HEARTBEAT (0) ohne Zeitstempel}
        1:   if MAVmsg.Checked[1] then SensorStatus;   {MAV_SYS_STATUS (1)}
        2:   Systemzeit;                           {SYSTEM_TIME (2) AppLog only}
        22:  if MAVmsg.Checked[2] then ParamValue; {PARAM_VALUE ($16)}
        24:  if MAVmsg.Checked[3] or gx then
               GPSAusgabe;                         {GPS_RAW_INT ($18)}
//      25:  GPS_Status;                           {GPS_STATUS ($19) only # sats added}
        30:  if MAVmsg.Checked[4] then Attitude;   {ATTITUDE ($1E)}
        32:  if MAVmsg.Checked[5] then LocalPosNed;    {LOCAL_POSITION_NED ($20)}
        33:  if MAVmsg.Checked[6] then GlobalPosInt;   {GLOBAL_POSITION_INT ($21)}
        65:  if MAVmsg.Checked[7] then RCchannels; {RC_CHANNELS ($41)}
        74:  if MAVmsg.Checked[8] then vfr_hud;    {VFR_HUD ($4A)}
        87:  PositionTargetGlobal;                 {POSITION_TARGET_GLOBAL_INT 87 ($57), only AppLogHighlighter}
        105: if MAVmsg.Checked[9] then HighresIMU; {HIGHRES_IMU ($69)}
        141: if MAVmsg.Checked[10] then Altitude;  {MSG_ID_ALTITUDE 141 ($8D)}
        147: if MAVmsg.Checked[11] then Batt_Status;   {BATTERY_STATUS ($93)}
        245: if MAVmsg.Checked[12] then ExtAusgabe;    {Extended_SYS_State ($F5)}
        253: TextAusgabe;                          {Statustext ($FD)}
      else                                         {Standard Ausgabe}
        StandardAusgabe;                           {Hexwerte für alle anderen Msg}
      end;
      for i:=0 to lenfix-1 do begin                {FixPart Teil 1 für alle}
        gridDetails.Cells[i, zhl]:=IntToHex(dsbuf[i], 2);           {in Hex}
//        gridDetails.Cells[i, zhl]:=IntToStr(dsbuf[i]);            {in Dec}
      end;
      gridDetails.Cells[lenfix+1, zhl]:=IntToStr(len); {Payload Länge eintragen}
      gridDetails.Cells[lenfix, zhl]:=MsgIDtoStr(e);   {Message Name}
    end else begin
      if ov10 then begin                           {Search PX4 Emergency}
        case e of
          0: Heartbeat;                            {Set result if Mavstate = critical or emergency}
          30, 65: bg:=GetIntFromBuf(0, 4)/secpd/1000; {Zeitstempel speichern}
          253: TextAusgabe;                        {Looking for severity EMERGENCY}
        end;
      end;
      if gx and (e=24) then
        GPSAusgabe;                                {GPS raw_int for track w/o table}
      if ov11 then begin                           {Overview PX4 text messages}
        case e of
          30, 65: bg:=GetIntFromBuf(0, 4)/secpd/1000; {Zeitstempel}
          87:  PositionTargetGlobal;               {POSITION_TARGET_GLOBAL_INT 87 ($57)}
          253: TextOverview;
        end;
      end;
    end;
  end;

begin
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blockieren}
  zhl:=0;
  msl:=0;                                          {MAV landed_state undef}
  hbt:=0;                                          {MAV state uninit/unknown}
  mmf:=0;                                          {MAV Mode Flags}
  latw1:=0;                                        {init for position_target_global_int}
  lonw1:=0;
  s:='';                                           {noch keine Ausgabe}
  ele0:=0;                                         {Höhe bei Start}
  distmax:=0;                                      {maximale Entfernung}
  distg:=0;                                        {länge Route}
  elemax:=-1000000;
  ucap:=0;                                         {Used battery capacity}
  bgl:=0;
  bglg:=0;
  tstr:='';
  skoor:='';
  bg:=0;                                           {Zeitstempel allg}
  isGPS:=false;
  dzt:=true;                                       {Systemtime mindestens einmal anzeigen}
  itemp:='';
  homestr:='';                                     {URL Homepoint, wenn vorhanden}
  ismq:=false;
  topp[fnz, 5]:=0;                                 {Pointer für Suche Null setzen}
  result:=false;
  for i:=0 to csvanz do
    csvarr[i]:='';
  if FileSize(fn)>lenfixP then begin
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    HDiaInit;                                      {Höhendiagramm initalisieren}
    if tb then begin
      KursorAus;
      SetSensorEnv;
      Chart1.DisableRedrawing;
      Chart3.DisableRedrawing;
      Chart4.DisableRedrawing;
      Chart5.DisableRedrawing;
      Chart1ConstantLine2.Position:=0;
      Chart1ConstantLine2.Active:=false;
      Chart3LineSeries1.Clear;
      Chart3LineSeries2.Clear;
      Chart4LineSeries1.Clear;
      Chart5LineSeries1.Clear;
      Chart3.ZoomFull;
      Chart4.ZoomFull;
      Chart5.ZoomFull;
      Chart3LineSeries1.SeriesColor:=ColorButton2.ButtonColor;
      Chart4LineSeries1.SeriesColor:=ColorButton3.ButtonColor;
      Chart5LineSeries1.SeriesColor:=ColorButton4.ButtonColor;
      Chart1.AxisList[2].Title.Caption:=rsDistHome;        {Entfernung}
      Chart3.AxisList[0].Title.Caption:=csvVolt+' [V]';    {y-Achse top}
      Chart4.AxisList[0].Title.Caption:=csvAmp+' [A]';     {y-Achse middle}
      Chart5.AxisList[0].Title.Caption:=csvUcap+' [mAh]';  {y-Achse bottom}
      Chart3.Hint:=rsChart+tab1+Chart3.AxisList[0].Title.Caption;
      Chart4.Hint:=rsChart+tab1+Chart4.AxisList[0].Title.Caption;
      Chart5.Hint:=rsChart+tab1+Chart5.AxisList[0].Title.Caption;
      if (pcMain.ActivePageIndex>3) or             {tabScan and higher}
         (pcMain.ActivePage=tabOverview) then
        pcMain.ActivePage:=tabDetails;             {Zur Tabelle springen}

      gridDetails.BeginUpdate;
      gridDetails.RowCount:=1;
      gridDetails.ColCount:=YTHPcols;              {Spaltenanzahl vorbelegen}
      gridDetails.Cells[0, 0]:=Fix+'1';            {Wirklicher FixPart}
      gridDetails.Cells[1, 0]:=Fix+'2';
      gridDetails.Cells[2, 0]:='SeqNo';            {bekannte Spalten}
      gridDetails.Cells[3, 0]:='SysID';            {The H520 uses the mavlink
                system ID 1. This is currently set fixed and can't be changed}
      gridDetails.Cells[4, 0]:='CompID';
      gridDetails.Cells[5, 0]:=csvMsgID+'0';
      gridDetails.Cells[6, 0]:=csvMsgID+'1';
      gridDetails.Cells[7, 0]:=csvMsgID+'2';
      gridDetails.Cells[8, 0]:='MsgName';
      gridDetails.Cells[lenfix+1, 0]:='lenPL';     {Länge Payload Header als Trenner}

      for i:=lenfix+1 to lenfixP-2 do              {Rest Fixpart, aber mit Daten}
        gridDetails.Cells[i+1, 0]:=fix+IntToStr(i);
      for i:=lenfixP-1 to gridDetails.ColCount-2 do        {Payload Byte Nummern}
        gridDetails.Cells[i+1, 0]:='PL'+IntToStr(i-lenfixP+2);

      gridDetails.ColWidths[8]:=110;               {Msg Name}
      gridDetails.EndUpdate;
      Chart1.EnableRedrawing;
      Chart3.EnableRedrawing;
      Chart4.EnableRedrawing;
      Chart5.EnableRedrawing;
    end;
    FillChar(dsbuf, length(dsbuf), 0);             {Datenbuffer löschen}
    maplist:=TStringList.Create;
    outlist:=TStringList.Create;
    datlist:=TStringList.Create;                   {Ausgabedatei für csv-Daten}

    infn:=TMemoryStream.Create;
    try
      ftm:=FileDateToDateTime(FileAge(fn));
      infn.LoadFromFile(fn);
      StatusBar1.Panels[5].Text:=rsWait;
      StatusBar1.Update;
      AppLog.Lines.Add(LineEnding);
      AppLog.Lines.Add(fn);

      if gx then begin                             {Eventuell KML oder GPX erzeugen}
        if rgOutFormat.ItemIndex=2 then begin
          GPXHeader(cbxText.Text, fn, ftm, maplist)
        end else begin
          KMLHeader(fn, ftm, maplist);
        end;
      end;

      if tb then
        gridDetails.BeginUpdate;

      AppLog.BeginUpdate(false);                   {Without UnDo Block}
      while infn.Position<(infn.Size-lenfixP) do begin {bis zum Ende der Datei}
        len:=0;                                    {Reset for error detection}
        try
          repeat
            b:=infn.ReadByte;
          until (b=MagicMAVlinkV2) or (infn.Position>infn.Size-lenfixP);
          len:=infn.ReadByte;                      {Länge Payload mit CRC}
          infn.ReadBuffer(dsbuf, len+lenfixP-2);   {Länge Rest-Datensatz mit
                                    FixPart, aber ohne $FD und Längen-Byte (-2)}
          AusgabeSensor;                           {alles anzeigen, ohne Filter}
        except
          if zhl>0 then
            AppLog.Lines.Add('''Broken record No'''+suff+
                               IntToStr(zhl)+', Byte'+suff+IntToHex(b, 2)+
                               ', Payload length'+suff+IntToStr(len));
{Usually the last record in a tlog file is too short compared to payload length,
 thus this exception will be raised for each file at the end.}
        end;
      end;
      AppLog.EndUpdate;

      if tb then begin
        gridDetails.RowCount:=zhl+1;
        gridDetails.EndUpdate;
      end;

      if gx and
         (skoor<>'') then begin                    {KML oder GPX speichern}
        if rgOutFormat.ItemIndex=2 then begin      {GPX}
          GPXfooter1(maplist);
          maplist.Add('<wpt '+skoor);              {Landepunkt}
          maplist.Add(' <time>'+tstr+'</time>');
          maplist.Add(write_nme('Stop'));
          maplist.Add(GPXet1);
          GPXfooter2(maplist);
          maplist.SaveToFile(ChangeFileExt(fn,
                             rgOutFormat.Items[rgOutFormat.ItemIndex]));
        end else begin                             {KML/KMZ}
          for i:=0 to outlist.Count-1 do
            maplist.Add(outlist[i]);
          maplist.Add('  </gx:Track>');
          maplist.Add('</'+pmtag);                 {End playable track}
          placemark(maplist, '#landing', '',      {Landepunkt}
                    StringReplace(skoor, tab1, sep, [rfReplaceAll]), tstr);
          KMLFooter2(maplist);
          maplist.SaveToFile(ChangeFileExt(fn, rgOutFormat.Items[0]));
          if rgOutFormat.ItemIndex=1 then
            MacheKMZ(ChangeFileExt(fn, rgOutFormat.Items[0]));
        end;
      end;

      if tb then begin
        speDataPoint.MaxValue:=zhl;
        StatusBar1.Panels[5].Text:=fn;
        StatusBar1.Panels[0].Text:=IntToStr(FileSize(fn));
        StatusBar1.Panels[1].Text:=IntToStr(zhl);
        s:=vTypeToStr(YTHPid)+tab4;                {Default: H Plus}
        if ExtractFileExt(fn)=hext then s:=vTypeToStr(H5id)+tab4;
        if ismq then s:=vTypeToStr(MQid)+tab4;
        Chart1.Title.Text.Add(s+ExtractFileName(fn));
        if ele0<>0 then begin
          AppLog.Lines.Add(LineEnding);
          AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                             URLGmap(FloatToStr(lat1),
                                     FloatToStr(lon1)));
{GPS Datensatz in AppLogHighlighter als Link mit Google Maps Adresse ablegen}
          if homestr<>'' then
            AppLog.Lines.Add(homestr);
          AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                             URLGmap(FloatToStr(lat2),
                                     FloatToStr(lon2)));
(*          AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                             URLosm(FloatToStr(lat2),
                                    FloatToStr(lon2))); *)
          s:=Format('%-25s', [rsGPSh+rsAbsh+suff])+
             Format('%7.1f', [ele0/1000])+'m';
          Chart1.Title.Text.Add(s);                {absolute Höhe für Null}
          AppLog.Lines.Add(s);
          AppLog.Lines.Add(Format('%-25s', ['Absolute '+rsGridCell5+suff])+
                             Format('%7.1f', [elemax/1000])+'m');
          AppLog.Lines .Add(Format('%-25s', ['Relative '+rsGridCell5+suff])+
                              Format('%7.1f', [(elemax-ele0)/1000])+'m');
          AppLog.Lines.Add(LineEnding);
          AppLog.Lines .Add(Format('%-25s', [rsGridCell6+suff])+
                              Format('%7.1f', [distmax])+'m');
          AppLog.Lines .Add(Format('%-25s', [rsGridCell7+suff])+
                              Format('%7.1f', [distg])+'m');
          AppLog.Lines.Add(LineEnding);
          if ucap>0 then begin
            AppLog.Lines.Add(Format('%-25s', [csvUcap+suff])+
                               Format('%7.0f', [ucap])+'mAh');
            AppLog.Lines.Add(Format('%-25s', [csvCap+suff])+
                               Format('%7.0d', [battremain])+'%');
            if battremain<100 then
              AppLog.Lines.Add(Format('%-25s', [rsAbattCap+suff])+
                                 Format('%7.0f', [100*ucap/(100-battremain)])+'mAh');
          end;
        end;

        Chart1.Title.Visible:=true;                {Diagrammtitel anzeigen}
        AppLog.Lines.Add(LineEnding);
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsMAVLink+tab1+rsDS);
        AppLog.Lines.Add(LineEnding);
        if (datlist.Count>2) and
           cbMAVasCSV.Checked then begin
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
              csvUcap+sep+'Climb rate'+sep+'Throttle'+sep+csvHeading+sep+csvAltMSL+sep+
              csvSWload+sep+'53'+sep+'54'+sep+'55'+sep+'56'+sep+
              'Onboard paramater name'+sep+'Parameter value'+sep+
              csvMsgID+sep+'CH used';
          for i:=1 to 18 do
            s:=s+sep+'CH'+IntToStr(i);
          datlist.Insert(0, s);
          datlist.SaveToFile(ChangeFileExt(fn, fext));  {als *.csv speichern}
        end;
      end;
    finally
      infn.Free;
      maplist.Free;
      outlist.Free;
      datlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.AusgabeMessages(const fn: string; {List of used MsgID in PX4 file}
                 var outlist: TStringList);        {List as CSV}
var
  inf: TMemoryStream;
  msglist: TStringList;
  e, i, zhl: integer;
  s: string;

  procedure MsgID_MAV_V2;
  var
    i, len: integer;
    b: byte;
    dsbuf: array[0..YTHPcols] of byte;

  begin
    while inf.Position<(inf.Size-lenfixP) do begin {bis zum Ende der Datei}
      try
        repeat                                   {Scan whole file for msgIDs}
          b:=inf.ReadByte;
        until (b=MagicMAVlinkV2) or (inf.Position>inf.Size-lenfixP);
        len:=inf.ReadByte;                       {Länge Payload mit CRC}
        inf.ReadBuffer(dsbuf, len+lenfixP-2);    {Read Rest-Datensatz mit
                                  FixPart, aber ohne $FD und Längen-Byte (-2)}
        e:=0;
        inc(zhl);
        for i:=0 to 2 do
          e:=e+dsbuf[lenfix-3+i]*(256**i);       {MsgID 3 Byte als Zahl}
        if e<512 then                            {assumption highest message ID number}
          msglist.Add('$'+IntToHex(e, 3));       {list as hex IDs}
      except
      end;
    end;
  end;

  procedure MsgID_MAV_V1;
  var
    len: integer;
    b: byte;
    dsbuf: array[0..YTHPcols] of byte;

  begin
    while inf.Position<(inf.Size-lenfixP) do begin {bis zum Ende der Datei}
      try
        repeat                                   {Scan whole file for msgIDs}
          b:=inf.ReadByte;
        until (b=MagicBC) or (inf.Position>=inf.Size-lenfix);
        len:=inf.ReadByte;                       {Länge Payload mit CRC}
        inf.ReadBuffer(dsbuf, len+lenfix-2);     {-2: two bytes already got}
        inc(zhl);
        msglist.Add('$'+IntToHex(dsbuf[3], 3));  {list as hex IDs}
      except
      end;
    end;
  end;


begin
  zhl:=0;
  if FileSize(fn)>lenfixP then begin
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    msglist:=TStringList.Create;
    msglist.Sorted:=true;
    msglist.Duplicates:=dupIgnore;
    outlist.Clear;
    inf:=TMemoryStream.Create;
    try
      inf.LoadFromFile(fn);
      s:=ExtractFileName(fn);
      if (pos(nfile, s)=1) and (pos(sext, s)>10) then
        MsgID_MAV_V1
      else
        MsgID_MAV_V2;

      if msglist.Count>0 then begin
        AppLog.Lines.Add(fn);
        AppLog.Lines.Add(rsUsedMAV);
        AppLog.Lines.Add(LineEnding);
        s:=csvMsgID+#9+csvMsgID+#9+'*'+#9+'MAVlink Message';
        AppLog.Lines.Add(s);
        s:=StringReplace(s, #9, sep, [rfReplaceAll]);
        outlist.Add(s);
        StatusBar1.Panels[0].Text:=IntToStr(zhl);
        StatusBar1.Panels[1].Text:=IntToStr(msglist.Count);
        StatusBar1.Panels[5].Text:=rsUsedMAV;
        for i:=0 to msglist.Count-1 do begin
	  e:=Hex2Dec(msglist[i]);                  {convert back to integer}
  	  s:=IntToStr(e)+#9+msglist[i]+#9;
          if e in MAVmsgX then                     {extracted messages}
            s:=s+'x';
          s:=s+#9+UpCase(MsgIDtoStr(e));
 	  AppLog.Lines.Add(s);
          s:=StringReplace(s, #9, sep, [rfReplaceAll]);
	  outlist.Add(s);
        end;
        AppLog.Lines.Add(LineEnding);
      end;
    finally
      inf.Free;
      msglist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TForm1.ReadMAV(const mode: integer);     {Sensor Datei H auslesen}
var fn: string;

begin
  if lbFlights.Items.Count>0 then begin
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+npath+
        PathDelim+nfile+lbFlights.Items[lbFlights.ItemIndex]+sext;  {Sensor}
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

        if chr(buf[i])=csvsep then begin           {Semicolon als Trenner}
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
    gridDetails.Tag:=17;
    v_type:=1;
    result:=true;
  end else begin
    for i:=0 to l.count-1 do begin
      if l[i]=fmode then begin
        gridDetails.Tag:=i;                        {Position f_mode merken}
        result:=true;                              {validiert, f_mode gefunden}
        break;
      end;

      if l[i]=pfmode then begin
        gridDetails.Tag:=i;                        {Position fMode merken}
        result:=true;                              {validiert, fMode gefunden}
        v_type:=YTHPid;                            {YTH Plus gefunden}
        break;
      end;
    end;
  end;
end;

function TForm1.GetFW(var fwout: TarrFW): integer; {Firmware auslesen}
var fn: string;

begin
  result:=0;                                       {Zeiger Feld im Ausgabearray}
  if lbFlights.Items.Count>0 then begin
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+npath+
        PathDelim+nfile+lbFlights.Items[lbFlights.ItemIndex]+sext; {Sensor}
    if FileSize(fn)>FWsz then
      result:=ReadFW(fn, fwout);
  end;
end;

procedure TForm1.SelDirSet;                        {FlightLog auswählen}
begin
  StatusBar1.Panels[5].Text:=capSelDir;
  AppLog.Lines.Add(StatusBar1.Panels[5].Text);
  SelectDirectoryDialog1.Title:=capSelDir;
  if SelectDirectoryDialog1.Execute then begin
    cbxLogDir.Text:=SelectDirectoryDialog1.FileName;
    SelDirAct('');
  end;
end;

procedure TForm1.SelDirProt;                       {Protokollverzeichnis auswählen}
begin
  StatusBar1.Panels[5].Text:=capSelProt;
  AppLog.Lines.Add(StatusBar1.Panels[5].Text);
  SelectDirectoryDialog1.Title:=capSelProt;
  if SelectDirectoryDialog1.Execute then
    cbxScanDir.Text:=SelectDirectoryDialog1.FileName;
end;

procedure TForm1.EnSave;                           {Speichern erlauben}
begin
  if lbFlights.Items.Count>0 then begin
    btnConv.Enabled:=true;
    mnConvert.Enabled:=true;
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);    {Button beenden}
begin
  Close;
end;

procedure TForm1.btnCutClick(Sender: TObject);     {Cut files for analysis}
begin
  ManualCut;
end;

procedure TForm1.CutLegacy(mode: integer);
var
  x: integer;
  fn, fno: string;                                 {file name, file number}
  z:  integer;
  bg: TDateTime;
  inlist, outlist: TStringList;

begin
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  fno:='';
  try
    z:=random(8)+1;
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
        kfile+lbFlights.Items[lbFlights.ItemIndex]+fext;  {Telemetry}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      if mode=1 then begin                         {Autocut, define start- and endpoint automatically}
        cutb:=ZeitToDT(copy(inlist[1], 1, lzyu), 0);      {defaults for non fly}
        cute:=ZeitToDT(copy(inlist[inlist.Count-1], 1, lzyu), 0);

        for x:=1 to inlist.count-1 do begin        {Find start a/c}
          if inlist[x].Split([sep])[gridDetails.Tag]<>'16' then begin
            if x>SpaceAutoCut then
              cutb:=ZeitToDT(copy(inlist[x-SpaceAutoCut], 1, lzyu), 0);
            break;
          end;
        end;

        for x:=inlist.count-1 downto 1 do begin    {Find last landing}
          if inlist[x].Split([sep])[gridDetails.Tag]<>'16' then begin
            if inlist.count>(x+SpaceAutoCut+1) then
              cute:=ZeitToDT(copy(inlist[x+SpaceAutoCut], 1, lzyu), 0);
            break;
          end;
        end;

      end;
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

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+spath+
        PathDelim+sfile+lbFlights.Items[lbFlights.ItemIndex]+fext; {RemGPS}
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

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+fpath+
        PathDelim+ffile+lbFlights.Items[lbFlights.ItemIndex]+fext; {Remote}
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
      AppLog.Lines.Add(capCut+suff+rsGridCell2+tab1+           {von}
                         StatusBar1.Panels[3].Text+tab1+rsGridCell3+tab1+
                         StatusBar1.Panels[4].Text);           {bis}
      if v_type=YTHPid then begin                  {YTH Plus}
        StatusBar1.Panels[5].Text:=mndirp+wldcd+GetNr(ExtractFileName(fno))+tab1+rsSaved;
      end else begin
        StatusBar1.Panels[5].Text:=mndir+wldcd+GetNr(ExtractFileName(fno))+tab1+rsSaved;
      end;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      SelDirAct('');
    end;
  finally
    inlist.Free;
    outlist.Free;
  end;
end;

procedure TForm1.ManualCut;                        {Cut files for analysis}
var inlist, outlist: TStringList;
    fno: string;

  procedure CutBreeze;
  var x: integer;
      fn: string;                                  {file name}
      bg: TDateTime;

  begin
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
        lbFlights.Items[lbFlights.ItemIndex]+bext; {Breeze Logdateien}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      for x:=0 to 8 do
        outlist.Add(inlist[x]);
      for x:=9 to inlist.count-1 do begin
        bg:=ZeitToDT(copy(inlist[x], 1, lzbr), brID);
        if (bg>=cutb) and
           (bg<=cute) then outlist.Add(inlist[x]);
      end;
      fno:=RandomFN(fn, brID, 0);
      outlist.SaveToFile(fno);
      AppLog.Lines.Add(capCut+suff+rsGridCell2+tab1+        {von}
                         StatusBar1.Panels[3].Text+tab1+rsGridCell3+tab1+
                         StatusBar1.Panels[4].Text);             {bis}
      StatusBar1.Panels[5].Text:=ExtractFileName(fno)+tab1+rsSaved;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  end;

  procedure CutH501;
  var i, z: integer;
      bg: TDateTime;
      fn: string;

  begin
    z:=random(8)+1;
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+H5file+
        lbFlights.Items[lbFlights.ItemIndex]+fext; {H501 logs}
    if FileExists(fn) then begin
      inlist.LoadFromFile(fn);
      outlist.Add(inlist[0]);                      {take over header}
      for i:=1 to inlist.Count-1 do begin
        bg:=ZeitToDT(copy(inlist[i], 1, lzyu), 0);
        if (bg>=cutb) and
           (bg<=cute) then
         outlist.Add(inlist[i]);
      end;
      fno:=RandomFN(fn, 0, z);
      outlist.SaveToFile(fno);
    end;
  end;

begin
  if (cutb>0) and                                  {nur zur Sicherheit}
     (cute>cutb) and
     (cutbidx=lbflights.ItemIndex) then begin
    inlist:=TStringList.Create;
    outlist:=TStringList.Create;
    fno:='';
    try
      case v_type of
        brID: CutBreeze;                           {Breeze}
        H501ID: CutH501;                           {Tom's Hubsan log recorder}
      else
        CutLegacy(0);                              {legacy Yuneec}
      end;
    finally
      FreeAndNil(inlist);
      FreeAndNil(outlist);
    end;
  end;
end;

procedure TForm1.btnConvClick(Sender: TObject);    {Button Konvertieren}
var x: integer;
    fn: string;

begin
  Merkliste(cbxText, speItems.Value);              {Type merken}
  if lbFlights.Items.Count>0 then begin
    for x:=0 to lbFlights.Items.Count-1 do begin
      case v_type of
        brID: begin                                {Breeze}
                case rgOutFormat.ItemIndex of
                  2: MacheGPX(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                            lbFlights.Items[x]+bext, x);    {GPX}
                  4: MacheRR(x);
                else                               {ansonsten KML/KMZ}
                  MacheKML(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                           lbFlights.Items[x]+bext, x);
                end;
              end;
        H501ID:
              begin                                {Hubsan log recorder}
                if rgOutFormat.ItemIndex=2 then    {GPX}
                  MacheGPX(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                           H5file+lbFlights.Items[x]+fext, x)
                else                               {ansonsten KML/KMZ}
                  MacheKML(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                           H5file+lbFlights.Items[x]+fext, x);
              end;
        MQid: begin
                fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                    nfile+lbFlights.Items[x]+wext; {Sensor_*.txt}
                if Fileexists(fn) then begin
                  ShowSensorPlus(fn, x, true, false, false, false);
                end else begin                     {alternativ yuneec_*.log file}
                  fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                      mfile+lbFlights.Items[x]+bext;
                  if Fileexists(fn) then begin
                    ShowSensorPlus(fn, x, true, false, false, false);
                  end;
                end;
                StatusBar1.Panels[5].Text:=fn;
              end;
        H5id: begin
                fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                    lbFlights.Items[x]+hext;        {*.tlog}
                if Fileexists(fn) then begin
                  ShowSensorPlus(fn, x, true, false, false, false);
                  StatusBar1.Panels[5].Text:=fn;
                end;
              end;

        else begin
                case rgOutFormat.ItemIndex of
                  0,1: MacheKML(IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
                                kfile+lbFlights.Items[x]+fext, x);
                  2: MacheGPX(IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
                                kfile+lbFlights.Items[x]+fext, x);
                  3: MacheDash(x);
                  4: MacheRR(x);
                  5: MacheCCC(IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
                                kfile+lbFlights.Items[x]+fext);
                end;
        end;
      end;                                         {case Vehicle Type}
    end;
    btnConv.Enabled:=false;
    mnConvert.Enabled:=false;
  end;
end;

function TForm1.ComplFN(st: string; tp: TDateTime): string;  {Namensstamm und Zeit}
                                                   {Dateinamen mit Nummer ergänzen}
begin
  result:=st;
  case rgTimeType.ItemIndex of
    0: result:=st+FormatDateTime(dzf+'_hhnnss', tp);  {Datum und Zeit}
    1: result:=st+FormatDateTime(dzf, tp);            {nur Datum}
    2: result:=st+IntToStr(DateTimeToUnix(tp));       {UNIX Zeitstempel}
    3: result:=st+IntToStr(random(89999)+10000);      {Random number}
  end;
end;

procedure TForm1.btnArchiveClick(Sender: TObject);    {Archivieren durch Umbenennen}
var newdir: string;
    p: integer;
    spl: TStringArray;
    rpath: string;

begin
  if cbxLogDir.Text<>'' then begin                 {nicht leer}
    cbxLogDir.Text:=ExcludeTrailingPathDelimiter(cbxLogDir.Text);
    newdir:=cbxLogDir.Text;
    spl:=newdir.Split(PathDelim);                  {dazu Pfad splitten}
    newdir:=spl[high(spl)];
    rpath:=mndir;                                  {default: FlightLog}
    if v_type=YTHPid then
      rpath:=mndirp;                               {nur für YTH Plus}
    if (tend>0) and                                {Endezeit}
       (pos(dkpath, cbxLogDir.Text)<1) and         {nicht Telemetry}
       (newdir=rpath) then begin
      newdir:=ComplFN(ExtractFileDir(cbxLogDir.Text)+PathDelim+rpath, tend);
      if RenameFile(IncludeTrailingPathDelimiter(cbxLogDir.Text),
                    IncludeTrailingPathDelimiter(newdir)) then begin
        p:=cbxLogDir.Items.IndexOf(cbxLogDir.Text);
        if p>=0 then
          cbxLogDir.Items[p]:=newdir;
        cbxLogDir.Text:=newdir;
      end else begin
        StatusBar1.Panels[5].Text:=rsNotRename;
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=rsNoArch;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  end;
end;

procedure TForm1.cbThunderChange(Sender: TObject); {Thunderbird Toggeln}
begin
  if cbThunder.Checked then begin
    cbThunder.Tag:=rgAltitudeType.ItemIndex;       {save last setting}
    rgAltitudeType.ItemIndex:=0;                   {Set to 'Absolute'}
  end else
    rgAltitudeType.ItemIndex:=cbThunder.Tag;       {restore setting}
end;

procedure TForm1.cbxLogDirChange(Sender: TObject);
begin
  SelDirAct('');                                   {Alles neu laden}
end;

procedure TForm1.cbxLogDirSelect(Sender: TObject);
begin
  SelDirAct('');                                   {Alles neu laden}
end;

procedure TForm1.cbxTextMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Delete model list}
begin
  if ssCtrl in Shift then
    cbxText.Items.Clear;
end;

procedure TForm1.cbxScanDirDblClick(Sender: TObject);
begin
  OpenDocument(IncludeTrailingPathDelimiter(cbxScanDir.Text));
end;

procedure TForm1.cbxScanDirMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Verzeichnisliste löschen}
begin
  if ssCtrl in Shift then
    cbxScanDir.Items.Clear;
end;

procedure TForm1.cbxSearchMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Suchliste löschen}
begin
  if ssCtrl in Shift then
    cbxSearch.Items.Clear;
end;

{Der Typhoon H Plus sendet viele unnötige Datensätze mit unsinnige Werten, welche
 die Auswertung beeinträchtigen. Diese Werte können ausgeblendet werden.}

function TForm1.CheckVT(vt, fm: string): boolean;  {Vehicle Type prüfen für YTH Plus}
var fmode: integer;

begin
  result:=true;                                    {default: Alles geht durch}
  if (v_type=YTHPid) and                           {only for Typhoon H Plus}
     (cbCleanHplus.Checked) then begin             {only if cleaning is allowed}
    fmode:=StrToIntDef(fm, 0);
    if (StrToIntDef(trim(vt), 0)<>defVT) or        {useless Vehicle Types}
       (fmode=0) or                                {useless FlightModes}
       (fmode>19) then                             {fMode 20 ist wahrscheinlich ein Fehler}
      result:=false;                               {suppress data set}
  end;
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
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBarScan.Position:=ProgressBarScan.Position+1;
    if inlist.count>minlines then begin            {Überschrift und mind. 10 Zeilen}
      try
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        splitlist.DelimitedText:=inlist[0];        {Überschrift einlesen, f_mode ermitteln}
        vld:=fModeFinden(splitlist);               {setzt gridDetails.Tag mit Spaltenindex}
        if v_type<>YTHPid then begin
          splitlist.DelimitedText:=inlist[2];      {2. Datenzeile, v_type ermitteln}
          v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
          OverwriteVT;                             {Overwrite for PX4 Thunderbird}
        end;

        for x:=1 to inlist.Count-1 do
        if CheckE7(inlist[x]) then begin           {Plausicheck für YTH Plus}
          splitlist.DelimitedText:=inlist[x];
          if (splitlist.Count>anzsp) and           {Plausicheck YTH und generell gegen Leerzeilen}
              CheckVT(splitlist[gridDetails.Tag+2],
                      splitlist[gridDetails.Tag]) then begin
            h:=StrToFloatN(splitlist[4]);          {Altitude}
            if testh(h) then begin                 {nicht bei unsinnigen Höhenwerten}
              tas:=StrToFloatN(splitlist[7]);      {True Air Speed in m/s}
              u:=StrToFloatN(splitlist[2]);        {LiPo Spannung}
              if (u<umin) and (u>5) then
                umin:=u;
              if NichtLeer(splitlist[3]) and       {Simulatorflug}
                 (splitlist[15]='231') then
                simu:=true;
              fxmode:=splitlist[gridDetails.Tag];  {FlightMode}
              if GetRFM(fxmode, v_type,
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
                ed:=ZeitToDT(splitlist[0], v_type);
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
                  if tas>tasmaxg then
                    tasmaxg:=tas;
                  if slat<>'' then begin           {Startpunkt mit GPS}
                    lat2:=StrToFloatN(splitlist[5]);
                    lon2:=StrToFloatN(splitlist[6]);
                    dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                    ddist:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
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
            AppLog.Lines.Add(''''+capLabel6+Format('%6d', [x])+       {Datenpunkt ausgeben}
                               suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Einlesen}
        flt:=flt+ed-bg1;
        splitlist.DelimitedText:=inlist[inlist.count-1];
        tend:=ZeitToDT(splitlist[0], v_type);      {letzten Zeitstempel merken}
      except
        StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS;
        AppLog.Lines.Add('''5094'+suff+StatusBar1.Panels[5].Text);
      end;
    end;

    if vld and
       ((v_type<>YTHPid) or                        {Mindestflugzeit nur beim YTH Plus}
        (cbCleanHplus.Checked=false) or            {wenn Bereinigung eingestellt ist}
        (flt>minflt)) then begin                   {Anzeige gültiger Auswertung}
      splitlist.DelimitedText:=inlist[1];          {1. Datenzeile für fehlenden Beginn/Typ}
      fmod:=StrToIntDef(splitlist[gridDetails.Tag+2],2);
      if g>3 then begin                            {nur wenn Daten vorhanden sind}
        ProgressBarScan.Update;
        olist.Add(rsFlightNr+csvsep+IntToStr(fln)+csvsep);
        gridScanResult.RowCount:=fln+1;
        gridScanResult.Cells[0, fln]:=IntToStr(fln);
        gridScanResult.Cells[1, fln]:=capNachweis+suff+
                                   FormatDateTime(vzf, bg)+bind+
                                   FormatDateTime(zzf, ed);
        olist.Add(rsVType+csvsep+vtypeToStr(fmod)+csvsep);
        if not gpsu then
          olist.Add(csvsep+rsNoGPS+csvsep);            {GPS off by Pilot}
        olist.Add(rsGridCell1+csvsep+FormatDateTime(dzf, bg)+csvsep);
        olist.Add(rsGridCell2+csvsep+FormatDateTime(zzf, bg)+csvsep);
        olist.Add(rsGridCell3+csvsep+FormatDateTime(zzf, ed)+csvsep);
        olist.Add(rsDauer+csvsep+FormatDateTime(zzf, flt)+csvsep);   {Flugzeit}
        olist.Add(rsStartpkt+csvsep+               {Startposition in GoogleMaps}
                       URLGMap(KoToStr(lat1), KoToStr(lon1))+csvsep);
        if modestr<>'' then olist.Add(rsMode+csvsep+modestr+csvsep);
        if simu then begin                         {Simulatorzeit}
          if cbSimu.Checked then
            gflt:=gflt+flt;
        end else
          gflt:=gflt+flt;                          {Gesamtzeit aufaddieren}
        gstr:=gstr+(strecke/1000);                 {Flugstrecke in km}
        fln:=fln+1;                                {nächste Flugnummer}
        flt:=flt*24;                               {Dauer in Stunden}
        if rgSpeedUnit.ItemIndex=2 then begin      {Imperial}
          olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmaxg/fft)+'ft'+csvsep);
          olist.Add(rsGridCell6+csvsep+FormatFloat(dzfl, emax/fft)+'ft'+csvsep);
          olist.Add(rsGridCell7+csvsep+FormatFloat(dzfl, strecke/fft)+'ft'+csvsep);
          olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, tasmaxg*fmph)+'mph'+csvsep);
          if flt>0 then
            olist.Add(rsAvgSpeed+csvsep+
            FormatFloat(dzfl, strecke/flt/fmile/1000)+'mph'+csvsep);
        end else begin                             {Metric}
          olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmaxg)+'m'+csvsep);
          olist.Add(rsGridCell6+csvsep+FormatFloat(dzfl, emax)+'m'+csvsep);
          olist.Add(rsGridCell7+csvsep+FormatFloat(dzfl, strecke)+'m'+csvsep);
          olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, tasmaxg*fkmh)+'km/h'+csvsep);
          if flt>0 then
            olist.Add(rsAvgSpeed+csvsep+
            FormatFloat(dzfl, strecke/flt/1000)+'km/h'+csvsep);
        end;
        olist.Add(rsRest+csvsep+FormatFloat(dzfl, umin)+'V = ~'+
        IntToStr(round(VtoProz(v_type, umin)))+'%'+csvsep);
      end else begin                               {reduzierte Ausgabe}
        if n>3 then begin                          {Ausgabe für Flüge ohne GPS}
          olist.Add(rsFlightNr+csvsep+IntToStr(fln)+csvsep);
          gridScanResult.RowCount:=fln+1;
          gridScanResult.Cells[0, fln]:=IntToStr(fln);
          gridScanResult.Cells[1, fln]:=capNachweis+suff+
                                     FormatDateTime(vzf, bg)+bind+
                                     FormatDateTime(zzf, ed);
          olist.Add(rsVType+csvsep+vtypeToStr(fmod)+csvsep);
          if simu then
            olist.Add(csvsep+rsSimulator+csvsep)
          else
            oList.Add(csvsep+rsNoGPS+csvsep);
          olist.Add(rsGridCell1+csvsep+FormatDateTime(dzf, bg)+csvsep);
          olist.Add(rsGridCell2+csvsep+FormatDateTime(zzf, bg)+csvsep);
          olist.Add(rsGridCell3+csvsep+FormatDateTime(zzf, ed)+csvsep);
          olist.Add(rsDauer+csvsep+FormatDateTime(zzf, flt)+csvsep);
          if modestr<>'' then olist.Add(rsMode+csvsep+modestr+csvsep);
          fln:=fln+1;
          gflt:=gflt+flt;
          if rgSpeedUnit.ItemIndex=2 then begin
            olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmax/fft)+'ft'+csvsep);
            olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, tasmax*fmph)+'mph'+csvsep)
          end else begin
            olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmax)+'m'+csvsep);
            olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, tasmax*fkmh)+'km/h'+csvsep);
          end;
          olist.Add(rsRest+csvsep+FormatFloat(dzfl, umin)+'V = ~'+
          IntToStr(round(VtoProz(v_type, umin)))+'%'+csvsep);
        end;
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.H501ProtoWerte(fn: string;        {Dateiname}
                            olist: TstringList;    {Datensatz per Flug/Datei}
                        var fln: integer;          {Flugnummer}
                        var gflt: TDateTime;       {Flugzeit}
                        var gstr: double);         {Flugstrecke}

var x, n, g, frme: integer;
    inlist, splitlist: TStringList;
    bg, bg1, ed, flt, dtm: TDateTime;
    hmax, h, hmaxg, u, umin, velomax, velomaxg, velo: double;
    slat, vehid: string;
    dist, ddist, lat1, lat2, lat3, lon1, lon2, lon3, emax, strecke: double;
    vld, gpsu: boolean;
    modestr: string;                               {letzter Flightmode}

const bgid=999999;

begin
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=csvsep;
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
  flt:=0;                                          {reale Flugzeit}
  ed:=0;
  dtm:=0;                                          {Date: get from Filename}
  velomax:=0;                                      {Höchstgeschwindigkeit}
  velomaxg:=0;                                     {mit GPS}
  vehid:=vtypeToStr(H501ID);                       {Hubsan vorbelegen}
  modestr:='';
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBarScan.Position:=ProgressBarScan.Position+1;
    Application.ProcessMessages;
    if inlist.count>minlines then begin            {Überschrift und mind. 10 Zeilen}
      try
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        dtm:=GetDateFromFile(copy(ExtractFileName(fn), h5file.length+1, 10)); {Date from file name}
        for x:=1 to inlist.count-1 do begin        {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>14 then begin         {Konsistenz checken}
            h:=StrToFloatN(splitlist[4]);          {Altitude}
            u:=StrToFloatN(splitlist[9]);          {LiPo in V}
            if (u<umin) then umin:=u;              {Minimum merken}
            if testh(h) then begin
              inc(n);
              ed:=dtm+ZeitToDT(splitlist[0], H501ID);
              if splitlist.Count>19 then           {Velocity}
                velo:=H501velo(StrToFloatN(splitlist[19]));
              if velo>velomax then
                velomax:=velo;                     {maximum speed}
              if bg>ed then bg:=ed;                {Beginnzeit ohne GPS}
              if bg1>ed then bg1:=ed;              {ev. neue Beginnzeit}
              if h>hmax then hmax:=h;
              if NichtLeer(splitlist[2]) and
                 NichtLeer(splitlist[3]) then begin
                inc(g);
                if velo>velomaxg then
                  velomaxg:=velo;                  {maximum speed with GPS}
                frme:=StrToIntDef(splitlist[1], 0); {frames}
                if frme>0 then vld:=true;          {valid if frames set}
                gpsu:=(frme and 1)>0;              {GPS frame available}
                if slat='' then begin
                  slat:=splitlist[2];              {Homepoint speichern}
                  lat1:=StrToFloatN(slat);
                  lon1:=StrToFloatN(splitlist[3]);
                  lat3:=lat1;
                  lon3:=lon1;
                end;
                if h>hmaxg then hmaxg:=h;
                if slat<>'' then begin             {Startpunkt mit GPS}
                  lat2:=StrToFloatN(splitlist[2]);
                  lon2:=StrToFloatN(splitlist[3]);
                  dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                  ddist:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                  if dist>emax then emax:=dist;
                  strecke:=strecke+ddist;          {Strecke aufaddieren}
                  lat3:=lat2;                      {letzten Punkt speichern}
                  lon3:=lon2;
                end;
              end;                                 {Ende mit GPS Daten}
            end;
          end else begin
            StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS; {Ende Konsistenz checken}
            AppLog.Lines.Add('''5287'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Einlesen}
        flt:=flt+ed-bg1;
        splitlist.DelimitedText:=inlist[inlist.count-1];
        tend:=dtm+ZeitToDT(splitlist[0], H501ID);  {letzten Zeitstempel merken}
      except
        StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS;
        AppLog.Lines.Add('''5295'+suff+StatusBar1.Panels[5].Text);
      end;
    end;

    if vld then begin                              {Anzeige gültiger Auswertung}
      splitlist.DelimitedText:=inlist[1];          {1. Datenzeile für fehlenden Beginn/Typ}
      if g>3 then begin                            {nur wenn Daten vorhanden sind}
        ProgressBarScan.Update;
        olist.Add(rsFlightNr+csvsep+IntToStr(fln)+csvsep);
        gridScanResult.RowCount:=fln+1;
        gridScanResult.Cells[0, fln]:=IntToStr(fln);
        gridScanResult.Cells[1, fln]:=capNachweis+suff+
                                   FormatDateTime(vzf, bg)+bind+
                                   FormatDateTime(zzf, ed);
        olist.Add(rsVType+csvsep+vehid+csvsep);
        if not gpsu
           then olist.Add(csvsep+rsNoGPS+csvsep);      {No GPS frames}
        olist.Add(rsGridCell1+csvsep+FormatDateTime(dzf, bg)+csvsep);
        olist.Add(rsGridCell2+csvsep+FormatDateTime(zzf, bg)+csvsep);
        olist.Add(rsGridCell3+csvsep+FormatDateTime(zzf, ed)+csvsep);
        olist.Add(rsDauer+csvsep+FormatDateTime(zzf, flt)+csvsep);   {Flugzeit}
        olist.Add(rsStartpkt+csvsep+                 {Startposition in GoogleMaps}
                                   URLGMap(KoToStr(lat1), KoToStr(lon1))+csvsep);
        if modestr<>'' then olist.Add(rsMode+csvsep+modestr+csvsep);
        gflt:=gflt+flt;                            {Gesamtzeit aufaddieren}
        gstr:=gstr+(strecke/1000);                 {Flugstrecke in km}
        flt:=flt*24;                               {Dauer in Stunden}
        fln:=fln+1;
        if rgSpeedUnit.ItemIndex=2 then begin      {Imperial}
          olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmaxg/fft)+'ft'+csvsep);
          olist.Add(rsGridCell6+csvsep+FormatFloat(dzfl, emax/fft)+'ft'+csvsep);
          olist.Add(rsGridCell7+csvsep+FormatFloat(dzfl, strecke/fft)+'ft'+csvsep);
          olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, velomaxg*fmph)+'mph'+csvsep);
          if flt>0 then
            olist.Add(rsAvgSpeed+csvsep+
            FormatFloat(dzfl, strecke/flt/fmile/1000)+'mph'+csvsep);
        end else begin                             {Metric}
          olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmaxg)+'m'+csvsep);
          olist.Add(rsGridCell6+csvsep+FormatFloat(dzfl, emax)+'m'+csvsep);
          olist.Add(rsGridCell7+csvsep+FormatFloat(dzfl, strecke)+'m'+csvsep);
          olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, velomaxg*fkmh)+'km/h'+csvsep);
          if flt>0 then
            olist.Add(rsAvgSpeed+csvsep+
            FormatFloat(dzfl, strecke/flt/1000)+'km/h'+csvsep);
        end;
      end else begin                               {reduzierte Ausgabe}
        if n>3 then begin                          {Ausgabe für Flüge ohne GPS}
          olist.Add(rsFlightNr+csvsep+IntToStr(fln)+csvsep);
          gridScanResult.RowCount:=fln+1;
          gridScanResult.Cells[0, fln]:=IntToStr(fln);
          gridScanResult.Cells[1, fln]:=capNachweis+suff+
                                     FormatDateTime(vzf, bg)+bind+
                                     FormatDateTime(zzf, ed);
          olist.Add(rsVType+csvsep+vehid+csvsep);
          oList.Add(csvsep+rsNoGPS+csvsep);
          olist.Add(rsGridCell1+csvsep+FormatDateTime(dzf, bg)+csvsep);
          olist.Add(rsGridCell2+csvsep+FormatDateTime(zzf, bg)+csvsep);
          olist.Add(rsGridCell3+csvsep+FormatDateTime(zzf, ed)+csvsep);
          olist.Add(rsDauer+csvsep+FormatDateTime(zzf, flt)+csvsep);
          if modestr<>'' then olist.Add(rsMode+csvsep+modestr+csvsep);
          fln:=fln+1;
          gflt:=gflt+flt;
          if rgSpeedUnit.ItemIndex=2 then begin
            olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmax/fft)+'ft'+csvsep);
            olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, velomax*fmph)+'mph'+csvsep);
          end else begin
            olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmax)+'m'+csvsep);
            olist.Add(rsGridCell8+csvsep+FormatFloat(dzfl, velomax*fkmh)+'km/h'+csvsep);
          end;
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
    vld, gpsu, nflg: boolean;
    fxmode, lfmode, modestr: string;               {letzter Flightmode}

const bgid=999999;

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
  nflg:=true;
  gpsu:=false;                                     {GPS off erkennen}
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
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBarScan.Position:=ProgressBarScan.Position+1;
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
                    dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                    ddist:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
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
            AppLog.Lines.Add('''5481'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Einlesen}
        flt:=flt+ed-bg1;
        splitlist.DelimitedText:=inlist[inlist.count-1];
        tend:=ZeitToDT(splitlist[0], brID);        {letzten Zeitstempel merken}
      except
        StatusBar1.Panels[5].Text:=fn+tab1+rsInvalid+tab1+rsDS;
        AppLog.Lines.Add('''5489'+suff+StatusBar1.Panels[5].Text);
      end;
    end;

    if vld then begin                              {Anzeige gültiger Auswertung}
      splitlist.DelimitedText:=inlist[1];          {1. Datenzeile für fehlenden Beginn/Typ}
      if g>3 then begin                            {nur wenn Daten vorhanden sind}
        ProgressBarScan.Update;
        olist.Add(rsFlightNr+csvsep+IntToStr(fln)+csvsep);
        gridScanResult.RowCount:=fln+1;
        gridScanResult.Cells[0, fln]:=IntToStr(fln);
        gridScanResult.Cells[1, fln]:=capNachweis+suff+
                                   FormatDateTime(vzf, bg)+bind+
                                   FormatDateTime(zzf, ed);
        olist.Add(rsVType+csvsep+vehid+csvsep);
        if not gpsu then olist.Add(csvsep+rsNoGPS+csvsep); {GPS off by Pilot}
        olist.Add(rsGridCell1+csvsep+FormatDateTime(dzf, bg)+csvsep);
        olist.Add(rsGridCell2+csvsep+FormatDateTime(zzf, bg)+csvsep);
        olist.Add(rsGridCell3+csvsep+FormatDateTime(zzf, ed)+csvsep);
        olist.Add(rsDauer+csvsep+FormatDateTime(zzf, flt)+csvsep);   {Flugzeit}
        olist.Add(rsStartpkt+csvsep+                 {Startposition in GoogleMaps}
                                   URLGMap(KoToStr(lat1), KoToStr(lon1))+csvsep);
        if modestr<>'' then olist.Add(rsMode+csvsep+modestr+csvsep);
        gflt:=gflt+flt;                            {Gesamtzeit aufaddieren}
        gstr:=gstr+(strecke/1000);                 {Flugstrecke in km}
        flt:=flt*24;                               {Dauer in Stunden}
        fln:=fln+1;
        if rgSpeedUnit.ItemIndex=2 then begin
          olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmaxg/fft)+'ft'+csvsep);
          olist.Add(rsGridCell6+csvsep+FormatFloat(dzfl, emax/fft)+'ft'+csvsep);
          olist.Add(rsGridCell7+csvsep+FormatFloat(dzfl, strecke/fft)+'ft'+csvsep);
          if flt>0 then
            olist.Add(rsAvgSpeed+csvsep+
            FormatFloat(dzfl, strecke/flt/fmile/1000)+'mph'+csvsep);
        end else begin
          olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmaxg)+'m'+csvsep);
          olist.Add(rsGridCell6+csvsep+FormatFloat(dzfl, emax)+'m'+csvsep);
          olist.Add(rsGridCell7+csvsep+FormatFloat(dzfl, strecke)+'m'+csvsep);
          if flt>0 then
            olist.Add(rsAvgSpeed+csvsep+
            FormatFloat(dzfl, strecke/flt/1000)+'km/h'+csvsep);
        end;
//        olist.Add(rsRest+csvsep+'~'+IntToStr(round(BrUmrech(umin)))+'%'+csvsep);
      end else begin                               {reduzierte Ausgabe}
        if n>3 then begin                          {Ausgabe für Flüge ohne GPS}
          olist.Add(rsFlightNr+csvsep+IntToStr(fln)+csvsep);
          gridScanResult.RowCount:=fln+1;
          gridScanResult.Cells[0, fln]:=IntToStr(fln);
          gridScanResult.Cells[1, fln]:=capNachweis+suff+
                                     FormatDateTime(vzf, bg)+bind+
                                     FormatDateTime(zzf, ed);
          olist.Add(rsVType+csvsep+vehid+csvsep);
          oList.Add(csvsep+rsNoGPS+csvsep);
          olist.Add(rsGridCell1+csvsep+FormatDateTime(dzf, bg)+csvsep);
          olist.Add(rsGridCell2+csvsep+FormatDateTime(zzf, bg)+csvsep);
          olist.Add(rsGridCell3+csvsep+FormatDateTime(zzf, ed)+csvsep);
          olist.Add(rsDauer+csvsep+FormatDateTime(zzf, flt)+csvsep);
          if modestr<>'' then olist.Add(rsMode+csvsep+modestr+csvsep);
          fln:=fln+1;
          gflt:=gflt+flt;
          if rgSpeedUnit.ItemIndex=2 then begin
            olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmax/fft)+'ft'+csvsep);
          end else begin
            olist.Add(rsGridCell5+csvsep+FormatFloat(dzfl, hmax)+'m'+csvsep);
          end;
//          olist.Add(rsRest+csvsep+'~'+IntToStr(round(BrUmrech(umin)))+'%'+csvsep);
        end;
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
  end;
end;

procedure TForm1.btnScanErrClick(Sender: TObject); {Probleme suchen}
var vlist, flist, inlist, splitlist: TStringList;
    i, zhl, vt: integer;
    vstr: string;

  function GetFMPos: boolean;                      {Position f_mode to gridDetails.Tag}
  begin                                            {Gibt bei Fehler true zurück}
    try
      splitlist.DelimitedText:=inlist[0];
      fModeFinden(splitlist);                      {hier wird YTH Plus schon erkannt}
      splitlist.DelimitedText:=inlist[3];          {Vehicle type bestimmen}
      vt:=StrToInt(splitlist[gridDetails.Tag+2]);  {eventuell Abbruch bei Fehler}
      if v_type<>YTHPid then                       {erkannten H Plus nicht überschreiben}
        v_type:=vt;
      OverwriteVT;
  {zusätzlicher Filter nach Vehicle Type. Vehicle type 5 ist H oder H Plus}
      result:=cbVehicleType.Checked and
              (vt<>rgVehicleType.ItemIndex+1);
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
    if inlist.Count>minlines then begin            {Datei durchsuchen}
      if GetFMPos then exit;
      case v_type of
        3: vstr:='8';                              {8 nur bei 350QX}
//        YTHPid: vstr:='??';                      {unbekannt}
        MQid: vstr:=emcyID;
        H5id: vstr:=emcyID;
      else
        vstr:='12';                                {Yuneec legacy}
      end;
      cbxSearch.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=gridDetails.Tag then
          exit;                                    {invalid data sets}
        if trim(splitlist[gridDetails.Tag])=vstr then
          inc(num);
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
    if inlist.Count>minlines then begin            {Datei durchsuchen}
      if GetFMPos then exit;
      vstr:='32';                                  {Compass Cali Warning}
      cbxSearch.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=(gridDetails.Tag+3) then exit; {zerstörte Datensätze}
        flag:=StrToIntDef(trim(splitlist[gridDetails.Tag+3]), 0);
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
    if inlist.Count>minlines then begin            {Datei durchsuchen}
      if GetFMPos then
        exit;
      vstr:='32';                                  {Compass Cali Warning}
      cbxSearch.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=(gridDetails.Tag+3) then exit; {zerstörte Datensätze}
        flag:=StrToIntDef(trim(splitlist[gridDetails.Tag+3]), 0);
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
    if inlist.Count>minlines then begin            {Datei durchsuchen}
      cbxSearch.Text:=IntToStr(stkntrl);           {Suche vordefinieren 2048}
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

  function VoltW2(const fn: string): boolean;      {Voltage warning 2}
  var k, num, flag: integer;
  begin
    num:=0;
    result:=false;
    inlist.LoadFromFile(fn);
    if inlist.Count>minlines then begin            {Datei durchsuchen}
      if GetFMPos then
        exit;
      vstr:='2';
      cbxSearch.Text:=vstr;                        {Suche vordefinieren}
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if splitlist.Count<=(gridDetails.Tag+3) then
          exit;                                    {zerstörte Datensätze}
        flag:=StrToIntDef(trim(splitlist[gridDetails.Tag+3]), 0);
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
    if inlist.Count>minlines then begin            {Datei durchsuchen}
      if GetFMPos then
        exit;
      try
        w:=StrToFloatN(cbxSearch.Text);            {Suchfeld}
      except
        w:=9.9;                                    {3S minimum V}
        cbxSearch.Text:=FloatToStr(w);
      end;
      for k:=1 to inlist.Count-1 do begin          {Nach Fehlern suchen}
        splitlist.DelimitedText:=inlist[k];
        if (splitlist.Count>gridDetails.Tag+2) and
            CheckVT(splitlist[gridDetails.Tag+2],  {zerstörte Datensätze ausblenden}
                    splitlist[gridDetails.Tag]) then begin
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
    if inlist.Count>minlines then begin            {Remote Datei durchsuchen}
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
    if inlist.Count>minlines then begin            {Datei durchsuchen}
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
        if splitlist[p]=cbxSearch.Text then begin  {Wert aus Suchfeld suchen}
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

  procedure AusgabeFileList; inline;               {Fill file list for scanned files if found something}
  begin
    inc(zhl);
    gridScanResult.RowCount:=zhl+1;
    gridScanResult.Cells[0, zhl]:=IntToStr(zhl);
    gridScanResult.Cells[1, zhl]:=flist[i];
  end;

begin
  if trim(cbxScanDir.Text)='' then
    exit;
  btnScanErr.Tag:=0;                               {Info Mode used}
    setlength(topp, 1);
  btnShowhex.Tag:=0;
  gridScanResult.RowCount:=1;                      {Tabelle löschen}
  gridScanResult.Cells[1, 0]:=capProb+bind+rsFLDir;   {Überschrift}
  ProgressBarScan.Position:=0;
  rgQuelle.ItemIndex:=0;                           {File type: Telemetry}
  zhl:=0;                                          {Trefferzähler}
  gridDetails.Tag:=DefaultPosFlightMode;           {default Position bei neuer FW ST10+}
  cbxSearch.Text:=trim(StringReplace(cbxSearch.Text, sep, '.', []));
  cbxScanDir.Text:=ExcludeTrailingPathDelimiter(cbxScanDir.Text);
  flist:=TStringList.Create;
  vlist:=TStringList.Create;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;                   {CSV zerlegen}
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  try
    FindAllDirectories(vlist, IncludeTrailingPathDelimiter(cbxScanDir.Text));
    vlist.Add(IncludeTrailingPathDelimiter(cbxScanDir.Text));
    case rgErrType.ItemIndex of
      5, 6: begin
              rgQuelle.ItemIndex:=2;               {File type: Remote}
              for i:=0 to vlist.Count-1 do SuchFile(vlist[i], ffile+wldcd+fext, flist);
            end;
      7: begin                                     {Edit/Find abh. von selektierter Spalte}
           rgQuelle.ItemIndex:=LabeledEdit1.Tag;   {File type übernehmen}
           case LabeledEdit1.Tag of
             1:   for i:=0 to vlist.Count-1 do SuchFile(vlist[i], sfile+wldcd+fext, flist);
             2:   for i:=0 to vlist.Count-1 do SuchFile(vlist[i], ffile+wldcd+fext, flist);
             3:   for i:=0 to vlist.Count-1 do SuchFile(vlist[i], nfile+wldcd+sext, flist);
             h501ID: for i:=0 to vlist.Count-1 do SuchFile(vlist[i], h5file+wldcd+fext, flist);
             else for i:=0 to vlist.Count-1 do SuchFile(vlist[i], kfile+wldcd+fext, flist);
           end;
         end;
      8, 9: for i:=0 to vlist.Count-1 do begin     {Sensor Dateien}
              if v_type=YTHPid then
                SuchFile(vlist[i], nfile+sextP, flist)
              else begin
                SuchFile(vlist[i], nfile+wldcd+sext, flist);
                SuchFile(vlist[i], wldcd+skyext, flist);
              end;
            end;
      10..12: for i:=0 to vlist.Count-1 do begin   {PX4 Sensor Dateien}
                SuchFile(vlist[i], wldcd+hext, flist);         {H520}
                Suchfile(vlist[i], nfile+wldcd+wext, flist);   {H Plus}
                Suchfile(vlist[i], mfile+wldcd+bext, flist);   {Mantis Q}
              end;
      else begin                                   {default action}
        for i:=0 to vlist.Count-1 do SuchFile(vlist[i], kfile+wldcd+fext, flist);
      end;
    end;                                           {default file type Telemetry}
    StatusBar1.Panels[0].Text:=IntToStr(vlist.Count);  {Anzahl Verzeichnisse}
    StatusBar1.Panels[1].Text:=IntToStr(flist.Count);  {Anzahl Dateien}
    AppLog.Lines.Add(LineEnding);
    AppLog.Lines.Add(rgErrType.Items[rgErrType.ItemIndex]);
    AppLog.Lines.Add(cbxScanDir.Text);
    AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDateien);
    gridScanResult.BeginUpdate;
    if flist.Count>0 then begin                    {Dateien vorhanden}
      flist.Sort;
      ProgressBarScan.Max:=flist.Count;
      for i:=0 to flist.Count-1 do begin
        case rgErrType.ItemIndex of
          0: if Emergency12(flist[i]) then AusgabeFileList;
          1: if CCWNum(flist[i]) then AusgabeFileList;
          2: if CCWTime(flist[i]) then AusgabeFileList;
          3: if VoltW2(flist[i]) then AusgabeFileList;
          4: if VoltMin(flist[i]) then AusgabeFileList;
          5: if StickCali(flist[i]) then AusgabeFileList;  {Sticks in Remote}
          6: if TeamMode(flist[i]) then AusgabeFileList;   {Suche nach 1433.0 in Remote}
          7: if EditFind(flist[i]) then AusgabeFileList;
          8: if FileSize(flist[i])>FWsz then AusgabeFileList;  {Sensorfile > 6 Byte}
          9: if FindSensorFW(flist[i]) then AusgabeFileList;   {Sensorfile mit FW}
          10: if ShowSensorPlus(flist[i], 0, false, false, true, false) then
                AusgabeFileList;
          11: if ShowSensorPlus(flist[i], 0, false, false, false, true) then
                AusgabeFileList;
        end;
        ProgressBarScan.Position:=i;
        Application.ProcessMessages;
      end;
    end;
    gridScanResult.Cells[0, 0]:=rsNum+' ('+IntToStr(zhl)+')';  {Überschrift}
    gridScanResult.EndUpdate;
    if zhl>0 then begin                            {Anzahl Treffer}
      StatusBar1.Panels[5].Text:=IntToStr(zhl)+rsSuspect;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      cbxScanDir.Text:=ExcludeTrailingPathDelimiter(cbxScanDir.Text);
      MerkListe(cbxScanDir, speItems.Value);
    end else begin
      StatusBar1.Panels[5].Text:=rsNoFound;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    ProgressBarScan.Position:= ProgressBarScan.Max;
    if rgErrType.ItemIndex=11 then                 {Go to AppLogHighlighter}
      pcMain.ActivePage:=tabApplog;
  finally
    FreeAndNil(flist);
    FreeAndNil(vlist);
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.btnSaveApplogClick(Sender: TObject);   {AppLogHighlighter speichern}
begin
  SaveDialog1.Title:=rsFileSave;
  SaveDialog1.FileName:=ComplFN(tabAppLog.Caption, now)+wext;
  if SaveDialog1.Execute then begin
    AppLog.Lines.SaveToFile(SaveDialog1.FileName);
    AppLog.Lines.Clear;
    StatusBar1.Panels[5].Text:=tabAppLog.Caption+tab1+rsSaved+
                               suff+SaveDialog1.FileName;
  end;
end;

procedure TForm1.btnDelAppLogClick(Sender: TObject);   {Lösche AppLogHighlighter}
begin
  AppLog.Lines.Clear;
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
    outlist.Add(LineEnding);
    outlist.Add(tabs(rsFlightReport, suff, tabu)+cbxText.text);  {ggf. Seriennummer}
    outlist.Add(tabs(rsCreat+tab1, suff, tabu)+
                FormatDateTime(mzf, now)+'h  '+tab1+rsBy+tab1+
                AppName+tab2+AppVersion);
    if cbCleanHplus.Checked then
      outlist.Add(capCheckBox9);
    outlist.Add(LineEnding);
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
    outlist.Add(LineEnding);
    outlist.Add(trenner+trenner+trenner);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(tabs(rsGFtime, suff, tabu)+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(tabs(rsGFtime, suff, tabu)+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if rgSpeedUnit.ItemIndex=2 then begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FormatFloat(dzfl, gdist/fmile)+'mi');
      end else begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FormatFloat(dzfl, gdist)+'km');
      end;
    prtext:=wexdef;                                {speichern als Text}
  end;

  procedure CSVAusgabe;                            {Flugprotokoll als CSV Datei}
  var csvlist: array [0..15] of string;
      x, y: integer;
      s: string;
  begin
    outlist.Add(capNachweis);
    outlist.Add(LineEnding);
    s:=rsCreat+tab1+csvsep+FormatDateTime(mzf, now)+'h'+csvsep+
       tab1+rsBy+tab1+AppName+tab2+AppVersion;
    if cbCleanHplus.Checked then s:=s+csvsep+capCheckBox9;
    outlist.Add(s);
    outlist.Add(LineEnding);
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
    for y:=low(csvlist)+1 to high(csvlist) do
      prtext:=prtext+csvsep+csvlist[y];
    outlist.Add(prtext);
    for y:=low(csvlist) to high(csvlist) do
      csvlist[y]:=''; {Array löschen}
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then begin            {neuer Datensatz}
        if csvlist[1]<>'' then begin               {alle außer 1.}
          prtext:=csvlist[0];
          for y:=low(csvlist)+1 to high(csvlist) do begin
            prtext:=prtext+csvsep+csvlist[y];
            csvlist[y]:='';                        {Array löschen}
          end;
          outlist.Add(prtext);
        end;
        csvlist[0]:=flist[1];
      end;
      if flist[0]=rsVType then
        csvlist[1]:=flist[1];
      if flist[0]='' then
        csvlist[2]:=flist[1];
      if flist[0]=rsGridCell1 then
        csvlist[3]:=flist[1];
      if flist[0]=rsGridCell2 then
        csvlist[4]:=flist[1];
      if flist[0]=rsGridCell3 then
        csvlist[5]:=flist[1];
      if flist[0]=rsDauer then
        csvlist[6]:=flist[1];
      if flist[0]=rsStartpkt then
        csvlist[7]:=flist[1];
      if flist[0]=rsAdresse then
        csvlist[8]:=flist[1];
      if flist[0]=rsMode then
        csvlist[9]:=flist[1];
      if flist[0]=rsGridCell5 then
        csvlist[10]:=flist[1];
      if flist[0]=rsGridCell6 then
        csvlist[11]:=flist[1];
      if flist[0]=rsGridCell7 then
        csvlist[12]:=flist[1];
      if flist[0]=rsGridCell8 then
        csvlist[13]:=flist[1];
      if flist[0]=rsAvgSpeed then
        csvlist[14]:=flist[1];
      if flist[0]=rsRest then
        csvlist[15]:=flist[1];
    end;
    prtext:=csvlist[0];                            {letzten DS ausgeben}
    for y:=low(csvlist)+1 to high(csvlist) do
      prtext:=prtext+csvsep+csvlist[y];
    outlist.Add(prtext);
    outlist.Add(LineEnding);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(rsGFtime+csvsep+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(rsGFtime+csvsep+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if rgSpeedUnit.ItemIndex=2 then begin
        outlist.Add(rsGFstr+csvsep+FormatFloat(dzfl, gdist/fmile)+'mi');
      end else begin
        outlist.Add(rsGFstr+csvsep+FormatFloat(dzfl, gdist)+'km');
      end;
    prtext:=csvdef;                                {speichern als CSV}
  end;

begin            {ganzes Verzeichnis durchsuchen nach Telemetry_*.csv}
  if (v_type=brID) or
     (v_type=H501ID) then exit;
  if cbxScanDir.Items.Count>0 then
    for x:=cbxScanDir.Items.Count-1 downto 0 do    {Liste putzen}
      if not DirectoryExists(cbxScanDir.Items[x]) then cbxScanDir.Items.Delete(x);
  if (cbxScanDir.Text<>'') and
     (DirectoryExists(cbxScanDir.Text)) then begin
    cbxScanDir.Text:=ExcludeTrailingPathDelimiter(cbxScanDir.Text);
    MerkListe(cbxScanDir, speItems.Value);
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    vlist:=TStringList.Create;
    flist:=TStringList.Create;
    outlist:=TStringList.Create;
    flno:=1;
    gftime:=0;                                     {Gesamt Flugzeit}
    gdist:=0;                                      {gesamt Strecke, nur bei GPS}
    datpos:=0;
    try
      FindAllDirectories(vlist, IncludeTrailingPathDelimiter(cbxScanDir.Text));
      vlist.Add(IncludeTrailingPathDelimiter(cbxScanDir.Text));
      for x:=0 to vlist.Count-1 do
        SuchFile(vlist[x], kfile+wldcd+fext, flist);
      if flist.Count>1 then begin                  {genug Dateien?}
        StatusBar1.Panels[0].Text:=IntToStr(vlist.Count);  {Anzahl Verzeichnisse}
        StatusBar1.Panels[1].Text:=IntToStr(flist.Count);  {Anzahl Telemetrie}
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+rsTurns);
        StatusBar1.Update;
        vlist.Clear;  {brauchen wir hier nicht mehr, wird csv für Ausgabe benutzt}
        flist.Sort;
        ProgressBarScan.Max:=flist.Count;
        gridScanResult.BeginUpdate;
        for x:=0 to flist.Count-1 do               {Dateien abarbeiten}
          ProtoWerte(flist[x], vlist, flno, gftime, gdist);
        gridScanResult.Cells[0,0]:=rsNum+'('+IntToStr(flno-1)+')';
        gridScanResult.EndUpdate;
        gfd:=Trunc(gftime);                        {Anzahl Tage der Flugzeit}
        flist.Clear;  {brauchen wir hier nicht mehr, wird splitlist}
        flist.Delimiter:=csvsep;                   {Semicolon als Trenner}
        flist.StrictDelimiter:=True;               {keine Spaces als Trenner}
        case rgCSVtext.ItemIndex of                {Ausgabeformat}
          0: TextAusgabe;
          1: CSVAusgabe;
        end;
        StatusBar1.Panels[5].Text:=rsGFtime+suff;
        if gfd=0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                                     FormatDateTime(zzf, gftime)+'h';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                            IntToStr(gfd)+'d '+FormatDateTime(zzf, gftime)+'h';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        if gdist>0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+' / '+rsGFstr+suff;
          if rgSpeedUnit.ItemIndex=2 then begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FormatFloat(dzfl, gdist/fmile)+'mi';
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end else begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FormatFloat(dzfl, gdist)+'km';
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end;
        end;
        Screen.Cursor:=crDefault;
        SaveDialog1.Title:=rsProtSave;
        SaveDialog1.InitialDir:=cbxScanDir.Text;
        SaveDialog1.FileName:=CleanDN(capNachweis+prtext);
        if SaveDialog1.Execute then begin
          ProgressBarScan.Position:=0;
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
    AppLog.Lines.Add('''6175'+suff+StatusBar1.Panels[5].Text);
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
    outlist.Add(LineEnding);
    outlist.Add(tabs(rsFlightReport, suff, tabu)+cbxText.text); {ggf. Seriennummer}
    outlist.Add(tabs(rsCreat+tab1, suff, tabu)+
                FormatDateTime(mzf, now)+'h  '+tab1+rsBy+tab1+
                AppName+tab2+AppVersion);
    outlist.Add(LineEnding);
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
    outlist.Add(LineEnding);
    outlist.Add(trenner+trenner+trenner);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(tabs(rsGFtime, suff, tabu)+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(tabs(rsGFtime, suff, tabu)+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if rgSpeedUnit.ItemIndex=2 then begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FormatFloat(dzfl, gdist/fmile)+'mi');
      end else begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FormatFloat(dzfl, gdist)+'km');
      end;
    prtext:=wexdef;                                {speichern als Text}
  end;

  procedure CSVAusgabe;                            {Flugprotokoll als CSV Datei}
  var csvlist: array [0..15] of string;
      x, y: integer;
  begin
    outlist.Add(capNachweis);
    outlist.Add(LineEnding);
    outlist.Add(rsCreat+tab1+csvsep+FormatDateTime(mzf, now)+'h'+csvsep+tab1+rsBy+tab1+
                AppName+tab2+AppVersion);
    outlist.Add(LineEnding);
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
    csvlist[13]:=rsN_A;
    csvlist[14]:=rsAvgSpeed;
    csvlist[15]:=rsRest;
    prtext:=csvlist[0];
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+csvsep+csvlist[y];
    outlist.Add(prtext);
    for y:=low(csvlist) to high(csvlist) do csvlist[y]:=''; {Array löschen}
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then begin            {neuer Datensatz}
        if csvlist[1]<>'' then begin               {alle außer 1.}
          prtext:=csvlist[0];
          for y:=low(csvlist)+1 to high(csvlist) do begin
            prtext:=prtext+csvsep+csvlist[y];
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
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+csvsep+csvlist[y];
    outlist.Add(prtext);
    outlist.Add(LineEnding);
    outlist.Add(LineEnding);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(rsGFtime+csvsep+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(rsGFtime+csvsep+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if rgSpeedUnit.ItemIndex=2 then begin
        outlist.Add(rsGFstr+csvsep+FormatFloat(dzfl, gdist/fmile)+'mi');
      end else begin
        outlist.Add(rsGFstr+csvsep+FormatFloat(dzfl, gdist)+'km');
      end;
    prtext:=csvdef;                                {speichern als CSV}
  end;

begin            {ganzes Verzeichnis durchsuchen nach Telemetry_*.csv}
  if v_type=H501ID then begin
    H501FlugBuch;
    exit;
  end;
  if cbxScanDir.Items.Count>0 then
    for x:=cbxScanDir.Items.Count-1 downto 0 do    {Liste putzen}
      if not DirectoryExists(cbxScanDir.Items[x]) then
        cbxScanDir.Items.Delete(x);
  if (cbxScanDir.Text<>'') and
     (DirectoryExists(cbxScanDir.Text)) then begin
    cbxScanDir.Text:=ExcludeTrailingPathDelimiter(cbxScanDir.Text);
    MerkListe(cbxScanDir, speItems.Value);
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    vlist:=TStringList.Create;
    flist:=TStringList.Create;
    outlist:=TStringList.Create;
    flno:=1;
    gftime:=0;                                     {Gesamt Flugzeit}
    gdist:=0;                                      {gesamt Strecke, nur bei GPS}
    datpos:=0;
    try
      FindAllDirectories(vlist, IncludeTrailingPathDelimiter(cbxScanDir.Text));
      vlist.Add(IncludeTrailingPathDelimiter(cbxScanDir.Text));
      for x:=0 to vlist.Count-1 do
        SuchFile(vlist[x], wldcd+bext, flist);
      if flist.Count>1 then begin                  {genug Dateien?}
        StatusBar1.Panels[1].Text:=IntToStr(flist.Count); {Anzahl BrTelemetrie}
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+rsTurns);
        StatusBar1.Panels[0].Text:=IntToStr(vlist.Count); {Anzahl Verzeichnisse}
        StatusBar1.Update;
        vlist.Clear;  {brauchen wir hier nicht mehr, wird csv für Ausgabe}
        flist.Sort;
        ProgressBarScan.Max:=flist.Count;
        gridScanResult.BeginUpdate;
        for x:=0 to flist.Count-1 do               {Dateien abarbeiten}
          BrProtoWerte(flist[x], vlist, flno, gftime, gdist);
        gridScanResult.Cells[0,0]:=rsNum+'('+IntToStr(flno-1)+')';
        gridScanResult.EndUpdate;
        gfd:=Trunc(gftime);                        {Anzahl Tage der Flugzeit}
        flist.Clear;  {brauchen wir hier nicht mehr, wird splitlist}
        flist.Delimiter:=csvsep;                   {Semicolon als Trenner}
        flist.StrictDelimiter:=True;               {keine Spaces als Trenner}
        case rgCSVtext.ItemIndex of                {Ausgabeformat}
          0: TextAusgabe;
          1: CSVAusgabe;
        end;
        StatusBar1.Panels[5].Text:=rsGFtime+suff;
        if gfd=0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                                     FormatDateTime(zzf, gftime)+'h';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                            IntToStr(gfd)+'d '+FormatDateTime(zzf, gftime)+'h';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        if gdist>0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+' / '+rsGFstr+suff;
          if rgSpeedUnit.ItemIndex=2 then begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FormatFloat(dzfl, gdist/fmile)+'mi';
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end else begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FormatFloat(dzfl, gdist)+'km';
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end;
        end;
        Screen.Cursor:=crDefault;
        SaveDialog1.Title:=rsProtSave;
        SaveDialog1.InitialDir:=cbxScanDir.Text;
        SaveDialog1.FileName:=CleanDN(capNachweis+'YBr'+prtext);
        if SaveDialog1.Execute then begin
          ProgressBarScan.Position:=0;
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
    AppLog.Lines.Add('''6387'+suff+StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.H501FlugBuch;                     {Hubsan H501}
var vlist, flist, outlist: TStringList;
    x, flno, gfd, datpos: integer;
    prtext: string;
    gftime: TDateTime;
    gdist: Double;

  procedure TextAusgabe;                           {Flugprotokoll als Textdatei}
  var x: integer;
  begin
    outlist.Add(trenner+tab1+capNachweis+tab1+trenner);
    outlist.Add(LineEnding);
    outlist.Add(tabs(rsFlightReport, suff, tabu)+cbxText.text); {ggf. Seriennummer}
    outlist.Add(tabs(rsCreat+tab1, suff, tabu)+
                FormatDateTime(mzf, now)+'h  '+tab1+rsBy+tab1+
                AppName+tab2+AppVersion);
    outlist.Add(LineEnding);
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
    outlist.Add(LineEnding);
    outlist.Add(trenner+trenner+trenner);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(tabs(rsGFtime, suff, tabu)+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(tabs(rsGFtime, suff, tabu)+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if rgSpeedUnit.ItemIndex=2 then begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FormatFloat(dzfl, gdist/fmile)+'mi');
      end else begin
        outlist.Add(tabs(rsGFstr, suff, tabu)+FormatFloat(dzfl, gdist)+'km');
      end;
    prtext:=wexdef;                                {speichern als Text}
  end;

  procedure CSVAusgabe;                            {Flugprotokoll als CSV Datei}
  var csvlist: array [0..15] of string;
      x, y: integer;
  begin
    outlist.Add(capNachweis);
    outlist.Add(LineEnding);
    outlist.Add(rsCreat+tab1+csvsep+FormatDateTime(mzf, now)+'h'+csvsep+tab1+rsBy+tab1+
                AppName+tab2+AppVersion);
    outlist.Add(LineEnding);
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
    csvlist[13]:=rsN_A;
    csvlist[14]:=rsAvgSpeed;
    csvlist[15]:=rsRest;
    prtext:=csvlist[0];
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+csvsep+csvlist[y];
    outlist.Add(prtext);
    for y:=low(csvlist) to high(csvlist) do csvlist[y]:=''; {Array löschen}
    for x:=0 to vlist.count-1 do begin             {Textausgabe}
      flist.DelimitedText:=vlist[x];
      if flist[0]=rsFlightNr then begin            {neuer Datensatz}
        if csvlist[1]<>'' then begin               {alle außer 1.}
          prtext:=csvlist[0];
          for y:=low(csvlist)+1 to high(csvlist) do begin
            prtext:=prtext+csvsep+csvlist[y];
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
    for y:=low(csvlist)+1 to high(csvlist) do prtext:=prtext+csvsep+csvlist[y];
    outlist.Add(prtext);
    outlist.Add(LineEnding);
    outlist.Add(LineEnding);
    if gfd=0 then                                  {ohne Tage}
      outlist.Add(rsGFtime+csvsep+FormatDateTime(zzf, gftime)+'h')
    else                                           {mit Tagen}
      outlist.Add(rsGFtime+csvsep+IntToStr(gfd)+'d '+
                  FormatDateTime(zzf, gftime)+'h');
    if gdist>0 then
      if rgSpeedUnit.ItemIndex=2 then begin
        outlist.Add(rsGFstr+csvsep+FormatFloat(dzfl, gdist/fmile)+'mi');
      end else begin
        outlist.Add(rsGFstr+csvsep+FormatFloat(dzfl, gdist)+'km');
      end;
    prtext:=csvdef;                                {speichern als CSV}
  end;

begin            {ganzes Verzeichnis durchsuchen nach H501_*.csv}
  if cbxScanDir.Items.Count>0 then
    for x:=cbxScanDir.Items.Count-1 downto 0 do     {Liste putzen}
      if not DirectoryExists(cbxScanDir.Items[x]) then
        cbxScanDir.Items.Delete(x);
  if (cbxScanDir.Text<>'') and
     (DirectoryExists(cbxScanDir.Text)) then begin
    cbxScanDir.Text:=ExcludeTrailingPathDelimiter(cbxScanDir.Text);
    MerkListe(cbxScanDir, speItems.Value);
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    vlist:=TStringList.Create;
    flist:=TStringList.Create;
    outlist:=TStringList.Create;
    flno:=1;
    gftime:=0;                                     {Gesamt Flugzeit}
    gdist:=0;                                      {gesamt Strecke, nur bei GPS}
    datpos:=0;
    try
      FindAllDirectories(vlist, IncludeTrailingPathDelimiter(cbxScanDir.Text));
      vlist.Add(IncludeTrailingPathDelimiter(cbxScanDir.Text));
      for x:=0 to vlist.Count-1 do
        SuchFile(vlist[x], h5file+wldcd+fext, flist);
      StatusBar1.Panels[1].Text:=IntToStr(flist.Count); {Anzahl H501 files}
      if flist.Count>1 then begin                  {genug Dateien?}
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+rsTurns);
        StatusBar1.Panels[0].Text:=IntToStr(vlist.Count); {Anzahl Verzeichnisse}
        StatusBar1.Update;
        vlist.Clear;  {brauchen wir hier nicht mehr, wird csv für Ausgabe}
        flist.Sort;
        ProgressBarScan.Max:=flist.Count;
        gridScanResult.BeginUpdate;
        for x:=0 to flist.Count-1 do               {Dateien abarbeiten}
          H501ProtoWerte(flist[x], vlist, flno, gftime, gdist);
        gridScanResult.Cells[0,0]:=rsNum+'('+IntToStr(flno-1)+')';
        gridScanResult.EndUpdate;
        gfd:=Trunc(gftime);                        {Anzahl Tage der Flugzeit}
        flist.Clear;  {brauchen wir hier nicht mehr, wird splitlist}
        flist.Delimiter:=csvsep;                   {Semicolon als Trenner}
        flist.StrictDelimiter:=True;               {keine Spaces als Trenner}
        case rgCSVtext.ItemIndex of                {Ausgabeformat}
          0: TextAusgabe;
          1: CSVAusgabe;
        end;
        StatusBar1.Panels[5].Text:=rsGFtime+suff;
        if gfd=0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                                     FormatDateTime(zzf, gftime)+'h';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                            IntToStr(gfd)+'d '+FormatDateTime(zzf, gftime)+'h';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        if gdist>0 then begin
          StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+' / '+rsGFstr+suff;
          if rgSpeedUnit.ItemIndex=2 then begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FormatFloat(dzfl, gdist/fmile)+'mi';
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end else begin
            StatusBar1.Panels[5].Text:=StatusBar1.Panels[5].Text+
                       FormatFloat(dzfl, gdist)+'km';
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end;
        end;
        Screen.Cursor:=crDefault;
        SaveDialog1.Title:=rsProtSave;
        SaveDialog1.InitialDir:=cbxScanDir.Text;
        SaveDialog1.FileName:=CleanDN(capNachweis+'H501'+prtext);
        if SaveDialog1.Execute then begin
          ProgressBarScan.Position:=0;
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
    AppLog.Lines.Add('''7593'+suff+StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.btnFlugBuchClick(Sender: TObject); {Flugbuch erstellen}
begin
  Merkliste(cbxText, speItems.Value);
  gridScanResult.RowCount:=1;                      {Tabelle löschen}
  gridScanResult.Cells[1, 0]:=capNachweis;         {Überschrift}
  ProgressBarScan.Position:=0;
  btnScanErr.Tag:=1;
  YFlugBuch;
  BrFlugBuch;                                      {noch Breeze und H501 versuchen}
end;

procedure TForm1.cbPilotChange(Sender: TObject);   {Pilotenpfad geändert}
begin
  EnSave;                                          {Speichern erlauben}
end;

procedure TForm1.PlatformLesen;          {Headerdaten aus Breeze anzeigen}
var fn: string;
    inlist: TStringlist;
    x, k: integer;
    s1, s2: string;
begin
  fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
      lbFlights.Items[lbFlights.ItemIndex]+bext;
  inlist:=TStringList.Create;
  try
    if FileExists(fn)then begin
      inlist.LoadFromFile(fn);
      AppLog.Lines.Add(LineEnding);
      if pos(plfAndr, inlist[2])>0 then
        btnArchive.Tag:=1
      else
        btnArchive.Tag:=0;
      for x:=1 to 5 do begin
        s1:='';
        s2:=inlist[x];
        AppLog.Lines.Add(s2);
        if s2.length>3 then begin
          for k:=1 to s2.length do begin
            if s2[k]<>':' then
              s1:=s1+s2[k]
            else
              break;
          end;
          s2:=copy(s2, k+1, s2.length-k);
          gridFirmware.Cells[0, x]:=s1;
          gridFirmware.Cells[1, x]:=s2;
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
  if pcSettings3.ActivePage=tabCommon then begin
    gridFirmware.Cells[0, 1]:=rsKamera;
    gridFirmware.Cells[0, 2]:=rsGimbal;
    gridFirmware.Cells[0, 3]:=rsAutoP;
    gridFirmware.Cells[0, 4]:='?';
    gridFirmware.Cells[0, 5]:=rsRealSense;
    az:=GetFW(FWarr);
    if az>2 then begin
      AppLog.Lines.Add(LineEnding);
      for i:=0 to az do begin
        gridFirmware.Cells[1, i+1]:=FWarr[i];
        if FWarr[i]<>'' then
          AppLog.Lines.Add(gridFirmware.Cells[0, i+1]+suff+FWArr[i]);
      end;
    end;
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

{Reset zoom by mouseclick and Ctrl key of middle mouse key for all 3 charts}
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
               ZeitToDT(gridDetails.Cells[0, speDataPoint.Value], v_type));
    StatusBar1.Panels[4].Text:=FormatDateTime(zzf,
               ZeitToDT(gridDetails.Cells[0, abst], v_type));
    if pos(capTabSheet10, StatusBar1.Panels[5].Text)<1 then begin
      StatusBar1.Panels[5].Text:=gridDetails.Cells[10, speDataPoint.Value]+'cm - '+
                 fmode+'='+gridDetails.Cells[2, speDataPoint.Value]+tab1+
                 brFModeToStr(StrToInt(gridDetails.Cells[gridDetails.Tag, speDataPoint.Value]));
    end;
  end;

  procedure AnzeigeYLegacy;
  begin
    StatusBar1.Panels[3].Text:=FormatDateTime(zzf+zzz,
               ZeitToDT(gridDetails.Cells[0, speDataPoint.Value], v_type));
    StatusBar1.Panels[4].Text:=FormatDateTime(zzf+zzz,
               ZeitToDT(gridDetails.Cells[0, abst], v_type));
    if pos(capTabSheet10, StatusBar1.Panels[5].Text)<1 then begin
      StatusBar1.Panels[5].Text:=gridDetails.Cells[2, speDataPoint.Value]+'V - '+
                 gridDetails.Cells[4, speDataPoint.Value]+'m - '+fmode+'='+
                 gridDetails.Cells[gridDetails.Tag, speDataPoint.Value]+tab1+
                 FModeToStr(StrToInt(gridDetails.Cells[gridDetails.Tag, speDataPoint.Value]));
    end;
  end;

  procedure AnzeigeH501;                           {ToDo}
  begin
    StatusBar1.Panels[3].Text:=FormatDateTime(zzf+zzz,
               ZeitToDT(gridDetails.Cells[0, speDataPoint.Value], v_type));
    StatusBar1.Panels[4].Text:=FormatDateTime(zzf+zzz,
               ZeitToDT(gridDetails.Cells[0, abst], v_type));
    if pos(capTabSheet10, StatusBar1.Panels[5].Text)<1 then begin
      StatusBar1.Panels[5].Text:=gridDetails.Cells[9, speDataPoint.Value]+'V - '+
                 gridDetails.Cells[4, speDataPoint.Value]+'m';
    end;
  end;

begin
  if ChartToolset2DataPointCrosshairTool1.Enabled then begin
    idx:=ASender.PointIndex+1;
    if (idx>1) and                                 {im gültigen Bereich}
       (idx<gridDetails.RowCount) then
      speDataPoint.Value:=idx;                     {Datenindex übernehmen}
    abst:=speDataPoint.Value+speNumPoints.Value;     {Ende des Intervalls}

    try                                            {Versuchen, zweiten Kursor zu setzen}
      Chart1ConstantLine1.Position:=Chart1LineSeries1.XValue[abst];
      Chart1ConstantLine1.Active:=true;            {Anzeige Ende Intervall}
    except
      Chart1ConstantLine1.Active:=false;
    end;
    if abst>gridDetails.RowCount-2 then            {Überlauf vermeiden}
      abst:=speDataPoint.Value;

    case v_type of
      brID: AnzeigeBreeze;                         {Breeze}
      H501ID: AnzeigeH501;                         {flaretom log recorder}
    else
      AnzeigeYLegacy;                              {legacy Yuneec}
    end;
  end;
end;

procedure TForm1.cbExtrudeChange(Sender: TObject); {Extrude für KML geändert}
begin
  if cbExtrude.Checked then
    rgAltitudeType.ItemIndex:=1;                   {Set relative to ground}
  EnSave;
end;

procedure TForm1.cbVehicleTypeChange(Sender: TObject); {Filteranzeige}
begin
  rgVehicleType.Enabled:=cbVehicleType.Checked;
end;

function nahe(lat1, lon1, lat2, lon2: double): boolean; {Wert in der Nähe}
begin
  result:=(abs(lat1-lat2)<nhw) and (abs(lon1-lon2)<nhw);
end;

procedure TForm1.cbDashwChange(Sender: TObject);   {Dashware geändert}
begin
  EnSave;                                          {Speichern erlauben}
end;

procedure TForm1.ColorButton1Click(Sender: TObject);
begin                     {Erneut Speichern erlauben, wenn Farbe geändert wurde}
  EnSave;
end;

procedure TForm1.btnDefaultProfileClick(Sender: TObject);   {Defaulteinstellung}
begin                     {Reset Schnellanalyse für alle drei Histogramme}
  TimerDiashow.Enabled:=false;
  SetProfile(0);
  cbxProfiles.ItemIndex:=0;                        {Profiles rücksetzen}
end;

procedure TForm1.btnDeleteLnClick(Sender: TObject);
var
  zhl: integer;
  dlist, outlist: TStringList;
  fn: string;

  procedure CleanLn(fn1: string);
  var
    i: integer;

  begin
    dlist.LoadFromFile(fn1);
    outlist.Clear;
    outlist.Add(dlist[0]);
    for i:=1 to dlist.Count-1 do begin
      if pos(edDeleteLn.Text, dlist[i])=1 then begin
        inc(zhl);
      end else begin
        outlist.Add(dlist[i]);
      end;
    end;
    if outlist.Count>1 then begin
      outlist.SaveToFile(fn1);
      dlist.SaveToFile(ChangeFileExt(fn1, bakext));
    end;
  end;

begin
  zhl:=0;
  OpenDialog1.Title:=capDeleteLn;
  OpenDialog1.InitialDir:=cbxLogDir.Text;
  if edDeleteLn.text<>'' then begin
    if OpenDialog1.Execute then begin
      dlist:=TStringList.Create;
      outlist:=TStringList.Create;
      try
        CleanLn(OpenDialog1.FileName);
{Try the other flightlog files if legacy Yuneec telemetry file was selected as first file}
        if pos(dkpath+PathDelim+kfile, OpenDialog1.FileName)>1 then begin
          fn:=ReplaceText(OpenDialog1.FileName, dkpath+PathDelim+kfile, fpath+PathDelim+ffile);
          if FileExists(fn) then
            CleanLn(fn);
          fn:=ReplaceText(OpenDialog1.FileName, dkpath+PathDelim+kfile, spath+PathDelim+sfile);
          if FileExists(fn) then
            CleanLn(fn);
        end;
        StatusBar1.Panels[0].Text:=IntToStr(zhl);
      finally
        dlist.Free;
        outlist.Free;
      end;
    end;
  end;
end;

procedure TForm1.lblGitHubClick(Sender: TObject);      {Open GitHub repo}
begin
  if OpenURL(lblGitHub.Hint) then
    lblGitHub.Font.Color:=clPurple;
end;

procedure TForm1.lblGitHubMouseEnter(Sender: TObject); {Anitated GitHub link}
begin
  lblGitHub.Font.Style:=lblGitHub.Font.Style+[fsBold];
end;

procedure TForm1.lblGitHubMouseLeave(Sender: TObject);
begin
  lblGitHub.Font.Style:=lblGitHub.Font.Style-[fsBold];
end;

procedure TForm1.mnAutoCutClick(Sender: TObject);
begin
  CutLegacy(1);                                    {AutoCut}
end;

procedure TForm1.mnDownloadClick(Sender: TObject);
begin
  CheckVersion;
end;

{Profiles sind von mir häufig benutzte Einstellungen für die Schnellanalyse.
 Sie sind hier hart codiert und vom Benutzer nicht editierbar.}

procedure TForm1.cbxProfilesChange(Sender: TObject);
begin                                              {Profile selected}
  if cbxProfiles.ItemIndex>0 then
    SetProfile(cbxProfiles.ItemIndex);
end;

procedure TForm1.SetProfile(idx: integer);         {Profile selected}

  procedure DefaultCl;
  begin
    cbxProfiles.ItemIndex:=0;
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

  procedure ProfileH501;                           {Für Hubsan nur default}
  begin
    LabeledEdit1.Tag:=H501ID;
    LabeledEdit2.Tag:=H501ID;
    LabeledEdit3.Tag:=H501ID;
    LabeledEdit1.Text:='Heading';
    LabeledEdit2.Text:='Roll';
    LabeledEdit3.Text:='Pitch';
    DefaultCl;
  end;

  procedure ProfileYTHPlus;
  begin
    case idx of
      0: begin      {Default Button: Reset Schnellanalyse für alle drei Histogramme}
           LabeledEdit1.Text:='fskRssi';
           LabeledEdit2.Text:=csvVolt;
           LabeledEdit3.Text:='gpsAccH';
           DefaultCl;
         end;
  {** Hier Profiles editieren ** :: ** Profiles auch in PopupMenuProfile **}
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
  {** Hier Profiles editieren ** :: ** Profiles auch in PopupMenuProfile **}
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
  if (v_type=MQid) or
     (v_type=H5id) then
       exit;
  LabeledEdit1.Tag:=0;                             {Tag=0: Telemetry (Default)}
  LabeledEdit2.Tag:=0;
  LabeledEdit3.Tag:=0;
  case v_type of
    brID: ProfileBreeze;
    h501ID: ProfileH501;
    YTHPid: ProfileYTHPlus;
  else
    ProfileYLegacy;
  end;
  pcMain.ActivePage:=tabAnalyze3;                  {Umschalten auf Schnellanalyse}
  Anzeige;                                         {Schnellanalyse ausführen}
  if TimerDiashow.Enabled and
     (idx>0) then begin
    StatusBar1.Panels[5].Text:=rsProfile+suff+cbxProfiles.Items[idx];
    AppLog.Lines.Add(StatusBar1.Panels[5].Text);
  end;
end;

procedure TForm1.cbxProfilesDblClick(Sender: TObject);
begin
  pcMain.ActivePage:=tabAnalyze3;                  {Umschalten auf Schnellanalyse}
  Anzeige;
end;

procedure TForm1.cbxLogDirDblClick(Sender: TObject);
begin                                              {Verzeichnis öffnen}
//  OpenDocument(IncludeTrailingPathDelimiter(cbxLogDir.Text));

  OpenDocument(cbxLogDir.Text);
end;

procedure TForm1.cbxLogDirMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Liste löschen}
begin
  if ssCtrl in Shift then
    cbxLogDir.Items.Clear;
end;

procedure TForm1.mnFlDelClick(Sender: TObject);    {Delete selected FlightLog}
var nf: integer;                                   {Number files deleted}

  function DeleteOneFile(fnx: string): boolean;    {Delete file with file name fn}
  begin
    result:=false;
    if FileExists(fnx) then begin
      if DeleteFile(fnx) then begin
        result:=true;                              {Successful}
        inc(nf);                                   {Number files deleted +1}
      end;
    end;
  end;

  procedure DelMQ;                                 {Mantis Q}
  begin
                                                   {Sensor_*.txt vom Mantis Q}
    if not DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                         nfile+lbFlights.Items[lbFlights.ItemIndex]+wext) then
                                                   {alternativ yuneec_*.log file}
      DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                    mfile+lbFlights.Items[lbFlights.ItemIndex]+bext);
  end;

  function Del3files: boolean;                     {Legacy without sensor files}
  var fn: string;
  begin
    result:=false;
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text);                                                {Delete Telemetry}
    if DeleteOneFile(fn+dkpath+PathDelim+kfile+    {Telemetry}
                          lbFlights.Items[lbFlights.ItemIndex]+fext) then begin
      DeleteOneFile(fn+spath+PathDelim+sfile+      {Try RemoteGPS}
                    lbFlights.Items[lbFlights.ItemIndex]+fext);
      DeleteOneFile(fn+fpath+PathDelim+ffile+      {Try Remote file}
                    lbFlights.Items[lbFlights.ItemIndex]+fext);
      result:=true;                                {At last Telemetry deleted}
    end;
  end;

  procedure Del4Files;                             {legacy with sensor files}
  begin
    if Del3files then begin                        {Delete also sensor file für Typhoon H}
      DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+npath+
                    PathDelim+nfile+lbFlights.Items[lbFlights.ItemIndex]+sext);
    end;
  end;

begin
  nf:=0;                                           {Number files deleted}
  if lbFlights.ItemIndex>=0 then begin             {Only if the selection is valid}
    case v_type of
      1..4, 6, YTHPid, ThBid: Del3files;
      5: Del4Files;
      MQid: DelMQ;                                 {Sensor files Mantis Q}
      H5id: DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                          lbFlights.Items[lbFlights.ItemIndex]+hext);   {.tlog}
      MQcsvID: DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                          lbFlights.Items[lbFlights.ItemIndex]+bext);   {.log}
      BrID: DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                          lbFlights.Items[lbFlights.ItemIndex]+bext);
      H501ID: DeleteOneFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+h5file+
                            lbFlights.Items[lbFlights.ItemIndex]+fext); {Hubsan}

    end;
    if nf>0 then begin
      StatusBar1.Panels[5].Text:=rsFLdelete+tab1+
                                 lbFlights.Items[lbFlights.ItemIndex]+suff+
                                 IntToStr(nf)+tab1+rsFilesDel;
      StatusBar1.Invalidate;
      AppLog.Lines.Add(LineEnding);
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      AppLog.Lines.Add(LineEnding);
      ResetCut;                                    {Reset cut timestamps}
      FreigabeCut(true);                           {Output to status is true}
      TimerDblClick.Enabled:=false;
      TimerDiashow.Enabled:=false;                 {Stop Diashow Profiles}
      if Form2<>nil then
        Form2.Close;                               {Detailfenster schließen}
      tpos:=0;                                     {Position zurücksetzen}
      SelDirAct('');                               {Reload all}
    end;
  end;
end;

procedure TForm1.mnReloadClick(Sender: TObject);   {Reload files}
begin
  SelDirAct('');
end;

procedure TForm1.mnSaveTabClick(Sender: TObject);           {Save grid details}
begin
  SaveDialog1.Title:=titSaveTab;
  if lbFlights.Items.Count>0 then begin
    SaveDialog1.FileName:='tab'+lbFlights.Items[lbFlights.ItemIndex]+fext;
  end else
    SaveDialog1.FileName:=ChangeFileExt(OpenDialog1.FileName, '')+'tab.csv';
  if SaveDialog1.Execute then begin
    gridDetails.SaveToCSVFile(SaveDialog1.FileName,';');
  end;
end;

procedure TForm1.speDataPointEditingDone(Sender: TObject);  {Change dataset number}
begin
  GoToZ(4);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
//  topp:=nil;
end;

procedure TForm1.FormDblClick(Sender: TObject);    {AboutBox}
begin
  if MessageDlg(capForm1+sLineBreak+AppName+tab2+AppVersion+
                sLineBreak+sLineBreak+meinname+sLineBreak+homepage+sLineBreak+email,
                mtInformation,[mbHelp, mbOK],0)=mrNone then
    OpenManual;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin                                              {Drop Directory to App Window}
  Application.BringToFront;
  cbxLogDir.Text:=GetFlightLogDir(FileNames[0]);
  SelDirAct(cbxLogDir.Text);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if pcMain.ActivePage=tabHDia then begin          {Höhendiagramm}
    if ssCtrl in Shift then begin
      if ChartToolset2DataPointCrosshairTool1.Enabled then begin
        case key of               {Analyse erweitern/einschränken mit +  oder -}
          107, 187: speNumPoints.Value:=speNumPoints.Value+1; {+}
          109, 189: speNumPoints.Value:=speNumPoints.Value-1; {-}
        end;
        if key=vk_n then
          ManualCut;                               {Ausschneiden}
      end;
      if key=vk_c then
        Chart1.CopyToClipboardBitmap;              {Höhenprofil ins Clipboard}
    end;

    if key=vk_ESCAPE then
      KursorAus;                                   {Fadenkreuz aus}
  end;

  if pcMain.ActivePage=tabAnalyze3 then begin      {Schellanalyse}
    if key=vk_ESCAPE then TimerDiashow.Enabled:=false;   {Diashow Profiles stoppen}
    if (key=vk_F5) and
       (v_type<>BrID) then                         {H501 ???}
      TimerDiashow.Enabled:=true;                  {Weiter}
  end;

  if key=VK_ESCAPE then
    Close;
end;

procedure TForm1.FormShow(Sender: TObject);        {All to do after load session properties}
var
  i, bl: integer;

begin
  if not InitDone then begin                       {Verhindern, dass alles nochmal gemacht wird}
    lblSaturation.Caption:=capLabel4+tab2+IntToStr(tbrSaturation.Position);
    bl:=MAVmsg.Tag;

    if cbHighLight.Checked then                    {Switch on HighLighter}
      AppLog.Highlighter:=AppLogHighlighter;

    for i:=MAVmsg.Items.count-1 downto 0 do begin  {Get back Item check settings from .Tag}
      bl:=bl shr 1;
      MAVmsg.Checked[i]:=(bl and 1)=1;
    end;

    if trim(ParamstrUTF8(1))<>'' then begin        {versuchen, Datei zu öffnen}
      try
        Application.BringToFront;
        cbxLogDir.Text:=GetFlightLogDir(ParamstrUTF8(1));
        SelDirAct(cbxLogDir.Text);
      except
        SelDirAct('');                             {Übergebenes Verzeichnis}
      end;
    end else
      SelDirAct('');                               {Alles neu laden}

    InitDone:=true;
  end;
end;

procedure TForm1.Image4Click(Sender: TObject);
begin
  OpenURL(LazURL);
end;

procedure TForm1.lblMAVcommonClick(Sender: TObject); {Open MAVlink info (common)}
begin
  OpenURL(MAVurl);
end;

procedure TForm1.lblManualClick(Sender: TObject);     {Hilfe aufrufen}
begin
  if OpenManual then
    lblManual.Font.Color:=clPurple;
end;

procedure TForm1.lblManualMouseEnter(Sender: TObject); {Link animieren}
begin
  lblManual.Font.Style:=lblManual.Font.Style+[fsBold];
end;

procedure TForm1.lblManualMouseLeave(Sender: TObject); {Link animieren}
begin
  lblManual.Font.Style:=lblManual.Font.Style-[fsBold];
end;

procedure TForm1.lblUpdateClick(Sender: TObject);     {Download update}
begin
  CheckVersion;
end;

procedure TForm1.lblUpdateMouseEnter(Sender: TObject); {Link animieren}
begin
  lblUpdate.Font.Style:=lblUpdate.Font.Style+[fsBold];
end;

procedure TForm1.lblUpdateMouseLeave(Sender: TObject); {Link animieren}
begin
  lblUpdate.Font.Style:=lblUpdate.Font.Style-[fsBold];
end;

procedure TForm1.GetDDdata(lab: TLabeledEdit); {Wert für Schnellanalyse übergeben}
begin
  if TreeView1.Selected<>nil then begin
    cbxProfiles.ItemIndex:=0;
    lab.Text:=TreeView1.Selected.Text;
    if TreeView1.Selected.Parent.Text=mndir  then
      lab.Tag:=brID;
    if TreeView1.Selected.Parent.Text=h5file then
      lab.Tag:=H501ID;
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

procedure TForm1.lblMAVcommonMouseEnter(Sender: TObject); {Link animation}
begin
  lblMAVcommon.Font.Style:=lblManual.Font.Style+[fsBold];
end;

procedure TForm1.lblMAVcommonMouseLeave(Sender: TObject);
begin
  lblMAVcommon.Font.Style:=lblManual.Font.Style-[fsBold];
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
          break;                                   {letzte Spalte gefunden}
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
    {if gridDetails.ColCount=csvanz  .... PX4 CSV auch aufnehmen?}

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
        lbFlights.Items[lbFlights.ItemIndex]+bext; {Breeze}
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, mndir);
      inlist.LoadFromFile(fn);
      if inlist.count>8 then
        addnodes(inlist[8], newn);
      LabeledEdit1.Tag:=brID;                      {hart einstellen}
      LabeledEdit2.Tag:=brID;
      LabeledEdit3.Tag:=brID;
      exit;
    end;

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+h5file+
        lbFlights.Items[lbFlights.ItemIndex]+fext; {H501 log recorder}
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, H5file);
      inlist.LoadFromFile(fn);
      splitlist.Delimiter:=csvsep;
      if inlist.count>8 then
        addnodes(inlist[0], newn);
      LabeledEdit1.Tag:=H501ID;                    {hart einstellen}
      LabeledEdit2.Tag:=H501ID;
      LabeledEdit3.Tag:=H501ID;
      exit;
    end;

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
        kfile+lbFlights.Items[lbFlights.ItemIndex]+fext;
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, dkpath);      {create Tree node for Telemetry}
      inlist.LoadFromFile(fn);
      if inlist.count>2 then
        hdstr:=inlist[0];
      if pos(sep+'1'+sep, hdstr)>20 then           {H920 + ST24, old firmware}
        hdstr:=FakeHeader;                         {Replace header}
      if hdstr<>'' then
        addnodes(hdstr, newn);
    end;

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+spath+
                 PathDelim+sfile+lbFlights.Items[lbFlights.ItemIndex]+fext;
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, spath);
      inlist.LoadFromFile(fn);
      if inlist.count>2 then
        addnodes(inlist[0], newn);
    end;

    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+fpath+
                 PathDelim+ffile+lbFlights.Items[lbFlights.ItemIndex]+fext;
    if FileExists(fn) then begin
      newn:=TreeView1.Items.Add(nil, fpath);
      inlist.LoadFromFile(fn);
      if inlist.count>2 then
        addnodes(inlist[0], newn);
    end;
  finally
    TreeView1.FullExpand;
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

procedure TForm1.AppLogTimeStamp(s: string);       {AppLogHighlighter einteilen}
begin
  AppLog.Lines.Add(s);
  AppLog.Lines.Add(trenner+tab1+FormatDateTime(vzf+zzz, now)+tab1+trenner);
  AppLog.Lines.Add(LineEnding);
end;

procedure TForm1.SetSensorEnv;    {Bedienoberfläche für Sensor Anzeige anpassen}
begin
  Chart1BarSeries1.Clear;
  Chart1BarSeries2.Clear;
  Chart1BarSeries3.Clear;
  Chart1BarSeries4.Clear;
  Chart1BarSeries5.Clear;
  Chart1BarSeries6.Clear;
  PopupMenuTab.Items[0].Enabled:=false;            {GoogleMaps}
  PopupMenuTab.Items[1].Enabled:=false;            {OSM}
  PopupMenuTab.Items[4].Enabled:=false;            {Datenanalyse}
  PopupMenuTab.Items[5].Enabled:=false;            {GoTo #}
  PopupMenuTab.Items[6].Enabled:=false;            {GoTo Errorflags}
  PopupMenuTab.Items[9].Enabled:=false;            {Start}
  PopupMenuTab.Items[10].Enabled:=false;           {Stop}
  mnCursorEin.Enabled:= false;                     {HDiagamm Kursor}
  mnProfiles.Enabled:=false;                       {Profiles}
  mnSlideshow.Enabled:=false;                      {Diashow}
end;

procedure TForm1.ResetSensorEnv;
begin
  PopupMenuTab.Items[0].Enabled:=true;             {GoogleMaps}
  PopupMenuTab.Items[1].Enabled:=true;             {OSM}
  PopupMenuTab.Items[4].Enabled:=true;             {Datenanalyse}
  PopupMenuTab.Items[5].Enabled:=true;             {GoTo #}
  PopupMenuTab.Items[6].Enabled:=true;             {GoTo Errorflags}
  PopupMenuTab.Items[9].Enabled:=true;             {Start}
  PopupMenuTab.Items[10].Enabled:=true;            {Stop}
  mnCursorEin.Enabled:= true;                      {HDiagamm Kursor}
  mnProfiles.Enabled:=true;                        {Profiles}
  mnSlideshow.Enabled:=true;                       {Diashow}
end;

procedure TForm1.Anzeige;                          {neu anzeigen}
var x: integer;

  procedure AnzBreeze;
  begin
    rgQuelle.ItemIndex:=0;
    rgQuelle.Enabled:=false;                       {nur Telemetrie}
    case pcMain.ActivePageIndex of
      0: gridOverview.TopRow:=topp[0, 4];          {0 wird sowieso gefüllt}
      1: BrAnzeigeCSV(0);
      2: BrHDiagramm(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                     lbFlights.Items[lbFlights.ItemIndex]+bext);
      3: AnzeigeSchnell;
      5: PlatformLesen;                            {tabSettings}
    end;
  end;

  procedure AnzYLegacy;
  begin
    rgQuelle.Enabled:=true;                        {Auswahl Remote möglich}
    case pcMain.ActivePageIndex of
      0: gridOverview.TopRow:=topp[0, 4];          {0 wird sowieso gefüllt}
      1: AnzeigeCSV(0);
      2: HDiagramm(IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
                   kfile+lbFlights.Items[lbFlights.ItemIndex]+fext);
      3: AnzeigeSchnell;
      5: FirmwareLesen;                            {tabSettings}
    end;
  end;

  procedure AnzH501;
  begin
    rgQuelle.Enabled:=true;                        {Auswahl Remote möglich}
    case pcMain.ActivePageIndex of
      0: gridOverview.TopRow:=topp[0, 4];          {0 wird sowieso gefüllt}
      1: H501AnzeigeCSV(0);
      2: H501HDiagramm(IncludeTrailingPathDelimiter(cbxLogDir.Text)+H5file+
                       lbFlights.Items[lbFlights.ItemIndex]+fext);
      3: AnzeigeSchnell;
    end;
  end;

begin
  if (v_type=MQid) or                              {nichts tun für MantisQ}
     (v_type=H5id) then                            {nichts tun für H520}
       exit;
  AppLogTimeStamp('');
  DefaultFormatSettings.DecimalSeparator:='.';
  if (pcMain.Tag>0) and
     (pcMain.ActivePage<>tabAnalyze3) then
    try
      Form2.Chart1ConstantLine1.Active:=false;     {nicht bei Schnellanalyse}
    except
    end;
  for x:=1 to gridFirmware.RowCount-1 do           {FW Tabelle löschen}
    gridFirmware.Cells[1, x]:='';
  if lbFlights.Items.Count>0 then begin            {Dateien erkannt}
    lbFlights.Tag:=lbFlights.ItemIndex;            {Dateinummer merken}
    case rgQuelle.ItemIndex of                     {Voreinstellungen abh. von Quelle}
      0: ResetSensorEnv;                           {Kopter - Telemetrie}
      1: ResetSensorEnv;                           {ST10 - RemoteGPS}
      2: begin                                     {Funk - Remote}
           ResetSensorEnv;
           PopupMenuTab.Items[0].Enabled:=false;   {GoogleMaps}
           PopupMenuTab.Items[1].Enabled:=false;   {OSM}
         end;
      3: SetSensorEnv;                             {Sensor files}
    end;
    case v_type of
      brID: AnzBreeze;                             {Breeze}
      H501ID: AnzH501;                             {Hubsan}
    else
      AnzYLegacy;                                  {Rest der Yuneec Welt}
    end;
    cbxProfiles.Enabled:=rgQuelle.Enabled;         {Profiles nicht bei Breeze}
    mnMainStart.Enabled:=rgQuelle.Enabled;         {Menüs abh. von Type}
    mnMainStop.Enabled:=rgQuelle.Enabled;
    mnProfiles.Enabled:=rgQuelle.Enabled;          {Menü Profiles nicht für Breeze}
    StatusBar1.Tag:=0;                             {Zeiten nicht mit kopieren}
    LoadTree;                                      {Spaltenliste aktualisieren}
  end;                                             {Ende keine Dateien}
end;

procedure TForm1.ShowMQ;                           {Anzeige SensorFile Mantis Q}
var fn: string;
begin
  if lbFlights.Tag<>lbFlights.ItemIndex then begin {nur neue Datei}
    PopupMenuTab.Items[0].Enabled:=false;          {GoogleMaps}
    PopupMenuTab.Items[1].Enabled:=false;          {OSM}
    PopupMenuTab.Items[4].Enabled:=false;          {Datenanalyse}
    PopupMenuTab.Items[5].Enabled:=false;          {GoTo #}
    PopupMenuTab.Items[6].Enabled:=false;          {GoTo Errorflags}
    PopupMenuTab.Items[9].Enabled:=false;          {Start}
    PopupMenuTab.Items[10].Enabled:=false;         {Stop}
    pcMain.ActivePage:=tabDetails;                 {Details - table of CSV data}
    rgQuelle.ItemIndex:=3;                         {Sensor Files}
    rgQuelle.Enabled:=false;
    lbFlights.Tag:=lbFlights.ItemIndex;            {Dateinummer merken}
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
        nfile+lbFlights.Items[lbFlights.ItemIndex]+wext;
    if Fileexists(fn) then begin
      StatusBar1.Panels[5].Text:=fn;
      ShowSensorPlus(fn, lbFlights.ItemIndex, cbSensorKML.Checked, true, false, false);
    end else begin                                 {alternativ yuneec_*.log file}
      fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
          mfile+lbFlights.Items[lbFlights.ItemIndex]+bext;
      if Fileexists(fn) then begin
        StatusBar1.Panels[5].Text:=fn;
        ShowSensorPlus(fn, lbFlights.ItemIndex, cbSensorKML.Checked, true, false, false);
      end;
    end;
  end;
end;

procedure TForm1.ShowH520;                         {Anzeige TLOG File H520}
var fn: string;
begin
  if lbFlights.Tag<>lbFlights.ItemIndex then begin {nur neue Datei}
    PopupMenuTab.Items[0].Enabled:=false;          {GoogleMaps}
    PopupMenuTab.Items[1].Enabled:=false;          {OSM}
    PopupMenuTab.Items[4].Enabled:=false;          {Datenanalyse}
    PopupMenuTab.Items[5].Enabled:=false;          {GoTo #}
    PopupMenuTab.Items[6].Enabled:=false;          {GoTo Errorflags}
    PopupMenuTab.Items[9].Enabled:=false;          {Start}
    PopupMenuTab.Items[10].Enabled:=false;         {Stop}
    pcMain.ActivePage:=tabDetails;                 {Details}
    rgQuelle.ItemIndex:=3;                         {Sensor Files}
    rgQuelle.Enabled:=false;
    lbFlights.Tag:=lbFlights.ItemIndex;            {Dateinummer Index merken}
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
        lbFlights.Items[lbFlights.ItemIndex]+hext; {Dateinamenstamm+.tlog}
    if Fileexists(fn) then begin
      StatusBar1.Panels[5].Text:=fn;
      ShowSensorPlus(fn, lbFlights.ItemIndex, cbSensorKML.Checked, true, false, false);
    end;
  end;
end;

procedure TForm1.lbFlightsClick(Sender: TObject);  {Anzeige wenn Dateiwechsel}
begin
  if (lbFlights.Items.Count>0) and
     (lbFlights.ItemIndex<>lbFlights.Tag) then begin {nur bei neuer Datei}
    TimerDblClick.Enabled:=false;
    TimerDiashow.Enabled:=false;                   {Stop Diashow Profiles}
    if Form2<>nil then
      Form2.Close;                                 {Detailfenster schließen}
    ResetCut;                                      {Reset timestamps for Cut}
    tpos:=0;                                       {Position zurücksetzen}
    case v_type of
      MQid: ShowMQ;                                {Sensor_*.txt vom Mantis Q}
      H5id: ShowH520;                              {*.tlog vom H520}
      else Anzeige;                                {alle anderen herkömmlich}
    end;
  end;
end;

procedure TForm1.MAVmsgDefault;                    {All messages true}
var i: integer;
begin
  for i:=0 to MAVmsg.Items.Count-1 do              {All MAV messages selected}
    MAVmsg.Checked[i]:=true;
  MAVmsg.Tag:=$FFFF;                               {Read/store all items as true}
end;

procedure TForm1.MAVmsgDblClick(Sender: TObject);  {All messages true by double click}
var i: integer=0;                                  {or invert with Ctrl}
    shstate: TShiftState;
begin
  shstate:=GetKeyShiftState;
  MAVmsg.Tag:=0;
  if ssCtrl in shstate then begin                  {or invert with Ctrl}
    for i:=0 to MAVMsg.Items.Count-1 do
      MAVmsg.Checked[i]:=not MAVmsg.Checked[i];
    if MAVmsg.Checked[i] then
      MAVmsg.Tag:=MAVmsg.Tag or 1;
    MAVmsg.Tag:=MAVmsg.Tag shl 1;
  end else
    MAVmsgDefault;                                 {all checked}
end;

procedure TForm1.MAVmsgItemClick(Sender: TObject; Index: integer);
var i: integer;                                    {Save check settings to .Tag}
begin
  MAVmsg.Tag:=0;
  for i:=0 to MAVmsg.Items.Count-1 do begin
    if MAVmsg.Checked[i] then
      MAVmsg.Tag:=MAVmsg.Tag or 1;
    MAVmsg.Tag:=MAVmsg.Tag shl 1;
  end;
end;

procedure TForm1.mnSaveAsQuickClick(Sender: TObject); {Menü: GoTo Settings}
begin
  pcMain.ActivePage:=tabSettings;
  pcSettings3.ActivePage:=tabData;
end;

procedure TForm1.mnResetProfileClick(Sender: TObject); {Menü: Default}
begin
  TimerDiashow.Enabled:=false;
  if (v_type=MQid) or                              {nichts tun für MantisQ}
     (v_type=H5id) then                            {nichts tun für H520}
       exit;
  SetProfile(0);
  cbxProfiles.ItemIndex:=0;                        {Profiles rücksetzen}
end;

procedure TForm1.mnProfModeClick(Sender: TObject); {Menü Profile FlightModes}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(1);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.FreigabeCut(a: boolean);          {Freigabe und Anzeige Cut}
var ts: TDateTime;
begin
  btnCut.Enabled:=false;                           {Default: nicht gültig}
  lblBegin.Caption:=capLabel13;
  lblEnde.Caption:=capLabel14;
  lblDuration.Caption:='';                         {Dauer leer}
  if (cutb>0) and
     (cute>0) and
     (cutb>cute) then begin
    ts:=cute;                                      {Zeitstempel austauschen}
    cute:=cutb;
    cutb:=ts;
  end;
  if cutb>0 then
    lblBegin.Caption:=FormatDateTime(vzf, cutb);
  if cute>0 then
    lblEnde.Caption:=FormatDateTime(vzf, cute);
  if (cutb>0) and
     (cute>cutb) then begin                        {Dauer anzeigen}
    btnCut.Enabled:=true;
    cutbidx:=lbflights.ItemIndex;                  {To what file timestamps belong}
    lblDuration.Caption:=rsDauer+tab1+FormatDateTime('= nn:ss'+zzz, cute-cutb);
    if a then begin
      StatusBar1.Panels[5].Text:=lblDuration.Caption;  {Textfeld überschreiben}
      AppLog.Lines.Add(capCut+tab1+StatusBar1.Panels[5].Text);
    end;
  end else
    StatusBar1.Tag:=0;                             {nicht kopieren}
  if a then begin                                  {Ausgabe in Statuszeile erlauben}
    StatusBar1.Panels[3].Text:=lblBegin.Caption;
    StatusBar1.Panels[4].Text:=lblEnde.Caption;
  end;
  mnCut.Enabled:=btnCut.Enabled;
  mnCutTab.Enabled:=btnCut.Enabled;
  StatusBar1.Tag:=0;                               {nicht kopieren}
end;

procedure TForm1.mnDelTabClick(Sender: TObject);   {Reset Beginn/Ende}
begin
  ResetCut;                                        {Reset timestamps for Cut}
  FreigabeCut(true);
end;

procedure TForm1.mnStartTabClick(Sender: TObject); {Startpunkt setzen}
begin
  SetStartP;
end;

procedure TForm1.SetStartP;                        {Startpunkt setzen}
begin
  try
    cutb:=ZeitToDT(gridDetails.Cells[0, speDataPoint.Value], v_type);
  except
    cutb:=0;
  end;
  FreigabeCut(true);
end;

procedure TForm1.mnStopTabClick(Sender: TObject);  {Endpunkt setzen}
begin
  SetEndP;
end;

procedure TForm1.SetEndP;                          {Endpunkt setzen}
begin
  try
    cute:=ZeitToDT(gridDetails.Cells[0, speDataPoint.Value], v_type);
  except
    cute:=0;
  end;
  FreigabeCut(true);
end;

procedure TForm1.pcMainChange(Sender: TObject);
begin                                              {Tabs umschalten}
  if gridDetails.ColCount>=csvanz then             {nichts tun bei Anzeige Sensor}
    exit;
  TimerDiashow.Enabled:=false;                     {Diashow Profiles stoppen}
  GroupBox11.Tag:=0;                               {Suche zurücksetzen}
  ProgressBarScan.Position:=0;
  cbxProfiles.ItemIndex:=0;                        {Profiles zurücksetzen}
  rgVehicleType.Enabled:=cbVehicleType.Checked;
  cbxSearch.Enabled:=false;
  EnableMultiSelect;
  case pcMain.ActivePageIndex of                   {alle anderen Fälle ohne Änderung}
    1: begin                                       {Datentabelle}
         cbxSearch.Enabled:=true;                  {nur bei Datentabelle}
         if gridDetails.ColCount<YTHPcols then begin
           gridOverview.Tag:=pcMain.ActivePageIndex;
           Anzeige;
         end;
       end;
    2: begin
         gridOverview.Tag:=pcMain.ActivePageIndex; {Höhendiagramm}
         Anzeige;
       end;
    3: begin
         gridOverview.Tag:=pcMain.ActivePageIndex; {Schnellanalyse}
         Anzeige;
       end;
    4: cbxSearch.Enabled:=true;                    {Scan, enable search, no recording of ActivePageIndex}
    5: Anzeige;                                    {FW anzeigen wenn vorhanden}
  end;
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
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
        lbFlights.Items[z]+bext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.Count>9 then begin                     {Skip}
      vld:=(pos(brsnid, inlist[5])>0) and            {Breeze DroneSN}
           (pos(brfmode, inlist[8])>0);              {Header flightMode}
      StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
      for x:=9 to inlist.count-1 do begin            {Daten einlesen}
        splitlist.DelimitedText:=inlist[x];
        if (splitlist.Count>anzsp) then begin        {Konsistenz checken (Breeze)}
          h:=StrToFloatN(splitlist[10])/100;         {Altitude}
          if testh(h) then begin
            u:=BrUmrech(StrToFloatN(splitlist[21])); {Voltage}
            if (u>umax) and
               (u<110) then
              umax:=u;
            e:=StrToIntDef(trim(splitlist[19]), 0);  {Errorflag}
            topp[z, 6]:=topp[z, 6] or e;             {ID für Zeile einfärben e-flags}
            if (e and 1)>0 then
              uw1:=true;
            if (e and 2)>0 then
              uw2:=true;
            if (e and 4)>0 then                      {Motor failsave}
              topp[z, 6]:=topp[z, 6] or 256;         {ID für Emergency}
            if (trim(splitlist[14])<>'0') then begin {reale Flüge}
              if not nflg then begin
                if bg1<bgid then flt:=flt+ed-bg1;    {Rest noch aufaddieren}
                bg1:=bgid;                           {bg zurücksetzen bei Lücken}
              end;
              inc(n);
              nflg:=true;
              ed:=ZeitToDT(splitlist[0], v_type);
              if bg>ed then
                bg:=ed;                              {Beginnzeit ohne GPS}
              if bg1>ed then
                bg1:=ed;                             {Teil-Beginnzeit ohne GPS}
              if h>hmax then
                hmax:=h;
              if (u<umin) and
                  (u>0) then
                 umin:=u;
              if NichtLeer(splitlist[12]) and
                 NichtLeer(splitlist[13]) then begin
                inc(g);
                if (slat='') then begin
                  slat:=splitlist[12];               {Homepoint speichern}
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
                if slat<>'' then begin               {Startpunkt mit GPS}
                  lat2:=BrCoordToFloat(splitlist[12]);
                  lon2:=BrCoordToFloat(splitlist[13]);
                  dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                  if dist>emax then emax:=dist;
                  ddist:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                  strecke:=strecke+ddist;            {Strecke aufaddieren}
                  lat3:=lat2;                        {letzten Punkt speichern}
                  lon3:=lon2;
                end;
              end;                                   {Ende mit GPS Daten}
            end else
              nflg:=false;
          end;                                       {Ende realer Flug}
        end else begin
          StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
          AppLog.Lines.Add('''8287'+suff+StatusBar1.Panels[5].Text);
        end;
      end;
      flt:=flt+ed-bg1;
      splitlist.DelimitedText:=inlist[inlist.count-1];
      tend:=ZeitToDT(splitlist[0], v_type);          {letzten Zeitstempel merken}
    end;
  end;

  procedure WerteH501;
  var x, frms: integer;
  begin
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+H5file+
        lbFlights.Items[z]+fext;
    splitlist.Delimiter:=csvsep;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
    for x:=1 to inlist.count-1 do begin            {Daten einlesen}
      splitlist.DelimitedText:=inlist[x];
      if (splitlist.Count>17) then begin           {Konsistenz checken (H501)}
        tas:=0;
        h:=StrToFloatN(splitlist[4]);              {Altitude}
        frms:=StrToIntDef(splitlist[1], 0);
        if frms>0 then vld:=true;
        if testh(h) then begin
          u:=StrToFloatN(splitlist[9]);            {Voltage}
          if splitlist.Count>19 then
            tas:=H501velo(StrToFloatN(splitlist[19]));
          if (u>umax) and
             (u<110) then
            umax:=u;
          if tas>tasmax then
            tasmax:=tas;
          inc(n);
          ed:=GetDateFromFile(lbFlights.Items[z]);
          ed:=ed+ZeitToDT(splitlist[0], v_type);
          if bg>ed then
            bg:=ed;                                {Beginnzeit ohne GPS}
          if bg1>ed then
            bg1:=ed;                               {Teil-Beginnzeit ohne GPS}
          if h>hmax then
            hmax:=h;
          if (u<umin) and
              (u>0) then
             umin:=u;
          if NichtLeer(splitlist[2]) and
             NichtLeer(splitlist[3]) then begin
            inc(g);
            if (slat='') then begin
              slat:=splitlist[2];                  {Homepoint speichern}
              lat1:=StrToFloatN(slat);
              lon1:=StrToFloatN(splitlist[3]);
              lat3:=lat1;
              lon3:=lon1;
            end;
            if tas>tasmaxg then
              tasmaxg:=tas;
            if h>hmaxg then
              hmaxg:=h;
            if (u>umaxg) and
               (u<110) then
              umaxg:=u;
            if (u<uming) and
               (u>0) then
              uming:=u;
            if slat<>'' then begin                 {Startpunkt mit GPS}
              lat2:=StrToFloatN(splitlist[2]);
              lon2:=StrToFloatN(splitlist[3]);
              dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
              if dist>emax then emax:=dist;
              ddist:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
              strecke:=strecke+ddist;              {Strecke aufaddieren}
              lat3:=lat2;                          {letzten Punkt speichern}
              lon3:=lon2;
            end;
          end;                                     {Ende mit GPS Daten}
        end;                                       {Ende realer Flug}
      end else begin
        StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
        AppLog.Lines.Add('''8370'+suff+StatusBar1.Panels[5].Text);
      end;
    end;
    flt:=flt+ed-bg1;
    splitlist.DelimitedText:=inlist[inlist.count-1];
    tend:=ZeitToDT(splitlist[0], v_type);          {letzten Zeitstempel merken}
  end;

   procedure WerteYLegacy;
   var x: integer;
   begin
     fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
         kfile+lbFlights.Items[z]+fext;
     try
       inlist.LoadFromFile(fn);
     except
       StatusBar1.Panels[5].Text:=fn+nixda;
       AppLog.Lines.Add(StatusBar1.Panels[5].Text);
     end;
     if inlist.count>2 then begin                  {Überschrift und mind. 2 Zeile}
       StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
       splitlist.DelimitedText:=inlist[0];         {Überschrift einlesen, f_mode ermitteln}
       if (splitlist.Count>anzsp) then begin       {genug Spalten in Überschrift}
         vld:=fModeFinden(splitlist);              {Position f-mode merken}
         if v_type<>YTHPid then begin              {YTH Plus nicht überschreiben}
           splitlist.DelimitedText:=inlist[2];     {2. Datenzeile, v_type ermitteln}
           v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
           OverwriteVT;                            {Overwrite for PX4 Thunderbird}
         end;
         for x:=1 to inlist.Count-1 do begin
           if CheckE7(inlist[x]) then begin
             splitlist.DelimitedText:=inlist[x];
             if (splitlist.Count>anzsp) and        {Konsistenz Daten checken}
                 CheckVT(splitlist[gridDetails.Tag+2],
                         splitlist[gridDetails.Tag]) then begin   {YTH Plus sinnvoll}
               h:=StrToFloatN(splitlist[4]);       {Altitude}
               if testh(h) then begin
                 tas:=StrToFloatN(splitlist[7]);   {True Air Speed in m/s}
                 if (StrToFloatN(splitlist[3])>0) and               {Simulatorflug}
                    (splitlist[15]='231') then
                   simu:=true;
                 u:=StrToFloatN(splitlist[2]);     {Voltage}
                 if (u>umax) and
                    (u<200) then
                   umax:=u;
                 if GetRFM(splitlist[gridDetails.Tag],  {Flight Mode}
                           v_type,                 {Vehicle Type}
                           InFlight(h, tas1, tas)) then begin
                   if not nflg then begin          {nur reale Flüge}
                     if bg1<bgid then
                       flt:=flt+ed-bg1;
                     bg1:=bgid;
                   end;
                   inc(n);                         {Anzahl Datensätze real}
                   nflg:=true;

                   if v_type=YTHPid then           {YTH Plus vorerst Error Flag ausblenden}
                     e:=0
                   else
                     e:=StrToIntDef(splitlist[gridDetails.Tag+3], 0); {Errorflag}

                   topp[z, 6]:=topp[z, 6] or e;    {ID für Zeile einfärben e-flags}
                   if (e and 1)>0 then
                     uw1:=true;
                   if (e and 2)>0 then
                     uw2:=true;
                   if v_type=3 then begin          {Blade 350QX}
                     if splitlist[gridDetails.Tag]='8' then
                       topp[z, 6]:=topp[z, 6] or 256; {ID für Emergency}

                   end else
                     if v_type=YTHPid then begin   {YTH Plus}
                     //   Emergency unknown for YTH Plus --> nichts tun

                   end else begin                  {Alle anderen sollten 12 haben}
                     if splitlist[gridDetails.Tag]='12' then
                       topp[z, 6]:=topp[z, 6] or 256; {ID für Emergency}
                   end;
                   ed:=ZeitToDT(splitlist[0], v_type);
                   if bg>ed then
                     bg:=ed;                       {Beginnzeit ohne GPS}
                   if bg1>ed then
                     bg1:=ed;                      {Teil-Beginnzeit ohne GPS}
                   if h>hmax then
                     hmax:=h;
                   if tas>tasmax then
                     tasmax:=tas;
                   if (u<umin) and
                      (u>5) then
                     umin:=u;
                   if NichtLeer(splitlist[5]) and
                      NichtLeer(splitlist[6]) then begin  {GPS Koordinaten vorh.}
                     inc(g);
                     if (slat='') and
                        (lowercase(splitlist[8])=idtrue) then begin
                       slat:=splitlist[5];         {Homepoint speichern}
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
                        (u>5) then
                       uming:=u;
                     if slat<>'' then begin        {Startpunkt mit GPS}
                       lat2:=StrToFloatN(splitlist[5]);
                       lon2:=StrToFloatN(splitlist[6]);
                       dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
                       ddist:=DistanceBetweenTwoCoordinates(lat3, lon3, lat2, lon2); {Entfernung zum letzten Punkt}
                       if dist>emax then           {größte Entfernung zum Start}
                         emax:=dist;
                       strecke:=strecke+ddist;     {Strecke aufaddieren}
                       lat3:=lat2;                 {letzten Punkt speichern}
                       lon3:=lon2;
                     end;
                   end;                            {Ende mit GPS Daten}
                 end else
                   nflg:=false;                    {Ende realer Flug}
                 tas1:=tas;                        {letzte tas merken für Glättung}
               end;
             end;                                  {Ende Konsistenz und CheckVT}
           end;                                    {Ende CheckE7}
         end;                                      {Ende Einlesen}
         flt:=flt+ed-bg1;
         splitlist.DelimitedText:=inlist[inlist.count-1];
         tend:=ZeitToDT(splitlist[0], v_type);     {letzten Zeitstempel merken}
       end else begin
         StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;    {Ende Konsistenz checken}
         AppLog.Lines.Add('''8505'+suff+StatusBar1.Panels[5].Text);
       end;
     end;
   end;

begin
  if (v_type=H5id) or
     (v_type=MQid) then
    exit;                                          {nichts tun bei Anzeige PX4}
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  OverwriteVT;
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
    case v_type of
      brID: WerteBreeze;
      H501ID: WerteH501;
    else
      WerteYLegacy;
    end;
    if vld then begin                              {Anzeige gültiger Auswertung}
      gridOverview.BeginUpdate;
        flt:=round(flt*secpd)/secpd;               {Runden um Anzeigefehler zu vermeiden}
        if (g>3) and                               {alles ausgeben wenn GPS-Daten vorhanden sind}
           ((v_type<>YTHPid) or                    {Mindestflugzeit nur beim YTH Plus}
            (cbCleanHplus.Checked=false) or        {wenn Bereinigung eingestellt ist}
            (flt>minflt)) then begin               {Anzeige gültiger Auswertung}
          btnFlugBuch.Tag:=btnFlugBuch.Tag+1;
          gridOverview.Cells[1,z+1]:=FormatDateTime(dzf, bg);
          gridOverview.Cells[2,z+1]:=FormatDateTime(zzf, bg);
          gridOverview.Cells[3,z+1]:=FormatDateTime(zzf, ed);
          gridOverview.Cells[4,z+1]:=FormatDateTime('nn:ss', flt);     {Flugzeit}
          if rgSpeedUnit.ItemIndex=2 then begin
            gridOverview.Cells[5,z+1]:=FormatFloat(dzfl, hmaxg/fft)+'ft';
            gridOverview.Cells[6,z+1]:=FormatFloat(dzfl, emax/fft)+'ft';
            gridOverview.Cells[7,z+1]:=FormatFloat(dzfl, strecke/fft)+'ft';
            gridOverview.Cells[8,z+1]:=FormatFloat(dzfl, tasmaxg*fmph)+'mph'
          end else begin
            gridOverview.Cells[5,z+1]:=FormatFloat(dzfl, hmaxg)+'m';
            gridOverview.Cells[6,z+1]:=FormatFloat(dzfl, emax)+'m';
            gridOverview.Cells[7,z+1]:=FormatFloat(dzfl, strecke)+'m';
            gridOverview.Cells[8,z+1]:=FormatFloat(dzfl, tasmaxg*fkmh)+'km/h';
          end;
          if v_type=brID then begin
            gridOverview.Cells[9,z+1]:=IntToStr(round(umaxg))+'%';
            gridOverview.Cells[10,z+1]:=IntToStr(round(uming))+'%';
          end else begin
            gridOverview.Cells[9,z+1]:=FormatFloat(dzfl, umaxg)+'V';
            gridOverview.Cells[10,z+1]:=FormatFloat(dzfl, uming)+'V';
          end;
          if tasmaxg=0 then
            gridOverview.Cells[8,z+1]:='';
        end else begin                             {reduzierte Ausgabe}
          if (n>3) and                             {Ausgabe für Flüge ohne GPS}
             ((v_type<>YTHPid) or                  {Mindestflugzeit nur beim YTH Plus}
              (cbCleanHplus.Checked=false) or      {wenn Bereinigung eingestellt ist}
              (flt>minflt)) then begin             {Anzeige gültiger Auswertung}
            btnFlugBuch.Tag:=btnFlugBuch.Tag+1;
            gridOverview.Cells[1,z+1]:=FormatDateTime(dzf, bg);
            gridOverview.Cells[2,z+1]:=FormatDateTime(zzf, bg);
            gridOverview.Cells[3,z+1]:=FormatDateTime(zzf, ed);
            gridOverview.Cells[4,z+1]:=FormatDateTime('nn:ss', flt);
            if rgSpeedUnit.ItemIndex=2 then begin
              gridOverview.Cells[5,z+1]:=FormatFloat(dzfl, hmax/fft)+'ft';
              gridOverview.Cells[8,z+1]:=FormatFloat(dzfl, tasmax*fmph)+'mph'
            end else begin
              gridOverview.Cells[5,z+1]:=FormatFloat(dzfl, hmax)+'m';
              gridOverview.Cells[8,z+1]:=FormatFloat(dzfl, tasmax*fkmh)+'km/h';
            end;
            if v_type=brID then begin
//              gridOverview.Cells[8,z+1]:='';     {Breeze keine Speed}
              gridOverview.Cells[9,z+1]:=IntToStr(round(umax))+'%';
              gridOverview.Cells[10,z+1]:=IntToStr(round(umin))+'%';
            end else begin
              gridOverview.Cells[9,z+1]:=FormatFloat(dzfl, umax)+'V';
              gridOverview.Cells[10,z+1]:=FormatFloat(dzfl, umin)+'V';
            end;
            if tasmax=0 then
              gridOverview.Cells[8,z+1]:='';
          end else begin                           {Ausgabe sonstige (ohne Flug)}
            if v_type=brID then
              splitlist.DelimitedText:=inlist[9]
            else
              splitlist.DelimitedText:=inlist[1];
            bg:=ZeitToDT(splitlist[0], v_type);    {Fake Beginnzeit}
            gridOverview.Cells[1,z+1]:=FormatDateTime(dzf, bg);
            gridOverview.Cells[2,z+1]:=FormatDateTime(zzf, bg);
            gridOverview.Cells[3,z+1]:=FormatDateTime(zzf, tend);
            gridOverview.Cells[4,z+1]:=FormatDateTime('nn:ss',
                                      round((tend-bg)*secpd)/secpd);
            if v_type=brID then
              gridOverview.Cells[9,z+1]:=IntToStr(round(umax))+'%'
            else
              gridOverview.Cells[9,z+1]:=FormatFloat(dzfl, umax)+'V';
          end;
        end;
      gridOverview.Cells[1, gridOverview.RowCount-1]:=rsTurns+suff+
                                                    IntToStr(btnFlugBuch.Tag);
      if simu then
        gridOverview.Cells[6,z+1]:=rsSimulator;
      if uw1 then
        gridOverview.Cells[10,z+1]:=gridOverview.Cells[10,z+1]+' !';
      if uw2 then
        gridOverview.Cells[10,z+1]:=gridOverview.Cells[10,z+1]+'!';

      gridOverview.AutoSizeColumns;
      gridOverview.EndUpdate;
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
    ha:=StrToFloatN(splitlist[10])/100;            {Höhe Anfang}
    lat1:=BrCoordToFloat(splitlist[12]);
    lon1:=BrCoordToFloat(splitlist[13]);
    splitlist.DelimitedText:=inlist[inlist.Count-1];
    he:=StrToFloatN(splitlist[10])/100;            {Höhe Ende}
    emax:=0;
    if ha>he+hsw then begin                        {Sinken}
      an:=an+rsDescend+tab1+vms(dur, ha-he)+kma;
    end;
    if he>ha+hsw then begin                        {Steigen}
      an:=an+rsAscend+tab1+vms(dur, he-ha)+kma;
    end;
    for i:=0 to inlist.Count-1 do begin
      splitlist.DelimitedText:=inlist[i];
      lat2:=BrCoordToFloat(splitlist[12]);
      lon2:=BrCoordToFloat(splitlist[13]);
      dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);    {Entfernung zum 1. Punkt}
      if dist>emax then
        emax:=dist;
    end;
  end;

  procedure AnalyseYLegacy;
  var i: integer;
  begin
    ha:=StrToFloatN(splitlist[4]);                 {Höhe Anfang}
    lat1:=StrToFloatN(splitlist[5]);
    lon1:=StrToFloatN(splitlist[6]);
    splitlist.DelimitedText:=inlist[inlist.Count-1];
    he:=StrToFloatN(splitlist[4]);                 {Höhe Ende}
    emax:=0;
    if ha>he+hsw then begin                        {Sinken}
      an:=an+rsDescend+tab1+vms(dur, ha-he)+kma;
    end;
    if he>ha+hsw then begin                        {Steigen}
      an:=an+rsAscend+tab1+vms(dur, he-ha)+kma;
    end;
    for i:=0 to inlist.Count-1 do begin
      splitlist.DelimitedText:=inlist[i];
      lat2:=StrToFloatN(splitlist[5]);
      lon2:=StrToFloatN(splitlist[6]);
      dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);    {Entfernung zum Startpunkt}
      if dist>emax then
        emax:=dist;
    end;
  end;

  procedure AnalyseH501;
  var i: integer;
  begin
    ha:=StrToFloatN(splitlist[4]);                 {Höhe Anfang}
    lat1:=StrToFloatN(splitlist[2]);
    lon1:=StrToFloatN(splitlist[3]);
    splitlist.DelimitedText:=inlist[inlist.Count-1];
    he:=StrToFloatN(splitlist[4]);                 {Höhe Ende}
    emax:=0;
    if ha>he+hsw then begin                        {Sinken}
      an:=an+rsDescend+tab1+vms(dur, ha-he)+kma;
    end;
    if he>ha+hsw then begin                        {Steigen}
      an:=an+rsAscend+tab1+vms(dur, he-ha)+kma;
    end;
    for i:=0 to inlist.Count-1 do begin
      splitlist.DelimitedText:=inlist[i];
      lat2:=StrToFloatN(splitlist[2]);
      lon2:=StrToFloatN(splitlist[3]);
      dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);    {Entfernung zum Startpunkt}
      if dist>emax then
        emax:=dist;
    end;
  end;

  procedure anatelemetr;   {Telemetrie auswerten, Sinken, Steigen, Geschw.}
  begin
    splitlist.DelimitedText:=inlist[0];
    case v_type of
      brID: AnalyseBreeze;                         {Breeze}
      H501ID: AnalyseH501;
    else
      AnalyseYLegacy;                              {Rest der Yuneec Welt}
    end;
    an:=an+rsGridCell6+tab1+FormatFloat(dzfl, emax)+'m, '+
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
      dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);    {Entfernung zum 1. Punkt}
      if dist>emax then
        emax:=dist;
    end;
    an:=an+rsGPSh+tab1+FormatFloat(dzfl, ha)+'m, '+
           rsGridCell6+tab1+FormatFloat(dzfl, emax)+'m, '+
           rsSpeed+tab1+tab1+vms(dur, emax);
  end;

  procedure anafunk;                               {Stickbewegungen}
  var i, k, w, max, min: integer;
  begin
    splitlist.DelimitedText:=inlist[0];            {Flightmode Schalter lesen}
    an:=an+SwitchToStr(5, v_type, splitlist[5])+tab2;
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
    Application.ProcessMessages;
    an:='';
    try
      n:=speDataPoint.Value+speNumPoints.Value+1;
      if n>gridDetails.RowCount-1 then
        n:=gridDetails.RowCount-1;                 {Ende der Datei}
      AppLog.Lines.Add(capTabSheet10+tab1+rsFor+tab1+
                         IntToStr(speDataPoint.Value)+'-'+      {von}
                         IntToStr(n));             {bis}
      for x:=speDataPoint.Value to n do begin      {Liste zur Auswertung füllen}
        s:=gridDetails.Cells[0, x];
        for y:=1 to gridDetails.ColCount-1 do
          s:=s+sep+gridDetails.Cells[y, x];
        inlist.add(s);
      end;
      if inlist.Count>2 then begin
        splitlist.DelimitedText:=inlist[0];
        bg:=ZeitToDT(splitlist[0], v_type);        {Beginnzeitpunkt}
        if (cutb=0) or (bg<cutb) then
          cutb:=bg                                 {Beginn setzen}
        else
          if bg>cutb then cute:=bg;                {Ende setzen}
        FreigabeCut(false);                        {Status nicht überschreiben}
        splitlist.DelimitedText:=inlist[inlist.Count-1];
        dur:=ZeitToDT(splitlist[0], v_type)-bg;
        case rgQuelle.ItemIndex of
          0: anatelemetr;
          1: anast10;
          2: anafunk;
        end;
        StatusBar1.Panels[5].Text:=capTabSheet10+tab1+rsFor+tab1+
                                   NumSec(dur)+'s'+suff+an;
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      StatusBar1.Tag:=1;                           {Zeiten zum Kopieren freigeben}
    finally                                        {alles wieder freigeben}
      FreeAndNil(inlist);
      FreeAndNil(splitlist);
      Screen.Cursor:=crDefault;
    end;
  except
    StatusBar1.Panels[5].Text:=capTabSheet10+suff+rsError;
    AppLog.Lines.Add(StatusBar1.Panels[5].Text);
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

{see also:
https://wiki.lazarus.freepascal.org/CsvDocument#Loading_a_csv_document_into_a_StringGrid}

procedure TForm1.AnzeigeCSV(const mode: integer);  {Dateien als Tabelle anzeigen}
var i, x, p, n, zhl: integer;
    inlist, splitlist: TStringList;
    fn, slat, slon, sstr: string;
    tpos1: TDateTime;
    rs: boolean;

begin
  n:=Label3.Tag;                        {zwischenspeichern, wird sonst zerstört}
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des gridDetails überschrieben}
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  gridDetails.ColCount:=0;                         {alles löschen}
  gridDetails.RowCount:=14;
  gridDetails.ColCount:=defaultcol;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  rs:=false;
  slat:='';
  zhl:=0;
  try
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text);
    TreeView1.Items.Clear;
    Label3.Tag:=n;                                 {Spalte retten für ReadMAV}
    case rgQuelle.ItemIndex of                     {Dateityp}
      0: fn:=fn+kpath+kfile;                       {Kopter = telemetry_}
      1: fn:=fn+spath+PathDelim+sfile;             {ST10 = RemoteGPS_}
      2: fn:=fn+fpath+PathDelim+ffile;             {Funk = Remote_}
      3: begin                                     {Sensor_}
           ReadMav(mode);
           exit;
         end;
    end;
    fn:=fn+lbFlights.Items[lbFlights.ItemIndex]+fext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>1 then begin
      StatusBar1.Panels[5].Text:=fn;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end else begin
      StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then begin
      try
        splitlist.DelimitedText:=inlist[0];        {Überschrift einlesen}
        if rgQuelle.ItemIndex=0 then begin         {nur bei Telemetrie}
          if splitlist[19]='1' then begin          {H920 + ST24, old firmware}
            inlist[0]:=FakeHeader;
            splitlist.DelimitedText:=inlist[0];    {restore fake header}
          end;
          topp[lbFlights.ItemIndex, 5]:=0;         {Pointer Null setzen}
          fModeFinden(splitlist);                  {Position f-mode merken}
          if v_type<>YTHPid then begin
            splitlist.DelimitedText:=inlist[2];    {Vehicle Type merken}
            v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
            OverWriteVT;                           {Overwrite for PX4 Thunderbird}
            splitlist.DelimitedText:=inlist[0];    {Überschrift wiederherstellen}
          end else begin
            if cbCleanHplus.Checked then
              AppLog.Lines.Add(capCheckBox9);      {Bei YTH Plus anzeigen, ob bereinigt}
            splitlist[3]:=rsRest;                  {Column Current to batt remaining}
          end;
          StaticText1.Caption:=vtypeToStr(v_type); {anzeigen}
        end;
        speDataPoint.MaxValue:=inlist.Count;
        speNumPoints.MaxValue:=inlist.Count-10;
        speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        gridDetails.RowCount:=inlist.Count;        {vorbelegen}
        gridDetails.ColCount:=splitlist.Count;
        gridDetails.Cells[0,0]:=rsHDcell0;
        for i:=1 to splitlist.count-1 do begin
          gridDetails.Cells[i,0]:=splitlist[i];
          if pos(lcol, splitlist[i])>0 then begin  {letzte Spalte gefunden}
            gridDetails.Cells[i,0]:=lcol;
            gridDetails.ColCount:=i+1;
            break;                                 {wegen Fehler beim H, alte FW}
          end;
        end;

        gridDetails.BeginUpdate;
        p:=1;
        for x:=1 to inlist.count-1 do begin        {Daten einlesen}
          inc(zhl);
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>6 then begin
            if mode=0 then begin                   {Default: alle}
              for i:=0 to splitlist.count-1 do
                gridDetails.Cells[i, x]:=splitlist[i];
              if (rgQuelle.ItemIndex=0) then begin {nur bei Telemetrie}
                if (v_type=5) and                  {RealSense erkennen beim YTH}
                   (splitlist[gridDetails.Tag-1]='245') then rs:=true;
                if (StrToIntDef(splitlist[gridDetails.Tag+3], 0) shr 1)>0 then begin
                  mnGoToErr.Enabled:=true;
                  if topp[lbFlights.ItemIndex, 5]=0 then
                    topp[lbFlights.ItemIndex, 5]:=x;
                end;
                if (slat='') and                   {noch kein Homepoint}
                   NichtLeer(splitlist[5]) and
                   NichtLeer(splitlist[6]) and
                   GetRFM(splitlist[gridDetails.Tag], {f_mode, vehicle type}
                          v_type, true) then begin
                  slat:=splitlist[5];              {Homepoint speichern}
                  slon:=splitlist[6];              {Homepoint speichern}
                end;
              end;
              if (rgQuelle.ItemIndex=1) and        {Remote GPS}
                 (slat='') and                     {noch kein Homepoint}
                 (x>6) and                         {nicht grad die 1. Zeile}
                  NichtLeer(splitlist[1]) and
                  NichtLeer(splitlist[2]) then begin
                slat:=splitlist[2];                {RC Position speichern}
                slon:=splitlist[1];                {RC Position speichern}
              end;

              if x=100 then                        {Check till line 100}
                gridDetails.AutoSizeColumns;
            end;
            if mode=1 then begin                   {Filtermode}
              try
                sstr:=UpCase(trim(splitlist[n]));
              except
                sstr:=splitlist[0];
              end;
              if (sstr=cbxSearch.Text) or          {kurz -> vollqualifiziert}
                 (((sstr.length>4) or (pos('.', sstr)>0)) and  {Punkt drin oder lang}
                 (pos(cbxSearch.Text, sstr)>0)) then begin      {teilqualifiziert}
                for i:=0 to splitlist.count-1 do   {selektierte Zeile}
                  gridDetails.Cells[i, p]:=splitlist[i];
                inc(p);
              end;
            end;                                   {Ende Filtermode}
          end else begin
            StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsEmpty+tab1+
                                       capLabel6+Format('%6d', [x]);
            AppLog.Lines.Add('''9076'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}
        gridDetails.Col:=CellFocus[rgQuelle.ItemIndex, 0]; {load cell focus}
        gridDetails.Row:=CellFocus[rgQuelle.ItemIndex, 1];

        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1 );
          StatusBar1.Panels[2].Text:=rsSelection;
          AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
          if p>1 then begin
            gridDetails.RowCount:=p;
          end else gridDetails.RowCount:=5;        {leere Tabelle}
          gridDetails.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=gridDetails.Cells[n,0]+' = "'+
                                     cbxSearch.Text+'"';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          if rs then
            StaticText1.Caption:=StaticText1.Caption+'+'+rsRealSense;
          StatusBar1.Panels[2].Text:=rgOutFormat.Items[rgOutFormat.ItemIndex];
          gridDetails.TopRow:=topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]; {gemerkten Top setzen}
          if tpos1>0 then begin          {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to gridDetails.RowCount-1 do
              if ZeitToDT(gridDetails.Cells[0, i], v_type)>tpos1 then
                break;                             {Stelle gefunden}
            gridDetails.TopRow:=i-gridDetails.VisibleRowCount-1;   {zeitl. Pos setzen}
            gridDetails.Row:=i;
            gridDetails.Col:=CellFocus[rgQuelle.ItemIndex, 0];     {load column focus}
            CellFocus[rgQuelle.ItemIndex, 0]:=gridDetails.Col;     {save cell focus}
            CellFocus[rgQuelle.ItemIndex, 1]:=gridDetails.Row;
          end;
        end;
        gridDetails.EndUpdate;

        AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(slat, slon));   {Anzeige Start und Ende}
        AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(splitlist[5], splitlist[6]));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [zhl]);
        AppLog.Lines.Add('''9118'+suff+StatusBar1.Panels[5].Text);
      end;
      if pcMain.ActivePage=tabDetails then
        gridDetails.SetFocus;
    end else begin                                 {Datei leer}
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
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
  n:=Label3.Tag;                                   {zwischenspeichern, wird sonst zerstört}
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  SetSensorEnv;
  mnGoogleMap.Enabled:=true;                       {GoogleMaps Link}
  mnOSM.Enabled:=true;                             {OSM Link}
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(fn);

    if inlist.count>1 then begin                   {Laden inklusive Überschrift}
      gridDetails.BeginUpdate;
      pcMain.ActivePage:=tabdetails;
      gridDetails.ColCount:=csvanz;                {auch ID für PX4 CSV}
      gridDetails.RowCount:=inlist.Count;          {nur wenn Daten vorhanden sind}
      for i:=0 to Inlist.Count-1 do begin
        gridDetails.Rows[i].Delimiter:=sep;
        gridDetails.Rows[i].StrictDelimiter:=true;
        gridDetails.Rows[i].DelimitedText:=inlist[i];
        if i=10 then
          gridDetails.AutoSizeColumns;             {only once}
      end;
      gridDetails.EndUpdate;
    end;

    StatusBar1.Panels[1].Text:=IntToStr(inlist.Count-1);
    StatusBar1.Panels[5].Text:=fn;
    AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    AppLog.Lines.Add(StatusBar1.Panels[1].Text+' PX4 '+rsMAVlink+tab1+rsDS);
    AppLog.Lines.Add(LineEnding);
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
  btnClose.Tag:=0;                       {Annahme Breeze Telemetrie in Meter}
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des gridDetails überschrieben}
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  for i:=0 to gridDetails.ColCount-1 do
    gridDetails.Cols[i].Clear;
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  gridDetails.RowCount:=1;
  gridDetails.ColCount:=0;                         {alles löschen}
  slat:='';
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+lbFlights.Items[lbFlights.ItemIndex]+bext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;

    if inlist.count>minlines then begin
      try
        StaticText1.Caption:=vtypeToStr(brid);     {Typ anzeigen}
        if pos(plfAndr, inlist[2])>0 then
          btnArchive.Tag:=1                        {Platform is Android}
        else
          btnArchive.Tag:=0;                       {else iOS}
        if pos(brsnid, inlist[5])=1 then           {Serial number}
          StaticText1.Caption:=StringReplace(inlist[5], brsnid,
                               vtypeToStr(brID),[rfIgnoreCase]);
        cbxText.Text:=StaticText1.Caption;
        Merkliste(cbxText, speItems.Value);
        splitlist.DelimitedText:=inlist[8];        {Überschrift einlesen}
        rgQuelle.ItemIndex:=0;
        topp[lbFlights.ItemIndex, 5]:=0;           {Pointer Null setzen}
        speDataPoint.MaxValue:=inlist.Count-9;
        speNumPoints.MaxValue:=inlist.Count-10;
        speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-10);
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        if inlist.count>minlines then begin
          StatusBar1.Panels[5].Text:=fn;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        gridDetails.RowCount:=inlist.Count-8;      {vorbelegen}
        gridDetails.ColCount:=splitlist.Count;
        for i:=0 to splitlist.count-1 do
          gridDetails.Cells[i,0]:=splitlist[i];

        gridDetails.BeginUpdate;
        splitlist.DelimitedText:=inlist[10];       {2. Zeile einlesen}
        for i:=3 to 9 do begin                     {Prüfen, ob ft verwendet wird}
          try
            lat1:=StrToFloatN(splitlist[i]);
            case i of                              {Tag=1, wenn Telemetrie in ft}
              3: if lat1>200  then btnClose.Tag:=1; {distance:     40..200}
              4: if lat1>20   then btnClose.Tag:=1; {hight:         7..20}
              6: if lat1>300  then btnClose.Tag:=1; {goHomeHight:  30..300}
              7: if lat1>800  then btnClose.Tag:=1; {maxHight:     30..800}
              8: if lat1>1000 then btnClose.Tag:=1; {maxDistance: 100..1000}
              9: if lat1>500  then btnClose.Tag:=1; {maxSpeed:    100..500}
            end;
          except
            btnClose.Tag:=0;                        {Annahme Werte in Meter}
          end;
        end;

        p:=1;
        for x:=9 to inlist.count-1 do begin        {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin      {Konsistenz checken (Breeze)}
            if mode=0 then begin
              for i:=0 to splitlist.count-1 do
                gridDetails.Cells[i,x-8]:=splitlist[i];
              if (rgQuelle.ItemIndex=0) and
                 ((StrToIntDef(splitlist[19], 0) shr 1)>0) then begin
                mnGoToErr.Enabled:=true;           {Go to error flag}
                if topp[lbFlights.ItemIndex, 5]=0 then
                  topp[lbFlights.ItemIndex, 5]:=x-8;
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
              if (slat=cbxSearch.Text) or          {kurz -> vollqualifiziert}
                 (((slat.length>4) or (pos('.', slat)>0)) and   {Punkt drin oder lang}
                 (pos(cbxSearch.Text, slat)>0)) then begin       {teilqualifiziert}
                for i:=0 to splitlist.count-1 do   {selektierte Zeile}
                  gridDetails.Cells[i, p]:=splitlist[i];
                inc(p);
              end;
            end;
          end else begin
            StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
            AppLog.Lines.Add('''9229'+suff+StatusBar1.Panels[5].Text);
          end;
          if x=20 then
            gridDetails.AutoSizeColumns;           {only once}
        end;                                       {Ende Daten einlesen}

        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1);
          StatusBar1.Panels[2].Text:=rsSelection;
         AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
         if p>1 then begin
            gridDetails.RowCount:=p;
          end else
            gridDetails.RowCount:=5;               {leere Tabelle}
          gridDetails.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=gridDetails.Cells[n,0]+' = "'+
                                     cbxSearch.Text+'"';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[2].Text:=rgOutFormat.Items[rgOutFormat.ItemIndex];
          x:=(inlist.count-9) div 2;               {Daten irgendwo in der Mitte}
          if tpos1>0 then begin     {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to gridDetails.RowCount-1 do
              if ZeitToDT(gridDetails.Cells[0, i], brID)>tpos1 then
                break; {Stelle gefunden}
            gridDetails.TopRow:=i-gridDetails.VisibleRowCount-1;   {zeitl. Pos setzen}
          end;
        end;
        gridDetails.EndUpdate;

        AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(BrCoordFormat(slat),
                           BrCoordFormat(slon)));  {Anzeige Start und Ende}
        AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(BrCoordFormat(splitlist[12]),
                           BrCoordFormat(splitlist[13])));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [x]);
        AppLog.Lines.Add('''9336'+suff+StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    gridDetails.TopRow:=topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]; {Top setzen}
    if pcMain.ActivePage=tabDetails then
      gridDetails.SetFocus;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
    Label3.Tag:=n;
  end;
end;

{https://www.rc-drohnen-forum.de/thread/10002-flugdatenrekorder-f%C3%BCr-hubsan-901a-transmitter-geht-wahrscheinlich-auch-f%C3%BCr-den-9/?postID=78350#post78384

flaretom:

- Ich würde H501_Datum_Zeit.csv vorschlagen. Offiziell hat der H501 ja kein Log
   --> sollte keine Namenskonflikte geben.
- Float -> habe ich so, gibt es eine Anforderung an die Nachkommastellen?
- Strings habe ich (noch) nicht
- Spaltentitel sind vorhanden:

Time	Zeitstempel
frames  Bitmap mit den seit dem letzten SD-Karten schreiben empfangene Frames
              (cNavData = 0x01,cTeleData = 0x02, cControl = 0x04)
Lat	Latitude
Lon	Longitude
Elev	Elevation (noch 0, da die Daten im Transmitter-Display angezeigt werden,
              ich habe noch nicht weiß wo sie in den Frames stehen)
Dist	Distance (da sind zwar Daten, aber eigentlich gilt das Gleiche wie für Elevation)
Heading;Roll;Pitch;	Fluglage in Grad (0.1°Auflösung)
VBat	Akkuspannung des Kopters (0.1V Auflösung)
Sats	Anzahl Satelliten
throttle;rudder;pitch;yaw;	Potiwerte wie vom Transmitter gesendet
marker	!= wenn Rekordertasten betätigt
video	!= 0 wenn Video aktiviert ist (in der Beispieldatei noch leer)
photo	!= 0 wenn Photo gemacht wurde (in der Beispieldatei noch leer)}

procedure TForm1.H501AnzeigeCSV(const mode: integer); {H501 Dateien als Tabelle anzeigen}
var i, x, n, p: integer;
    inlist, splitlist: TStringList;
    fn, slat, slon: string;
    tpos1: TDateTime;

begin
  n:=Label3.Tag;                        {zwischenspeichern, wird sonst zerstört}
  btnClose.Tag:=0;                       {H501 Telemetrie in Meter}
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des gridDetails überschrieben}
  screen.Cursor:=crHourGlass;
  for i:=0 to gridDetails.ColCount-1 do
    gridDetails.Cols[i].Clear;
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  Application.ProcessMessages;
  gridDetails.RowCount:=1;
  gridDetails.ColCount:=0;                         {alles löschen}
  slat:='';
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=csvsep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+h5file+
        lbFlights.Items[lbFlights.ItemIndex]+fext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;

    if inlist.count>1 then begin
      try
        splitlist.DelimitedText:=inlist[0];        {Überschrift einlesen}
        rgQuelle.ItemIndex:=0;
        topp[lbFlights.ItemIndex, 5]:=0;           {Pointer Null setzen}
        StaticText1.Caption:=vtypeToStr(H501id);   {Typ anzeigen}
        speDataPoint.MaxValue:=inlist.Count;
        speNumPoints.MaxValue:=inlist.Count-10;
        speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        gridDetails.RowCount:=inlist.Count;        {vorbelegen}
        gridDetails.ColCount:=splitlist.Count;
        StatusBar1.Panels[5].Text:=fn;
        for i:=0 to splitlist.count-1 do
          gridDetails.Cells[i, 0]:=splitlist[i];

        gridDetails.BeginUpdate;
        p:=1;
        for x:=1 to inlist.count-1 do begin        {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>14 then begin         {Konsistenz checken (Breeze)}
            if mode=0 then begin
              for i:=0 to splitlist.count-1 do
                gridDetails.Cells[i, x]:=splitlist[i];
              if (slat='') and                     {Noch kein Homepoint}
                 NichtLeer(splitlist[2]) and
                 NichtLeer(splitlist[3]) then begin
                slat:=splitlist[2];                {Homepoint Breeze speichern}
                slon:=splitlist[3];
              end;
            end;

            if mode=1 then begin                   {Filtermode}
              try
                slat:=UpCase(trim(splitlist[n]));
              except
                slat:=splitlist[0];
              end;
              if (slat=cbxSearch.Text) or             {kurz -> vollqualifiziert}
                 (((slat.length>4) or (pos('.', slat)>0)) and   {Punkt drin oder lang}
                 (pos(cbxSearch.Text, slat)>0)) then begin       {teilqualifiziert}
                for i:=0 to splitlist.count-1 do   {selektierte Zeile}
                  gridDetails.Cells[i, x]:=splitlist[i];
              end;
            end;
            inc(p);
          end else begin
            StatusBar1.Panels[5].Text:=rsInvalid+tab1+rsDS;
            AppLog.Lines.Add('''9391'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}

        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1);
          StatusBar1.Panels[2].Text:=rsSelection;
         AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
         if p>1 then begin
            gridDetails.RowCount:=p;
          end else
            gridDetails.RowCount:=5;               {leere Tabelle}
          gridDetails.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=gridDetails.Cells[n,0]+' = "'+
                                     cbxSearch.Text+'"';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[2].Text:=rgOutFormat.Items[rgOutFormat.ItemIndex];
          x:=(inlist.count-1) div 2;               {Daten irgendwo in der Mitte}
          if tpos1>0 then begin     {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to gridDetails.RowCount-1 do
              if ZeitToDT(gridDetails.Cells[0, i], H501ID)>tpos1 then
                break; {Stelle gefunden}
            gridDetails.TopRow:=i-gridDetails.VisibleRowCount-1;   {zeitl. Pos setzen}
          end;
        end;
        gridDetails.EndUpdate;

        AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(slat, slon));   {Anzeige Start und Ende}
        AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(splitlist[2], splitlist[3]));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [x]);
        AppLog.Lines.Add('''9495'+suff+StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    gridDetails.TopRow:=topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]; {Top setzen}
    if pcMain.ActivePage=tabDetails then
      gridDetails.SetFocus;
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
  tpos1:=tpos;  {letzte Pos merken, wird beim Neuzeichnen des gridDetails überschrieben}
  screen.Cursor:=crHourGlass;
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  gridDetails.RowCount:=1;
  gridDetails.ColCount:=0;                         {alles löschen}
  Application.ProcessMessages;
  slat:='';
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+lbFlights.Items[lbFlights.ItemIndex]+bext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;

    if inlist.count>15 then begin
      try
        StaticText1.Caption:=vtypeToStr(MQcsvID);  {Typ anzeigen}
        if pos(plfAndr, inlist[2])>0 then
          btnArchive.Tag:=1                        {Platform is Android}
        else
          btnArchive.Tag:=0;                       {else iOS}
        if pos(brsnid, inlist[8])=1 then           {Serial number}
          StaticText1.Caption:=StringReplace(inlist[5], brsnid,
                               vtypeToStr(MQcsvID),[rfIgnoreCase]);
        cbxText.Text:=StaticText1.Caption;         {Drone ID Mantis}
        Merkliste(cbxText, speItems.Value);
        splitlist.DelimitedText:=inlist[15];       {Überschrift einlesen}
        rgQuelle.ItemIndex:=0;
        topp[lbFlights.ItemIndex, 5]:=0;           {Pointer Null setzen}
        speDataPoint.MaxValue:=inlist.Count-16;
        speNumPoints.MaxValue:=inlist.Count-17;
        speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-17);
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        if inlist.count>18 then begin
          StatusBar1.Panels[5].Text:=fn;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
        gridDetails.RowCount:=inlist.Count-15;     {vorbelegen}
        gridDetails.ColCount:=splitlist.Count;
        for i:=0 to splitlist.count-1 do           {Überschrift auslesen}
          gridDetails.Cells[i,0]:=splitlist[i];

        gridDetails.BeginUpdate;
        p:=1;
        for x:=16 to inlist.count-1 do begin       {Daten einlesen}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>anzsp then begin      {Konsistenz checken (Breeze)}
            if mode=0 then begin
              for i:=0 to splitlist.count-1 do
                gridDetails.Cells[i, x-15]:=splitlist[i];
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
            AppLog.Lines.Add('''9525'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}
        if mode=1 then begin
          StatusBar1.Panels[1].Text:=IntToStr(p-1);
          StatusBar1.Panels[2].Text:=rsSelection;
         AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsSelection);
         if p>1 then begin
            gridDetails.RowCount:=p;
          end else
            gridDetails.RowCount:=5;               {leere Tabelle}
          gridDetails.TopRow:=1;                   {nach oben springen}
          StatusBar1.Panels[5].Text:=gridDetails.Cells[n,0]+' = "'+
                                     cbxSearch.Text+'"';
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end else begin
          StatusBar1.Panels[2].Text:=rgOutFormat.Items[rgOutFormat.ItemIndex];
          x:=(inlist.count-9) div 2;               {Daten irgendwo in der Mitte}
          if tpos1>0 then begin     {Scrollen zu letzter Zeit im voriger Tabelle}
            for i:=1 to gridDetails.RowCount-1 do
              if ZeitToDT(gridDetails.Cells[0, i], MQcsvID)>tpos1 then
                break; {Stelle gefunden}
            gridDetails.TopRow:=i-gridDetails.VisibleRowCount-1;   {zeitl. Pos setzen}
          end;
        end;
        gridDetails.EndUpdate;

        AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(BrCoordFormat(slat),
                           BrCoordFormat(slon)));  {Anzeige Start und Ende}
        AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(BrCoordFormat(splitlist[12]),
                           BrCoordFormat(splitlist[13])));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [x]);
        AppLog.Lines.Add('''9631'+suff+StatusBar1.Panels[5].Text);
      end;
    end else begin
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    gridDetails.TopRow:=topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]; {Top setzen}
    if pcMain.ActivePage=tabDetails then
      gridDetails.SetFocus;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
    Label3.Tag:=n;
  end;
end;

{ Legacy / default
Chart1BarSeries1: Series Color:=clFuchsia  (Angle Mode – Purple LED)
Chart1BarSeries2: Series Color:=clGreen    (für Smart Mode)
Chart1BarSeries3: Series Color:=clRTH      (für RTH)
Chart1BarSeries4: Series Color:=clMaroon   (Emergency)
Chart1BarSeries5: Series Color:=clNoGPS    (Orange)
Chart1BarSeries6: Series Color:=clBlue     (Sports Mode, Stability)
}

procedure TForm1.HdiaColor;                        {Colors for Bar series depending on vehicle type}
begin
  Chart1BarSeries1.SeriesColor:=clAngle;
  Chart1BarSeries2.SeriesColor:=clSmart;
  Chart1BarSeries3.SeriesColor:=clRTH;
  Chart1BarSeries4.SeriesColor:=clEmergency;
  Chart1BarSeries5.SeriesColor:=clNoGPS;
  Chart1BarSeries6.SeriesColor:=clSport;
  if v_type=ThBid then                             {Thunderbird}
    Chart1BarSeries3.SeriesColor:=clAcro;
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
Dies funktioniert nicht beim Typhoon H Plus. }

procedure TForm1.HDiaInit;
var
  bwPercent: Integer;

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
  Chart1BarSeries6.Clear;
  case v_type of
    3: bwPercent:=60;                              {for Blade 350QX}
    ThBid: bwPercent:=50;                          {Thunderbird}
  else
    bwPercent:=100;
  end;

  HDiaColor;

  Chart1BarSeries1.BarWidthPercent:=bwPercent;
  Chart1BarSeries2.BarWidthPercent:=bwPercent;
  Chart1BarSeries3.BarWidthPercent:=bwPercent;
  Chart1BarSeries4.BarWidthPercent:=bwPercent;
  Chart1BarSeries5.BarWidthPercent:=bwPercent;
  Chart1BarSeries6.BarWidthPercent:=bwPercent;
{$IFDEF LINUX}
  Chart1BarSeries1.Transparency:=0;                {sonst schwarzer Hintergrund}
{$ELSE}
  Chart1BarSeries1.Transparency:=50;               {The same for all modes}
{$ENDIF}
  Chart1BarSeries2.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries3.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries4.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries5.Transparency:=Chart1BarSeries1.Transparency;
  Chart1BarSeries6.Transparency:=Chart1BarSeries1.Transparency;
  Chart1LineSeries1.Clear;                         {Spanungskurve}
  Chart1LineSeries2.Clear;                         {Hüllkurve}
  Chart1LineSeries1.SeriesColor:=clBlue;
  if cbCap.Checked then
    Chart1ConstantLine2.Active:=false              {Voltage low level}
  else begin
    case v_type of                                 {Umin Linie anzeigen}
      1: Chart1ConstantLine2.Position:=lipomin*6;  {H920 6S}
      5: Chart1ConstantLine2.Position:=lipomin*4;  {YTH  4S}
      H5id: Chart1ConstantLine2.Position:=lipomin*4;   {H520 4S}
      YTHPid: Chart1ConstantLine2.Position:=lipomin*4; {H Plus 4S}
      ThBid: Chart1ConstantLine2.Position:=lipomin*4;  {Thunderbird}
      H501ID: Chart1ConstantLine2.Position:=lipomin*2; {H501 2S}
    else
      Chart1ConstantLine2.Position:=lipomin*3;     {3S default}
    end;
    Chart1ConstantLine2.Active:=true;              {Voltage low level depending on battery}
  end;
  Chart1ConstantLine1.Active:=false;               {Cursor line off}
end;

procedure TForm1.HDiagramm(fn: string);            {Höhenprofil anzeigen}
var x: integer;
    inlist, splitlist: TStringList;
    h, u, hmxg, alt1, baseh: double;
    gps, simu: boolean;
    bg: TDateTime;

  procedure HDiaBlade350;
  begin
    Chart1LineSeries2.AddXY(bg, h);                {Hüllkurve}
    case StrToIntDef(splitlist[gridDetails.Tag], 25) of
      25: Chart1BarSeries1.AddXY(bg, h);           {Angle (AP mode)}
      11: Chart1BarSeries6.AddXY(bg, h);           {Stability mit/ohne GPS}
      12: Chart1BarSeries2.AddXY(bg, h);           {Smart}
      9, 14: Chart1BarSeries3.AddXY(bg, h);        {RTH}
      10, 13: Chart1BarSeries5.AddXY(bg, h);       {Agility}
      8:  Chart1BarSeries4.AddXY(bg, h);           {Emergency}
    end;
  end;

  procedure HDiaYTHPlus;
  begin
    Chart1LineSeries2.AddXY(bg, h);                {Hüllkurve}
    case StrToIntDef(splitlist[gridDetails.Tag], 5) of
      4: Chart1BarSeries5.AddXY(bg, h);            {ohne GPS}
      5: Chart1BarSeries1.AddXY(bg, h);            {Angle}
      6: Chart1BarSeries2.AddXY(bg, h);            {Smart}
      7: Chart1BarSeries6.AddXY(bg, h);            {Sport mode blau}
//    10, 14, 17, 20: Chart1BarSeries4.AddXY(bg, h); {maroon}
      12, 13: Chart1BarSeries3.AddXY(bg, h);       {RTH}
    end;
  end;

  procedure HDiaYLegacy;
  begin
    Chart1LineSeries2.AddXY(bg, h);                {Hüllkurve}
    case StrToIntDef(splitlist[gridDetails.Tag], 3) of        {f_mode; default: Angle}
      3, 4:                    Chart1BarSeries1.AddXY(bg, h); {Angle}
      6, 21, 23, 26..29, 33:   Chart1BarSeries2.AddXY(bg, h); {Smart}
      13, 14, 20:              Chart1BarSeries3.AddXY(bg, h); {Agility + RTH}
      12:                      Chart1BarSeries4.AddXY(bg, h); {Emergency}
      2, 5, 7, 22, 24, 31, 32: Chart1BarSeries5.AddXY(bg, h); {ohne GPS, Manual}
      0, 1:                    Chart1BarSeries6.AddXY(bg, h); {Stability}
    end;
  end;

  procedure HDiaThunderbird;   {only needed as long as absolute altitude provided}
  var vh: integer;
  begin
    if baseh>1 then
      vh:=round(baseh)-40
    else
      vh:=1;
    if h>vh then                                   {skip altitude=0}
      alt1:=h
    else
      h:=alt1;                                     {If 0 the keep previous value}
    Chart1LineSeries2.AddXY(bg, h-baseh);          {Hüllkurve}
    case StrToIntDef(splitlist[gridDetails.Tag], 3) of
      0:  Chart1BarSeries6.AddXY(bg, h-baseh);     {Stabilized mode, blue}
      1:  Chart1BarSeries5.AddXY(bg, h-baseh);     {Altitude mode, orange}
      3:  Chart1BarSeries1.AddXY(bg, h-baseh);     {Position mode, purple}
      12: Chart1BarSeries4.AddXY(bg, h-baseh);     {Emergency}
      20: Chart1BarSeries3.AddXY(bg, h-baseh);     {Rattitude/Rate mode, red (clAcro)}
      33, 13: Chart1BarSeries2.AddXY(bg, h-baseh); {Mission mode, RTH, green}
    end;
  end;

begin
  screen.Cursor:=crHourGlass;
  rgQuelle.ItemIndex:=0;                        {Umschalten auf Telemetrie}
  Application.ProcessMessages;
  AnzeigeCSV(0);                     {dazu passende Tabelle laden für Analyse}
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  gps:=false;
  simu:=false;                                     {Simulatorflug}
  hmxg:=0;                                         {Höhe gesamt für Anzeige}
  alt1:=0;
  baseh:=0;                                        {default: relative altitude}
  try
    HDiaInit;
    gridDetails.Tag:=DefaultPosFlightMode;         {default Position bei neuer FW}
    try
      inlist.LoadFromFile(fn);                     {Telemtry Datei laden}
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then begin
      KursorAus;                                   {Fadenkreuz aus bei neuem Flug}
      mnCursorEin.Enabled:=true;                   {Analyse erlauben}
      splitlist.DelimitedText:=inlist[0];          {Überschrift einlesen zum f_mode ermitteln}
      fModeFinden(splitlist);                      {Position f-mode merken}
      speDataPoint.MaxValue:=inlist.Count;
      speNumPoints.MaxValue:=inlist.Count-10;
      speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);

      Chart1.DisableRedrawing;
      if inlist.Count>1500 then
        Chart1BarSeries1.BarPen.Width:=2           {Bar smoothing}
      else
        Chart1BarSeries1.BarPen.Width:=4;          {dyn. anpassen}
      if v_type=ThBid then                         {for Thunderbird}
        Chart1BarSeries1.BarPen.Width:=Chart1BarSeries1.BarPen.Width+2;
      Chart1BarSeries2.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries3.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries4.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries5.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries6.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      StatusBar1.Panels[5].Text:=rsHProfil+fn;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
      AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);

      if v_type=ThBid then begin                   {check data for Thunderbird}
        baseh:=GethFromST10(lbFlights.ItemIndex, 0);{From begin of the Remote CSV file}
        AppLog.Lines.Add(rsGCSalt+suff+
                           FormatFloat(ctfl, baseh)+'m');
        for x:=1 to inlist.count-1 do begin        {erstmal Daten checken}
          try
            splitlist.DelimitedText:=inlist[x];
            if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and
               (Lowercase(splitlist[8])=idtrue) then begin
              GPS:=true;
            end;
            h:=StrToFloatN(splitlist[4]);
            if testh(h) then begin
              if (hmxg<h) then
                hmxg:=h;                           {Höhe für Gipfelhöhe Anzeige}
            end;
          except
            StatusBar1.Panels[5].Text:=rsInvalid;
            AppLog.Lines.Add('''9896'+capLabel6+Format('%6d', [x])+  {Datenpunkt ausgeben}
                               suff+StatusBar1.Panels[5].Text);
          end;
        end;
        alt1:=baseh;                                {set default GCS altitude}

      end else begin                                {all other legacy Yuneec}
        for x:=1 to inlist.count-1 do begin         {erstmal Daten checken}
          try
            splitlist.DelimitedText:=inlist[x];
            if (splitlist.Count>anzsp) and          {Konsistenz checken (YTH)}
                CheckVT(splitlist[gridDetails.Tag+2],
                        splitlist[gridDetails.Tag]) then begin  {YTH Plus sinnvoll}
              if (NichtLeer(splitlist[3])) and
                 (splitlist[15]='231') then         {ID für Simulator}
                simu:=true;
              if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and
                 (Lowercase(splitlist[8])=idtrue) then
                GPS:=true;
              h:=StrToFloatN(splitlist[4]);
              if testh(h) then begin
                if (hmxg<h) then
                  hmxg:=h;                         {Höhe für Gipfelhöhe Anzeige}
              end;
            end;
          except
            StatusBar1.Panels[5].Text:=rsInvalid;
            AppLog.Lines.Add('''9923'+capLabel6+Format('%6d', [x])+  {Datenpunkt ausgeben}
                               suff+StatusBar1.Panels[5].Text);
          end;
        end;
      end;
      hmxg:=hmxg-baseh;
      if rgSpeedUnit.ItemIndex=2 then              {Überschrift im Diagramm ausgeben}
        Chart1.Title.Text.Add(rsGridCell5+suff+Format('%f', [hmxg/fft])+'ft')
      else
        Chart1.Title.Text.Add(rsGridCell5+suff+Format('%f', [hmxg])+'m');
      if simu then begin
        Chart1LineSeries1.SeriesColor:=clGray;     {Simulation}
        Chart1.Title.Text.Add(rsSimulator);
      end;

      if not gps then begin
        Chart1LineSeries1.SeriesColor:=clNoGPS;    {ohne GPS}
        Chart1.Title.Text.Add(rsNoGPS);
      end;
      if (v_type=ThBid) and
         (baseh<>0) then begin
        if rgSpeedUnit.ItemIndex=2 then            {GCS altitude im Diagramm ausgeben}
          Chart1.Title.Text.Add(rsGCSalt+suff+Format('%f', [baseh/fft])+'ft')
        else
          Chart1.Title.Text.Add(rsGCSalt+suff+Format('%f', [baseh])+'m');
      end;

      for x:=1 to inlist.count-1 do begin
        try                                        {Dateneinlesen}
          splitlist.DelimitedText:=inlist[x];
          if (splitlist.Count>anzsp) and           {Anzahl Spalten}
              CheckVT(splitlist[gridDetails.Tag+2],
                      splitlist[gridDetails.Tag]) then begin  {V-type beim YTH Plus}
            bg:=ZeitToDT(splitlist[0], v_type);
            h:=StrToFloatN(splitlist[4]);
            u:=StrToFloatN(splitlist[2]);
            if cbCap.Checked then
              Chart1LineSeries1.AddXY(bg, VtoProz(v_type, u))
            else
              Chart1LineSeries1.AddXY(bg, u);      {Spannungskurve}

            if testh(h) and
               (bg>0) then begin
              case v_type of                       {Vehicle Type}
                3: HDiaBlade350;                   {Blade 350QX}
                YTHPid: HDiaYTHPlus;               {YTH Plus}
                ThBid: HDiaThunderbird;            {assign f_mode to color for TB}
              else                                 {Q500 und andere}
                HdiaYLegacy;
              end;
            end;
          end;
        except
          StatusBar1.Panels[5].Text:=rsInvalid;
          AppLog.Lines.Add('''9886'+capLabel6+Format('%6d', [x])+   {Datenpunkt ausgeben}
                             suff+StatusBar1.Panels[5].Text);
        end;
      end;
      Chart1.Title.Visible:=true;
      Chart1.EnableRedrawing;
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
  Application.ProcessMessages;
  rgQuelle.ItemIndex:=0;                        {Umschalten auf Telemetrie}
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
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;

    if inlist.count>minlines then begin
      KursorAus;                                   {Fadenkreuz aus bei neuem Flug}
      speDataPoint.MaxValue:=inlist.Count-9;
      speNumPoints.MaxValue:=inlist.Count-10;
      speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
      if inlist.Count>1500 then
        Chart1BarSeries1.BarPen.Width:=2           {Bar smoothing}
      else
        Chart1BarSeries1.BarPen.Width:=4;          {dyn. anpassen}
      Chart1BarSeries2.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries3.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries4.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries5.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      Chart1BarSeries6.BarPen.Width:=Chart1BarSeries1.BarPen.Width;
      StatusBar1.Panels[5].Text:=rsHProfil+fn;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      StatusBar1.Panels[1].Text:=IntToStr(inlist.count-10);
      AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
      for x:=9 to inlist.count-1 do                {Dateneinlesen}
      try
        splitlist.DelimitedText:=inlist[x];
        if splitlist.Count>anzsp then begin
          if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
             BrGPSfix(splitlist[20]) then GPS:=true;  {fraglich, was loseGPSact ist}
          bg:=ZeitToDT(splitlist[0], v_type);
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
            2:     Chart1BarSeries6.AddXY(bg, h);  {In flight: blue}
            16:    Chart1BarSeries4.AddXY(bg, h);  {Notlandung: maroon}
          end;
        end else begin
          StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
      except
        StatusBar1.Panels[5].Text:=rsInvalid;
        AppLog.Lines.Add('''9972'+capLabel6+Format('%6d', [x])+  {Datenpunkt ausgeben}
                           suff+StatusBar1.Panels[5].Text);
      end;
      if not gps then
        Chart1LineSeries1.SeriesColor:=clNoGPS;    {ohne GPS: Voltage rot}
    end;
    if rgSpeedUnit.ItemIndex=2 then                {Überschrift im Diagramm ausgeben}
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

procedure TForm1.H501HDiagramm(fn: string);          {Höhenprofil H501 anzeigen}
var x: integer;
    inlist, splitlist: TStringList;
    h, u, hmxg: double;
    gps: boolean;
    bg: TDateTime;

begin
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  rgQuelle.ItemIndex:=0;                        {Umschalten auf Telemetrie}
  H501AnzeigeCSV(0);                   {dazu passende Tabelle laden für Analyse}
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=csvsep;                       {Semicolon}
  splitlist.StrictDelimiter:=True;
  gps:=false;
  hmxg:=0;
  try
    HDiaInit;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;

    if inlist.count>minlines then begin
      KursorAus;                                   {Fadenkreuz aus bei neuem Flug}
      speDataPoint.MaxValue:=inlist.Count;
      speNumPoints.MaxValue:=inlist.Count-10;
      speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
      if inlist.Count>1500 then
        Chart1BarSeries1.BarPen.Width:=2           {Bar smoothing}
      else
        Chart1BarSeries1.BarPen.Width:=4;          {dyn. anpassen   1=fuchsia}
      Chart1BarSeries2.BarPen.Width:=Chart1BarSeries1.BarPen.Width;  {green}
      Chart1BarSeries3.BarPen.Width:=Chart1BarSeries1.BarPen.Width;  {red}
      Chart1BarSeries4.BarPen.Width:=Chart1BarSeries1.BarPen.Width;  {maroon}
      Chart1BarSeries5.BarPen.Width:=Chart1BarSeries1.BarPen.Width;  {orange}
      Chart1BarSeries6.BarPen.Width:=Chart1BarSeries1.BarPen.Width;  {blue}
      StatusBar1.Panels[5].Text:=rsHProfil+fn;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
      AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
      for x:=1 to inlist.count-1 do                {Read all data}
      try
        splitlist.DelimitedText:=inlist[x];
        if splitlist.Count>14 then begin
          if (NichtLeer(splitlist[2]) or
              NichtLeer(splitlist[3])) then GPS:=true;

          bg:=ZeitToDT(splitlist[0], v_type);
          h:=H501alt(StrToFloatN(splitlist[4]));
          if hmxg<h then
            hmxg:=h;                               {Maximum elevation}

          u:=StrToFloatN(splitlist[9]);
          if cbCap.Checked then                    {Remaining capacity}
            Chart1LineSeries1.AddXY(bg, VtoProz(v_type, u))
          else
            Chart1LineSeries1.AddXY(bg, u);        {Spannungskurve}
          Chart1LineSeries2.AddXY(bg, h);          {Hüllkurve}

          case StrToInt(splitlist[1]) of
            0: Chart1BarSeries3.AddXY(bg, h);      {red}
            1, 2, 4: Chart1BarSeries5.AddXY(bg, h);  {orange: 1 frame}
            3, 5, 6: Chart1BarSeries6.AddXY(bg, h);  {blue: 2 frames}
            7: Chart1BarSeries2.AddXY(bg, h);      {all frames: green}
          else
            Chart1BarSeries4.AddXY(bg, h);         {all other frames: maroon}
          end;
        end else begin
          StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        end;
      except
        StatusBar1.Panels[5].Text:=rsInvalid;
        AppLog.Lines.Add('''10067'+capLabel6+Format('%6d', [x])+  {Datenpunkt ausgeben}
                           suff+StatusBar1.Panels[5].Text);
      end;
      if not gps then
        Chart1LineSeries1.SeriesColor:=clNoGPS;    {ohne GPS: Voltage rot}
    end;
    if rgSpeedUnit.ItemIndex=2 then                {Überschrift im Diagramm ausgeben}
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

procedure TForm1.AnzeigeSchnell;                   {Schnellanalyse}
var inlist0, inlist1, inlist2, splitlist: TStringList;
    fn: string;
    bg: TDateTime;
    w: double;

  procedure MakeQuickAnalysisChartForTelemetry;
  begin

  end;

  procedure MakeQuickAnalysisChart(lab: TLabeledEdit; hist: TLineSeries; cht: TChart);
  var
    p: integer;

    procedure MakeQuickAnalysisChartForTelemetry;
    var
      i: integer;

    begin
      begin                                        {Telemetry}
        splitlist.DelimitedText:=inlist0[0];       {Column header}
        if splitlist.count>30 then                 {Firmware error YTH}
          for i:=15 to splitlist.count-1 do
            if pos(lcol, splitlist[i])>0 then begin
              splitlist[i]:=lcol;                  {Zahlen wegwerfen}
              break;
            end;
        p:=splitlist.IndexOf(lab.Text);            {find index of column}

        if p>0 then begin
          for i:=1 to inlist0.count-1 do begin
            splitlist.DelimitedText:=inlist0[i];
            if CheckVT(splitlist[gridDetails.Tag+2],
                       splitlist[gridDetails.Tag]) then begin
              if p=8 then
                w:=BoolToDouble(splitlist[8])
              else
                w:=TransformW(0, p, StrToFloatN(splitlist[p]));
              if (p=1) and (w=0) then begin        {fsk_rssi ist Null}
                bg:=ZeitToDT(splitlist[0], v_type);
                Chart3LineSeries2.AddXY(bg, -1);
              end else begin                       {Alle anderenWerte einlesen}
                if (p<>4) or
                   ((p=4) and testh(w)) then begin
                  bg:=ZeitToDT(splitlist[0], v_type);
                  hist.AddXY(bg, w);
                end;
              end;
            end;
          end;
        end;
      end;   {Fehler im Hauptteil der Procedur werfen mit Fehlerausgabe}
    end;

    procedure MakeQuickAnalysisChartForRemoteGPS;
    var
      i: integer;

    begin
      try                                       {RemoteGPS, kann fehlen}
        splitlist.DelimitedText:=inlist1[0];    {Column header}
        p:=splitlist.IndexOf(lab.Text);         {find index of column}

        if p>0 then begin
          for i:=1 to inlist1.count-1 do begin  {Werte einlesen}
            splitlist.DelimitedText:=inlist1[i];
            bg:=ZeitToDT(splitlist[0], v_type);
            w:=TransformW(1, p, StrToFloatN(splitlist[p]));
            hist.AddXY(bg, w);
          end;
        end;
      except
        {fehlende Datei RemoteGPS ist kein Fehler, muss aber abgefangen werden}
      end;
    end;

    procedure MakeQuickAnalysisChartForRemote;
    var
      i: integer;

    begin
      try                                       {Remote, kann fehlen}
        splitlist.DelimitedText:=inlist2[0];    {Column header}
        p:=splitlist.IndexOf(lab.Text);         {find index of column}

        if p>0 then begin
          cht.AxisList[0].Title.Caption:=       {Bezeichnung überschreiben}
            ChToStr(cht.AxisList[0].Title.Caption, p);
          for i:=1 to inlist2.count-1 do begin  {Werte einlesen}
            splitlist.DelimitedText:=inlist2[i];
            bg:=ZeitToDT(splitlist[0], v_type);
            w:=TransformW(2, p, StrToFloatN(splitlist[p]));
            hist.AddXY(bg, w);
          end;
        end;
      except
        {fehlende Datei Remote ist kein Fehler, muss aber abgefangen werden}
      end;
    end;

  begin
    case lab.Tag of
      0: MakeQuickAnalysisChartForTelemetry;
      1: MakeQuickAnalysisChartForRemoteGPS;
      2: MakeQuickAnalysisChartForRemote;
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
        bg:=ZeitToDT(splitlist[0], v_type);
        if (pos('lat', lab.Text)=1) or
           (pos('lon', lab.Text)=1) then begin
          w:=BrCoordToFloat(splitlist[p]);
        end else
          w:=brTransformW(p, StrToFloatN(splitlist[p]));
        hist.AddXY(bg, w);
      end;
    end;
  end;

  procedure H501MakeSAH(lab: TLabeledEdit; hist: TLineSeries);
  var x, p: integer;

  begin
    splitlist.DelimitedText:=inlist0[0];           {Column header}
    p:=splitlist.IndexOf(lab.Text);                {find index of column}
    if p>0 then begin
      for x:=1 to inlist0.count-1 do begin         {Werte einlesen}
        splitlist.DelimitedText:=inlist0[x];
        bg:=ZeitToDT(splitlist[0], v_type);
        w:=H501TransformW(p, StrToFloatN(splitlist[p]));
        hist.AddXY(bg, w);
      end;
    end;
  end;

  procedure FillTelemetry;
  begin
    if inlist0.Count=0 then begin
      fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+
          kfile+lbFlights.Items[lbFlights.ItemIndex]+fext;
      if FileExists(fn) then begin
        inlist0.LoadFromFile(fn);
        if pos(sep+'1'+sep, inlist0[0])>20 then    {if H920 + ST24, old firmware}
          inlist0[0]:=FakeHeader;                  {Replace header}
      end;
    end;
  end;

  procedure FillRemoteGPS;
  begin
    if inlist1.Count=0 then begin
      fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+spath+
          PathDelim+sfile+lbFlights.Items[lbFlights.ItemIndex]+fext;
      if FileExists(fn) then inlist1.LoadFromFile(fn);
    end;
  end;

  procedure FillRemote;
  begin
    if inlist2.Count=0 then begin
      fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+fpath+
          PathDelim+ffile+lbFlights.Items[lbFlights.ItemIndex]+fext;
      if FileExists(fn) then
        inlist2.LoadFromFile(fn);
    end;
  end;

  procedure FillBreezeData;
  begin
    if inlist0.Count=0 then begin
      fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
          lbFlights.Items[lbFlights.ItemIndex]+bext;
      if FileExists(fn) then
        inlist0.LoadFromFile(fn);
    end;
  end;

  procedure FillH501Data;
  begin
    if inlist0.Count=0 then begin
      fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+H5file+
          lbFlights.Items[lbFlights.ItemIndex]+fext;
      if FileExists(fn) then
        inlist0.LoadFromFile(fn);
    end;
  end;

  procedure ChkFileFill(lab1: TLabeledEdit);       {Inlist füllen, aber nur wenn leer}
  begin
    case lab1.Tag of                               {Datei laden, wenn noch nicht gefüllt}
      0:      FillTelemetry;
      1:      FillRemoteGPS;
      2:      FillRemote;
      brid:   FillBreezeData;
      H501ID: FillH501Data;
    end;
  end;

begin
  GroupBox4.Tag:=1;                                {Anzeige gelaufen}
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist0:=TStringList.Create;
  inlist1:=TStringList.Create;
  inlist2:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  if v_type=h501ID then
    splitlist.Delimiter:=csvsep;
  splitlist.StrictDelimiter:=True;
  Chart3LineSeries1.Clear;
  Chart3LineSeries2.Clear;
  Chart4LineSeries1.Clear;
  Chart5LineSeries1.Clear;
  Chart3LineSeries1.BeginUpdate;
  Chart3LineSeries2.BeginUpdate;
  Chart4LineSeries1.BeginUpdate;
  Chart5LineSeries1.BeginUpdate;
  Chart3.ZoomFull;
  Chart4.ZoomFull;
  Chart5.ZoomFull;
  Chart3LineSeries1.SeriesColor:=ColorButton2.ButtonColor;
  Chart4LineSeries1.SeriesColor:=ColorButton3.ButtonColor;
  Chart5LineSeries1.SeriesColor:=ColorButton4.ButtonColor;
  Chart3.AxisList[0].Title.Caption:=LabeledEdit1.Text;  {y-Achse top}
  Chart4.AxisList[0].Title.Caption:=LabeledEdit2.Text;  {y-Achse middle}
  Chart5.AxisList[0].Title.Caption:=LabeledEdit3.Text;  {y-Achse bottom}
  Chart3.Hint:=rsChart+tab1+LabeledEdit1.Text;
  Chart4.Hint:=rsChart+tab1+LabeledEdit2.Text;
  Chart5.Hint:=rsChart+tab1+LabeledEdit3.Text;
  try
    try                                            {cover empty files}
      ChkFileFill(LabeledEdit1);
      case LabeledEdit1.Tag of
        brID: brMakeSAH(LabeledEdit1, Chart3LineSeries1);  {Breeze}
        H501ID: H501MakeSAH(LabeledEdit1, Chart3LineSeries1) {H501}
      else
        MakeQuickAnalysisChart(LabeledEdit1, Chart3LineSeries1, Chart3);
      end;

      ChkFileFill(LabeledEdit2);
      case LabeledEdit2.Tag of
        brID: brMakeSAH(LabeledEdit2, Chart4LineSeries1);  {Breeze}
        H501ID: H501MakeSAH(LabeledEdit2, Chart4LineSeries1) {H501}
      else
        MakeQuickAnalysisChart(LabeledEdit2, Chart4LineSeries1, Chart4);
      end;

      ChkFileFill(LabeledEdit3);
      case LabeledEdit3.Tag of
        brID: brMakeSAH(LabeledEdit3, Chart5LineSeries1);  {Breeze}
        H501ID: H501MakeSAH(LabeledEdit3, Chart5LineSeries1) {H501}
      else
        MakeQuickAnalysisChart(LabeledEdit3, Chart5LineSeries1, Chart5);
      end;
    except
      StatusBar1.Panels[5].Text:=rsCheckSettings+capAnalyse;
      AppLog.Lines.Add('''10281'+suff+StatusBar1.Panels[5].Text+tab1+rsDS);
    end;
  finally
    Chart3LineSeries1.EndUpdate;
    Chart3LineSeries2.EndUpdate;
    Chart4LineSeries1.EndUpdate;
    Chart5LineSeries1.EndUpdate;
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
  klist.Add(write_nme(capForm1));
  klist.Add('<description>'+
              FormatDateTime(mzf, dt)+'h - '+
              ExtractFileName(f)+'</description>');
  klist.Add('<Style id="Flightpath">');
  klist.Add(tab2+'<LineStyle>');
  klist.Add(tab4+'<color>'+ColorToKMLColor(ColorButton1.ButtonColor)+'</color>');  {Farbe der Linie}
  klist.Add(tab4+'<width>'+IntToStr(speLinePath.Value)+'</width>');
  klist.Add(tab2+'</LineStyle>');
  klist.Add(tab2+'<PolyStyle><color>7f00ff00</color></PolyStyle>'); {for Waypoints}
  klist.Add(tab2+'<IconStyle><Icon><href>'+aircrafticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="GrndStn">');               {Pilots track}
  klist.Add(tab2+'<LineStyle>');
  klist.Add(tab4+'<color>FF000000</color>');       {Color black for STxx positions}
  klist.Add(tab4+'<width>2</width>');
  klist.Add(tab2+'</LineStyle>');
  klist.Add('</Style>');

 {Some definitions for placemarks, addressed by #ID}
  klist.Add('<Style id="starting">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+starticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="landing">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+stopicon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="alert">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+alerticon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  if cbMarker.Checked then begin                   {Only needed when Marker set to true}
    klist.Add('<Style id="waypoint">');
    klist.Add(tab2+'<IconStyle><Icon><href>'+wpicon+'</href></Icon></IconStyle>');
    klist.Add('</Style>');
  end;

  klist.Add('<Style id="info">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+infoicon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');

  klist.Add('<Style id="photo">');
  klist.Add(tab2+'<IconStyle><Icon><href>'+fotoicon+'</href></Icon></IconStyle>');
  klist.Add('</Style>');
end;

{https://developers.google.com/kml/documentation/kml_tut
 http://www.zonums.com/gmaps/kml_color/
 http://kml4earth.appspot.com/icons.html     (Icons)
 http://googlegeodevelopers.blogspot.de/2010/07/making-tracks-new-kml-extensions-in.html
 http://gps.hillclimb.de/?page_id=504

 <extrude>1</extrude>}

procedure TForm1.MacheKML(fn: string; z: integer); {Hauptfunktion - konvertieren in KML}
var
  inlist, kmllist, splitlist, outlist, outlist1, placelist: TStringList;
  x, bdt, fmd, lfmd: integer;
  h501vid, h501ph, h501mk: integer;
  n: integer=0;
  dn, skoor, stime, lkoor, ltime: string;
  ts, bg, dt: TDateTime;
  absh, lat, lon, dist: Double;
  lgcy: boolean;

const
  fmdnil=300;                                      {not existent flight mode}

  procedure Timemarker(const lat1, lon1: double);  {Add time marker to KML}
  begin
    dist:=dist+DistanceBetweenTwoCoordinates(lat, lon, lat1, lon1);   {Distance to previous pos}
    lat:=lat1;
    lon:=lon1;
    if dist>tbrDistWP.Position then begin          {Time marker}
      placemark(placelist, '#waypoint', FormatDateTime('nn:ss', ts)+'s', lkoor, ltime);
      dist:=0;                                     {Reset distance}
    end;
  end;

  procedure kmlLegacy;
  begin
    if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and   {Werte validieren}
       testh(StrToFloatN(splitlist[4])) and
       CheckVT(splitlist[gridDetails.Tag+2],
               splitlist[gridDetails.Tag]) and
       GetRFM(splitlist[gridDetails.Tag],
              v_type, true) then begin
      bg:=ZeitToDT(splitlist[0], v_type);
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}

      if cbMarker.Checked then
        TimeMarker(StrToFloatN(splitlist[5]), StrToFloatN(splitlist[6]));

      fmd:=StrToIntDef(splitlist[gridDetails.Tag], fmdnil);
      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf+zzz, ts)+'Z';
      lkoor:=ChrKoor(splitlist[6])+tab1+
             ChrKoor(splitlist[5])+tab1+splitlist[4]; {lon + lat + alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        if rgAltitudeType.ItemIndex=0 then
          absh:=GethFromST10(z, bg);
        skoor:=ChrKoor(splitlist[6])+sep+
               ChrKoor(splitlist[5])+sep+dzfl;     {ohne Höhe}
      end;
      inc(n);
      outlist1.Add(ltime);                         {Zeitstempel}
      outlist.Add(lkoor);                          {Koordinaten}

      if (lfmd<>fmdnil) and
         (fmd<>fmdnil) and
         (fmd<>lfmd) then begin
        case fmd of                                {Additional Placemarks}
          3: placemark(placelist, '#info', 'Angle mode', lkoor, ltime);
          4, 7, 22, 24:  placemark(placelist, '#alert', 'GPS lost', lkoor, ltime);
          12: placemark(placelist, '#alert', 'Emergency', lkoor, ltime);
          6, 21, 23: placemark(placelist, '#info', 'Smart mode', lkoor, ltime);
          26..29: placemark(placelist, '#info', 'Task', lkoor, ltime);
          13, 14: placemark(placelist, '#info', 'RTH', lkoor, ltime);
        end;
      end;
      lfmd:=fmd;                                   {only in real flights}
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

      if cbMarker.Checked then
        TimeMarker(StrToFloatN(splitlist[9]), StrToFloatN(splitlist[10]));

      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf, ts)+'Z';
      lkoor:=ChrKoor(splitlist[10])+tab1+
             ChrKoor(splitlist[9])+tab1+
             splitlist[5];                         {lon + lat + alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=ChrKoor(splitlist[10])+sep+
               ChrKoor(splitlist[9])+sep+dzfl;     {ohne Höhe}
      end;
      inc(n);
      outlist1.Add(ltime);                         {Zeitstempel}
      outlist.Add(lkoor);                          {Koordinaten}
    end;
  end;

  procedure kmlBreeze;                             {eine Datenzeile für Breeze}
  begin
    bg:=ZeitToDT(splitlist[0], v_type);
    if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
       (trim(splitlist[14])<>'0') and              {real flight mode}
//        BrGPSfix(splitlist[20]) and              {GPS fix --- Remove?}
        (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}

      if cbMarker.Checked then
        TimeMarker(BrCoordToFloat(splitlist[12]), BrCoordToFloat(splitlist[13]));

      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf, ts)+'Z';
      lkoor:=BrCoordFormat(splitlist[13])+tab1+
             BrCoordFormat(splitlist[12])+tab1+
             BrTeilen(splitlist[10], 1);           {lon + lat + alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=BrCoordFormat(splitlist[13])+sep+
               BrCoordFormat(splitlist[12])+sep+dzfl;  {ohne Höhe}
      end;
      inc(n);
      outlist1.Add(ltime);                         {Zeitstempelliste}
      outlist.Add(lkoor);                          {Koordinatenliste}
    end;
  end;

  procedure kmlH501;
  var w: integer;

  begin
    bg:=dt+ZeitToDT(splitlist[0], v_type);
    if (NichtLeer(splitlist[2]) or
        NichtLeer(splitlist[3])) and
       (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}

      if cbMarker.Checked then
        TimeMarker(StrToFloatN(splitlist[2]), StrToFloatN(splitlist[3]));

      ltime:=FormatDateTime(dzf, ts)+'T'+
             FormatDateTime(zzf, ts)+'Z';
      lkoor:=ChrKoor(splitlist[3])+tab1+           {lon + lat + alt}
             ChrKoor(splitlist[2])+tab1+
             FormatFloat(dzfl, H501alt(StrToFloatN(splitlist[4])));
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=ChrKoor(splitlist[3])+sep+
               ChrKoor(splitlist[2])+sep+dzfl;     {ohne Höhe}
      end;

      w:=StrToIntDef(splitlist[15], 0);            {Marker}
      if (w>0) and (w<>H501mk) then
        placemark(placelist, '#info', 'marker', lkoor, ltime);
      H501mk:=w;

      w:=StrToIntDef(splitlist[16], 0);            {Video}
      if (w>0) and (w<>H501vid) then
        placemark(placelist, '#photo', 'video', lkoor, ltime);
      H501vid:=w;

      w:=StrToIntDef(splitlist[17], 0);            {Photo}
      if (w>0) and (w<>H501ph) then
        placemark(placelist, '#photo', 'photo', lkoor, ltime);
      H501ph:=w;

      inc(n);
      outlist1.Add(ltime);                         {Zeitstempel}
      outlist.Add(lkoor);                          {Koordinaten}
    end;
  end;

begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  outlist1:=TStringList.Create;
  kmllist:=TStringList.Create;
  placelist:=TStringList.Create;
  splitlist:=TStringList.Create;
  bdt:=1;
  h501vid:=0;                                      {Video nicht gestartet}
  h501ph:=0;                                       {photo}
  h501mk:=0;                                       {Marker}
  lfmd:=fmdnil;                                    {last flight mode, default: nil}
  dt:=0;
  dist:=0;
  lat:=0;
  lgcy:=true;                                      {Additional checks}
  splitlist.Delimiter:=sep;
  case v_type of
    brID: begin
            bdt:=9;                                {Start line for conversion}
            lgcy:=false;
          end;
    h501ID: begin
              splitlist.Delimiter:=csvsep;
              lgcy:=false;
              dt:=GetDateFromFile(lbFlights.Items[z]);
            end;
    MQcsvID: lgcy:=false;
  end;

  splitlist.StrictDelimiter:=True;
  stime:='';
  absh:=0;
  try
    if fn<>'' then begin
      try
        inlist.LoadFromFile(fn);
      except
        StatusBar1.Panels[5].Text:=fn+nixda;
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      end;

      if inlist.count>minlines then
      try
        if lgcy then begin
          splitlist.DelimitedText:=inlist[0];      {Überschrift einlesen, f_mode ermitteln}
          fModeFinden(splitlist);                  {Position f-mode merken}
          splitlist.DelimitedText:=inlist[bdt+1];  {2. Datenzeile, Zeit}
          if (v_type<>YTHPid) then begin           {vehicle type ermitteln}
            v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
            OverWriteVT;                           {Overwrite for PX4 Thunderbird}
          end;
        end;
        splitlist.DelimitedText:=inlist[bdt];      {1. Datenzeile, Zeit}
        ts:=dt+ZeitToDT(splitlist[0], v_type)+nowUTC-now;
        KMLheader(fn, ts, kmllist);
        for x:=bdt to inlist.Count-1 do
          if CheckE7(inlist[x]) then begin
            splitlist.DelimitedText:=inlist[x];
            if splitlist.Count>14 then begin
            case v_type of
              brID: kmlBreeze;
              H501ID: kmlH501;
              MQcsvID: kmlMQcsv;
            else
              kmlLegacy;
            end;
          end else begin
             AppLog.Lines.Add(rsNotUsed+suff+inlist[x]);
          end;
        end;
        placemark(kmllist, '#starting', '', skoor, stime);

        kmllist.Add('<'+pmtag);
        kmllist.Add(write_nme(cbxText.Text));
        kmllist.Add(tab2+'<description>'+ExtractFileName(fn)+'</description>');
        kmllist.Add(tab2+'<styleUrl>#Flightpath</styleUrl>');
        kmllist.Add(tab2+'<gx:Track>');            {Start playable track}
{wenn absolute Höhe aus ST10 kommt und es eingestellt ist}
        if (absh<>0) and
           (rgAltitudeType.ItemIndex=0) then begin
          kmllist.Add(tab4+'<gx:altitudeOffset>'+
                      FormatFloat(dzfl, absh)+
                      '</gx:altitudeOffset>');
          kmllist.Add(tab4+'<'+amtag+rgAltitudeType.Items[0]+'</'+amtag);
        end else begin
          if rgAltitudeType.ItemIndex=2 then
            kmllist.Add(tab4+'<'+amtag+rgAltitudeType.Items[2]+'</'+amtag)
          else
            kmllist.Add(tab4+'<'+amtag+rgAltitudeType.Items[1]+'</'+amtag);
        end;

        if cbExtrude.Checked then
          kmllist.Add(tab4+extru);
        if n>10 then begin
          for x:=0 to outlist.count-1 do           {Timestamps}
            kmllist.add(tab6+'<'+KMLwhen+outlist1[x]+'</'+KMLwhen);
          for x:=0 to outlist.count-1 do           {Coordinates}
            kmllist.add(tab6+'<gx:coord>'+outlist[x]+'</gx:coord>');
        end;
        kmllist.Add(tab2+'</gx:Track>');
        kmllist.Add('</'+pmtag);                   {End playable track}
        if placelist.Count>4 then                  {Additional placemarks}
          for x:=0 to placelist.Count-1 do
            kmllist.Add(placelist[x]);
        placemark(kmllist, '#landing', '', lkoor, ltime); {Landing placemark}

{bei Bedarf den Pfad der ST10/16 auch einspeichern}
        if lgcy and cbPilot.Checked then begin
          dn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+spath+
                             PathDelim+sfile+lbFlights.Items[z]+fext;
          if  FileExists(dn) and (n>10) then begin {RemoteGPS als Pfad}
            try
              inlist.LoadFromFile(dn);
            except
              StatusBar1.Panels[5].Text:=dn+nixda;
              AppLog.Lines.Add(StatusBar1.Panels[5].Text);
            end;
            if inlist.count>5 then begin
              kmllist.Add('<'+pmtag);
              kmllist.Add(write_nme(spath));
              kmllist.Add(tab2+'<description>'+ExtractFileName(dn)+'</description>');
              kmllist.Add(tab2+'<styleUrl>#GrndStn</styleUrl>');
              kmllist.Add(tab2+'<LineString>');
              kmllist.Add(tab4+'<'+amtag+'clampToGround</'+amtag);
              kmllist.Add(tab4+'<'+cotag);
              for x:=1 to inlist.Count-1 do begin
                splitlist.DelimitedText:=inlist[x];
                if (splitlist.Count>5) and           {Werte validieren}
                   (NichtLeer(splitlist[1]) or NichtLeer(splitlist[2])) then
                   kmllist.add(tab6+ChrKoor(splitlist[1])+sep+   {lon + lat}
                               ChrKoor(splitlist[2])+sep+dzfl);  {ohne Höhe}
              end;
              KMLfooter1(cotag, kmllist);
            end;
          end;
        end;           {Ende wenn Pilotenpfad angezeigt werden kann/soll}
        KMLfooter2(kmllist);

        if n>10 then begin                         {kleine Dateien ausblenden}
          dn:=ResultDN(fn, rgOutFormat.Items[0]);  {Always recreate because of settings}
          try
            kmllist.SaveToFile(dn);
            StatusBar1.Panels[5].Text:=dn+tab1+rsSaved;
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
            if rgOutFormat.ItemIndex=1 then
              MacheKMZ(dn);
          except
            StatusBar1.Panels[5].Text:=dn+tab1+rsNotSaved;
            AppLog.Lines.Add('''10667'+suff+StatusBar1.Panels[5].Text);
          end;
        end;
      except
        AppLog.Lines.Add('''10681'+suff+rsInvalid+tab1+rsDS);
      end;
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(outlist);
    FreeAndNil(outlist1);
    FreeAndNil(kmllist);
    FreeAndNil(placelist);
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
    KMLZipper.FileName:=ChangeFileExt(fn, rgOutFormat.Items[1]);
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
  x, bdt: integer;
  n: Integer=0;
  rdn, dn, skoor, stime, lkoor, ltime, lalt: string;
  ts, bg, dt: TDateTime;
  absh, ch: double;
  lgcy: boolean;

  procedure gpxBreeze;
  begin
    bg:=ZeitToDT(splitlist[0], brID);
    if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
       (trim(splitlist[14])<>'0') and              {real flight mode}
        BrGPSfix(splitlist[20]) and
       (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=tab1+'<time>'+FormatDateTime(dzf, ts)+'T'+
                      FormatDateTime(zzf, ts)+'Z</time>';
      lkoor:=tab1+GPXlat+BrCoordFormat(splitlist[12])+GPXlon+
                        BrCoordFormat(splitlist[13])+'">';   {lat + lon}
      lalt:=GPXele+BrTeilen(splitlist[10], 1)+'</ele>';      {alt}
      if stime='' then begin                       {Startpunkt merken}
        stime:=ltime;
        skoor:=lkoor;
      end;
      inc(n);
      outlist.Add(lkoor+lalt+ltime);
    end;
  end;

  procedure gpxH501;
  begin
    bg:=dt+ZeitToDT(splitlist[0], v_type);
    if (NichtLeer(splitlist[2]) or
        NichtLeer(splitlist[3])) and
       (bg>0) then begin
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=tab1+'<time>'+FormatDateTime(dzf, ts)+'T'+
                      FormatDateTime(zzf, ts)+'Z</time>';
      lkoor:=tab1+GPXlat+ChrKoor(splitlist[2])+GPXlon+
                         ChrKoor(splitlist[3])+'">'; {lat + lon}
      lalt:=GPXele+
            FormatFloat(dzfl, H501alt(StrToFloatN(splitlist[4])))+
            '</ele>';                              {alt}

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
        CheckVT(splitlist[gridDetails.Tag+2],
                splitlist[gridDetails.Tag]) and
        GetRFM(splitlist[gridDetails.Tag],
               v_type, true) and
        CheckE7(inlist[x]) then begin
      bg:=ZeitToDT(splitlist[0], v_type);
      ts:=bg+nowUTC-now;                           {UTC Zeitstempel errechnen}
      ltime:=tab1+'<time>'+FormatDateTime(dzf, ts)+'T'+
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
        lalt:=FormatFloat(ctfl, ch);
      except                                       {lalt unverändert übernehmen}
      end;
      lalt:=GPXele+lalt+'</ele>';                  {alt}
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
  rdn:=ResultDN(fn, rgOutFormat.Items[rgOutFormat.ItemIndex]);
  if FileExists(rdn) then
    exit;                                          {Do nothing if file already exists}
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist:=TStringList.Create;
  outlist:=TStringList.Create;
  kmllist:=TStringList.Create;
  splitlist:=TStringList.Create;
  bdt:=1;
  lgcy:=true;                                      {Additional checks}
  dt:=0;                                           {additional days not used}
  splitlist.Delimiter:=sep;
  case v_type of
    brID: begin
            bdt:=9;                                {Start line for conversion}
            lgcy:=false;
          end;
    h501ID: begin
              splitlist.Delimiter:=csvsep;
              lgcy:=false;
              dt:=GetDateFromFile(lbFlights.Items[z]);  {Correction for date}
            end;
  end;
  splitlist.StrictDelimiter:=True;
  stime:='';
  try
    if fn<>'' then begin
      try
        inlist.LoadFromFile(fn);
      except
        StatusBar1.Panels[5].Text:=fn+nixda;
        AppLog.Lines.Add(StatusBar1.Panels[5].Text);
      end;
      if inlist.count>minlines then
      try
        if lgcy then begin
          splitlist.DelimitedText:=inlist[0];      {Überschrift einlesen, f_mode ermitteln}
          fModeFinden(splitlist);                  {Position f-mode merken}
          splitlist.DelimitedText:=inlist[bdt+1];  {2. Datenzeile, Zeit}
          if v_type<>YTHPid then begin             {v_type ermitteln}
            v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
            OverWriteVT;                           {Overwrite for PX4 Thunderbird}
          end;
        end;

        splitlist.DelimitedText:=inlist[bdt];      {1. Datenzeile, Zeit}
        ts:=dt+ZeitToDT(splitlist[0], v_type)+nowUTC-now;
        GPXheader(cbxText.Text, fn, ts, kmllist);
        for x:=bdt to inlist.Count-1 do begin      {Startpunkt ermitteln}
          splitlist.DelimitedText:=inlist[x];
          if splitlist.Count>14 then begin
            case v_type of
              brID: gpxBreeze;                     {Breeze}
              H501ID: gpxH501;
              MQcsvID: gpxMQcsv;                   {Mantis q CSV format}
            else                                   {Yuneec legacy}
              gpxYLegacy;
            end;
          end else begin
            StatusBar1.Panels[5].Text:=rsInvalid;
            AppLog.Lines.Add('''10897'+suff+StatusBar1.Panels[5].Text);
          end;
        end;

        kmllist.Add('<wpt'+skoor);                 {Startpunkt}
        kmllist.Add(tab1+GPXele+'0.0</ele>');
        kmllist.Add(stime);
        kmllist.Add(write_nme('Start'));
        kmllist.Add(GPXet1);

        kmllist.Add('<trk>');
        kmllist.Add(write_nme(ExtractFileName(fn)));
        kmllist.Add(tab2+'<trkseg>');
        if n>10 then for x:=0 to outlist.count-1 do
          kmllist.add(tab4+'<trkpt'+outlist[x]+GPXet3);
        GPXfooter1(kmllist);

        dn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+spath+
                         PathDelim+sfile+lbFlights.Items[z]+fext;
        if cbPilot.Checked and
           FileExists(dn) and
           (n>10) then begin  {RemoteGPS als Pfad}
          try
            inlist.LoadFromFile(dn);
          except
            StatusBar1.Panels[5].Text:=dn+nixda;
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          end;
          if inlist.count>5 then begin
            kmllist.Add('<trk>');
            kmllist.Add(write_nme(ExtractFileName(dn)));
            kmllist.Add(tab2+'<trkseg>');
            for x:=1 to inlist.Count-1 do begin
              splitlist.DelimitedText:=inlist[x];
              if (splitlist.Count>5) and           {Werte validieren}
                 (NichtLeer(splitlist[1]) or NichtLeer(splitlist[2])) then begin
                ts:=ZeitToDT(splitlist[0], v_type)+nowUTC-now;  {UTC Zeitstempel errechnen}
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
        kmllist.Add(write_nme('Stop'));
        kmllist.Add(GPXet1);
        GPXfooter2(kmllist);
        if n>10 then begin                         {kleine Dateien ausblenden}
          try
            kmllist.SaveToFile(rdn);
            StatusBar1.Panels[5].Text:=rdn+tab1+rsSaved;
            AppLog.Lines.Add(StatusBar1.Panels[5].Text);
          except
            StatusBar1.Panels[5].Text:=rdn+tab1+rsNotSaved;
            AppLog.Lines.Add('''10960'+suff+StatusBar1.Panels[5].Text);
          end;
        end;
      except
        AppLog.Lines.Add('''10963'+suff+rsInvalid+tab1+rsDS);
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

{https://support.virtual-surveyor.com/en/support/solutions/articles/1000261351
 https://support.virtual-surveyor.com/en/support/solutions/articles/1000261349}
function TForm1.GethFromST10(const z: integer; const dt: TDateTime): double;
             {Höhe über ellipsoid WGS84 aus RemoteGPS_xxxx}
             {z: Index der Datei, dt: Zeitpunkt wo Höhe genommen werden soll}
var
  inlist: TStringList;
  spl: TStringArray;
  n: Integer=0;
  x: integer;
  hw, hx: double;

begin
  result:=0;
  hw:=0;
  inlist:=TStringList.Create;
  try
    try
      inlist.LoadFromFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+
                          spath+PathDelim+sfile+lbFlights.Items[z]+fext);
      for x:=1 to inlist.count-1 do begin
        spl:=inlist[x].Split(sep);
        if High(spl)>5 then begin
          if ZeitToDT(spl[0], v_type)>dt then begin {Find time (take-off)}
            hx:=StrToFloatN(spl[3]);
            if (abs(hx)>0.1) and testh(hx) then begin  {check if valid value}
              hw:=hw+hx;
              inc(n);
            end;
            if n>40 then
              break;                               {Break after 41 values}
          end;
        end;
      end;
      result:=hw/n-1;                              {Average value from 41 data sets,
                                                    minus alt of the RC controller to ground (1m)}
    except
      result:=0;
      AppLog.Lines.Add('Altitude taken from RC GPS failed! Requested time stamp: '+
                         FormatDateTime(zzf, dt));
    end;
  finally
    if IsNan(result) then
      result:=0;
    FreeAndNil(inlist);
  end;
end;

procedure TForm1.MacheDash(z: integer); {ST2DASH Art, Z: index der Datei
DashWare profile:
http://www.drohnen-forum.de/index.php/Attachment/9815-Yuneec-Q500-Dashware-Profile-v1-5-zip/?s=430b777284562460d8986cbff5c7717c665db155}
var
  inlist, dashlist, splitlist: TStringList;
  x, n: Integer;
  rdn, fn, s: string;
  hgrd, dist, lat1, lat2, lon1, lon2: double;
  bg: TDateTime;

begin
  fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
      kpath+kfile+lbFlights.Items[z]+fext;
  rdn:=ResultDN(fn, rgOutFormat.Items[rgOutFormat.ItemIndex]); {+dashw.csv}
  if FileExists(rdn) then             {Do nothing if target file already exists}
    exit;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist:=TStringList.Create;
  dashlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  hgrd:=0;
  lat1:=0;
  lon1:=0;
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then
    try
      splitlist.DelimitedText:=inlist[0];          {Überschrift einlesen, f_mode ermitteln}
      fModeFinden(splitlist);                      {Position f-mode merken}
      splitlist.DelimitedText:=inlist[2];          {2. Datenzeile, Zeit}
      if v_type<>YTHPid then begin                 {v_type ermitteln}
        v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
        OverWriteVT;                               {Overwrite for PX4 Thunderbird}
      end;
      s:=Trim(inlist[0]);
      s:=StringReplace(s, 'altitude', 'ascent', [rfIgnoreCase]);
      dashlist.Add('distance_from_start'+sep+'datetime'+sep+'altitude'+s);
      for x:=1 to inlist.Count-1 do
        if CheckE7(inlist[x]) then begin
        splitlist.DelimitedText:=inlist[x];
        if (splitlist.Count>anzsp) and             {Konsistenz checken (YTH)}
            CheckVT(splitlist[gridDetails.Tag+2],
                    splitlist[gridDetails.Tag]) and
            testh(StrToFloatN(splitlist[4])) then begin
          try
            if ((NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
               (GetRFM(splitlist[gridDetails.Tag],
                       v_type, true)) or
                cbDashw.Checked) then begin
              lat2:=StrToFloatN(splitlist[5]);
              lon2:=StrToFloatN(splitlist[6]);
              bg:=ZeitToDT(splitlist[0], v_type);
{Startpunkt extra validieren, oben ist noch ein OR !}
              if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
                 GetRFM(splitlist[gridDetails.Tag],
                        v_type, true) then begin
                if (lat1=0) and (lon1=0) then begin  {Startpunkt festlegen}
                  hgrd:=GethFromST10(z, bg);
                  dist:=0;
                  lon1:=lon2;
                  lat1:=lat2;
                end else begin                     {Distanz zum Startpunkt berechnen}
                  dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);   {Entfernung zum Startpunkt}
                end;
              end else begin
                dist:=0;
                if IsMStart(splitlist[gridDetails.Tag], splitlist[gridDetails.Tag+2]) then begin
                  lat1:=0;                         {Startpunkt zurücksetzen}
                  lon1:=0;
                end;
              end;
              s:=FormatFloat(dzfl, dist)+sep+      {distance_from_start}
                 FormatDateTime(vzf+zzz, bg)+sep+
                 FormatFloat(ctfl, StrToFloatN(splitlist[4])+hgrd); {altitude}
              splitlist[8]:=KorrBool(splitlist[8]);         {gps_used}
              splitlist[14]:=KorrSigned(splitlist[14],255); {Motorstatus}
              splitlist[15]:=KorrSigned(splitlist[15],255); {IMU status}
              splitlist[16]:=KorrSigned(splitlist[16],255); {press_comp_status}
              for n:=1 to splitlist.count-1 do
                s:=s+sep+splitlist[n];
              dashlist.add(s);
            end;
          except
           AppLog.Lines.Add(''''+capLabel6+Format('%6d', [x])+   {Datenpunkt ausgeben}
                             suff+StatusBar1.Panels[5].Text);
         end;
        end else begin
          StatusBar1.Panels[5].Text:=rsInvalid;
        end;
      end;
      if dashlist.count>10 then begin
        try
          dashlist.SaveToFile(rdn);
          StatusBar1.Panels[5].Text:=rdn+tab1+rsSaved;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        except
          StatusBar1.Panels[5].Text:=rdn+tab1+rsNotSaved;
          AppLog.Lines.Add('''11130'+suff+StatusBar1.Panels[5].Text);
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
 http://racerender.com/RR3/docs/HowTo-DataInput.html

 not for PX4 and not for H501}

procedure TForm1.MacheRR(z: integer); {RaceRender kompatible Datei}
var
  inlist, dashlist, splitlist: TStringList;
  rdn, fn, s: string;
  dist, lat1, lat2, lon1, lon2: double;
  bg: TDateTime;

  procedure RRBreeze;
  var x: integer;
  begin
    for x:=9 to inlist.count-1 do begin
      splitlist.DelimitedText:=inlist[x];
      if (NichtLeer(splitlist[12]) or NichtLeer(splitlist[13])) and
          BrGPSfix(splitlist[20]) and
         (ZeitToDT(splitlist[0], v_type)>0) and
         ((trim(splitlist[14])<>'0') or cbDashw.Checked) then begin
        bg:=ZeitToDT(splitlist[0], v_type);
        lat2:=BrCoordToFloat(splitlist[12]);
        lon2:=BrCoordToFloat(splitlist[13]);
        if (lat1=0) and (lon1=0) then begin        {Startpunkt festlegen}
          dist:=0;
          lon1:=lon2;
          lat1:=lat2;
        end else begin           {Distanz zum Startpunkt berechnen}
          dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2); {Entfernung zum Startpunkt}
        end;
        s:='0';
        if (StatusToByte(trim(splitlist[20])) and 128)>0 then
          s:='1';                                  {GPS fix}
        dashlist.add(FormatDateTime(zzf+'.zz', bg)+sep+      {Time}
                     BrCoordFormat(splitlist[13])+sep+       {lon}
                     BrCoordFormat(splitlist[12])+sep+       {lat}
                     BrTeilen(splitlist[10], 1)+sep+         {altitude}
                     FormatFloat(dzfl, dist)+sep+            {distance_from_start}
                     s+sep+                                  {gps_update}
                     '0'+sep+                                {Speed}
                     BrTeilen(splitlist[17], 2)+sep+         {Heading (yaw)}
                     BrTeilen(splitlist[16], 2)+sep+         {pitch}
                     BrTeilen(splitlist[15], 2)+sep+         {roll}
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
          CheckVT(splitlist[gridDetails.Tag+2],
                  splitlist[gridDetails.Tag]) and
          testh(StrToFloatN(splitlist[4])) then begin
        try
          if ((NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
             (GetRFM(splitlist[gridDetails.Tag],
                     v_type, true)) or
              cbDashw.Checked) then begin
            lat2:=StrToFloatN(splitlist[5]);
            lon2:=StrToFloatN(splitlist[6]);
            bg:=ZeitToDT(splitlist[0], v_type);
{Startpunkt extra validieren, oben mit cbDashw () relativiert}
            if (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and  {GPS}
               GetRFM(splitlist[gridDetails.Tag],
                      v_type, true) then begin
              if (lat1=0) and
                 (lon1=0) then begin               {Startpunkt festlegen}
                dist:=0;
                lon1:=lon2;
                lat1:=lat2;
              end else begin                       {Distanz zum Startpunkt berechnen}
                dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2);  {Entfernung zum Startpunkt}
              end;
            end else begin
              dist:=0;
              if IsMStart(splitlist[gridDetails.Tag], splitlist[gridDetails.Tag+2]) then begin
                lat1:=0;                           {Startpunkt zurücksetzen}
                lon1:=0;
              end;
            end;
            dashlist.add(FormatDateTime(zzf+'.zz', bg)+sep+      {Time}
                         ChrKoor(splitlist[6])+sep+              {lon}
                         ChrKoor(splitlist[5])+sep+              {lat}
                         splitlist[4]+sep+                       {altitude}
                         FormatFloat(dzfl, dist)+sep+            {distance_from_start}
                         KorrBool(splitlist[8])+sep+             {gps_update}
                         splitlist[7]+sep+                       {Speed}
                         splitlist[12]+sep+                      {Heading (yaw)}
                         splitlist[13]+sep+                      {pitch}
                         splitlist[11]+sep+                      {roll}
                         splitlist[gridDetails.Tag]+sep+         {f_mode}
                         splitlist[1]+sep+                       {RSSI}
                         splitlist[2]+sep+                       {Voltage}
                         splitlist[10]+sep+                      {NumSats}
                         splitlist[gridDetails.Tag+4]);          {Accuracy (gps_accH)}
          end;
        except
        end;
      end else begin
        StatusBar1.Panels[5].Text:=rsInvalid;
      end;
    end;
  end;

begin
  case v_type of
    brID: fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
              lbFlights.Items[z]+bext;
  else                                             {Yuneec legacy}
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+
        kpath+kfile+lbFlights.Items[z]+fext;
  end;
  rdn:=ResultDN(fn, rgOutFormat.Items[rgOutFormat.ItemIndex]); {+_rr.csv}
  if FileExists(rdn) then
    exit;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist:=TStringList.Create;
  dashlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  lat1:=0;
  lon1:=0;
  try
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>2 then
    try
      if v_type<>brID then begin
        splitlist.DelimitedText:=inlist[0];          {Überschrift einlesen, f_mode ermitteln}
        fModeFinden(splitlist);                      {Position f-mode merken}
        splitlist.DelimitedText:=inlist[2];          {2. Datenzeile, Zeit}
        if (v_type<>YTHPid) then begin               {Vehicle type ermitteln}
          v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
          OverWriteVT;                               {Overwrite for PX4 Thunderbird}
        end;
      end;
      dashlist.Add(rrk+'RaceRender Data');
      dashlist.Add(rrk+vtypeToStr(v_type));
      if v_type=brID then dashlist.Add(rrk+trim(inlist[5]));
      dashlist.Add('Time'+sep+'Longitude'+sep+'Latitude'+sep+'Altitude (m)'+sep+
                   'Distance (m)'+sep+'GPS_Update'+sep+'Speed (m/s)'+sep+
                   'Heading'+sep+'Pitch'+sep+'Roll'+sep+'FlightMode'+sep+
                   'RSSI'+sep+'Voltage'+sep+'NumSats'+sep+'Accuracy');
      if v_type=brID then begin             {Breeze}
        RRBreeze;
      end else begin
        RRYLegacy;
      end;                                         {Ende Daten aus Telemetry einlesen}
      if dashlist.count>10 then begin              {Datei speichern}
        try
          dashlist.SaveToFile(rdn);
          StatusBar1.Panels[5].Text:=rdn+tab1+rsSaved;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        except
          StatusBar1.Panels[5].Text:=rdn+tab1+rsNotSaved;
          AppLog.Lines.Add('''11313'+suff+StatusBar1.Panels[5].Text);
        end;
      end;
    except
      AppLog.Lines.Add('''11317'+suff+rsInvalid);
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
    rdn: string;                                   {Result file name for CCC file}

begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  SetLength(ahwp, 0);                              {Array rücksetzen}
  np:=0;
  diralt:=0;
  try
    inlist.LoadFromFile(fn);                       {Telemetriefile laden}
    if inlist.Count>50 then begin
      splitlist.DelimitedText:=inlist[2];          {2. Datenzeile, Zeit}
      if v_type<>YTHPid then begin                 {v_type ermitteln}
        v_type:=StrToIntDef(splitlist[gridDetails.Tag+2], defVT);
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
            CheckVT(splitlist[gridDetails.Tag+2],
                    splitlist[gridDetails.Tag]) and
           (NichtLeer(splitlist[5]) or NichtLeer(splitlist[6])) and
            testh(wpele) and
            GetRFM(splitlist[gridDetails.Tag],
                   v_type, true) then begin
          lat2:=StrToFloatN(splitlist[5]);
          lon2:=StrToFloatN(splitlist[6]);
          dist:=DistanceBetweenTwoCoordinates(lat1, lon1, lat2, lon2); {Entfernung zum letzten Punkt}
          dir:=StrToFloatN(splitlist[12]);         {direction (yaw)}
          if (dist>tbrDistWP.Position) or
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
        AppLog.Lines.Add(''''+capLabel6+Format('%6d', [x])+ {Datenpunkt ausgeben}
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
        rdn:=ResultDN(fn, 'CCC');                  {Always recreate because of settings}
        try
          inlist.SaveToFile(rdn);
          StatusBar1.Panels[5].Text:=rdn+tab1+rsSaved;
          AppLog.Lines.Add(StatusBar1.Panels[5].Text);
        except
          StatusBar1.Panels[5].Text:=rdn+tab1+rsNotSaved;
          AppLog.Lines.Add('''11426'+suff+StatusBar1.Panels[5].Text);
        end;
      end else begin
        StatusBar1.Panels[5].Text:=rsError;
        AppLog.Lines.Add('''11430'+suff+StatusBar1.Panels[5].Text);
      end;
      StatusBar1.Panels[5].Text:=IntToStr(np)+rsNumWP;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  finally
    ahwp:=nil;
    FreeAndNil(splitlist);
    FreeAndNil(inlist);
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.rgQuelleClick(Sender: TObject);   {Quelle wählen}
begin
  if (v_type=MQid) or                              {nichts tun für MantisQ}
     (v_type=H5id) then                            {nichts tun für H520}
       exit;
  if (v_type=YTHPid) and                           {H Plus Sensor files}
     (rgQuelle.ItemIndex=3) then begin
       OpenSensorPlus;
  end else begin
    Anzeige;
  end;
end;

procedure TForm1.rgOutFormatClick(Sender: TObject);    {Format geändert}
begin
  EnSave;
  StatusBar1.Panels[2].Text:=rgOutFormat.Items[rgOutFormat.ItemIndex];
  btnConv.Hint:=hntBitBtn2+' ('+StatusBar1.Panels[2].Text+')';
end;

procedure TForm1.rgSpeedUnitClick(Sender: TObject);    {MPH updaten}
begin
  SelDirAct('');
end;

procedure TForm1.rgAltitudeTypeClick(Sender: TObject);
begin
  EnSave;                                          {Speichern erlauben}
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

procedure TForm1.sbtnLogDirClick(Sender: TObject); {FlightLog Verzeichnis}
begin
  SelDirSet;
end;

procedure TForm1.sbtnScanDirClick(Sender: TObject);   {Dir Protokoll}
begin
  SelDirProt;                  {Verzeichnis zum Durchsuchen auswählen}
end;

procedure TForm1.speLinePathChange(Sender: TObject);   {Liniendicke}
begin
  EnSave;                                          {Speichern erlauben}
end;

procedure TForm1.speProzChange(Sender: TObject);   {Percent to remote value}
begin
  speStk.Value:=ProzToStk(speProz.Value);
end;

procedure TForm1.speProzEditingDone(Sender: TObject);
begin
  speStk.Value:=ProzToStk(speProz.Value);          {Remote value to percent}
end;

procedure TForm1.speStkEditingDone(Sender: TObject);
begin
  speProz.Value:=StkToProz(speStk.Value);
end;

procedure TForm1.StatusToClipboard;                {Statuszeile kopieren}
begin
  if StatusBar1.Tag>0 then                         {Zeiten mit kopieren}
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

procedure TForm1.gridDetailsClick(Sender: TObject);{Kursor im Diagramm anzeigen}
var ts, tb, te: TDateTime;

begin
  Label3.Tag:=gridDetails.Col;                     {selektierte Spalte ermitteln}
  speDataPoint.Value:=gridDetails.Row;             {Zeile übernehmen}
  CellFocus[rgQuelle.ItemIndex, 0]:=gridDetails.Col;   {save cell focus}
  CellFocus[rgQuelle.ItemIndex, 1]:=gridDetails.Row;
  if (rgQuelle.ItemIndex<>3) and
     (gridDetails.ColCount<YTHPcols) then begin    {nicht bei Sensordateien}
    ts:=ZeitToDT(gridDetails.Cells[0, gridDetails.Row], v_type);
    if ts>0 then begin
      if (pcMain.Tag>0) and
         (Form2.Chart1.Visible) then begin
        try
          tb:=ZeitToDT(gridDetails.Cells[0, 1], v_type);
          te:=ZeitToDT(gridDetails.Cells[0, gridDetails.RowCount-1], v_type);
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

procedure TForm1.gridDetailsDblClick(Sender: TObject); {Hilfe zum Zelleninhalt}
var
  num_matches: integer=0;

begin
  if IsLegacyDrone(v_type) and (rgQuelle.ItemIndex=0) and
     ((gridDetails.Col=5) or (gridDetails.Col=6)) then begin
    OpenURL(URLGMap(gridDetails.Cells[5,gridDetails.Row],
                    gridDetails.Cells[6,gridDetails.Row]));
  end else begin
    StatusBar1.Panels[5].Text:=GetCellInfo(gridDetails.Col, gridDetails.Row);
    AppLog.Lines.Add(StatusBar1.Panels[5].Text);     {in AppLogHighlighter aufnehmen}
    if gridDetails.Col>0 then begin
      num_matches:=FilterColumn(gridDetails, gridDetails.Col, gridDetails.Cells[gridDetails.Col, gridDetails.Row]);
      StatusBar1.Panels[5].Text:=IntToStr(num_matches)+'/'+
                                 IntToStr(gridDetails.RowCount-1)+tab4+
                                 StatusBar1.Panels[5].Text;
    end else
      ResetAllFilterColumn(gridDetails);
  end;
end;

procedure TForm1.gridDetailsPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var dta, npos: double;
const mxw=1365;

  procedure Farblauf(f, w: integer);               {Farbabstufung zeichnen}
  var p: integer;                                  {f.. Farbe (index im Array), w..Wert}

  begin
    p:=abs(w);
    if p>mxw then
      p:=mxw;
    CellColorSetting(gridDetails, farbskala[f, p*High(farbskala[0])div mxw]);
  end;

  procedure TelemBreeze;                           {Telemetrie Breeze}
  var e: integer;

  begin
    case aCol of
       2: begin                                    {Flight Mode}
            e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);
            case e of
              2, 4, 5, 7: CellColorSetting(gridDetails, clTasks);
              8:  CellColorSetting(gridDetails, clRTH);
              6:  CellColorSetting(gridDetails, clAngle);  {Pilot}
              10: CellColorSetting(gridDetails, clNoGPS);
            end;
          end;
      14: begin                                    {AutoTakeOff}
            e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);
            Case e of
              2:     CellColorSetting(gridDetails, clSkyBlue); {Flug}
              1, 18: CellColorSetting(gridDetails, clMoneyGreen);
              16:    CellColorSetting(gridDetails, clAttention);
            end;
          end;
      18: if (gridDetails.Cells[aCol, aRow]<>'15') then         {Motorschaden}
            CellColorSetting(gridDetails, clError);
      19: begin                                    {error Zellen einfärben}
            e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);   {Errorflag}
            if (e and 1)<>0 then
              CellColorSetting(gridDetails, clVolt1);
            if (e and 2)<>0 then
              CellColorSetting(gridDetails, clVolt2);
            if (e shr 2)<>0 then
              CellColorSetting(gridDetails, clErrFlag);
          end;
      20: if BrGPSfix(gridDetails.Cells[aCol, aRow]) then
            CellColorSetting(gridDetails, clFairGood);          {Num Sats}
    end;
  end;

  procedure TelemH501;                             {Telemetrie H501}
  var e: integer;
      h, v: double;

  begin
    case aCol of
      1: begin
           e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);    {Frames}
           CellColorSetting(gridDetails, FramesToColor(e));
           exit;
         end;
      4: begin                                     {Elevation}
           try
             if rgSpeedUnit.ItemIndex=2 then
               h:=StrToFloatN(gridDetails.Cells[aCol, aRow])/fft  {in ft}
             else
               h:=StrToFloatN(gridDetails.Cells[aCol, aRow]);     {in m}
             v:=StrToFloatN(GetFNr(gridOverview.Cells[5, lbFlights.ItemIndex+1]));
           except
             h:=0;
           end;
           if (h>1) and
              testh(h) and
              (v>0) and
              (h+0.1>=v) then
             CellColorSetting(gridDetails, clYellow);  {Gipfelhöhe}
           exit;
         end;
      9: begin                                     {Voltage}
           v:=StrToFloatN(gridDetails.Cells[aCol, aRow]);
           VoltToColor(gridDetails, v);
         end;
      18: begin                                    {RSSI, 100% as default}
            e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 100);
            RSSIpToColor(gridDetails, e);
          end;
    end;
  end;

  procedure TelemYTHP;                             {Telemetrie YTH Plus}
  var e: integer;
      h, v: double;

  begin
    if CheckVT(gridDetails.Cells[21, aRow],
               gridDetails.Cells[19, aRow]) then begin {nur mit gültiger vehicle ID}
      case aCol of
         1: if (gridDetails.Cells[aCol, aRow]>'') then begin   {RSSI}
              try
                h:=abs(StrToFloatN(gridDetails.Cells[aCol, aRow]));
              except
                h:=0;                              {Max}
              end;
              CellColorSetting(gridDetails, RSSItoColor(h));
            end;
         4: begin                                  {Höhe}
              try
                if rgSpeedUnit.ItemIndex=2 then
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow])/fft  {in ft}
                else
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow]);     {in m}
                v:=StrToFloatN(GetFNr(gridOverview.Cells[5, lbFlights.ItemIndex+1]));
              except
                h:=0;
              end;
              if (h>1) and
                 testh(h) and
                 (v>0) and
                 (h+0.1>=v) then
                CellColorSetting(gridDetails, clPeaks);           {Gipfelhöhe}
              exit;
            end;
         7: begin                     {True Air Speed, nicht für vSpeed, hSpeed}
              try
                if rgSpeedUnit.ItemIndex=2 then
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow])*fmph  {in mph}
                else
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow])*fkmh; {in km/h}
                v:=StrToFloatN(GetFNr(gridOverview.Cells[8, lbFlights.ItemIndex+1]));
              except
                h:=0;
              end;
              if (aCol=7) and
                 (h>1) and
                 (v>0) and
                 (h+0.1>=v) then                   {Korrekturwert wegen Runden}
                CellColorSetting(gridDetails, clPeaks);         {Topspeed}
              exit;
            end;
        19: begin                                  {Flight mode}
              e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);
              case e of                            {flight modes wie Chart1BarSeries}
                4:   CellColorSetting(gridDetails, clNoGPS);    {Manual Mode, no GPS}
                5:   CellColorSetting(gridDetails, clAngle);
                6:   CellColorSetting(gridDetails, clSmart);
                7:   CellColorSetting(gridDetails, clSport);
                12, 13: CellColorSetting(gridDetails, clRTH);
              end;
            end;
      end;
      if aCol=gridDetails.Tag+4 then begin         {GPS horizontale accuracy}
        try
          h:=StrToFloatN(gridDetails.Cells[aCol, aRow]);
        except
          h:=10;                                   {Max}
        end;
        if h>2.5 then begin
          CellColorSetting(gridDetails, clError);
          exit;
        end;
        if (h<=2.5) and (h>1.8) then begin
          CellColorSetting(gridDetails, clAttention);
          exit;
        end;
        if (h<=1.8) and (h>=1) then begin
          CellColorSetting(gridDetails, clFairGood);
          exit;
        end;
        if (h<1) and
           (h>0) then
          CellColorSetting(gridDetails, clVeryGood);
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
    if gridDetails.Cells[gridDetails.Tag+2, aRow]>'0' then begin {nur mit gültiger vehicle ID}
      case aCol of
         1: if (gridDetails.Cells[aCol, aRow]>'') then begin   {RSSI}
              try
                h:=abs(StrToFloatN(gridDetails.Cells[aCol, aRow]));
              except
                h:=0;                              {Max}
              end;
              if h>0 then
                CellColorSetting(gridDetails, RSSItoColor(h));
            end;
         2: begin                                  {Voltage}
              v:=StrToFloatN(gridDetails.Cells[aCol, aRow]);
              VoltToColor(gridDetails, v);
            end;
         4: begin                                  {Altitude}
              try
                if rgSpeedUnit.ItemIndex=2 then
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow])/fft  {in ft}
                else
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow]);     {in m}
                v:=StrToFloatN(GetFNr(gridOverview.Cells[5, lbFlights.ItemIndex+1]));
              except
                h:=0;
              end;
              if (h>1) and
                 testh(h) and
                 (v>0) and
                 (h+0.1>=v) then
                CellColorSetting(gridDetails, clPeaks);  {Gipfelhöhe}
              exit;
            end;
         7: begin                                  {True Air Speed}
              try
                if rgSpeedUnit.ItemIndex=2 then
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow])*fmph  {in mph}
                else
                  h:=StrToFloatN(gridDetails.Cells[aCol, aRow])*fkmh; {in km/h}
                v:=StrToFloatN(GetFNr(gridOverview.Cells[8, lbFlights.ItemIndex+1]));
              except
                h:=0;
              end;
              if (aCol=7) and
                 (h>1) and
                 (v>0) and
                 (h+0.1>=v) then                   {Korrekturwert wegen Runden}
                CellColorSetting(gridDetails, clPeaks); {Topspeed}
              exit;
            end;
        14: begin                                  {Motorstatus}
              if (gridDetails.Cells[aCol, aRow]<>'15') and
                 (gridDetails.Cells[aCol, aRow]<>'63') and
                 (gridDetails.Cells[aCol, aRow]<>'-1') and
                 (gridDetails.Cells[aCol, aRow]<>'255') then
                CellColorSetting(gridDetails, clError); {Motorschaden}
              exit;
            end;
      end;
      if aCol=(gridDetails.Tag+3) then begin {error Zellen einfärben}
        e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0); {Errorflag}
        if e>0 then begin
          if (e and 1)<>0 then
            CellColorSetting(gridDetails, clVolt1);
          if (e and 2)<>0 then
            CellColorSetting(gridDetails, clVolt2);
          if (e shr 2)<>0 then
            CellColorSetting(gridDetails, clErrFlag);
        end;
        exit;
      end;
      if aCol=gridDetails.Tag then begin           {f_mode Zellen einfärben}
        e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);       {Flight Mode}
        FMColor(gridDetails, e, v_type);           {Flight mode color settings}
        exit;
      end;
      if aCol=gridDetails.Tag+4 then begin         {GPS horizontale accuracy}
        try
          h:=StrToFloatN(gridDetails.Cells[aCol, aRow]);
        except
          h:=10;                                   {Max}
        end;
        if h>2.5 then begin
          CellColorSetting(gridDetails, clError);
          exit;
        end;
        if (h<=2.5) and (h>1.8) then begin
          CellColorSetting(gridDetails, clAttention);
          exit;
        end;
        if (h<=1.8) and (h>=1) then begin
          CellColorSetting(gridDetails, clFairGood);
          exit;
        end;
        if (h<1) and (h>0) then
          CellColorSetting(gridDetails, clVeryGood);
        exit;
      end;
    end;
  end;

  procedure Telemetrie;                            {Farben bei Telemetry je Typ}
  begin
    if gridDetails.Cells[aCol, aRow]>'' then begin
      case v_type of
        brID: TelemBreeze;                         {Breeze}
        H501ID: TelemH501;
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
    if (v_type=YTHPid) and                         {nur bei YTH Plus}
       (aCol=4) then begin                         {SatelliteCount}
      e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 0);
      if e>0 then begin
        if e>10 then begin                         {11 bis x Sats grün}
          CellColorSetting(gridDetails, clVeryGood);
          exit;
        end;
        if e<5 then begin                          {1 bis 4 Sats rot}
          CellColorSetting(gridDetails, clAttention);
          exit;
        end else begin                             {5 bis 10 Sats gelb}
          CellColorSetting(gridDetails, clFairGood);
          exit;
        end;
      end else exit;
    end;
  end;

  procedure RC_Ch;                                 {Farben bei Remote (Channels)}
  var e, p: integer;

  begin
    if gridDetails.Cells[aCol, aRow]>'' then begin
      e:=round(trunc(StrToFloatN(gridDetails.Cells[aCol, aRow])));
      case aCol of
        1: begin
             p:=e-stkntrl;
             if p<0 then Farblauf(1, p)            {nach unten}
                    else Farblauf(0, p);           {nach oben}
             if e=0 then                           {rote Taste}
               CellColorSetting(gridDetails, clAttention);
           end;
        2, 3, 4:
           begin                                   {die restlichen Knüppel}
             p:=e-stkntrl;
             if p<0 then Farblauf(1, p)            {nach unten}
                    else Farblauf(0, p);           {nach oben}
           end;
        5: begin                                   {Flight mode switch}
             if e=stkntrl then begin               {Angle}
               CellColorSetting(gridDetails, clAngle);
               exit;
             end;
             if e=stkup then begin                 {SportMode: blau}
               if v_type=YTHPid then
                 CellColorSetting(gridDetails, clSport)
               else                                {Smart}
                 CellColorSetting(gridDetails, clSmart);
               exit;
             end;
             if e=stkdown then                     {RTH}
               CellColorSetting(gridDetails, clRTH);
          end;
      end;
    end;
  end;

  procedure FarbenSensor;
  begin
    if (aCol<lenfix-3) then
      CellColorSetting(gridDetails, clMoneyGreen);  {die wirklich fixen Bytes}
    if (aCol=lenfix-3) then
      CellColorSetting(gridDetails, clSkyBlue);     {Message ID (0)}
    if gridDetails.ColCount>=YTHPcols then begin    {YTH Plus}
      if (aCol=lenfix-2) or
          (aCol=lenfix-1) then
        CellColorSetting(gridDetails, clSkyBlue);   {Message ID 1 und 2}
      if (aCol=lenfix) then
       CellColorSetting(gridDetails, clMoneyGreen); {Message Name YTH Plus}
      if (aCol=lenfix+1) then
        CellColorSetting(gridDetails, clSilver);    {Längenspalte YTH Plus}
    end else begin
      if (aCol=lenfix-2) then
        CellColorSetting(gridDetails, clSilver);    {Längenspalte andere mit Sensor Datei}
    end;
{und was auch immer noch bei Sensor Datei}
  end;

  procedure FarbenLegacy;
  begin
    if (aCol=0) and
       (aRow>1) then begin                         {Frequenz Timestamps}
      tpos:=ZeitToDT(gridDetails.Cells[0, aRow], v_type);
      npos:=ZeitToDT(gridDetails.Cells[0, aRow-1], v_type);
      dta:=tpos-npos;                              {Delta zur vorigen Zeile}
      if (v_type=brID) or
         (v_type=H501ID) then begin                {Yuneec Breeze/H501 delta timestamps}
        if dta>tsdelta2 then
          CellColorSetting(gridDetails, clAttention);
        if dta>tsdelta3 then
          CellColorSetting(gridDetails, clError);  {5sec für Breeze}
      end else begin                               {andere Kopter}
        if rgQuelle.ItemIndex=1 then begin         {RemoteGPS}
          if dta>tsdelta2 then                     {2sec, für YTH > 1 sec}
            CellColorSetting(gridDetails, clAttention);
          if dta>tsdelta3 then                     {5sec}
            CellColorSetting(gridDetails, clError);
        end else begin                             {Telemetry/Remote}
          if dta>tsdelta1 then                     {600ms}
            CellColorSetting(gridDetails, clAttention);
          if dta>tsdelta2 then
            CellColorSetting(gridDetails, clError); {2sec}
        end;
      end;
      if npos-tpos>tsdelta1 then                   {Zeitrücksprung}
        CellColorSetting(gridDetails, clLime);
      exit;
    end;
    case rgQuelle.ItemIndex of                     {abh. von Dateityp}
      0: Telemetrie;
      1: RC_GPS;
      2: RC_Ch;
    end;
  end;

  procedure FarbenPX4csv;                          {Einfärbungen bei PX4 CSV}
  var e: integer;

  begin
    case aCol of
       1: begin
            e:=StrToIntDef(gridDetails.Cells[aCol, aRow], 100);
            RSSIpToColor(gridDetails, e);
          end;
      59: CellColorSetting(gridDetails, clOlive); {Message ID}
    end;
  end;

(* Alignment of colums in TStringGrid
var
  tsc, tsr: TTextStyle;

// Preparation, once for the whole grid
    tsc:=csvGrid.Canvas.TextStyle;
    tsc.Alignment:=taCenter;
    tsr:=csvGrid.Canvas.TextStyle;
    tsr.Alignment:=taRightJustify;

// Usage, per cell depending on column (or cell)
      csvGrid.Canvas.TextStyle:=tsc;  {centered}
      csvGrid.Canvas.TextStyle:=tsr;  {right justify} *)

begin                                              {Main part}
  if (aRow>0) and                                  {nicht in Überschrift malen}
     (aState=[]) then begin                        {nicht, wenn selected}
    if not odd(aRow) then
      CellColorSetting(gridDetails, clTabs);
    if gridDetails.ColCount=csvanz then begin
      FarbenPX4csv;
    end else begin
      if (gridDetails.ColCount>=YTHPcols) or
         (rgQuelle.ItemIndex=3) then begin         {Sensor}
        FarbenSensor;
      end else begin                               {Rest wie gehabt}
        FarbenLegacy;
      end;
    end;
  end;
end;

procedure TForm1.DiaWerte(p: byte);                {Anzeige Diagramm Werte für Spalte p}
var x: integer;
    w, lat1, lon1, alt1: double;
    bg, lbg: TDateTime;
    s: string;
    vp, zp: boolean;                               {Valid data point, zero datapoint RSSI}

{Die Y-Achsenbezeichnung für die Diagramme entsprechend der Spalten anpassen}
  procedure PrepH501;                              {H501 vorbereiten}
  begin
    case p of                                      {Tom's Hubsan Log Recorder}
      4: s:=s+' [m]';                              {Elevation}
      2, 3: s:=rsDistHome;
      6, 7, 8: s:=s+' [°]';
      9: s:=s+' [V]';
      18: s:=s+' [%]';                             {RSSI}
      19: s:=s+' [m/s]';
    end;
  end;

  {Die Y-Achsenbezeichnung für die Diagramme entsprechend der Spalten anpassen}
  procedure PrepBreeze;                            {Breeze vorbereiten}
  begin
    case p of                                      {Telemetrie Breeze}
      0:  s:=rsSampling;
      10: s:=s+' [m]';
      12, 13: s:=rsDistHome;
      15, 16, 17: s:=s+' [°]';
      21: s:=s+' [%]';                             {Breeze: Restkapazität}
    end;
  end;

  procedure PrepYTHPlus;                           {YTH Plus vorbereiten}
  begin
    if p=0 then begin
      s:=rsSampling;
    end else
    case rgQuelle.ItemIndex of
      0: begin                                     {Telemetry}
            case p of
              1: s:=s+' [dBm]';
              2: s:=s+' [V]';
              3: s:=rsRest+' [%]';
              4: s:=s+' [m]';
              5, 6: s:=rsDistHome;
              7, 24, 25: s:=s+' ['+rgSpeedUnit.Items[rgSpeedUnit.ItemIndex]+']'; {selected speed}
              11, 12, 13: s:=s+' [°]';
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST16}
              1, 2: s:=rsDistHome;
              3: s:=s+' [m]';
              5: s:=s+' [cm]';                     {Accuracy GPS ST16}
              6: s:=s+' ['+rgSpeedUnit.Items[rgSpeedUnit.ItemIndex]+']';
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
    if p=0 then begin
      s:=rsSampling;
    end else
    case rgQuelle.ItemIndex of
      0: begin                                     {Telemetry}
            case p of
              1: s:=s+' [dBm]';
              2: s:=s+' [V]';
              3: if v_type=1
                   then s:=s+' [A]';               {nur H920}
              4: s:=s+' [m]';
              5, 6: s:=rsDistHome;
              7, 24, 25: s:=s+' ['+rgSpeedUnit.Items[rgSpeedUnit.ItemIndex]+']';  {selected speed}
              11, 12, 13: s:=s+' [°]';
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST10/ST16}
              1, 2: s:=rsDistHome;
              3: s:=s+' [m]';
              4: s:=s+' [cm]';                     {Accuracy GPS ST10}
              5: s:=s+' ['+rgSpeedUnit.Items[rgSpeedUnit.ItemIndex]+']';
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
      1, 14, 46, 49, 52: s:=s+' [%]';
      2: s:=s+' [V]';
      3: s:=s+' [A]';
      4, 21, 22, 37..40, 51: s:=s+' [m]';
      5, 6: s:=rsDistHome;
      7, 25, 41..43, 48: s:=s+' [m/s]';
      26..28: s:=s+' [m/s²]';
      11..13: s:=s+' [rad]';
      29..31: s:=s+' [rad/s]';
      32..34: s:=s+' [gauss]';
      35, 36: s:=s+' [mbar]';
      44, 50: s:=s+' [°]';
      45: s:=s+' [°C]';
      47: s:=s+' mAh';
      60..78: s:=s+' [µs]';
    end;
  end;

  function SamplingRegularity: double;             {Spalte 0, Timestamps Deltawert}
  begin
    result:=0;
    if lbg>0 then begin
      result:=(bg-lbg)*secpd*1000;                 {delta in ms}
    end;
    lbg:=bg;
  end;

{Die Zeitachse und Werte entsprechend der Datenformate in den Spalten
 anpassen und Diagramme anlegen}

  procedure ShowDiaBreeze;                         {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(gridDetails.Cells[0, x], v_type); {Zeitstempel}
    w:=StrToFloatN(gridDetails.Cells[p, x]);       {default: Wert einfach übernehmen}
    case p of                                      {Telemetrie Breeze}
      0: w:=SamplingRegularity;
      12, 13: begin                                {Koordinaten}
                 if ((lat1<>0) or (lon1<>0)) and   {Koordinaten vorhanden}
                    BrGPSfix(gridDetails.Cells[20, x]) then begin {GPS-Fix}
                   w:=DistanceBetweenTwoCoordinates(lat1, lon1, BrCoordToFloat(gridDetails.Cells[12, x]),
                                             BrCoordToFloat(gridDetails.Cells[13, x]));
                 end else begin                    {keine gültigen Koordinaten}
                   lat1:=BrCoordToFloat(gridDetails.Cells[12, x]);
                   lon1:=BrCoordToFloat(gridDetails.Cells[13, x]);
                   w:=0;
                 end;
              end;
      10, 15, 16, 17: w:=w/100;                    {Altitude, roll, pitch, yaw}
      20: w:=StatusToByte(gridDetails.Cells[p, x]) and 31;  {Number sats}
      21: w:=BrUmrech(w);                          {Spannung in % beim Breeze}
    end;
  end;

  procedure ShowDiaH501;                           {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(gridDetails.Cells[0, x], v_type); {Zeitstempel}
    w:=StrToFloatN(gridDetails.Cells[p, x]);       {default: Wert einfach übernehmen}
    case p of                                      {Telemetrie H501}
      2, 3: begin
              if x>1 then begin                    {Ignore first line}
                if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                  w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[2, x]),
                                            StrToFloatN(gridDetails.Cells[3, x]));
                end else begin             {Startpunkt erst setzen}
                  if testh(StrToFloatN(gridDetails.Cells[4, x])) then begin
                    lat1:=StrToFloatN(gridDetails.Cells[2, x]);
                    lon1:=StrToFloatN(gridDetails.Cells[3, x]);
                  end;
                  w:=0;
                end;
              end;
            end;
      4: w:=H501alt(w);
      5: w:=H501dist(w);
      19: w:=SpeedX(H501velo(w));
    end;
  end;

  procedure ShowDiaYTHPlus;                        {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(gridDetails.Cells[0, x], v_type); {Zeitstempel}
    case rgQuelle.ItemIndex of                     {Ausgewählte Datei}
      0: begin                                     {Telemetry}
           if CheckVT(gridDetails.Cells[gridDetails.Tag+2, x],
                      gridDetails.Cells[gridDetails.Tag, x]) then begin
              w:=StrToFloatN(gridDetails.Cells[p, x]); {default: Wert einfach übernehmen}
              case p of                            {Liste der Spalten für Dia}
                0: w:=SamplingRegularity;
                2: if w<=5 then                    {Suppress initial values}
                     vp:=false;
                5, 6: begin
                        if x>1 then begin          {1. Zeile ignorieren}
                          if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                            w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[5, x]),
                                                      StrToFloatN(gridDetails.Cells[6, x]));
                          end else begin           {noch kein Startpunkt, Startpunkt setzen}
                            if testh(StrToFloatN(gridDetails.Cells[4, x])) then begin
                              lat1:=StrToFloatN(gridDetails.Cells[5, x]);
                              lon1:=StrToFloatN(gridDetails.Cells[6, x]);
                            end;
                            w:=0;
                          end;
                        end;
                      end;
                7, 24, 25: w:=SpeedX(w);
              end;
              if (p=4) and (not testh(w)) then
                w:=0;                              {Korrektur unplausibler Höhe}
           end else                                {Ende Blödsinn ausblenden}
             vp:=false;                            {Data set not valid}
         end;
      1: begin                                     {RemoteGPS}
            w:=StrToFloatN(gridDetails.Cells[p, x]); {default: Wert einfach übernehmen}
            case p of                              {ST16}
              0: w:=SamplingRegularity;
              1,2: if (lat1<>0) or (lon1<>0) then begin
                     w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[2, x]),
                                   StrToFloatN(gridDetails.Cells[1, x]));
                   end else begin
                     if testh(StrToFloatN(gridDetails.Cells[3, x])) then begin
                       lat1:=StrToFloatN(gridDetails.Cells[2, x]);
                       lon1:=StrToFloatN(gridDetails.Cells[1, x]);
                     end;
                     w:=0;
                   end;
              6: w:=SpeedX(w/100);                 {Maßeinheit Speed unklar, cm/s ?}
            end;
            if (p=3) and (not testh(w)) then
              w:=0;                                {Korrektur unplausibler Werte (Höhe)}
         end;
      2: begin                                     {Remote: nur Angle}
           w:=StrToFloatN(gridDetails.Cells[p, x]); {default: Wert einfach übernehmen}
           case p of
             0: w:=SamplingRegularity;
             7: w:=TiltToGrad(StrToFloatN(gridDetails.Cells[p, x]));
           end;
         end;
    end;
  end;

  procedure ShowDiaYlegacy;                        {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(gridDetails.Cells[0, x], v_type); {Zeitstempel}
    w:=StrToFloatN(gridDetails.Cells[p, x]);       {default: Wert einfach übernehmen}
    if p=0 then
      w:=SamplingRegularity
    else
    case rgQuelle.ItemIndex of                     {Selected file}
      0: begin                                     {Telemetry}
            case p of                              {Liste der Spalten für Dia}
              1: if w=0 then begin                 {Suppress zero values from WiFi}
                   zp:=true;                       {Dont show zero for RSSI (WiFi) but indicate}
                   vp:=false;
                 end;
              3: if v_type=1 then
                   w:=H920Amp(w);                  {Stromsensor bei H920}
              4: if not testh(w) then
                   vp:=false;                      {Korrektur unplausibler Höhe}
              5, 6: begin
                      if x>1 then begin            {1. Zeile ignorieren}
                        if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                          w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[5, x]),
                                                    StrToFloatN(gridDetails.Cells[6, x]));
                        end else begin             {Startpunkt erst setzen}
                          if testh(StrToFloatN(gridDetails.Cells[4, x])) then begin
                            lat1:=StrToFloatN(gridDetails.Cells[5, x]);
                            lon1:=StrToFloatN(gridDetails.Cells[6, x]);
                          end;
                          w:=0;
                        end;
                      end;
                    end;
              7: w:=SpeedX(w);
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST10/ST16}
              1,2: if (lat1<>0) or (lon1<>0) then begin
                     w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[2, x]),
                                   StrToFloatN(gridDetails.Cells[1, x]));
                   end else begin
                     if testh(StrToFloatN(gridDetails.Cells[3, x])) then begin
                       lat1:=StrToFloatN(gridDetails.Cells[2, x]);
                       lon1:=StrToFloatN(gridDetails.Cells[1, x]);
                     end;
                     w:=0;
                   end;
              5: w:=SpeedX(w/100);                 {Maßeinheit Speed unklar, cm/s}
            end;
            if (p=3) and (not testh(w)) then
              vp:=false; {Korrektur unplausibler Werte (Höhe)}
         end;
      2: begin                                     {Remote}
           if p=7 then
             w:=TiltToGrad(StrToFloatN(gridDetails.Cells[p, x]));
         end;
    end;
  end;

  procedure ShowThunderbird;                       {Zeit + Diagramm zeichnen}
  begin
    bg:=ZeitToDT(gridDetails.Cells[0, x], defVT);  {Zeitstempel}
    w:=StrToFloatN(gridDetails.Cells[p, x]);       {default: Wert einfach übernehmen}
    if p=0 then
      w:=SamplingRegularity
    else
    case rgQuelle.ItemIndex of                     {Gewählte Datei}
      0: begin                                     {Telemetry}
            case p of                              {Liste der Spalten für Dia}
              4: if testh(w) and
                    (abs(w-alt1)<20) then begin    {Altitude is plausible}
                   alt1:=w;
                 end else                          {Implausible altitude}
                   w:=alt1;                        {keep previous altitude}
              5, 6: begin
                      if x>1 then begin            {1. Zeile ignorieren}
                        if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                          w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[5, x]),
                                                    StrToFloatN(gridDetails.Cells[6, x]));
                        end else begin             {Startpunkt erst setzen}
                          if testh(StrToFloatN(gridDetails.Cells[4, x])) then begin
                            lat1:=StrToFloatN(gridDetails.Cells[5, x]);
                            lon1:=StrToFloatN(gridDetails.Cells[6, x]);
                          end;
                          w:=0;
                        end;
                      end;
                    end;
              7: w:=SpeedX(w);
            end;
         end;
      1: begin                                     {RemoteGPS}
            case p of                              {ST16}
              1,2: if (lat1<>0) or (lon1<>0) then begin
                     w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[2, x]),
                                   StrToFloatN(gridDetails.Cells[1, x]));
                   end else begin
                     if testh(StrToFloatN(gridDetails.Cells[3, x])) then begin
                       lat1:=StrToFloatN(gridDetails.Cells[2, x]);
                       lon1:=StrToFloatN(gridDetails.Cells[1, x]);
                     end;
                     w:=0;
                   end;
              5: w:=SpeedX(w/100);                 {Possibly cm/s}
            end;
            if (p=3) and (not testh(w)) then
              vp:=false;                           {Nix anzeigen bei unplausiblen Werten (Höhe)}
         end;
      2: begin                                     {Remote}
           if p=7 then w:=TiltToGrad(StrToFloatN(gridDetails.Cells[p, x]));
         end;
    end;
  end;

  procedure ShowDiaPX4csv;                         {Zeit + wert ermitteln}
  begin
    bg:=ScanDateTime(zzf+zzz, gridDetails.Cells[0, x]);
    w:=StrToFloatN(gridDetails.Cells[p, x]);       {default: Wert einfach übernehmen}
    case p of                                      {CSV Datei, Spalte p}
      4: if not testh(w) then
           w:=0;                                   {Korrektur unplausibler Höhe}
      5, 6: begin                                  {Entfernung zum Startpunkt}
              if ((lat1<>0) or (lon1<>0)) then begin {Startpunkt vorhanden}
                w:=DistanceBetweenTwoCoordinates(lat1, lon1, StrToFloatN(gridDetails.Cells[5, x]),
                                          StrToFloatN(gridDetails.Cells[6, x]));
              end else begin                       {Startpunkt erst setzen}
                if testh(StrToFloatN(gridDetails.Cells[4, x])) then begin
                  lat1:=StrToFloatN(gridDetails.Cells[5, x]);
                  lon1:=StrToFloatN(gridDetails.Cells[6, x]);
                end;
                w:=0;
              end;
            end;
    end;
  end;

begin
  if p=0 then begin
    if gridDetails.ColCount=csvanz then exit;      {not for PX4 CSV}
    if v_type=H501ID then exit;                    {Not for Hubsan}
  end;
  if (gridDetails.RowCount>25) then begin          {genügend Zeilen}
    DoForm2Show(0);
    pcMain.Tag:=1;
    w:=0;
    lbg:=0;
    lat1:=0;
    lon1:=0;
    alt1:=0;
    if v_type=ThBid then
      alt1:=GethFromST10(lbFlights.ItemIndex, 0);   {Set to ST16 altitude}
    s:=gridDetails.Cells[p, 0];
    Form2.Caption:=rsChart+' "'+s+'"';
    if rgQuelle.ItemIndex=2 then Form2.Caption:=ChToStr(Form2.Caption, p);
    Form2.gridAnzwerte.Visible:=false;
    if gridDetails.ColCount=csvanz then begin      {eigenes PX4 CSV Format}
      PrepPX4csv;
    end else begin
      case v_type of
        brID: PrepBreeze;                          {Y-Achsen vorbereiten}
        H501ID: PrepH501;
        YTHPid: PrepYTHPlus;                       {YTH Plus}
      else
        PrepYlegacy;
      end;
    end;                                           {Ausgabe in s}
    Form2.Chart1.AxisList[0].Title.Caption:=s;     {y-Achse bezeichnen}
    Form2.Chart1.AxisList[0].LabelSize:=lblsize;   {y-Achse ausrichten}
    Form2.Chart1.Visible:=true;
    Form2.Chart1LineSeries1.Clear;
    Form2.Chart1LineSeries2.Clear;
    Form2.Chart1LineSeries1.Pointer.Visible:=false;
    Form2.Chart1.ZoomFull;
    Form2.Chart1.Tag:=rgQuelle.ItemIndex;          {Tell what type of values}

    Form2.Chart1.DisableRedrawing;
    for x:=2 to gridDetails.RowCount-1 do begin    {skip first line}
      vp:=true;                                    {Default: Datapoint is valid}
      zp:=false;                                   {Not a RSSI zero point (WiFi)}
      if gridDetails.Cells[p, x]<>'' then begin    {skip empty cells}
        if (v_type=ThBid) and                      {only for Thunderbird}
           (p=4) and
           (gridDetails.Cells[p, x]=dzfl) then     {absolute altitude = 0}
          Continue;                                {skip zero altitude}
        try
          if gridDetails.ColCount=csvanz then begin  {PX4 CSV}
            ShowDiaPX4csv;
          end else begin
            case v_type of
              brID: ShowDiaBreeze;                 {Breeze}
              H501ID: ShowDiaH501;
              YTHPid: ShowDiaYTHPlus;              {YTH Plus}
              ThBid: ShowThunderbird;              {H480 Thunderbird, chart per column}
            else                                   {andere Kopter}
              ShowDiaYlegacy;
            end;
          end;
        except
          w:=0;
        end;
        try
          if vp then begin
            Form2.Chart1LineSeries1.AddXY(bg, w);
            if zp then
              Form2.Chart1LineSeries2.AddXY(bg, -1); {Indication for existing WiFi connection from CGOx}
          end;
        except
          Form2.Close;
        end;
      end;
    end;
    Form2.Chart1.EnableRedrawing;
  end;                                             {Ende p>0}
end;

procedure TForm1.ZhlWerte(p: byte);                {Anzeige Statistik Werte}
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
  a: Array of sdat;                                {schneller mit dyn. Array by Corpsman}
  i, j: integer;
  b: boolean;                                      {kein neuer Wert}
  s:    string;                                    {neuer Wert, alter Wert}
  six:  boolean;                                   {Bedingung für Sechs Spalten}

  procedure AusgWe(ag: sdat);                      {Statistikdaten ausgeben}

    procedure vbd;                                 {Ausgabe von - bis - Dauer}
    var dur: TDateTime;

    begin
      Form2.gridAnzwerte.Cells[3, Form2.gridAnzwerte.RowCount-1]:=
        FormatDateTime(zagf, ag.Beginn);
      Form2.gridAnzwerte.Cells[4, Form2.gridAnzwerte.RowCount-1]:=
        FormatDateTime(zagf, ag.Ende);
      dur:=round((ag.Ende-ag.Beginn)*secpd)/secpd;
      Form2.gridAnzwerte.Cells[5, Form2.gridAnzwerte.RowCount-1]:=
        FormatDateTime(dagf, dur);
    end;

    procedure ZhlBreeze;
    begin
      try                                          {3. Spalte füllen}
        case p of
          2:  begin                          {Breeze Flight Mode}
                Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  BrfmodeToStr(StrToInt(ag.Value));
                vbd;
              end;
          11: Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                brIMUStatusToStr(StatusToByte(ag.Value));  {Breeze IMU Status}
          14: begin                         {Breeze AutoTakeOff}
                Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  AutoTakeOffToStr(StrToInt(ag.Value));
                vbd;
              end;
          18: Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                MotStatusToStr(StatusToByte(ag.Value));  {motor_status}
          19: begin                         {error flag}
                Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  eflagToStr(ag.Value);
                vbd;
              end;
        end;
      except
        AppLog.Lines.Add(rsError+' during count of values at Breeze');
      end;
    end;

    procedure ZhlH501;
    begin
      try                                          {3. Spalte füllen}
        case p of
          1:  begin
                Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  FrameToStr(StrToInt(ag.Value));
//                vbd;                             {with times}
              end;
        end;
      except
        AppLog.Lines.Add(rsError+' during count of values at Hubsan Log Recorder');
      end;
    end;

    procedure ZhlYLegacy;
    begin
      if rgQuelle.ItemIndex=0 then                 {Telemetry}
      try
        if p=8 then begin                          {GPS used}
          vbd;
        end;
        if p=14 then begin                         {motor_status}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            MotStatusToStr(StatusToByte(ag.Value));
        end;
        if p=15 then                               {imu_status}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            IMUstatusToStr(StatusToByte(ag.Value));
        if p=gridDetails.Tag-1 then                {press_compass_status}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            PCGstatusToStr(StatusToByte(ag.Value), v_type);
        if p=gridDetails.Tag then begin            {f_mode}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            fmodeToStr(StrToInt(ag.Value));
          vbd;
        end;
        if p=gridDetails.Tag+2 then                {vehicle type}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            vtypeToStr(StrToInt(ag.Value));
        if p=gridDetails.Tag+3 then begin          {error flag}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            eflagToStr(ag.Value);
          vbd;
        end;
      except
        AppLog.Lines.Add(rsError+' during count of values at Telemetry');
      end;
      if rgQuelle.ItemIndex=2 then                 {Remote}
      try
        if (p=5) then begin                        {Flight Mode switch}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            SwitchToStr(p, v_type, ag.Value);
          vbd;
        end;
        if (p=6) or (p=9) or (p=10) then           {Rest}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            SwitchToStr(p, v_type, ag.Value);
        if p=11 then                               {Landing gear}
          Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
            LandGearToStr(ag.Value);
      except
        AppLog.Lines.Add(rsError+' during count of values at Remote');
      end;
    end;

  begin
    Form2.gridAnzwerte.RowCount:=Form2.gridAnzwerte.RowCount+1;
    Form2.gridAnzwerte.Cells[0, Form2.gridAnzwerte.RowCount-1]:=FormSR(ag.Value, 4);
    Form2.gridAnzwerte.Cells[1, Form2.gridAnzwerte.RowCount-1]:=IntToStrFL(ag.Count, 6);
    if gridDetails.ColCount=csvanz then begin      {eigenes PX4 CSV Format}
      try                                          {3. Spalte füllen}
        if ag.Value<>'' then begin                 {leere Zellen nicht auflösen}
          case p of
            15: Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  MSenStat(Hex2Dec('$'+ag.Value)); {Sensor health}
            18: Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  MSTtoStr(Hex2Dec('$'+ag.Value)); {MAV state}
            19: Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  MMFtoStr(Hex2Dec('$'+ag.Value)); {MAV mode flag}
            59: Form2.gridAnzwerte.Cells[2, Form2.gridAnzwerte.RowCount-1]:=
                  MsgIDtoStr(StrToInt(ag.Value));  {MAV message ID}
          end;
        end;
      except
        AppLog.Lines.Add(rsError+' during count of PX4 CSV values');
      end;
    end else begin
      case v_type of
        brID: ZhlBreeze;                           {Breeze}
        H501ID: ZhlH501;
      else
        ZhlYLegacy;                                {all others}
      end;
    end;
  end;

  procedure DreiSpalten;  {zusätzliche Kommentarspalte in Statistik anlegen}
  begin
    Form2.gridAnzwerte.ColCount:=3;                 {Kommentarspalte anfügen}
    Form2.OnResize(Form2);                          {Spalten anpassen}
    Form2.gridAnzwerte.Cells[2, 0]:=rsDescript;     {Beschreibung}
  end;

  procedure SechsSpalten;        {zusätzliche Kommentarspalte + Beginn/Ende}
  begin
    Form2.gridAnzwerte.ColCount:=6;                 {Drei Spalten anfügen}
    Form2.OnResize(Form2);                          {Spalten anpassen}
    Form2.gridAnzwerte.Cells[2, 0]:=rsDescript;     {Beschreibung}
    Form2.gridAnzwerte.Cells[3, 0]:=rsGridCell2;    {Beginn}
    Form2.gridAnzwerte.Cells[4, 0]:=rsGridCell3;    {Ende}
    Form2.gridAnzwerte.Cells[5, 0]:=rsDauer;        {Dauer}
  end;

  procedure CountValues;                           {Zählerspalte versorgen}
  begin
    a[high(a)].Ende:=ZeitToDT(gridDetails.Cells[0, i], v_type);
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
  if pcMain.Tag>0 then
    Form2.Chart1ConstantLine1.Active:=false;
  pcMain.Tag:=0;
  a:=nil;
  for i:=1 to gridDetails.RowCount - 1 do begin
    if (v_type=H501ID) or
       ((CheckVT(gridDetails.Cells[gridDetails.Tag+2, i], {testen auf YTH Plus Fehler}
                gridDetails.Cells[gridDetails.Tag, i])) or
       (gridDetails.ColCount=csvanz) or            {eigenes PX4 CSV Format ohne six}
       (rgQuelle.ItemIndex>0)) then begin          {Test aber nur bei telemetry}
      s:=trim(gridDetails.Cells[p, i]);
      if assigned(a) then begin  {ist array schon einmal initialisiert worden ?}
        six:=false;
        case v_type of
          brID: begin                              {Breeze}
                  case p of
                    2:  six:=true;                 {FlightMode}
                    14: six:=true;                 {AutoTakeOFF}
                    19: six:=true;                 {Error flags}
                  end;
                end;
(*      H501ID: begin
                  if p=1 then six:=true;           {Frames with times}
                end;                              *)
        YTHPid: begin      {YTH Plus}
                  case rgQuelle.ItemIndex of
                    0: begin                       {Telemetry}
                        if p=gridDetails.Tag then
                          six:=true;               {f_mode}
                        if p=8 then
                          six:=true;               {GPS used}
                       end;
                    2: begin                       {Remote}
                         if p=5 then
                           six:=true;              {CH4 Mode Switch}
                       end;
                  end;
                end;
        else begin                                 {Rest Yuneec}
            case rgQuelle.ItemIndex of
              0: begin                             {Telemetry}
                   if p=gridDetails.Tag+3 then
                     six:=true;                    {Error flag}
                   if p=gridDetails.Tag then
                     six:=true;                    {f_mode}
                   if p=8 then
                     six:=true;                    {GPS used}
                 end;
              2: begin                             {Remote}
                   if p=5 then
                     six:=true;                    {CH4 Mode Switch}
                 end;
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
        a[0].Beginn:=ZeitToDT(gridDetails.Cells[0, i], v_type);
        a[0].Ende:=a[0].Beginn;                    {Zeitstempel intialisieren}
      end;
    end;
  end;                                             {Ende Spalte untersuchen}

  DoForm2Show(720);        {Detailfenster anzeigen und mit Statistik füllen}
  Form2.Caption:=rsStatistik+' "'+gridDetails.Cells[p, 0]+'"';
  if rgQuelle.ItemIndex=2 then case p of        {Remote}
     5: Form2.Caption:=Form2.Caption+' - S4 Flight Mode Switch';
     6: Form2.Caption:=Form2.Caption+' - Flight Mode Add';
     9: Form2.Caption:=Form2.Caption+' - S1 Gimbal tilt mode';
    10: Form2.Caption:=Form2.Caption+' - S2 Gimbal pan mode';
    11: Form2.Caption:=Form2.Caption+' - S5 '+rsLandgear;
  end;
  Form2.gridAnzwerte.Visible:=true;
  Form2.Chart1.Visible:=false;
  Form2.edTime.Visible:=false;
  Form2.gridAnzwerte.BeginUpdate;
  Form2.gridAnzwerte.RowCount:=1;
  Form2.gridAnzwerte.ColCount:=2;
  Form2.gridAnzwerte.Cells[0, 0]:=gridDetails.Cells[p, 0];
  Form2.gridAnzwerte.Cells[1, 0]:=rsAnzahl;
  if gridDetails.ColCount=csvanz then begin        {eigenes PX4 CSV Format}
    case p of                                      {muss zu Formatwandlung passen}
      15, 18, 19, 59: DreiSpalten;
    end;
  end else
  case v_type of
    brID: begin                                    {Liste der kommentierten Statistiken, Breeze}
            case p of                              {muss zu Formatwandlung passen}
              11, 18: DreiSpalten;
              2, 14, 19: SechsSpalten;
            end;
          end;
//  H501ID: if p=1 then SechsSpalten;              {with times}

  H501ID: if p=1 then
            DreiSpalten;

  YTHPid: begin                                    {YTH Plus}
            case rgQuelle.ItemIndex of
              0: begin                             {Telemetry}
                   if p=gridDetails.Tag+2 then
                     DreiSpalten;                  {Vehicle Type mit Erklärungen}
                   if p=8 then
                     SechsSpalten;                 {GPS used mit Zeittabelle}
                   if p=gridDetails.Tag then
                     SechsSpalten;                 {fMode}
                 end;
              2: begin                             {Remote}
                   if (p=6) or (p=9) or (p=10) or (p=11) then
                     DreiSpalten;
                   if p=5 then
                     SechsSpalten;                 {Flight mode switch}
                 end;
            end;
          end;
  else begin                                       {Legacy Yuneec}
    case rgQuelle.ItemIndex of
      0: begin                                     {Telemetry}
           if p=8 then
             SechsSpalten;                         {GPS used}
           if p=gridDetails.Tag then
             SechsSpalten;                         {f_mode}
           if p=gridDetails.Tag+3 then
             SechsSpalten;                         {Errorflags}
           if p=14 then
             DreiSpalten;                          {Motor Status}
           if p=15 then
             DreiSpalten;                          {IMU Status}
           if p=gridDetails.Tag-1 then
             DreiSpalten;                          {PressCompasStatus}
           if p=gridDetails.Tag+2 then
             DreiSpalten;                          {Vehicle Type mit Erklärungen}
         end;
      2: begin                                     {Remote}
           if (p=6) or (p=9) or (p=10) or (p=11) then
             DreiSpalten;
           if p=5 then
             SechsSpalten;
         end;
      end;
    end;
  end;
  for i:=0 to high(a) do
    AusgWe(a[i]);                                  {Werte ausgeben}
  Form2.gridAnzwerte.AutoSizeColumns;
  Form2.gridAnzwerte.EndUpdate;

  Form2.Invalidate;
  setlength(a,0);                                  {Array aufräumen}
end;

{Hier wird verzweigt, ob ein Diagramm oder eine Liste angezeigt werden soll.
 Dies muss entsprechend des Inhalts der Spalten angepasst werden.}

procedure TForm1.AnzeigeAddHist(Index: integer);   {Index ist Nummer Spalte}
begin
  if gridDetails.ColCount=csvanz then begin        {PX4 CSV self-def format}
    case index of
      9, 15..20, 57, 59, 60: ZhlWerte(Index);      {Tabelle}
      1..7, 10..14, 21..52, 61..78: DiaWerte(Index);
    end;
  end else                                         {sonstige, wie gehabt}
  case rgQuelle.ItemIndex of
    0: begin
         case v_type of
           brID: begin                             {Yuneec Breeze}
                   case Index of                   {Telemetrie Spalten auswerten}
                     2..9, 11, 14, 18, 19: ZhlWerte(Index);
                     10, 12, 13, 15..17, 20, 21: DiaWerte(Index);
                   end;
                 end;
           H501ID: case index of
                     1: ZhlWerte(Index);
                   else
                     DiaWerte(index);
                   end;
         else begin                                {andere Yuneec Kopter}
           case Index of                           {Telemetrie Spalten auswerten}
             8, 9, 14..20: ZhlWerte(Index);
             0..7, 10..13: DiaWerte(Index);
             else begin                            {variable Spalten}
               if index=gridDetails.Tag+2 then
                 ZhlWerte(Index);
               if index=gridDetails.Tag+3 then
                 ZhlWerte(Index);
               if index=gridDetails.Tag+4 then
                 DiaWerte(Index);
               if index=gridDetails.Tag+5 then
                 DiaWerte(Index);                  {YTH Plus vSpeed}
               if index=gridDetails.Tag+6 then
                 DiaWerte(Index);                  {YTH Plus hSpeed}
             end;
           end;
         end;
         end;

       end;
    1: DiaWerte(Index);                            {ST10 Spalten auswerten}
    2: case Index of                               {Funk Spalten auswerten}
         0..4, 7..8:  DiaWerte(Index);
         5..6, 9..24: ZhlWerte(Index);
       end;
  end;
end;

procedure TForm1.gridDetailsHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if IsColumn and
      (gridDetails.ColCount>defaultcol) and        {wenn Daten vorhanden sind}
      (gridDetails.ColCount<YTHPcols) then begin   {nicht bei YTH Plus Sensor}
    AnzeigeAddHist(Index);
    Label3.Tag:=Index;
  end;
  ResetAllFilterColumn(gridDetails);
end;

procedure TForm1.gridDetailsKeyUp(Sender: TObject; var Key: Word; {Taste im StringGrid}
  Shift: TShiftState);
begin
  topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]:=gridDetails.TopRow; {Top merken}
  if key=vk_F3 then TabSuchen;                     {F3 weitersuchen}
  if key=vk_F4 then TabSelect;                     {F4 Filter setzen}
  if (key=vk_F5) or (key=vk_ESCAPE) then begin     {F5 Filter rücksetzen}
    case v_type of
      brID: BrAnzeigeCSV(0);                       {Breeze}
      H501ID: H501AnzeigeCSV(0);
    else
      AnzeigeCSV(0);                               {andere Yuneec}
    end;
  end;
  if ssCtrl in Shift then begin
    if key=vk_c then begin
      if ssAlt in Shift then
        gridDetails.CopyToClipboard(false)         {alles}
      else gridDetails.CopyToClipboard(true);      {only Selection}
    end;
    if key=vk_f then TabSuchen;                    {Suchen}
    if key=vk_b then SetStartP;                    {Begin point}
    if key=vk_e then SetEndP;                      {End point}
    if key=vk_s then TabSelect;                    {Filterfunktion analog Suche}
    if key=vk_n then ManualCut;                    {Ausschneiden}
    if key=vk_m then CutLegacy(1);                 {Automatisch Ausschneiden}
  end;
end;

{http://www.delphipraxis.net/13528-stringgrid-rechte-mousetaste-reihe-selektieren.html
 Pop-up Menü mit rechter Maustaste aufrufen}
procedure TForm1.gridDetailsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  iCol: integer=1;
  iRow: integer=1;
  grSel: TGridRect;

begin
  gridDetails.MouseToCell(x, y, iCol, iRow);
  if trim(gridDetails.Cells[iCol, iRow])<>'' then begin
    if Button=mbRight then begin                   {rechte Maustaste}
      grSel.Top:=iRow;
      grSel.Left:=0;                               {ganze Zeile markieren}
      grSel.Right:=gridDetails.ColCount;
      grSel.Bottom:=iRow;
      gridDetails.Selection:=grSel;
      PopupMenuTab.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y); {AutoPopup:=false!}
      speDataPoint.Value:=iRow;                    {Zeile für Start/Endpunkt}
    end;
    if Button=mbLeft then begin
      speDataPoint.Value:=iRow;                    {Für Start/Endpunkt}
      if ssCtrl in Shift then cbxSearch.Text:=gridDetails.Cells[iCol, iRow];
    end;              {Linke Maustase und Shift, dann Zelle ins Suchfeld übernehmen}
    topp[lbFlights.ItemIndex, 5]:=gridDetails.TopRow+gridDetails.VisibleRowCount+1;
    topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]:=gridDetails.TopRow; {Top merken}
  end;
end;

procedure TForm1.gridDetailsSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if aRow<>cbxSearch.Tag then
    GroupBox11.Tag:=0;                             {andere Spalte-Suche zurücksetzen}
  cbxSearch.Tag:=aRow;
  cbxSearch.Enabled:=true;
end;

{StringGrid Sortierung siehe
 http://wiki.lazarus.freepascal.org/Grids_Reference_Page#Sorting_Columns_or_Rows}
procedure TForm1.gridOverviewCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);             {bestimmt, wie Spalte sortiert wird}
var f1, f2: double;
begin
  if ACol>4 then begin                             {bereinigt als Float sortieren}
    if TryStrToFloat(GetFNr(gridOverview.Cells[ACol,ARow]), f1) and
       TryStrToFloat(GetFNr(gridOverview.Cells[BCol,BRow]), f2)
    then result:=CompareValue(f1, f2);
  end else result:=CompareText(gridOverview.Cells[ACol,ARow],  {als Text}
                               gridOverview.Cells[BCol,BRow]);
  if gridOverview.SortOrder=soDescending then result:=-result; {Sortierrichtung}
end;

procedure TForm1.gridOverviewDblClick(Sender: TObject);  {Doppelclick zu Tabelle}
begin
  if lbFlights.Items.Count>0 then begin
    case v_type of
      MQid: ShowMQ;                                {MantisQ}
      H5id: ShowH520;                              {eine TLOG Datei H520 anzeigen}
      else begin
        pcMain.ActivePageIndex:=gridOverview.Tag;  {Merker für letztes TabSheet}
        Anzeige;                                   {alle andern Typen}
      end;
    end;
  end;
end;

procedure TForm1.gridOverviewPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  topp[0, 4]:=gridOverview.TopRow;                 {Top merken für Übersicht}
  if aRow>0 then begin
    if not odd(aRow) then
      CellColorSetting(gridOverview, clTabs);      {Jede 2. Zeile grau}
    if (aState=[]) and (aCol>0) then begin         {1. Spalte ausschliessen}
      if aRow<gridOverview.RowCount-1 then begin   {Fußzeile ausschliessen}
        if (topp[aRow-1, 6] and 32)>0 then         {Compass error, ganze Zeile}
          CellColorSetting(gridOverview, clErrFlag);
        if ((topp[aRow-1, 6] and 1)>0) and
           (aCol=10) then                          {Voltage 1}
          CellColorSetting(gridOverview, clVolt1);
        if ((topp[aRow-1, 6] and 2)>0) and
           (aCol=10) then                          {Voltage 2}
          CellColorSetting(gridOverview, clVolt2);
        if ((topp[aRow-1, 6] and 256)>0) and
           (aCol<4) then                           {Emergency}
          CellColorSetting(gridOverview, clEmergency);
      end else begin
        if (v_type<>MQid) and
           (v_type<>H5id) then begin
          CellColorSetting(gridOverview, clMoneyGreen); {Summenzeile}
        end;
      end;
    end;
  end;
end;

{StringGrid Sortierung siehe
 http://wiki.lazarus.freepascal.org/Grids_Reference_Page#Sorting_Columns_or_Rows}
procedure TForm1.gridOverviewHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);                                  {Spalten sortieren}
begin
  if IsColumn then begin
    if gridOverview.SortOrder=soDescending then
      gridOverview.SortOrder:=soAscending
    else
      gridOverview.SortOrder:=soDescending;
    gridOverview.SortColRow(true, Index,
                           gridOverview.FixedRows, gridOverview.RowCount-2);
  end;
end;

procedure TForm1.gridOverviewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=vk_c) and (ssCtrl in Shift) then gridOverview.CopyToClipboard(false);
end;

procedure TForm1.gridOverviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);              {Datei auswählen}
var
  iCol: integer=1;
  iRow: integer=1;
  grSel: TGridRect;
  i: integer;

begin
  if (lbFlights.Items.Count>0) and (Button=mbLeft) then begin  {linke Maustaste}
    gridOverview.MouseToCell(x, y, iCol, iRow);
    grSel.Top:=iRow;
    grSel.Left:=0;                                 {ganze Zeile markieren}
    grSel.Right:=gridOverview.ColCount;
    grSel.Bottom:=iRow;
    gridOverview.Selection:=grSel;
    if iRow<gridOverview.RowCount-1 then           {Summenspalte ausblenden}
      for i:=0 to lbFlights.Items.Count-1 do       {Datei in ListBox auswählen}
        if lbFlights.Items[i]=gridOverview.Cells[0, iRow] then begin
          lbFlights.ItemIndex:=i;                  {Index finden und auswählen}
          break;                                   {fertig, abbrechen}
    end;
  end;
end;

procedure TForm1.gridFirmwareKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);                             {Firmwarestände kopieren}
begin
  if (key=vk_c) and (ssCtrl in Shift) then gridFirmware.CopyToClipBoard(false);
end;

procedure TForm1.gridFirmwareMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var k: integer;                                    {Firmwaretabelle löschen}

begin
  if ssCtrl in Shift then
    for k:=1 to gridFirmware.RowCount-1
      do gridFirmware.Cells[1, k]:='';
end;

procedure TForm1.gridScanResultDblClick(Sender: TObject); {Ergebniszeile}
var fn: string;

begin
  if btnScanErr.Tag=1 then begin                   {Flugbuch}
    if FileExists(SaveDialog1.FileName) then OpenDocument(SaveDialog1.FileName)
      else OpenDocument(IncludeTrailingPathDelimiter(cbxScanDir.Text));
  end else begin                                   {Dateiliste}
    if (gridScanResult.Tag>0) then begin           {Es wurde etwas gefunden}
      if (rgErrType.ItemIndex=10) or
         (rgErrType.ItemIndex=11) then begin
        fn:=gridScanResult.Cells[1, gridScanResult.Tag];
        ShowSensorPlus(fn, 0, cbSensorKML.Checked, true, false, false);
        pcMain.ActivePage:=tabAppLog;              {Springe zum AppLogHighlighter}
      end else
      if (rgErrType.ItemIndex<8) or                {Sensor Dateien}
         (v_type<>YTHPid) then begin               {H Plus}
        fn:=gridScanResult.Cells[1, gridScanResult.Tag];
        cbxLogDir.Text:=GetFlightLogDir(fn);
        SelDirAct(fn);
        cbxSearch.Enabled:=true;                   {Suche erlauben}
        pcMain.ActivePage:=tabDetails;             {Springe zur Dateiansicht}
      end;
    end;
  end;
end;

procedure TForm1.gridScanResultKeyUp(Sender: TObject; var Key: Word; {Strg+C}
  Shift: TShiftState);
begin
  if (key=vk_c) and (ssCtrl in Shift) then
    gridScanResult.CopyToClipboard(false);
end;

procedure TForm1.gridScanResultMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  iCol: integer=1;
  iRow: integer=1;

begin
  gridScanResult.Tag:=0;                           {default: Header}
  if Button=mbLeft then begin                      {linke Maustaste}
    gridScanResult.MouseToCell(x, y, iCol, iRow);
    gridScanResult.Tag:=iRow;                      {Zeile markieren}
  end;
end;

procedure TForm1.gridScanResultResize(Sender: TObject); {Spalte anpassen}
begin
  gridScanResult.ColWidths[1]:=gridScanResult.Width-gridScanResult.ColWidths[0]-30;
end;

procedure TForm1.tabAnalyze3Resize(Sender: TObject); {Höhe Diagramme anpassen}
begin
  Chart3.Height:=tabAnalyze3.Height div 3;
  Chart4.Height:=tabAnalyze3.Height div 3;
  Chart5.Height:=tabAnalyze3.Height div 3;
  Chart4.Width:=Chart3.Width;
  Chart4.Top:=Chart3.Top+Chart3.Height;
end;

procedure TForm1.TimerDblClickTimer(Sender: TObject); {Abfrage Doppelclick Form2}
var tp: TDateTime;
    i: integer;
begin
  if timestr<>'' then begin                        {Tageszeit auslesen}
    try
      tp:=EncodeTime(StrToInt(copy(timestr,1,2)),
                     StrToInt(copy(timestr,4,2)),
                     StrToInt(copy(timestr,7,2)), 0);
    except
      cbxSearch.Text:=timestr;                     {andere Werte in Spalte suchen}
      timestr:='';
      tp:=0;
      TabSuchen;
    end;
    timestr:='';                                   {Aktion zurücksetzen}
    Form2.mnGoTo.Enabled:=false;
    if (tp>0) and (gridDetails.RowCount>10) then begin   {nur wenn es sich lohnt}
      for i:=1 to gridDetails.RowCount-1 do begin   {Zeitstempel suchen}
        if frac(ZeitToDT(gridDetails.Cells[0, i], v_type))>=tp then begin
          speDataPoint.Value:=i;                    {gefunden und springen}
          GoToZ(1);
          break;
        end;
      end;
    end;
    BringToFront;                                  {Hauptfenster nach vorn}
  end;     {nichts tun, wenn kein Zeitstring durch Doppelclick vorhanden ist}
end;

procedure TForm1.TimerDiashowTimer(Sender: TObject); {Diashow Profiles}
begin
  if lbFlights.Items.Count>0 then begin
    SetProfile(TimerDiashow.Tag);
    TimerDiashow.Tag:=TimerDiashow.Tag+1;
    if TimerDiashow.Tag>=cbxProfiles.Items.Count then TimerDiashow.Tag:=0; {wieder von vorn}
  end else TimerDiashow.Enabled:=false;            {Ohne Daten nix anzeigen}
end;

procedure TForm1.tbrSaturationChange(Sender: TObject); {Saturation adjustment}
begin
  EnSave;
  tbrSaturation.SelEnd:=tbrSaturation.Position;
  lblSaturation.Caption:=capLabel4+tab2+IntToStr(tbrSaturation.Position);
end;

procedure TForm1.tbrDistWPChange(Sender: TObject); {Entfernung für Waypoits}
var s: string;
begin
  tbrDistWP.SelEnd:=tbrDistWP.Position;
  s:=IntToStr(tbrDistWP.Position)+'m';
  if rgSpeedUnit.ItemIndex=2 then s:=IntToStr(Round(tbrDistWP.Position/fft))+'ft';
  tbrDistWP.Hint:=s;
  lblDistWP.Caption:=capLabel12+tab2+s;
  btnConv.Enabled:=true;
end;

procedure TForm1.mnGoogleMapClick(Sender: TObject); {Zeige in GoogleMaps}
begin
  if gridDetails.ColCount=csvanz then begin        {Self-dev PX4 CSV format}
    OpenURL(URLGMap(gridDetails.Cells[5, gridDetails.Row],
                    gridDetails.Cells[6, gridDetails.Row]));
    exit;
  end;

  if lbFlights.Items.Count>0 then begin
    case v_type of                                 {Breeze lat/lon Format}
      brID: OpenURL(URLGMap(BrCoordFormat(gridDetails.Cells[12, gridDetails.Row]),
                      BrCoordFormat(gridDetails.Cells[13, gridDetails.Row])));

      H501ID: OpenURL(URLGMap(gridDetails.Cells[2, gridDetails.Row],
                              gridDetails.Cells[3, gridDetails.Row]));
    else
      begin                                        {Andere}
        case rgQuelle.ItemIndex of
          0: OpenURL(URLGMap(gridDetails.Cells[5,gridDetails.Row],
                             gridDetails.Cells[6,gridDetails.Row]));
          1: OpenURL(URLGMap(KoToStr(StrToFloatN(gridDetails.Cells[2, gridDetails.Row])),
                             KoToStr(StrToFloatN(gridDetails.Cells[1, gridDetails.Row]))));
        end;
      end;
    end;
  end;
end;

procedure TForm1.mnHomepageClick(Sender: TObject); {Homepage aufrufen}
begin
  OpenURL(homepage);
end;

procedure TForm1.mnCopyHistClick(Sender: TObject); {Höhenprofil in Zwischenablage}
begin
  Chart1.CopyToClipboardBitmap;                    {Höhenprofil}
end;

procedure TForm1.mnSaveAsHistClick(Sender: TObject); {PopUp Höhenprofil als Datei}
begin
  SaveDialog1.Title:=rsHDiaSave;
  SaveDialog1.FileName:=CleanDN(pcMain.Page[2].Caption+pngdef);
  if SaveDialog1.Execute then
    Chart1.SaveToFile(TPortableNetworkGraphic, SaveDialog1.FileName);
end;

procedure TForm1.mnExploreLogClick(Sender: TObject); {Arbeitsverzeichnis öffnen}
begin
  OpenDocument(IncludeTrailingPathDelimiter(cbxLogDir.Text));
end;

procedure TForm1.mnSelDirLogClick(Sender: TObject); {Menü SelDir}
begin
  SelDirSet;
end;

procedure TForm1.mnProfErrClick(Sender: TObject);  {Menü Profile Errors}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(2);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.mnProfGPSClick(Sender: TObject);  {Menü Profile GPS}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(3);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.mnSelDirRecClick(Sender: TObject); {Menü Protokollverzeichnis}
begin
  SelDirProt;
end;

procedure TForm1.mnProfThrClick(Sender: TObject);  {Menü Profile Throttle}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(4);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.mnProfPitchClick(Sender: TObject); {Menü Profile Pitch}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(5);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.TabSelect;                        {Daten selektieren}
begin
  if (lbFlights.Items.Count>0) and                 {nur wenn Dateien da sind}
     (gridDetails.ColCount<YTHPcols) then begin    {nicht bei YTH Plus Sensor}
    tpos:=0;                                       {Position zurücksetzen}
    cbxSearch.Text:=UpCase(trim(cbxSearch.Text));
    if cbxSearch.Text<>'' then begin               {mit Filter}
      Merkliste(cbxSearch, speItems.Value);        {Suchwerte DropDownListe füllen}
      cbxSearch.Hint:=rsSelection+' in '+gridDetails.Cells[Label3.Tag, 0];
      case v_type of
        brID: BrAnzeigeCSV(1);                     {Breeze Filtermode}
        H501ID: H501AnzeigeCSV(1);
      else
        AnzeigeCSV(1);                             {legacy Yuneec Filtermode}
      end;
    end else                                       {wenn leer, alles anzeigen}
      Case v_type of
        brID: BrAnzeigeCSV(0);                     {Breeze}
        H501ID: H501AnzeigeCSV(0);
      else
        AnzeigeCSV(0);                             {ohne Filter}
      end;
  end;
end;

procedure TForm1.TabSuchen;                        {In Tabelle suchen}
var x: integer;
    grSel: TGridRect;
    s: string;

begin
  cbxSearch.Text:=StringReplace(cbxSearch.Text, sep, '.', []);
  cbxSearch.Text:=UpCase(trim(cbxSearch.Text));
  if cbxSearch.Enabled and (cbxSearch.Text<>'') then begin  {Suchen}
    if cbxSearch.Tag<2 then GroupBox11.Tag:=0;     {oben nix gefunden}
    Merkliste(cbxSearch, speItems.Value);          {DropDownListe füllen}
    cbxSearch.Hint:=hntComboBox9+' in '+gridDetails.Cells[Label3.Tag, 0];
    for x:=cbxSearch.Tag to gridDetails.RowCount-1 do begin
      cbxSearch.Tag:=x+1;
      s:=UpCase(trim(gridDetails.Cells[Label3.Tag, x]));
      if (s=cbxSearch.Text) or                     {kurz -> vollqualifiziert}
         (((s.length>4) or (pos('.', s)>0)) and    {Punkt drin oder lang}
         (pos(cbxSearch.Text, s)>0)) then begin    {teilqualifiziert}
        GroupBox11.Tag:=GroupBox11.Tag+1;          {gefunden pro Spalte merken}
        speDataPoint.Value:=x;                     {Datenpunkt anzeigen}
        grSel.Top:=x;
        grSel.Left:=0;                             {ganze Zeile markieren}
        grSel.Right:=gridDetails.ColCount;
        grSel.Bottom:=x;
        gridDetails.Selection:=grSel;
        if x>8 then gridDetails.TopRow:=x-4        {Abstand zu oben}
               else gridDetails.TopRow:=x;
        break;                                     {gefunden}
      end;
    end;
    s:=' in '+gridDetails.Cells[Label3.Tag, 0];
    if GroupBox11.Tag>0 then begin
      StatusBar1.Panels[5].Text:='"'+cbxSearch.Text+'"'+s;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end else begin
      StatusBar1.Panels[5].Text:='"'+cbxSearch.Text+'"'+nixda+s;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    StatusBar1.Panels[1].Text:=IntToStr(GroupBox11.Tag);  {Anzahl Treffer}
    AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsFound);
    if cbxSearch.Tag>gridDetails.RowCount-2 then cbxSearch.Tag:=1;
  end else GroupBox11.Tag:=0;                      {leer - Suche zurücksetzen}
end;

procedure TForm1.KursorAus;                        {Fadenkreuz aus}
begin
  Chart1ConstantLine1.Active:=false;
  ChartToolset2DataPointCrosshairTool1.CrosshairPen.Visible:=false;
  ChartToolset2DataPointCrosshairTool1.Enabled:=false;
  mnCursorEin.Caption:=capCrossHairOn;
  Chart1.Cursor:=crDefault;
  if cutb>0 then StatusBar1.Panels[3].Text:=FormatDateTime(vzf, cutb);
  if cute>0 then StatusBar1.Panels[4].Text:=FormatDateTime(vzf, cute);
  if (cutb>0) and
     (cute>cutb) then begin                        {Dauer wieder anzeigen}
    btnCut.Enabled:=true;
    StatusBar1.Panels[5].Text:=rsDauer+tab1+FormatDateTime('= nn:ss'+zzz, cute-cutb);
  end;
end;

procedure TForm1.mnCursorEinClick(Sender: TObject);  {Menü: Cursor ein}
begin
  if ChartToolset2DataPointCrosshairTool1.Enabled=false then begin {Fadenkreuz ein}
    ChartToolset2DataPointCrosshairTool1.CrosshairPen.Visible:=true;
    ChartToolset2DataPointCrosshairTool1.Enabled:=true;
    mnCursorEin.Caption:=capCrossHairOff;
    StatusBar1.Panels[5].Text:='';
    Chart1.Cursor:=crCross;
    StatusBar1.Tag:=1;                             {Zeiten zum Kopieren vorhanden}
  end else KursorAus;                              {Fadenkreuz aus}
end;

procedure TForm1.mnCopyTabClick(Sender: TObject);  {Tabelle kopieren}
begin
  if rgQuelle.ItemIndex=3 then gridDetails.CopyToClipboard(false) {Sensor: Alles}
                             else gridDetails.CopyToClipboard(true); {nur Zeile}
end;

procedure TForm1.mnProfRollClick(Sender: TObject); {Menü Profile Roll}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(6);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.mnProfYawClick(Sender: TObject);  {Menü Profile Yaw}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(7);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.mnProf3AxisClick(Sender: TObject); {Menü Profile 3Axis}
begin
  TimerDiashow.Enabled:=false;
  SetProfile(8);
  cbxProfiles.ItemIndex:=0;
end;

procedure TForm1.mnSlideshowClick(Sender: TObject); {Menu Diashow Profiles}
begin
  if (v_type=MQid) or                              {nichts tun für MantisQ}
     (v_type=H5id) or                              {nichts tun für H520}
     (v_type=BrID) then                            {nichts tun für Breeze}
       exit;
  TimerDiashow.Tag:=0;
  TimerDiashow.Enabled:=true;
end;

procedure TForm1.GoToZ(a: integer);                {Gehe zu Zeilennummer, a..freie Zeilen oben}
var grSel: TGridRect;
    z: integer=1;

begin
  if gridDetails.RowCount>1 then begin
    if speDataPoint.Value<gridDetails.RowCount then
      z:=speDataPoint.Value
    else
      z:=gridDetails.RowCount-1;
    gridDetails.Col:=1;
    gridDetails.Row:=z;
    if z>(a*2) then gridDetails.TopRow:=z-a        {Abstand zu oben}
               else gridDetails.TopRow:=z;
    grSel.Top:=z;
    grSel.Left:=0;                                 {ganze Zeile markieren}
    grSel.Right:=gridDetails.ColCount;
    grSel.Bottom:=grSel.Top;
    gridDetails.Selection:=grSel;
    speDataPoint.Value:=z;
    CellFocus[rgQuelle.ItemIndex, 1]:=z;
  end;
  topp[lbFlights.ItemIndex, rgQuelle.ItemIndex]:=gridDetails.TopRow; {Top merken}
end;

procedure TForm1.mnGoToTabClick(Sender: TObject);  {zur Zeilennummer scrollen}
begin
  if lbFlights.Items.Count>0 then
    GoToZ(4);
end;

procedure TForm1.mnAnalyseTabClick(Sender: TObject);  {Datenanalyse im gridDetails}
begin
  if lbFlights.Items.Count>0 then begin
    GoToZ(0);
    Analyse;
  end;
end;

procedure TForm1.mnGoToErrClick(Sender: TObject);  {Gehe zum nächsten Fehler}
var x: integer;

begin
  if rgQuelle.ItemIndex=0 then begin               {nur bei Telemetrie}
    for x:=topp[lbFlights.ItemIndex, 5]+1 to gridDetails.RowCount-1 do begin
      if (StrToIntDef(gridDetails.Cells[gridDetails.Tag+3,x], 0) shr 1)>0 then begin
        if x<gridDetails.RowCount-1 then begin
          speDataPoint.Value:=x-1;
          topp[lbFlights.ItemIndex, 5]:=x+gridDetails.VisibleRowCount+1;
          GoToZ(4);
          gridDetails.Col:=gridDetails.Tag+3;
          CellFocus[rgQuelle.ItemIndex, 0]:=gridDetails.Col;
        end;
        break;
      end;
    end;
  end;
end;

procedure TForm1.mnGoTableClick(Sender: TObject);  {gehe zur Tabelle}
begin
  if lbFlights.Items.Count>0 then begin
    pcMain.ActivePage:=tabDetails;
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
  lbFlights.ItemIndex:=-1;                         {deselektieren}
  lbFlights.Items.Clear;                           {und löschen}
  btnArchive.Enabled:=true;                        {Default: achive function enabled}
  btnScanErr.Enabled:=true;                        {Default: Scan enabled}
  tend:=0;
  SetLength(topp, 2);                              {Topzeilen array}
  n:=0;                                            {Anzahl erstmal Null}
  btnAutoCut.Enabled:=false;
  try
    if FindFirst(IncludeTrailingPathDelimiter(dn)+kpath+
                 kfile+wldcd+fext, faAnyFile, sr) = 0 then
    try                                            {1. Versuch, normale FlightLog Struktur}
      repeat
        lbFlights.Items.Add(GetNr(sr.Name));       {Dateiliste neu aufbauen}
        inc(n);                                    {Dateien zählen}
      until FindNext(sr)<>0;
      if n>0 then begin
        gridOverview.ColWidths[0]:=fw0;            {default}
        gridOverview.Update;
        btnAutoCut.Enabled:=true;
      end;
    finally
      FindClose(sr);
    end;

    if n=0 then begin                              {2. Versuch ohne Unterverzeichnisse}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+kfile+wldcd+
                   fext, faAnyFile, sr) = 0 then try
        repeat
          lbFlights.Items.Add(GetNr(sr.Name));     {Dateiliste neu aufbauen}
          inc(n);                                  {Dateien zählen}
        until FindNext(sr)<>0;
        if n>0 then begin
          kpath:='';                {nur, wenn dort wirklich was gefunden wurde}
          gridOverview.ColWidths[0]:=fw1;
          gridOverview.Update;
          btnAutoCut.Enabled:=true;
        end;
      finally
        FindClose(sr);
      end;
    end;

    if n=0 then begin                              {Test auf log files vom Breeze}
      v_type:=brID;                                {erstmal ID für Breeze setzen
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
            lbFlights.Items.Add(GetNr(sr.Name));   {Dateiliste neu aufbauen}
            inc(n);                                {Dateien zählen}
          end;
        until FindNext(sr)<>0;
        if n>0 then begin    {Breeze: nur, wenn dort wirklich was gefunden wurde}
          gridOverview.ColWidths[0]:=fw1;
          gridOverview.Update;
        end else begin                             {doch kein Breeze}
          GetDefVT;                                {Overwrite for PX4 Thunderbird}
        end;
      finally
        FindClose(sr);
      end;
    end;

    if n=0 then begin                              {noch ein Versuch für Mantis}
      v_type:=MQid;                                {erstmal ID für MantisQ setzen}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+nfile+wldcd, faAnyFile, sr)=0 then
      try
        repeat
          if IsMantisQ(IncludeTrailingPathDelimiter(dn)+sr.Name) then begin
            lbFlights.Items.Add(GetNr(sr.Name));   {Dateiliste neu aufbauen}
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
            lbFlights.Items.Add(GetNr(sr.Name));   {Dateiliste aufbauen}
            inc(n);                                {Dateien zählen}
          end;
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;

      if n>0 then begin    {Mantis: nur, wenn dort wirklich was gefunden wurde}
        gridOverview.ColWidths[0]:=fw1;
        lbFlights.Tag:=-1;                         {noch keine Datei angezeigt}
      end else begin                               {doch kein MantisQ}
        GetDefVT;                                  {Overwrite for PX4 Thunderbird}
      end;
    end;

    if n=0 then begin                              {noch ein Versuch für H520}
      v_type:=H5id;                                {erstmal ID für MantisQ setzen}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+wldcd+hext, faAnyFile, sr)=0 then
      try
        repeat                                     {*.tlog files}
          lbFlights.Items.Add(GetNr(sr.Name));     {Dateiliste neu aufbauen}
          inc(n);                                  {Dateien zählen}
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;

      if n>0 then begin     {H520: nur, wenn dort wirklich was gefunden wurde}
        gridOverview.ColWidths[0]:=fw1;
        lbFlights.Tag:=-1;                         {noch keine Datei angezeigt}
      end else begin                               {doch kein H520}
        GetDefVT;                                  {Overwrite for PX4 Thunderbird}
      end;
    end;

    if n=0 then begin                              {V4.3: Just another try for Hubsan}
      v_type:=H501ID;                              {erstmal ID für H501 setzen}
      if FindFirst(IncludeTrailingPathDelimiter(dn)+h5file+wldcd+fext, faAnyFile, sr)=0 then
      try
        repeat                                     {H501_*.csv files}
          lbFlights.Items.Add(GetNr(sr.Name));     {Dateiliste neu aufbauen}
          inc(n);                                  {Dateien zählen}
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;

      if n>0 then begin     {Hubsan: nur, wenn dort wirklich was gefunden wurde}
        gridOverview.ColWidths[0]:=fw1;
        lbFlights.Tag:=-1;                         {noch keine Datei angezeigt}
        btnArchive.Enabled:=false;                 {no achive function}
        btnScanErr.Enabled:=false;                 {No Scan function}
      end else begin                               {doch kein H520}
        GetDefVT;                                  {Overwrite for PX4 Thunderbird}
      end;
    end;

    if n>0 then begin                              {irgendwas von oben wurde gefunden}
      mnAutoCut.Enabled:=btnAutoCut.Enabled;
      StaticText1.Caption:=VTypeToStr(v_type);
      cbxText.Text:=StaticText1.Caption;           {Drone ID Mantis}
      Merkliste(cbxText, speItems.Value);          {Type merken}
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
      gridOverview.BeginUpdate;
      gridOverview.RowCount:=lbFlights.Items.Count+1; {Übersichtstabelle ohne Summe}
      gridOverview.Row:=1;
      gridOverview.Col:=1;
      for x:=1 to 10 do                            {Spalten}
        for n:=1 to gridOverview.RowCount-1 do     {Zeilen}
          gridOverview.Cells[x, n]:='';            {Inhalte erstmal löschen}
      gridOverview.Cells[0,0]:='';                 {oben links auch löschen}
      btnFlugBuch.Tag:=0;
      for x:=0 to lbFlights.Items.Count-1 do
        gridOverview.Cells[0, x+1]:=lbFlights.Items[x];
      if (v_type<>MQid) and                        {nicht nur Sensor PX4}
         (v_type<>H5id) then begin                 {Übersichtstabelle füllen}
        gridOverview.RowCount:=gridOverview.RowCount+1; {Summenzeile}
        for x:=0 to lbFlights.Items.Count-1 do
          Werte(x);                                {Telemetrie durchsuchen für Übersicht}
        gtm:=0;
        wrt:=0;
        gridOverview.Cells[0, gridOverview.RowCount-1]:=rsSumme;
        for x:=1 to gridOverview.RowCount-2 do begin {Summen anzeigen}
          try
            wrt:=wrt+StrToFloatN(GetFNr(gridOverview.Cells[7, x]));  {Entfernung}
          except
          end;
          try
            if trim(gridOverview.Cells[5, x])<>'' then
              gtm:=gtm+StrToTime('00:'+trim(gridOverview.Cells[4, x]));
          except
          end;
        end;
        p:=trunc(gtm);
        if p>0 then
          gridOverview.Cells[4, gridOverview.RowCount-1]:=IntToStr(p)+'d '+
                              FormatDateTime(zzf, gtm)
               else
          gridOverview.Cells[4, gridOverview.RowCount-1]:=
                              FormatDateTime(zzf, gtm);
        gtm:=gtm*24;                               {in Stunden}
        if rgSpeedUnit.ItemIndex=2 then begin
          wrt:=wrt/5280;
          gridOverview.Cells[7, gridOverview.RowCount-1]:=
                              FormatFloat(ctfl, wrt)+'mi';
          if gtm>0 then                            {avarage speed in mph}
            gridOverview.Cells[8, gridOverview.RowCount-1]:='Ø '+
                              FormatFloat(ctfl, wrt/gtm)+'mph';
        end else begin
          wrt:=wrt/1000;
          gridOverview.Cells[7, gridOverview.RowCount-1]:=
                              FormatFloat(ctfl, wrt)+'km';
          if gtm>0 then                            {avarage speed in km/h}
            gridOverview.Cells[8, gridOverview.RowCount-1]:='Ø '+
                              FormatFloat(ctfl, wrt/gtm)+'km/h';
        end;
        gridOverview.Cells[0,0]:=rsDateien+suff+IntToStr(n);
        StatusBar1.Panels[0].Text:=rsDateien+suff+IntToStr(n);
        tpos:=0;                                   {zeitl. Pos im gridDetails}
        AppLog.Lines.Add('{');                     {Tabelle als Comment für
                                                    SynAnySyn-Highlighter}
        for p:=0 to gridOverview.RowCount-1 do begin  {Übersicht ausgeben}
          s:='';                                   {Datenzeile neu}
          for x:=0 to gridOverview.ColCount-1 do begin
            s:=s+Format('%14.13s', [gridOverview.Cells[x, p]]);  {Datenzeile füllen}
          end;
          AppLog.Lines.Add(s);                     {Ausgeben in AppLogHighlighter}
        end;
        AppLog.Lines.Add('}');
      end;
      GridOverview.EndUpdate;
      GridOverview.AutoSizeColumns;
      lbFlights.ItemIndex:=0;                      {Default: 1. Zeile auswählen}
    end;                                           {Ende Übersichtstabelle}
  except
    n:=0;                                          {nix gefunden}
    AppLog.Lines.Add('''13797'+suff+'Valid files missing in CheckNumTurns');
  end;
  result:=n;                                       {Anzahl übergeben}
end;

procedure TForm1.cbHighLightChange(Sender: TObject); {AppLog Highlighter}
begin
  if cbHighLight.Checked then                      {Switch on HighLighter}
    AppLog.Highlighter:=AppLogHighlighter
  else
    AppLog.HighLighter:=nil;
  AppLog.Invalidate;
end;

procedure TForm1.cbMarkerChange(Sender: TObject); {Time marker changed --> enable convertion}
begin
  btnConv.Enabled:=true;
end;

procedure TForm1.mnHexdumpClick(Sender: TObject);  {Tools Menu Hexdump}
begin
  OpenDialog1.Title:='Select file for '+capHexdump+'...';
  if OpenDialog1.Execute then begin
    HexHeader(OpenDialog1.FileName);               {Block size taken only once per file, changes ingnored}
    HexAusgabe(OpenDialog1.FileName);
  end;
end;

procedure TForm1.btnShowHexClick(Sender: TObject); {Button Hexdump}
begin
  if btnShowhex.Tag=0 then begin                   {No file selected}
    OpenDialog1.Title:='Select file for '+capHexdump+'...';
    if OpenDialog1.Execute then begin              {Select one file for dump}
      HexHeader(OpenDialog1.FileName);
    end;
  end;
  if btnShowhex.Tag>1 then
    HexAusgabe(OpenDialog1.FileName);
end;

procedure TForm1.HexHeader(const fn: string);      {all actions before hexdump}
var fnsize, numblk: integer;
begin
  pcMain.ActivePage:=tabApplog;                    {Switch to AppLogHighlighter}
  AppLog.Lines.Clear;                              {Empty AppLog for hexdump}
  fnsize:=FileSize(fn);
  btnShowHex.Tag:=2048;                            {File selected}
  btnShowHex.Tag:=btnShowHex.Tag shl rgBlockSize.ItemIndex;  {Block size at 1st run}
  numblk:=fnsize div btnShowHex.Tag + 1;
  speBlockNum.Value:=1;
  speBlockNum.MaxValue:=numblk;
  AppLog.Lines.Add(LineEnding);
  StatusBar1.Panels[0].Text:=IntToStr(fnsize);
  StatusBar1.Panels[1].Text:=IntToStr(numblk);
  StatusBar1.Panels[5].Text:=CapHexdump+suff+ExtractFileName(fn);
  AppLog.Lines.Add(StatusBar1.Panels[5].Text);
  AppLog.Lines.Add(rsFilesize+suff+StatusBar1.Panels[0].Text+
                     ' bytes = '+StatusBar1.Panels[1].Text+' blocks');
  AppLog.Lines.Add(LineEnding);
end;

procedure TForm1.HexAusgabe(const fn: string);     {Display a binary file as hex print}
var bytesread, adr, p1, p2, i, zhl: integer;
    instream: TFileStream;
    rwert: array [1..8192] of byte;
    zeile: string;
    zch: char;                                     {Block size in btnShowhex.Tag}

const
  hdrstr1='Relative  Bytes                                             ASCII';
  hdrstr2='address   0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F';
  strstr= '----------------------------------------------------------------------------';
  adrlen=8;                                        {Length addres string}
  numbyte=16;                                      {Number of bytes in one line}

begin
  zeile:='Block No: '+IntToStr(speBlockNum.Value); {write block header}
  AppLog.Lines.Add(zeile);
  AppLog.Lines.Add(hdrstr1);
  AppLog.Lines.Add(hdrstr2);
  AppLog.Lines.Add(strstr);
  zhl:=4;
  instream:=TFileStream.Create(fn, fmOpenRead or fmShareDenyWrite); {file open}
  try
    instream.position:=btnShowhex.Tag*(speBlockNum.Value-1);  {Start address of block}
    bytesread:=instream.Read(rwert, btnShowhex.Tag);
    adr:=0;                                        {Address counter offset}
    p1:=1;                                         {position in buffer for Hex}
    p2:=1;                                         {position in buffer for Char}
    AppLog.BeginUpdate(false);
      repeat                                       {Go through the whole read buffer}
        zeile:=IntToHex(adr+(speBlockNum.Value-1)*btnShowhex.Tag, adrlen)+tab2;
        for i:=1 to numbyte do begin               {Hex dump of one line á 16 bytes}
          if p1<=bytesread then
            zeile:=zeile+IntToHex(rwert[p1], 2)+tab1
          else
            zeile:=zeile+tab2+tab1;                {Fill the rest with spaces}
          inc(adr);
          inc(p1);
        end;

        zeile:=zeile+tab2;
        for i:=1 to numbyte do begin               {ASCII part of the line}
          if rwert[p2] in valchars then
            zch:=chr(rwert[p2])
          else
            zch:='.';                              {not a character}
          if p2<=bytesread then
            zeile:=Zeile+zch;
          inc(p2);
        end;
        AppLog.Lines.Add(zeile);
        inc(zhl);
      until p1>bytesread;                          {all copied bytes read}
      AppLog.Lines.Add(LineEnding);
      AppLog.TopLine:=AppLog.Lines.Count-zhl;
    AppLog.EndUpdate;
    if speBlockNum.Value=speBlockNum.MaxValue then {last block dumped, end file}
      btnShowhex.Tag:=0;                           {next file?}
    speBlockNum.Value:=speBlockNum.Value+1;        {next block}
  finally
    instream.Free
  end;
end;

{###############################################################################
 Für spezielle Sonderauswertungen einer CSV Datei. Diese Sonderfälle müssen extra
 programmiert werden. Hier ist der Rahmen dazu.}

procedure TForm1.btnSpecialClick(Sender: TObject);  {Spezielle Auswertung für CSV-Datei}
begin
//  AuswertungCSVdatei;
//  if OpenDialog1.Execute then TLOGanalysis(OpenDialog1.FileName);
  IMUstatusCheck;
end;


{Sonderauswertung einer CSV-Datei : Hier ist der Rahmen dazu.
 Enable Button Special: Hold Ctrl key during program start-up }

(*
procedure TForm1.AuswertungCSVdatei;               {Spezielle Auswertung für CSV-Datei}
var i, j, zhl: integer;
    inlist, splitlist: TStringList;
    fn, slat, slon, vlat, vlon: string;
    tme, tmev: TDateTime;
    wrt, wrtv, ddist, tdiff, rslt1, rslt2: double;
begin
  screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  gridDetails.ColCount:=0;                         {alles löschen}
  gridDetails.RowCount:=14;
  zhl:=0;
  slat:='';
  vlat:='';
  tmev:=0;
  wrtv:=0;
  ddist:=0;
  rslt1:=0;
  rslt2:=0;
  gridDetails.ColCount:=defaultcol;
  inlist:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  try
    fn:=IncludeTrailingPathDelimiter(cbxLogDir.Text)+kpath+kfile;   {Fake telemetry_xxx.csv}
    fn:=fn+lbFlights.Items[lbFlights.ItemIndex]+fext;
    try
      inlist.LoadFromFile(fn);
    except
      StatusBar1.Panels[5].Text:=fn+nixda;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>1 then begin
      StatusBar1.Panels[5].Text:=fn;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end else begin
      StatusBar1.Panels[5].Text:=fn+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
    if inlist.count>minlines then begin
      try
        splitlist.DelimitedText:=inlist[0];        {Überschrift einlesen}
        speDataPoint.MaxValue:=inlist.Count;
        speNumPoints.MaxValue:=inlist.Count-10;
        speDataPoint.Hint:=hntSpinEdit3+', max. '+IntToStr(speDataPoint.MaxValue);
        StatusBar1.Panels[1].Text:=IntToStr(inlist.count-1);
        AppLog.Lines.Add(StatusBar1.Panels[1].Text+tab1+rsDS);
        gridDetails.RowCount:=inlist.Count;        {vorbelegen}
        gridDetails.ColCount:=splitlist.Count;
        for i:=1 to splitlist.Count-1 do
          gridDetails.Cells[i, 0]:=splitlist[i];
        gridDetails.Cells[0,0]:=rsHDcell0;

        gridDetails.BeginUpdate;
        for i:=1 to inlist.count-1 do begin        {Daten einlesen}
          inc(zhl);
          splitlist.DelimitedText:=inlist[i];
          if splitlist.Count>6 then begin
            for j:=0 to splitlist.count-1 do
              gridDetails.Cells[j, i]:=splitlist[j];

            if (slat='') and                       {noch kein Homepoint}
                NichtLeer(splitlist[5]) and
                NichtLeer(splitlist[6]) then begin
              slat:=splitlist[5];                  {Homepoint speichern}
              slon:=splitlist[6];                  {Homepoint speichern}
              if vlat='' then begin                {previous coordinate}
                vlat:=splitlist[5];
                vlon:=splitlist[6];
              end;
            end;

{spezielle Auswertung hier beginnen}
            tme:=ZeitToDT(splitlist[0], 3);        {Zeit}
            if tmev=0 then
              tmev:=tme;
            tdiff:=tme-tmev;
            wrt:=StrToFloat(splitlist[4]);         {Altitude}
            if wrtv=0 then
              wrtv:=wrt;
            if tdiff>0 then begin
              if vlat<>'' then begin               {only with valid coordinates}
                ddist:=DistanceBetweenTwoCoordinates(StrToFloatN(vlat), StrToFloatN(vlon),
                                  StrToFloatN(splitlist[5]), StrToFloatN(splitlist[6]));

                rslt1:=ddist/tdiff/secpd;          {Ground speed}

                vlat:=splitlist[5];
                vlon:=splitlist[6];
              end;

              rslt2:=(wrt-wrtv)/tdiff/Secpd;       {Vertical speed}

              tmev:=tme;
              wrtv:=wrt;
            end;
            gridDetails.Cells[7, i]:=FormatFloat(ctfl, rslt1);
            gridDetails.Cells[11, i]:=FormatFloat(ctfl, rslt2);
{Ende spezielle Auswertung}
            if i=(inlist.Count div 2) then
              gridDetails.AutoSizeColumns;
          end else begin
            StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsEmpty+tab1+
                                       capLabel6+Format('%6d', [i]);
            AppLog.Lines.Add('''14045'+suff+StatusBar1.Panels[5].Text);
          end;
        end;                                       {Ende Daten einlesen}

        gridDetails.Cells[7, 0]:='grnd_spd';
        gridDetails.Cells[11, 0]:='vert_spd';

        gridDetails.AutoSizeColumn(0);
        gridDetails.EndUpdate;

        AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLGmap(slat, slon));   {Anzeige Start und Ende}
        AppLog.Lines.add(Format('%-10s', [capLabel13+suff])+
                           URLosm(slat, slon));
        AppLog.Lines.Add(LineEnding);
        AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLGmap(splitlist[5], splitlist[6]));
        AppLog.Lines.add(Format('%-10s', [capLabel14+suff])+
                           URLosm(splitlist[5], splitlist[6]));
      except
        StatusBar1.Panels[5].Text:=ExtractFileName(fn)+suff+rsInvalid+tab1+
                                   capLabel6+Format('%6d', [zhl]);
        AppLog.Lines.Add('''14067'+suff+StatusBar1.Panels[5].Text);
      end;
      pcMain.ActivePage:=tabDetails;
      gridDetails.SetFocus;
      gridDetails.CopyToClipboard(false);          {Tabelle im Clipboard ablegen}
    end else begin                                 {Datei leer}
      StatusBar1.Panels[5].Text:=ExtractFileName(fn)+tab1+rsEmpty;
      AppLog.Lines.Add(StatusBar1.Panels[5].Text);
    end;
  finally
    FreeAndNil(inlist);
    FreeAndNil(splitlist);
    Screen.Cursor:=crDefault;
  end;
end; *)


(*
{https://github.com/mavlink/c_library_v2/tree/master/common

 Special analysis TLOG (Beispiel für eigene CSV Listen:
 Hier: Schreibe alle Höhenangaben aus 24, 33 und 141 in eine Tabelle
 Siehe function ShowSensorPlus}

procedure TForm1.TLOGanalysis(fn: string);         {Check & compare altitudes}

var dsbuf: array[0..YTHPcols] of byte;
    i, len, zhl: integer;
    infn: TMemoryStream;
    b: byte;
    csvlist: TStringList;
    s: string;                                     {GPX Ausgabe, homepoint}
    bg: TDateTime;
    csvarr: array[1..csvanz] of string;            {Werte für CSV-Ausgabe}

  procedure SenCSVausgabe;                         {Ausgabe Werte aus Sensor}
  var i: integer;
      c: string;
  begin
    c:=FormatDateTime(zzf+zzz, bg);                {aktueller Zeitstempel}
    for i:=1 to 14 do                              {csv Daten ausgeben}
      c:=c+sep+csvarr[i];
    csvlist.Add(c);
 //   for i:=1 to 14 do csvarr[i]:='';             {ggf. CSV löschen -> Rohdaten}
  end;

  function GetIntFromBuf(const p, a: integer): uint64; {Position/Anzahl Bytes}
  var i: integer;

  begin
    result:=0;
    for i:=0 to a-1 do begin
      result:=result+dsbuf[lenfix+i+p]*(256**i);
    end;
  end;

  function GetFloatFromBuf(const p: integer): double; {Position, Länge immer 4}
  var i: integer;
      wfl: array[0..3] of Byte;
      wx: Single absolute wfl;

  begin
    result:=0;
    for i:=0 to 3 do                               {Endianess prüfen (to/downto)}
      wfl[i]:=dsbuf[lenfix+i+p];                   {4 byte aus Buffer ausschneiden}
    result:=wx;                                    {Typecast mittels absolute}
  end;

 {GPS_RAW_INT 24:
  alt [mm] Altitude (MSL). Positive for up. Note that virtually all GPS modules
           provide the MSL altitude in addition to the WGS84 altitude.}

  procedure GPSAusgabe;                            {GPS_RAW_INT auswerten (24)}
  var tme: uint64;                                 {unsigned Integer}
      lat, lon, ele: integer;                      {int32}

  begin
    tme:=GetIntFromBuf(0, 8);                      {in mysec}
    bg:=tme/Secpd/1000000;                         {Zeitstempel überall verfügbar}
    lat:=GetIntFromBuf(8, 4);                      {uint32}
    lon:=GetIntFromBuf(12, 4);
    ele:=GetIntFromBuf(16, 4);                     {Höhe}

    csvarr[1]:=FormatFloat(ctfl, ele/1000);        {Altitude MSL (51 -> 1)}
    csvarr[10]:=FloatToStr(lat/10000000);
    csvarr[11]:=FloatToStr(lon/10000000);

    SenCSVAusgabe;
  end;

{GLOBAL_POSITION_INT 33:
 alt [mm]          Altitude (MSL). Note that virtually all GPS modules provide
                   both WGS84 and MSL.*/
 relative_alt [mm] Altitude above ground}

  procedure GlobalPosInt;                          {Msg Global_POSTION_INT (33)}
  var tme: uint32;
      lat, lon, altr, ele: integer;                {int32}

  begin
    if dsbuf[lenfix-4]=1 then begin                {Ausgaben nur für AUTOPILOT1}
      tme:=GetIntFromBuf(0, 4);                    {in ms}
      bg:=tme/Secpd/1000;                          {Zeitstempel überall verfügbar}

      lat:=GetIntFromBuf(4, 4);
      lon:=GetIntFromBuf(8, 4);                    {degrees E7  (/10000000)}
      ele:=GetIntFromBuf(12, 4);                   {altitude MSL [mm]}
      altr:=GetIntFromBuf(16, 4);                  {relative alt [mm]}

      csvarr[2]:=FormatFloat(ctfl, ele/1000);      {altitude MSL 51 -> 2}
      csvarr[3]:=FormatFloat(ctfl, altr/1000);     {altitude relative  4 -> 3}
      csvarr[12]:=FloatToStr(lat/10000000);        {Koordinaten}
      csvarr[13]:=FloatToStr(lon/10000000);

      SenCSVAusgabe;                               {CSV Datensatz schreiben}
    end;
  end;

{8 altitude_monotonic [m] This altitude measure is initialized on system boot
                          and monotonic (it is never reset, but represents the
                          local altitude change).
                          The only guarantee on this field is that it will never
                          be reset and is consistent within a flight.
                          The recommended value for this field is the
                          uncorrected barometric altitude at boot time.
                          This altitude will also drift and vary between flights.
12 altitude_amsl     +[m] This altitude measure is strictly above mean sea level
                          and might be non-monotonic (it might reset on events
                          like GPS lock or when a new QNH value is set).
                          It should be the altitude to which global altitude
                          waypoints are compared to.
                          Note that it is *not* the GPS altitude, however,
                          most GPS modules already output MSL by default and
                          not the WGS84 altitude.
16 altitude_local     [m] This is the local altitude in the local coordinate frame.
                          It is not the altitude above home, but in reference to
                          the coordinate origin (0, 0, 0). It is up-positive.
20 altitude_relative +[m] This is the altitude above the home position.
                          It resets on each change of the current home position.
24 altitude_terrain   [m] This is the altitude above terrain. It might be fed
                          by a terrain database or an altimeter.
                          Values smaller than -1000 should be interpreted as unknown.
28 bottom_clearance   [m] This is not the altitude, but the clear space below
                          the system according to the fused clearance estimate.
                          It generally should max out at the maximum range of
                          e.g. the laser altimeter. It is generally a moving target.
                          A negative value indicates no measurement available.}

  procedure Altitude;                              {MAVLINK_MSG_ID_ALTITUDE 141 ($8D)}
  var tme: uint64;                                 {unsigned Integer}
      fval: double;

  begin
    tme:=GetIntFromBuf(0, 8);                      {in µs}
    bg:=tme/secpd/1000000;                         {Zeitstempel überall verfügbar}

    fval:=GetFloatFromBuf(8);                      {altitude_monotonic x -> 4}
    csvarr[4]:=FormatFloat(ctfl, fval);
    fval:=GetFloatFromBuf(12);                     {altitude MSL in m  51 -> 5}
    csvarr[5]:=FormatFloat(ctfl, fval);
    fval:=GetFloatFromBuf(16);                     {altitude_local x -> 6}
    csvarr[6]:=FormatFloat(ctfl, fval);
    fval:=GetFloatFromBuf(20);                     {altitude relative in m  4 -> 7}
    csvarr[7]:=FormatFloat(ctfl, fval);
    fval:=GetFloatFromBuf(24);                     {altitude_terrain x -> 8}
    csvarr[8]:=FormatFloat(ctfl, fval);
    fval:=GetFloatFromBuf(24);                     {bottom_clearance x -> 9}
    csvarr[9]:=FormatFloat(ctfl, fval);

    SenCSVAusgabe;
  end;

  procedure AusgabeSensor;                         {Datenausgabe abh. von MsgID}
  var e: integer;

  begin
    e:=GetIntFromBuf(-3, 3);                       {MsgID 3 Byte als Zahl}
    csvarr[14]:=IntToStr(e);                       {Message ID dezimal hinten}
    inc(zhl);                                      {Datensätze zählen}
    case e of                                      {Ausgabe spez MAVmsg}
      24:  GPSAusgabe;                             {GPS_RAW_INT 24 ($18)}
      33:  GlobalPosInt;                           {GLOBAL_POSITION_INT 33 ($21)}
      141: Altitude;                               {MSG_ID_ALTITUDE 141 ($8D)}
    end;
  end;

begin
  mnGoToErr.Enabled:=false;                        {gehe zum nächsten Fehler blocken}
  zhl:=0;
  s:='';                                           {noch keine Ausgabe}
  bg:=0;                                           {Zeitstempel allg}
  for i:=0 to csvanz do
    csvarr[i]:='';
  if FileSize(fn)>lenfixP then begin
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    FillChar(dsbuf, length(dsbuf), 0);             {Datenbuffer löschen}
    csvlist:=TStringList.Create;                   {Ausgabedatei für csv-Daten}
    infn:=TMemoryStream.Create;
    try
      infn.LoadFromFile(fn);
      AppLog.Lines.Add(LineEnding);
      AppLog.Lines.Add(fn);
      while infn.Position<(infn.Size-lenfixP) do begin {bis zum Ende der Datei}
        len:=0;                                    {Reset for error detection}
        try
          repeat
            b:=infn.ReadByte;
          until (b=dsIDP) or (infn.Position>infn.Size-lenfixP);
          len:=infn.ReadByte;                      {Länge Payload mit CRC}
          infn.ReadBuffer(dsbuf, len+lenfixP-2);   {Länge Rest-Datensatz mit
                                    FixPart, aber ohne $FD und Längen-Byte (-2)}
          AusgabeSensor;                           {alles anzeigen}
        except
          if zhl>0 then
            AppLog.Lines.Add('''Broken record No'''+suff+
                               IntToStr(zhl)+', Byte'+suff+IntToHex(b, 2)+
                               ', Payload length'+suff+IntToStr(len));
{Usually the last record in a tlog file is too short compared to payload length,
 thus this exception will be raised for each file at the end.}
        end;
      end;

      if (csvlist.Count>2) then begin              {CSV Header generieren}
         s:=csvTime+sep+'alt [24]'+sep
            +'alt [33]'+sep+'relative_alt [33]'+sep+
            'altitude_monotonic [141]'+sep+'altitude_amsl [141]'+sep+
            'altitude_local [141]'+sep+'altitude_relative [141]'+sep+
            'altitude_terrain [141]'+sep+'bottom_clearance [141]'+sep+
            'lat [24]'+sep+'lon [24]'+sep+
            'lat [33]'+sep+'lon [33]'+sep+csvMsgID;
        csvlist.Insert(0, s);
        csvlist.SaveToFile(ChangeFileExt(fn, fext));  {als *.csv speichern}
      end;
    finally
      infn.Free;
      csvlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end;
end;  *)

procedure TForm1.IMUstatusCheck;
const
  IMUpos=15;

var
  filelist, splitlist, csvlist, imulist, imutemp: TStringList;
  i: integer;

  procedure IMU_CheckOneTelemetryFile(fn: string);
  var
    i: integer;
    IMUstatus, IMUstatus_old: integer;

  begin
    IMUstatus_old:=256;
    imutemp.Clear;
    csvlist.LoadFromFile(fn);
    if csvlist.Count<25 then                          {Have enough data to do analysis}
      exit;
    splitlist.DelimitedText:=csvlist[8];              {8 --> skip eventually invalid datasets at the beginning}
    if StrToIntDef(splitlist[19], 999)<>DefVT then    {Only for H480 (default flight mode 5)}
      exit;
    imutemp.Add('File: '+ExtractFileName(fn)+'   Date: '+
                 copy(csvlist[8], 1, 4)+'-'+          {Year}
                 copy(csvlist[8], 5, 2)+'-'+          {Month}
                 copy(csvlist[8], 7, 2));             {Day}
    for i:=1 to csvlist.Count-1 do begin
      splitlist.DelimitedText:=csvlist[i];
      if Splitlist.Count>19 then begin
        if StrToIntDef(splitlist[17], 999) in rfm2 then begin   {Only during flight}
          IMUstatus:=StrToIntDef(splitlist[IMUpos],999);
          if IMUstatus<256 then begin
            if IMUstatus<>IMUstatus_old then begin
              imutemp.Add(splitlist[0]+Format('%5d', [IMUstatus])+tab2+
                          IntToBin(IMUstatus, 8, 4)+tab2+'"'+IMUstatusToStr(IMUstatus)+'"');
            end;
            IMUstatus_old:=IMUstatus;
          end;
        end;
      end;
    end;
    imutemp.Add('');
    if imutemp.Count>2 then
      for i:=0 to imutemp.Count-1 do
        imulist.Add(imutemp[i]);
  end;

begin
  filelist:=TStringList.Create;
  csvlist:=TStringList.Create;
  imulist:=TStringList.Create;
  imutemp:=TStringList.Create;
  splitlist:=TStringList.Create;
  splitlist.Delimiter:=sep;
  splitlist.StrictDelimiter:=True;
  Screen.Cursor:=crHourGlass;
  try
    FindAllFiles(filelist, cbxLogDir.Text, kfile+wldcd+fext);
    filelist.Sort;
    StatusBar1.Panels[0].Text:=IntToStr(filelist.Count);
    for i:=0 to filelist.Count-1 do begin
      StatusBar1.Panels[1].Text:=IntToStr(i+1);
      IMU_CheckOneTelemetryFile(filelist[i]);
    end;
    StatusBar1.Panels[5].Text:='IMU analysis done';
    imulist.SaveToFile(IncludeTrailingPathDelimiter(cbxLogDir.Text)+'IMU_Status_list.txt');
  finally
    filelist.Free;
    csvlist.Free;
    imulist.Free;
    imutemp.Free;
    splitlist.Free;
    Screen.Cursor:=crDefault;
  end;
end;

end.

