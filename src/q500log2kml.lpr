program q500log2kml;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols, q500log2kml_main, anzwerte,
  Splash_sc;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='Yuneec FlightLog Auswertung';
  RequireDerivedFormResource := True;
  Application.Initialize;
{Splash screen vor Hauptprogramm anzeigen}
  Form4:=TForm4.Create(nil);
  Form4.Show;
  Form4.Refresh;
  Application.ProcessMessages;

  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

