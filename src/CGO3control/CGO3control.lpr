program CGO3control;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, CGO3control_main;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='Yuneec FlightLog Auswertung';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

