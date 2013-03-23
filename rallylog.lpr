program rallylog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pl_excontrols, appwindow, SynchedThreads, rl_commsmanager,
  rl_msgevents, serialdevice,
rl_firmata, remoteutils, ftd2xx, ConnectedDevice,
  pl_luicontrols;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

