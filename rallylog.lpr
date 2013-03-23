program rallylog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pl_excontrols, appwindow, SynchedThreads, rl_commsmanager,
  rl_msgevents, serialdevice,
rl_firmata, ftd2xx, rl_connecteddevice,
  pl_luicontrols;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

