unit appwindow;

{$mode objfpc}{$H+}

interface

uses
  firmata, rallylogserialmanager, rallyLogEvents, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }
  TForm1 = class(TForm,IRallyLogEventListener)
    btn_Connect: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblID: TLabel;
    procedure btn_ConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    { private declarations }
    fComManager: TCommunicationManager;
    procedure handleRallyLogEvent(event: TRallyLogEvent);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   fComManager := TCommunicationManager.Create();
   fComManager.addSysexEventListener(self);
end;

procedure TForm1.btn_ConnectClick(Sender: TObject);
begin
   if(btn_Connect.Caption = 'Connect') then
      begin
        fComManager.disconnect();
        btn_Connect.Caption:='DisConnect';
      end
   else
      begin
        fComManager.connect();
        btn_Connect.Caption:='Connect';
      end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   fComManager.free();
end;


// handleRallylogEvent
// callback when a sysex message is received and processed by the ComManager
// This will contain values requested or broadcast from the Device and is
// used to update the GUI fields
 procedure TForm1.handleRallyLogEvent(event: TRallyLogEvent);
 begin
      // update controls when we receive a response event
    case event.Command of
         TFirmata.RSP_REPORT_ID: lblID.Caption:=char(event.Values[0]);      // ID Label
    end;
 end;

end.

