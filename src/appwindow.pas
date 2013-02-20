unit appwindow;

{$mode objfpc}{$H+}

interface

uses
  remoteutils, firmata, rallylogserialmanager, rallyLogEvents, Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }
  TForm1 = class(TForm,IRallyLogEventListener)
    btn_Connect: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblRemoteDate: TLabel;
    lblRemoteTime: TLabel;
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
   if(fComManager.isConnected) then
      begin
        fComManager.disconnect();
        btn_Connect.Caption:='Connect';
      end
   else
      begin
        fComManager.connect();
        fComManager.requestDeviceId();
        btn_Connect.Caption:='DisConnect';
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
         TFirmata.RSP_REPORT_ID: lblID.Caption:=inttostr(event.Values[TFirmata.VAL_ID]);     // ID Label
         TFirmata.RSP_REPORT_RTC:
         begin
              lblID.Caption := inttostr(event.Values[TFirmata.VAL_ID]);     // ID Label
              lblRemoteTime.Caption:= inttostr(event.Values[TFirmata.VAL_RTC_HOUR]) + ':'
                                      + inttostr(event.Values[TFirmata.VAL_RTC_MIN]) + ':'
                                      + inttostr(event.Values[TFirmata.VAL_RTC_SEC]);
              lblRemoteDate.Caption:= inttostr(event.Values[TFirmata.VAL_RTC_DAY]) + '/'
                                      + inttostr(event.Values[TFirmata.VAL_RTC_MONTH]) + '/'
                                      + inttostr(event.Values[TFirmata.VAL_RTC_YEAR]);
         end;
    end;
 end;

end.

