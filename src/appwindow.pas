unit appwindow;

{$mode objfpc}{$H+}

interface

uses
  remoteutils, firmata, rallylogserialmanager, rallyLogEvents, Classes,
  SysUtils, FileUtil, cySimpleGauge, TplLEDIndicatorUnit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Spin;

type

  { TForm1 }
  TForm1 = class(TForm,IRallyLogEventListener)
    btn_Connect: TButton;
    btnSetID: TButton;
    btnBat: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lblBattery: TLabel;
    lblRemoteDate: TLabel;
    lblRemoteTime: TLabel;
    lblID: TLabel;
    plLED1: TplLEDIndicator;
    spineditID: TSpinEdit;
    procedure btnBatClick(Sender: TObject);
    procedure btnSetIDClick(Sender: TObject);
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

procedure TForm1.btnSetIDClick(Sender: TObject);
begin
  if (fComManager.isConnected) then
     fComManager.setId(spineditID.Value);
end;

procedure TForm1.btnBatClick(Sender: TObject);
begin
  if (fComManager.isConnected) then
     fComManager.requestBattery();
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
 var
   voltage: Integer;
 begin
      // update controls when we receive a response event
    case event.Command of
         TFirmata.RSP_REPORT_ID: lblID.Caption:=inttostr(event.Values[TFirmata.VAL_ID]);     // ID Label
         TFirmata.RSP_REPORT_BAT: lblBattery.Caption := inttostr(event.Values[Tfirmata.VAL_BAT_HI]);
         TFirmata.RSP_REPORT_RTC:
         begin
              voltage := (event.Values[Tfirmata.VAL_BAT_HI] *256) + event.Values[Tfirmata.VAL_BAT_LO];
              with plLed1 do
              begin
                   plLed1.Position:= trunc((voltage / 960) * 100);
                   if (voltage > 700) AND (voltage < 600) then
                      Foreground:=clYellow
                   else if (voltage <= 600) then
                      Foreground:=clRed
                   else
                     Foreground := clLime;
              end;
              lblID.Caption := inttostr(event.Values[TFirmata.VAL_ID]);     // ID Label
              lblBattery.Caption := inttostr(voltage) ;
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

