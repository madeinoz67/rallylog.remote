unit appwindow;

{$mode objfpc}{$H+}

interface

uses
  remoteutils, rl_firmata, rl_commsmanager, rallyLogEvents, Classes,
  SysUtils, FileUtil, JLabeledDateTimeEdit, cySimpleGauge, TplLEDIndicatorUnit,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls;

type

  { TForm1 }
  TForm1 = class(TForm,IRallyLogEventListener)
    btn_SyncTime: TButton;
    btnSetID: TButton;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblBattery: TLabel;
    lblRemoteDate: TLabel;
    lblLocalDate: TLabel;
    lblRemoteTime: TLabel;
    lblID: TLabel;
    lblLocalTime: TLabel;
    plLED1: TplLEDIndicator;
    spineditID: TSpinEdit;
    Timer1: TTimer;
    procedure btnSetIDClick(Sender: TObject);
    procedure btn_SyncTimeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  private
    { private declarations }
    fComManager: TCommunicationManager;
    procedure handleRallyLogEvent(event: TRallyLogEvent);
    procedure handleDeviceConnectEvent(sender: TObject);
    procedure handleDeviceDisConnectEvent(sender: TObject);
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
   fComManager.ConnectedDevice.onDeviceConnected:= @handleDeviceConnectEvent;
   fComManager.ConnectedDevice.onDeviceDisconnected:= @handleDeviceDisConnectEvent;

end;

procedure TForm1.btn_SyncTimeClick(Sender: TObject);
begin
   if(fComManager.isConnected) then
   begin
        fComManager.syncTimeDate();
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
     fComManager.disconnect();
     Application.Terminate;
end;


procedure TForm1.btnSetIDClick(Sender: TObject);
begin
  if (fComManager.isConnected) then
     fComManager.setId(spineditID.Value);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   fComManager.free();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lblLocalTime.caption:= TimeToStr(time);
  lblLocalDate.Caption:= DateToStr(date);
end;


// handleRallylogEvent
// callback when a sysex message is received and processed by the ComManager
// This will contain values requested or broadcast from the Device and is
// used to update the GUI fields
 procedure TForm1.handleRallyLogEvent(event: TRallyLogEvent);
 var
   voltage: Integer;
   strMin, strSec, strDay, strMth: String;
 begin
      // update controls when we receive a response event
    case event.Command of
         TFirmata.RSP_REPORT_ID: lblID.Caption:=inttostr(event.Values[TFirmata.VAL_ID]);     // ID Label
         TFirmata.RSP_REPORT_BAT: lblBattery.Caption := inttostr((event.Values[Tfirmata.VAL_BAT_HI] *256) + event.Values[Tfirmata.VAL_BAT_LO]);
         TFirmata.RSP_REPORT_RTC:
         begin
              voltage := (event.Values[Tfirmata.VAL_BAT_HI] *256) + event.Values[Tfirmata.VAL_BAT_LO];
              with plLed1 do
              begin
                  plLed1.Position:= trunc((100 * (voltage - 500)  ) / 400) ;    // scale to display 5.0-9.0v = 0-100%
                   if (voltage < 750) AND (voltage > 650) then
                      Foreground:=clYellow
                   else if (voltage <= 650) then
                      Foreground:=clRed
                   else
                     Foreground := clLime;
              end;

              lblID.Caption := inttostr(event.Values[TFirmata.VAL_ID]);     // ID Label
              lblBattery.Caption := floattostr(voltage/100) ;

              // Pad some of our returned RTC values for display
              if (event.Values[TFirmata.VAL_RTC_MIN] < 10) then
                 strMin := '0' + inttostr(event.Values[TFirmata.VAL_RTC_MIN])
              else
                strMin := inttostr(event.Values[TFirmata.VAL_RTC_MIN]);

              if (event.Values[TFirmata.VAL_RTC_SEC] < 10) then
                 strSec := '0' + inttostr(event.Values[TFirmata.VAL_RTC_SEC])
              else
                strSec := inttostr(event.Values[TFirmata.VAL_RTC_SEC]);

              if (event.Values[TFirmata.VAL_RTC_DAY] < 10) then
                 strDay := '0' + inttostr(event.Values[TFirmata.VAL_RTC_DAY])
              else
                strDay := inttostr(event.Values[TFirmata.VAL_RTC_DAY]);

              if (event.Values[TFirmata.VAL_RTC_MONTH] < 10) then
                 strMth := '0' + inttostr(event.Values[TFirmata.VAL_RTC_MONTH])
              else
                strMth := inttostr(event.Values[TFirmata.VAL_RTC_MONTH]);


              lblRemoteTime.Caption:= inttostr(event.Values[TFirmata.VAL_RTC_HOUR]) + ':'
                                      + strMin + ':'
                                      + strSec;
              lblRemoteDate.Caption:= strDay + '/'
                                      + strMth + '/'
                                      + inttostr(event.Values[TFirmata.VAL_RTC_YEAR]);
         end;
    end;
 end;

 procedure TForm1.handleDeviceConnectEvent(sender: TObject);
 begin
     fComManager.connect();
     btn_SyncTime.Enabled:=true;
     btnSetID.Enabled:=true;
 end;

 procedure TForm1.handleDeviceDisConnectEvent(sender: TObject);
 begin
      fComManager.disconnect();
      lblRemoteTime.Caption:='';
      lblRemoteDate.Caption:='';
      lblID.Caption:='';
      lblBattery.Caption:='0';
      plLed1.Position:=0;
      btn_SyncTime.Enabled:=false;
      btnSetID.Enabled:=false;
 end;

end.

