unit appwindow;

{$mode objfpc}{$H+}

interface

uses
  remoteutils, firmata, rallylogserialmanager, rallyLogEvents, Classes,
  SysUtils, FileUtil, JLabeledDateTimeEdit, cySimpleGauge, TplLEDIndicatorUnit,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TForm1 }
  TForm1 = class(TForm,IRallyLogEventListener)
    btn_Connect: TButton;
    btnSetID: TButton;
    btnBat: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
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
    procedure Label2Click(Sender: TObject);

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

procedure TForm1.Label2Click(Sender: TObject);
begin

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

end.

