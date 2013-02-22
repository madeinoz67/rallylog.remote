unit ConnectedDevice;

{$mode objfpc}{$H+}

interface

uses
  ftd2xx, Classes, SysUtils, SdpoSerial,ComCtrls,ExtCtrls, contnrs,
  SharedLogger, filechannel;



type
    TDeviceState = (STATE_SEARCHING, STATE_CONNECTED);
    TReceiveState = (STATE_START, STATE_CMD, STATE_ADDR, STATE_DATA, STATE_PROCESSING);

    const MAX_NUM_SERIAL_NUMBER_CHARS = 50;

type TSerialNumber = array [0..(MAX_NUM_SERIAL_NUMBER_CHARS - 1)] of Char;
type PSerialNumber = ^TSerialNumber;

    TConnectedDevice = Class(TComponent)
      private
         fDevice : TFtd2xxDevice;
         fOnDeviceConnect: TNotifyEvent;
         fOnDeviceDisconnect: TNotifyEvent;
         connectTimer : TTimer;
         fDeviceState : TDeviceState;
          procedure  onDeviceTimer(Sender: TObject);
      protected

         function getComPortNumber(): Integer;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        //FTDI DLL Functions
        function isConnected: Boolean;
        function getSerialNumber : String;
        function GetDeviceCount: DWord;

      published
        property onDeviceConnected: TNotifyEvent read fOnDeviceConnect write fOnDeviceConnect;
        property onDeviceDisconnected: TNotifyEvent read fOnDeviceDisconnect write fOnDeviceDisconnect;
        property comPortNumber: Integer read getComPortNumber;
    end;

  implementation

    Constructor TConnectedDevice.Create(AOwner: TComponent);
    begin
      inherited Create(aOwner);

      fDevice := TFtd2xxDevice.create;
      //Logger.Send('[TConnectedDevice] created');
      fDeviceState := STATE_SEARCHING;          // initial device state

      connectTimer := TTimer.Create(self);      // device connection timer
      connectTimer.OnTimer:= @onDeviceTimer;
      connectTimer.Interval:=1000;              // 1 second
      connectTimer.Enabled:=true;               // start the timer

    end;

    Destructor TConnectedDevice.Destroy;
    begin
      connectTimer.Free;
      fDevice.Free;
      inherited;
    end;

    function TConnectedDevice.GetDeviceCount: DWord;
    begin
         result := fDevice.getNumDevices();
    end;

    function TConnectedDevice.getComPortNumber(): Integer;
    begin
         if (fDevice.isConnected) then
            result := fDevice.GetComPortNumber()
         else
             result := 0;
    end;

    function TConnectedDevice.GetSerialNumber: String;
    begin
         Result := fDevice.getSerialNumber();
    end;

    function TConnectedDevice.isConnected: Boolean;
    begin
         result := fDevice.isConnected();
    end;

    procedure TConnectedDevice.onDeviceTimer(Sender: TObject);
    begin
      //Logger.send('[TConnectedDevice.onDeviceTimer]');
      case fDeviceState of
           STATE_SEARCHING:
             begin
              if (isConnected) then
                 begin
                      fDeviceState := STATE_CONNECTED;
                      if assigned(fOnDeviceConnect) then
                         fOnDeviceConnect(self);     // Send Notification Event
                 end
             end;

           STATE_CONNECTED:
             begin
             if (not(isConnected)) then
                begin
                     fDeviceState := STATE_SEARCHING;
                     if assigned(fOnDeviceDisconnect) then
                        fOnDeviceDisconnect(self);   // Send Notification Event
                end
             end;
      end;

    end;

end.

