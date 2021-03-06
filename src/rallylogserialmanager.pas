unit rallylogserialmanager;

{$mode objfpc}{$H+}

interface

uses
  ConnectedDevice, firmata, SdpoSerial,rallylogevents, Classes, SysUtils;

type

  TCommunicationManager = class(TRallyLogEventDispatcher)
    private
      fDevice: TConnectedDevice;
      fComPort: TSdpoSerial;
      fConnected: boolean;
      fParsingSysex: boolean;
      fSysexBytesRead: integer;
      fStoredInputData: TDynByteArray;

      function sendSysexMessage(const command: byte ): integer;
      function sendSysexMessage(const command: byte; const values: TDynByteArray): integer;

     // function strtoByte(const Value: String): TDynByteArray;
     // function byteToString(const Value: TDynByteArray): String;

      procedure serialEvent(Sender: TObject);
      procedure processInput();
      procedure processSysexStringMessage(message: TDynByteArray);

    public
      constructor create(); override;
      destructor destroy(); override;

      procedure requestDeviceId();
      procedure requestBattery();

      procedure setId(id: byte);
      procedure syncTimeDate();

      function connect(): boolean;
      function disconnect(): boolean;

      property isConnected: boolean read fConnected;

      property ConnectedDevice: TConnectedDevice read fDevice;

  end;

implementation

  Constructor TCommunicationManager.Create();
  begin
    inherited;
    fDevice := TConnectedDevice.create(nil);

    setLength(fStoredInputData,1);           // init the array
    fConnected := false;
    fParsingSysex := false;
    fComPort := TSdpoSerial.create(nil);
    with fComPort do
    begin
         BaudRate:=br_57600;   // default baud of FirmataLite Arduino library
         //Device:='COM1';       //TODO: Testing need to select system comport device instead of hard coded
         OnRxData:= @serialEvent;
    end;
  end;   // Create

  Destructor TCommunicationManager.Destroy();
  begin
    fDevice.Free;
    fComport.Free;
    inherited;
  end;  // Destroy

  function TCommunicationManager.sendSysexMessage(const command:byte): integer;
  var
     value: TDynByteArray;
  begin
       setLength(value,1);
       try
          result := sendSysexMessage(command, value);

       finally
          setLength(value,0);
       end;
  end;  //SendSysexMEssage

  function TCommunicationManager.sendSysexMessage(const command: byte; const values: TDynByteArray): integer;
  var
      totalSize: integer;
      message: TDynByteArray;
      i: integer;
  begin
       if(fConnected)then
       begin
                        totalSize := length(values)+3;
			setLength(message, totalSize);

			message[0] := TFirmata.CMD_SYSEX_START;
			message[1] := command;

                        for i:= 2 to length(values)+2 do
                        begin
                             message[i] := values[i-2];
                        end; //for

			message[totalSize-1] := TFirmata.CMD_SYSEX_END;

			try
                           for i:=0 to length(message)-1 do
                               fComPort.SynSer.SendByte(message[i]);
                        finally
                          setLength(message,0);
                        end;  // Try
	end // If
        else
        begin
             //TODO: logger.warn("Trying to send, but not connected");
        end; // else

  end; //SendSysexMessage

  function TCommunicationManager.connect(): Boolean;
  begin
       if(fDevice.isConnected) then
       begin
          while ( fDevice.comPortNumber = 0) do
             Sleep(50);   // Wait until Comport# get assigned

          fComPort.Device:='COM' + intToStr(fDevice.comPortNumber);
          fComPort.Open;
          fConnected := true;
       end
       else
          result := fConnected;
  end; //connect

  function TCommunicationManager.disconnect(): boolean;
  begin
       fComPort.Close;
       fConnected := false;
       result := fConnected;
  end;  //disconnect

  //Callback whenever port receives some data
  procedure TCommunicationManager.serialEvent(Sender: TObject);
  begin
    While (fComPort.DataAvailable) do
    begin
        processInput();
    end;
  end; //serialEvent

  //process the data received
  procedure TCommunicationManager.processInput();
  var
    bData : Byte;
    bCommand: Byte;
    message: TDynByteArray;
    values: TDynByteArray;
    iMessageIndex : integer;
    iStoredInputDataIndex: integer;
    iNumOfValues: integer;
    iValuesIndex: integer;
    LSB, MSB: integer;
  begin
    bData := fComPort.SynSer.RecvByte(0);                      // Read a single byte from the serial port

    if (fParsingSysex AND (fSysexBytesRead < TFirmata.SYSEX_MAX_DATA_BYTES)) then
    begin
        if(bData = TFirmata.CMD_SYSEX_START) then                       // start of sysex command
        begin //if
          // a new start command
          setLength(fStoredInputData,0);                       // clear the array by deallocating
          setLength(fStoredInputData,TFirmata.SYSEX_MAX_DATA_BYTES);    // Reset read buffer
	  fSysexBytesRead := 0;
        end //if
        else if (bData = TFirmata.CMD_SYSEX_END) then                   // end of sysex command
             begin
                  bCommand := fStoredInputData[0];

                  // For Sysex String commands
		  if (bCommand = TFirmata.CMD_SYSEX_STRING) then
                  begin
                     setLength(message, fSysexBytesRead-1);
                     iMessageIndex := 0;
		     iStoredInputDataIndex := 1;

		       while(iMessageIndex < fSysexBytesRead-1) do
                       begin
		          message[iMessageIndex] := fStoredInputData[iStoredInputDataIndex];
			  inc(iMessageIndex);
			  inc(iStoredInputDataIndex);
		       end; //while

		     processSysexStringMessage(message);    // do something with the String Message
                     setLength(message,0);                  //deallocate
                  end // if

                  else // Sysex all other types of binary Commands
                  begin
                     iNumOfValues := (fSysexBytesRead-1) div 2;
		     setLength(values, iNumOfValues);

		     iValuesIndex := 0;
		     iStoredInputDataIndex := 1;

		     while(iValuesIndex < iNumOfValues) do
                     begin
			LSB := fStoredInputData[iStoredInputDataIndex];
			MSB := fStoredInputData[iStoredInputDataIndex+1];

			values[iValuesIndex] := MSB*128+LSB;
			inc(iValuesIndex);
			iStoredInputDataIndex := iStoredInputDataIndex+2;
		     end; //while

		     dispatchSysexEvent(TRallyLogEvent.Create(bCommand, values));  // send sysex message to all listeners
                     setLength(values,0);                               // deallocate values

                  end; //else
                  setLength(fStoredInputData,TFirmata.SYSEX_MAX_DATA_BYTES);
		  fParsingSysex := false;
		  fSysexBytesRead := 0;
             end //else if
        else // read all bytes after Sysex start into buffer
        begin
           fStoredInputData[fSysexBytesRead] := bData;
	   inc(fSysexBytesRead);
        end;  //else
    end //if
    else
    begin
         if(bData = TFirmata.CMD_SYSEX_START)then
         begin
	      fParsingSysex := true;
	      setLength(fStoredInputData, TFirmata.SYSEX_MAX_DATA_BYTES);
	      fSysexBytesRead := 0;
         end  // if
         else
         begin
            // ignore this byte if not in sysex parsing
	      fSysexBytesRead := 0;
	 end;
    end; //else
  end;

  //Request ID
  procedure TCommunicationManager.requestDeviceId();
  begin
       sendSysexMessage(TFirmata.CMD_ID_REQUEST);
  end; //requestDeviceId

  //Request Battery
  procedure TCommunicationManager.requestBattery();
  begin
       sendSysexMessage(TFirmata.CMD_BAT_REQUEST);
  end; //requestBattery

   // print a sysex String Message to Console and Log file
  procedure TCommunicationManager.processSysexStringMessage(message: TDynByteArray);
  begin
    //TODO: print message to console
    //TODO: prind message to log file
  end;

  // sets ID
 procedure TCommunicationManager.setID(id: Byte);
 var
   value: TDynByteArray;
 begin
      setLength(value, 1);
      value[0] := id;
      sendSysexMessage(TFirmata.CMD_ID_SET, value);
      setLength(value,0);
 end;

 procedure TCommunicationManager.syncTimeDate();
 var
    value: TDynByteArray;
    DD,MTH,YY: word;
    HH,MIN,SS,MS: word;
 begin
      setLength(value, 11);
      DecodeDate(Date,YY,MTH,DD);
      DecodeTime(Time,HH,MIN,SS, MS);
      value[TFirmata.VAL_RTC_YEAR] := YY - 2000;
      value[TFirmata.VAL_RTC_MONTH] := MTH;
      value[TFirmata.VAL_RTC_DAY] := DD;
      value[TFirmata.VAL_RTC_HOUR] := HH;
      value[TFirmata.VAL_RTC_MIN] := MIN;
      value[TFirmata.VAL_RTC_SEC] := SS +1;
      sendSysexMessage(TFirmata.CMD_RTC_SET, value);
      setLength(value,0);
 end;

end.

