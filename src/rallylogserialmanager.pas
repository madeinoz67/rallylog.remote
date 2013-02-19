


unit rallylogserialmanager;

{$mode objfpc}{$H+}

interface

uses
  firmata, SdpoSerial,rallylogevents, Classes, SysUtils;

type

  TCommunicationManager = class(TRallyLogEventDispatcher)
    private
      fComPort: TSdpoSerial;
      fConnected: boolean;
      fParsingSysex: boolean;
      fSysexBytesRead: integer;
      fStoredInputData: TDynByteArray;

      function sendSysexMessage(const command: byte ): integer;
      function sendSysexMessage(const command: byte; const values: TDynByteArray): integer;

      function strtoByte(const Value: String): TDynByteArray;
      function byteToString(const Value: TDynByteArray): String;

      procedure serialEvent(Sender: TObject);
      procedure processInput();
      procedure processSysexStringMessage(message: TDynByteArray);

    public
      constructor create(); override;
      destructor destroy(); override;

      procedure requestDeviceId();
      procedure requestBattery();

      procedure connect();
      procedure disconnect();

      property isConnected: boolean read fConnected;

  end;

implementation

  Constructor TCommunicationManager.Create();
  begin
    inherited;
    setLength(fStoredInputData,1);           // init the array
    fConnected := false;
    fParsingSysex := false;
    fComPort := TSdpoSerial.create(nil);
    with fComPort do
    begin
         BaudRate:=br_57600;   // default baud of FirmataLite Arduino library
         Device:='COM3';       //TODO: Testing need to select system comport device instead of hard coded
         OnRxData:= @serialEvent;
    end;
  end;   // Create

  Destructor TCommunicationManager.Destroy();
  begin
    fComport.Free;
    inherited;
  end;  // Destroy

  function TCommunicationManager.sendSysexMessage(const command:byte): integer;
  begin
      result := sendSysexMessage(command, nil);
  end;  //SendSysexMEssage

  function TCommunicationManager.sendSysexMessage(const command: byte; const values: TDynByteArray): integer;
  begin
       result := -1;
  end; //SendSysexMessage

  procedure TCommunicationManager.connect();
  begin
       fComPort.Open;
       fConnected := true;
  end; //connect

  procedure TCommunicationManager.disconnect();
  begin
       fComPort.Close;
       fConnected := false;
  end;  //disconnect

  //Callback whenever port receives data
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

    if (fParsingSysex AND (fSysexBytesRead < SYSEX_MAX_DATA_BYTES)) then
    begin
        if(bData = CMD_SYSEX_START) then                       // start of sysex command
        begin //if
          // a new start command
          setLength(fStoredInputData,0);                       // clear the array by deallocating
          setLength(fStoredInputData,SYSEX_MAX_DATA_BYTES);    // Reset read buffer
	  fSysexBytesRead := 0;
        end //if
        else if (bData = CMD_SYSEX_END) then                   // end of sysex command
             begin
                  bCommand := fStoredInputData[0];

                  // For Sysex string commands
		  if (bCommand = CMD_SYSEX_STRING) then
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

		     processSysexStringMessage(message);
                  end // if

                  // Sysex Commands
                  else
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

		     dispatchSysexEvent(TRallyLogEvent.Create(bCommand, values));  // send message to all listeners
                  end; //else
                  setLength(fStoredInputData,SYSEX_MAX_DATA_BYTES);
		  fParsingSysex := false;
		  fSysexBytesRead := 0;
             end //else if
        else
        begin
           fStoredInputData[fSysexBytesRead] := bData;
	   inc(fSysexBytesRead);
        end;  //else
    end //if
    else
    begin
         if(bData = CMD_SYSEX_START)then
         begin
	      fParsingSysex := true;
	      setLength(fStoredInputData, SYSEX_MAX_DATA_BYTES);
	      fSysexBytesRead := 0;
         end  // if
         else
         begin
            // waste this bytes if not in a sysex parsing
	      fSysexBytesRead := 0;
	 end;
    end; //else
  end;

  //Request ID
  procedure TCommunicationManager.requestDeviceId();
  begin
       sendSysexMessage(CMD_ID_REQUEST);
  end; //requestDeviceId

  //Request Battery
  procedure TCommunicationManager.requestBattery();
  begin
       sendSysexMessage(CMD_BAT_REQUEST);
  end; //requestBattery

// Convert Stribng to Byet
function TCommunicationManager.strToByte(const Value: String): TDynByteArray;
var
    I: integer;
begin
    SetLength(Result, Length(Value));
    for I := 0 to Length(Value) - 1 do
        Result[I] := ord(Value[I + 1]);// – 48;
end;

// convert Byte to Srring
function TCommunicationManager.byteToString(const Value: TDynByteArray): String;
var
    I: integer;
    S : String;
    letter: char;
begin
    S := '';
    for I := Length(Value)-1 Downto 0 do
    begin
        letter := Chr(Value[I]); // + 48);
        S := letter + S;
    end;
    Result := S;
end;

   // print a sysex String Message to Console and Log file
  procedure TCommunicationManager.processSysexStringMessage(message: TDynByteArray);
  begin
     //TODO: print message to console
     //TODO: prind message to log file
  end;

end.

