


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
      fStoredInputData: Array of String;

      function sendSysexMessage(const command: byte ): integer;
      function sendSysexMessage(const command: byte; const values: TDynByteArray): integer;

      function strtoByte(const Value: String): TDynByteArray;
      function byteToString(const Value: TDynByteArray): String;

      procedure serialEvent(Sender: TObject);
      procedure processInput();

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
    fConnected := false;
    fParsingSysex := false;
    fComPort := TSdpoSerial.create(nil);
    with fComPort do
    begin
         BaudRate:=br_57600;   // default baud of FirmataLite Arduino library
         Device:='COM3';       //TODO: Testing need to select system comport device
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
        fcomport.SynSer.;
    end;
  end; //serialEvent

  //process the data received
  procedure TCommunicationManager.processInput();
  var
    sData : String[1];  // SERIAL DATA
    bData : Byte;
  begin

    //PSEUDO CODE
    // SData <- FComport.read
    // convert sData to bData
    //
    //bData := 0;

    sData := fComPort.ReadData;

    if length(sData) = 0 then
       exit;
    bData := ord(sData[0]);

    if (fParsingSysex AND (fSysexBytesRead < SYSEX_MAX_DATA_BYTES)) then
    begin
        if(bData = CMD_SYSEX_START) then
        begin //if
          // a new command start
	  // clear buffer
	 // fStoredInputData = new int[SYSEX_MAX_DATA_BYTES];
	  fSysexBytesRead := 0;
        end; //if
    end //if
    else
    begin

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

end.

