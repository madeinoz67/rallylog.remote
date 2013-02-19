unit rallylogevent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDynByteArray = Array of Byte;

  TRallyLogEvent = class (Tobject)
    private
      fCommand : Byte;
      fValues : TDynByteArray;
    public
      constructor Create(command: Byte; values: TDynByteArray);
      property Command: Byte read fCommand write fCommand;
      property Values: TDynByteArray read fValues write fValues;
  end;

implementation

  Constructor TRallyLogEvent.Create(command: Byte; values: TDynByteArray);
  begin
    fCommand := command;
    fValues := values;
  end;
end.

