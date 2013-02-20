unit remoteutils;

{$mode objfpc}{$H+}

interface

uses
  rallylogevents,Classes, SysUtils;

implementation
     // Convert Stribng to Byet
function strToByte(const Value: String): TDynByteArray;
var
    I: integer;
begin
    SetLength(Result, Length(Value));
    for I := 0 to Length(Value) - 1 do
        Result[I] := ord(Value[I + 1]);// – 48;
end;

// convert Byte to Srring
function byteToStr(const Value: TDynByteArray): String;
var
    I: integer;
    S : String;
    letter: char;
begin
    S := '';
    for I := Length(Value)-1 Downto 0 do
    begin
        letter := Chr(Value[I] + 48);
        S := letter + S;
    end;
    Result := S;
end;
end.

