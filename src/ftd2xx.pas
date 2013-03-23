{
  Part of the RallyLog.Remote Project  http://code.google.com/p/rallylog

  ----------------------------------------------------------

  FTDI class implemtation of ftd2xx.dll for USB Serial Device

  Not all functions of the DLL have been implemented

  ----------------------------------------------------------

  Copyright (C) 2013 Stephen Eaton seaton@strobotics.com.au

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ftd2xx;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, dynlibs;

type FT_STATUS = Integer;

const MAX_NUM_SERIAL_NUMBER_CHARS = 50;

type TSerialNumber = array [0..(MAX_NUM_SERIAL_NUMBER_CHARS - 1)] of Char;
type PSerialNumber = ^TSerialNumber;

const MAX_NUM_DESCRIPTION_CHARS = 256;

type TDescription = array [0..(MAX_NUM_DESCRIPTION_CHARS - 1)] of Char;
type PDescription = ^TDescription;

const MAX_NUM_DLL_VERSION_CHARS = 10;

type TDllVersion = array [0..(MAX_NUM_DLL_VERSION_CHARS - 1)] of Char;
type PDllVersion = ^TDllVersion;

const MAX_NUM_ERROR_MESSAGE_CHARS = 100;

type TErrorMessage = array [0..(MAX_NUM_ERROR_MESSAGE_CHARS - 1)] of Char;
type PErrorMessage = ^TErrorMessage;

type TFtd2xxDevice = class(TObject)
 private
  hLib: TlibHandle;
  hDevice: DWord;
  fComPortNumber: DWord;
 public
  constructor create;
  destructor destroy; override;

  function isConnected(): Boolean;

  function  open(): FT_STATUS;
  function  close(): FT_STATUS;
  function  getSerialNumber(): String;
  function  GetComPortNumber(): Integer;
  function  GetNumDevices(): Integer;
  function  GetDeviceSerialNumber(SerialNumberIndex: Longword; pSerialNumberBuffer: PSerialNumber): FT_STATUS;
  function  GetDeviceDescription(DescriptionIndex: Longword; pDescriptionBuffer: PDescription): FT_STATUS;
  function  GetDeviceLocationID(LocationIDIndex: Longword; pLocationIDBuffer: PLongword): FT_STATUS;
  function  GetDeviceChipID(ChipIDIndex: Longword; pChipIDBuffer: PLongword): FT_STATUS;
  function  GetDllVersion(DllVersionBuffer: pDllVersion): FT_STATUS;
  function  GetErrorCodeString(Language: String; StatusCode: FT_STATUS; pErrorMessageBuffer: PErrorMessage): FT_STATUS;

  Const
 // FT_Status Values
  FT_SUCCESS = 0;
  FT_INVALID_HANDLE = 1;               // FT_INVALID_HANDLE
  FT_DEVICE_NOT_FOUND	= 2;           // FT_DEVICE_NOT_FOUND
  FT_DEVICE_NOT_OPENED = 3;            // FT_DEVICE_NOT_OPENED
  FT_IO_ERROR =	4;                     // FT_IO_ERROR
  FT_INSUFFICIENT_RESOURCES =	5;     // FT_INSUFFICIENT_RESOURCES
  FT_INVALID_PARAMETER = 6;            // FT_INVALID_PARAMETER

  FT_LIST_NUMBER_ONLY = $80000000;

  FT_BUFFER_SIZE_TOO_SMALL = 20;
  FT_PASSED_NULL_POINTER = 21;
  FT_INVALID_LANGUAGE_CODE = 22;
  FT_INVALID_STATUS_CODE = $FFFFFFFF;

  FT_DLL_NAME = 'ftd2xx.dll';
end;


implementation

var
  // Used By FT_ListDevices
   FT_Device_Count : DWord;
   FT_Device_String_Buffer : array [1..50] of Char;
   FT_Device_String : String;
   FT_Device_Location : DWord;
   //USB_Device_Info_Node : FT_Device_Info_Node;
   FT_Event_Handle : DWord;

function FT_Open(DeviceIndex: Integer; ftHandle: Pointer): FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FT_Open';
function FT_Close(ftHandle: DWord): FT_STATUS;  stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FT_Close';

function FT_GetComPortNumber (ftHandle: DWord; pComPortNumber: PLongWord): FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FT_GetComPortNumber';

function FT_GetNumDevices(pvArg1:Pointer; pvArg2:Pointer; dwFlags:Dword) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FT_ListDevices';
function FTID_GetDeviceSerialNumber(DeviceIndex: Longword ; SerialBuffer: Pointer ; SerialBufferLength: Longword) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FTID_GetDeviceSerialNumber';
function FTID_GetDeviceDescription(DeviceIndex: Longword ; DescriptionBuffer: Pointer ; DescriptionBufferLength: Longword) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FTID_GetDeviceDescription';
function FTID_GetDeviceLocationID(DeviceIndex: Longword ; LocationIDBuffer: Pointer) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FTID_GetDeviceLocationID';
function FTID_GetDeviceChipID(DeviceIndex: Longword ; ChipIDBuffer: Pointer) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FTID_GetDeviceChipID';
function FTID_GetDllVersion(DllVersionBuffer: pDllVersion; BufferSize: Longword) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FTID_GetDllVersion';
function FTID_GetErrorCodeString(Language: PChar; StatusCode: FT_STATUS; ErrorMessageBuffer: PErrorMessage; BufferSize: Longword) : FT_STATUS; stdcall; External TFtd2xxDevice.FT_DLL_NAME name 'FTID_GetErrorCodeString';

constructor TFtd2xxDevice.create;
begin
  inherited;
    hDevice := 0;
   //if fileexists(FT_DLL_NAME) then  { just a sanity check, could extend this }
    hLib:= LoadLibrary(FT_DLL_NAME);
end;

destructor TFtd2xxDevice.destroy;
begin
  if hDevice <> 0 then
    close();
  if hLib <> 0 then UnloadLibrary(hLib); { release DLL library }
  hLib:= 0; { again sanity }

  inherited;
end;

function  TFtd2xxDevice.open(): FT_STATUS;
begin
  if hlib <> 0 then
    Result := FT_Open(0, @hDevice);
end;

function  TFtd2xxDevice.close(): FT_STATUS;
begin
  if (hlib <> 0) and (hDevice <> 0) then
    Result := FT_Close(hDevice);
end;

function TFtd2xxDevice.getSerialNumber(): String;
begin
  {
        if(isConnected) THEN
         begin
          if (GetFTDeviceSerialNo(0) = FT_OK) THEN
             begin
              if Length(SerialNumber)>0 then
                 SetString(Result, PChar(@SerialNumber[0]), Length(SerialNumber))
              else
                 Result := '';
             end;
         end
      else
          result := '';
  }
     Result := '';  //TODO Implement getSerialNumber
end;

function  TFtd2xxDevice.GetComPortNumber(): Integer;
begin

  if (hlib <> 0) then
    if (open() = FT_SUCCESS) then
      begin
       if( FT_GetComPortNumber (hDevice, @fComPortNumber) = FT_SUCCESS) then
            Result := fComPortNumber;
       close();
       end
    else
        Result := 0;
end;

function  TFtd2xxDevice.GetNumDevices(): Integer;
begin
  if hLib <> 0 then
     if(FT_GetNumDevices(@FT_Device_Count,Nil,FT_LIST_NUMBER_ONLY) = FT_SUCCESS) then
       result := FT_Device_Count
     else
       result := 0
  else
      result :=0;
end;


function TFtd2xxDevice.isConnected(): Boolean;
begin
     if(GetNumDevices<> 0 ) then
         result := true
      else
         result := false;
end;

function  TFtd2xxDevice.GetDeviceSerialNumber(SerialNumberIndex: Longword ; pSerialNumberBuffer: PSerialNumber): FT_STATUS;
begin
  if hLib <> 0 then
     Result := FTID_GetDeviceSerialNumber(SerialNumberIndex, pSerialNumberBuffer, MAX_NUM_SERIAL_NUMBER_CHARS);

end;

function  TFtd2xxDevice.GetDeviceDescription(DescriptionIndex: Longword ; pDescriptionBuffer: PDescription): FT_STATUS;
begin
  if hLib <> 0 then
     Result := FTID_GetDeviceDescription(DescriptionIndex, pDescriptionBuffer, MAX_NUM_DESCRIPTION_CHARS);
end;

function  TFtd2xxDevice.GetDeviceLocationID(LocationIDIndex: Longword ; pLocationIDBuffer: PLongword): FT_STATUS;
begin
  if hLib <> 0 then
     Result := FTID_GetDeviceLocationID(LocationIDIndex, pLocationIDBuffer);
end;

function  TFtd2xxDevice.GetDeviceChipID(ChipIDIndex: Longword ; pChipIDBuffer: PLongword): FT_STATUS;
begin
  if hLib <> 0 then
     Result := FTID_GetDeviceChipID(ChipIDIndex, pChipIDBuffer);
end;

function  TFtd2xxDevice.GetDllVersion(DllVersionBuffer: pDllVersion): FT_STATUS;
begin
  if hLib <> 0 then
     Result := FTID_GetDllVersion(DllVersionBuffer, MAX_NUM_DLL_VERSION_CHARS);
end;

function  TFtd2xxDevice.GetErrorCodeString(Language: String ; StatusCode: FT_STATUS;
                                      pErrorMessageBuffer: PErrorMessage): FT_STATUS;
begin
  if hLib <> 0 then
     Result := FTID_GetErrorCodeString(PChar(Language), StatusCode, pErrorMessageBuffer, MAX_NUM_ERROR_MESSAGE_CHARS);
end;

end.
