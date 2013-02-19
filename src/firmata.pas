unit firmata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;



const      SYSEX_MAX_DATA_BYTES     = 32;   // max bytes in message

// firmata command bytes
const      CMD_ID_REQUEST           = $01;  // unit ID
const      CMD_ID_SET               = $02;
const      CMD_BAT_REQUEST          = $03;  // battery voltage
const      CMD_RTC_REQUEST          = $04;  // RTC get current time/date from devic
const      CMD_RTC_SET              = $05;  // RTC Set Command time/date in device

const      CMD_SYSEX_STRING	    = $71;  // command identifier for string messages

const      CMD_SYSEX_START          = $F4;  // start of a SYSEX message
const      CMD_SYSEX_END            = $F7;  // end of a SYSEX message
const      CMD_REPORT_VERSION       = $F9;  // report firmware version
const      CMD_SYSTEM_RESET         = $FF;  // Reset


// response Codes
const      RSP_REPORT_READY        = $01;
const      RSP_REPORT_ID           = $02;
const      RSP_REPORT_BAT          = $03;
const      RSP_REPORT_RTC          = $04;

// Values Parameters
const     VAL_ID                   = $01;
const     VAL_BAT                  = $01;


implementation

end.

