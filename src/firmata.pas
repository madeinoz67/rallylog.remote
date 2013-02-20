unit firmata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TFirmata = Class(TObject)
    public
      const
        SYSEX_MAX_DATA_BYTES     = 32;    // max bytes in message
        // firmata command bytes
        CMD_ID_REQUEST           = $01;  // unit ID
        CMD_ID_SET               = $02;
        CMD_BAT_REQUEST          = $03;  // battery voltage
        CMD_RTC_REQUEST          = $04;  // RTC get current time/date from devic
        CMD_RTC_SET              = $05;  // RTC Set Command time/date in device

        CMD_SYSEX_STRING	 = $71;  // command identifier for string messages

        CMD_SYSEX_START          = $F0;  // start of a SYSEX message
        CMD_SYSEX_END            = $F7;  // end of a SYSEX message
        CMD_REPORT_VERSION       = $F9;  // report firmware version
        CMD_SYSTEM_RESET         = $FF;  // Reset

        // response Codes
        RSP_REPORT_READY        = $01;
        RSP_REPORT_ID           = $02;
        RSP_REPORT_BAT          = $03;
        RSP_REPORT_RTC          = $04;

        // Values Parameters
        VAL_ID                   = $00;
        VAL_BAT                  = $01;

        VAL_RTC_SEC              = $01;
        VAL_RTC_MIN              = $02;
        VAL_RTC_HOUR             = $03;
        VAL_RTC_DAY              = $04;
        VAL_RTC_MONTH            = $05;
        VAL_RTC_YEAR             = $06;
        VAL_RTC_CAL              = $07;
        VAL_RTC_STATUS           = $08;




  end;

implementation

end.

