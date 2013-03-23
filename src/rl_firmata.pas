{
  Part of the RallyLog.Remote Project  http://code.google.com/p/rallylog

  ----------------------------------------------------------

  Firmarta Protocol definitions

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
unit rl_firmata;

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
        VAL_BAT_HI               = $01;
        VAL_BAT_LO               = $02;

        VAL_RTC_SEC              = $03;
        VAL_RTC_MIN              = $04;
        VAL_RTC_HOUR             = $05;
        VAL_RTC_DAY              = $06;
        VAL_RTC_MONTH            = $07;
        VAL_RTC_YEAR             = $08;
        VAL_RTC_CAL              = $09;
        VAL_RTC_STATUS           = $0A;

  end;

implementation

end.

