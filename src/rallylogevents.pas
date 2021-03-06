unit rallylogevents;

{$mode objfpc}{$H+}

interface

uses
   Contnrs, Classes, SysUtils;

type

    TDynByteArray = Array of Byte;

    TRallyLogEvent = class (Tobject)
    private
      fCommand : Byte;
      fValues : TDynByteArray;
    public
      constructor Create(const command: Byte; const values: TDynByteArray);
      property Command: Byte read fCommand write fCommand;
      property Values: TDynByteArray read fValues write fValues;
    end;


    IRallyLogEventListener = interface
       ['{03125C4A-3E84-4E98-928D-2514321E73D2}']     // interface GUID
        procedure handleRallyLogEvent(event: TRallyLogEvent);
    end;

    TListenerList = class(TObjectList)
       private
         procedure setListener(aIndex: integer; const value: IRallyLogEventListener);
         function getListener(aIndex: integer): IRallyLogEventListener;
       public
         property items[aindex:integer] : IRallyLogEventListener read getListener write setListener; default;
         function add(aListener: IRallyLogEventListener): Integer;
         function remove(aListener: IRallyLogEventListener): Integer;
     end;

    TRallyLogEventDispatcher = Class abstract(TObject)
      private
        fListeners: TListenerList;
      public
        Constructor Create();  virtual;
        Destructor Destroy();  override;
        procedure dispatchSysexEvent(const sysexEvent: TRallyLogEvent);
        procedure addSysexEventListener(listener: IRallyLogEventListener);
        procedure removeSysexEventListener(listener: IRallyLogEventListener);
    end;

implementation

     { TRallyLogEvent }
     Constructor TRallyLogEvent.Create(const command: Byte; const values: TDynByteArray);
     begin

        fCommand := command;
        fValues := values;
     end;

     { TListenerList }

     function TListenerList.add(aListener: IRallyLogEventListener): integer;
     begin
        Result := inherited add(aListener as TObject);
     end;

     function TlistenerList.remove(aListener: IRallyLogEventListener): integer;
     begin
        result := inherited remove(aListener as TObject);
     end;

     procedure TListenerList.setListener(aIndex: integer; const value: IRallyLogEventListener);
     begin
        inherited Items[aIndex] := value as TObject;
     end;

     function TListenerList.getListener(aIndex: integer): IRallyLogEventListener;
     begin
        result := inherited items[aIndex] as IRallyLogEventListener;
     end;

      { TRallyLogEventDispatcher }
      constructor TRallyLogEventDispatcher.Create();
      begin
         inherited;
        fListeners := TListenerList.create(false);  // we dont want to own the objects in the list
      end;

      destructor TRallyLogEventDispatcher.Destroy();
      begin
        //fListeners.clear;
        flisteners.free;
        inherited
      end;

      // tell all listeners to process the event message
      procedure TRallyLogEventDispatcher.dispatchSysexEvent(const sysexEvent: TRallyLogEvent);
      var
         i: integer;
      begin
         for i := 0 to fListeners.Count -1 do
             fListeners[i].handleRallyLogEvent(sysexEvent);
      end;

      procedure TRallyLogEventDispatcher.addSysexEventListener(listener: IRallyLogEventListener);
      begin
        fListeners.add(listener);
      end;

      procedure TRallyLogEventDispatcher.removeSysexEventListener(listener: IRallyLogEventListener);
      begin
         fListeners.Remove(listener);
      end;
end.

