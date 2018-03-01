program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

// COPYRIGHT TOPKUNG1

uses
  Windows,
  System.SysUtils,
  // JSON reader/writer because the internal one is f*ck up
  XSuperJSON in 'Tools\XSuperJSON.pas',
  XSuperObject in 'Tools\XSuperObject.pas',
  Core in 'Core.pas';

var
  Msg: TMsg;
  bRet: LongBool;
  MainObject: TMainCore;

begin
  try
    // Initialize the main core to control application
    MainObject := TMainCore.Create;
    // The below function is using for handle console application not to close
    repeat
      bRet := GetMessage(Msg, 0, 0, 0);
      if Integer(bRet) = -1 then
      begin
        Break;
      end
      else
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    until not bRet;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
