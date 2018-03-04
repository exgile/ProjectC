program Project1;

{$APPTYPE CONSOLE}

{$R *.res}

// COPYRIGHT TOPKUNG1

uses
  Windows,
  System.SysUtils,
  Core in 'Core.pas',
  mORMotReport in 'SynPDF\mORMotReport.pas',
  SynCommons in 'SynPDF\SynCommons.pas',
  SynCrypto in 'SynPDF\SynCrypto.pas',
  SynGdiPlus in 'SynPDF\SynGdiPlus.pas',
  SynLZ in 'SynPDF\SynLZ.pas',
  SynPdf in 'SynPDF\SynPdf.pas',
  SynZip in 'SynPDF\SynZip.pas';

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
