unit Core;

interface

uses
  System.Threading, System.SysUtils, System.StrUtils, XSuperObject;

type
  TMainCore = class
    private
      fCommand: ITask;
      procedure StartCommand;
    public
      constructor Create;
      destructor Destroy;
  end;

implementation

{ TMainCore }

constructor TMainCore.Create;
begin
  // Start the command-line
  Self.StartCommand;
end;

destructor TMainCore.Destroy;
begin

end;

procedure TMainCore.StartCommand;
var
  Command, CommandParameters: string;
  InputArray: TArray<string>;
  C: string;
  D: Double;
begin
  fCommand := TTask.Create(
    procedure()
    begin
      while True do
      begin
        Readln(C);
        InputArray := C.Split([' ']);

        if Length(InputArray) >= 1 then
          Command := InputArray[0]
        else
          Command := string.Empty;

        if Length(InputArray) >= 2 then
          CommandParameters := InputArray[1]
        else
          CommandParameters := string.Empty;

        case IndexStr(Command, ['start']) of
          0:
            begin
              WriteLn('Begin to started');
            end;
          -1:
            WriteLn('Command not found.');
        end;
      end;
    end);

  fCommand.Start;
end;

end.
