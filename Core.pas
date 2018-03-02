unit Core;

interface

uses
  System.Threading, System.SysUtils, System.Classes, System.StrUtils;

resourcestring
  WordFileName = 'words.txt';
  FolderName = 'Data';

type
  TMainCore = class
    private
      fCommand: ITask;
      procedure StartCommand;
      procedure StartProcess;
      procedure StartFolderReportLevel1;
      function Tab(Count: UInt8): string;
      function GetDirSize(const Path: String): UInt32;
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

  // First Check whether the main folder exist or not
  if not DirectoryExists(FolderName) then
    if not CreateDir(FolderName) then
      WriteLn(Format('System Error: Directory ''%s'' has not been created', [FolderName]));
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

        case IndexStr(Command, ['start', 'getsize', 'report1']) of
          0:
            begin
              Self.StartProcess;
            end;
          1:
            begin
              writeln(GetDirSize('Data'));
            end;
          2:
            begin
              StartFolderReportLevel1;
            end;
          -1:
            WriteLn('Command not found.');
        end;
      end;
    end);

  fCommand.Start;
end;

procedure TMainCore.StartFolderReportLevel1;
var
  Searched, SearchedLv2, SearchedFile: TSearchRec;
  WordString: TStringList;
begin
  WordString := TStringList.Create;
  try
    if FindFirst(FolderName + '/*', faDirectory, Searched) = 0 then
    begin
      repeat
        if (Searched.Name = '..') or (Searched.Name = '.') then
          Continue;

        WordString.Add( Format('Directory Name: %s%s Size:%dkb' ,[Searched.Name, Tab(2), (Self.GetDirSize(FolderName + '/' + Searched.Name) div 1024 )]));

        if FindFirst(FolderName + '/' + Searched.Name + '/*', faDirectory,
          SearchedLv2) = 0 then
        begin
          repeat
            if (SearchedLv2.Name = '..') or (SearchedLv2.Name = '.') then
              Continue;

            if FindFirst(FolderName + '/' + Searched.Name + '/' +
              SearchedLv2.Name + '/*.*', faAnyFile, SearchedFile) = 0 then
            begin
              repeat
                if (SearchedFile.Name = '..') or (SearchedFile.Name = '.') then
                  Continue;

                WordString.Add( Format('%s - File Name: %s%s Size: %dbyte(s)' ,[Tab(1), SearchedFile.Name, Tab(7), SearchedFile.Size]));
              until FindNext(SearchedFile) <> 0;
            end;

          until FindNext(SearchedLv2) <> 0;
        end;

      until FindNext(Searched) <> 0;
    end;
    WordString.SaveToFile('report.txt');
    WriteLn('Report Succeed. Saved to report.txt');
  finally
    WordString.Free;
  end;
end;

procedure TMainCore.StartProcess;
var
  WordFile: TextFile;
  Worded: String;
  WordString: TStringList;
  Count: Int8;
begin
  WordString := TStringList.Create;
  try
    // Map file to variable
    AssignFile(WordFile, WordFileName);

    // Open the file handle for read/write
    Reset(WordFile);

    // loop for read by each line
    while not Eof(WordFile) do
    begin
      ReadLn(WordFile, Worded);
      // we must ensure that name has more than 2 lengths
      if Length(Worded) >= 2 then
      begin
        // Clear previous string first
        WordString.Clear;
        for Count := 0 to 99 do
        begin
          WordString.Add(Worded);
        end;

        // check if level 1 folder is existed if not create one
        if not DirectoryExists(Format('%s/%s', [FolderName, Worded[1]])) then
          CreateDir(Format('%s/%s', [FolderName, Worded[1]]));

        // check if level 2 folder is existed if not create one
        if not DirectoryExists(Format('%s/%s/%s', [FolderName, Worded[1], Worded[2]])) then
          CreateDir(Format('%s/%s/%s', [FolderName, Worded[1], Worded[2]]));

        // save to file
        WordString.SaveToFile(Format('%s/%s/%s/%s.txt', [FolderName, Worded[1], Worded[2], LowerCase(Worded)]));
      end;
    end;

    // close the file handle
    CloseFile(WordFile);
  finally
    WordString.Free;
  end;
end;

function TMainCore.GetDirSize(const Path: String): UInt32;
var
  TotalSize: LongInt;
  procedure Find(Str: String);
  var
    MySearch: TSearchRec;
    FindResult: Integer;
  begin
    FindResult := FindFirst(Str + '\*.*', faArchive + faHidden + faAnyFile +
      faVolumeID + faSysFile + faDirectory, MySearch);
    while FindResult = 0 do
    begin
      if (MySearch.Attr = faDirectory) and (MySearch.Name <> '.') and
        (MySearch.Name <> '..') then
        Find(Str + '\' + MySearch.Name)
      else
        TotalSize := TotalSize + MySearch.Size;
      FindResult := FindNext(MySearch);
    end;
    FindClose(MySearch);
  end;
begin
  TotalSize := 0;
  Find(Path);
  Result := TotalSize;
end;

function TMainCore.Tab(Count: UInt8): string;
var
  I: UInt8;
begin
  Result := '';

  for I := 0 to Count-1 do
    Result := Result + #9;
end;

end.
