unit Core;

interface

uses
  System.Threading, System.SysUtils, System.Classes, System.StrUtils, System.ZIP, Math,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.DAPT;

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
      procedure StartDiffReport;
      procedure StartDictionary2SQL;
      function Tab(Count: UInt8): string;
      function GetDirSize(const Path: String): UInt32;
      function DiffPercent(Val1, Val2: UInt32): UInt32;
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

function TMainCore.DiffPercent(Val1, Val2: UInt32): UInt32;
begin
  Exit( Round((1 - (Val1 / Val2)) * 100) );
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

        case IndexStr(Command, ['start', 'report', 'diff', 'word2sql']) of
          0:
            begin
              Self.StartProcess;
            end;
          1:
            begin
              StartFolderReportLevel1;
            end;
          2:
            begin
              StartDiffReport;
            end;
          3:
            begin
              StartDictionary2SQL;
            end;
          -1:
            WriteLn('Command not found.');
        end;
      end;
    end);

  fCommand.Start;
end;

procedure TMainCore.StartDictionary2SQL;
var
  Connection: TFDConnection;
  Query: TFDQuery;
  DicFile: TextFile;
  Word: String;
begin
  Connection := TFDConnection.Create(nil);
  Query := TFDQuery.Create(nil);
  AssignFile(DicFile, WordFileName);
  try
    Reset(DicFile);

    Query.Connection := Connection;

    Connection.DriverName := 'SQLite';
    Connection.Params.Database := 'SQLData.s3db';
    Connection.Open;

    // create the table if not exists
    // in this case we must ensure that table is full-text-search for searching purpose
    Query.SQL.Clear;
    Query.SQL.Add('CREATE VIRTUAL TABLE IF NOT EXISTS Dictionary USING FTS5(Keyword)');
    Query.ExecSQL;

    while not Eof(DicFile) do
    begin
      ReadLn(DicFile, Word);

      Query.SQL.Clear;
      Query.SQL.Add( Format('INSERT INTO Dictionary(Keyword) VALUES(''%s'')', [Word]) );
      Query.ExecSQL;
    end;
  finally
    Query.Free;
    Connection.Free;
    CloseFile(DicFile);
  end;
end;

procedure TMainCore.StartDiffReport;
var
  Dir, Files: TSearchRec;
  Zip: TZipFile;
  TextCreate: TStringList;
  DirSize, FileSizes: Integer;
begin
  Zip := TZipFile.Create;
  TextCreate := TStringList.Create;
  try
    // Zip File First
    if FindFirst(FolderName + '/*', faDirectory, Dir) = 0 then
    begin
      repeat
        if (Dir.Name = '..') or (Dir.Name = '.') then
          Continue;

        if Dir.Attr = faDirectory then
          Zip.ZipDirectoryContents(FolderName + '/' + Dir.Name + '.zip', FolderName + '/' + Dir.Name);
      until FindNext(Dir) <> 0;
    end;

    // Find Diff zip and dir
    if FindFirst(FolderName + '/*', faDirectory, Dir) = 0 then
    begin
      repeat
        if (Dir.Name = '..') or (Dir.Name = '.') then
          Continue;

        if not (Dir.Attr = faDirectory) then
          Continue;

        if FindFirst(FolderName + '/' + Dir.Name + '.zip', faAnyFile + faArchive, Files) = 0 then
        begin
          DirSize := Round(Self.GetDirSize(FolderName + '/' + Dir.Name) / 1024);
          FileSizes := Round(Files.Size / 1024);
          TextCreate.Add( Format('Directory Name: %s Size: %dkb%sZip Size: %dkb%sDifferent: %d%%', [Dir.Name, DirSize, Tab(1), FileSizes, Tab(2), Self.DiffPercent(FileSizes, DirSize)]) );
        end;
      until FindNext(Dir) <> 0 ;
    end;

    TextCreate.SaveToFile('dirreport.txt');
    WriteLn(' Dir Diff Reported was saved to dirreport.txt');
  finally
    Zip.Free;
    TextCreate.Free;
    FindClose(Dir);
    FindClose(Files);
  end;
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
    FindClose(Searched);
    FindClose(SearchedLv2);
    FindClose(SearchedFile);
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
