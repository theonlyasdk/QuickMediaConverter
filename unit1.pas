unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  lclintf, Windows, FormAbout, FormTools, LCLType,
  Clipbrd, Buttons, ComCtrls, Process;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnSameAsSource: TButton;
    BtnChooseSource: TButton;
    BtnChooseOut: TButton;
    BtnOpenFfmpegDl: TButton;
    BtnChooseFfmpeg: TButton;
    BtnConvert: TButton;
    BtnOptions: TButton;
    BtnAbout: TButton;
    BtnTools: TButton;
    CbTargetFmt: TComboBox;
    LblTitle: TLabel;
    Label2: TLabel;
    ExecOutput: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SelectDirectoryDialog2: TSelectDirectoryDialog;
    SelectDirectoryDialog3: TSelectDirectoryDialog;
    StatusBar: TStatusBar;
    TbSource: TLabeledEdit;
    TbOutput: TLabeledEdit;
    TbFfmpeg: TLabeledEdit;
    procedure BtnAboutClick(Sender: TObject);
    procedure BtnChooseFfmpegClick(Sender: TObject);
    procedure BtnOpenFfmpegDlMouseEnter(Sender: TObject);
    procedure BtnSameAsSourceClick(Sender: TObject);
    procedure BtnChooseOutClick(Sender: TObject);
    procedure BtnChooseSourceClick(Sender: TObject);
    procedure BtnConvertClick(Sender: TObject);
    procedure BtnOpenFfmpegDlClick(Sender: TObject);
    procedure BtnSameAsSourceMouseEnter(Sender: TObject);
    procedure BtnToolsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    function HasSupportedMediaFiles(const ADirectory: string): Boolean;
  private
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

function TFormMain.HasSupportedMediaFiles(const ADirectory: string): Boolean;
var
  SearchRec: TSearchRec;
  FileFound: Integer;
  Ext: string;
  FileExt: string;
const
  SupportedExtensions: array[0..5] of string = ('.mp3', '.mp4', '.wav', '.avi', '.mkv', '.flv');
begin
  Result := False;

  FileFound := FindFirst(ADirectory + '*.*', faAnyFile, SearchRec);
  try
    while FileFound = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = 0 then // Not a directory
      begin
        FileExt := LowerCase(ExtractFileExt(SearchRec.Name));
        for Ext in SupportedExtensions do
        begin
          if FileExt = Ext then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
      FileFound := FindNext(SearchRec);
    end;
  finally
     SysUtils.FindClose(SearchRec);
  end;
end;



procedure TFormMain.FormCreate(Sender: TObject);
var
  DownloadsPath: array[0..MAX_PATH] of Char; // Buffer for the path
  SearchRec: TSearchRec;
  FileFound: Integer;
  FfmpegDir: string;
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  GetEnvironmentVariable('USERPROFILE', DownloadsPath, SizeOf(DownloadsPath));
  StrCat(DownloadsPath, '\Downloads\');

  FileFound := FindFirst(DownloadsPath + '*', faDirectory, SearchRec);
  try
    while FileFound = 0 do
    begin
      if (SearchRec.Attr and faDirectory <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        FfmpegDir := DownloadsPath + SearchRec.Name + '\bin\ffmpeg.exe';
        if FileExists(FfmpegDir) then
        begin
          TbFfmpeg.Text := ExtractFilePath(FfmpegDir);
          Break;
        end;
      end;
      FileFound := FindNext(SearchRec);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TFormMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  FileName: String;
  Directory: String;
begin
  if Length(FileNames) > 0 then
  begin
    FileName := FileNames[0];
    if DirectoryExists(FileName) then
      TbSource.Text := IncludeTrailingPathDelimiter(FileName)
    else
    begin
      Directory := ExtractFilePath(FileName);
      TbSource.Text := IncludeTrailingPathDelimiter(Directory);
    end;
  end;
end;


procedure TFormMain.BtnOpenFfmpegDlClick(Sender: TObject);
begin
  OpenURL('https://github.com/BtbN/FFmpeg-Builds/releases');
  ShowMessage('1. Download ffmpeg-master-latest-win64-lgpl-shared.zip and extract it to your Downloads folder.' + sLineBreak + '2. Choose the directory using the choose button. (Or restart the application and it will automatically detect the folder from Downloads)');
end;

procedure TFormMain.BtnSameAsSourceMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := 'Click to set the output folder to the same location as the source folder.';
end;

procedure TFormMain.BtnToolsClick(Sender: TObject);
begin
  ToolsForm.Show;
end;

procedure TFormMain.BtnChooseSourceClick(Sender: TObject);
var
  SelectedDir: string;
begin
  if DirectoryExists(Trim(TbSource.Text)) then
    SelectDirectoryDialog1.InitialDir := Trim(TbSource.Text);

  if SelectDirectoryDialog1.Execute then
  begin
    SelectedDir := IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);

    if not HasSupportedMediaFiles(SelectedDir) then
    begin
      if Assigned(ExecOutput) then
      begin
          // Add a newline first if the output doesn't always end with one
          if (Length(ExecOutput.Text) > 0) and (ExecOutput.Text[Length(ExecOutput.Text)] <> #10) then
             ExecOutput.Lines.Add('');
          ExecOutput.Lines.Add('[Warning] The selected directory "' + SelectedDir + '" does not contain any supported audio or video files.');
      end;
    end;

    TbSource.Text := SelectedDir;
  end;
end;


procedure TFormMain.BtnConvertClick(Sender: TObject);
var
  SourceDir: string;
  OutputDir: string;
  FfmpegDir: string;
  TargetFormat: string;
  SearchRec: TSearchRec;
  FileFound: Integer;
  Command: string;
  FileExt: string;
  Process: TProcess;
begin
  SourceDir := Trim(TbSource.Text);
  OutputDir := Trim(TbOutput.Text);
  FfmpegDir := '';

  if (SourceDir = '') or (OutputDir = '') then
  begin
    MessageBox(0, 'Please ensure all fields are filled in.', 'Error', MB_ICONERROR);
    Exit;
  end;

  if not DirectoryExists(SourceDir) then
  begin
    MessageBox(0, 'The source directory does not exist. Please select a valid directory.', 'Error', MB_ICONERROR);
    Exit;
  end;

  if not DirectoryExists(OutputDir) then
  begin
    MessageBox(0, 'The output directory does not exist.', 'Error', MB_ICONERROR);
    Exit;
  end;

  if Pos('bin', LowerCase(FfmpegDir)) = 0 then
  begin
    if DirectoryExists(IncludeTrailingPathDelimiter(TbFfmpeg.Text) + 'bin') then
      FfmpegDir := IncludeTrailingPathDelimiter(TbFfmpeg.Text) + 'bin'
    else
      FfmpegDir := Trim(TbFfmpeg.Text);
  end;

  if not DirectoryExists(FfmpegDir) then
  begin
    MessageBox(0, 'The FFmpeg directory does not exist. Please download it by clicking the download button.', 'Error', MB_ICONERROR);
    Exit;
  end;

  case CbTargetFmt.ItemIndex of
    0: TargetFormat := 'mp3';
    1: TargetFormat := 'wav';
    2: TargetFormat := 'flac';
  else
    MessageBox(0, 'Please select a valid target format.', 'Error', MB_ICONERROR);
    Exit;
  end;

  FileFound := FindFirst(SourceDir + '*.*', faAnyFile, SearchRec);
  try
    while FileFound = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        FileExt := LowerCase(ExtractFileExt(SearchRec.Name));
        if (FileExt = '.mp3') or (FileExt = '.wav') or (FileExt = '.flac') then
        begin
          Command := Format('"%s%s" -i "%s%s" "%s%s.%s"', [FfmpegDir, PathDelim + 'ffmpeg', SourceDir, SearchRec.Name, OutputDir, ChangeFileExt(SearchRec.Name, ''), TargetFormat]);

          ExecOutput.Lines.Add(Command);

          Process := TProcess.Create(nil);
          try
            try
              Process.Executable := FfmpegDir + PathDelim + 'ffmpeg.exe';
              Process.Parameters.Add(Format('-i "%s%s" "%s%s.%s"', [SourceDir, SearchRec.Name, OutputDir, ChangeFileExt(SearchRec.Name, ''), TargetFormat]));
              Process.Options := Process.Options + [poUsePipes];
              Process.ShowWindow := swoHIDE;
              Process.Execute;

              if Process.ExitCode <> 0 then
              begin
                MessageBox(0, PChar('Failed to execute FFmpeg command. Exit code: ' + IntToStr(Process.ExitCode)), 'Error', MB_ICONERROR);
                Exit;
              end;
            except on E: Exception do begin
              MessageBox(0, 'Error starting process', 'Error', MB_ICONERROR);
              StatusBar.SimpleText := 'Conversion error. ';
              end;
          end;
          finally
            Process.Free;
          end;
        end;
      end;
      FileFound := FindNext(SearchRec);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;

  StatusBar.SimpleText := 'Conversion completed successfully!';
  ExecOutput.Append('Conversion completed successfully!');
end;

procedure TFormMain.BtnChooseOutClick(Sender: TObject);
begin
  if SelectDirectoryDialog2.Execute then
     TbOutput.Text := SelectDirectoryDialog2.FileName + PathDelim;
end;

procedure TFormMain.BtnChooseFfmpegClick(Sender: TObject);
var
  FfmpegPath: string;
  BinPath: string;
begin
  if (TbFfmpeg.Text <> '') and DirectoryExists(TbFfmpeg.Text) then
     SelectDirectoryDialog3.FileName := TbFfmpeg.Text;

  if SelectDirectoryDialog3.Execute then
  begin
    FfmpegPath := SelectDirectoryDialog3.FileName + PathDelim;
    BinPath := FfmpegPath + 'bin' + PathDelim + 'ffmpeg.exe';

    // Check if the bin folder exists and if ffmpeg.exe is present
    if not DirectoryExists(FfmpegPath + 'bin') then
    begin
      MessageBox(0, 'Error: The "bin" subfolder does not exist in the selected directory. Please download the FFmpeg binary zip, extract it to a folder, and select the folder containing the "bin" folder.', 'Error', MB_ICONERROR);
      Exit;
    end;

    if not FileExists(BinPath) then
    begin
      MessageBox(0, 'Error: The file "ffmpeg.exe" does not exist in the "bin" subfolder. Please download the FFmpeg binary zip, extract it to a folder, and select the folder containing the "bin" folder.', 'Error', MB_ICONERROR);
      Exit;
    end;

    TbFfmpeg.Text := FfmpegPath;
  end;
end;

procedure TFormMain.BtnOpenFfmpegDlMouseEnter(Sender: TObject);
begin
  Statusbar.SimpleText := 'Click to download FFmpeg binaries.';
end;

procedure TFormMain.BtnAboutClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TFormMain.BtnSameAsSourceClick(Sender: TObject);
begin
  TbOutput.Text := TbSource.Text;
end;

end.

