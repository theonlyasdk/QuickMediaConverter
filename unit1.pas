unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  lclintf, Windows, FormAbout, FormTools, LCLType,
  Clipbrd, Buttons, ComCtrls, Spin, Process;

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
    CbAudioChannels: TComboBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LblBitrate: TLabel;
    LblTitle: TLabel;
    Label2: TLabel;
    ExecOutput: TMemo;
    PageControl1: TPageControl;
    ScrollBox1: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SelectDirectoryDialog2: TSelectDirectoryDialog;
    SelectDirectoryDialog3: TSelectDirectoryDialog;
    SeSampleRate: TSpinEdit;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TbSource: TLabeledEdit;
    TbOutput: TLabeledEdit;
    TbFfmpeg: TLabeledEdit;
    TbBitrate: TTrackBar;
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
    procedure CbTargetFmtChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    function HasSupportedMediaFiles(const ADirectory: string): Boolean;
    procedure TbBitrateChange(Sender: TObject);
  private
    procedure FormatChanged();
    procedure UpdateBitrate();
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

function TrackbarProgressToBitrate(Progress: Integer): string;
begin
     // 8, 16, 24, 32, 40, 48, 64, 80, 96, 112, 128, 160, 192, 224, 256, or 320
     case Progress of
       1: Exit('8');
       2: Exit('16');
       3: Exit('24');
       4: Exit('32');
       5: Exit('40');
       6: Exit('48');
       7: Exit('64');
       8: Exit('80');
       9: Exit('96');
       10: Exit('112');
       11: Exit('128');
       12: Exit('160');
       13: Exit('192');
       14: Exit('224');
       15: Exit('256');
       16: Exit('320');
       else Exit('256');
     end;
end;

procedure TFormMain.UpdateBitrate();
begin;
  LblBitrate.Caption := Concat(TrackbarProgressToBitrate(TbBitrate.Position), 'Kbit/s');
end;

procedure TFormMain.TbBitrateChange(Sender: TObject);
begin
     UpdateBitrate();
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  DownloadsPath: array[0..MAX_PATH] of Char; // Buffer for the path
  SearchRec: TSearchRec;
  FileFound: Integer;
  FfmpegDir: string;
begin
  UpdateBitrate();

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
  MessageDlg('1. Download ffmpeg-master-latest-win64-lgpl-shared.zip and extract it to your Downloads folder.' + #13#10 + '2. Choose the directory using the choose button. (Or restart the application and it will automatically detect the folder from Downloads)', mtInformation, [mbOK], 0);
end;

procedure TFormMain.BtnSameAsSourceMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := 'Click to set the output folder to the same location as the source folder.';
end;

procedure TFormMain.BtnToolsClick(Sender: TObject);
begin
  ToolsForm.Show;
end;

procedure TFormMain.FormatChanged();
begin
     TbBitrate.Enabled := (CbTargetFmt.ItemIndex = 0);
     LblBitrate.Caption := '...';
     if (CbTargetFmt.ItemIndex = 0) then UpdateBitrate();
end;

procedure TFormMain.CbTargetFmtChange(Sender: TObject);
begin
     FormatChanged();
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

function StringsToSpaceSeparated(const Strings: TStrings): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Strings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ' '; // Add a space if it's not the first string
    Result := Result + Strings[i];
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
  FileExt: string;
  Process: TProcess;
  OutputOptions, OutputFile: string;
begin
  SourceDir := Trim(TbSource.Text);
  OutputDir := Trim(TbOutput.Text);
  FfmpegDir := '';
  OutputOptions := '';

  if (SourceDir = '') or (OutputDir = '') then
  begin
    MessageDlg('Please ensure all fields are filled in.', mtError, [mbOK], 0);
    Exit;
  end;

  if not DirectoryExists(SourceDir) then
  begin
    MessageDlg('The source directory does not exist. Please select a valid directory.', mtError, [mbOK], 0);
    Exit;
  end;

  if not DirectoryExists(OutputDir) then
  begin
    MessageDlg('The output directory does not exist.', mtError, [mbOK], 0);
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
    MessageDlg('The FFmpeg directory does not exist. Please download it by clicking the download button.', mtError, [mbOK], 0);
    Exit;
  end;

  case CbTargetFmt.ItemIndex of
    0: TargetFormat := 'mp3';
    1: TargetFormat := 'wav';
    2: TargetFormat := 'flac';
  else
    MessageDlg('Please select a valid target format.', mtError, [mbOK], 0);
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
          Process := TProcess.Create(nil);
          try
            try
              // If it is mp3
              if CbTargetFmt.ItemIndex = 0 then
              begin
                   OutputOptions := Format(
                     '-b:a %s -ar %s',
                     [Concat(TrackbarProgressToBitrate(TbBitrate.Position), 'K'), SeSampleRate.Value.ToString]
                   );
              end;

              if CbAudioChannels.ItemIndex > 0 then
              begin
                OutputOptions := OutputOptions + ' ' + Format('-ac %d', [CbAudioChannels.ItemIndex]);
              end;

              Process.Executable := FfmpegDir + PathDelim + 'ffmpeg.exe';
              Process.Parameters.Add(Format('-i "%s%s" %s "%s%s.%s"',
                 [SourceDir, SearchRec.Name, OutputOptions, OutputDir, ChangeFileExt(SearchRec.Name, ''), TargetFormat]
              ));

              OutputFile := Format('%s%s.%s', [OutputDir, ChangeFileExt(SearchRec.Name, ''), TargetFormat]);
              if FileExists(OutputFile) then
              begin
                if MessageDlg('File Already exists, continue?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
                  Exit;
              end;

              ExecOutput.Lines.Add(
                Concat(FfmpegDir, PathDelim, 'ffmpeg.exe', ' ', StringsToSpaceSeparated(Process.Parameters))
              );

              Process.Options := Process.Options + [poUsePipes];
              Process.ShowWindow := swoHIDE;
              Process.Execute;

              if Process.ExitCode <> 0 then
              begin
                MessageDlg('Failed to execute FFmpeg command. Exit code: ' + IntToStr(Process.ExitCode), mtError, [mbOK], 0);
                Exit;
              end;
            except on E: Exception do begin
              MessageDlg('Error starting process', mtError, [mbOK], 0);
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
      MessageDlg('Error: The "bin" subfolder does not exist in the selected directory. Please download the FFmpeg binary zip, extract it to a folder, and select the folder containing the "bin" folder.', mtError, [mbOK], 0);
      Exit;
    end;

    if not FileExists(BinPath) then
    begin
      MessageDlg('Error: The file "ffmpeg.exe" does not exist in the "bin" subfolder. Please download the FFmpeg binary zip, extract it to a folder, and select the folder containing the "bin" folder.', mtError, [mbOK], 0);
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

