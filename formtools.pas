unit FormTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ShellAPI, Windows;

type

  { TToolsForm }

  TToolsForm = class(TForm)
    BtnSetFromSource: TButton;
    BtnRun: TButton;
    BtnChooseFolder: TButton;
    CbbFileType: TComboBox;
    CbDeleteFiles: TCheckBox;
    LeFromFolder: TLabeledEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure BtnChooseFolderClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnSetFromSourceClick(Sender: TObject);
    procedure CbDeleteFilesChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private
    procedure SetControlsEnabled(Flag: boolean);

  public

  end;

var
  ToolsForm: TToolsForm;

implementation
uses unit1;

{$R *.lfm}

{ TToolsForm }

procedure TToolsForm.SetControlsEnabled(Flag: boolean);
begin
  CbbFileType.Enabled := Flag;
  BtnRun.Enabled := Flag;
  BtnChooseFolder.Enabled := Flag;
  LeFromFolder.Enabled := Flag;
  BtnSetFromSource.Enabled := Flag;
end;

procedure TToolsForm.CbDeleteFilesChange(Sender: TObject);
begin
     SetControlsEnabled(CbDeleteFiles.Checked);
end;

procedure TToolsForm.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  FileName: String;
  Directory: String;
begin
  if not LeFromFolder.Enabled then Exit;
  if Length(FileNames) > 0 then
  begin
    FileName := FileNames[0];
    if DirectoryExists(FileName) then
      LeFromFolder.Text := IncludeTrailingPathDelimiter(FileName)
    else
    begin
      Directory := ExtractFilePath(FileName);
      LeFromFolder.Text := IncludeTrailingPathDelimiter(Directory);
    end;
  end;
end;

procedure TToolsForm.BtnChooseFolderClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
     LeFromFolder.Text:=SelectDirectoryDialog1.FileName;
end;

procedure TToolsForm.BtnRunClick(Sender: TObject);
var
  FileType: string;
  SearchRec: TSearchRec;
  FileFound: Integer;
  FolderPath: string;
  Confirmation: Integer;
  NumFilesDeleted: Integer;
  FileOp: TSHFileOpStruct;
  FullPathToFile: string;
  PathWithMask: string;   // Use full path for searching
begin
  FolderPath := Trim(LeFromFolder.Text);

  if FolderPath <> '' then
    FolderPath := IncludeTrailingPathDelimiter(FolderPath);

  if not DirectoryExists(FolderPath) then
  begin
     MessageBox(Handle, 'Error: The specified folder does not exist.', 'Error', MB_ICONERROR);
     Exit;
  end;

  if CbDeleteFiles.Checked then
  begin
    case CbbFileType.ItemIndex of
      0: FileType := '*.mp3';
      1: FileType := '*.wav';
      2: FileType := '*.flac';
    else
      MessageBox(Handle, 'Please select a file type to delete.', 'Info', MB_ICONINFORMATION);
      Exit;
    end;

    Confirmation := MessageDlg(Format('Are you sure you want to move all %s files from %s to the Recycle Bin?', [FileType, FolderPath]),
                               mtConfirmation, [mbYes, mbNo], 0);

    if Confirmation = mrYes then
    begin
      SetControlsEnabled(false);
      CbDeleteFiles.Enabled := false;

      NumFilesDeleted := 0;
      PathWithMask := FolderPath + FileType; // e.g., C:\MyMusic\*.mp3

      ZeroMemory(@FileOp, SizeOf(FileOp));
      FileOp.wFunc := FO_DELETE;
      FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION; // Use Recycle Bin, no "are you sure" for each file

      FileFound := FindFirst(PathWithMask, faAnyFile, SearchRec);
      try
        while FileFound = 0 do
        begin
          FullPathToFile := FolderPath + SearchRec.Name;

          FileOp.pFrom := PChar(FullPathToFile + #0#0); // Double null-terminate the single path

          if SHFileOperation(@FileOp) = 0 then // 0 indicates success
            Inc(NumFilesDeleted)
          else
          begin
             FormMain.ExecOutput.Lines.Add('Failed to delete: ' + FullPathToFile);
          end;

          FileFound := FindNext(SearchRec);
        end;
      finally
        Sysutils.FindClose(SearchRec);
      end;

      SetControlsEnabled(true);
      CbDeleteFiles.Enabled := true;

      if NumFilesDeleted > 0 then
      begin
        MessageBox(Handle, PChar(Format('%d file(s) of type %s moved to the Recycle Bin from %s.', [NumFilesDeleted, FileType, FolderPath])), 'Info', MB_ICONINFORMATION);
        //CbDeleteFiles.Checked := False;
      end
      else
        MessageBox(Handle, PChar(Format('No files matching %s were found in %s.', [FileType, FolderPath])), 'Info', MB_ICONINFORMATION); // Changed to Info, not an error
    end;
  end;
end;


procedure TToolsForm.BtnSetFromSourceClick(Sender: TObject);
begin
  if LeFromFolder.Enabled then
  begin
    if Assigned(FormMain) and Assigned(FormMain.TbSource) then
    begin
      LeFromFolder.Text := FormMain.TbSource.Text;
    end
    else
    begin
       MessageBox(0, 'Error: Could not access the main form source path.', 'Internal Error', MB_ICONERROR);
    end;
  end
  else
  begin
       MessageBox(0, 'Enable the delete option first to set the folder.', 'Info', MB_ICONINFORMATION);
  end;
end;

end.

