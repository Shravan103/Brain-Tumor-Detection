# Brain-Tumor-Detection
A Python-based brain tumor detection application, leveraging computer vision and image processing techniques. Built using OpenCV for image manipulation, Tkinter for the graphical user interface, NumPy for numerical computations, and PIL (Python Imaging Library) for image handling, this project aims to detect and analyze brain tumors from MRI scans.

uses
  IniFiles, System.SysUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';
  Ini := TIniFile.Create(IniPath);
  try
    Left := Ini.ReadInteger('FormSettings', 'Left', Left);
    Top := Ini.ReadInteger('FormSettings', 'Top', Top);
    Width := Ini.ReadInteger('FormSettings', 'Width', Width);
    Height := Ini.ReadInteger('FormSettings', 'Height', Height);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Ini: TIniFile;
  IniPath: string;
begin
  IniPath := ExtractFilePath(ParamStr(0)) + 'settings.ini';
  Ini := TIniFile.Create(IniPath);
  try
    Ini.WriteInteger('FormSettings', 'Left', Left);
    Ini.WriteInteger('FormSettings', 'Top', Top);
    Ini.WriteInteger('FormSettings', 'Width', Width);
    Ini.WriteInteger('FormSettings', 'Height', Height);
  finally
    Ini.Free;
  end;
end;