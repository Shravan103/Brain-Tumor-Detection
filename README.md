# Brain-Tumor-Detection
A Python-based brain tumor detection application, leveraging computer vision and image processing techniques. Built using OpenCV for image manipulation, Tkinter for the graphical user interface, NumPy for numerical computations, and PIL (Python Imaging Library) for image handling, this project aims to detect and analyze brain tumors from MRI scans.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Load CSV data into the grid when form starts
procedure TForm1.FormCreate(Sender: TObject);
var
  FileLines: TStringList;
  i, j: Integer;
  Parts: TArray<string>;
begin
  // Set up grid columns and header row
  StringGrid1.ColCount := 5;
  StringGrid1.RowCount := 2; // Start with header + 1 empty row
  StringGrid1.FixedRows := 1;

  // Enable editing
  StringGrid1.Options := StringGrid1.Options + [goEditing];

  // Set header names
  StringGrid1.Cells[0, 0] := 'SrNo';
  StringGrid1.Cells[1, 0] := 'Name';
  StringGrid1.Cells[2, 0] := 'Address';
  StringGrid1.Cells[3, 0] := 'Age';
  StringGrid1.Cells[4, 0] := 'Role';

  // Load data if file exists
  if FileExists('UserData.csv') then
  begin
    FileLines := TStringList.Create;
    try
      FileLines.LoadFromFile('UserData.csv');
      StringGrid1.RowCount := FileLines.Count + 1;

      for i := 0 to FileLines.Count - 1 do
      begin
        Parts := FileLines[i].Split([',']);
        for j := 0 to Length(Parts) - 1 do
          StringGrid1.Cells[j, i + 1] := Parts[j]; // i+1 to skip header row
      end;
    finally
      FileLines.Free;
    end;
  end;
end;

// Save grid data back to CSV when button is clicked
procedure TForm1.Button1Click(Sender: TObject);
var
  FileLines: TStringList;
  i, j: Integer;
  Line: string;
begin
  FileLines := TStringList.Create;
  try
    for i := 1 to StringGrid1.RowCount - 1 do // Skip header row
    begin
      Line := '';
      for j := 0 to StringGrid1.ColCount - 1 do
      begin
        Line := Line + StringGrid1.Cells[j, i];
        if j < StringGrid1.ColCount - 1 then
          Line := Line + ',';
      end;
      if Trim(Line) <> '' then
        FileLines.Add(Line);
    end;

    FileLines.SaveToFile('UserData.csv');
    ShowMessage('Data saved!');
  finally
    FileLines.Free;
  end;
end;

end.