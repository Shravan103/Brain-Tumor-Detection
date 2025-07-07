# Brain-Tumor-Detection
A Python-based brain tumor detection application, leveraging computer vision and image processing techniques. Built using OpenCV for image manipulation, Tkinter for the graphical user interface, NumPy for numerical computations, and PIL (Python Imaging Library) for image handling, this project aims to detect and analyze brain tumors from MRI scans.


unit CalculatorApp;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls;

type
  TCalculator = class(TForm)
    CalcDisplay: TLabel;

    AddSign, MinusSign, MultiplySign, DivideSign: TButton;
    One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Zero: TButton;
    Decimal, ClearBtn, DeleteBtn, EqualsSign, PlusMinusToggle: TButton;

    procedure FormCreate(Sender: TObject);
    procedure NumberClick(Sender: TObject);
    procedure OperatorClick(Sender: TObject);
    procedure EqualsSignClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DecimalClick(Sender: TObject);
    procedure PlusMinusToggleClick(Sender: TObject);
  private
    { Private declarations }
    Operand1: Double;
    Operator: string;
    IsNewInput: Boolean;
  public
    { Public declarations }
  end;

var
  Calculator: TCalculator;

implementation

{$R *.dfm}

procedure TCalculator.FormCreate(Sender: TObject);
begin
  CalcDisplay.Caption := '0';
  IsNewInput := True;
end;

procedure TCalculator.NumberClick(Sender: TObject);
begin
  if IsNewInput or (CalcDisplay.Caption = '0') then
    CalcDisplay.Caption := '';

  CalcDisplay.Caption := CalcDisplay.Caption + (Sender as TButton).Caption;
  IsNewInput := False;
end;

procedure TCalculator.OperatorClick(Sender: TObject);
begin
  Operand1 := StrToFloatDef(CalcDisplay.Caption, 0);
  Operator := (Sender as TButton).Caption;
  IsNewInput := True;
end;

procedure TCalculator.EqualsSignClick(Sender: TObject);
var
  Operand2, Result: Double;
begin
  Operand2 := StrToFloatDef(CalcDisplay.Caption, 0);

  try
    if Operator = '+' then
      Result := Operand1 + Operand2
    else if Operator = '-' then
      Result := Operand1 - Operand2
    else if Operator = '×' then
      Result := Operand1 * Operand2
    else if Operator = '÷' then
    begin
      if Operand2 = 0 then
        raise Exception.Create('Cannot divide by zero');
      Result := Operand1 / Operand2;
    end
    else
      Result := Operand2; // No operator pressed

    CalcDisplay.Caption := FloatToStr(Result);
    IsNewInput := True;
  except
    on E: Exception do
    begin
      CalcDisplay.Caption := 'Error';
      IsNewInput := True;
    end;
  end;
end;

procedure TCalculator.ClearBtnClick(Sender: TObject);
begin
  CalcDisplay.Caption := '0';
  Operand1 := 0;
  Operator := '';
  IsNewInput := True;
end;

procedure TCalculator.DeleteBtnClick(Sender: TObject);
begin
  if Length(CalcDisplay.Caption) > 1 then
    CalcDisplay.Caption := Copy(CalcDisplay.Caption, 1, Length(CalcDisplay.Caption) - 1)
  else
    CalcDisplay.Caption := '0';
end;

procedure TCalculator.DecimalClick(Sender: TObject);
begin
  if Pos('.', CalcDisplay.Caption) = 0 then
    CalcDisplay.Caption := CalcDisplay.Caption + '.';
end;

procedure TCalculator.PlusMinusToggleClick(Sender: TObject);
var
  Value: Double;
begin
  Value := StrToFloatDef(CalcDisplay.Caption, 0);
  Value := -Value;
  CalcDisplay.Caption := FloatToStr(Value);
end;

end.