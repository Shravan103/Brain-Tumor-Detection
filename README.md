# Brain-Tumor-Detection
A Python-based brain tumor detection application, leveraging computer vision and image processing techniques. Built using OpenCV for image manipulation, Tkinter for the graphical user interface, NumPy for numerical computations, and PIL (Python Imaging Library) for image handling, this project aims to detect and analyze brain tumors from MRI scans.
unit CalculatorApp;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TCalculator = class(TForm)
    CalcDisplay: TLabel;
    AddSign, MinusSign, MultiplySign, DivideSign, EqualsSign: TButton;
    One, Two, Three, Four, Five, Six, Seven, Eight, Nine: TButton;
    ClearBtn, DeleteBtn, PlusMinusToggle: TButton;
    Zero, Dot: TButton; // Add if not yet created

    procedure NumberClick(Sender: TObject);
    procedure OperatorClick(Sender: TObject);
    procedure EqualsSignClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure PlusMinusToggleClick(Sender: TObject);
    procedure DotClick(Sender: TObject);
  private
    operand1: Double;
    currentOperator: string;
    isNewInput: Boolean;
  public
  end;

var
  Calculator: TCalculator;

implementation

{$R *.dfm}

procedure TCalculator.NumberClick(Sender: TObject);
begin
  if isNewInput then
  begin
    CalcDisplay.Caption := '';
    isNewInput := False;
  end;

  if CalcDisplay.Caption = '0' then
    CalcDisplay.Caption := '';

  CalcDisplay.Caption := CalcDisplay.Caption + (Sender as TButton).Caption;
end;

procedure TCalculator.OperatorClick(Sender: TObject);
begin
  operand1 := StrToFloatDef(CalcDisplay.Caption, 0);
  currentOperator := (Sender as TButton).Caption;
  isNewInput := True;
end;

procedure TCalculator.EqualsSignClick(Sender: TObject);
var
  operand2, result: Double;
begin
  operand2 := StrToFloatDef(CalcDisplay.Caption, 0);

  if currentOperator = '+' then
    result := operand1 + operand2
  else if currentOperator = '-' then
    result := operand1 - operand2
  else if currentOperator = '×' then
    result := operand1 * operand2
  else if currentOperator = '÷' then
  begin
    if operand2 = 0 then
    begin
      CalcDisplay.Caption := 'Error';
      Exit;
    end;
    result := operand1 / operand2;
  end
  else
    result := operand2;

  CalcDisplay.Caption := FloatToStr(result);
  isNewInput := True;
end;

procedure TCalculator.ClearBtnClick(Sender: TObject);
begin
  CalcDisplay.Caption := '0';
  operand1 := 0;
  currentOperator := '';
  isNewInput := True;
end;

procedure TCalculator.DeleteBtnClick(Sender: TObject);
begin
  if Length(CalcDisplay.Caption) > 1 then
    CalcDisplay.Caption := Copy(CalcDisplay.Caption, 1, Length(CalcDisplay.Caption) - 1)
  else
    CalcDisplay.Caption := '0';
end;

procedure TCalculator.PlusMinusToggleClick(Sender: TObject);
var
  value: Double;
begin
  value := StrToFloatDef(CalcDisplay.Caption, 0);
  value := -value;
  CalcDisplay.Caption := FloatToStr(value);
end;

procedure TCalculator.DotClick(Sender: TObject);
begin
  if Pos('.', CalcDisplay.Caption) = 0 then
    CalcDisplay.Caption := CalcDisplay.Caption + '.';
end;

end.