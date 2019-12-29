unit EditExp;

{
 EditExp Ver 1.00

 Expression Evaluator
 by Ricardo Barrenechea Compuserve 73050,261
                        Internet   RBarre@spi-cis.com

}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Grids;

type
  TEditExp = class(TEdit)
  private
    { Private declarations }
  protected
    { Protected declarations }

    FValue  : double;        { Value of expression}
    FSValue : string;        { Value of expression - string}
    FMask   : string;        { Mask to convert FValue to FSValue}

    FError  : integer;       { Number of Error}

    FVarGrid : TStringGrid; { TStringGrid where to read Var's}
    FUseVar  : boolean;     { Use Var's ?}
    FExp     : string;      { Expression without Var's}


    { Get Functions for Properties}
    function GetValue  : double;
    function GetSValue : string;
    function GetExp    : string;


    { Set Procedure for properties}
    procedure SetMask(value :string);
    procedure SetVarGrid(value:TStringGrid);
    procedure SetUseVar(value:boolean);

    { Protected Methods}
    procedure Calc;                       { CORE !}
    procedure ChangeVar;                  { Internal to Change Var's to their values}


  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy;                   override;


    { Public Methods}
    function Error    : boolean;          { True or false}
    function GetError : string;           { Error string}


    { Public Properties}
    property  Value  : double read GetValue;     { VALUE }
    property  SValue : string read GetSValue;    { STRING}
    property  Exp    : string read GetExp;       { EXPRESSION}

  published
    { Published declarations }
    property  Mask    : string read FMask write SetMask;
    property  VarGrid : TStringGrid read FVarGrid write SetVarGrid;
    property  UseVar  : boolean read FUseVar write SetUseVar;
  end;


{ Global FUNCTION !}
function Evaluate(formula: string; var status,location: integer) : double;

procedure Register;


implementation


{================================== EVALUATE ================================}
{ ------------------------------- Declarations ----------------------------- }

const MaxReal = 1.0E+37;       { Maximum real value that we will allow }
      MaxFact = 33;            { 33! = E+37 }
      MaxExpo = 85;            { exp(85) = E+37 }



var   Region,                  { Check if result is defined. eg fact(-1.2) }
      Divzero,                 { Check if a division by zero occured. eg 1/0 }
      Overflow,                { Check if result becomes too large. eg 100! }
      Complex : boolean;       { Check if result is complex. eg sqrt(-1) }


{ -------------------------------------------------------------------------- }



constructor TEditExp.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);

     { No Error, No Var's, No Grid}
     FError:=0;
     FUseVar:=false;
     FVarGrid:=nil;
end;


destructor TEditExp.Destroy;
begin
     inherited Destroy;
end;


procedure TEditExp.ChangeVar;
var i,k     : integer;
    CellVar : string;

begin
   { Change Var's labels with Var's values}
   with FVarGrid do
   begin
      if (ColCount>2) and (RowCount>1) then
      begin
         i:=1;
         while (i<RowCount) and (Cells[1,i]<>'') do
         begin
              CellVar:='['+UpperCase(Cells[1,i])+']';

              { All the ocurrences of CellVar in Expression}
              repeat
                  k:=Pos(CellVar, FExp);

                  if k>0 then
                  begin
                       delete(FExp, k, length(CellVar) );
                       insert(Cells[2,i], FExp, k);
                  end;
              until k=0;

              Inc(i);

         end; {while}
      end; {if}
   end; {with}
end;

procedure TEditExp.Calc;
var cod,loc : integer;
begin
     FExp:=UpperCase(Text);
     if (FUseVar) and (FVarGrid<>nil) then
     begin
          ChangeVar;
     end;

     { I don't use loc [location of Error] (till now)}
     FValue:=Evaluate(FExp,cod,loc); { CORE !!!}
     if cod<>0 then FValue:=0;
     FError:=cod;
end;

{
 GetValue GetExp & GetSValue
 first do CALC
 It can be done in a diferent way !
}
function TEditExp.GetValue : double;
begin
     Calc;
     GetValue :=FValue;
end;

function TEditExp.GetExp : string;
begin
     Calc;
     GetExp :=FExp;
end;

function TEditExp.GetSValue: string;
begin
     Calc;
     GetSValue :=FormatFloat(FMask,FValue);
end;

function TEditExp.GetError : string;
var s: string;
begin
     Case FError of
          0: s:=' No error ';
          1: s:=' Illegal character';
          2: s:=' Incorrect syntax';
          3: s:=' Illegal or missing parenthese';
          4: s:=' Incorrect real format';
          5: s:=' Illegal function';
          6: s:=' Result is undefined';
          7: s:=' Result is too large';
          8: s:=' Result is complex';
          9: s:=' Division by zero';
     end;
     GetError:=s;
end;


procedure TEditExp.SetMask(value :string);
begin
     FMask:=value;
end;

function TEditExp.Error : boolean;
begin
     Error:= (FError<>0);
end;

procedure TEditExp.SetVarGrid(value:TStringGrid);
begin
     FVarGrid:=value;
end;

procedure TEditExp.SetUseVar(value:boolean);
begin
     FUseVar:=value;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TEditExp]);
end;



{============================================================================}
{================================== EVALUATE ================================}
{ From a VERY VERY OLD Pascal 3.0 Function !
{============================================================================}


{ Initialize boolean flags. }
  procedure Init_Booleans;
  begin
    region:=true;              { Result is initially inside region }
    complex:=false;            { Result is not complex }
    divzero:=false;            { There is no division by zero }
    overflow:=false;           { and no overflow. }
  end;  { Init_Booleans }



{ Check to see if doing an operation on a and b will cause an overflow and
  set the OVERFLOW boolean accordingly }

  procedure CheckOverflow(a,b: real; operation: char);
  begin
    case operation of
      '*':  if abs(a) > 1.0 then overflow:=abs(b) > MaxReal/abs(a);
      '/':  if b <> 0.0 then
            begin
              b:=1/b;
              if abs(a) > 1.0 then overflow:=abs(b) > MaxReal/abs(a)
            end
            else overflow:=true;
      '+':  if b > 0.0 then overflow:=a > (MaxReal - b)
            else overflow:=a < (-MaxReal - b);
      '-':  if b < 0.0 then overflow:=a > (MaxReal + b)
            else overflow:=a < (-MaxReal + b);
      else overflow:=true;                       { Default for bad operation }
    end;  { case }
  end;  { CheckOverflow }


{ ----------------------------------------------------------------------------
   The following functions -- asin,acos,tan,cot,sec,csc,sinh,cosh,tanh,sech,
   csch,coth,fact -- will default to 0.0 if a division by zero occurs, the
   result is complex or undefined, or if an overflow occurs.

   If you are using these functions independently of the Evaluate procedure
   then the procedure Init_Booleans should be called first to reinitialize
   error checking.
---------------------------------------------------------------------------- }


                             { --- Arc Sine --- }

  function Asin(r: real): real;
  begin
    if abs(r) < 1.0 then asin:=arctan(r/sqrt(1-r*r))
    else
      if abs(r) = 1.0 then asin:=(r/abs(r))*pi/2
      else
      begin
        asin:=0.0;
        complex:=true
      end  { else }
  end;  { asin }

                            { --- Arc Cosine --- }

  function Acos(r: real): real;
  begin
    if r = 0.0 then acos:=pi/2
    else
    begin
      if abs(r) < 1.0 then
      begin
        if r < 0.0 then acos:=arctan(sqrt(1-r*r)/r)+pi
        else acos:=arctan(sqrt(1-r*r)/r)
      end
      else
        if abs(r) = 1.0 then
          if r = 1.0 then acos:=0.0
          else acos:=pi
        else
        begin
          acos:=0.0;
          complex:=true
        end
    end
  end;  { acos }

                             { --- Tangent --- }

  function Tan(r: real): real;
  begin
    if cos(r) = 0.0 then
    begin
      tan:=0.0;
      divzero:=true
    end
    else tan:=sin(r)/cos(r)
  end;  { tan }

                            { --- CoTangent --- }

  function Cot(r: real): real;
  begin
    if sin(r) = 0.0 then
    begin
      cot:=0.0;
      divzero:=true
    end
    else cot:=cos(r)/sin(r)
  end;  { cot }

                              { --- Secant --- }

  function Sec(r: real): real;
  begin
    if cos(r) = 0.0 then
    begin
      sec:=0.0;
      divzero:=true
    end
    else sec:=1/cos(r)
  end;  { Sec }

                            { --- CoSecant --- }

  function Csc(r: real): real;
  begin
    if sin(r) = 0.0 then
    begin
      csc:=0.0;
      divzero:=true
    end
    else csc:=1/sin(r)
  end;  { Csc }

                               { --- Sinh --- }

  function Sinh(r: real): real;
  begin
    if abs(r) <= MaxExpo then sinh:=(exp(r)-exp(-r))/2
    else
    begin
      overflow:=true;
      sinh:=0.0
    end
  end;  { Sinh }

                             { --- CoSinh --- }

  function Cosh(r: real): real;
  begin
    if abs(r) <= MaxExpo then cosh:=(exp(r)+exp(-r))/2
    else
    begin
      overflow:=true;
      cosh:=0.0
    end
  end;  { Cosh }

                               { --- Tanh --- }

  function Tanh(r: real): real;
  begin
    if cosh(r) = 0.0 then
    begin
      tanh:=0.0;
      divzero:=true
    end
    else
    begin
      CheckOverflow(sinh(r),cosh(r),'/');
      if not overflow then tanh:=sinh(r)/cosh(r)
      else tanh:=0.0
    end
  end;  { Tanh }

                               { --- Sech --- }

  function Sech(r: real): real;
  begin
    if cosh(r) = 0.0 then
    begin
      sech:=0.0;
      divzero:=true
    end
    else
    begin
      CheckOverflow(1,cosh(r),'/');
      if not overflow then sech:=1/cosh(r)
      else sech:=0.0
    end
  end;  { Sech }

                             { --- CoSech --- }

  function Csch(r: real): real;
  begin
    if sinh(r) = 0.0 then
    begin
      csch:=0.0;
      divzero:=true
    end
    else
    begin
      CheckOverflow(1,sinh(r),'/');
      if not overflow then csch:=1/sinh(r)
      else csch:=0.0
    end
  end;  { Csch }

                             { --- CoTanh --- }

  function Coth(r: real): real;
  begin
    if sinh(r) = 0.0 then
    begin
      coth:=0.0;
      divzero:=true
    end
    else
    begin
      CheckOverflow(cosh(r),sinh(r),'/');
      if not overflow then coth:=cosh(r)/sinh(r)
      else coth:=0.0
    end
  end;  { Coth }

                            { --- Factorial --- }

  function Fact(r:real): real;
  var i: integer;
      resulta: real;
  begin
    if (r < 0.0) or (trunc(r) <> r) then
    begin
      resulta:=0.0;
      region:=false
    end
    else
    begin
      resulta:=1.0;
      if trunc(r) < MaxFact then
        for i:=1 to trunc(r) do
          resulta:=resulta*i
      else
      begin
        overflow:=true;
        resulta:=0.0
      end
    end;
    fact:=resulta
  end;  { Fact }


{ ----------------------------------------------------------------------------
  The function Evaluate is passed a mathematical expression in the form of a
  string (formula) to be evaluated and returns the following:

  If no errors occur during evaluation then:
      Result = evaluated expression
      Status = 0
      Location = 0

  If an error occurs then:
      Result = 0.0
      Status = error type
      Location = location of error in formula

  Error types:
   0: No error occured
   1: Illegal character
   2: Incorrect syntax
   3: Illegal or missing parenthese
   4: Incorrect real format
   5: Illegal function
   6: Result is undefined
   7: Result is too large
   8: Result is complex
   9: Division by zero

---------------------------------------------------------------------------- }


function Evaluate(formula: string; var status,location: integer):double;

{ ---- Declaration ---- }

const  numbers: set of char = ['0'..'9'];                 { Digits }
       RightPar: set of char = [')',']','}'];             { Right parentheses }
       LeftPar: set of char = ['(','[','{'];              { Left parentheses }
       alpha: set of char = ['A'..'Z'];                   { Alpha characters }
       operators: set of char = ['+','-','*','/','^'];    { Operators }
       eofline = ^M;

var ch: char;                                            { Current character }
    resulto: real;                                        { Final value }

{ ---- Internal routines ---- }

{ Check to see if an error has occured }
  function Ok: boolean;
  begin
    ok:= region and (not divzero) and (not complex)
         and (not overflow) and (status = 0);
  end;  { Ok }

{ Get the next character in the string and increment the location pointer. }
  procedure NextCh;
  begin
    repeat
      location:=location+1;                              { Increment pointer }
      if location <= length(formula) then ch:=formula[location]
      else ch:=eofline;
      if not (ch in alpha + numbers + LeftPar + RightPar + operators
              + ['.',' ','!',eofline]) then
        status:=1;                                       { Illegal char. }
    until ch <> ' ';                                     { Skip blanks }
  end  { NextCh };


{ ---- Nested functions ---- }


  function Expression: real;
  label quit;
  var e,e_hold: real;
      opr: char;
      Leading_Sign,Nested_Function: boolean;


    function SimpleExpression: real;
    label quit;
    var s,s_hold: real;
        opr: char;


      function Term: real;
      label quit;
      var  t,t_hold: real;


        function SignedFactor: Real;


          function Factor: Real;
          label quit;
          type StandardFunction =
                 (fpi,fabs,fsqrt,fsqr,fln,flog,fexp,ffact,
                  fsinh,fcosh,ftanh,fsech,fcsch,fcoth,
                  fsin,fcos,ftan,fsec,fcsc,fcot,fasin,facos,fatan);

               StandardFunctionList = array[StandardFunction] of string[4];

           const StandardFunctionNames: StandardFunctionList =
                  ('PI','ABS','SQRT','SQR','LNG','LOG','EXP','FACT',
                   'SINH','COSH','TANH','SECH','CSCH','COTH',
                   'SIN','COS','TAN','SEC','CSC','COT','ASIN','ACOS','ATAN');

           var Found: Boolean;
               l: integer;
               F: Real;
               str: string;
               Sf: StandardFunction;
               start,position: integer;

          begin { Function Factor }

          { Exit if error }
            if not ok then begin f:=0.0; goto quit end;

          { Get a real or integer expression }
            if ch in numbers+['.'] then
            begin
              start:=location;
              if ch in numbers then repeat NextCh until not (ch in numbers);
              if ch = '.' then repeat NextCh until not (ch in numbers);

            { Get the E format of a real expression }
              if ch = 'E' then
              begin
                NextCh;
                if ch = 'X' then location:=location - 1      { Skip EXP(...) }
                else
                  if not (ch in numbers + ['+','-']) then status:=4
                  else repeat NextCh until not (ch in numbers);
              end;

            { Check the real format }
              str:=copy(formula,start,location-start);

              { Remove all spaces in str otherwise val will bomb! }
              while pos(' ',str) <> 0 do delete(str,pos(' ',str),1);

              val(str,f,position);
              if position <> 0 then
              begin
                location:=start+position;
                status:=4                            { Incorrect Real format }
              end;
            end    { end if ch in number }

          { The character is not a digit }
            else
            begin

            { Check for for the beginning of a "sub" expression }
              if ch in LeftPar then
              begin
                NextCh;
                F:=Expression;                       { RECURSION !!! }
                if ok and (ch in RightPar) then      { Check for implicit * }
                begin
                  NextCh;
                  if not (ch in operators+LeftPar+RightPar+['!',eofline]) then
                  begin
                    ch:='*';
                    location:=location-1
                  end;
                end
                else status:=3                         { Illegal parenthese }
              end  { if ch in LeftPar }

            { It should be a function }
              else
              begin
                found:=false;

              { Search for the function among our list }
                for sf:=fpi to fatan do
                  if not found then
                  begin
                    l:=length(StandardFunctionNames[sf]);

                    if copy(formula,location,l)=StandardFunctionNames[sf] then
                    begin
                      location:=location+l-1;
                      NextCh;
                      if sf <> fpi then
                      begin
                        Nested_Function:=true;
                        F:=Factor
                      end;

                    { Assign values according to the function }
                      case sf of
                        fpi:      f:=pi;                  { pi is predefined }
                        fsqr:     if f < 1.0e+19 then f:=sqr(f)
                                  else
                                  begin
                                    f:=0.0;
                                    overflow:=true
                                  end;
                        fabs:     f:=abs(f);
                        fsqrt:    if f < 0.0 then
                                  begin
                                    complex:=true;
                                    f:=0.0
                                  end
                                  else f:=sqrt(f);

                        fsin:     f:=sin(f);
                        fcos:     f:=cos(f);
                        ftan:     f:=tan(f);
                        fasin:    f:=asin(f);
                        facos:    f:=acos(f);
                        fatan:    f:=arctan(f);
                        fsec:     f:=sec(f);
                        fcsc:     f:=csc(f);
                        fcot:     f:=cot(f);

                        fsinh:    f:=sinh(f);
                        fcosh:    f:=cosh(f);
                        ftanh:    f:=tanh(f);
                        fsech:    f:=sech(f);
                        fcsch:    f:=csch(f);
                        fcoth:    f:=coth(f);

                        fexp:     if abs(f) < MaxExpo then f:=exp(f)
                                  else
                                    if f < 0 then f:=0.0
                                    else
                                    begin
                                      overflow:=true;
                                      f:=0.0
                                    end;
                        ffact:    f:=fact(f);
                        fln :     if f < 0.0 then
                                  begin
                                    complex:=true;
                                    f:=0.0
                                  end
                                  else
                                    if f = 0.0 then
                                    begin
                                      overflow:=true;
                                      f:=0.0
                                    end
                                    else f:=ln(f);
                        flog:     if f < 0.0 then
                                  begin
                                    complex:=true;
                                    f:=0.0
                                  end
                                  else
                                    if f = 0.0 then
                                    begin
                                      overflow:=true;
                                      f:=0.0
                                    end
                                    else f:=ln(f)/ln(10);
                      end; { Case }

                      found:=true;
                      Nested_Function:=false;

                    { Check for a trailing factorial symbol }
                      if ch = '!' then
                      begin
                        f:=fact(f);
                        NextCh
                      end

                    end   { If copy = function }
                  end;  { If not found }

              { Check for more errors }
                if (not found) and ok and not (ch in alpha) then
                  status:=2;                                  { Illegal Syntax }
                if (not found) and ok and (ch in alpha) then
                  status:=5;                                { Illegal function }

              end  { Else not ch in LeftPar .. ie. it should be a function }
            end; { else the character is not a digit }

          { Check for a trailing factorial symbol }
            if ok and (not Nested_Function) and (ch = '!') then
            begin
              f:=fact(f);
              NextCh
            end;

          { Assign final value }
     quit:  Factor:=F
          end;  { Factor inside SignedFactor }

        begin  { SignedFactor }
          if ch = '-' then
          begin
            NextCh;
            SignedFactor:= -Factor
          end
          else SignedFactor:=Factor;
        end  { SignedFactor inside Term };


      begin { Term }
        if not ok then begin t:=0.0; goto quit end;          { Exit }

        t:=SignedFactor;
        while ch = '^' do
        begin
          if not ok then begin t:=0.0; goto quit end;        { Exit }

          NextCh;
          t_hold:=SignedFactor;

        { Check for illegal power }
          if ((t < 0.0) and ((t_hold-trunc(t_hold)) <> 0.0)) or (t = 0.0) then
          begin
            t:=0.0;
            complex:=true
          end

        { Power is legal }
          else
          begin
            if t < 0.0 then
            begin
              CheckOverflow(ln(-t),t_hold,'*');
              if not Ok then begin t:=0.0; goto quit end;     { Exit }

              if ln(-t)*t_hold <= MaxExpo then
                case trunc(abs(t_hold)) mod 2 = 0 of
                  true:  t:=exp(ln(-t)*t_hold);
                  false: t:=-exp(ln(-t)*t_hold)
                end
              else
              begin
                t:=0.0;
                overflow:=true
              end
            end  { if t < 0.0 }

            else  { t >= 0.0 }
            begin
              CheckOverflow(ln(t),t_hold,'*');
              if not Ok then begin t:=0.0; goto quit end;     { Exit }

              if ln(t)*t_hold <= MaxExpo then t:=exp(ln(t)*t_hold)
              else
              begin
                t:=0.0;
                overflow:=true
              end
            end  { else t >= 0.0 }
          end  { else not illegal power }
        end;  { while }
  quit: Term:=t;
      end;  { Term inside SimpleExpression }


    begin { SimpleExpression }
      if not ok then begin s:=0.0; goto quit end;                     { Exit }
      s:=term;

    { Check for implicit multiplication and insert missing "*" }
      if ok and (ch in LeftPar + alpha + numbers + ['.']) then
      begin
        ch:='*';
        location:=location-1
      end;

      while ch in ['*','/'] do
      begin
        if not ok then begin s:=0.0; goto quit end;                    { Exit }
        opr:=ch;
        NextCh;

      { Check for implicit multiplication and insert missing "*" }
        if opr in LeftPar + alpha + numbers + ['.'] then
        begin
          opr:='*';
          ch:='(';
          location:=location-1
        end;

        s_hold:=term;
        case opr of
          '*': begin
                 CheckOverflow(s,s_hold,'*');
                 if not overflow then s:=s*s_hold
                 else s:=0.0
               end;
          '/': begin
                 divzero:=s_hold = 0.0;
                 if not divzero then
                 begin
                   CheckOverflow(s,s_hold,'/');
                   if not overflow then s:=s/s_hold
                   else s:=0.0
                 end
                 else s:=0.0
               end
        end;  { Case }

      { Check for implicit multiplication and insert missing "*" }
        if ok and (ch in LeftPar + alpha + numbers + ['.']) then
        begin
          ch:='*';
          location:=location-1
        end
      end;  { while }

    { Assign final value }
quit: SimpleExpression:=s;
    end;  { SimpleExpression inside Expression }


  begin { Expression }
    if not ok then begin e:=0.0; goto quit end;                       { Exit }
    Nested_Function:=false;

    Leading_Sign:= ch = '-';                              { The default is + }
    if ch in ['+','-'] then Nextch;                      { Skip leading sign }
    case Leading_Sign of                              { Set for leading sign }
      true:   e:= -SimpleExpression;
      false:  e:= SimpleExpression
    end;

    while ch in ['+','-'] do
    begin
      if not ok then begin e:=0.0; goto quit end;                     { Exit }
      opr:=ch;
      NextCh;

      e_hold:=SimpleExpression;
      case opr of
        '+': begin
               CheckOverflow(e,e_hold,'+');
               if not overflow then e:=e+e_hold
               else e:=0.0;
             end;
        '-': begin
               CheckOverflow(e,e_hold,'-');
               if not overflow then e:=e-e_hold
               else e:=0.0;
             end;
      end;  { case }
    end;  { while }
  quit: Expression:=e;
  end; { Expression inside Evaluate }

var i:integer;

begin { Evaluate }

{ Initialize }
  for i:=1 to length(formula) do
     formula[i]:=upcase(formula[i]);
  Init_Booleans;
  status:=0;
  location:=0;
  NextCh;

{ Get result }
  resulto:=Expression;

{ Check for final errors }
  if ok then if ch <> eofline then status:=2;             { Incorrect Syntax }

  if not region then status:=6;
  if overflow then status:=7;
  if complex then status:=8;
  if divzero then status:=9;

  if status in [4,6..9] then location:=location-1;
  if status = 0 then location:=0
  else resulto:=0.0;

  Evaluate:=resulto;
end { Evaluate };

end.
