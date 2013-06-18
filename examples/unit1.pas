unit Unit1;

{$mode objfpc}{$H+}


 {$DEFINE fpGUI}
interface

uses
  sak, Unit2, Unit3, Forms, StdCtrls, Grids, Dialogs, SysUtils, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    color_: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    size_: TListBox;
    memo1: TMemo;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Stock: TStringGrid;


    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }

  end;

var
  Form1: TForm1;
  tempi: integer;


implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.Button2Click(Sender: TObject);
begin
  SAKLoadLib;
  button2.Enabled := False;
  button3.Enabled := True;

  /// You may change the default gender and language.
  /// gender : male or female,
  /// language : langage code
  /// Here example for Portugues/Brasil woman

// SAKSetVoice(female,'pt');

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SAKUnloadLib;
  button2.Enabled := True;
  button3.Enabled := False;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  form2.Show;
end;

function randommoney : string;
var
  x : integer;
    begin
    x := random(3) ;
    case x of
     0 : result := ' €' ;
     1 : result := ' £' ;
     2 : result := ' $' ;
     end;
     end;

procedure TForm1.Button5Click(Sender: TObject);
var
  x,y : integer;
begin
    for x := 0 to 4 do
       for y := 1 to 4 do
      form3.Stock.Cells[x, y] := inttostr(random(1000)) + randommoney ;
    form3.Show;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  opendialog1.Execute;

end;

procedure TForm1.Button7Click(Sender: TObject);
begin
 savedialog1.Execute;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Inc(tempi);
  label1.Caption := 'Test enter ' + IntToStr((tempi));
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SAKFreeLib ;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  x,y : integer;
begin
  randomize;
     for x := 1 to 4 do
       for y := 1 to 4 do
      form1.Stock.Cells[x, y] := inttostr(random(1000)) + randommoney ;
    tempi := 0;
end;

end.
