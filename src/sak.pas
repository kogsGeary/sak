unit sak;

{*******************************************************************************
*                         Speech Assistive Kit ( sak )                         *
*                  --------------------------------------                      *
*                                                                              *
*          Assistive Procedures using eSpeak and Portaudio libraries           *
*                                                                              *
*                                                                              *
*                 Fred van Stappen /  fiens@hotmail.com                        *
*                                                                              *
*                                                                              *
********************************************************************************
*  1 th release: 2013-06-15  (multi objects, multi forms)                      *
*                                                                              *
********************************************************************************}
    {
    Copyright (C) 2013  Fred van Stappen

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA  02110-1301  USA
    }

interface

uses
  {$IF DEFINED(LCL)}// for LCL
  Forms,
  Grids,
  Controls,
  StdCtrls,
  ExtCtrls,
  Dialogs,
     {$else}/// for fpGUI
  fpg_base,
  fpg_main,
  fpg_grid,
  fpg_button,
  fpg_CheckBox,
  fpg_RadioButton,
  fpg_ComboBox,
  fpg_ListBox,
  fpg_memo,
  fpg_edit,
  fpg_form,
  fpg_dialogs,
    {$endif}
  Classes, Math, SysUtils, uos_PortAudio, uos_eSpeak;
const
  male = 1;
  female = 2;
type
  TSAK_VoiceConfig = record
    voice: espeak_VOICE;
    Parameter: espeak_PARAMETER;
  end;

type
  TProc = procedure of object;
  TOnEnter = procedure(Sender: TObject) of object;
  TOnClick = procedure(Sender: TObject) of object;
  TOnChange = procedure(Sender: TObject) of object;
  TOnDestroy = procedure(Sender: TObject) of object;
  {$IF DEFINED(LCL)}
  TOnKeyDown = procedure(Sender: TObject; var Key: word; Shift: TShiftState) of object;
  TOnKeyUp = procedure(Sender: TObject; var Key: word; Shift: TShiftState) of object;
  TOnKeyPress = procedure(Sender: TObject; var Key: char) of object;
  TOnMouseDown = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer) of object;
  TOnMouseUp = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer) of object;
  TOnSelectionChange = procedure(Sender: TObject; User: boolean) of object;
  TOnSelectionChangeDialog = procedure(Sender: TObject) of object;

  {$else}//// fpGUI
  TOnKeyChar = procedure(Sender: TObject; Key: TfpgChar; var ifok: boolean) of object;
  TOnKeyPress = procedure(Sender: TObject; var Key: word; var Shift: TShiftState;
    var ifok: boolean) of object;
  TOnMouseDown = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; const Pointm: TPoint) of object;
  TOnMouseUp = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; const Pointm: TPoint) of object;
  TOnFocusChange = procedure(Sender: TObject; col: longint; row: longint) of object;
      {$endif}

type
  TSAK_Assistive = class(TObject)
  private
    TheObject: TObject;
    OriOnKeyPress: TOnKeyPress;
    OriOnClick: TOnClick;
    OriOnEnter: TOnEnter;
    OriOnMouseDown: TOnMouseDown;
    OriOnMouseUp: TOnMouseUp;
    OriOnChange: TOnChange;
    OriOnDestroy: TOnDestroy;
     {$IF DEFINED(LCL)}
    OriOnKeyDown: TOnKeyDown;
    OriOnKeyUp: TOnKeyUp;
    OriOnSelectionChange: TOnSelectionChange;
    OriOnSelectionChangeDialog: TOnSelectionChangeDialog;
      {$else}
    OriOnKeyChar: TOnKeyChar;
    OriOnFocusChange: TOnFocusChange;
      {$endif}
  public
    Description: ansistring;
    Soundfile: ansistring;
  end;

type
  TSAK_Init = class(TObject)
  public
    PA_FileName: ansistring;
    ES_FileName: ansistring;
    ES_DataDirectory: ansistring;
    isloaded: boolean;
    isworking: boolean;
    isrepeat: boolean;
    CompCount: integer;
    CheckObject : TObject;
    CheckKey: word;
    CheckShift: TShiftState ;
    AssistiveData: array of TSAK_Assistive;
     {$IF DEFINED(LCL)}
    CheckKeyChar : Char;
    TimerCount: TTimer;
    TimerRepeat: TTimer;
      {$else}//// fpGUI
    CheckKeyChar : TfpgChar;
    CheckCol, CheckRow : longint ;
    TimerCount: TfpgTimer;
    TimerRepeat: TfpgTimer;
      {$endif}
    procedure SAKEnter(Sender: TObject);
    procedure SAKChange(Sender: TObject);
    procedure SAKClick(Sender: TObject);
    procedure SAKDestroy(Sender: TObject);
    procedure CheckCount(Sender: TObject);
    procedure CheckRepeatEnter(Sender: TObject);
    procedure CheckRepeatChange(Sender: TObject);
    procedure CheckRepeatKeyPress(Sender: TObject);
      {$IF DEFINED(LCL)}
    procedure CheckRepeatDialog(Sender: TObject);
    procedure CheckRepeatSelectionChange(Sender: TObject);
    procedure CheckKeyUp(Sender: TObject);
    procedure SAKKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SAKKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SAKKeyPress(Sender: TObject; var Key: char);
    procedure SAKMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SAKMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SAKSelectionChange(Sender: TObject; User: boolean);
    procedure SAKSelectionChangeDialog(Sender: TObject);
           {$else}//// fpGUI
    procedure CheckRepeatKeyChar(Sender: TObject);
    procedure CheckFocusChange(Sender: TObject);
    procedure SAKKeyChar(Sender: TObject; Key: TfpgChar; var ifok: boolean);
    procedure SAKKeyPress(Sender: TObject; var Key: word; var Shift: TShiftState;
      var ifok: boolean);
    procedure SAKMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; const pointm: Tpoint);
    procedure SAKMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; const pointm: Tpoint);
    procedure SAKFocusChange(Sender: TObject; col: longint; row: longint);
       {$endif}

  private
    function LoadLib: integer;
    function unLoadLib: integer;
    procedure InitObject;
  end;

//// Load with default
function SAKLoadlib: integer;

/// Load with custom
function SAKLoadLib(PortaudioLib: string; eSpeakLib: string;
  eSpeakDataDir: string): integer;

function SAKUnloadLib: integer;

function SAKFreeLib: integer;

////// Change voice language or/and gender
function SAKSetVoice(gender : shortint; language : string) : integer;
//// gender : 1 = male, 2 = female.
//// language : is the language code, for example :
//// 'en' for english, 'fr' for french, 'pt' for Portugues, etc...
//// (check in /espeak-data if your language is there...)

///// Start speaking the text with default voice
function SAKSay(Text: string): integer;

///// Start speaking the text with custom voice
function SAKSay(Text: string; VoiceParam: TSAK_VoiceConfig): integer;

///// Start speaking the character with default voice
function SAKSay(char: word): integer;

//// Start speaking with custom voice
function SAKSay(char: word; VoiceParam: TSAK_VoiceConfig): integer;

/// stop speaking;
function SAKStop: integer;

function WhatName(Sender: TObject): string;

var
  old8087cw: word;
  DefVoice, CustomVoice : TSAK_VoiceConfig;

  InitSpeech: TSAK_Init;
  mouseclicked: boolean;

implementation

/////////////////////////// Capture Assistive Procedures


function WhatName(Sender: TObject): string;
begin
      {$IF DEFINED(LCL)}
  if (Sender is TButton) then
    Result := TButton(Sender).Caption
  else
   if (Sender is TColorButton) then
    Result := TColorButton(Sender).Caption
  else
  if (Sender is TForm) then
    Result := TForm(Sender).Caption
  else
  if (Sender is TEdit) then
    Result := TEdit(Sender).Name
  else
  if (Sender is TMemo) then
    Result := TMemo(Sender).Name
  else
  if (Sender is TCheckBox) then
    Result := TCheckBox(Sender).Caption
  else
  if (Sender is TRadiobutton) then
    Result := TRadiobutton(Sender).Caption
  else
  if (Sender is TStringgrid) then
    Result := TStringgrid(Sender).Name
  else
  if (Sender is TListBox) then
    Result := TListBox(Sender).Name
  else
  if (Sender is TComboBox) then
    Result := TComboBox(Sender).Name
  else
  if (Sender is TOpenDialog) then
    Result := TSaveDialog(Sender).title
  else
  if (Sender is TOpenDialog) then
    Result := TSaveDialog(Sender).title;

      {$else}//// fpGUI
  if (Sender is TfpgButton) then
    Result := Tfpgbutton(Sender).Text
  else
  if (Sender is TfpgForm) then
    Result := TfpgForm(Sender).WindowTitle
  else
  if (Sender is TfpgEdit) then
    Result := TfpgEdit(Sender).Name
  else
  if (Sender is TfpgMemo) then
    Result := TfpgMemo(Sender).Name
  else
  if (Sender is TfpgStringgrid) then
    Result := TfpgStringgrid(Sender).Name
  else
  if (Sender is TfpgRadiobutton) then
    Result := TfpgRadiobutton(Sender).Text
  else
  if (Sender is TfpgCheckBox) then
    Result := TfpgCheckBox(Sender).Text
  else
  if (Sender is TfpgListBox) then
    Result := TfpgListBox(Sender).text
  else
  if (Sender is TfpgFileDialog) then
    Result := TfpgFileDialog(Sender).WindowTitle
  else
  if (Sender is TfpgComboBox) then
    Result := TfpgComboBox(Sender).Name ;
              {$endif}
end;

procedure TSAK_Init.SAKDestroy(Sender: TObject);
var
  i : integer;
begin
isworking := false ;
timercount.Enabled := false;
 unLoadLib;
   for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (Sender = InitSpeech.AssistiveData[i].TheObject) and
        (InitSpeech.AssistiveData[i].OriOnDestroy <> nil) then
      begin
        InitSpeech.AssistiveData[i].OriOnDestroy(Sender);
       exit;
       end;
  end;
// CompCount := fpgapplication.ComponentCount + 1 ;
  isworking := true ;
  timercount.Enabled := True;
   end;

procedure TSAK_Init.SAKClick(Sender: TObject);
var
  texttmp, nameobj: string;
  i: integer;
begin
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (Sender = InitSpeech.AssistiveData[i].TheObject) then
    begin
      mouseclicked := True;

      nameobj := whatname(Sender);

      texttmp := InitSpeech.AssistiveData[i].Description + ' ' +
        nameobj + ' executed';

      espeak_Key(pointer(texttmp));

      if InitSpeech.AssistiveData[i].OriOnClick <> nil then
       InitSpeech.AssistiveData[i].OriOnClick(Sender);

      exit;
    end;
  end;
end;

procedure TSAK_Init.SAKChange(Sender: TObject);
begin
 TimerRepeat.OnTimer := @CheckRepeatChange;
 TimerRepeat.Enabled:=false;
 TimerRepeat.Interval:=500;
 TimerRepeat.Enabled:=true;
 CheckObject := sender;
end;

  {$IF DEFINED(LCL)}
procedure TSAK_Init.SAKSelectionChange(Sender: TObject; User: boolean);
 begin
 TimerRepeat.OnTimer := @CheckRepeatSelectionChange;
 TimerRepeat.Enabled:=false;
 TimerRepeat.Interval:=500;
 CheckObject := sender;
 TimerRepeat.Enabled:=true;
end;



procedure TSAK_Init.CheckRepeatSelectionChange(Sender: TObject);
var
  i: integer;
  texttmp: string;
  user : boolean ;
begin
user := false;
TimerRepeat.Enabled:=false;
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (CheckObject is TListBox) then
      with CheckObject as TListBox do
        texttmp := GetSelectedText + ' selected';
    espeak_Key(pointer(texttmp));

    if InitSpeech.AssistiveData[i].OriOnSelectionChange <> nil then
         InitSpeech.AssistiveData[i].OriOnSelectionChange(CheckObject, user);
  exit;
  end;
end;

procedure TSAK_Init.SAKSelectionChangeDialog(Sender: TObject);
begin
 TimerRepeat.OnTimer := @CheckRepeatDialog;
 TimerRepeat.Interval:=500;
 TimerRepeat.Enabled:=false;
 TimerRepeat.Enabled:=true;
 CheckObject := sender;
end;

procedure TSAK_Init.CheckRepeatDialog(Sender: TObject);
var
  i, x : integer;
  texttmp: string;
begin
  TimerRepeat.Enabled:=false;
  x := 0;
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
       if (CheckObject is TSaveDialog) then
    begin
      x := 1;
      with CheckObject as TSaveDialog do
        texttmp := FileName + ' selected';
    end;

    if (CheckObject is TOpenDialog) then
    begin
      x := 1;
      with CheckObject as TOpenDialog do
        texttmp := FileName + ' selected';
     end;
  if x = 1 then
  begin
     espeak_Key(pointer(texttmp));
     if InitSpeech.AssistiveData[i].OriOnSelectionChangeDialog <> nil then
     InitSpeech.AssistiveData[i].OriOnSelectionChangeDialog(CheckObject);
      exit;
  end;
    end;
end;

procedure TSAK_Init.CheckRepeatChange(Sender: TObject);
var
  i: integer;
  texttmp: string;
begin
  TimerRepeat.Enabled:=false;
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (CheckObject is TComboBox) then
      with CheckObject as TComboBox do
        texttmp := Text + ' selected';

    if (CheckObject is TCheckBox) then
      with CheckObject as TCheckBox do

        if Checked = False then
          texttmp := 'Change  ' + Caption + ', in false'
        else
          texttmp :=
            'Change  ' + Caption + ', in true';

    if (CheckObject is TRadioButton) then
      with CheckObject as TRadioButton do

        if Checked = False then
          texttmp := 'Change  ' + Caption + ', in false'
        else
          texttmp :=
            'Change  ' + Caption + ', in true';

    espeak_Key(pointer(texttmp));

    if InitSpeech.AssistiveData[i].OriOnChange <> nil then
     InitSpeech.AssistiveData[i].OriOnChange(CheckObject);
    exit;
  end;
end;

procedure TSAK_Init.SAKMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

begin
  /// usefull ?
end;

procedure TSAK_Init.SAKMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  /// usefull ?
end;

{$else}//// fpGUI

procedure TSAK_Init.CheckRepeatChange(Sender: TObject);
var
  i: integer;
  texttmp: string;
begin
   TimerRepeat.Enabled:=false;
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (CheckObject is TfpgComboBox) then
      with CheckObject as TfpgComboBox do
        texttmp := Text + ' selected';

    if (CheckObject is TfpgListBox) then
      with CheckObject as TfpgListBox do
        texttmp := Text + ' selected';

    if (CheckObject is TfpgCheckBox) then
      with CheckObject as TfpgCheckBox do

        if Checked = False then
          texttmp := 'Change  ' + Text + ', in false'
        else
          texttmp :=
            'Change  ' + Text + ', in true';

    if (CheckObject is TfpgRadioButton) then
      with CheckObject as TfpgRadioButton do

        if Checked = False then
          texttmp := 'Change  ' + Text + ', in false'
        else
          texttmp :=
            'Change  ' + Text + ', in true';


    espeak_Key(pointer(texttmp));

    if InitSpeech.AssistiveData[i].OriOnChange <> nil then
    InitSpeech.AssistiveData[i].OriOnChange(CheckObject);

    exit;
  end;
end;

procedure TSAK_Init.SAKFocusChange(Sender: TObject; col: longint; row: longint);
begin
 TimerRepeat.Enabled:=false;
 TimerRepeat.Interval:=500;
 TimerRepeat.OnTimer := @CheckFocusChange;
 TimerRepeat.Enabled:=true;
 CheckObject := sender;
 CheckCol := col;
 CheckRow := row ;
end ;

procedure TSAK_Init.CheckFocusChange(Sender: TObject);
var
  i: integer;
  texttmp: string;
begin
 TimerRepeat.Enabled:=false;
  for i := 0 to high(InitSpeech.AssistiveData) do
    if (CheckObject = InitSpeech.AssistiveData[i].TheObject) and
      (CheckObject is tfpgstringgrid) then
    begin
      with CheckObject as tfpgstringgrid do
      begin
        texttmp := ColumnTitle[focuscol] + ', row ' + IntToStr(focusrow + 1) +
          '. ' + Cells[focuscol, focusrow];

        espeak_Key(pointer(texttmp));
      end;

      if InitSpeech.AssistiveData[i].OriOnFocusChange <> nil then
     InitSpeech.AssistiveData[i].OriOnFocusChange(CheckObject, CheckCol, CheckRow);

      exit;
    end;
end;


procedure TSAK_Init.SAKMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; const pointm: Tpoint);
begin
end;

procedure TSAK_Init.SAKMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; const pointm: Tpoint);
begin
end;

  {$endif}

procedure TSAK_Init.SAKEnter(Sender: TObject);
begin
 TimerRepeat.OnTimer := @CheckRepeatEnter;
 TimerRepeat.Interval:=600;
 TimerRepeat.Enabled:=false;
 TimerRepeat.Enabled:=true;
 CheckObject := sender;
end;

procedure TSAK_Init.CheckRepeatEnter(Sender: TObject);
var
  texttmp, nameobj: string;
  i: integer;
begin
  if mouseclicked = False then
  begin
    TimerRepeat.Enabled:=false;
    for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
    begin
      if (CheckObject = InitSpeech.AssistiveData[i].TheObject) then
      begin
        nameobj := whatname(CheckObject);
        texttmp := InitSpeech.AssistiveData[i].Description + ' ' +
          nameobj + ' selected';
        espeak_Key(pointer(texttmp));
        if InitSpeech.AssistiveData[i].OriOnEnter <> nil then
          InitSpeech.AssistiveData[i].OriOnEnter(CheckObject);

        exit;
      end;
    end;
  end;
  mouseclicked := False;

end;


{$IF DEFINED(LCL)}
procedure TSAK_Init.SAKKeyPress(Sender: TObject; var Key: char);
begin
  TimerRepeat.Enabled:=false;
  TimerRepeat.OnTimer := @CheckRepeatKeyPress;
 TimerRepeat.Interval:=300;
 CheckObject := sender;
 CheckKeyChar := Key;
 TimerRepeat.Enabled:=true;
end;


procedure TSAK_Init.CheckRepeatKeyPress(Sender: TObject);
var
  tempstr: string;
  i: integer;
begin
   TimerRepeat.Enabled:=false;
  tempstr := CheckKeyChar;
  tempstr := trim(tempstr);
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (CheckObject = InitSpeech.AssistiveData[i].TheObject) then
    begin

      tempstr := CheckKeyChar;
      tempstr := trim(tempstr);
      try
        if tempstr <> '' then
          espeak_Key(pointer(tempstr));
      except
      end;
      if InitSpeech.AssistiveData[i].OriOnKeyPress <> nil then
        InitSpeech.AssistiveData[i].OriOnKeyPress(CheckObject,CheckKeyChar);

      exit;
    end;
  end;
end;

{$else}////// fpGUI

procedure TSAK_Init.SAKKeyPress(Sender: TObject; var Key: word;
  var Shift: TShiftState; var ifok: boolean);
begin
 TimerRepeat.OnTimer := @CheckRepeatKeyPress;
 TimerRepeat.Interval:=300;
 TimerRepeat.Enabled:=false;
 TimerRepeat.Enabled:=true;
 CheckObject := sender;
 CheckKey := key ;
 CheckShift := Shift;
end;

procedure TSAK_Init.CheckRepeatKeyPress(Sender: TObject);
var
  i: integer;
  texttmp: string;
  ifok : boolean;
begin
   ifok := true;
 TimerRepeat.Enabled:=false;
  for i := 0 to high(InitSpeech.AssistiveData) do
  begin
    if (CheckObject = InitSpeech.AssistiveData[i].TheObject) then
    begin

      if (CheckKey = 27) then
        espeak_Cancel
      else
      begin
        case CheckKey of

          keyPEnter: espeak_Key('enter');
          8: espeak_Key('back space');
          32: if (CheckObject is TfpgCheckBox) or (CheckObject is TfpgRadioButton) or
              (CheckObject is TfpgComboBox) or (CheckObject is TfpgListBox) then
            else
              espeak_Key('space');
          57394: espeak_Key('up');
          57395: espeak_Key('down');
          57396: espeak_Key('left');
          57397: espeak_Key('right');
          57601: espeak_Key('f 1');
          57602: espeak_Key('f 2');
          57603: espeak_Key('f 3');
          57604: espeak_Key('f 4');
          57605: espeak_Key('f 5');
          57606: espeak_Key('f 6');
          57607: espeak_Key('f 7');
          57608: espeak_Key('f 8');
          57609: espeak_Key('f 9');
          57610: espeak_Key('f 10');
          57611: espeak_Key('f 11');
          9: espeak_Key('tab');
          58112: espeak_Key('shift left');
          58176: espeak_Key('shift right');
          58113: espeak_Key('control right');
          58177: espeak_Key('control left');
          18: espeak_Key('alt');
          58247: espeak_Key('caps lock');
          65535: espeak_Key('alt gr');
          33: espeak_Key('page up');
          34: espeak_Key('page down');
          46: espeak_Key('delete');
          57378: espeak_Key('insert');
          27: espeak_Key('escape');
          35: espeak_Key('end');
          57612: if (CheckObject is TfpgMemo) then
              with CheckObject as TfpgMemo do
                espeak_Key(pointer(Text))
            else
            if (CheckObject is Tfpgedit) then
              with CheckObject as Tfpgedit do
                espeak_Key(pointer(Text))
            else
              espeak_Key('f 12');
        end;

        if InitSpeech.AssistiveData[i].OriOnKeyPress <> nil then
          InitSpeech.AssistiveData[i].OriOnKeyPress(CheckObject, CheckKey, CheckShift, ifok);
                exit;
      end;
    end;
  end;

end;

{$endif}

  {$IF DEFINED(LCL)}

procedure TSAK_Init.SAKKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
 TimerRepeat.OnTimer := @CheckKeyUp;
 TimerRepeat.Interval:=600;
 TimerRepeat.Enabled:=false;
 CheckObject := sender;
 CheckKey := Key;
 CheckShift := Shift;
 TimerRepeat.Enabled:=true;
 end ;

procedure TSAK_Init.CheckKeyUp(Sender: TObject);
var
  i: integer;
  texttmp: string;
begin
  TimerRepeat.Enabled:=false;
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin

    if ((CheckKey = 38) or (CheckKey = 37) or (CheckKey = 39) or (CheckKey = 13) or (CheckKey = 40)) and
      (CheckObject is tstringgrid) then
      with CheckObject as tstringgrid do
      begin
        if (fixedrows = 1) and (fixedcols = 1) then
          texttmp := Cells[col, 0] + ', ' + Cells[0, row] + '. ' + Cells[col, row]
        else
        if (fixedrows = 1) and (fixedcols = 0) then
          texttmp := Cells[col, 0] + ', row ' + IntToStr(row) + '. ' + Cells[col, row]
        else
        if (fixedrows = 0) and (fixedcols = 1) then
          texttmp := 'column  ' + IntToStr(col) + ' , ' + Cells[0, row] +
            '. ' + Cells[col, row]
        else
          texttmp := 'column  ' + IntToStr(col) + ' , row  ' +
            IntToStr(row) + '. ' + Cells[col, row];

        espeak_Key(pointer(texttmp));
      end;

    if InitSpeech.AssistiveData[i].OriOnKeyUp <> nil then
      InitSpeech.AssistiveData[i].OriOnKeyUp(CheckObject, CheckKey, CheckShift);

    exit;
  end;
end;

procedure TSAK_Init.SAKKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i: integer;
  texttmp: string;
begin

  for i := 0 to high(InitSpeech.AssistiveData) do
  begin
    if (Sender = InitSpeech.AssistiveData[i].TheObject) then
    begin
      if (key = 27) then
        espeak_Cancel
      else
      begin
        case key of

          13: if (Sender is TButton) then
            else
              espeak_Key('enter');
          8: espeak_Key('back space');
          32: if (Sender is TCheckBox) or (Sender is TButton) then
            else
              espeak_Key('space');
          38: espeak_Key('up');
          40: espeak_Key('down');
          37: espeak_Key('left');
          39: espeak_Key('right');
          112: espeak_Key('f 1');
          113: espeak_Key('f 2');
          114: espeak_Key('f 3');
          115: espeak_Key('f 4');
          116: espeak_Key('f 5');
          117: espeak_Key('f 6');
          118: espeak_Key('f 7');
          119: espeak_Key('f 8');
          120: espeak_Key('f 9');
          121: espeak_Key('f 10');
          122: espeak_Key('f 11');
          9: espeak_Key('tab');
          16: espeak_Key('shift');
          17: espeak_Key('control');
          18: espeak_Key('alt');
          20: espeak_Key('caps lock');
          236: espeak_Key('alt gr');
          33: espeak_Key('page up');
          34: espeak_Key('page down');
          46: espeak_Key('delete');
          45: espeak_Key('insert');
          27: espeak_Key('escape');
          35: espeak_Key('end');
          123: if (Sender is tmemo) then
              with Sender as tmemo do
                espeak_Key(pointer(Text))
            else
            if (Sender is tedit) then
              with Sender as tedit do
                espeak_Key(pointer(Text))
            else
              espeak_Key('f 12');
        end;

        if InitSpeech.AssistiveData[i].OriOnKeyDown <> nil then
          InitSpeech.AssistiveData[i].OriOnKeyDown(Sender, Key, Shift);
        exit;
      end;
    end;
  end;
end;

   {$else}/// for fpGUI

procedure TSAK_Init.SAKKeyChar(Sender: TObject; Key: TfpgChar; var ifok: boolean);
begin
 TimerRepeat.OnTimer := @CheckRepeatKeyChar;
 TimerRepeat.Enabled:=false;
 TimerRepeat.Interval:=300;
 TimerRepeat.Enabled:=true;
 CheckObject := sender;
 CheckKeyChar := key ;
end;

procedure TSAK_Init.CheckRepeatKeyChar(Sender: TObject);
var
  tempstr: string;
  i: integer;
  ifok : boolean;
begin
 ifok := true;
  TimerRepeat.Enabled:=false;
  tempstr := CheckKeyChar;
  tempstr := trim(tempstr);
  for i := 0 to (Length(InitSpeech.AssistiveData) - 1) do
  begin
    if (CheckObject = InitSpeech.AssistiveData[i].TheObject) then
    begin

      tempstr := CheckKeyChar;
      tempstr := trim(tempstr);
      try
        if tempstr <> '' then
          espeak_Key(pointer(tempstr));
      except
      end;
      if InitSpeech.AssistiveData[i].OriOnKeyChar <> nil then
      InitSpeech.AssistiveData[i].OriOnKeyChar(CheckObject, CheckKeyChar, ifok);

      exit;
    end;
  end;
end;

        {$endif}

////////////////////// Loading Procedure

function SAKLoadLib(PortaudioLib: string; eSpeakLib: string;
  eSpeakDataDir: string): integer;
begin
  Result := -1;

  if assigned(InitSpeech) then

    initspeech.isloaded := True
  else
  begin
    InitSpeech := TSAK_Init.Create;
    initspeech.isWorking := True;
    initspeech.isloaded := False;
    if directoryexists(eSpeakDataDir) then
    begin
      Result := 0;
      initspeech.ES_DataDirectory := eSpeakDataDir;
    end;
    if (Result = 0) and (fileexists(PortaudioLib)) then
    begin
      Result := 0;
      initspeech.PA_FileName := PortaudioLib;
    end;

    if (Result = 0) and (fileexists(eSpeakLib)) then
    begin
      Result := 0;
      initspeech.ES_FileName := eSpeakLib;
    end;
  end;
  if (Result = 0) or (initspeech.isloaded = True) then
  begin
    initspeech.isworking := True;
    Result := InitSpeech.loadlib;
  end;
end;

function SAKLoadLib: integer;
var
  ordir: string;
begin
  Result := -1;
  if assigned(InitSpeech) then
  begin

    initspeech.isloaded := True;
  end
  else
  begin
    InitSpeech := TSAK_Init.Create;
    initspeech.isloaded := False;
    ordir := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
   {$ifdef windows}
    InitSpeech.ES_DataDirectory := ordir + '\sakit';
     {$else}
    InitSpeech.ES_DataDirectory := ordir + '/sakit';
       {$endif}

   {$ifdef windows}
    if fileexists(ordir + 'Portaudio_x86.dll') then
    begin
      Result := 0;
      initspeech.PA_FileName := ordir + 'Portaudio_x86.dll';
    end
    else
    if fileexists(ordir + '\sakit\lib\Portaudio_x86.dll') then
    begin
      initspeech.PA_FileName := ordir + '\sakit\lib\Portaudio_x86.dll';
      Result := 0;
    end;

    if Result = 0 then
    begin
      Result := -1;
      if fileexists(ordir + 'eSpeak_x86.dll') then
      begin
        Result := 0;
        initspeech.ES_FileName := ordir + 'espeak_x86.dll';
      end
      else
      if fileexists(ordir + '\sakit\lib\eSpeak_x86.dll') then
      begin
        initspeech.ES_FileName := ordir + '\sakit\lib\eSpeak_x86.dll';
        Result := 0;
      end;
    end;
         {$else}
         {$IF DEFINED(Linux) and  defined(cpu64)}
    if fileexists(ordir + 'LibPortaudio_x64.so') then
    begin
      Result := 0;
      initspeech.PA_FileName := ordir + 'LibPortaudio_x64.so';
    end
    else
    if fileexists(ordir + '/sakit/lib/LibPortaudio_x64.so') then
    begin
      initspeech.PA_FileName := ordir + '/sakit/lib/LibPortaudio_x64.so';
      Result := 0;
    end;

    if Result = 0 then
    begin
      Result := -1;
      if fileexists(ordir + 'libespeak_x64.so') then
      begin
        Result := 0;
        initspeech.ES_FileName := ordir + 'libespeak_x64.so';
      end
      else
      if fileexists(ordir + '/sakit/lib/libespeak_x64.so') then
      begin
        initspeech.ES_FileName := ordir + '/sakit/lib/libespeak_x64.so';
        Result := 0;
      end;
    end;
      {$else}
      {$IF DEFINED(Linux) and defined(cpu86) }
    if fileexists(ordir + 'LibPortaudio_x86.so') then
    begin
      Result := 0;
      initspeech.PA_FileName := ordir + 'LibPortaudio_x86.so';
    end
    else
    if fileexists(ordir + '/sakit/lib/LibPortaudio_x86.so') then
    begin
      initspeech.PA_FileName := ordir + '/sakit/lib/LibPortaudio_x86.so';
      Result := 0;
    end;
    if Result = 0 then
    begin
      Result := -1;
      if fileexists(ordir + 'libespeak_x86.so') then
      begin
        Result := 0;
        initspeech.ES_FileName := ordir + 'libespeak_x86.so';
      end
      else
      if fileexists(ordir + '/sakit/lib/libespeak_x86.so') then
      begin
        initspeech.ES_FileName := ordir + '/sakit/lib/libespeak_x86.so';
        Result := 0;
      end;
    end;
                {$endif}
                {$endif}
                {$endif}
  end;

  if (Result = 0) or (initspeech.isloaded = True) then
  begin
    initspeech.isworking := True;
    Result := InitSpeech.loadlib;
  end;

end;

procedure TSAK_Init.InitObject;
var
  i, f: integer;
begin
  mouseclicked := False;
  SetLength(InitSpeech.AssistiveData, 0);

         {$IF DEFINED(LCL)}
     for f := 0 to application.ComponentCount - 1 do  ///
  begin
    SetLength(InitSpeech.AssistiveData, Length(InitSpeech.AssistiveData) + 1);
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1] :=
      TSAK_Assistive.Create();
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
      'Form';
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
      TForm(application.Components[f]);
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnKeyPress :=
      TForm(application.Components[f]).OnKeyPress;
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnKeyDown :=
      TForm(application.Components[f]).OnKeyDown;
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
      TForm(application.Components[f]).OnEnter;
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnMouseDown :=
      TForm(application.Components[f]).OnMouseDown;
      InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnDestroy :=
   TForm(application.Components[f]).OnDestroy;

    TForm(application.Components[f]).OnKeyPress := @InitSpeech.SAKKeyPress;
    TForm(application.Components[f]).OnKeyDown := @InitSpeech.SAKKeyDown;
    TForm(application.Components[f]).OnEnter := @InitSpeech.SAKEnter;
    TForm(application.Components[f]).OnMouseDown := @InitSpeech.SAKMouseDown;
    TForm(application.Components[f]).OnDestroy := @InitSpeech.SAKDestroy;

    with (application.Components[f]) as TForm do

      for i := 0 to ComponentCount - 1 do
      begin
        if (Components[i] is TCheckBox) or (Components[i] is TButton) or
        (Components[i] is TColorButton) or
          (Components[i] is TMemo) or (Components[i] is TRadioButton) or
          (Components[i] is TEdit) or (Components[i] is TStringGrid) or
           (Components[i] is TSaveDialog) or (Components[i] is TOpenDialog) or
          (Components[i] is TListBox) or (Components[i] is TComboBox)
         then
        begin
          SetLength(InitSpeech.AssistiveData, Length(InitSpeech.AssistiveData) + 1);

          InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1] :=
            TSAK_Assistive.Create();
          if (Components[i] is TButton) then
          begin

            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Button';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TButton(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TButton(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyDown :=
              TButton(Components[i]).OnKeyDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnClick :=
              TButton(Components[i]).OnClick;

            TButton(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TButton(Components[i]).OnKeyDown := @InitSpeech.SAKKeyDown;
            TButton(Components[i]).OnClick := @InitSpeech.SAKClick;
          end
          else
            if (Components[i] is TColorButton) then
          begin

            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Button';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TColorButton(Components[i]);
              InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnClick :=
              TColorButton(Components[i]).OnClick;
              TColorButton(Components[i]).OnClick := @InitSpeech.SAKClick;
          end
          else
           if (Components[i] is TSaveDialog) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].Description :=
              'Save Dialog';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TSaveDialog(Components[i]);
             InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnSelectionChangeDialog :=
              TSaveDialog(Components[i]).OnSelectionChange;
              TSaveDialog(Components[i]).OnSelectionChange := @InitSpeech.SAKSelectionChangeDialog;
          end
          else
          if (Components[i] is TOpenDialog) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].Description :=
              'Open Dialog';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TOpenDialog(Components[i]);
             InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnSelectionChangeDialog :=
              TOpenDialog(Components[i]).OnSelectionChange;
              TOpenDialog(Components[i]).OnSelectionChange := @InitSpeech.SAKSelectionChangeDialog;
          end
          else
          if (Components[i] is TListBox) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].Description :=
              'List Box';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TListBox(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TListBox(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TListBox(Components[i]).OnMouseDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnSelectionChange :=
              TListBox(Components[i]).OnSelectionChange;

            TListBox(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            ;
            TListBox(Components[i]).OnSelectionChange := @InitSpeech.SAKSelectionChange;
            TListBox(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TRadioButton) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].Description :=
              'Radio Button';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TRadioButton(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TRadioButton(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TRadioButton(Components[i]).OnMouseDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnChange :=
              TRadioButton(Components[i]).OnChange;

            TRadioButton(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TRadioButton(Components[i]).OnChange := @InitSpeech.SAKChange;
            TRadioButton(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TComboBox) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].Description :=
              'Combo Box';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TComboBox(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TComboBox(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TComboBox(Components[i]).OnMouseDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnChange :=
              TComboBox(Components[i]).OnChange;

            TComboBox(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TComboBox(Components[i]).OnChange := @InitSpeech.SAKChange;
            TComboBox(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TCheckBox) then
          begin

            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Check Box';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TCheckBox(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TCheckBox(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TCheckBox(Components[i]).OnMouseDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnChange :=
              TCheckBox(Components[i]).OnChange;
            TCheckBox(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TCheckBox(Components[i]).OnChange := @InitSpeech.SAKChange;
            TCheckBox(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TStringGrid) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'String Grid';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].TheObject :=
              TStringGrid(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnEnter :=
              TStringGrid(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyPress :=
              TStringGrid(Components[i]).OnKeyPress;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyDown :=
              TStringGrid(Components[i]).OnKeyDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnKeyUp :=
              TStringGrid(Components[i]).OnKeyUp;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TStringGrid(Components[i]).OnMouseDown;

            TStringGrid(Components[i]).OnKeyPress := @InitSpeech.SAKKeyPress;
            TStringGrid(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TStringGrid(Components[i]).OnKeyDown := @InitSpeech.SAKKeyDown;
            TStringGrid(Components[i]).OnKeyUp := @InitSpeech.SAKKeyUp;
            TStringGrid(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TMemo) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Memo';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TMemo(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TMemo(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyPress :=
              TMemo(Components[i]).OnKeyPress;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyDown :=
              TMemo(Components[i]).OnKeyDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TMemo(Components[i]).OnMouseDown;
            TMemo(Components[i]).OnKeyPress := @InitSpeech.SAKKeyPress;
            TMemo(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TMemo(Components[i]).OnKeyDown := @InitSpeech.SAKKeyDown;
            TMemo(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TEdit) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Edit';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].TheObject :=
              TEdit(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnEnter :=
              TEdit(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyPress :=
              TEdit(Components[i]).OnKeyPress;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyDown :=
              TEdit(Components[i]).OnKeyDown;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TEdit(Components[i]).OnMouseDown;

            TEdit(Components[i]).OnKeyPress := @InitSpeech.SAKKeyPress;
            TEdit(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TEdit(Components[i]).OnKeyDown := @InitSpeech.SAKKeyDown;
            TEdit(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end;

        end;
      end;
  end;

    {$else}// fpGUI

     SetLength(InitSpeech.AssistiveData, Length(InitSpeech.AssistiveData) + 1);
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1] :=
      TSAK_Assistive.Create();

    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
      'Application';

    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
      Tfpgapplication(fpgapplication);

   InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnKeyPress :=
    Tfpgapplication(fpgapplication).OnKeyPress;

  Tfpgapplication(fpgapplication).OnKeyPress := @InitSpeech.SAKKeyPress;


  for f := 0 to fpgapplication.formCount - 1 do  /// fpgui
  begin
    SetLength(InitSpeech.AssistiveData, Length(InitSpeech.AssistiveData) + 1);
    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1] :=
      TSAK_Assistive.Create();

    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
      'Form';

    InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
      TfpgForm(fpgapplication.Forms[f]);

   InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnKeyPress :=
   TfpgForm(fpgapplication.Forms[f]).OnKeyPress;

   InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnDestroy :=
   TfpgForm(fpgapplication.Forms[f]).OnDestroy;

  InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnMouseDown :=
  TfpgForm(fpgapplication.Forms[f]).OnMouseDown;

  TfpgForm(fpgapplication.Forms[f]).OnMouseDown := @InitSpeech.SAKMouseDown;
  TfpgForm(fpgapplication.Forms[f]).OnKeyPress := @InitSpeech.SAKKeyPress;
  TfpgForm(fpgapplication.Forms[f]).OnDestroy := @InitSpeech.SAKDestroy;

    with (fpgapplication.Forms[f]) as TfpgForm do

      for i := 0 to ComponentCount - 1 do
      begin
        if (Components[i] is TfpgButton) or (Components[i] is TfpgMemo) or
          (Components[i] is TfpgEdit) or (Components[i] is TfpgStringGrid) or
          (Components[i] is TfpgCheckBox) or (Components[i] is TfpgRadiobutton) or
          (Components[i] is TfpgListBox) or (Components[i] is TfpgComboBox)
          // or (Components[i] is TfpgFileDialog) or (Components[i] is TfpgSaveDialog)
        then
        begin
          SetLength(InitSpeech.AssistiveData, Length(InitSpeech.AssistiveData) + 1);

          InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1] :=
            TSAK_Assistive.Create();
          if (Components[i] is TfpgButton) then
          begin

            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Button';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TfpgButton(Components[i]);
       InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnClick :=
       TfpgButton(Components[i]).OnClick;
       TfpgButton(Components[i]).OnClick := @InitSpeech.SAKClick;
          end
          else
          if (Components[i] is TfpgStringGrid) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Grid';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].TheObject :=
              TfpgStringGrid(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnFocusChange :=
              TfpgStringGrid(Components[i]).OnFocusChange;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnMouseDown :=
              TfpgStringGrid(Components[i]).OnMouseDown;
            TfpgStringGrid(Components[i]).OnFocusChange := @InitSpeech.SAKFocusChange;
            TfpgStringGrid(Components[i]).OnMouseDown := @InitSpeech.SAKMouseDown;
          end
          else
          if (Components[i] is TfpgRadiobutton) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Radio Button';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].TheObject :=
              TfpgRadiobutton(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnChange :=
              TfpgRadiobutton(Components[i]).OnChange;

            TfpgRadiobutton(Components[i]).OnChange := @InitSpeech.SAKChange;
          end
          else

          if (Components[i] is TfpgCheckBox) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Check Box';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TfpgCheckBox(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnChange :=
              TfpgCheckBox(Components[i]).OnChange;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TfpgCheckBox(Components[i]).OnEnter;

            TfpgCheckBox(Components[i]).OnChange := @InitSpeech.SAKChange;
            TfpgCheckBox(Components[i]).OnEnter := @InitSpeech.SAKEnter;
          end
          else
          if (Components[i] is TfpgListBox) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'List Box';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TfpgListBox(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnChange :=
              TfpgListBox(Components[i]).OnChange;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TfpgListBox(Components[i]).OnEnter;

            TfpgListBox(Components[i]).OnChange := @InitSpeech.SAKChange;
            TfpgListBox(Components[i]).OnEnter := @InitSpeech.SAKEnter;
          end
          else
          if (Components[i] is TfpgComboBox) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Combo Box';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TfpgComboBox(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnChange :=
              TfpgComboBox(Components[i]).OnChange;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TfpgComboBox(Components[i]).OnEnter;
            TfpgComboBox(Components[i]).OnChange := @InitSpeech.SAKChange;
            TfpgComboBox(Components[i]).OnEnter := @InitSpeech.SAKEnter;
          end
          else
          if (Components[i] is TfpgMemo) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Memo';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TfpgMemo(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TfpgMemo(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyChar :=
              TfpgMemo(Components[i]).OnKeyChar;

            TfpgMemo(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TfpgMemo(Components[i]).OnKeyChar := @InitSpeech.SAKKeyChar;
          end
          else
          if (Components[i] is TfpgEdit) then
          begin
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].Description :=
              'Edit';
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].TheObject :=
              TfpgEdit(Components[i]);
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) - 1].OriOnEnter :=
              TfpgEdit(Components[i]).OnEnter;
            InitSpeech.AssistiveData[Length(InitSpeech.AssistiveData) -
              1].OriOnKeyChar :=
              TfpgEdit(Components[i]).OnKeyChar;
            TfpgEdit(Components[i]).OnEnter := @InitSpeech.SAKEnter;
            TfpgEdit(Components[i]).OnKeyChar := @InitSpeech.SAKKeyChar;
          end;
        end;
      end;
  end;
      {$endif}
end;

///////////////// loading sak
  {$IF DEFINED(LCL)}
procedure TSAK_Init.CheckCount(Sender: TObject);
begin
  timercount.Enabled := False;
  if isWorking = True then
  begin
    if application.ComponentCount <> CompCount then
    begin
      UnLoadLib;
      InitObject;
      CompCount := application.ComponentCount;
    end;
    timercount.Enabled := True;
  end;
end;

{$else}// fpGUI
procedure TSAK_Init.CheckCount(Sender: TObject);
begin
  timercount.Enabled := False;
  if (isWorking = True) then
  begin
  if fpgapplication.ComponentCount <> CompCount then
  begin
    SAKUnLoadLib;
    InitObject;
    CompCount := fpgapplication.ComponentCount;
  end;
  timercount.Enabled := True;
end;
end;
 {$endif}

 function TSAK_Init.LoadLib: integer;
begin
  Result := -1;
  old8087cw := Get8087CW;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
    exOverflow, exUnderflow, exPrecision]);
  Set8087CW($133f);
  if initspeech.isloaded = True then
    Result := 0
  else
  begin
    if not fileexists(PA_FileName) then
      Result := -2
    else
    if Pa_Load(PA_FileName) then
    begin
      Result := 0;
      Pa_Initialize();
    end
    else
      Result := -21;
    if Result = 0 then
    begin
      if not fileexists(ES_FileName) then
        Result := -3
      else
      if es_Load(es_FileName) then
      begin
            {$IFDEF Windows}
        Result := espeak_Initialize(AUDIO_OUTPUT_SYNCH_PLAYBACK,
          500, pointer(ES_DataDirectory), 0);
            {$else}
        Result := espeak_Initialize(AUDIO_OUTPUT_PLAYBACK, 300,
          pointer(ES_DataDirectory), 0);
            {$ENDIF}
         {$IFDEF lcl}
        if Result > -1 then begin
          TimerCount := Ttimer.Create(TimerCount);
           TimerRepeat := Ttimer.Create(TimerRepeat);
          end;
         {$else}
        if Result > -1 then
        begin
          TimerRepeat := Tfpgtimer.Create(50000);
          TimerRepeat.Enabled := False;
          TimerCount := Tfpgtimer.Create(50000);
          TimerCount.Enabled := False;
        end;
         {$ENDIF}
        end;

      end;
    end;

  if Result > -1 then
  begin
    initspeech.isloaded := True;
         {$IFDEF lcl}
    CompCount := application.ComponentCount;
         {$else}
    CompCount := fpgapplication.ComponentCount;
         {$ENDIF}
    InitObject;
    espeak_Key('sak is working...');
    TimerRepeat.Enabled:=false;
    TimerRepeat.Interval := 600;
    TimerCount.Enabled := False;
    TimerCount.Interval := 700;
    timerCount.OnTimer := @CheckCount;
    if InitSpeech.isWorking = True then
      TimerCount.Enabled := True;
  end
  else
    Result := -31;
end;

function SAKFreeLib: integer;
var
  i: integer;
begin
   if assigned(InitSpeech) then
  begin
    InitSpeech.TimerCount.Enabled:=false;
     InitSpeech.TimerRepeat.Enabled:=false;
  SAKUnLoadLib;
  sleep(100);
   InitSpeech.TimerCount.Free;
   InitSpeech.TimerRepeat.Free;
     for i := 0 to high(InitSpeech.AssistiveData) do
      InitSpeech.AssistiveData[i].Free;
    InitSpeech.Free;
   sleep(100);
  ES_Unload();
  sleep(100);
  Pa_Unload();
  Set8087CW(old8087cw);
end;
 end;

function SAKUnLoadLib: integer;
begin
  InitSpeech.isWorking := False;
  InitSpeech.UnLoadLib;
end;

function TSAK_Init.UnLoadLib: integer;
var
  i: integer;
begin
  if assigned(InitSpeech) then
  begin
    InitSpeech.TimerCount.Enabled := False;
    {$IF DEFINED(LCL)}
       for i := 0 to high(InitSpeech.AssistiveData) do
    begin
       if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TForm) then
      begin
        TForm(InitSpeech.AssistiveData[i].TheObject).OnKeyPress :=
          InitSpeech.AssistiveData[i].OriOnKeyPress;
        TForm(InitSpeech.AssistiveData[i].TheObject).OnKeyDown :=
          InitSpeech.AssistiveData[i].OriOnKeyDown;
        TForm(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].oriOnEnter;
        TForm(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
         TForm(InitSpeech.AssistiveData[i].TheObject).OnDestroy :=
          InitSpeech.AssistiveData[i].OriOnDestroy;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TCheckBox) then
      begin
        TCheckBox(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TCheckBox(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
        TCheckBox(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TComboBox) then
      begin
        TComboBox(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TComboBox(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
        TComboBox(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TListBox) then
      begin
        TListBox(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TListBox(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
        TListBox(InitSpeech.AssistiveData[i].TheObject).OnSelectionChange :=
          InitSpeech.AssistiveData[i].OriOnSelectionChange;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TRadioButton) then
      begin
        TRadioButton(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TRadioButton(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
        TRadioButton(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TButton) then
      begin
        TButton(InitSpeech.AssistiveData[i].TheObject).OnKeyDown :=
          InitSpeech.AssistiveData[i].OriOnKeyDown;
        TButton(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TButton(InitSpeech.AssistiveData[i].TheObject).OnClick :=
          InitSpeech.AssistiveData[i].OriOnClick;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TEdit) then
      begin
        TEdit(InitSpeech.AssistiveData[i].TheObject).OnKeyPress :=
          InitSpeech.AssistiveData[i].OriOnKeyPress;
        TEdit(InitSpeech.AssistiveData[i].TheObject).OnKeyDown :=
          InitSpeech.AssistiveData[i].OriOnKeyDown;
        TEdit(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TEdit(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TMemo) then
      begin
        TMemo(InitSpeech.AssistiveData[i].TheObject).OnKeyPress :=
          InitSpeech.AssistiveData[i].OriOnKeyPress;
        TMemo(InitSpeech.AssistiveData[i].TheObject).OnKeyDown :=
          InitSpeech.AssistiveData[i].OriOnKeyDown;
        TMemo(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TMemo(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TStringgrid) then
      begin
        TStringgrid(InitSpeech.AssistiveData[i].TheObject).OnKeyPress :=
          InitSpeech.AssistiveData[i].OriOnKeyPress;
        TStringgrid(InitSpeech.AssistiveData[i].TheObject).OnKeyDown :=
          InitSpeech.AssistiveData[i].OriOnKeyDown;
        TStringgrid(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
        TStringgrid(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
        TStringgrid(InitSpeech.AssistiveData[i].TheObject).OnKeyUp :=
          InitSpeech.AssistiveData[i].OriOnKeyUp;
      end;
    end;

     {$else}// fpGUI


    for i := 0 to high(InitSpeech.AssistiveData) do
    begin
        if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is Tfpgapplication) then
      begin
        Tfpgapplication(InitSpeech.AssistiveData[i].TheObject).OnKeyPress :=
          InitSpeech.AssistiveData[i].OriOnKeyPress;
        end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgForm) then
      begin
        TfpgForm(InitSpeech.AssistiveData[i].TheObject).OnKeyPress :=
          InitSpeech.AssistiveData[i].OriOnKeyPress;
        TfpgForm(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
        TfpgForm(InitSpeech.AssistiveData[i].TheObject).OnDestroy :=
          InitSpeech.AssistiveData[i].OriOnDestroy;

      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgButton) then
      begin
        TfpgButton(InitSpeech.AssistiveData[i].TheObject).OnClick :=
          InitSpeech.AssistiveData[i].OriOnClick;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgEdit) then
      begin
        TfpgEdit(InitSpeech.AssistiveData[i].TheObject).OnKeyChar :=
          InitSpeech.AssistiveData[i].OriOnKeyChar;
        TfpgEdit(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgMemo) then
      begin
        TfpgMemo(InitSpeech.AssistiveData[i].TheObject).OnKeyChar :=
          InitSpeech.AssistiveData[i].OriOnKeyChar;
        TfpgMemo(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgStringgrid) then
      begin
        TfpgStringgrid(InitSpeech.AssistiveData[i].TheObject).OnFocusChange :=
          InitSpeech.AssistiveData[i].OriOnFocusChange;
        TfpgStringgrid(InitSpeech.AssistiveData[i].TheObject).OnMouseDown :=
          InitSpeech.AssistiveData[i].OriOnMouseDown;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgCheckBox) then
      begin
        TfpgCheckBox(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
        TfpgCheckBox(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgComboBox) then
      begin
        TfpgComboBox(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
        TfpgComboBox(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgListBox) then
      begin
        TfpgListBox(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
        TfpgListBox(InitSpeech.AssistiveData[i].TheObject).OnEnter :=
          InitSpeech.AssistiveData[i].OriOnEnter;
      end
      else
      if (assigned(InitSpeech.AssistiveData[i].TheObject)) and
        (InitSpeech.AssistiveData[i].TheObject is TfpgRadiobutton) then
      begin
        TfpgRadiobutton(InitSpeech.AssistiveData[i].TheObject).OnChange :=
          InitSpeech.AssistiveData[i].OriOnChange;
      end;
    end;

     {$endif}
    SetLength(InitSpeech.AssistiveData, 0);
  end;
end;
////////////////////// Voice Config Procedures
function SAKSetVoice(gender : shortint; language : string) : integer;
begin
CustomVoice.voice.languages:= pchar(language);
CustomVoice.voice.gender:=gender;
espeak_SetVoiceByProperties(CustomVoice.voice);
end;

////////////////////// Speecher Procedures ////////////////

function SAKSay(Text: string): integer; ///// Start speaking the text with default voice
begin
  espeak_Key(pointer(Text));
end;

function SAKSay(Text: string; VoiceParam: TSAK_VoiceConfig): integer;
  ///// Start speaking the text with custom voice
begin
  espeak_Key(pointer(Text));   //// TODO
end;

function SAKSay(char: word): integer;
  ///// Start speaking the character with default voice
begin
  espeak_Char(char);
end;

function SAKSay(char: word; VoiceParam: TSAK_VoiceConfig): integer;
  ///// Start speaking with custom voice
begin
    espeak_Char(char); ///// TODO
end;

function SAKStop: integer; /// stop speaking;
begin
  espeak_Cancel;
end;


end.