(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit ReplayUserDialog;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils, XForm,
  {$ifndef JScript}
  Forms, Controls, Dialogs, ExtCtrls,LazsUtils,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel, XVBox, XHBox, XMemo, XButton,
  XLabel, XEditBox
  , EventsInterface;

type

  { TReplayUserDialogForm }

  TReplayUserDialogForm = class(TXForm)
    ReplayUserEntry: TXEditBox;
    {$ifndef JScript}
    ReplayUserDialogTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$endif}
  private
  public
    procedure RunReplayDialog(Sender:TObject);
  end;

Procedure AddChars(instring:string;counter,loopEnd:integer);

var
  ReplayUserDialogForm: TReplayUserDialogForm;
  theText:String;

implementation

{$R *.lfm}

Procedure AddChars(instring:string;counter,loopEnd:integer);
var tempstr:string;
begin
{$ifndef JScript}
   if (counter<loopEnd+1) then
   begin
     tempstr:=LeftStr(instring,counter);
     EditAttributeValue('ReplayUserEntry','','ItemValue',tempstr);
     Application.ProcessMessages;
     sleep(200);
     AddChars(instring,counter+1,loopEnd);
     if (counter=loopEnd) then
     begin
       Application.ProcessMessages;
       sleep(1000);
       closeXForm('ReplayUserDialogForm','');
     end;
   end;
{$else}
   // already queued up ALL of the AddChars calls (so not daisy-chained here)
   tempstr:=LeftStr(instring,counter+1);
   EditAttributeValue('ReplayUserEntry','','ItemValue',tempstr);
{$endif}
end;

procedure TReplayUserDialogForm.RunReplayDialog(Sender:TObject);
var
  i,loopEnd:integer;
begin
  {$ifndef JScript}
  ReplayUserDialogTimer.Enabled:=false;
  AddChars(theText,0,length(theText));
  {$else}
  loopEnd:=length(theText);
  // Queue up all of the AddChars jobs.
  asm
    //console.log('RunReplayDialog. text='+pas.ReplayUserDialog.theText);
    for (var i=0; i<loopEnd; i++) {
      myTimeout(pas.ReplayUserDialog.AddChars,200,'AddChars',0,pas.ReplayUserDialog.theText,i,loopEnd);
    }
    myTimeout(pas.XForm.CloseXForm,400,'CloseXForm ReplayUserDialogForm',0,'ReplayUserDialogForm','');
  end;
  {$endif}
end;

{ TReplayUserDialogForm }

{$ifndef JScript}
procedure TReplayUserDialogForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

procedure TReplayUserDialogForm.FormShow(Sender: TObject);
begin
  ReplayUserDialogTimer.OnTimer:=@RunReplayDialog;
  ReplayUserDialogTimer.Enabled:=true;
end;

{$endif}


end.

