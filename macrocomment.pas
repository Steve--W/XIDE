(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit MacroComment;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils, XForm, Events,
  {$ifndef JScript}
  Forms, Controls, Dialogs, ExtCtrls,LazsUtils,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel, XVBox, XHBox, XMemo, XButton,
  XLabel, XEditBox
  , EventsInterface, XHTMLText;

type

  { TMacroCommentForm }

  TMacroCommentForm = class(TXForm)
    MacroCommentMemo: TXMemo;
    MacroCommentDone: TXButton;
    MacroCommentBtns: TXHBox;
    MacroCommentHTMLText: TXHTMLText;
    XVBox1: TXVBox;

    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$endif}
    procedure MacroCommentDoneHandleButtonClick(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
    procedure MacroCommentFormHandleClosure(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
  private

  public
    procedure SaveMacroComment;
    procedure ShowMemo;
    procedure ShowHTMLText(txt:String);
  end;

var
  MacroCommentForm: TMacroCommentForm;

{$ifdef JScript}
procedure  ContinueReplay;
{$endif}


implementation

{$R *.lfm}

{ TMacroCommentForm }

{$ifndef JScript}
procedure TMacroCommentForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

procedure TMacroCommentForm.FormShow(Sender: TObject);
begin

end;

{$endif}

procedure TMacroCommentForm.SaveMacroComment;
begin
  // user has closed the form after insertng comment text.
//  {$ifndef JScript}
   HandleEvent(nil,'UserInput','UIRootNode','',MacroCommentForm.MacroCommentMemo.ItemValue);
//  {$else}
//  asm
//     pas.Events.handleEvent(null,'UserInput','UIRootNode','',str);
//  end;
//  {$endif}
end;

{$ifdef JScript}
procedure  ContinueReplay;
begin
  asm
    //alert('continue replay...');
    var idx=pas.EventLogging.MacroEventList.CurrentEvent;
    myTimeout(pas.EventLogging.CheckEventDone, 210, 'checkdone',0,idx,0);
    myTimeout(pas.EventLogging.SetCompletedEvent,200,('SetCompletedEvent '+idx.toString()),0,idx);
  end;
end;
{$endif}

procedure TMacroCommentForm.MacroCommentDoneHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  if MacroCommentMemo.IsVisible=true then
    SaveMacroComment;
  closeXForm('MacroCommentForm','');     // will call Closure handler
end;

procedure TMacroCommentForm.MacroCommentFormHandleClosure(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
begin
//  showmessage('comment form closure');
  {$ifdef JScript}
  if MacroCommentMemo.IsVisible=false then
  // continue replay...
  begin
    ContinueReplay;
  end;
  {$endif}
end;

procedure TMacroCommentForm.ShowMemo;
begin
  MacroCommentMemo.ItemValue:='';
  MacroCommentMemo.IsVisible:=true;
  MacroCommentHTMLText.IsVisible:=false;
end;

procedure TMacroCommentForm.ShowHTMLText(txt:String);
begin
  MacroCommentMemo.IsVisible:=false;
  MacroCommentHTMLText.SourceText:=txt;
  MacroCommentHTMLText.IsVisible:=true;
end;

end.

