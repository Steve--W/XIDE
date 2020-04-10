(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit AboutUnit;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
Classes, SysUtils, StringUtils, NodeUtils, XForm,
{$ifndef JScript}
Forms, Controls, Dialogs,LazsUtils,UtilsJSCompile,pas2jscompiler,
{$ifdef Chromium}
uCEFApplication,
{$endif}
{$else}
HTMLUtils,
{$endif}
PyXUtils,WrapperPanel, XVBox, XHBox, XMemo, XButton,
XLabel, XEditBox
, EventsInterface;

type

  { TAboutXIDEForm }

  TAboutXIDEForm = class(TXForm)
    AboutFormXVBox1: TXVBox;
    AboutFormXMemo1: TXMemo;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$endif}
  private

  public

  end;

var
  AboutXIDEForm: TAboutXIDEForm;
  AboutFormRoot:TDataNode;

implementation

{$R *.lfm}

var
  AboutText:String;


{ TAboutXIDEForm }

{$ifndef JScript}
procedure TAboutXIDEForm.FormCreate(Sender: TObject);
var
  str:string;
  vs:String;
  {$ifndef JScript}
  MyCompilerObj :TMyCompilerObj;
  {$endif}
begin
  myNode:=DoXFormCreated(self);
  AboutFormRoot:=FindDataNodeById(myNode,'AboutFormXVBox1','',true);

  str:=AboutText;
  str:=myStringReplace(str,'<<<XIDE>>>','0.1.beta',1,-1);
  str:=myStringReplace(str,'<<<LAZ>>>','2.1.0',1,-1);
  str:=myStringReplace(str,'<<<FPC>>>','3.3.1',1,-1);

  {$ifndef JScript}
  // get version of pas2js
  MyCompilerObj :=TMyCompilerObj.Create;
  MyCompilerObj.Compiler:=TPas2jsCompiler.Create;
  vs:=MyCompilerObj.Compiler.GetVersion(false);
  str:=myStringReplace(str,'<<<Pas2JS>>>',vs,1,-1);
  MyCompilerObj.Compiler.Free;
  MyCompilerObj.Free;

  {$ifdef Chromium}
  // get version of cef library
  str:=myStringReplace(str,'<<<CEF>>>',GlobalCEFApp.LibCefVersion,1,-1);
  //showmessage('CEF Library version '+GlobalCEFApp.LibCefVersion);
  {$endif}
  {$ifdef Python}
  str:=myStringReplace(str,'<<<Python>>>',PyXUtils.PythonVersion,1,-1);
  {$endif}
  {$else}
  {$ifdef Python}
  //!!!! (version of pyodide???)
  {$endif}
  {$endif}

  AboutFormXMemo1.ItemValue:=str;
end;

procedure TAboutXIDEForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, AboutFormXVBox1);
end;

procedure TAboutXIDEForm.FormShow(Sender: TObject);
begin

end;
{$endif}
begin
  AboutText :=
  '           Helping people write simple apps that work "everywhere" ' + LineEnding
 +' ' + LineEnding
     +'XIDE is a combined Development and Run Time Environment designed to run in Chrome/Linux/Windows/Android with as little installation or learning curve as possible.' + LineEnding
     +' ' + LineEnding
     +'It allows WYSIWYG Pascal/RAD application development in up to date HTML5 browsers and any native code platform that supports Electron or Lazarus/Pas2JS and the Chromium Embedded Framework.' + LineEnding
     +'' + LineEnding
     +'The target applications are small, “in house”, client side, embedded or exploratory apps where the focus is on rapid development, specialist functionality or collaborative working.' + LineEnding
     +'' + LineEnding
     +'Applications developed for this run time will run client side in any of the target environments without modification or dependencies both on line or off line (once the run time has been installed or the web page downloaded).' + LineEnding
     +'' + LineEnding
     +'        Version <<<XIDE>>> for Chrome' + LineEnding
     +'        Date 11/03/2019' + LineEnding
     +'        Created with' + LineEnding
     +'            ..............Lazarus    <<<LAZ>>>' + LineEnding
     +'            ..............FPC        <<<FPC>>>' + LineEnding
     +'            ..............Pas2JS     <<<Pas2JS>>>' + LineEnding
     {$ifdef Chromium}
     +'            ..............CEF library <<<CEF>>>' + LineEnding
     {$endif}
     {$ifdef Python}
     {$ifndef JScript}
     +'            ..............Python4Delphi  <<<Python>>>' + LineEnding
     {$else}
     +'            ..............Python     (pyodide)' + LineEnding
     {$endif}
     {$endif}
     +'        Copyright  © Steve Wright all rights reserved' + LineEnding
     +'        License: GPL' + LineEnding
     +'                                                               ' + LineEnding
     +'Third Party Libraries and Tools included in the run time' + LineEnding
     +'            Pas2JS (http://wiki.freepascal.org/pas2js)  - GPL/LGPL Licence' + LineEnding
     +'            GPUJS  (https://github.com/gpujs/gpu.rocks) - MIT Licence' + LineEnding
     +'            Pell   (https://github.com/jaredreich/pell) - MIT Licence' + LineEnding
     +'            CEF4Delphi  (https://github.com/salvadordf/CEF4Delphi)    - ?????????? Licence' + LineEnding
     +'            Python4Delphi (https://github.com/pyscripter/python4delphi) - ?????????? Licence' + LineEnding
     +'            pyodide (http://https://pyodide.readthedocs.io/en/latest/)  - ????? Licence' + LineEnding
     +'' + LineEnding
     +'This program is free software: you can redistribute it and/or modify' + LineEnding
     +'    it under the terms of the GNU General Public License as published by' + LineEnding
     +'    the Free Software Foundation, either version 3 of the License, or' + LineEnding
     +'    any later version. See <https://www.gnu.org/licenses/>' + LineEnding
     +'' + LineEnding
     +'    This program is distributed in the hope that it will be useful,' + LineEnding
     +'    but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding
     +'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + LineEnding
     +'    GNU General Public License for more details.' + LineEnding
 ;
end.

