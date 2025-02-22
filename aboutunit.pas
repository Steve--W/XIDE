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
WebTranspilerUtils,HTMLUtils,
{$endif}
PyXUtils,WrapperPanel, XVBox, XHBox, XMemo, XButton,
XLabel, XEditBox;

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
    procedure BuildText;
  private

  public

  end;

var
  AboutXIDEForm: TAboutXIDEForm;
  AboutFormRoot:TDataNode;

implementation

{$R *.lfm}



{ TAboutXIDEForm }

{$ifndef JScript}
procedure TAboutXIDEForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
  AboutFormRoot:=FindDataNodeById(myNode,'AboutFormXVBox1','',true);

  BuildText;

end;

procedure TAboutXIDEForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, AboutFormXVBox1);
end;

procedure TAboutXIDEForm.FormShow(Sender: TObject);
begin
  BuildText;
end;
{$endif}
procedure TAboutXIDEForm.BuildText;
var
  AboutText:String;
  str:string;
  vs:String;
  {$ifndef JScript}
  MyCompilerObj :TMyCompilerObj;
  {$endif}
begin
  AboutText :=
     '              A simple "Browser First" IDE for Pascal and Python ' + LineEnding
     +' ' + LineEnding
     +'XIDE is a combined Client Side Run Time Library and RAD IDE intended to allow Pascal and/or Python development in the browser with the minimum of installation '
     +'overhead or learning curve while also being as platform independent as possible. It is targeted at Prototyping, Small Group Collaboration and Agile Line of '
     +'Business projects on any platform that is supported by Chrome or Electron, or Lazarus (and the Chrome Embedded Framework). It is not intended for the development of general-purpose ' + LineEnding
     +'public facing web sites.' + LineEnding
     +' ' + LineEnding
     +'        Version <<<XIDE>>> for Chrome' + LineEnding
     +'        Date Feb 2025' + LineEnding
     +'        Created with' + LineEnding
     +'            ..............Lazarus    <<<LAZ>>>' + LineEnding
     +'            ..............FPC        <<<FPC>>>' + LineEnding
     +'            ..............Pas2JS     <<<Pas2JS>>>' + LineEnding
     {$ifndef JScript}
     {$ifdef Chromium}
     +'            ..............CEF library <<<CEF>>>' + LineEnding
     {$endif}
     {$endif}
     {$ifdef Python}
     {$ifndef JScript}
     +'            ..............Python <<<Python>>>' + LineEnding
     {$else}
     +'            ..............Pyodide     <<<Pyodide>>>' + LineEnding
     {$endif}
     {$endif}
     +'        Copyright(c) Steve Wright all rights reserved' + LineEnding
     +'        License: GPL' + LineEnding
     +'                                                               ' + LineEnding
     +'Third Party Libraries and Tools included in the run time' + LineEnding
     +'            Pas2JS (http://wiki.freepascal.org/pas2js)  - GPL/LGPL Licence' + LineEnding
     +'            GPUJS  (https://github.com/gpujs/gpu.rocks) - MIT Licence' + LineEnding
     +'            Pell   (https://github.com/jaredreich/pell) - MIT Licence' + LineEnding
     +'            CEF4Delphi  (https://github.com/salvadordf/CEF4Delphi)    - Lesser GNU General Public License' + LineEnding
     +'            Python4Delphi (https://github.com/pyscripter/python4delphi) - MIT Licence' + LineEnding
     +'            pyodide (https://github.com/iodide-project/pyodide)  - Mozilla Public License 2.0' + LineEnding
     +'              + support for running pyodide offline from: ' + LineEnding
     +'                (https://github.com/iodide-project/pyodide/pull/606)  ' + LineEnding
     +'' + LineEnding
     +'This program is free software: you can redistribute it and/or modify' + LineEnding
     +'    it under the terms of the GNU General Public License as published by' + LineEnding
     +'    the Free Software Foundation, either version 3 of the License, or' + LineEnding
     +'    any later version. See <https://www.gnu.org/licenses/>' + LineEnding
     +'' + LineEnding
     +'    The program is distributed in the hope that it will be useful,' + LineEnding
     +'    but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding
     +'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + LineEnding
     +'    GNU General Public License for more details.' + LineEnding
 ;


  str:=AboutText;
  str:=myStringReplace(str,'<<<XIDE>>>','0.3.beta',1,-1);
  str:=myStringReplace(str,'<<<LAZ>>>','3.4',1,-1);
  str:=myStringReplace(str,'<<<FPC>>>','3.2.2',1,-1);

  {$ifndef JScript}
  // get version of pas2js
  MyCompilerObj :=TMyCompilerObj.Create;
  MyCompilerObj.Compiler:=TPas2jsCompiler.Create;
  vs:=MyCompilerObj.Compiler.GetVersion(false);
  str:=myStringReplace(str,'<<<Pas2JS>>>',vs,1,-1);
  MyCompilerObj.Compiler.Free;
  MyCompilerObj.Free;
  {$else}
  vs:=MyWebCompiler.Compiler.GetVersion(false);
  str:=myStringReplace(str,'<<<Pas2JS>>>',vs,1,-1);
  {$endif}

  {$ifndef JScript}
  {$ifdef Chromium}
  // get version of cef library
  str:=myStringReplace(str,'<<<CEF>>>',GlobalCEFApp.LibCefVersion,1,-1);
  //showmessage('CEF Library version '+GlobalCEFApp.LibCefVersion);
  {$endif}
  {$endif}
  {$ifdef Python}
  str:=myStringReplace(str,'<<<Python>>>',PyXUtils.PythonVersion,1,-1);
  str:=myStringReplace(str,'<<<Pyodide>>>','(basvandertol:runlocal)',1,-1);
  //!!!! (version of pyodide???)
  {$endif}

  AboutFormXMemo1.ItemValue:=str;
end;

begin
end.

