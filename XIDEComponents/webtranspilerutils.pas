unit WebTranspilerUtils;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, StringUtils,XCode
  {$ifndef JScript}
  ,dialogs
  {$else}
  ,webfilecache, pas2jswebcompiler
  {$endif}
  ;

{$ifdef JScript}
type
TWebCompilerObj = Class(TObject)
Private
  FCompiler : TPas2JSWebCompiler;
  FCodeEditor:TXCode;
  procedure OnUnitLoaded(Sender: TObject; aFileName: String; aError: string);
Protected
Public
  procedure DoLog(Sender: TObject; const Msg: String);
  Constructor Create;
  property myCodeEditor:TXCode read FCodeEditor write FCodeEditor;
  property Compiler:TPas2JSWebCompiler read FCompiler write FCompiler;
end;

procedure LoadRTLFilesForPas2JS(lWebFS : TPas2JSWebFS);    //TWebCompilerObj);

var MyWebCompiler:TWebCompilerObj;
{$endif}

procedure WriteIncFile(Compiler:TObject;IncName,EventType,IncPath:String;
                       var MainCode:TStringList;IncCode:TStringList);
function LoadIncludeFile(Compiler:TObject;FileName,IncPath:String):TStringList;

implementation

procedure WriteIncFile(Compiler:TObject;IncName,EventType,IncPath:String;
                       var MainCode:TStringList;IncCode:TStringList);
var
{$ifndef JScript}
   TheStream:TFileStream;
   {$endif}
   FileName:string;
begin
  if EventType<>'' then IncName:=IncName+'__'+EventType;

  if length(IncName)<251 then
  begin

    {$ifndef JScript}
    FileName:=IncPath+IncName+'.inc';

    try
      TheStream:=TFileStream.Create(Filename,fmCreate or fmOpenRead or fmOpenWrite or fmShareDenyNone);
      IncCode.SaveToStream(TheStream);
      TheStream.Free;
    except
      showmessage('Failed to create include file '+FileName);
    end;
    {$else}
    FileName:=IncName+'.inc';
    // save the generated inc file
    TPas2JSWebCompiler(Compiler).WebFS.SetFileContent(FileName,IncCode.Text);
    {$endif}

    MainCode.Add('{$I '+IncName+'.inc}');

  end
  else
    showmessage('Unable to write include file; name is too long. '+IncName);
end;

function LoadIncludeFile(Compiler:TObject;FileName,IncPath:String):TStringList;
var
  tmp1:TStringList;
  tmp:String;
  {$ifndef JScript}
  TheStream:TFileStream;
  {$endif}
begin
  tmp1:=TStringList.Create;
  {$ifndef JScript}
  try
  // find and load the include file...
  TheStream:=TFileStream.Create(IncPath+FileName,fmOpenRead or fmShareDenyNone);
  tmp1.LoadFromStream(TheStream);
  TheStream.Free;
  except
    try
    // try once more with a .inc suffix (FPC keeps this, pas2js doesn't (!?).....
    TheStream:=TFileStream.Create(IncPath+FileName+'.inc',fmOpenRead or fmShareDenyNone);
    tmp1.LoadFromStream(TheStream);
    FileName:=FileName+'.inc';
    TheStream.Free;
    except
      showmessage('file '+IncPath+FileName+' not available');
      tmp1.Clear;
    end;
  end;
  {$else}
 // tmp:=MyWebCompiler.Compiler.WebFS.GetFileContent(FileName);
  tmp:=TPas2JSWebCompiler(Compiler).WebFS.GetFileContent(FileName);
  tmp1.Text:=tmp;
  {$endif}
  result:=tmp1;
end;


{$ifdef JScript}
constructor TWebCompilerObj.Create;
begin
  FCompiler:=TPas2JSWebCompiler.Create;
end;

procedure TWebCompilerObj.DoLog(Sender: TObject; const Msg: String);
begin
  if myCodeEditor<>nil then
    myCodeEditor.MessageLines:=myCodeEditor.MessageLines+LineEnding+Msg;
end;
procedure TWebCompilerObj.OnUnitLoaded(Sender: TObject; aFileName: String; aError: string);
begin
  if myCodeEditor<>nil then
    if aError='' then
      myCodeEditor.MessageLines:=myCodeEditor.MessageLines+LineEnding+'Loaded: '+aFileName
    else
      myCodeEditor.MessageLines:=myCodeEditor.MessageLines+LineEnding+'Error Loading "'+aFileName+'": '+AError;
end;

procedure LoadRTLFilesForPas2JS(lWebFS : TPas2JSWebFS);  //Compiler:TPas2JSWebCompiler);    //TWebCompilerObj);
begin
  asm
    // minimal required rtl set....
    //lWebFS.SetFileContent('system.pas',pas.UtilsJSCompile.systempas);
    //lWebFS.SetFileContent('sysutils.pas',pas.UtilsJSCompile.sysutilspas);
    //lWebFS.SetFileContent('classes.pas',pas.UtilsJSCompile.classespas);
    //lWebFS.SetFileContent('rtlconsts.pas',pas.UtilsJSCompile.rtlconstspas);
    //lWebFS.SetFileContent('js.pas',pas.UtilsJSCompile.jspas);
    //lWebFS.SetFileContent('types.pas',pas.UtilsJSCompile.typespas);

    // common rtl set  (Desktop (windows/linux) and pas2js) ....
    lWebFS.SetFileContent('classes.pas',pas.CompileUserCode.classespas);
    lWebFS.SetFileContent('contnrs.pas',pas.CompileUserCode.contnrspas);
    lWebFS.SetFileContent('dateutils.pas',pas.CompileUserCode.dateutilspas);
    lWebFS.SetFileContent('js.pas',pas.CompileUserCode.jspas);
    lWebFS.SetFileContent('math.pas',pas.CompileUserCode.mathpas);
    lWebFS.SetFileContent('rtlconsts.pas',pas.CompileUserCode.rtlconstspas);
//    lWebFS.SetFileContent('rtti.pas',pas.CompileUserCode.rttipas);
    lWebFS.SetFileContent('strutils.pas',pas.CompileUserCode.strutilspas);
    lWebFS.SetFileContent('system.pas',pas.CompileUserCode.systempas);
    lWebFS.SetFileContent('sysutils.pas',pas.CompileUserCode.sysutilspas);
    lWebFS.SetFileContent('types.pas',pas.CompileUserCode.typespas);
    lWebFS.SetFileContent('typinfo.pas',pas.CompileUserCode.typinfopas);

  //  lWebFS.SetFileContent('timer.pas',pas.UtilsJSCompile.timerpas);
  //  lWebFS.SetFileContent('nodejs.pas',pas.UtilsJSCompile.nodejspas);
  //  lWebFS.SetFileContent('objpas.pas',pas.UtilsJSCompile.objpaspas);
  //  lWebFS.SetFileContent('libjquery.pas',pas.UtilsJSCompile.libjquerypas);
  //  lWebFS.SetFileContent('hotreloadclient.pas',pas.UtilsJSCompile.hotreloadclientpas);
  //  lWebFS.SetFileContent('class2pas.pas',pas.UtilsJSCompile.class2paspas);
  //  lWebFS.SetFileContent('browserconsole.pas',pas.UtilsJSCompile.browserconsolepas);
  //  lWebFS.SetFileContent('web.pas',pas.UtilsJSCompile.webpas);
  //  lWebFS.SetFileContent('webaudio.pas',pas.UtilsJSCompile.webaudiopas);
  //  lWebFS.SetFileContent('webbluetooth.pas',pas.UtilsJSCompile.webbluetoothpas);
  //  lWebFS.SetFileContent('webgl.pas',pas.UtilsJSCompile.webglpas);
  //  lWebFS.SetFileContent('webrouter.pas',pas.UtilsJSCompile.webrouterpas);
  end;
end;
{$endif}

begin
{$ifdef JScript}
MyWebCompiler := TWebCompilerObj.Create;
{$endif}
end.

