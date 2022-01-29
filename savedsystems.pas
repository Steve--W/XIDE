(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit SavedSystems;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils,
  {$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLIntf, ExtCtrls, Menus, ComCtrls, TypInfo, LazIDEIntf,
  LazsUtils,
  UtilsJSCompile, Events, Types,
  {$else}
  HTMLUtils,
  {$endif}
  XScrollBox, XVBox, XHBox, XTree, XMemo, XTabControl, XButton, XLabel,
  XEditBox, XCheckBox, XHyperLink, XRadioBtns, XForm, XComboBox,
  EventsInterface, XObjectInsp, XDataModel ;

type

  { TSavedSystemsForm }

  TSavedSystemsForm = class(TXForm)
    SavedSystemsList: TXTree;
    SavedSystemsVBox: TXVBox;
    SavedSystemsLoad: TXButton;
    SavedSystemsBtns: TXHBox;
    SavedSystemsCancel: TXButton;
    SavedSystemsDelete: TXButton;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    {$endif}
    procedure SavedSystemsCancelHandleButtonClick(e: TEventStatus;
      nodeID: AnsiString; myValue: AnsiString);
    procedure SavedSystemsDeleteHandleButtonClick(e: TEventStatus;
      nodeID: AnsiString; myValue: AnsiString);
    procedure SavedSystemsLoadHandleButtonClick(e: TEventStatus;
      nodeID: AnsiString; myValue: AnsiString);

  public
    procedure Initialise;

  end;

var
  SavedSystemsForm: TSavedSystemsForm;
//  TopComponent:TControl;

implementation

{$R *.lfm}

{$ifndef JScript}
//procedure TSavedSystemsForm.FormCreate(Sender: TObject);
//begin
//  myNode:=DoXFormCreated(self);
//end;
//
//procedure TSavedSystemsForm.FormResize(Sender: TObject);
//begin
//  DoFormResize(self, SavedSystemsVBox);
//end;
//

procedure TSavedSystemsForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

procedure TSavedSystemsForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, SavedSystemsVBox);
end;

{$endif}

procedure TSavedSystemsForm.SavedSystemsCancelHandleButtonClick(
  e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
begin
  TXForm(self).Showing:='No';
end;

function RemoveDate(NodeString:String):String;
var
  i,j:integer;
begin
  i:=FoundString(NodeString,'      ');
  if i>1 then
  begin
    for j:=1 to i-1 do
      result:=result+NodeString[j];
  end
  else
    result:=NodeString;
end;

procedure TSavedSystemsForm.SavedSystemsDeleteHandleButtonClick(
  e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
var
  SysName,SysPath:String;
begin
  // Get SysName from the currently selected item in SavedSystemsList
  SysName:=RemoveDate(SavedSystemsList.SelectedNodeText);
  SysPath:=SysName;
  if SysName<>'' then
  begin
    if XIDEConfirm('OK to delete stored system '+SysName+'?') then
    begin
      {$ifndef JScript}
      SysPath:='SavedSystems/'+SysName;
      {$endif}
      ClearLocalStore( SysPath+'.xide');
      if SysName <> UIRootNode.GetAttribute('SystemName',false).AttribValue then
        DeleteLocalDB(SysName,true);
      Initialise;
    end;
  end;
end;

procedure TSavedSystemsForm.SavedSystemsLoadHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  SysName:String;
begin
  // Get SysName from the currently selected item in SavedSystemsList
  SysName:=RemoveDate(SavedSystemsList.SelectedNodeText);
  if SysName<>'' then
  begin
    OILoadSavedSystem(SysName);
    TXForm(self).Showing:='No';
  end;
end;

procedure TSavedSystemsForm.Initialise;
var
  NamesList:TStringList;
  TreeString:String;
  i:integer;
begin
  // populate SavedSystemsList from stored data
  NamesList:=TStringList.Create;
  NamesList.Clear;
  DiscoverSavedSystems(NamesList);

//Example Tree String = '["myTreeName",["AAAAAA","BBBBB"],"CCCCC","DDDDDD",["EEEEE","FFFFF"],"GGGGG"]';
  TreeString:='["Saved Systems"';

  for i:=0 to NamesList.count-1 do
  begin
    if NamesList[i]<>'' then
      TreeString:=TreeString + ',"' + NamesList[i] + '"';
  end;
  TreeString:=TreeString + ']';
  SavedSystemsList.TreeData:=TreeString;

  NamesList.Free;
end;

end.

