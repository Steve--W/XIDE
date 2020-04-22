(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit CodeEditor;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes, SysUtils, StringUtils, NodeUtils,CompileUserCode,
  {$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLIntf, ExtCtrls, Menus, StdCtrls, ComCtrls, Grids,
  TypInfo, LazIDEIntf,
  LazsUtils,
  Events, Types,
  SynEdit, SynEditTypes,
  {$else}
  HTMLUtils,
  {$endif}
  webTranspilerUtils, UtilsJSCompile, WrapperPanel, XScrollBox, XVBox, XHBox, XTree, XMemo, XTabControl, XButton,
  XLabel, XEditBox, XCheckBox, XComboBox, XHyperLink, XRadioBtns, XCode, XForm, XGPUCanvas, XGPUEditor,
  XMenu, InputSelectUnit, PopupMemo,EventsInterface;



type

  { TCodeEditForm }

  TCodeEditForm = class(TXForm)
    CodeEditInputLabelsHBox: TXHBox;
    CodeEditFormXHBox2: TXHBox;
    CodeEditSaveBtn: TXButton;
    CodeEditCancelBtn: TXButton;
    CodeEditFormXHBox1: TXHBox;
    CodeEditFormXVBox1: TXVBox;
    CodeEdit: TXCode;
    CodeEditParamListScrollBox: TXScrollBox;
    CodeEditListLbl: TXLabel;
    CodeEditAddInputBtn: TXButton;
    CodeEditDelInputBtn: TXButton;
    CodeEditInputsVBox: TXVBox;
    CodeEditInputsVBox1: TXVBox;
    CodeEditColLabl1: TXLabel;
    CodeEditColLbl2: TXLabel;
    CodeEditColLbl3: TXLabel;
    CodeEditFunctionResultType: TXComboBox;
    CodeEditHelpBtn: TXButton;
    CodeEditMainTabs: TXTabControl;
    CodeEditMainCodeTab: TXTabSheet;
    CodeEditInitTab: TXTabSheet;
    CodeEditInit: TXCode;
    CodeEditFindBtn: TXButton;
    CodeEditFindTxt: TXEditBox;
    CodeEditFindNextBtn: TXButton;
    CodeEditFindCase: TXCheckBox;
    CodeEditFindGlobal: TXCheckBox;
    CodeEditContextLabel: TXLabel;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$endif}
    procedure AddInputBox(myInput:TCodeInputRec);
    procedure Initialise;
    procedure InitialiseOnShow(Context,NodeName,EvType:String);
    procedure CodeEditAddInputBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditDelInputBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditInitHandleClickMessage(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditCancelBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditHelpBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString;  myValue: AnsiString);
    procedure CodeEditSaveBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure EditInputSynonym(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure EditInputSource(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure ShowInputSelector(e:TEventStatus;nodeID: AnsiString;myValue: AnsiString);
    procedure InputCheckBoxClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditFindBtnHandleButtonClick(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
    procedure CodeEditFindNextBtnHandleButtonClick(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
    function ValidateInputs:Boolean;
    procedure DoLocalSearch(TextToFind:String);
    procedure DoGlobalSearch(TextToFind:String);
    procedure NavigateToFirstError;
    procedure SetCursorPosition(targetLine,targetChar:integer);
    procedure DisplayIncFile(FileName,lineNum,charPos:String);
    procedure HandleMessageClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString; ThisCodeEdit:TXCode);
    procedure CodeEditMainTabsHandleChange(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
  private

  public
    Mode:String;               //  dll, EventCode, FunctionCode, UnitCode, PasUnitCode, PythonScriptCode
    TargetNodeName:String;
    EventType:String;

    NumInputs:integer;
    MyInputs:TCodeInputs;
    CheckedInputs:Array of Boolean;
    {$ifdef JScript}
    LastSearchIndex:integer;
    RegExp:TObject;
    {$endif}
end;

var
  CodeEditForm: TCodeEditForm;
  CodeEditStatus:String;
  OIEditBox, CodeFormRoot:TDataNode;

implementation
uses XObjectInsp;

{$R *.lfm}

{ TCodeEditForm }

procedure TCodeEditForm.InitialiseOnShow(Context,NodeName,EvType:String);
begin
  TargetNodeName:=NodeName;
  EventType:=EvType;
  self.CodeEditContextLabel.LabelCaption:=Context+' '+NodeName+' '+EvType;
  CodeEdit.LabelText:='';
  NumInputs:=0;
  DeleteNodeChildren(CodeEditParamListScrollBox.myNode);

  CodeEditFindGlobal.Checked:=false;

  if (Mode='dll') then
  begin
    CodeEditFindTxt.ItemValue:='';
    CodeEdit.ContainerHeight:='100%';           // CodeEdit is % of the parent component (tabpage)
    CodeEditInitTab.IsVisible:=false;
    CodeEditFunctionResultType.IsVisible:=false;
    CodeEditInputsVBox.IsVisible:=false;
    CodeEdit.MessagesHeight:='30%';
  end
  else if (Mode='EventCode') then
  begin
    CodeEditFindTxt.ItemValue:='';
    CodeEditFunctionResultType.IsVisible:=false;
    CodeEditInputsVBox.IsVisible:=false;
    if CodeEdit.MessageLines='' then
      CodeEdit.MessagesHeight:='1'
    else
      CodeEdit.MessagesHeight:='30%';
    CodeEdit.ContainerHeight:='95%';           // CodeEdit is % of the parent component (tabpage)

    if FoundString(EventType,'Thread')<>1 then
    begin
      CodeEditInitTab.IsVisible:=true;
      CodeEditInit.ContainerHeight:='95%';
      CodeEditInit.MessagesHeight:='1';
     end
    else
    begin
      // no initialisation code allowed for worker threads
      CodeEditInitTab.IsVisible:=false;
    end;
  end
  else
  if (Mode='FunctionCode') then
  begin
    CodeEdit.ContainerHeight:='60%';           // CodeEdit is % of the parent component (tabpage)
    CodeEditInitTab.IsVisible:=false;
    CodeEditFunctionResultType.IsVisible:=true;
    if CodeEdit.MessageLines='' then
      CodeEdit.MessagesHeight:='1'
    else
      CodeEdit.MessagesHeight:='10%';
    CodeEditInputsVBox.IsVisible:=true;
    CodeEditFindTxt.ItemValue:='';

    InputSelectForm.ExclNodeName:=TargetNodeName;
    InputSelectForm.ExclPropertyName:='';
    InputSelectForm.PropType:='';
    InputSelectForm.Initialise;
  end
  else
  if (Mode='SearchCode') then
  begin
    CodeEdit.ContainerHeight:='100%';           // CodeEdit is % of the parent component (tabpage)
    CodeEditInitTab.IsVisible:=false;
    CodeEditFunctionResultType.IsVisible:=false;
    CodeEditInputsVBox.IsVisible:=false;
    CodeEdit.MessagesHeight:='30%';
    CodeEditFindGlobal.Checked:=true;
    CodeEditFindCase.Checked:=true;
    CodeEditFindTxt.HasFocus:=true;
    {$ifdef JScript}
    // if navigating here from a PasUnit sub-procedure on the code tree, then automatically run a search
    // for instances of the procedure name...
    if self.CodeEditFindTxt.ItemValue<>'' then
      DoGlobalSearch(self.CodeEditFindTxt.ItemValue);
    {$endif}
  end
  else
  begin
    CodeEdit.ContainerHeight:='95%';            // CodeEdit is % of the parent component (tabpage)
    CodeEditInitTab.IsVisible:=false;
    CodeEditFunctionResultType.IsVisible:=false;
    CodeEditInputsVBox.IsVisible:=false;
    if CodeEdit.MessageLines='' then
      CodeEdit.MessagesHeight:='1'
    else
      CodeEdit.MessagesHeight:='30%';
    CodeEditFindTxt.ItemValue:='';
  end;

  if CodeEditInitTab.IsVisible=true then
  begin
    // make the same messages visible on both tabs
    CodeEditInit.ContainerHeight:=CodeEdit.ContainerHeight;
    CodeEditInit.MessagesHeight:=CodeEdit.MessagesHeight;
    CodeEditInit.MessageLines:=CodeEdit.MessageLines;
  end;

  if (Mode<>'EventCode') then
    CodeEditMainTabs.TabIndex:=0;

  if (Mode<>'PythonScriptCode') then
    CodeEdit.Language:='Pascal'
  else
    CodeEdit.Language:='Python';

end;

{$ifndef JScript}
procedure TCodeEditForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);

  Initialise;
end;

procedure TCodeEditForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, CodeEditFormXVBox1);
  //!! fudge.... have to set minWidth as alClient fails to reset size of this panel....!!
  CodeEdit.myControl.Constraints.MinWidth:=CodeEdit.Width;
  CodeEdit.myControl.Constraints.MinHeight:=CodeEdit.Height;
end;

procedure TCodeEditForm.FormShow(Sender: TObject);
begin
  // scroll the messages to the bottom
  CodeEdit.TheMessages.VertScrollBar.Position:=CodeEdit.TheMessages.Lines.Count-1;
  if (Mode='dll') then
    NavigateToFirstError
  else if (Mode='SearchCode') then
  begin
    // if navigating here from a PasUnit sub-procedure on the code tree, then automatically run a search
    // for instances of the procedure name...
    if self.CodeEditFindTxt.ItemValue<>'' then
      DoGlobalSearch(self.CodeEditFindTxt.ItemValue);
    TEdit(CodeEditFindTxt.myControl).SetFocus;
  end;
end;

{$endif}

procedure TCodeEditForm.Initialise;
begin
  numInputs:=0;
  SetLength(MyInputs,0);
  SetLength(CheckedInputs,0);
  CodeFormRoot:=FindDataNodeById(myNode,'CodeEditFormXVBox1','',true);
  CodeEditFunctionResultType.OptionList:=Attributetypes;
  CodeEditFunctionResultType.ItemIndex:=0;

  TargetNodeName:='';
  EventType:='';
  self.CodeEditContextLabel.LabelCaption:='';
  CodeEdit.LabelText:='';
//  NumInputs:=0;
  DeleteNodeChildren(CodeEditParamListScrollBox.myNode);

  {$ifndef JScript}
  TXForm(self).Height:=trunc(MainForm.Height*90/100);
  TXForm(self).Width:=trunc(MainForm.Width*90/100);
  {$else}
  TXForm(self).Top:=50;
  TXForm(self).Left:=50;

  asm
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      var str='90%';
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',str);
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',str);
    }
  end;

  {$endif}

end;

procedure TCodeEditForm.AddInputBox(myInput:TCodeInputRec);
var
  HBoxNode:TDataNode;
  NewHBox:TXHBox;
  NewCheckBox:TXCheckBox;
  NewEditBox1, NewEditBox2:TXEditBox;
  NewButton:TXButton;
  ParentNode:TDataNode;
  BoxName,ItmValue:String;
begin
  numInputs:=numInputs+1;
  SetLength(myInputs,numInputs);
  myInputs[length(myInputs)-1]:=myInput;
  SetLength(CheckedInputs,numInputs);

  ParentNode:=CodeEditParamListScrollBox.myNode;
  BoxName:='CodeEditInputCombo_'+intToStr(numInputs);

  HBoxNode:=AddDynamicWidget('TXHBox',CodeEditForm,ParentNode,'HB'+BoxName,'','Left',-1);
  NewHBox:=TXHBox(HBoxNode.ScreenObject);
  NewCheckBox:=TXCheckBox(AddDynamicWidget('TXCheckBox',CodeEditForm,HBoxNode,'CB'+BoxName,'','Top',-1).ScreenObject);
  NewEditBox1:=TXEditBox(AddDynamicWidget('TXEditBox',CodeEditForm,HBoxNode,'EB1'+BoxName,'','Top',-1).ScreenObject);
  NewEditBox2:=TXEditBox(AddDynamicWidget('TXEditBox',CodeEditForm,HBoxNode,'EB2'+BoxName,'','Top',-1).ScreenObject);
  NewButton:=TXButton(AddDynamicWidget('TXButton',CodeEditForm,HBoxNode,'Btn'+BoxName,'','Top',-1).ScreenObject);


  {$ifndef JScript}
  NewHBox.BgColor:=HexRGBToColor('#BFCDDB');
  {$else}
  NewHBox.BgColor:='#DBCDBF';
  {$endif}

  NewHBox.Border:=false;

  NewCheckBox.LabelText:='';
  NewCheckBox.myNode.registerEvent('Click',@CodeEditForm.InputCheckBoxClick);

  NewEditBox1.ItemValue:=myInput.InputSynonym;
  NewEditBox1.LabelText:='';

  NewEditBox2.LabelText:='';
  NewEditBox2.ContainerWidth:='50%';
  NewEditBox2.BoxWidth:='100%';
  NewEditBox2.ReadOnly:=true;

  NewButton.Caption:='...';

  if myInput.InputNodeName<>'' then
    NewEditBox2.ItemValue:=myInput.InputNodeName+'.'+myInput.InputAttribName;

  NewEditBox1.myNode.registerEvent('Change',@CodeEditForm.EditInputSynonym);
  NewEditBox2.myNode.registerEvent('Change',@CodeEditForm.EditInputSource);
  NewButton.myNode.registerEvent('ButtonClick',@CodeEditForm.ShowInputSelector);
end;

procedure TCodeEditForm.InputCheckBoxClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  numstr:string;
  n:integer;
  bits:TStringList;
begin
  bits:=stringsplit(nodeId,'_');
  if bits.count>1 then
  begin
    numstr:=bits[1];
    n:=strtoint(numstr);
    CheckedInputs[n-1]:=MyStrToBool(myValue);
  end;
end;

procedure TCodeEditForm.EditInputSynonym(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  numstr:string;
  n:integer;
  bits:TStringList;
begin
  // index for myInputs always matches the integer suffix of the combo box name (-1).
  // Gaps due to deleted inputs are handled on form closure.
  bits:=stringsplit(nodeId,'_');
  if bits.count>1 then
  begin
    numstr:=bits[1];
    n:=strtoint(numstr);
    myInputs[n-1].InputSynonym:=myValue;
  end;
end;

procedure TCodeEditForm.EditInputSource(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  numstr:string;
  n:integer;
  bits,SourceBits:TStringList;
begin
  // index for myInputs always matches the integer suffix of the combo box name (-1).
  // Gaps due to deleted inputs are handled on form closure.
  bits:=stringsplit(nodeId,'_');
  if bits.count>1 then
  begin
    numstr:=bits[1];
    n:=strtoint(numstr);
    SourceBits:=StringSplit(myValue,'.');
    if SourceBits.Count=2 then
    begin
      myInputs[n-1].InputNodeName:=SourceBits[0];
      myInputs[n-1].InputAttribName:=SourceBits[1];
    end
    else
    begin
      myInputs[n-1].InputNodeName:=myValue;
    end;
  end;
end;

procedure TCodeEditForm.ShowInputSelector(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  numstr:string;
  n:integer;
  bits:TStringList;
begin
  // index for myInputs always matches the integer suffix of the combo box name (-1).
  // Gaps due to deleted inputs are handled on form closure.
  bits:=stringsplit(nodeId,'_');
  if bits.count>1 then
  begin
    numstr:=bits[1];
    n:=strtoint(numstr);
    InputSelectForm.TargetEditBoxNode:=FindDataNodeById(UIRootNode,'EB2'+'CodeEditInputCombo_'+intToStr(n),'',true);

    InputSelectForm.Initialise;
    ShowXForm('InputSelectForm',true);

    {$ifndef JScript}
    // on return...
    InputSelectForm.InputSelectClosed;
    {$endif}
  end;
end;

procedure TCodeEditForm.CodeEditHelpBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  ShowXForm('PopupMemoForm',false);
end;


procedure TCodeEditForm.CodeEditAddInputBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  emptyRec:TCodeInputRec;
begin
  // add an element to the CodeEditParamListScrollBox
  // The element is a ComboBox with avaliable options listing all system properties, and eligible functions.
  AddInputBox(emptyRec);
end;

procedure TCodeEditForm.CodeEditDelInputBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  i:integer;
  delNode:TDataNode;
begin
  //showmessage('not yet implemented');
  // delete all checked inputs
  for i:=0 to length(CheckedInputs)-1 do
  begin
    if CheckedInputs[i]=true then
    begin
      delNode:=FindDataNodeById(CodeFormRoot,'HBCodeEditInputCombo_'+IntToStr(i+1),'',true);
      DeleteNode(CodeEditParamListScrollBox.myNode,delNode);
      CheckedInputs[i]:=false;
      myInputs[i].InputNodeName:='';
      myInputs[i].InputAttribName:='';
    end;
  end;
end;

{$ifdef JScript}
Function GetErrorLineNum(ErrorString,Delimiter:string):integer;
  var   LinNumPos,linenum:integer;
        lineNumStr:string;
  begin
    lineNumStr:='-1';
    LinNumPos:=pos(Delimiter,ErrorString);
    if LinNumPos >0 then
      lineNumStr := rightstr(ErrorString,(1+length(errorstring)-length(Delimiter)-LinNumPos));
    Try
      linenum:= strtoint(lineNumStr);
    except
    On E : EConvertError do
      linenum:= -1;
    end;
    result:=linenum;
  end;
{$endif}

procedure TCodeEditForm.NavigateToFirstError;
var
  i:integer;
  FoundLineNum:Boolean;
  FileName,CharPos:string;
  LineNum:String;
  LNum:integer;
  tmp1:TStringList;
begin
  // search the messages list.  Look for a name(line,char) string.
  // If the line also contains keyword 'Error' then navigate to the indicated code block
  lNum:=-1;
  {$ifndef JScript}
  i:=0;
  while i<CodeEdit.TheMessages.lines.count do
  begin
    if FoundString(CodeEdit.TheMessages.lines[i],'Error')>0 then
    begin
      CodeEdit.GetFileNameLineNumAndCharPos(FoundLineNum,CodeEdit.TheMessages.lines[i], '(',FileName,LineNum,CharPos );
      if (trim(FileName)<>'')
      and (lineNum<>'') then
      begin
        LNum:=i;
        i:=CodeEdit.TheMessages.lines.count;
      end;
    end;
    i:=i+1;
  end;
  {$else}
  tmp1:=TStringList.Create;
  tmp1.Text:=CodeEdit.MessageLines;
  i:=0;
  while i<tmp1.count do
  begin
    if FoundString(tmp1[i],'Error')>0 then
    begin
      CodeEdit.GetFileNameLineNumAndCharPos(FoundLineNum,tmp1[i], '(',FileName,LineNum,CharPos );
      if (trim(FileName)<>'')
      and (lineNum<>'') then
      begin
        LNum:=i;
        i:=tmp1.count;
      end;
    end;
    i:=i+1;
  end;
  FreeAndNil(tmp1);
  {$endif}
  if lNum>-1 then
  begin
    DisplayIncFile(Filename,lineNum,CharPos);
  end;
end;

procedure TCodeEditForm.SetCursorPosition(targetLine,targetChar:integer);
begin
  // showmessage('targetline='+inttostr(targetline));
  // set cursor position
  if (targetChar>-1) then
    if CodeEditMainTabs.TabIndex=0 then
      CodeEdit.GoToLineCharPos(targetLine,targetChar)
    else
      CodeEditInit.GoToLineCharPos(targetLine,targetChar);
end;

procedure TCodeEditForm.DisplayIncFile(FileName,lineNum,charPos:String);
var
  tmp1:TStringList;
  FStrings,FStrings2:TStringList;
  TargetNode:TDataNode;
  targetLine:integer;
  Context:String;
begin
  {$ifndef JScript}
    tmp1:=LoadIncludeFile(nil,FileName,'tempinc/');
  {$else}
    tmp1:=LoadIncludeFile(myWebCompiler.Compiler,FileName,'tempinc/');
  {$endif}

  if tmp1.Count>0 then
  begin
    CodeEdit.ItemValue:=tmp1.Text;
    CodeEditInit.ItemValue:='';
    FreeAndNil(tmp1);

    self.TargetNodeName:='';
    self.EventType:='';
    //filename is NodeName + EventType
    FStrings2:=StringSplit(FileName,'.');
    if FStrings2.Count=2 then
    begin
      if FStrings2[1]='inc' then
      begin
        // could be a function or an event handler
        FStrings:=StringSplit(FStrings2[0],'__');
        if FStrings.Count>=2 then
        begin
          self.TargetNodeName:=FStrings[0];
          self.EventType:=FStrings[1];
          self.Mode:='EventCode';
          Context:='Event Handler';

          CodeEditInitTab.IsVisible:=true;

          // Load up both the main event code AND the initialisation code...
          TargetNode:=FindDataNodeById(SystemNodeTree,self.TargetNodeName,'',true);
          CodeEditInit.ItemValue:=TargetNode.GetEventInitCode(self.EventType);

          CodeEdit.ItemValue:=TargetNode.GetEventCode(self.EventType);
          targetLine:=StrToInt(linenum)-1;
          if FoundString(FileName,'Init.')>0 then
             CodeEditMainTabs.TabIndex:=1;
        end
        else
        begin
          self.TargetNodeName:=FStrings[0];
          self.Mode:='FunctionCode';
          Context:='Function';
          targetLine:=StrToInt(linenum);
        end;

        //self.CodeEditContextLabel.LabelCaption:=self.TargetNodeName+' '+self.EventType;
        //CodeEdit.LabelText:=self.TargetNodeName+' '+self.EventType;
        FreeAndNil(FStrings);
      end
      else
      begin
        self.TargetNodeName:=FStrings2[0];
        self.EventType:='';
        self.Mode:='UnitCode';
        Context:='PasUnit';
        //self.CodeEditContextLabel.LabelCaption:=self.TargetNodeName;
        //CodeEdit.LabelText:=self.TargetNodeName;
        targetLine:=StrToInt(linenum);
      end;
    end;
    FreeAndNil(FStrings2);
    self.InitialiseOnShow(Context,self.TargetNodeName,self.EventType);
    SetCursorPosition(targetLine,strToInt(charPos));
  end
  else
  begin
    showmessage('This file is not available for edit');
  end;
  FreeAndNil(tmp1);
end;

procedure TCodeEditForm.HandleMessageClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString; ThisCodeEdit:TXCode);
var linenumber,targetLine:integer;
    SelectedLine,FileName,CharPos:string;
    FoundLineNum:Boolean;
    FStrings:TStringList;
    tmp,LineNum,Context:String;
    TargetNode:TDataNode;
    Messages:TStringList;
begin
  //showmessage('CodeEdit HandleClickMessage '+ nodeID + ' '+myValue);
 {$ifndef JScript}
 linenumber:=thisCodeEdit.TheMessages.CaretPos.Y;
 SelectedLine:= thisCodeEdit.TheMessages.lines[linenumber];
 thisCodeEdit.GetFileNameLineNumAndCharPos(FoundLineNum,SelectedLine, '(',FileName,LineNum,CharPos );
 {$else}
 // Find the message line thats been clicked on
 try
 linenumber:=StrToInt(myValue);
 except
   On E : EConvertError do
   EXIT;
 end;
 Messages:=TStringList.Create;
 Messages.Text:=thisCodeedit.MessageLines;
 //showmessage('linenumber='+inttostr(linenumber)+' count='+inttostr(Messages.Count));
 if linenumber>Messages.Count then
   EXIT;
 SelectedLine:= Messages[linenumber-1];

 // Find the indicated line number from the message
 thisCodeEdit.GetFileNameLineNumAndCharPos(FoundLineNum,SelectedLine, '(',FileName,LineNum,CharPos );
 //showmessage('FileName='+FileName+' Indicated Linenum is '+linenum);
 {$endif}

 CodeEditMainTabs.TabIndex:=0;

 //load the indicated file from the tempinc folder into the code edit box
 if (trim(FileName)<>'')
 and (lineNum<>'')
 and (charPos<>'') then
 begin
   FStrings:=StringSplit(FileName,'.');
   if ((FStrings.Count=2)
     and (FStrings[1]='inc'))
   or (CodeEditForm.Mode = 'dll') then
   begin
     DisplayIncFile(FileName,lineNum,CharPos);
   end
   else
   begin
     //if Filename includes a dot, then it's a nodename and eventtype.
     //if it contains .AnimationCode, then it's a GPU kernel block
     //Otherwise it's a code element (function or Pasunit or PythonScript nodename)

     // save any edits already done...
     CodeEditStatus:='ok';
     if CodeEditForm.Mode<>'SearchCode' then
       CodeEditorClosed(nil);

     if FStrings.Count>1 then
     begin
       self.TargetNodeName:=FStrings[0];
       targetNode:=FindDataNodeByid(systemNodeTree,self.TargetNodeName,'',false);
       if targetNode<>nil then
       begin
         if (FStrings.Count=3)
         and (FStrings[2]='AnimationCode') then
         begin
           self.Mode:='AnimationCode';
           Context:=targetNode.NodeType;
           self.EventType:='';
           CodeEditInit.ItemValue:='';
           CodeEdit.ItemValue:='';
           CodeEditMainTabs.TabIndex:=0;
         end
         else if FStrings.Count>2 then
         begin
           self.EventType:=FStrings[1];
           self.Mode:='EventCode';
           Context:='Event Handler';
           CodeEditInit.ItemValue:=TargetNode.GetEventInitCode(self.EventType);
           CodeEdit.ItemValue:=TargetNode.GetEventCode(self.EventType);
           if FStrings[2]='EventInitCode' then
           begin
             CodeEditMainTabs.TabIndex:=1;
           end
           else
           begin
             CodeEditMainTabs.TabIndex:=0;
           end;
         end
         else
         begin
           if targetNode.NodeType='PasUnit' then
             self.Mode:='PasUnitCode'
           else if targetNode.NodeType='Function' then
             self.Mode:='FunctionCode';
           Context:=targetNode.NodeType;
           self.EventType:='';
           CodeEditInit.ItemValue:='';
           CodeEdit.ItemValue:=TargetNode.GetAttribute('Code',true).AttribValue;
           CodeEditMainTabs.TabIndex:=0;
         end;
       end;
     end;

     targetLine:=StrToInt(linenum)+1;
     if self.Mode<>'AnimationCode' then
     begin
       self.InitialiseOnShow(Context,targetNode.NodeName,self.EventType);
       SetCursorPosition(targetLine,strToInt(charPos));
     end
     else
     begin
       // GPU Animation code has a separate editor form...
       ShowGPUKernel(targetNode,FileName,targetLine,CharPos);

     end;
   end;

   FreeAndNil(FStrings);
 end;
end;

procedure TCodeEditForm.CodeEditHandleClickMessage(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  self.HandleMessageClick(e,nodeID,myValue,CodeEdit);
end;

procedure TCodeEditForm.CodeEditInitHandleClickMessage(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  self.HandleMessageClick(e,nodeID,myValue,CodeEditInit);
end;

procedure TCodeEditForm.CodeEditMainTabsHandleChange(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  if CodeEditMainTabs.TabIndex = 1 then
    if CodeEditForm.CodeEditInit.ItemValue = '' then
      CodeEditForm.CodeEditInit.ItemValue := DfltEventCode;
end;

procedure TCodeEditForm.DoGlobalSearch(TextToFind:String);
  {$ifndef JScript}
  procedure SearchThisText(TheName,TheType,TheText:String);
  var
    lineNumber: integer;
    TempMemo:TMemo;
    atPos:integer;
  begin
     TempMemo:=TMemo.Create(nil);
     TempMemo.Lines.Text:=TheText;
      for lineNumber := 0 to TempMemo.lines.count-1 do
      begin
        if self.CodeEditFindCase.Checked then
          atPos:=Pos( UpperCase(TextToFind), UpperCase(TempMemo.lines[lineNumber]) )
        else
          atPos:=Pos( TextToFind, TempMemo.lines[lineNumber] );
        if atPos > 0 then
          //ShowMessage( 'Found in line ' + IntToStr(lineNumber) );
          //self.CodeEdit.TheMessages.Append(TheName+' : '+TheType+' : '+TempMemo.lines[lineNumber]);
          self.CodeEdit.AddMessage(TheName+'.'+TheType+'('+inttostr(lineNumber)+','+inttostr(atPos)+') '+TempMemo.lines[lineNumber]);
      end;
  end;
  {$else}
  procedure SearchThisText(TheName,TheType,TheText:String);
  var
    lines:TStringList;
    i,p:integer;
  begin
    lines:=TStringList.Create;
    lines.Text:=TheText;
    for i:=0 to lines.Count-1 do
    begin
      if self.CodeEditFindCase.Checked then
        p:=FoundStringCI(lines[i], TextToFind)
      else
        p:=FoundString(lines[i], TextToFind);
      if p>0 then
        self.CodeEdit.AddMessage(TheName+'.'+TheType+'('+inttostr(i)+','+inttostr(p)+') '+lines[i]);
    end;
  end;
  {$endif}

  procedure SearchEventsCode(CurrentItem:TDataNode);
  var dflt:String;
      i,k,numchildren:integer;
      AllKernels:TAnimCodeArray;
  begin
    dflt:=DfltEventCode;
    if CurrentItem<>nil then
    if CurrentItem.NameSpace='' then
    begin
        numchildren:=length(CurrentItem.ChildNodes);
        for i:=0 to CurrentItem.myEventTypes.Count-1 do
        begin
          if (CurrentItem.HasUserEventCode(CurrentItem.myEventTypes[i]))
          and (CurrentItem.GetEventCode(CurrentItem.myEventTypes[i])<>dflt) then
          begin
             SearchThisText(CurrentItem.nodeName+'.'+CurrentItem.myEventTypes[i],
                            'EventCode',
                            CurrentItem.GetEventCode(CurrentItem.myEventTypes[i]));
             SearchThisText(CurrentItem.nodeName+'.'+CurrentItem.myEventTypes[i],
                            'EventInitCode',
                            CurrentItem.GetEventInitCode(CurrentItem.myEventTypes[i]));
          end;
        end;
        if (CurrentItem.NodeType='TXGPUCanvas') then
        begin
          AllKernels:=TXGPUCanvas(CurrentItem.ScreenObject).FetchAllAnimCode;
          for k:=0 to length(AllKernels)-1 do
          begin
            SearchThisText(CurrentItem.nodeName+'.'+inttostr(k),'AnimationCode',AllKernels[k].CodeBlock.Text);
          end;
        end;

        for i:=0 to numchildren-1 do
        begin
          SearchEventsCode(CurrentItem.ChildNodes[i]);
        end;

    end;
  end;
  procedure SearchCodeNode(ThisNode:TdataNode);
  var
    i:integer;
  begin
    if (ThisNode.NodeType='PasUnit')
    or (ThisNode.NodeType='PythonScript')
    or (ThisNode.NodeType='Function') then
    begin
      SearchThisText(ThisNode.NodeName,ThisNode.NodeType,ThisNode.GetAttribute('Code',true).AttribValue);
    end;
    for i:=0 to length(ThisNode.ChildNodes)-1 do
      SearchCodeNode(ThisNode.ChildNodes[i]);
  end;

begin
  // text blocks to be searched....
  //   Raw Units
  //   Functions
  //   Event handlers
  // search through each text block, listing name, line number and line text for each found occurrence
  // (list found elements in messages block)
  //!! save any changes first....??
  CodeEditStatus:='ok';
  if CodeEditForm.Mode<>'SearchCode' then
  begin
    CodeEditorClosed(nil);
    CodeEditForm.Mode:='SearchCode';
    CodeEditForm.InitialiseOnShow('Search Results','','');
  end;

  self.CodeEdit.MessageLines:='';
  self.CodeEdit.ItemValue:='';
  SearchCodeNode(CodeRootNode);
  SearchEventsCode(UIRootNode);

end;

procedure TCodeEditForm.DoLocalSearch(TextToFind:String);
var
    found:integer;
    {$ifndef JScript}
    opts:TSynSearchOptions;
    {$else}
    txt,qual:String;
    {$endif}
begin
    {$ifndef JScript}
    if self.CodeEditFindCase.Checked then
      opts := [ssoEntireScope]
    else
      opts:= [ssoEntireScope,ssoMatchCase];
    found:=self.CodeEdit.TheEditor.SearchReplace(TextToFind,'',opts);

    {$else}
    qual:='g';
    if self.CodeEditFindCase.Checked then
      qual:='gi';
    txt:=TextToFind;
    asm
    var textfield=document.getElementById('CodeEditContentsReal');
    //var regex=new RegExp(txt, "gi");
    var regex=new RegExp(txt, qual);
    this.RegExp=regex;

    var ok=regex.test(textfield.value);
    //alert('test regex.lastIndex='+regex.lastIndex);

    if (!ok)
      {found=-1;}
    else {
      found=regex.lastIndex;  }

    //alert('search(regex) '+regex+' found='+found);
    this.LastSearchIndex=found;
    end;
    if found>-1 then
      self.CodeEdit.GoToCharPos(found-length(txt));
    self.LastSearchIndex:=found;
    found:=found+1;
    {$endif}
  if found<1 then
    showmessage('Not found');

end;

procedure TCodeEditForm.CodeEditFindBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  if self.CodeEditFindTxt.ItemValue='' then
    showmessage('Please enter text to be found')
  else
  begin
    if CodeEditFindGlobal.Checked=false then
      DoLocalSearch(self.CodeEditFindTxt.ItemValue)
    else
      DoGlobalSearch(self.CodeEditFindTxt.ItemValue);
  end;
end;
procedure TCodeEditForm.CodeEditFindNextBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
    found,startidx:integer;
    {$ifndef JScript}
    opts:TSynSearchOptions;
    {$else}
    txt:String;
    {$endif}
begin
  if self.CodeEditFindTxt.ItemValue='' then
    showmessage('Please enter text to be found')
  else
  begin
    {$ifndef JScript}
    if self.CodeEditFindCase.Checked then
      opts := [ssoFindContinue]
    else
      opts:= [ssoFindContinue,ssoMatchCase];
    found:=self.CodeEdit.TheEditor.SearchReplace(self.CodeEditFindTxt.ItemValue,'',opts);
    {$else}
    txt:=self.CodeEditFindTxt.ItemValue;
    startidx:=self.LastSearchIndex;
    asm
    if ((this.RegExp==null)||(this.RegExp==undefined)) {
      alert('Please use "Find" first'); }
    else {
      var regex=this.RegExp;
      regex.lastIndex=startidx;
      var textfield=document.getElementById('CodeEditContentsReal');
      var ok=regex.test(textfield.value);
      if (!ok)
        {found=-1;}
      else {
        found=regex.lastIndex;  }
      //alert('test regex.lastIndex='+regex.lastIndex);

      this.LastSearchIndex=found;
      //alert('regex '+regex+' found='+found);
      }
    end;
    if found>-1 then
      self.CodeEdit.GoToCharPos(found-length(txt));
    found:=found+1;
    {$endif}
    if found<1 then
      showmessage('No further occurrences');

  end;
end;


function TCodeEditForm.ValidateInputs:Boolean;
var
  ok:Boolean;
  i:integer;
begin
  ok:=true;
  for i:=0 to length(MyInputs)-1 do
    if trim(myInputs[i].InputNodeName)<>'' then
    begin
      if trim(myInputs[i].InputSynonym)='' then
        ok:=false;
    end;
  if ok=false then
    showmessage('Please provide a Synonym for each declared input');

  result:=ok;
end;

procedure TCodeEditForm.CodeEditSaveBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
    XFormNode:TDataNode;
    ok:Boolean;
begin
  ok:=ValidateInputs;
  if ok then
  begin
    CodeEditStatus:='ok';
    TXForm(self).Showing:='No';
       CodeEditorClosed(OIEditBox);
  end;
end;

procedure TCodeEditForm.CodeEditCancelBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
    XFormNode:TDataNode;
begin
  CodeEditStatus:='Cancel';
  TXForm(self).Showing:='No';
  {$ifndef JScript}
  {$else}
  OIEditBox:=nil;
  {$endif}
end;


end.

