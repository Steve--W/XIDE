(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit EventLogging;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}
interface
uses
  SysUtils,Classes, StringUtils, NodeUtils, EventsInterface,TypInfo,
  Events, XForm, XTree, XComboBox, XTabControl, XMenu, ReplayUserDialog,
  MacroComment, PasteDialogUnit,
  {$ifndef JScript}
  Dialogs, Forms, Controls, ComCtrls, MouseAndKeyInput, LazsUtils,
  UtilsJSCompile;
  {$Else}
  HTMLUtils;
  {$endif}

const fieldDelimiter:string = '%^&';
      eventDelimiter:string = '&^%';

{$ifndef JScript}
type  TmouseAnimParams = record
  Shift:TShiftState;
  ScreenX:integer;
  ScreenY:integer;
  duration:integer;
end;
  PmouseAnimParams = ^TmouseAnimParams;
type THelperProcs = class(TObject)
   procedure CallMove(Data:PtrInt);
end;
{$endif}

type

  TMacroEvent = record
    EventType:String;
    NodeId:String;
    eventvalue:string;
  end;

  TMacroEvents= Array of TMacroEvent;

  TMacroEventList = class(TObject)
      MacroEvents:TMacroEvents;
      RecordingEnabled:boolean;
      ReplayEnabled:boolean;
      Replaying:Boolean;
      Eventcount:integer;
      CurrentEvent:integer;     // used during replay
      CompletedEvent:integer;   // used during replay
      initialised:Boolean;
      StartState:String;
      constructor create;
  //    function CheckForResumeFromLocalStorage:boolean;
      procedure add(e:TMacroEvent);
      function  ToMyString:string;
      procedure FromMyString(instring:String);
      procedure clear;
      procedure SetMenuVisibility;
  //    procedure ContinueReplay;
   end;

procedure TrapEventForMacro(e:TEventStatus);
procedure StartRecording;
procedure StopRecording;
procedure ResumeMacroRecording;
procedure StartReplay;
procedure FinishReplay;
procedure InsertComment;
procedure SaveMacroToClip;
procedure DoMacroLoad(MacroString:String);
procedure LoadMacroFromClip(e:TEventStatus;nodeId:string);
function ReplayUserInput(promptText:String):String;
procedure DoNextEvent(idx:integer);
function AdvanceEventLog:TMacroEvent;
procedure SetCompletedEvent(idx:integer);
{$ifndef JScript}
procedure  ReplayEvent(e:TEventStatus;EventType,nodeID,NameSpace,myValue:PChar);
{$else}
procedure CheckEventDone(i,waitcount:integer);
{$endif}
procedure ReplayEvent(e:TEventStatus;EventType,nodeID,NameSpace,myValue:String);
procedure MovePointer(NameSpace,NodeName1,NodeName2,Sub1,Sub2:String;drag:Boolean);

var
    MacroEventList:TMacroEventList;
    LoggingLabelNode:TDataNode;
    {$ifndef JScript}
    Helperprocs:THelperProcs;
    {$else}
    MyTimeoutFunc:String;
    {$endif}

implementation
uses XObjectInsp;


function StringToSubStringList(InString,delimiter:String):TStringList;
var items : TStringList;
begin
//showmessage('splitting >'+Instring+'<');
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  items.LineBreak:=delimiter;
  items.text:= InString;
//showmessage('bitcount='+inttostr(items.count));
  StringToSubStringList:=items;
end;


function stringsplit(str:string; separator:string):TStringList;
var
   localStringList:TStringList;
begin
  localStringList:=StringToSubStringList(str,separator);
  result:=localStringList;
end;

procedure ReplayEvent(e:TEventStatus;EventType,nodeID,NameSpace,myValue:string);
// this procedure mirrors the 'user' actions required on UI widgets prior to
// re-executing the relevant event handler.

  procedure SetAStringproperty(Widget:TObject;PropName,NewValue:String);
  begin
    {$ifndef JScript}
    SetStrProp(Widget,PropName,NewValue);
    {$else}
    SetStringProp(Widget,PropName,NewValue);
    {$endif}
  end;
  procedure SetAIntegerproperty(Widget:TObject;PropName:String;NewValue:integer);
  begin
    {$ifndef JScript}
    SetOrdProp(Widget,PropName,NewValue);
    {$else}
    SetNativeIntProp(Widget,PropName,NewValue);
    {$endif}
  end;

  var
    eventNode:TDataNode;
    handled:Boolean;
    i:integer;
    bits:TStringList;
    NodeObj:TNodeEventValue;
    {$ifndef JScript}
    MyRecPtr: PNodeDataRec;
    {$else}
    ob:TObject;
    {$endif}
  begin
//    showmessage('ReplayEvent '+EventType+' '+nodeID+' '+myValue);
    handled:=false;
    if NodeId<>'' then
    begin
      eventNode:=FindDataNodeById(SystemNodeTree,NodeId,NameSpace,true);
      // depending on the type of node, and the event type, we may need to set the state of the widget.
      if EventType='Change' then
      begin
        if (eventNode.NodeType='TXEditBox')
        or (eventNode.NodeType='TXCode')
        or (eventNode.NodeType='TXColorPicker')
        or (eventNode.NodeType='TXDatePicker')
        or (eventNode.NodeType='TXMemo')
        or (eventNode.NodeType='TXNumberSpinner')
        or (eventNode.NodeType='TXNumericSlider')
        or (eventNode.NodeType='TXRadioBtns')
        then
        begin
          {$ifndef JScript}
          if (eventNode.NodeType<>'TXEditBox')  //desktop: not handled - this will not raise a new change event, so still need to call the change handler
          then
            handled:=true;
          {$else}
          if (eventNode.NodeType<>'TXEditBox')  //browser: not handled - this will not raise a new change event, so still need to call the change handler
          and (eventNode.NodeType<>'TXRadioBtns')
          then
            handled:=true;
          {$endif}
          SetAStringproperty(eventNode.ScreenObject,'ItemValue',myValue);
        end
        else if (eventNode.NodeType='TXComboBox')  then
        begin
          //handled:=true;
          i:=TXComboBox(eventNode.ScreenObject).IndexOfOption(myValue);
          SetAIntegerproperty(eventNode.ScreenObject,'ItemIndex',i);
        end
        else if (eventNode.NodeType='TXTabControl') then
        begin
          //handled:=true;
          {$ifndef JScript}
          i:=TXTabControl(eventNode.ScreenObject).IndexOfPage(myValue);
          SetAIntegerproperty(eventNode.ScreenObject,'TabIndex',i);
          {$else}
          ChangeTabPage(myValue,NodeId,NameSpace);
          {$endif}
        end
        else if (eventNode.NodeType='TXTable') then
        begin
          handled:=true;
          SetAStringproperty(eventNode.ScreenObject,'TableData',myValue);
        end;
      end
      else if EventType='Click' then
      begin
        if (eventNode.NodeType='TXTable') then
        begin
          // Click needs to set SelectedRow, column...
          // event data is <row>,<col>
          bits:=TStringList.Create;
          bits.StrictDelimiter:=true;
          bits.Delimiter:=',';
          bits.Text:=myValue;
          if bits.count=2 then
          begin
            SetAIntegerproperty(eventNode.ScreenObject,'SelectedRow',StrToInt(bits[0]));
            SetAIntegerproperty(eventNode.ScreenObject,'SelectedCol',StrToInt(bits[1]));
          end;
          bits.Free;
        end;
      end

      else if EventType='ButtonClick' then
      begin
        {$ifndef JScript}
        {$else}
        if (eventNode.NodeType='TXButton') then
        begin
          //handled:=true;
          asm
            ob=document.getElementById(myValue+'Contents');
            if (ob!=null) {
              // visual click...
              //ob.style.WebkitAnimationPlayState = "running";   // "paused";
              //ob.className = current[0].className.replace(" active", "");
              //ob.focus();
              ob.className += " replayButton";
              myTimeout(function(){pas.HTMLUtils.removeClassName(ob, "replayButton");}, 300, 'replayButton',0);
            }
          end;
        end;
        {$endif}
      end
      else if EventType='TreeNodeClick' then
      begin
        if (eventNode.NodeType='TXTree') then
        begin
          // TreeNodeClick needs to set the selected node in the tree
          // event data is node id (eg DETAILS element in HTML)
          SetAStringproperty(eventNode.ScreenObject,'SelectedNodeId',myValue);
        end;
      end
      else if EventType='DragStart' then
      begin
        if (eventNode.NodeType='TXTree') then
        begin
          {$ifndef JScript}
          handled:=true;  //!!!! tbd
          {$else}
          DraggingTree:=TXTree(eventNode.ScreenObject);
          DraggingTree.NodeBeingDragged:=myValue;
          {$endif}
        end;

      end
      else if EventType='Drop' then
      begin
        if (eventNode.NodeType='TXTree') then
        begin
          {$ifndef JScript}
          //!!!!
          {$else}
          if DraggingTree<>nil then
          begin
            // set up 'e' data
            NodeObj:=TNodeEventValue.Create;
            e:=TEventStatus.Create('Drop',nodeId);
            e.NameSpace:=NameSpace;
            asm
              ob=document.getElementById(myValue+'Summary');
              if (ob!=null) {
                NodeObj.DstText=ob.innerHTML; //TEXT of node being dropped on
              }
            end;
            NodeObj.myTree:=eventNode.ScreenObject;
            NodeObj.SrcText:=DraggingTree.NodeBeingDragged;
            NodeObj.SourceName:=DraggingTree.myNode.NodeName;
            NodeObj.myNode:=ob;   //!!!! the summary node being dropped on
            e.ValueObject:=NodeObj;
          end;
          {$endif}
        end;
      end;
    end;

    if not handled then
    begin
      {$ifndef JScript}
      handleEvent(e,EventType,nodeId,NameSpace,myValue);
      {$else}
      // timeout here basically (eg.) because TreeNodeClick opens the tree node on a timeout, and we want to see
      // those changes on screen before continuing.
      asm
        myTimeout(pas.Events.handleEvent,5,'handleEvent',0,e,EventType,nodeID,NameSpace,myValue);
      end;
      {$endif}

    end;
end;
{$ifndef JScript}
procedure  ReplayEvent(e:TEventStatus;EventType,nodeID,NameSpace,myValue:PChar);
begin
  ReplayEvent(e,EventType,nodeID,NameSpace,StrPas(myValue));
end;
{$endif}

procedure MovePointer(NameSpace,NodeName1,NodeName2,Sub1,Sub2:String;drag:Boolean);
  var
    Node1,Node2:TDataNode;
    {$ifndef JScript}
    SelectedControl1,SelectedControl2:TControl;
    PmouseAnimInfo1,PmouseAnimInfo2: PmouseAnimParams;
    P:TPoint;
    function FindTreeNodePos(TreeNode:TDataNode; id:String):TPoint;
      var
        theTree:TXTree;
        theNode:TTreeNode;
        lRect: TRect;
        lPoint: TPoint;
    begin
       theTree:=TXTree(TreeNode.ScreenObject);
       theNode:=TMyTreeView(theTree.myControl).FindNodeById(id);
       if theNode<>nil then
       begin
         lRect := theNode.DisplayRect(true);
         lPoint.X := lRect.Left;
         lPoint.Y := lRect.Bottom;
         lPoint := TTreeView(theTree.myControl).ClientToScreen(lPoint);
       end;
       result:=lPoint;
    end;
    {$endif}

begin
  {$ifndef JScript}
  Node1:=findDataNodeById(SystemNodeTree,NodeName1,NameSpace,true);
  Node2:=findDataNodeById(SystemNodeTree,NodeName2,NameSpace,true);

  if (Node1<>nil) and (Node2<>nil)
  and (Node1.MyForm = Node2.myForm)
  and (Node1.NodeClass='UI') and (Node2.NodeClass='UI')
  then
  begin
     // change the cursor to the pointy finger cursor during the move
     Node1.MyForm.Cursor := crHandPoint;       //!!!! not working

     new(PmouseAnimInfo1);
     PmouseAnimInfo1^.duration:=500;
     PmouseAnimInfo1^.Shift:=[];

     if (Node1.NodeType= 'TXTree')
     and (sub1<>'') then
     begin
        P:=FindTreeNodePos(Node1,sub1);
        PmouseAnimInfo1^.ScreenX:=P.X;
        PmouseAnimInfo1^.ScreenY:=P.Y;
        PmouseAnimInfo1^.ScreenX:=-1;
        PmouseAnimInfo1^.ScreenY:=-1;
     end
     else
     begin
       // find the lazarus controls handles
       SelectedControl1:=TControl(Node1.screenobject);

       // if there is an inner component, use that...
       if HasProperty(SelectedControl1,'myControl') then
         SelectedControl1:=TControl(GetObjectProp(SelectedControl1,'myControl'));

       if (SelectedControl1.Visible=true)
       and (SelectedControl1.ClassName<>'TXMenuItem') then
       begin
         P:=SelectedControl1.ClientToScreen(Point(0,0));
         //showmessage(inttostr(P.X)+' '+inttostr(P.Y));
         PmouseAnimInfo1^.ScreenX:=P.X+trunc(SelectedControl1.width/2);
         PmouseAnimInfo1^.ScreenY:=P.Y+trunc(SelectedControl1.height/2);
       end;
     end;

     if PmouseAnimInfo1^.ScreenX>-1 then
     begin
       //showmessage('move to '+inttostr(PmouseAnimInfo1^.ScreenX)+','+inttostr(PmouseAnimInfo1^.ScreenY));
       // move the mouse instantly to the middle of the first control on the form
       Application.QueueAsyncCall(@HelperProcs.CallMove,PtrInt(PmouseAnimInfo1));
     end;

     new(PmouseAnimInfo2);
     PmouseAnimInfo2^.duration:=500;
     PmouseAnimInfo2^.Shift:=[];
     PmouseAnimInfo2^.ScreenX:=-1;
     PmouseAnimInfo2^.ScreenY:=-1;

     if (Node2.NodeType= 'TXTree')
     and (sub2<>'') then
     begin
        P:=FindTreeNodePos(Node2,sub2);
        PmouseAnimInfo2^.ScreenX:=P.X;
        PmouseAnimInfo2^.ScreenY:=P.Y;
     end
     else
     begin
       SelectedControl2:=TControl(Node2.screenobject);

       if HasProperty(SelectedControl2,'myControl') then
         SelectedControl2:=TControl(GetObjectProp(SelectedControl2,'myControl'));

       if SelectedControl2.Visible
       and (SelectedControl2.ClassName<>'TXMenuItem') then
       begin
         P:=SelectedControl2.ClientToScreen(Point(0,0));
         PmouseAnimInfo2^.ScreenX:=P.X+trunc(SelectedControl2.width/2);
         PmouseAnimInfo2^.ScreenY:=P.Y+trunc(SelectedControl2.height/2);
       end;
     end;

     if PmouseAnimInfo2^.ScreenX>-1 then
     begin
       //showmessage('move to '+inttostr(PmouseAnimInfo2^.ScreenX)+','+inttostr(PmouseAnimInfo2^.ScreenY));
       // move the mouse slowly to the middle of the second control on the form
       Application.QueueAsyncCall(@HelperProcs.CallMove,PtrInt(PmouseAnimInfo2));
     end;

   end;

  {$else}

     asm

      function CursorMove(x1,y1,x2,y2,drag) {
           // This function will change the anim
           // Find your animation rule by name
    	var ss = document.styleSheets[0];
    	var anim;
            try {
              for (var i=0; i<ss.cssRules.length; i++) {
                   //alert('i='+i+' name='+ss.cssRules[i].name);
    		if (ss.cssRules[i].name === 'anim') {
    		  anim = ss.cssRules[i];
    		  break;
    	        }
                  }
               if (anim==null) {alert('did not find anim');}
               else {
       	      var SelectedcssRule1 = anim.cssRules[0]; // This indicates the first line of "anim" above.
    	      var SelectedcssRule2 = anim.cssRules[1]; // This indicates the second line of "anim" above.
    	      // Change the attributes
    	      SelectedcssRule1.style.cssText = ' top: '+y1+'px; '+' left: '+x1+'px; ';
    	      SelectedcssRule2.style.cssText = ' top: '+y2+'px; '+' left: '+x2+'px; ';
               }
            }
            catch(err) { alert('Error '+ err.message); }
    }
      function ACTransEnd(event) {
        // NB. this doesn't fire if the animation is interrupted
        // Next event replay needs to wait for this animation to complete.
        AnimationWaiting=false;
      }

      function DoAnimation(){
         // run the new animation by re-creating the AutomatedCursor div...
         var elm = document.getElementById("AutomatedCursor");
      	   var newone = elm.cloneNode(true);
      	   elm.parentNode.replaceChild(newone, elm);
         newone.id = "AutomatedCursor";
         if (drag==true) {
           newone.innerHTML = "&#128188;";
         }
         else {
           newone.innerHTML = "&#9756;";
         }
         AnimationWaiting=true;
         newone.addEventListener("animationend", ACTransEnd);
      }

       function Point(x, y) {
          this.x = x;
          this.y = y;
        }
       function GetElementXY(SelectedControlName) {
         var SelectedElement = document.getElementById(SelectedControlName);
         var rect = SelectedElement.getBoundingClientRect();
         console.log('GetElementXY('+SelectedControlName+') rect='+rect.top+','+rect.left+' '+rect.width+','+rect.height);
         var p = new Point(Math.trunc(rect.left + rect.width/2), Math.trunc(rect.top + rect.height/2));
         return p;
       }

       console.log(' CursorMove '+NodeName1+':'+Sub1+' to '+NodeName2+':'+Sub2);
       var ptr = document.getElementById("AutomatedCursor");
       ptr.style.display = "block";

       if (Sub1=='') {
         // Find the centre-point of the given element
         var p1=GetElementXY(NodeName1);
       }
       else {
         // Find the centre-point of the given tree node
         p1=GetElementXY(Sub1);
       }
       if (Sub2=='') {
         // Find the centre-point of the given element
         var p2=GetElementXY(NodeName2);
       }
       else {
         // Find the centre-point of the given tree node
         p2=GetElementXY(Sub2);
       }
       if ((p2.x==0)&&(p2.y==0)) {p2.x=p1.x; p2.y=p1.y}   //!! sometimes the returned rect is zero...
       console.log('cursormove '+p1.x+','+p1.y+' to '+p2.x+','+p2.y);
       CursorMove(p1.x,p1.y,p2.x,p2.y,drag);
       DoAnimation();
     end;
  {$endif}
end;

procedure HidePointer;
begin
  {$ifndef JScript}
  // not relevant in the Lazarus/desktop environment
  {$else}
 asm
   var ptr = document.getElementById("AutomatedCursor");
   ptr.style.display = "none";
 end;
  {$endif}
end;

procedure SetCompletedEvent(idx:integer);
begin
   MacroEventList.CompletedEvent:=idx;
end;

procedure DoNextEvent(idx:integer);
var p1,p2,p3:string;
    TempEvent:TMacroEvent;
    StartTime,EndTime,NewTime: TDateTime;
    i:integer;
    sub1,sub2:String;
    LastNode,ThisNode:TDataNode;
begin
  MacroEventList.CurrentEvent:=idx;
  i:=idx;
  TempEvent:=MacroEventList.MacroEvents[i];
  p1:=TempEvent.EventType;
  p2:=TempEvent.NodeId;
  p3:=TempEvent.eventvalue;


  EditAttributeValue(LoggingLabelNode,'LabelCaption','Replay... >'+  p1 +'<  >'+  p2+'<  >'+ p3 +'< ');

     if  TempEvent.EventType = 'OpenSubMenu'
     then
     begin
       ThisNode:=FindDataNodeById(SystemNodeTree,TempEvent.NodeId,'',true);
       {$ifndef JScript}
       TXMenuItem(ThisNode.ScreenObject).Click;
       {$else}
       {$endif}
     end
     else
     begin
       if   (TempEvent.NodeId <> 'EventsMacroStartRecording')
       and  (TempEvent.NodeId <> 'EventsMacroStopRecording')
       and  (TempEvent.NodeId <> 'EventsMacroStartReplay')
       and  (TempEvent.NodeId <> 'EventsMacroSaveToClip')
       and  (TempEvent.NodeId <> 'EventsMacroLoadFromClip')
       and  (TempEvent.NodeId <> 'EventsMacroResumeRecording')
       and  (TempEvent.NodeId <> 'MacroCommentForm')
       then
       begin
         // Move the animated pointer....
         if (length(trim(p2))>0)
         then
         begin
           sub1:='';
           sub2:='';
           if (i>0) and (length(trim(MacroEventList.MacroEvents[i-1].NodeId))>0) then
             LastNode:=FindDataNodeById(SystemNodeTree,MacroEventList.MacroEvents[i-1].NodeId,'',true)
           else
             LastNode:=UIRootNode;
           ThisNode:=FindDataNodeById(SystemNodeTree,MacroEventList.MacroEvents[i].NodeId,'',true);
           {$ifdef JScript}
           asm
           console.log('DoNextEvent '+i+' EventType='+p1+' LastNode='+LastNode.NodeName+' ThisNode='+ThisNode.NodeName);
           end;
           {$endif}
           if (LastNode<>nil) and (ThisNode<>nil) then
           begin
             if LastNode.NodeType='TXTree' then
               sub1:=TXTree(LastNode.ScreenObject).SelectedNodeId;
             if ThisNode.NodeType='TXTree' then
               sub2:=TXTree(ThisNode.ScreenObject).SelectedNodeId;
             if (TempEvent.EventType='Drop') then
               MovePointer('',LastNode.NodeName,p2,sub1,sub2,true)
             else
               MovePointer('',ThisNode.NodeName,p2,sub1,sub2,false);
           end;
         end;

         // Do the Event processing......
         {$ifndef JScript}
         ReplayEvent(nil,p1,p2,'',p3);
         {$else}
         asm
         myTimeout(pas.EventLogging.ReplayEvent,20,'ReplayEvent',0,null,p1,p2,'',p3);
         end;
         {$endif}
         //...........................

       end;
  //     else
  //     begin
  //       if TempEvent.NodeId <> 'EventsMacroStopRecording' then
  //         showmessage('Event >'+TempEvent.NodeId +'< logged in the macro but not replayed' );
  //     end;
     end;

     if i>=length(MacroEventList.MacroEvents)-1 then
     begin
       FinishReplay;
     end;

     {$ifndef JScript}
     SetCompletedEvent(idx);
     {$else}
     asm
       if (TempEvent.NodeId != 'EventsMacroInsertComment') {
         myTimeout(pas.EventLogging.CheckEventDone, 210, 'checkdone',0,idx,0);
         myTimeout(pas.EventLogging.SetCompletedEvent,200,('SetCompletedEvent '+idx.toString()),0,idx);
       }
     end;
     {$endif}
end;



Procedure TMacroEventList.SetMenuVisibility;
begin
 if self.Replaying=false then
 begin
   if self.RecordingEnabled then
   begin
     EditAttributeValue('EventsMacroMenu','','Caption','Macro - Recording');
     EditAttributeValue('EventsMacroStopRecording','','isEnabled', 'true') ;
     EditAttributeValue('EventsMacroStartRecording','','isEnabled', 'false');
     EditAttributeValue('EventsMacroStartReplay','','isEnabled', 'false');
     EditAttributeValue('EventsMacroInsertComment','','isEnabled', 'true') ;
     EditAttributeValue('EventsMacroSaveToClip','','isEnabled', 'false') ;
     EditAttributeValue('EventsMacroLoadFromClip','','isEnabled', 'false') ;
     EditAttributeValue('EventsMacroResumeRecording','','isEnabled', 'false') ;
   end
   else
   begin
     EditAttributeValue('EventsMacroMenu','','Caption','Macro');
     EditAttributeValue('EventsMacroStopRecording','','isEnabled', 'false') ;
     EditAttributeValue('EventsMacroStartRecording','','isEnabled', 'true');
     EditAttributeValue('EventsMacroStartReplay','','isEnabled', 'true');
     EditAttributeValue('EventsMacroInsertComment','','isEnabled', 'false') ;
     EditAttributeValue('EventsMacroSaveToClip','','isEnabled', 'true') ;
     EditAttributeValue('EventsMacroLoadFromClip','','isEnabled', 'true') ;
     EditAttributeValue('EventsMacroResumeRecording','','isEnabled', 'true') ;
   end;
 end
 else
 begin
   //!!!! but basically need to disallow ANY user interaction during replay ....?????
   EditAttributeValue('EventsMacroMenu','','Caption','Macro - Replaying');
   EditAttributeValue('EventsMacroStopRecording','','isEnabled', 'false') ;
   EditAttributeValue('EventsMacroStartRecording','','isEnabled', 'false');
   EditAttributeValue('EventsMacroStartReplay','','isEnabled', 'false');
   EditAttributeValue('EventsMacroInsertComment','','isEnabled', 'false') ;
   EditAttributeValue('EventsMacroSaveToClip','','isEnabled', 'false') ;
   EditAttributeValue('EventsMacroLoadFromClip','','isEnabled', 'false') ;
   EditAttributeValue('EventsMacroResumeRecording','','isEnabled', 'false') ;
 end;
//         SetPropertyValue('StopMacroReplay','isVisible', 'true');
//         SetPropertyValue('ResumeMacroReplay','isVisible', 'true');

end;

constructor TMacroEventList.create;
begin
  setlength(MacroEvents,0);
  EventCount:=0;
  RecordingEnabled:=false;
  ReplayEnabled:=false;
  Replaying:=false;
  initialised:=false;
end;

//      function TMacroEventList.CheckForResumeFromLocalStorage:boolean;
//      var startstring:string;
//      begin
//        RecordingEnabled:=false;
//        ReplayEnabled:=false;
//        startstring:=getPropertyValue('StoreMacro','DataValue');
//        if length(trim(startstring))>1 then
//        begin
//          //showmessage('Saved Event List is >'+startstring +'< ');
//          FromMyString(startstring );
//        end;
//        SetMenuVisibility;
//        //SetPropertyValue('RecordingStatus','LabelCaption','Not Recording');
//        initialised:=true;
//        if (ReplayEnabled=true)
//        then  CheckForResumeFromLocalStorage:=true
//        else  CheckForResumeFromLocalStorage:=false;
//      end;

procedure TMacroEventList.add(e:TMacroEvent);
begin
  if RecordingEnabled = true then
  begin
    Eventcount:=Eventcount+1;
    setlength(MacroEvents,Eventcount);
    MacroEvents[Eventcount-1].EventType:=e.EventType;
    MacroEvents[Eventcount-1].NodeId:=e.NodeId;
    MacroEvents[Eventcount-1].eventvalue:=e.eventvalue;
  end;
end;

procedure TMacroEventList.clear;
begin
    Eventcount:=0;
    MacroEventList.CurrentEvent :=0;
    MacroEventList.CompletedEvent:=-1;
    setlength(MacroEvents,Eventcount);
end;

function  TMacroEventList.ToMyString:string;
var i:integer;
    tempstring:string;
begin
  //tempstring:=SubstituteSpecials(self.StartState) + eventDelimiter +LineEnding;
  tempstring:=self.StartState + eventDelimiter +LineEnding;
  if RecordingEnabled then tempstring:=tempstring + 'True' + eventDelimiter else tempstring:=tempstring + 'False' + eventDelimiter +LineEnding;
  if ReplayEnabled then tempstring:=tempstring + 'True' + eventDelimiter else tempstring:=tempstring + 'False' + eventDelimiter +LineEnding;
  if initialised then tempstring:=tempstring + 'True' + eventDelimiter else tempstring:=tempstring + 'False' + eventDelimiter +LineEnding;
  tempstring:=tempstring + inttostr(Eventcount) + eventDelimiter +LineEnding;
  tempstring:=tempstring + inttostr(CurrentEvent) + eventDelimiter +LineEnding;
  tempstring:=tempstring + inttostr(CompletedEvent) + eventDelimiter +LineEnding;
  for i:=0 to Eventcount - 1 do
  begin
    tempstring:=tempstring + MacroEvents[i].EventType + fieldDelimiter
                           + MacroEvents[i].eventvalue +fieldDelimiter
                          + MacroEvents[i].NodeId     + eventDelimiter +LineEnding;
  end;
  ToMyString:=tempstring;
end;

procedure TMacroEventList.FromMyString(instring:String);
var tempstringlist1,tempstringlist2:TStringList;
    i:integer;
    str:string;
begin
  setlength(MacroEvents,0);
  instring:=myStringReplace(instring,eventDelimiter+LineEnding,eventDelimiter,-1,-1);

  tempstringlist1:=stringsplit(instring,eventDelimiter);
//  StartState:=UnSubstituteSpecials(tempstringlist1[0]);
  StartState:=tempstringlist1[0];
  if tempstringlist1[1]='True' then RecordingEnabled:=true else RecordingEnabled:=false;
  if tempstringlist1[2]='True' then ReplayEnabled:=true else ReplayEnabled:=false;
  if tempstringlist1[3]='True' then initialised:=true else initialised:=false;
  Eventcount:=strtoint(tempstringlist1[4]);
  CurrentEvent:=strtoint(tempstringlist1[5]);
  CompletedEvent:=strtoint(tempstringlist1[6]);
  //setlength(MacroEvents,tempstringlist1.Count-7);
  //showmessage('Num Events = >'+inttostr(tempstringlist1.Count-7)+'<');
  for i:=7 to  tempstringlist1.Count - 1 do
  begin
    str:=tempstringlist1[i];
    setlength(MacroEvents,i-6);
    //showmessage('Event >'+inttostr(i-7)+'< =  >'+tempstringlist1[i]+'<');
    tempstringlist2:=stringsplit( tempstringlist1[i],fieldDelimiter);
    if tempstringlist2.Count<>3 then showmessage('Error in creating event from event list item >'+tempstringlist1[i]+'<')
    else
    begin
      MacroEvents[i-7].EventType   :=tempstringlist2[0];
      MacroEvents[i-7].eventvalue  :=tempstringlist2[1];
      MacroEvents[i-7].NodeId      :=tempstringlist2[2];
    end;
  end;
end;

procedure StartRecording;
var
  ok:Boolean;
begin
  if (MacroEventList.RecordingEnabled=false )
  then
  begin
    ok:=true;
    if length(MacroEventList.MacroEvents)>0 then
      ok:=confirm('OK to start recording a new Macro?')
    else
      showmessage('Recording a new Macro');
    if ok then
    begin
      MacroEventList.clear;
      //      SetPropertyValue('StoreMacro','DataValue',' '); // initalise the stored macro string to a blank string
      MacroEventList.RecordingEnabled:=true;
      MacroEventList.SetMenuVisibility;
      MacroEventList.StartState:=BuildSystemString(false);
    end;
  end;
end;

procedure StopRecording;
begin
  if (MacroEventList.RecordingEnabled=true )
  then
  begin
    MacroEventList.RecordingEnabled:=false;
    MacroEventList.SetMenuVisibility;
    Showmessage('Macro Recording has ended');
  end;
end;

procedure ResumeMacroRecording;
begin
  MacroEventList.RecordingEnabled:=true;
  MacroEventList.SetMenuVisibility;
  showmessage('Macro Recording has been resumed');
end;

procedure FinishReplay;
begin
  if MacroEventList.Replaying=true then
  begin
     Hidepointer;
     EditAttributeValue(LoggingLabelNode,'LabelCaption',' ');
     showmessage('End of Macro Replay');
     MacroEventList.Replaying:=false;
     MacroEventList.SetMenuVisibility;
  end;
end;

{$ifdef JScript}

procedure CheckEventDone(i,waitcount:integer);
begin
   asm
       // console.log('checkdone. i='+i+' waitcount='+waitcount);
         // waiting for event i to complete
         if (waitcount>999) {alert('wait '+i+'timed out');}
         else
         {
           if ((pas.EventLogging.MacroEventList.CompletedEvent<i)||(JobsWaiting.length>0)) {
             myTimeout(pas.EventLogging.CheckEventDone, 200, ('checkdone '+i.toString()), 0, i, (waitcount+1));
           }
           else
           {
             if (pas.EventLogging.MacroEventList.CompletedEvent>=i) {
               if (i>0) {i=pas.EventLogging.MacroEventList.CurrentEvent;}
               if (i< pas.EventLogging.MacroEventList.Eventcount-1) {
//                 console.log('******************** queueing event '+(i+1)+' '+pas.EventLogging.MacroEventList.MacroEvents[i+1].EventType+' ****************************');
                 var diagn=('DoNextEvent '+(i+1).toString()+' '+pas.EventLogging.MacroEventList.MacroEvents[i+1].EventType);
                 myTimeout(pas.EventLogging.DoNextEvent, 800, diagn, 0, i+1);
                 }
               else {pas.EventLogging.FinishReplay();}
             }
             else {console.log('doing nothing. i='+i+' CurrentEvent='+pas.EventLogging.MacroEventList.CurrentEvent);}
           }
         }
   end;
end;
{$endif}

procedure StartReplay;
var
  RowCount,i:integer;
begin
  if (MacroEventList.RecordingEnabled=false )
  then
  begin
    if not DesignMode then
      showmessage('Cannot load system string - switch to Design Mode first')
    else
    begin
      if (MacroEventList.StartState='')
      or (length(MacroEventList.MacroEvents)=0) then
      begin
        showmessage('No recorded macro available for replay');
        Hidepointer;
        MacroEventList.SetMenuVisibility;
      end
      else
      begin
        DoSystemLoad(MacroEventList.StartState,'');

        //  LastNodeName:='UIRoot';
        MacroEventList.Replaying:=true;
        MacroEventList.CurrentEvent:=0;
        MacroEventList.CompletedEvent:=-1;
        MacroEventList.Eventcount:=length(MacroEventList.MacroEvents);
        MacroEventList.RecordingEnabled:=false;
        MacroEventList.SetMenuVisibility;
        RowCount:= MacroEventList.Eventcount;

        i:=0;
        {$ifndef JScript}
        for i:=0 to RowCount-1
        do
        begin
          //MacroEventList.CurrentEvent:=MacroEventList.CurrentEvent+1;
          Application.ProcessMessages;
          sleep(600);
          DoNextEvent(i);
        end;
        {$else}
        // using myTimeout so that the screen is refreshed with any updates, before running next event
        // BUT NEED TO HAVE THE PREVIOUS EVENT COMPLETED BEFORE FIRING OFF THE NEXT ONE, TO KEEP THE IN-EVENT TIMEOUTS IN ORDER
        CheckEventDone(-1,0);
        {$endif}
      end;
    end;
  end;
end;

procedure InsertComment;
var
  macroEvent:TMacroEvent;
begin
  if (MacroEventList.RecordingEnabled=false )
  then
  begin
    if MacroEventList.Replaying then
    begin
      // replay the comment      (original events logged are: Change, ButtonClick, UserInput)
      while macroevent.EventType<>'UserInput'  do
        macroevent:=EventLogging.AdvanceEventLog;

      if macroevent.EventType<>'UserInput' then
      begin
        showmessage('unable to replay inserted comment...');
      end
      else
      begin
        //showmessage(macroevent.eventvalue);
        MacroCommentForm.ShowHTMLText(macroevent.eventvalue);
        ShowXForm('MacroCommentForm',true,'');
      end;
    end
    else
      showmessage('Comments can only be added when Macro recording is active - Command ignored');
  end
  else
    //TextMessage:= XIDEPrompt('Enter a comment to be shown when this macro is replayed.......','') ;
  begin
    // pop up a Memo component
    MacroCommentForm.ShowMemo;
    ShowXForm('MacroCommentForm',true,'');
  end;
end;

function AdvanceEventLog:TMacroEvent;
var
  emptyrec:TMacroEvent;
begin
  if MacroEventList.CurrentEvent<(length(MacroEventList.MacroEvents)-1) then
  begin
    MacroEventList.CurrentEvent:=MacroEventList.CurrentEvent+1;
    result:=MacroEventList.MacroEvents[MacroEventList.CurrentEvent];
  end
  else
  begin
    emptyrec.eventvalue:='';
    result:=emptyrec;
  end;
end;


function ReplayUserInput(promptText:String):String;
var i,NameLength:integer;
    tempstr:string;
    macroEvent:TMacroEvent;
begin
  // pop the original user-entered string off the event log (should be next event...)
  macroEvent:=AdvanceEventLog;
  if macroEvent.EventType<>'UserInput' then
    showmessage('oops cannot retrieve original user input')
  else
  begin
    tempstr:=macroEvent.eventvalue;
    NameLength:=length(tempstr);
//    showmessage('showing popup to test replay of get name dialogue ... string = '+tempstr);
    ReplayUserDialog.theText:=tempstr;

    showXForm('ReplayUserDialogForm',true);
    {$ifndef JScript}
    while ReplayUserDialogForm.Showing<>'No' do
      Application.ProcessMessages;
    {$else}
    ReplayUserDialogForm.RunReplayDialog(nil);
    {$endif}
    result:=tempstr;
  end;
end;

procedure SaveMacroToClip;
var
  str:String;
begin
  str:=MacroEventList.ToMyString;
  myCopyToClip('Macro',str );
end;

procedure DoMacroLoad(MacroString:String);
begin
  MacroEventList.FromMyString(MacroString);
end;

procedure LoadMacroFromClip(e:TEventStatus;nodeId:string);
var
  MacroString:String;
  macroEvent:TMacroEvent;
begin

   if (e=nil)  or (e.InitRunning=false) then
   begin
     //showmessage('new e');
     if (e=nil) then
     begin
       e:=TEventStatus.Create('Click',nodeId);
     end;
     e.initRunning:=true;
     PasteDialogUnit.CompletionEvent:=e;
     {$ifdef JScript}
     e.AsyncProcsRunning.Add('CopyFromClip');
     {$endif}

   end
   else
   begin
     e.InitRunning:=false;
   end;

   if e.InitRunning then
   begin
     PasteDoneBtn.IsVisible:=false;
     MacroString:=mygetClipboardData('Macro');
   end;

   if e.EventHasWaitingAsyncProcs = false then
   // this is lazarus and a confirm dialog is not needed
   // otherwise this is HTML and we have waited for a ctrl-V event from the PasteDialog form
   begin
     {$ifndef JScript}
     // Lazarus only
     //showmessage('call DoSystemLoad '+SystemDescription);
     DoMacroLoad(MacroString);
     {$else}
     asm
       pas.NodeUtils.StartingUp=false;
       var pasteTarget = document.getElementById('PasteTargetContents');
       var PasteString = pasteTarget.value;
       //alert('Paste string = >'+PasteString+'<' );
       pas.EventLogging.DoMacroLoad(PasteString);
     end;
     {$endif}
   end;
 end;

//.......................trapper code......................
procedure TrapEventForMacro(e:TEventStatus);
var
  TempEvent:TMacroEvent;
  ok:boolean;
begin

   TempEvent.EventType  := e.EventType;
   TempEvent.NodeId     := e.NodeId;
   TempEvent.eventvalue := e.eventvalue;

   if (MacroEventList.RecordingEnabled=true )   // recording is in progress
   then
   begin
     ok:=true;
     if TempEvent.NodeId = 'EventsMacroSaveToClip' then
     begin
       Showmessage('Stop The Macro Recording before calling Save Macro To Clip');
       ok:=false;
     end
     else if TempEvent.NodeId = 'EventsMacroLoadFromClip' then
     begin
       Showmessage('Stop The Macro Recording before calling Load Macro From Clip');
       ok:=false;
     end
     else if TempEvent.NodeId = 'EventsMacroResumeRecording' then
     begin
       Showmessage('Macro Recording is already active - Resume Recording Command ignored');
       ok:=false;
     end
     else if   (TempEvent.NodeId = 'EventsMacroStartRecording') then
     begin
       Showmessage('Macro Recording is already active - Start Recording Command ignored');
       ok:=false;
     end
     else if  (TempEvent.NodeId = 'EventsMacroStartReplay') then
     begin
       Showmessage('Macro Recording is already active - Replay Command ignored');
       ok:=false;
     end;

     if (ok) and (TempEvent.NodeId <> 'EventsMacroStopRecording') then
     begin
       MacroEventList.add(TempEvent);
       EditAttributeValue(LoggingLabelNode,'LabelCaption','Record.... >'+  e.EventType +'<  >'+  e.NodeId+'<  >'+ e.eventvalue +'< ');
     end;
   end;
end;

{$ifndef JScript}
procedure THelperProcs.CallMove(Data:PtrInt);
var
 Params: TmouseAnimParams;
begin
 Params:=PmouseAnimParams(Data)^;
 MouseInput.Move(Params.Shift,Params.ScreenX,Params.ScreenY,Params.duration);
end;
{$endif}

begin
  // IMPORTANT: do not use any of the dll interface functions inside the
  // unit initialization section.....errors will occur.
   MacroEventList:=TMacroEventList.create;
   {$ifndef JScript}
   HelperProcs:=THelperProcs.Create;
   AdditionalScript:=
   '<script> ' +LineEnding
//   +'function mywait(ms) {   '  +LineEnding
//   +'     return new Promise(resolve => setTimeout(resolve, ms));  '  +LineEnding     //!!!! but this timeout does not refresh the screen
//   +'}    '  +LineEnding
//   +'async function mysleep(ms) {  '  +LineEnding
//   +'     await mywait(ms); '  +LineEnding
//   +'     return;   '  +LineEnding
//   +'}  '  +LineEnding

   +'var glbTimeoutWaiting=false;  '  +LineEnding
   +'var AnimationWaiting=false;  '  +LineEnding
   +'var jobId=0;     '  +LineEnding
   +'var JobsWaiting=[];  '  +LineEnding

   +'function runFn(fn,ms,fname,job,...args) {  '  +LineEnding
   +'  var ars='''';  '  +LineEnding
   +'  for (var i = 2; i < arguments.length; i++) {ars=ars+'' ''+arguments[i];} '  +LineEnding
//   +'  console.log(''running ''+fname+'' : ''+ars); '  +LineEnding
   +'  fn(...args);  '  +LineEnding
   +'  glbTimeoutWaiting=false;  '  +LineEnding
   +'  tryNextJob();  '  +LineEnding
   +'}                           '  +LineEnding
   +'function tryNextJob() { '  +LineEnding
   +'  if (JobsWaiting.length==0) return; '  +LineEnding
   +'  if ((glbTimeoutWaiting==false) '+LineEnding
   +'   &&(AnimationWaiting==false)) '+LineEnding
   +'    {  '  +LineEnding
   +'    var nextjob=JobsWaiting.shift();  '  +LineEnding
   +'    job=nextjob.jobid;  '  +LineEnding
   +'    glbTimeoutWaiting=true;  '  +LineEnding
//   +'    console.log(''Queueing ''+nextjob.fname);  '  +LineEnding
//   +'    console.log(nextjob); '  +LineEnding
   +'    setTimeout(runFn,nextjob.msec,   ...nextjob.args);    //args===[fn,msec,fname,job,...args]   '  +LineEnding
   +'    }   '  +LineEnding
   +'  else {   '  +LineEnding
   +'    setTimeout(tryNextJob,100);  '  +LineEnding
   +'    }  '  +LineEnding
   +'}  '  +LineEnding
   +'function myTimeout(fn,msec,fname,job,...args) {  '  +LineEnding
   +'  var ars='''';   '  +LineEnding
   +'  for (var i = 3; i < arguments.length; i++) {ars=ars+'' ''+arguments[i];}   '  +LineEnding
//   +'  if (job==0) {  '  +LineEnding
   +'    jobId=jobId+1;  '  +LineEnding
   +'    if (jobId>30000) jobId=1;  '  +LineEnding
   +'    job=jobId;  '  +LineEnding
//   +'    console.log(''myTimeout ***** new job ''+job+'' ''+fname);   '  +LineEnding
   +'    JobsWaiting.push({jobid:job, func:fn, msec:msec, fname:fname, args:arguments});  '  +LineEnding
   +'    tryNextJob();  '  +LineEnding
//   +'  }   '  +LineEnding
   +'  return job;     '  +LineEnding
   +'}  '  +LineEnding

   +'</script>';
   {$endif}
end.

