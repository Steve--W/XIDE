unit XIDESettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils,
  {$ifndef JScript}
  LazsUtils,
  {$endif}
  CompileUserCode, XObjectInsp, XVBox, XMemo, XForm, XEditBox, XComboBox,
  XButton, EventsInterface, XCheckBox;

type

  { TXIDESettingsForm }

  TXIDESettingsForm = class(TXForm)
    DeployModeCombo: TXComboBox;
    ResourcesPosCombo: TXComboBox;
    SettingsDoneButton: TXButton;
    XsettingsVBox: TXVBox;
    FPCLocationEditBox: TXEditBox;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$endif}
    procedure InitialiseOnShow;
    procedure CloseSettings;
    procedure SettingsDoneButtonHandleButtonClick(e: TEventStatus;
      nodeID: AnsiString; myValue: AnsiString);
  private

  public

  end;

var
  XIDESettingsForm: TXIDESettingsForm;

implementation

{$R *.lfm}

{ TXIDESettingsForm }

{$ifndef JScript}
procedure TXIDESettingsForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

procedure TXIDESettingsForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, XsettingsVBox);
end;

procedure TXIDESettingsForm.FormShow(Sender: TObject);
begin
end;

{$endif}

procedure TXIDESettingsForm.CloseSettings;
var
  Lines:TStringList;
  NewPath:String;
begin
  UIRootNode.SetAttributeValue('ShowResources',ResourcesPosCombo.ItemValue);
  if DesignMode then
    RedisplayResourceTree;
  UIRootNode.SetAttributeValue('DeploymentMode',DeployModeCombo.ItemValue);

  {$ifndef JScript}
  NewPath := FPCLocationEditBox.ItemValue;
  if NewPath<>'' then
  begin
    ConfigFPCPath := NewPath;
    Lines:=TStringList.Create;
    Lines.Add(ConfigFPCPath);
    Lines.SaveToFile('XIDERunSettings.dta');
    Lines.Free;
  end;
  {$endif}
end;

procedure TXIDESettingsForm.SettingsDoneButtonHandleButtonClick(
  e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
begin
  CloseXForm('XIDESettingsForm');
  CloseSettings;
end;

procedure TXIDESettingsForm.InitialiseOnShow;
begin
  ResourcesPosCombo.ItemValue:=UIRootNode.GetAttribute('ShowResources',true).AttribValue;
  DeployModeCombo.ItemValue:=UIRootNode.GetAttribute('DeploymentMode',true).AttribValue;
  {$ifndef JScript}
  FPCLocationEditBox.ItemValue:=ConfigFPCPath;
  {$else}
  FPCLocationEditBox.IsVisible:=false;
  {$endif}
end;

end.

