{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit XIDEComponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  XGPUCanvas, XThreads, WebTranspilerUtils, XComposite, XCompositeIntf, 
  XGPUEditor, X3DTable, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('XIDEComponents', @Register);
end.
