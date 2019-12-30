{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit EditExpLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  editexp, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('editexp', @editexp.Register);
end;

initialization
  RegisterPackage('EditExpLazarus', @Register);
end.
