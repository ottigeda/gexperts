///<summary>
/// stripped down version of ToolsAPI to be used in stand alone utitlities </summary>
unit ToolsAPI;

interface

uses
  Windows;

type
  IBorlandIDEServices = interface
    ['{C9E8E577-B5D8-43F3-BC84-6A734A015732}']

  end;

type
  IOTAAboutBoxServices = interface
    ['{1A3CDFB0-EBF6-449D-88D4-3D2991F8974F}']
    function AddPluginInfo(const ATitle, ADescription: string; AImage: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = ''): Integer;
    procedure RemovePluginInfo(Index: Integer);
  end;

type
  IOTASplashScreenServices = interface
    procedure AddPluginBitmap(const ACaption: string; ABitmap: HBITMAP;
      AIsUnRegistered: Boolean = False; const ALicenseStatus: string = '';
      const ASKUName: string = '');
  end;

var
  BorlandIDEServices: IBorlandIDEServices = nil;
  SplashScreenServices: IOTASplashScreenServices = nil;

implementation

end.
