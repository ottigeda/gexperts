unit AgentServerObjects_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.106  $
// File generated on 4/24/2001 14:41:03 from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\WINNT\msagent\AgentSvr.exe (1)
// LIBID: {A7B93C73-7B81-11D0-AC5F-00C04FD97575}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (F:\WINNT\System32\stdole2.tlb)
// ************************************************************************ //
{$WARN SYMBOL_PLATFORM OFF}
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Variants, Classes, Graphics, StdVCL, OleServer;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AgentServerObjectsMajorVersion = 2;
  AgentServerObjectsMinorVersion = 0;

  LIBID_AgentServerObjects: TGUID = '{A7B93C73-7B81-11D0-AC5F-00C04FD97575}';

  IID_IAgent: TGUID = '{A7B93C91-7B81-11D0-AC5F-00C04FD97575}';
  CLASS_AgentServer: TGUID = '{D45FD2FC-5C6E-11D1-9EC1-00C04FD7081F}';
  IID_IAgentUserInput: TGUID = '{A7B93C80-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentCommand: TGUID = '{A7B93C83-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentCommandEx: TGUID = '{B0913412-3B44-11D1-ACBA-00C04FD97575}';
  IID_IAgentCommands: TGUID = '{A7B93C85-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentCommandsEx: TGUID = '{6BA90C00-3910-11D1-ACB3-00C04FD97575}';
  IID_IAgentSpeechInputProperties: TGUID = '{A7B93C87-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentAudioOutputProperties: TGUID = '{A7B93C89-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentAudioOutputPropertiesEx: TGUID = '{A7B93CA0-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentPropertySheet: TGUID = '{A7B93C8B-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentBalloon: TGUID = '{A7B93C8D-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentBalloonEx: TGUID = '{D7A6D440-8872-11D1-9EC6-00C04FD7081F}';
  IID_IAgentCharacter: TGUID = '{A7B93C8F-7B81-11D0-AC5F-00C04FD97575}';
  IID_IAgentCharacterEx: TGUID = '{98BBE491-2EED-11D1-ACAC-00C04FD97575}';
  IID_IAgentEx: TGUID = '{48D12BA0-5B77-11D1-9EC1-00C04FD7081F}';
  IID_IAgentNotifySink: TGUID = '{00D18159-8466-11D0-AC63-00C04FD97575}';
  IID_IAgentNotifySinkEx: TGUID = '{08C75162-3C9C-11D1-91FE-00C04FD701A5}';
  IID_IAgentCommandWindow: TGUID = '{6D0ECB23-9968-11D0-AC6E-00C04FD97575}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAgent = interface;
  IAgentDisp = dispinterface;
  IAgentUserInput = interface;
  IAgentUserInputDisp = dispinterface;
  IAgentCommand = interface;
  IAgentCommandDisp = dispinterface;
  IAgentCommandEx = interface;
  IAgentCommandExDisp = dispinterface;
  IAgentCommands = interface;
  IAgentCommandsDisp = dispinterface;
  IAgentCommandsEx = interface;
  IAgentCommandsExDisp = dispinterface;
  IAgentSpeechInputProperties = interface;
  IAgentSpeechInputPropertiesDisp = dispinterface;
  IAgentAudioOutputProperties = interface;
  IAgentAudioOutputPropertiesDisp = dispinterface;
  IAgentAudioOutputPropertiesEx = interface;
  IAgentAudioOutputPropertiesExDisp = dispinterface;
  IAgentPropertySheet = interface;
  IAgentPropertySheetDisp = dispinterface;
  IAgentBalloon = interface;
  IAgentBalloonDisp = dispinterface;
  IAgentBalloonEx = interface;
  IAgentBalloonExDisp = dispinterface;
  IAgentCharacter = interface;
  IAgentCharacterDisp = dispinterface;
  IAgentCharacterEx = interface;
  IAgentCharacterExDisp = dispinterface;
  IAgentEx = interface;
  IAgentExDisp = dispinterface;
  IAgentNotifySink = interface;
  IAgentNotifySinkDisp = dispinterface;
  IAgentNotifySinkEx = interface;
  IAgentNotifySinkExDisp = dispinterface;
  IAgentCommandWindow = interface;
  IAgentCommandWindowDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AgentServer = IAgent;


// *********************************************************************//
// Interface: IAgent
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C91-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgent = interface(IDispatch)
    ['{A7B93C91-7B81-11D0-AC5F-00C04FD97575}']
    procedure Load(vLoadKey: OleVariant; out pdwCharID: Integer; out pdwReqID: Integer); safecall;
    procedure Unload(dwCharID: Integer); safecall;
    procedure Register(const punkNotifySink: IUnknown; out pdwSinkID: Integer); safecall;
    procedure Unregister(dwSinkID: Integer); safecall;
    procedure GetCharacter(dwCharID: Integer; out ppunkCharacter: IDispatch); safecall;
    procedure GetSuspended(out pbSuspended: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C91-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentDisp = dispinterface
    ['{A7B93C91-7B81-11D0-AC5F-00C04FD97575}']
    procedure Load(vLoadKey: OleVariant; out pdwCharID: Integer; out pdwReqID: Integer); dispid 1610743808;
    procedure Unload(dwCharID: Integer); dispid 1610743809;
    procedure Register(const punkNotifySink: IUnknown; out pdwSinkID: Integer); dispid 1610743810;
    procedure Unregister(dwSinkID: Integer); dispid 1610743811;
    procedure GetCharacter(dwCharID: Integer; out ppunkCharacter: IDispatch); dispid 1610743812;
    procedure GetSuspended(out pbSuspended: Integer); dispid 1610743813;
  end;

// *********************************************************************//
// Interface: IAgentUserInput
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C80-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentUserInput = interface(IDispatch)
    ['{A7B93C80-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetCount(out pdwCount: Integer); safecall;
    procedure GetItemID(dwItemIndex: Integer; out pdwCommandID: Integer); safecall;
    procedure GetItemConfidence(dwItemIndex: Integer; out plConfidence: Integer); safecall;
    procedure GetItemText(dwItemIndex: Integer; out pbszText: WideString); safecall;
    procedure GetAllItemData(out pdwItemIndices: OleVariant; out plConfidences: OleVariant; 
                             out pbszText: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentUserInputDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C80-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentUserInputDisp = dispinterface
    ['{A7B93C80-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetCount(out pdwCount: Integer); dispid 1610743808;
    procedure GetItemID(dwItemIndex: Integer; out pdwCommandID: Integer); dispid 1610743809;
    procedure GetItemConfidence(dwItemIndex: Integer; out plConfidence: Integer); dispid 1610743810;
    procedure GetItemText(dwItemIndex: Integer; out pbszText: WideString); dispid 1610743811;
    procedure GetAllItemData(out pdwItemIndices: OleVariant; out plConfidences: OleVariant; 
                             out pbszText: OleVariant); dispid 1610743812;
  end;

// *********************************************************************//
// Interface: IAgentCommand
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C83-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentCommand = interface(IDispatch)
    ['{A7B93C83-7B81-11D0-AC5F-00C04FD97575}']
    procedure SetCaption(const bszCaption: WideString); safecall;
    procedure GetCaption(out pbszCaption: WideString); safecall;
    procedure SetVoice(const bszVoice: WideString); safecall;
    procedure GetVoice(out pbszVoice: WideString); safecall;
    procedure SetEnabled(bEnabled: Integer); safecall;
    procedure GetEnabled(out pbEnabled: Integer); safecall;
    procedure SetVisible(bVisible: Integer); safecall;
    procedure GetVisible(out pbVisible: Integer); safecall;
    procedure SetConfidenceThreshold(lThreshold: Integer); safecall;
    procedure GetConfidenceThreshold(out plThreshold: Integer); safecall;
    procedure SetConfidenceText(const bszTipText: WideString); safecall;
    procedure GetConfidenceText(out pbszTipText: WideString); safecall;
    procedure GetID(out pdwID: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCommandDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C83-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentCommandDisp = dispinterface
    ['{A7B93C83-7B81-11D0-AC5F-00C04FD97575}']
    procedure SetCaption(const bszCaption: WideString); dispid 1610743808;
    procedure GetCaption(out pbszCaption: WideString); dispid 1610743809;
    procedure SetVoice(const bszVoice: WideString); dispid 1610743810;
    procedure GetVoice(out pbszVoice: WideString); dispid 1610743811;
    procedure SetEnabled(bEnabled: Integer); dispid 1610743812;
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743813;
    procedure SetVisible(bVisible: Integer); dispid 1610743814;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743815;
    procedure SetConfidenceThreshold(lThreshold: Integer); dispid 1610743816;
    procedure GetConfidenceThreshold(out plThreshold: Integer); dispid 1610743817;
    procedure SetConfidenceText(const bszTipText: WideString); dispid 1610743818;
    procedure GetConfidenceText(out pbszTipText: WideString); dispid 1610743819;
    procedure GetID(out pdwID: Integer); dispid 1610743820;
  end;

// *********************************************************************//
// Interface: IAgentCommandEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B0913412-3B44-11D1-ACBA-00C04FD97575}
// *********************************************************************//
  IAgentCommandEx = interface(IAgentCommand)
    ['{B0913412-3B44-11D1-ACBA-00C04FD97575}']
    procedure SetHelpContextID(ulID: Integer); safecall;
    procedure GetHelpContextID(out pulID: Integer); safecall;
    procedure SetVoiceCaption(const bszVoiceCaption: WideString); safecall;
    procedure GetVoiceCaption(out pbszVoiceCaption: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCommandExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B0913412-3B44-11D1-ACBA-00C04FD97575}
// *********************************************************************//
  IAgentCommandExDisp = dispinterface
    ['{B0913412-3B44-11D1-ACBA-00C04FD97575}']
    procedure SetHelpContextID(ulID: Integer); dispid 1610809344;
    procedure GetHelpContextID(out pulID: Integer); dispid 1610809345;
    procedure SetVoiceCaption(const bszVoiceCaption: WideString); dispid 1610809346;
    procedure GetVoiceCaption(out pbszVoiceCaption: WideString); dispid 1610809347;
    procedure SetCaption(const bszCaption: WideString); dispid 1610743808;
    procedure GetCaption(out pbszCaption: WideString); dispid 1610743809;
    procedure SetVoice(const bszVoice: WideString); dispid 1610743810;
    procedure GetVoice(out pbszVoice: WideString); dispid 1610743811;
    procedure SetEnabled(bEnabled: Integer); dispid 1610743812;
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743813;
    procedure SetVisible(bVisible: Integer); dispid 1610743814;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743815;
    procedure SetConfidenceThreshold(lThreshold: Integer); dispid 1610743816;
    procedure GetConfidenceThreshold(out plThreshold: Integer); dispid 1610743817;
    procedure SetConfidenceText(const bszTipText: WideString); dispid 1610743818;
    procedure GetConfidenceText(out pbszTipText: WideString); dispid 1610743819;
    procedure GetID(out pdwID: Integer); dispid 1610743820;
  end;

// *********************************************************************//
// Interface: IAgentCommands
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C85-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentCommands = interface(IDispatch)
    ['{A7B93C85-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetCommand(dwCommandID: Integer; out ppunkCommand: IUnknown); safecall;
    procedure GetCount(out pdwCount: Integer); safecall;
    procedure SetCaption(const bszCaption: WideString); safecall;
    procedure GetCaption(out pbszCaption: WideString); safecall;
    procedure SetVoice(const bszVoice: WideString); safecall;
    procedure GetVoice(out pbszVoice: WideString); safecall;
    procedure SetVisible(bVisible: Integer); safecall;
    procedure GetVisible(out pbVisible: Integer); safecall;
    procedure Add(const bszCaption: WideString; const bszVoice: WideString; bEnabled: Integer; 
                  bVisible: Integer; out pdwID: Integer); safecall;
    procedure Insert(const bszCaption: WideString; const bszVoice: WideString; bEnabled: Integer; 
                     bVisible: Integer; dwRefID: Integer; bBefore: Integer; out pdwID: Integer); safecall;
    procedure Remove(dwID: Integer); safecall;
    procedure RemoveAll; safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCommandsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C85-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentCommandsDisp = dispinterface
    ['{A7B93C85-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetCommand(dwCommandID: Integer; out ppunkCommand: IUnknown); dispid 1610743808;
    procedure GetCount(out pdwCount: Integer); dispid 1610743809;
    procedure SetCaption(const bszCaption: WideString); dispid 1610743810;
    procedure GetCaption(out pbszCaption: WideString); dispid 1610743811;
    procedure SetVoice(const bszVoice: WideString); dispid 1610743812;
    procedure GetVoice(out pbszVoice: WideString); dispid 1610743813;
    procedure SetVisible(bVisible: Integer); dispid 1610743814;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743815;
    procedure Add(const bszCaption: WideString; const bszVoice: WideString; bEnabled: Integer; 
                  bVisible: Integer; out pdwID: Integer); dispid 1610743816;
    procedure Insert(const bszCaption: WideString; const bszVoice: WideString; bEnabled: Integer; 
                     bVisible: Integer; dwRefID: Integer; bBefore: Integer; out pdwID: Integer); dispid 1610743817;
    procedure Remove(dwID: Integer); dispid 1610743818;
    procedure RemoveAll; dispid 1610743819;
  end;

// *********************************************************************//
// Interface: IAgentCommandsEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BA90C00-3910-11D1-ACB3-00C04FD97575}
// *********************************************************************//
  IAgentCommandsEx = interface(IAgentCommands)
    ['{6BA90C00-3910-11D1-ACB3-00C04FD97575}']
    procedure GetCommandEx(dwCommandID: Integer; out ppCommandEx: IAgentCommandEx); safecall;
    procedure SetDefaultID(dwID: Integer); safecall;
    procedure GetDefaultID(out pdwID: Integer); safecall;
    procedure SetHelpContextID(ulHelpID: Integer); safecall;
    procedure GetHelpContextID(out pulHelpID: Integer); safecall;
    procedure SetFontName(const bszFontName: WideString); safecall;
    procedure GetFontName(out pbszFontName: WideString); safecall;
    procedure SetFontSize(lFontSize: Integer); safecall;
    procedure GetFontSize(out lFontSize: Integer); safecall;
    procedure SetVoiceCaption(const bszVoiceCaption: WideString); safecall;
    procedure GetVoiceCaption(out bszVoiceCaption: WideString); safecall;
    procedure AddEx(const bszCaption: WideString; const bszVoice: WideString; 
                    const bszVoiceCaption: WideString; bEnabled: Integer; bVisible: Integer; 
                    ulHelpID: Integer; out pdwID: Integer); safecall;
    procedure InsertEx(const bszCaption: WideString; const bszVoice: WideString; 
                       const bszVoiceCaption: WideString; bEnabled: Integer; bVisible: Integer; 
                       ulHelpID: Integer; dwRefID: Integer; bBefore: Integer; out pdwID: Integer); safecall;
    procedure SetGlobalVoiceCommandsEnabled(bEnable: Integer); safecall;
    procedure GetGlobalVoiceCommandsEnabled(out pbEnabled: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCommandsExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BA90C00-3910-11D1-ACB3-00C04FD97575}
// *********************************************************************//
  IAgentCommandsExDisp = dispinterface
    ['{6BA90C00-3910-11D1-ACB3-00C04FD97575}']
    procedure GetCommandEx(dwCommandID: Integer; out ppCommandEx: IAgentCommandEx); dispid 1610809344;
    procedure SetDefaultID(dwID: Integer); dispid 1610809345;
    procedure GetDefaultID(out pdwID: Integer); dispid 1610809346;
    procedure SetHelpContextID(ulHelpID: Integer); dispid 1610809347;
    procedure GetHelpContextID(out pulHelpID: Integer); dispid 1610809348;
    procedure SetFontName(const bszFontName: WideString); dispid 1610809349;
    procedure GetFontName(out pbszFontName: WideString); dispid 1610809350;
    procedure SetFontSize(lFontSize: Integer); dispid 1610809351;
    procedure GetFontSize(out lFontSize: Integer); dispid 1610809352;
    procedure SetVoiceCaption(const bszVoiceCaption: WideString); dispid 1610809353;
    procedure GetVoiceCaption(out bszVoiceCaption: WideString); dispid 1610809354;
    procedure AddEx(const bszCaption: WideString; const bszVoice: WideString; 
                    const bszVoiceCaption: WideString; bEnabled: Integer; bVisible: Integer; 
                    ulHelpID: Integer; out pdwID: Integer); dispid 1610809355;
    procedure InsertEx(const bszCaption: WideString; const bszVoice: WideString; 
                       const bszVoiceCaption: WideString; bEnabled: Integer; bVisible: Integer; 
                       ulHelpID: Integer; dwRefID: Integer; bBefore: Integer; out pdwID: Integer); dispid 1610809356;
    procedure SetGlobalVoiceCommandsEnabled(bEnable: Integer); dispid 1610809357;
    procedure GetGlobalVoiceCommandsEnabled(out pbEnabled: Integer); dispid 1610809358;
    procedure GetCommand(dwCommandID: Integer; out ppunkCommand: IUnknown); dispid 1610743808;
    procedure GetCount(out pdwCount: Integer); dispid 1610743809;
    procedure SetCaption(const bszCaption: WideString); dispid 1610743810;
    procedure GetCaption(out pbszCaption: WideString); dispid 1610743811;
    procedure SetVoice(const bszVoice: WideString); dispid 1610743812;
    procedure GetVoice(out pbszVoice: WideString); dispid 1610743813;
    procedure SetVisible(bVisible: Integer); dispid 1610743814;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743815;
    procedure Add(const bszCaption: WideString; const bszVoice: WideString; bEnabled: Integer; 
                  bVisible: Integer; out pdwID: Integer); dispid 1610743816;
    procedure Insert(const bszCaption: WideString; const bszVoice: WideString; bEnabled: Integer; 
                     bVisible: Integer; dwRefID: Integer; bBefore: Integer; out pdwID: Integer); dispid 1610743817;
    procedure Remove(dwID: Integer); dispid 1610743818;
    procedure RemoveAll; dispid 1610743819;
  end;

// *********************************************************************//
// Interface: IAgentSpeechInputProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C87-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentSpeechInputProperties = interface(IDispatch)
    ['{A7B93C87-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetInstalled(out pbInstalled: Integer); safecall;
    procedure GetEnabled(out pbEnabled: Integer); safecall;
    procedure GetHotKey(out pbszHotCharKey: WideString); safecall;
    procedure GetLCID(out plcidCurrent: LongWord); safecall;
    procedure GetEngine(out pbszEngine: WideString); safecall;
    procedure SetEngine(const bszEngine: WideString); safecall;
    procedure GetListeningTip(out pbListeningTip: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentSpeechInputPropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C87-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentSpeechInputPropertiesDisp = dispinterface
    ['{A7B93C87-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetInstalled(out pbInstalled: Integer); dispid 1610743808;
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743809;
    procedure GetHotKey(out pbszHotCharKey: WideString); dispid 1610743810;
    procedure GetLCID(out plcidCurrent: LongWord); dispid 1610743811;
    procedure GetEngine(out pbszEngine: WideString); dispid 1610743812;
    procedure SetEngine(const bszEngine: WideString); dispid 1610743813;
    procedure GetListeningTip(out pbListeningTip: Integer); dispid 1610743814;
  end;

// *********************************************************************//
// Interface: IAgentAudioOutputProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C89-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentAudioOutputProperties = interface(IDispatch)
    ['{A7B93C89-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetEnabled(out pbEnabled: Integer); safecall;
    procedure GetUsingSoundEffects(out pbUsingSoundEffects: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentAudioOutputPropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C89-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentAudioOutputPropertiesDisp = dispinterface
    ['{A7B93C89-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743808;
    procedure GetUsingSoundEffects(out pbUsingSoundEffects: Integer); dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IAgentAudioOutputPropertiesEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93CA0-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentAudioOutputPropertiesEx = interface(IAgentAudioOutputProperties)
    ['{A7B93CA0-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetStatus(out plStatus: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentAudioOutputPropertiesExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93CA0-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentAudioOutputPropertiesExDisp = dispinterface
    ['{A7B93CA0-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetStatus(out plStatus: Integer); dispid 1610809344;
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743808;
    procedure GetUsingSoundEffects(out pbUsingSoundEffects: Integer); dispid 1610743809;
  end;

// *********************************************************************//
// Interface: IAgentPropertySheet
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C8B-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentPropertySheet = interface(IDispatch)
    ['{A7B93C8B-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetVisible(out pbVisible: Integer); safecall;
    procedure SetVisible(bVisible: Integer); safecall;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); safecall;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); safecall;
    procedure GetPage(out pbszPage: WideString); safecall;
    procedure SetPage(const bszPage: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentPropertySheetDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C8B-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentPropertySheetDisp = dispinterface
    ['{A7B93C8B-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetVisible(out pbVisible: Integer); dispid 1610743808;
    procedure SetVisible(bVisible: Integer); dispid 1610743809;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); dispid 1610743810;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); dispid 1610743811;
    procedure GetPage(out pbszPage: WideString); dispid 1610743812;
    procedure SetPage(const bszPage: WideString); dispid 1610743813;
  end;

// *********************************************************************//
// Interface: IAgentBalloon
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C8D-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentBalloon = interface(IDispatch)
    ['{A7B93C8D-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetEnabled(out pbEnabled: Integer); safecall;
    procedure GetNumLines(out plLines: Integer); safecall;
    procedure GetNumCharsPerLine(out plCharsPerLine: Integer); safecall;
    procedure GetFontName(out pbszFontName: WideString); safecall;
    procedure GetFontSize(out plFontSize: Integer); safecall;
    procedure GetFontBold(out pbFontBold: Integer); safecall;
    procedure GetFontItalic(out pbFontItalic: Integer); safecall;
    procedure GetFontStrikethru(out pbFontStrikethru: Integer); safecall;
    procedure GetFontUnderline(out pbFontUnderline: Integer); safecall;
    procedure GetForeColor(out plFGColor: Integer); safecall;
    procedure GetBackColor(out plBGColor: Integer); safecall;
    procedure GetBorderColor(out plBorderColor: Integer); safecall;
    procedure SetVisible(bVisible: Integer); safecall;
    procedure GetVisible(out pbVisible: Integer); safecall;
    procedure SetFontName(const bszFontName: WideString); safecall;
    procedure SetFontSize(lFontSize: Integer); safecall;
    procedure SetFontCharSet(sFontCharSet: Smallint); safecall;
    procedure GetFontCharSet(out psFontCharSet: Smallint); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentBalloonDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C8D-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentBalloonDisp = dispinterface
    ['{A7B93C8D-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743808;
    procedure GetNumLines(out plLines: Integer); dispid 1610743809;
    procedure GetNumCharsPerLine(out plCharsPerLine: Integer); dispid 1610743810;
    procedure GetFontName(out pbszFontName: WideString); dispid 1610743811;
    procedure GetFontSize(out plFontSize: Integer); dispid 1610743812;
    procedure GetFontBold(out pbFontBold: Integer); dispid 1610743813;
    procedure GetFontItalic(out pbFontItalic: Integer); dispid 1610743814;
    procedure GetFontStrikethru(out pbFontStrikethru: Integer); dispid 1610743815;
    procedure GetFontUnderline(out pbFontUnderline: Integer); dispid 1610743816;
    procedure GetForeColor(out plFGColor: Integer); dispid 1610743817;
    procedure GetBackColor(out plBGColor: Integer); dispid 1610743818;
    procedure GetBorderColor(out plBorderColor: Integer); dispid 1610743819;
    procedure SetVisible(bVisible: Integer); dispid 1610743820;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743821;
    procedure SetFontName(const bszFontName: WideString); dispid 1610743822;
    procedure SetFontSize(lFontSize: Integer); dispid 1610743823;
    procedure SetFontCharSet(sFontCharSet: Smallint); dispid 1610743824;
    procedure GetFontCharSet(out psFontCharSet: Smallint); dispid 1610743825;
  end;

// *********************************************************************//
// Interface: IAgentBalloonEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D7A6D440-8872-11D1-9EC6-00C04FD7081F}
// *********************************************************************//
  IAgentBalloonEx = interface(IAgentBalloon)
    ['{D7A6D440-8872-11D1-9EC6-00C04FD7081F}']
    procedure SetStyle(lStyle: Integer); safecall;
    procedure GetStyle(out plStyle: Integer); safecall;
    procedure SetNumLines(lLines: Integer); safecall;
    procedure SetNumCharsPerLine(lCharsPerLine: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentBalloonExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D7A6D440-8872-11D1-9EC6-00C04FD7081F}
// *********************************************************************//
  IAgentBalloonExDisp = dispinterface
    ['{D7A6D440-8872-11D1-9EC6-00C04FD7081F}']
    procedure SetStyle(lStyle: Integer); dispid 1610809344;
    procedure GetStyle(out plStyle: Integer); dispid 1610809345;
    procedure SetNumLines(lLines: Integer); dispid 1610809346;
    procedure SetNumCharsPerLine(lCharsPerLine: Integer); dispid 1610809347;
    procedure GetEnabled(out pbEnabled: Integer); dispid 1610743808;
    procedure GetNumLines(out plLines: Integer); dispid 1610743809;
    procedure GetNumCharsPerLine(out plCharsPerLine: Integer); dispid 1610743810;
    procedure GetFontName(out pbszFontName: WideString); dispid 1610743811;
    procedure GetFontSize(out plFontSize: Integer); dispid 1610743812;
    procedure GetFontBold(out pbFontBold: Integer); dispid 1610743813;
    procedure GetFontItalic(out pbFontItalic: Integer); dispid 1610743814;
    procedure GetFontStrikethru(out pbFontStrikethru: Integer); dispid 1610743815;
    procedure GetFontUnderline(out pbFontUnderline: Integer); dispid 1610743816;
    procedure GetForeColor(out plFGColor: Integer); dispid 1610743817;
    procedure GetBackColor(out plBGColor: Integer); dispid 1610743818;
    procedure GetBorderColor(out plBorderColor: Integer); dispid 1610743819;
    procedure SetVisible(bVisible: Integer); dispid 1610743820;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743821;
    procedure SetFontName(const bszFontName: WideString); dispid 1610743822;
    procedure SetFontSize(lFontSize: Integer); dispid 1610743823;
    procedure SetFontCharSet(sFontCharSet: Smallint); dispid 1610743824;
    procedure GetFontCharSet(out psFontCharSet: Smallint); dispid 1610743825;
  end;

// *********************************************************************//
// Interface: IAgentCharacter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C8F-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentCharacter = interface(IDispatch)
    ['{A7B93C8F-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetVisible(out pbVisible: Integer); safecall;
    procedure SetPosition(lLeft: Integer; lTop: Integer); safecall;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); safecall;
    procedure SetSize(lWidth: Integer; lHeight: Integer); safecall;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); safecall;
    procedure GetName(out pbszName: WideString); safecall;
    procedure GetDescription(out pbszDescription: WideString); safecall;
    procedure GetTTSSpeed(out pdwSpeed: Integer); safecall;
    procedure GetTTSPitch(out pwPitch: Smallint); safecall;
    procedure Activate(sState: Smallint); safecall;
    procedure SetIdleOn(bOn: Integer); safecall;
    procedure GetIdleOn(out pbOn: Integer); safecall;
    procedure Prepare(dwType: Integer; const bszName: WideString; bQueue: Integer; 
                      out pdwReqID: Integer); safecall;
    procedure Play(const bszAnimation: WideString; out pdwReqID: Integer); safecall;
    procedure Stop(dwReqID: Integer); safecall;
    procedure StopAll(lTypes: Integer); safecall;
    procedure Wait(dwReqID: Integer; out pdwReqID: Integer); safecall;
    procedure Interrupt(dwReqID: Integer; out pdwReqID: Integer); safecall;
    procedure Show(bFast: Integer; out pdwReqID: Integer); safecall;
    procedure Hide(bFast: Integer; out pdwReqID: Integer); safecall;
    procedure Speak(const bszText: WideString; const bszUrl: WideString; out pdwReqID: Integer); safecall;
    procedure MoveTo(x: Smallint; y: Smallint; lSpeed: Integer; out pdwReqID: Integer); safecall;
    procedure GestureAt(x: Smallint; y: Smallint; out pdwReqID: Integer); safecall;
    procedure GetMoveCause(out pdwCause: Integer); safecall;
    procedure GetVisibilityCause(out pdwCause: Integer); safecall;
    procedure HasOtherClients(out plNumOtherClients: Integer); safecall;
    procedure SetSoundEffectsOn(bOn: Integer); safecall;
    procedure GetSoundEffectsOn(out pbOn: Integer); safecall;
    procedure SetName(const bszName: WideString); safecall;
    procedure SetDescription(const bszDescription: WideString); safecall;
    procedure GetExtraData(out pbszExtraData: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCharacterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7B93C8F-7B81-11D0-AC5F-00C04FD97575}
// *********************************************************************//
  IAgentCharacterDisp = dispinterface
    ['{A7B93C8F-7B81-11D0-AC5F-00C04FD97575}']
    procedure GetVisible(out pbVisible: Integer); dispid 1610743808;
    procedure SetPosition(lLeft: Integer; lTop: Integer); dispid 1610743809;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); dispid 1610743810;
    procedure SetSize(lWidth: Integer; lHeight: Integer); dispid 1610743811;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); dispid 1610743812;
    procedure GetName(out pbszName: WideString); dispid 1610743813;
    procedure GetDescription(out pbszDescription: WideString); dispid 1610743814;
    procedure GetTTSSpeed(out pdwSpeed: Integer); dispid 1610743815;
    procedure GetTTSPitch(out pwPitch: Smallint); dispid 1610743816;
    procedure Activate(sState: Smallint); dispid 1610743817;
    procedure SetIdleOn(bOn: Integer); dispid 1610743818;
    procedure GetIdleOn(out pbOn: Integer); dispid 1610743819;
    procedure Prepare(dwType: Integer; const bszName: WideString; bQueue: Integer; 
                      out pdwReqID: Integer); dispid 1610743820;
    procedure Play(const bszAnimation: WideString; out pdwReqID: Integer); dispid 1610743821;
    procedure Stop(dwReqID: Integer); dispid 1610743822;
    procedure StopAll(lTypes: Integer); dispid 1610743823;
    procedure Wait(dwReqID: Integer; out pdwReqID: Integer); dispid 1610743824;
    procedure Interrupt(dwReqID: Integer; out pdwReqID: Integer); dispid 1610743825;
    procedure Show(bFast: Integer; out pdwReqID: Integer); dispid 1610743826;
    procedure Hide(bFast: Integer; out pdwReqID: Integer); dispid 1610743827;
    procedure Speak(const bszText: WideString; const bszUrl: WideString; out pdwReqID: Integer); dispid 1610743828;
    procedure MoveTo(x: Smallint; y: Smallint; lSpeed: Integer; out pdwReqID: Integer); dispid 1610743829;
    procedure GestureAt(x: Smallint; y: Smallint; out pdwReqID: Integer); dispid 1610743830;
    procedure GetMoveCause(out pdwCause: Integer); dispid 1610743831;
    procedure GetVisibilityCause(out pdwCause: Integer); dispid 1610743832;
    procedure HasOtherClients(out plNumOtherClients: Integer); dispid 1610743833;
    procedure SetSoundEffectsOn(bOn: Integer); dispid 1610743834;
    procedure GetSoundEffectsOn(out pbOn: Integer); dispid 1610743835;
    procedure SetName(const bszName: WideString); dispid 1610743836;
    procedure SetDescription(const bszDescription: WideString); dispid 1610743837;
    procedure GetExtraData(out pbszExtraData: WideString); dispid 1610743838;
  end;

// *********************************************************************//
// Interface: IAgentCharacterEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {98BBE491-2EED-11D1-ACAC-00C04FD97575}
// *********************************************************************//
  IAgentCharacterEx = interface(IAgentCharacter)
    ['{98BBE491-2EED-11D1-ACAC-00C04FD97575}']
    procedure ShowPopupMenu(x: Smallint; y: Smallint); safecall;
    procedure SetAutoPopupMenu(bAutoPopupMenu: Integer); safecall;
    procedure GetAutoPopupMenu(out pbAutoPopupMenu: Integer); safecall;
    procedure GetHelpFileName(out pbszName: WideString); safecall;
    procedure SetHelpFileName(const bszName: WideString); safecall;
    procedure SetHelpModeOn(bHelpModeOn: Integer); safecall;
    procedure GetHelpModeOn(out pbHelpModeOn: Integer); safecall;
    procedure SetHelpContextID(ulID: Integer); safecall;
    procedure GetHelpContextID(out pulID: Integer); safecall;
    procedure GetActive(out psState: Smallint); safecall;
    procedure Listen(bListen: Integer); safecall;
    procedure SetLanguageID(langid: Integer); safecall;
    procedure GetLanguageID(out plangid: Integer); safecall;
    procedure GetTTSModeID(out pbszModeID: WideString); safecall;
    procedure SetTTSModeID(const bszModeID: WideString); safecall;
    procedure GetSRModeID(out pbszModeID: WideString); safecall;
    procedure SetSRModeID(const bszModeID: WideString); safecall;
    procedure GetGUID(out pbszID: WideString); safecall;
    procedure GetOriginalSize(out plWidth: Integer; out plHeight: Integer); safecall;
    procedure Think(const bszText: WideString; out pdwReqID: Integer); safecall;
    procedure GetVersion(out psMajor: Smallint; out psMinor: Smallint); safecall;
    procedure GetAnimationNames(out punkEnum: IUnknown); safecall;
    procedure GetSRStatus(out plStatus: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCharacterExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {98BBE491-2EED-11D1-ACAC-00C04FD97575}
// *********************************************************************//
  IAgentCharacterExDisp = dispinterface
    ['{98BBE491-2EED-11D1-ACAC-00C04FD97575}']
    procedure ShowPopupMenu(x: Smallint; y: Smallint); dispid 1610809344;
    procedure SetAutoPopupMenu(bAutoPopupMenu: Integer); dispid 1610809345;
    procedure GetAutoPopupMenu(out pbAutoPopupMenu: Integer); dispid 1610809346;
    procedure GetHelpFileName(out pbszName: WideString); dispid 1610809347;
    procedure SetHelpFileName(const bszName: WideString); dispid 1610809348;
    procedure SetHelpModeOn(bHelpModeOn: Integer); dispid 1610809349;
    procedure GetHelpModeOn(out pbHelpModeOn: Integer); dispid 1610809350;
    procedure SetHelpContextID(ulID: Integer); dispid 1610809351;
    procedure GetHelpContextID(out pulID: Integer); dispid 1610809352;
    procedure GetActive(out psState: Smallint); dispid 1610809353;
    procedure Listen(bListen: Integer); dispid 1610809354;
    procedure SetLanguageID(langid: Integer); dispid 1610809355;
    procedure GetLanguageID(out plangid: Integer); dispid 1610809356;
    procedure GetTTSModeID(out pbszModeID: WideString); dispid 1610809357;
    procedure SetTTSModeID(const bszModeID: WideString); dispid 1610809358;
    procedure GetSRModeID(out pbszModeID: WideString); dispid 1610809359;
    procedure SetSRModeID(const bszModeID: WideString); dispid 1610809360;
    procedure GetGUID(out pbszID: WideString); dispid 1610809361;
    procedure GetOriginalSize(out plWidth: Integer; out plHeight: Integer); dispid 1610809362;
    procedure Think(const bszText: WideString; out pdwReqID: Integer); dispid 1610809363;
    procedure GetVersion(out psMajor: Smallint; out psMinor: Smallint); dispid 1610809364;
    procedure GetAnimationNames(out punkEnum: IUnknown); dispid 1610809365;
    procedure GetSRStatus(out plStatus: Integer); dispid 1610809366;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743808;
    procedure SetPosition(lLeft: Integer; lTop: Integer); dispid 1610743809;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); dispid 1610743810;
    procedure SetSize(lWidth: Integer; lHeight: Integer); dispid 1610743811;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); dispid 1610743812;
    procedure GetName(out pbszName: WideString); dispid 1610743813;
    procedure GetDescription(out pbszDescription: WideString); dispid 1610743814;
    procedure GetTTSSpeed(out pdwSpeed: Integer); dispid 1610743815;
    procedure GetTTSPitch(out pwPitch: Smallint); dispid 1610743816;
    procedure Activate(sState: Smallint); dispid 1610743817;
    procedure SetIdleOn(bOn: Integer); dispid 1610743818;
    procedure GetIdleOn(out pbOn: Integer); dispid 1610743819;
    procedure Prepare(dwType: Integer; const bszName: WideString; bQueue: Integer; 
                      out pdwReqID: Integer); dispid 1610743820;
    procedure Play(const bszAnimation: WideString; out pdwReqID: Integer); dispid 1610743821;
    procedure Stop(dwReqID: Integer); dispid 1610743822;
    procedure StopAll(lTypes: Integer); dispid 1610743823;
    procedure Wait(dwReqID: Integer; out pdwReqID: Integer); dispid 1610743824;
    procedure Interrupt(dwReqID: Integer; out pdwReqID: Integer); dispid 1610743825;
    procedure Show(bFast: Integer; out pdwReqID: Integer); dispid 1610743826;
    procedure Hide(bFast: Integer; out pdwReqID: Integer); dispid 1610743827;
    procedure Speak(const bszText: WideString; const bszUrl: WideString; out pdwReqID: Integer); dispid 1610743828;
    procedure MoveTo(x: Smallint; y: Smallint; lSpeed: Integer; out pdwReqID: Integer); dispid 1610743829;
    procedure GestureAt(x: Smallint; y: Smallint; out pdwReqID: Integer); dispid 1610743830;
    procedure GetMoveCause(out pdwCause: Integer); dispid 1610743831;
    procedure GetVisibilityCause(out pdwCause: Integer); dispid 1610743832;
    procedure HasOtherClients(out plNumOtherClients: Integer); dispid 1610743833;
    procedure SetSoundEffectsOn(bOn: Integer); dispid 1610743834;
    procedure GetSoundEffectsOn(out pbOn: Integer); dispid 1610743835;
    procedure SetName(const bszName: WideString); dispid 1610743836;
    procedure SetDescription(const bszDescription: WideString); dispid 1610743837;
    procedure GetExtraData(out pbszExtraData: WideString); dispid 1610743838;
  end;

// *********************************************************************//
// Interface: IAgentEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48D12BA0-5B77-11D1-9EC1-00C04FD7081F}
// *********************************************************************//
  IAgentEx = interface(IAgent)
    ['{48D12BA0-5B77-11D1-9EC1-00C04FD7081F}']
    procedure GetCharacterEx(dwCharID: Integer; out ppCharacterEx: IAgentCharacterEx); safecall;
    procedure GetVersion(out psMajor: Smallint; out psMinor: Smallint); safecall;
    procedure ShowDefaultCharacterProperties(x: Smallint; y: Smallint; bUseDefaultPosition: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48D12BA0-5B77-11D1-9EC1-00C04FD7081F}
// *********************************************************************//
  IAgentExDisp = dispinterface
    ['{48D12BA0-5B77-11D1-9EC1-00C04FD7081F}']
    procedure GetCharacterEx(dwCharID: Integer; out ppCharacterEx: IAgentCharacterEx); dispid 1610809344;
    procedure GetVersion(out psMajor: Smallint; out psMinor: Smallint); dispid 1610809345;
    procedure ShowDefaultCharacterProperties(x: Smallint; y: Smallint; bUseDefaultPosition: Integer); dispid 1610809346;
    procedure Load(vLoadKey: OleVariant; out pdwCharID: Integer; out pdwReqID: Integer); dispid 1610743808;
    procedure Unload(dwCharID: Integer); dispid 1610743809;
    procedure Register(const punkNotifySink: IUnknown; out pdwSinkID: Integer); dispid 1610743810;
    procedure Unregister(dwSinkID: Integer); dispid 1610743811;
    procedure GetCharacter(dwCharID: Integer; out ppunkCharacter: IDispatch); dispid 1610743812;
    procedure GetSuspended(out pbSuspended: Integer); dispid 1610743813;
  end;

// *********************************************************************//
// Interface: IAgentNotifySink
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00D18159-8466-11D0-AC63-00C04FD97575}
// *********************************************************************//
  IAgentNotifySink = interface(IDispatch)
    ['{00D18159-8466-11D0-AC63-00C04FD97575}']
    procedure Command(dwCommandID: Integer; const punkUserInput: IUnknown); safecall;
    procedure ActivateInputState(dwCharID: Integer; bActivated: Integer); safecall;
    procedure Restart; safecall;
    procedure Shutdown; safecall;
    procedure VisibleState(dwCharID: Integer; bVisible: Integer; dwCause: Integer); safecall;
    procedure Click(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); safecall;
    procedure DblClick(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); safecall;
    procedure DragStart(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); safecall;
    procedure DragComplete(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); safecall;
    procedure RequestStart(dwRequestID: Integer); safecall;
    procedure RequestComplete(dwRequestID: Integer; hrStatus: Integer); safecall;
    procedure BookMark(dwBookMarkID: Integer); safecall;
    procedure Idle(dwCharID: Integer; bStart: Integer); safecall;
    procedure Move(dwCharID: Integer; x: Integer; y: Integer; dwCause: Integer); safecall;
    procedure Size(dwCharID: Integer; lWidth: Integer; lHeight: Integer); safecall;
    procedure BalloonVisibleState(dwCharID: Integer; bVisible: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentNotifySinkDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {00D18159-8466-11D0-AC63-00C04FD97575}
// *********************************************************************//
  IAgentNotifySinkDisp = dispinterface
    ['{00D18159-8466-11D0-AC63-00C04FD97575}']
    procedure Command(dwCommandID: Integer; const punkUserInput: IUnknown); dispid 1610743808;
    procedure ActivateInputState(dwCharID: Integer; bActivated: Integer); dispid 1610743809;
    procedure Restart; dispid 1610743810;
    procedure Shutdown; dispid 1610743811;
    procedure VisibleState(dwCharID: Integer; bVisible: Integer; dwCause: Integer); dispid 1610743812;
    procedure Click(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743813;
    procedure DblClick(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743814;
    procedure DragStart(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743815;
    procedure DragComplete(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743816;
    procedure RequestStart(dwRequestID: Integer); dispid 1610743817;
    procedure RequestComplete(dwRequestID: Integer; hrStatus: Integer); dispid 1610743818;
    procedure BookMark(dwBookMarkID: Integer); dispid 1610743819;
    procedure Idle(dwCharID: Integer; bStart: Integer); dispid 1610743820;
    procedure Move(dwCharID: Integer; x: Integer; y: Integer; dwCause: Integer); dispid 1610743821;
    procedure Size(dwCharID: Integer; lWidth: Integer; lHeight: Integer); dispid 1610743822;
    procedure BalloonVisibleState(dwCharID: Integer; bVisible: Integer); dispid 1610743823;
  end;

// *********************************************************************//
// Interface: IAgentNotifySinkEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08C75162-3C9C-11D1-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentNotifySinkEx = interface(IAgentNotifySink)
    ['{08C75162-3C9C-11D1-91FE-00C04FD701A5}']
    procedure HelpComplete(dwCharID: Integer; dwCommandID: Integer; dwCause: Integer); safecall;
    procedure ListeningState(dwCharID: Integer; bListening: Integer; dwCause: Integer); safecall;
    procedure DefaultCharacterChange(const bszGUID: WideString); safecall;
    procedure AgentPropertyChange; safecall;
    procedure ActiveClientChange(dwCharID: Integer; lStatus: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentNotifySinkExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08C75162-3C9C-11D1-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentNotifySinkExDisp = dispinterface
    ['{08C75162-3C9C-11D1-91FE-00C04FD701A5}']
    procedure HelpComplete(dwCharID: Integer; dwCommandID: Integer; dwCause: Integer); dispid 1610809344;
    procedure ListeningState(dwCharID: Integer; bListening: Integer; dwCause: Integer); dispid 1610809345;
    procedure DefaultCharacterChange(const bszGUID: WideString); dispid 1610809346;
    procedure AgentPropertyChange; dispid 1610809347;
    procedure ActiveClientChange(dwCharID: Integer; lStatus: Integer); dispid 1610809348;
    procedure Command(dwCommandID: Integer; const punkUserInput: IUnknown); dispid 1610743808;
    procedure ActivateInputState(dwCharID: Integer; bActivated: Integer); dispid 1610743809;
    procedure Restart; dispid 1610743810;
    procedure Shutdown; dispid 1610743811;
    procedure VisibleState(dwCharID: Integer; bVisible: Integer; dwCause: Integer); dispid 1610743812;
    procedure Click(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743813;
    procedure DblClick(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743814;
    procedure DragStart(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743815;
    procedure DragComplete(dwCharID: Integer; fwKeys: Smallint; x: Integer; y: Integer); dispid 1610743816;
    procedure RequestStart(dwRequestID: Integer); dispid 1610743817;
    procedure RequestComplete(dwRequestID: Integer; hrStatus: Integer); dispid 1610743818;
    procedure BookMark(dwBookMarkID: Integer); dispid 1610743819;
    procedure Idle(dwCharID: Integer; bStart: Integer); dispid 1610743820;
    procedure Move(dwCharID: Integer; x: Integer; y: Integer; dwCause: Integer); dispid 1610743821;
    procedure Size(dwCharID: Integer; lWidth: Integer; lHeight: Integer); dispid 1610743822;
    procedure BalloonVisibleState(dwCharID: Integer; bVisible: Integer); dispid 1610743823;
  end;

// *********************************************************************//
// Interface: IAgentCommandWindow
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6D0ECB23-9968-11D0-AC6E-00C04FD97575}
// *********************************************************************//
  IAgentCommandWindow = interface(IDispatch)
    ['{6D0ECB23-9968-11D0-AC6E-00C04FD97575}']
    procedure SetVisible(bVisible: Integer); safecall;
    procedure GetVisible(out pbVisible: Integer); safecall;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); safecall;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAgentCommandWindowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6D0ECB23-9968-11D0-AC6E-00C04FD97575}
// *********************************************************************//
  IAgentCommandWindowDisp = dispinterface
    ['{6D0ECB23-9968-11D0-AC6E-00C04FD97575}']
    procedure SetVisible(bVisible: Integer); dispid 1610743808;
    procedure GetVisible(out pbVisible: Integer); dispid 1610743809;
    procedure GetPosition(out plLeft: Integer; out plTop: Integer); dispid 1610743810;
    procedure GetSize(out plWidth: Integer; out plHeight: Integer); dispid 1610743811;
  end;

// *********************************************************************//
// The Class CoAgentServer provides a Create and CreateRemote method to          
// create instances of the default interface IAgent exposed by              
// the CoClass AgentServer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAgentServer = class
    class function Create: IAgent;
    class function CreateRemote(const MachineName: string): IAgent;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAgentServer
// Help String      : Agent Class
// Default Interface: IAgent
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAgentServerProperties= class;
{$ENDIF}
  TAgentServer = class(TOleServer)
  private
    FIntf:        IAgent;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TAgentServerProperties;
    function      GetServerProperties: TAgentServerProperties;
{$ENDIF}
    function      GetDefaultInterface: IAgent;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAgent);
    procedure Disconnect; override;
    procedure Load(vLoadKey: OleVariant; out pdwCharID: Integer; out pdwReqID: Integer);
    procedure Unload(dwCharID: Integer);
    procedure Register(const punkNotifySink: IUnknown; out pdwSinkID: Integer);
    procedure Unregister(dwSinkID: Integer);
    procedure GetCharacter(dwCharID: Integer; out ppunkCharacter: IDispatch);
    procedure GetSuspended(out pbSuspended: Integer);
    property  DefaultInterface: IAgent read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAgentServerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TAgentServer
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TAgentServerProperties = class(TPersistent)
  private
    FServer:    TAgentServer;
    function    GetDefaultInterface: IAgent;
    constructor Create(AServer: TAgentServer);
  protected
  public
    property DefaultInterface: IAgent read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoAgentServer.Create: IAgent;
begin
  Result := CreateComObject(CLASS_AgentServer) as IAgent;
end;

class function CoAgentServer.CreateRemote(const MachineName: string): IAgent;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AgentServer) as IAgent;
end;

procedure TAgentServer.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D45FD2FC-5C6E-11D1-9EC1-00C04FD7081F}';
    IntfIID:   '{A7B93C91-7B81-11D0-AC5F-00C04FD97575}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAgentServer.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAgent;
  end;
end;

procedure TAgentServer.ConnectTo(svrIntf: IAgent);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAgentServer.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAgentServer.GetDefaultInterface: IAgent;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TAgentServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAgentServerProperties.Create(Self);
{$ENDIF}
end;

destructor TAgentServer.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TAgentServer.GetServerProperties: TAgentServerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TAgentServer.Load(vLoadKey: OleVariant; out pdwCharID: Integer; out pdwReqID: Integer);
begin
  DefaultInterface.Load(vLoadKey, pdwCharID, pdwReqID);
end;

procedure TAgentServer.Unload(dwCharID: Integer);
begin
  DefaultInterface.Unload(dwCharID);
end;

procedure TAgentServer.Register(const punkNotifySink: IUnknown; out pdwSinkID: Integer);
begin
  DefaultInterface.Register(punkNotifySink, pdwSinkID);
end;

procedure TAgentServer.Unregister(dwSinkID: Integer);
begin
  DefaultInterface.Unregister(dwSinkID);
end;

procedure TAgentServer.GetCharacter(dwCharID: Integer; out ppunkCharacter: IDispatch);
begin
  DefaultInterface.GetCharacter(dwCharID, ppunkCharacter);
end;

procedure TAgentServer.GetSuspended(out pbSuspended: Integer);
begin
  DefaultInterface.GetSuspended(pbSuspended);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAgentServerProperties.Create(AServer: TAgentServer);
begin
  inherited Create;
  FServer := AServer;
end;

function TAgentServerProperties.GetDefaultInterface: IAgent;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ActiveX',[TAgentServer]);
end;

end.
