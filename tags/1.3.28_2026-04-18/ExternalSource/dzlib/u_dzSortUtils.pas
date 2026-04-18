unit u_dzSortUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  Classes;

type
  ///<summary>
  /// raised in the sorted list templates instead of EListError so it is possible to
  /// specifically ignore these exceptions in the debugger if they are handled in code </summary>
  EdzListError = class(EListError)
  end;

// To use any of the sorting algorithms, you need to supply either the two callbacks
// TCompareItemsMeth and TSwapItemsMeth or pass a class implementing IQSDataHandler
// ("QS" stands for "QuickSort" from the time when there was only one sorting algorithm)

type
  ///<summary>
  /// Compare items at the given indexes and return values similar to the standard Delphi
  /// Compare functions e.g. CompareStr or CompareText
  /// @returns 0 if they are equal
  ///          <0 if Item[Idx1] < Item[Idx2]
  ///          >0 if Item[Idx1] > Item[Idx2] </summary>
  TCompareItemsMeth32 = function(_Idx1, _Idx2: Integer): Integer of object;
  TCompareItemsMeth = TCompareItemsMeth32;
  TCompareItemsMeth64 = function(_Idx1, _Idx2: Int64): Integer of object;
  ///<summary>
  /// Swap the items at the given indexes </summary>
  TSwapItemsMeth32 = procedure(_Idx1, _Idx2: Integer) of object;
  TSwapItemsMeth = TSwapItemsMeth32;
  TSwapItemsMeth64 = procedure(_Idx1, _Idx2: Int64) of object;

type
  ///<summary>
  /// Interface to be implemented for using the sorting algorithms </summary>
  ISortDataHandler = interface ['{C7B22837-F9C0-4228-A2E3-DC8BBF27DBA9}']
    /// Compare items at the given indexes and return values similar to the standard Delphi
    /// Compare functions e.g. CompareStr or CompareText
    /// @returns 0 if they are equal
    ///          <0 if Item[Idx1] < Item[Idx2]
    ///          >0 if Item[Idx1] > Item[Idx2] </summary>
    function Compare(_Idx1, _Idx2: Integer): Integer;
    ///<summary>
    /// Swap the items at the given indexes </summary>
    procedure Swap(_Idx1, _Idx2: Integer);
  end;

type
  ///<summary>
  /// Adds a Count method to the ISortDataHander interface </summary>
  ISortDataHandlerEx = interface(ISortDataHandler)['{C2DD7397-C0C3-4519-AA24-7FB2EF559DAD}']
    function Count: Integer;
  end;

type
  IQSDataHandler = ISortDataHandler
{$IFDEF SUPPORTS_DEPRECATED_TYPES}
  deprecated // use ISortDataHandler instead
{$ENDIF}
  ;

type
  ///<summary>
  /// Adapter class to wrap callback methods (TCompareItemsMeth and TSwapItemsMeth) as
  /// ISortDataHandler interface. This allows sorting algorithms to have a single implementation
  /// that works with both callback and interface-based callers.
  /// Used internally by sorting algorithm units to avoid code duplication. </summary>
  TSortDataHandlerAdapter = class(TInterfacedObject, ISortDataHandler)
  private
    FCompareMeth: TCompareItemsMeth;
    FSwapMeth: TSwapItemsMeth;
  public
    constructor Create(_CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth);
    function Compare(_Idx1, _Idx2: Integer): Integer;
    procedure Swap(_Idx1, _Idx2: Integer);
  end;

type
  ///<summary>
  /// Adapter class to wrap 64-bit callback methods (TCompareItemsMeth64 and TSwapItemsMeth64) as
  /// ISortDataHandler interface. </summary>
  TSortDataHandlerAdapter64 = class(TInterfacedObject, ISortDataHandler)
  private
    FCompareMeth: TCompareItemsMeth64;
    FSwapMeth: TSwapItemsMeth64;
  public
    constructor Create(_CompareMeth: TCompareItemsMeth64; _SwapMeth: TSwapItemsMeth64);
    function Compare(_Idx1, _Idx2: Integer): Integer;
    procedure Swap(_Idx1, _Idx2: Integer);
  end;

///<summary>
/// Try to optimize the pivot by chosing the median of Left, Right and ((Left + Right) shr 1) </summary>
function GetPivot(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth32): Integer; {$IFDEF SupportsInline} inline; {$ENDIF} overload;
function GetPivot(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth64): Integer; {$IFDEF SupportsInline} inline; {$ENDIF} overload;
///<summary>
/// Try to optimize the pivot by chosing the median of Left, Right and ((Left + Right) shr 1) </summary>
function GetPivot(_Left, _Right: Integer; _DataHandler: ISortDataHandler): Integer; {$IFDEF SupportsInline} inline; {$ENDIF} overload;

implementation

{ TSortDataHandlerAdapter }

constructor TSortDataHandlerAdapter.Create(_CompareMeth: TCompareItemsMeth; _SwapMeth: TSwapItemsMeth);
begin
  inherited Create;
  FCompareMeth := _CompareMeth;
  FSwapMeth := _SwapMeth;
end;

function TSortDataHandlerAdapter.Compare(_Idx1, _Idx2: Integer): Integer;
begin
  Result := FCompareMeth(_Idx1, _Idx2);
end;

procedure TSortDataHandlerAdapter.Swap(_Idx1, _Idx2: Integer);
begin
  FSwapMeth(_Idx1, _Idx2);
end;

{ TSortDataHandlerAdapter64 }

constructor TSortDataHandlerAdapter64.Create(_CompareMeth: TCompareItemsMeth64; _SwapMeth: TSwapItemsMeth64);
begin
  inherited Create;
  FCompareMeth := _CompareMeth;
  FSwapMeth := _SwapMeth;
end;

function TSortDataHandlerAdapter64.Compare(_Idx1, _Idx2: Integer): Integer;
begin
  Result := FCompareMeth(_Idx1, _Idx2);
end;

procedure TSortDataHandlerAdapter64.Swap(_Idx1, _Idx2: Integer);
begin
  FSwapMeth(_Idx1, _Idx2);
end;

function GetPivot(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth32): Integer;
begin
  Result := (_Left + _Right) shr 1;
  // try to optimize the pivot by chosing the
  // median of Left, Right and Result:
  if  _CompareMeth(_Left, Result) > 0 then begin
    if  _CompareMeth(Result, _Right) > 0 then begin
      // Result is already the median
    end else if  _CompareMeth(_Right, _Left) > 0 then begin
      Result := _Left;
    end else
      Result := _Right;
  end else begin
    if  _CompareMeth(_Right, Result) > 0 then begin
      // Result is already the median
    end else if  _CompareMeth(_Left, _Right) > 0 then begin
      Result := _Left;
    end else begin
      Result := _Right;
    end;
  end;
end;

function GetPivot(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth64): Integer;
begin
  Result := (_Left + _Right) shr 1;
  // try to optimize the pivot by chosing the
  // median of Left, Right and Result:
  if  _CompareMeth(_Left, Result) > 0 then begin
    if  _CompareMeth(Result, _Right) > 0 then begin
      // Result is already the median
    end else if  _CompareMeth(_Right, _Left) > 0 then begin
      Result := _Left;
    end else
      Result := _Right;
  end else begin
    if  _CompareMeth(_Right, Result) > 0 then begin
      // Result is already the median
    end else if  _CompareMeth(_Left, _Right) > 0 then begin
      Result := _Left;
    end else begin
      Result := _Right;
    end;
  end;
end;

function GetPivot(_Left, _Right: Integer; _DataHandler: ISortDataHandler): Integer;
begin
  Result := (_Left + _Right) shr 1;
  // try to optimize the pivot by chosing the
  // median of Left, Right and Result:
  if  _DataHandler.Compare(_Left, Result) > 0 then begin
    if  _DataHandler.Compare(Result, _Right) > 0 then begin
      // Result is already the median
    end else if  _DataHandler.Compare(_Right, _Left) > 0 then begin
      Result := _Left;
    end else
      Result := _Right;
  end else begin
    if  _DataHandler.Compare(_Right, Result) > 0 then begin
      // Result is already the median
    end else if  _DataHandler.Compare(_Left, _Right) > 0 then begin
      Result := _Left;
    end else begin
      Result := _Right;
    end;
  end;
end;

end.
