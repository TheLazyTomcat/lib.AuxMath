unit AuxMath;
{
  AuxMath_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and only this unit will be compiled in PurePascal mode.
}
{$IFDEF AuxMath_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}
{.$DEFINE PurePascal}

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------

{$IFOPT Q+}
  {$DEFINE AM_OverflowChecks}
{$ELSE}
  {$UNDEF AM_OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils,
  AuxTypes;


{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EAMException = class(Exception);

  EAMInvalidOperation = class(EAMException);

{===============================================================================
--------------------------------------------------------------------------------
                          Combined division and modulo
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs integer division and modulo as one operation, so there is no need to
  call them separately when both quotient and remainder are required.
}

procedure iDivMod(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure uDivMod(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure DivMod(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivMod(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivMod(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivMod(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                          Combined division and ceiling
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and Ceil as one optimized operation (the calculation does
  not use floating point unit/numbers, only integers). 
}

Function iDivCeil(Dividend,Divisor: Int8): Int8; overload;
Function iDivCeil(Dividend,Divisor: Int16): Int16; overload;
Function iDivCeil(Dividend,Divisor: Int32): Int32; overload;
Function iDivCeil(Dividend,Divisor: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uDivCeil(Dividend,Divisor: UInt8): UInt8; overload;
Function uDivCeil(Dividend,Divisor: UInt16): UInt16; overload;
Function uDivCeil(Dividend,Divisor: UInt32): UInt32; overload;
Function uDivCeil(Dividend,Divisor: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function DivCeil(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeil(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeil(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeil(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                           Combined division and floor
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and Floor as one optimized operation (the calculation does
  not utilize floating point unit/numbers).
}

Function iDivFloor(Dividend,Divisor: Int8): Int8; overload;
Function iDivFloor(Dividend,Divisor: Int16): Int16; overload;
Function iDivFloor(Dividend,Divisor: Int32): Int32; overload;
Function iDivFloor(Dividend,Divisor: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uDivFloor(Dividend,Divisor: UInt8): UInt8; overload;
Function uDivFloor(Dividend,Divisor: UInt16): UInt16; overload;
Function uDivFloor(Dividend,Divisor: UInt32): UInt32; overload;
Function uDivFloor(Dividend,Divisor: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function DivFloor(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloor(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloor(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloor(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              64bit ceil and floor                             
--------------------------------------------------------------------------------
===============================================================================}
{
  Standard Ceil and Floor functions are returning only Integers - that is,
  32bit numbers. Following functions are here for situations, where 64bit wide
  integers are required.
  
  Note that Trunc already does return 64bit integer. 
}

Function Ceil64(N: Extended): Int64;

Function Floor64(N: Extended): Int64;

{===============================================================================
--------------------------------------------------------------------------------
                         Is positive integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns true when given number is a positive integer power of 2, false
  otherwise.     
  Note that zero and negative numbers cannot be positive integer power of any
  base, therefore in those cases false is returned.
}

Function iIsPow2(N: Int8): Boolean; overload;
Function iIsPow2(N: Int16): Boolean; overload;
Function iIsPow2(N: Int32): Boolean; overload;
Function iIsPow2(N: Int64): Boolean; overload;

//------------------------------------------------------------------------------

Function uIsPow2(N: UInt8): Boolean; overload;
Function uIsPow2(N: UInt16): Boolean; overload;
Function uIsPow2(N: UInt32): Boolean; overload;
Function uIsPow2(N: UInt64): Boolean; overload;

//------------------------------------------------------------------------------

Function IsPow2(N: Int8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IsPow2(N: Int16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IsPow2(N: Int32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IsPow2(N: Int64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                            Integer logarithm base 2
--------------------------------------------------------------------------------
===============================================================================}
{
  If the given number is a positive integer power of 2, then [i/u]IntLog2 will
  return the exponent.
  If the number is zero, negative (for signed integers), or is generally not an
  integer power of 2, then it will return -1.
}

Function iIntLog2(N: Int8): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function iIntLog2(N: Int16): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function iIntLog2(N: Int32): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function iIntLog2(N: Int64): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

Function uIntLog2(N: UInt8): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function uIntLog2(N: UInt16): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function uIntLog2(N: UInt32): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function uIntLog2(N: UInt64): Int32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

Function IntLog2(N: Int8): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IntLog2(N: Int16): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IntLog2(N: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IntLog2(N: Int64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
             Try combined division and modulo by integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{
  If divisor is a positive integer power of 2, it will perform optimized
  integer division and modulo in one operation, and return true.
  Otherwise it will return false and both output parameters (quotient and
  remainder) are undefined.
}

Function iTryDivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

Function uTryDivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function uTryDivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function uTryDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function uTryDivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

Function TryDivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryDivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryDivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

(*
procedure iDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32; out IsPow2: Boolean); overload;
procedure uDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32; out IsPow2: Boolean); overload;


procedure iDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;
procedure uDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;


procedure iDivModPow2Fast(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;
procedure uDivModPow2Fast(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;
*)

{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of equal types
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns smaller/lower of the two given values.
}

Function iMin(A,B: Int8): Int8; overload;
Function iMin(A,B: Int16): Int16; overload;
Function iMin(A,B: Int32): Int32; overload;
Function iMin(A,B: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uMin(A,B: UInt8): UInt8; overload;
Function uMin(A,B: UInt16): UInt16; overload;
Function uMin(A,B: UInt32): UInt32; overload;
Function uMin(A,B: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function fMin(A,B: Extended): Extended; overload;
Function fMin(A,B: Currency): Currency; overload;
Function fMin(A,B: TDateTime): TDateTime; overload;
(*
Function Min(A,B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Min(A,B: Extended): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: Currency): Currency; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: TDateTime): TDateTime; overload;{$IFDEF CanInline} inline;{$ENDIF}
*)
{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of equal types
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns bigger/higher of the two given values.
}

Function iMax(A,B: Int8): Int8; overload;
Function iMax(A,B: Int16): Int16; overload;
Function iMax(A,B: Int32): Int32; overload;
Function iMax(A,B: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uMax(A,B: UInt8): UInt8; overload;
Function uMax(A,B: UInt16): UInt16; overload;
Function uMax(A,B: UInt32): UInt32; overload;
Function uMax(A,B: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function fMax(A,B: Extended): Extended; overload;

Function fMax(A,B: Currency): Currency; overload;

Function fMax(A,B: TDateTime): TDateTime; overload;

{===============================================================================
--------------------------------------------------------------------------------
                           Minimum of differing types                                                  
--------------------------------------------------------------------------------
===============================================================================}
(*
Function iiMin(A: Int8; B: Int16): Int8; overload;
Function iiMin(A: Int8; B: Int32): Int8; overload;
Function iiMin(A: Int8; B: Int64): Int8; overload;

Function iiMin(A: Int16; B: Int8): Int16; overload;
Function iiMin(A: Int16; B: Int32): Int16; overload;
Function iiMin(A: Int16; B: Int64): Int16; overload;

Function iiMin(A: Int32; B: Int8): Int32; overload;
Function iiMin(A: Int32; B: Int16): Int32; overload;
Function iiMin(A: Int32; B: Int64): Int32; overload;

Function iiMin(A: Int64; B: Int8): Int64; overload;
Function iiMin(A: Int64; B: Int16): Int64; overload;
Function iiMin(A: Int64; B: Int32): Int64; overload;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int8; B: UInt8): Int8; overload;
Function iuMin(A: Int8; B: UInt16): Int8; overload;
Function iuMin(A: Int8; B: UInt32): Int8; overload;
Function iuMin(A: Int8; B: UInt64): Int8; overload;

Function iuMin(A: Int16; B: UInt8): Int16; overload;
Function iuMin(A: Int16; B: UInt16): Int16; overload;
Function iuMin(A: Int16; B: UInt32): Int16; overload;
Function iuMin(A: Int16; B: UInt64): Int16; overload;

Function iuMin(A: Int32; B: UInt8): Int32; overload;
Function iuMin(A: Int32; B: UInt16): Int32; overload;
Function iuMin(A: Int32; B: UInt32): Int32; overload;
Function iuMin(A: Int32; B: UInt64): Int32; overload;

Function iuMin(A: Int64; B: UInt8): Int64; overload;
Function iuMin(A: Int64; B: UInt16): Int64; overload;
Function iuMin(A: Int64; B: UInt32): Int64; overload;
Function iuMin(A: Int64; B: UInt64): Int64; overload;

//------------------------------------------------------------------------------

Function uiMin(A: UInt8; B: Int8): UInt8; overload;
Function uiMin(A: UInt8; B: Int16): UInt8; overload;
Function uiMin(A: UInt8; B: Int32): UInt8; overload;
Function uiMin(A: UInt8; B: Int64): UInt8; overload;

Function uiMin(A: UInt16; B: Int8): UInt16; overload;
Function uiMin(A: UInt16; B: Int16): UInt16; overload;
Function uiMin(A: UInt16; B: Int32): UInt16; overload;
Function uiMin(A: UInt16; B: Int64): UInt16; overload;

Function uiMin(A: UInt32; B: Int8): UInt32; overload;
Function uiMin(A: UInt32; B: Int16): UInt32; overload;
Function uiMin(A: UInt32; B: Int32): UInt32; overload;
Function uiMin(A: UInt32; B: Int64): UInt32; overload;

Function uiMin(A: UInt64; B: Int8): UInt64; overload;
Function uiMin(A: UInt64; B: Int16): UInt64; overload;
Function uiMin(A: UInt64; B: Int32): UInt64; overload;
Function uiMin(A: UInt64; B: Int64): UInt64; overload;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt8; B: UInt16): UInt8; overload;
Function uuMin(A: UInt8; B: UInt32): UInt8; overload;
Function uuMin(A: UInt8; B: UInt64): UInt8; overload;

Function uuMin(A: UInt16; B: UInt8): UInt16; overload;
Function uuMin(A: UInt16; B: UInt32): UInt16; overload;
Function uuMin(A: UInt16; B: UInt64): UInt16; overload;

Function uuMin(A: UInt32; B: UInt8): UInt32; overload;
Function uuMin(A: UInt32; B: UInt16): UInt32; overload;
Function uuMin(A: UInt32; B: UInt64): UInt32; overload;

Function uuMin(A: UInt64; B: UInt8): UInt64; overload;
Function uuMin(A: UInt64; B: UInt16): UInt64; overload;
Function uuMin(A: UInt64; B: UInt32): UInt64; overload;

//------------------------------------------------------------------------------

Function fiMin(A: Extended; B: Int8): Extended; overload;
Function fiMin(A: Extended; B: Int16): Extended; overload;
Function fiMin(A: Extended; B: Int32): Extended; overload;
Function fiMin(A: Extended; B: Int64): Extended; overload;

Function fuMin(A: Extended; B: UInt8): Extended; overload;
Function fuMin(A: Extended; B: UInt16): Extended; overload;
Function fuMin(A: Extended; B: UInt32): Extended; overload;
Function fuMin(A: Extended; B: UInt64): Extended; overload;
*)

implementation

{===============================================================================
    Internals
===============================================================================}    
{$IF not Declared(NativeUInt64E)}
type
  UInt64Rec = packed record
    case Integer of
      0: (Lo, Hi: UInt32);
      1: (Cardinals: array [0..1] of UInt32);
      2: (Words: array [0..3] of UInt16);
      3: (Bytes: array [0..7] of UInt8);
  end;

//------------------------------------------------------------------------------

Function CompareUInt64(A,B: UInt64): Integer;
begin
If UInt64Rec(A).Hi > UInt64Rec(B).Hi then
  Result := +1
else If UInt64Rec(A).Hi < UInt64Rec(B).Hi then
  Result := -1
else
  begin
    // higher 32bits are the same, compare lower 32 bits
    If UInt64Rec(A).Lo > UInt64Rec(B).Lo then
      Result := +1
    else If UInt64Rec(A).Lo < UInt64Rec(B).Lo then
      Result := -1
    else
      Result := 0;
  end;
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                          Combined division and modulo
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivMod - signed integers
-------------------------------------------------------------------------------}

procedure iDivMod(Dividend,Divisor: Int8; out Quotient,Remainder: Int8);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AL              CL           DIL
         Divisor              DL              DL           SIL
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    MOVSX   AX, CL

    IDIV    DL                  // AL := AX div DL          AH := AX mod DL

    MOV     byte ptr [R8], AL
    SHR     AX, 8
    MOV     byte ptr [R9], AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOVSX   AX, DIL

    IDIV    SIL                 // AL := AX div SIL         AH := AX mod SIL

    MOV     byte ptr [RDX], AL
    SHR     AX, 8
    MOV     byte ptr [RCX], AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CBW                         // AX := sign_extend(AL)

    IDIV    DL                  // AL := AX div DL          AH := AX mod DL

    MOV     byte ptr [ECX], AL
    MOV     ECX, dword ptr [Remainder]
    MOV     byte ptr [ECX], AH
    
{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
Remainder := Dividend - (Quotient * Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivMod(Dividend,Divisor: Int16; out Quotient,Remainder: Int16);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AX              CX            DI
         Divisor              DX              DX            SI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}
  
    MOV     AX, CX
    MOV     CX, DX
    CWD                         // DX:AX := sign_extend(AX)

    IDIV    CX                  // AX := DX:AX div CX         DX := DX:AX mod CX

    MOV     word ptr [R8], AX
    MOV     word ptr [R9], DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV     R8, RDX
    MOV     AX, DI
    CWD                         // DX:AX := sign_extend(AX)

    IDIV    SI                  // AX := DX:AX div SI         DX := DX:AX mod SI

    MOV     word ptr [R8], AX
    MOV     word ptr [RCX], DX

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH    EBX

    MOV     BX, DX
    CWD                         // DX:AX := sign_extend(AX)

    IDIV    BX                  // AX := DX:AX div BX         DX := DX:AX mod BX

    MOV     word ptr [ECX], AX
    MOV     ECX, dword ptr [Remainder]
    MOV     word ptr [ECX], DX

    POP     EBX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
Remainder := Dividend - (Quotient * Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivMod(Dividend,Divisor: Int32; out Quotient,Remainder: Int32);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend             EAX             ECX           EDI
         Divisor             EDX             EDX           ESI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}
  
    MOV     EAX, ECX
    MOV     ECX, EDX
    CDQ                         // EDX:EAX := sign_extend(EAX)

    IDIV    ECX                 // EAX := EDX:EAX div ECX     EDX := EDX:EAX mod ECX

    MOV     dword ptr [R8], EAX
    MOV     dword ptr [R9], EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV     R8, RDX
    MOV     EAX, EDI
    CDQ                         // EDX:EAX := sign_extend(EAX)

    IDIV    ESI                 // EAX := EDX:EAX div ESI     EDX := EDX:EAX mod ESI

    MOV     dword ptr [R8], EAX
    MOV     dword ptr [RCX], EDX

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH    EBX

    MOV     EBX, EDX
    CDQ                         // EDX:EAX := sign_extend(EAX)

    IDIV    EBX                 // EAX := EDX:EAX div EBX     EDX := EDX:EAX mod EBX

    MOV     dword ptr [ECX], EAX
    MOV     ECX, dword ptr [Remainder]
    MOV     dword ptr [ECX], EDX

    POP     EBX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
Remainder := Dividend - (Quotient * Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int64; out Quotient,Remainder: Int64);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend          (EBP + 8)          RCX           RDI
         Divisor          (EBP + 16)         RDX           RSI
        Quotient             EAX^             R8^          RDX^
       Remainder             EDX^             R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}
  
    MOV     RAX, RCX
    MOV     RCX, RDX
    CQO                         // RDX:RAX := sign_extend(RAX)

    IDIV    RCX                 // RAX := RDX:RAX div RCX     EDX := EDX:EAX mod ECX

    MOV     qword ptr [R8], RAX
    MOV     qword ptr [R9], RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV     R8, RDX
    MOV     RAX, RDI
    CQO                         // RDX:RAX := sign_extend(RAX)

    IDIV    RSI                 // RAX := RDX:RAX div RSI     RDX := RDX:RAX mod RSI

    MOV     qword ptr [R8], RAX
    MOV     qword ptr [RCX], RDX

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH    EBX
    PUSH    ESI
    PUSH    EDI
    PUSH    EBP

    PUSH    EDX                             // pointer to remainder
    PUSH    EAX                             // pointer to quotient

    MOV     EBX, dword ptr [Divisor]        // divisor low dword
    MOV     ECX, dword ptr [Divisor + 4]    // divisor high dword

    MOV     EDX, dword ptr [Dividend + 4]   // dividend high dword
    MOV     EAX, dword ptr [Dividend]       // dividend low dword

    TEST    ECX, ECX
    JNZ     @CheckOverflow                  // divisor high dword is nonzero, do full division

    TEST    EDX, EDX
    JZ      @Fast32BitDivision              // both dividend and divisor high dwords are zero, do fast division

    TEST    EBX, EBX
    JZ      @Fast32BitDivision              // entire divisor is zero, go for div by zero exception

@CheckOverflow:

    // catch overflows (low(int64) div -1)

    MOV     EBP, EBX
    AND     EBP, ECX
    CMP     EBP, dword(-1)
    JNE     @Full64BitDivision

    TEST    EAX, EAX
    JNZ     @Full64BitDivision
    CMP     EDX, $80000000
    JNE     @Full64BitDivision

    MOV     EBP, $80000000
    DEC     EBP   // sets OF
    INTO          // raises overflow

@Full64BitDivision:
  {
    Store signs of quotient and remainder (into EBP and later to the stack).
    
    EBP bit 0 = sign of the quotient, bit 1 = sign of the remainder.
  }
    XOR     EBP, EBP

@NegateDividend:

    TEST    EDX, EDX
    JNS     @NegateDivisor
    NEG     EDX
    NEG     EAX
    SBB     EDX, 0
    MOV     EBP, 3

@NegateDivisor:

    TEST    ECX, ECX
    JNS     @MainDivision
    NEG     ECX
    NEG     EBX
    SBB     ECX, 0
    XOR     EBP, 1

@MainDivision:

    // prepare for calculations
    PUSH    EBP
    MOV     EBP, ECX
    XOR     ESI, ESI
    XOR     EDI, EDI

    MOV     ECX, 64

  {
    So, current situation is following:

      EAX ... low dword of (possibly negated) dividend
      EDX ... high dword of (possibly negated) dividend

      EBX ... low dword of (possibly negated) divisor
      EBP ... high dword of (possibly negated) divisor

      ECX ... main cycle counter

      ESI ... all zeroes, bits 64..95 of shift register
      EDI ... all zeroes, bits 96..127 of shift register
  }

@MainCalculationLoop:

    // shift register one place left (up)
    SHL     EAX, 1
    RCL     EDX, 1
    RCL     ESI, 1
    RCL     EDI, 1

    // compare higher 64bits of shift register with divisor
    CMP     EDI, EBP
    JB      @MainCalculationLoop_Continue
    JA      @MainCalculationLoop_Sub
    // higher 32bits of divisor and (what will be) remainder are the same...
    CMP     ESI, EBX
    JB      @MainCalculationLoop_Continue

@MainCalculationLoop_Sub:

    SUB     ESI, EBX
    SBB     EDI, EBP
    INC     EAX

@MainCalculationLoop_Continue:

    DEC     ECX
    JNZ     @MainCalculationLoop

  {
    We are done with loop, get signs for results and do correction.
    
    Quotient is in EDX:EAX, remainder is in EDI:ESI.
  }
    POP     EBP

@NegateQuotient:

    TEST    EBP, 1
    JZ      @NegateRemainder

    NEG     EDX
    NEG     EAX
    SBB     EDX, 0

@NegateRemainder:

    TEST    EBP, 2
    JZ      @StoreResult

    NEG     EDI
    NEG     ESI
    SBB     EDI, 0

@StoreResult:

    // store quotient
    POP     ECX
    MOV     dword ptr [ECX], EAX
    MOV     dword ptr [ECX + 4], EDX
    // store remainder
    POP     ECX
    MOV     dword ptr [ECX], ESI
    MOV     dword ptr [ECX + 4], EDI

    JMP     @RoutineEnd

@Fast32BitDivision:

  {
    Here, the dividend is in EAX, EDX is zero and divisor is in EBX. Pointers
    to remainder and quotient are pushed on stack (in that order).
    Since both dividend and divisor must be zero if their higher bits are zero,
    we do unsigned division.
  }

    DIV     EBX                 // EAX := EDX:EAX div EBX     EDX := EDX:EAX mod EBX

    POP     ECX
    MOV     dword ptr [ECX], EAX
    MOV     dword ptr [ECX + 4], 0
    POP     ECX
    MOV     dword ptr [ECX], EDX
    MOV     dword ptr [ECX + 4], 0

@RoutineEnd:

    POP     EBP
    POP     EDI
    POP     ESI    
    POP     EBX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
{
  Following (the multiplication) sometimes signals overflow even when it should
  not (in D7 at least, where __llmulo is called), therefore the overflows are
  explicitly disabled here to prevent problems
}
Remainder := Dividend - (Quotient * Divisor);
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    uDivMod - unsigned integers
-------------------------------------------------------------------------------}

procedure uDivMod(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AL              CL           DIL
         Divisor              DL              DL           SIL
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    MOVZX   AX, CL

    DIV     DL                  // AL := AX div DL          AH := AX mod DL

    MOV     byte ptr [R8], AL
    SHR     AX, 8
    MOV     byte ptr [R9], AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOVZX   AX, DIL

    DIV     SIL                 // AL := AX div SIL         AH := AX mod SIL

    MOV     byte ptr [RDX], AL
    SHR     AX, 8
    MOV     byte ptr [RCX], AL
    
  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    AND     AX, $FF

    DIV     DL                  // AL := AX div DL          AH := AX mod DL

    MOV     byte ptr [ECX], AL
    MOV     ECX, dword ptr [Remainder]
    MOV     byte ptr [ECX], AH

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
Remainder := Dividend - UInt8(Quotient * Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivMod(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AX              CX            DI
         Divisor              DX              DX            SI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}
  
    MOV     AX, CX
    MOV     CX, DX
    XOR     DX, DX

    DIV     CX                  // AX := DX:AX div CX         DX := DX:AX mod CX

    MOV     word ptr [R8], AX
    MOV     word ptr [R9], DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV     R8, RDX
    MOV     AX, DI
    XOR     DX, DX

    DIV     SI                  // AX := DX:AX div SI         DX := DX:AX mod SI

    MOV     word ptr [R8], AX
    MOV     word ptr [RCX], DX

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH    EBX

    MOV     BX, DX
    XOR     DX, DX

    DIV     BX                  // AX := DX:AX div BX         DX := DX:AX mod BX

    MOV     word ptr [ECX], AX
    MOV     ECX, dword ptr [Remainder]
    MOV     word ptr [ECX], DX

    POP     EBX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
Remainder := Dividend - UInt16(Quotient * Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivMod(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend             EAX             ECX           EDI
         Divisor             EDX             EDX           ESI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}
  
    MOV     EAX, ECX
    MOV     ECX, EDX
    XOR     EDX, EDX

    DIV     ECX                 // EAX := EDX:EAX div ECX     EDX := EDX:EAX mod ECX

    MOV     dword ptr [R8], EAX
    MOV     dword ptr [R9], EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV     R8, RDX
    MOV     EAX, EDI
    XOR     EDX, EDX

    DIV    ESI                  // EAX := EDX:EAX div ESI     EDX := EDX:EAX mod ESI

    MOV     dword ptr [R8], EAX
    MOV     dword ptr [RCX], EDX

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH    EBX

    MOV     EBX, EDX
    XOR     EDX, EDX

    DIV    EBX                  // EAX := EDX:EAX div EBX     EDX := EDX:EAX mod EBX

    MOV     dword ptr [ECX], EAX
    MOV     ECX, dword ptr [Remainder]
    MOV     dword ptr [ECX], EDX

    POP     EBX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend div Divisor;
{$IFDEF CPU64bit}
Remainder := Dividend - (Quotient * Divisor);
{$ELSE}
Remainder := UInt32(Dividend - (Int64(Quotient) * Divisor));
{$ENDIF}
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64);
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend          (EBP + 8)          RCX           RDI
         Divisor          (EBP + 16)         RDX           RSI
        Quotient             EAX^             R8^          RDX^
       Remainder             EDX^             R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}
  
    MOV     RAX, RCX
    MOV     RCX, RDX
    XOR     RDX, RDX

    DIV     RCX                 // RAX := RDX:RAX div RCX     EDX := EDX:EAX mod ECX

    MOV     qword ptr [R8], RAX
    MOV     qword ptr [R9], RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV     R8, RDX
    MOV     RAX, RDI
    XOR     RDX, RDX

    DIV     RSI                 // RAX := RDX:RAX div RSI     RDX := RDX:RAX mod RSI

    MOV     qword ptr [R8], RAX
    MOV     qword ptr [RCX], RDX

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH    EBX
    PUSH    ESI
    PUSH    EDI
    PUSH    EBP

    PUSH    EDX                             // pointer to remainder
    PUSH    EAX                             // pointer to quotient

    MOV     EBX, dword ptr [Divisor]        // divisor low dword
    MOV     ECX, dword ptr [Divisor + 4]    // divisor high dword

    MOV     EDX, dword ptr [Dividend + 4]   // dividend high dword
    MOV     EAX, dword ptr [Dividend]       // dividend low dword

    TEST    ECX, ECX
    JNZ     @Full64BitDivision              // divisor high dword is nonzero, do full division

    TEST    EDX, EDX
    JZ      @Fast32BitDivision              // both dividend and divisor high dwords are zero, do fast division

    TEST    EBX, EBX
    JZ      @Fast32BitDivision              // entire divisor is zero, go for div by zero exception

@Full64BitDivision:

    // prepare for calculations
    MOV     EBP, ECX
    XOR     ESI, ESI
    XOR     EDI, EDI

    MOV     ECX, 64

  {
    So, current situation is following:

      EAX ... low dword of (possibly negated) dividend
      EDX ... high dword of (possibly negated) dividend

      EBX ... low dword of (possibly negated) divisor
      EBP ... high dword of (possibly negated) divisor

      ECX ... main cycle counter

      ESI ... all zeroes, bits 64..95 of shift register
      EDI ... all zeroes, bits 96..127 of shift register
  }

@MainCalculationLoop:

    // shift register one place left (up)
    SHL     EAX, 1
    RCL     EDX, 1
    RCL     ESI, 1
    RCL     EDI, 1

    // compare higher 64bits of shift register with divisor
    CMP     EDI, EBP
    JB      @MainCalculationLoop_Continue
    JA      @MainCalculationLoop_Sub
    // higher 32bits of divisor and (what will be) remainder are the same...
    CMP     ESI, EBX
    JB      @MainCalculationLoop_Continue

@MainCalculationLoop_Sub:

    SUB     ESI, EBX
    SBB     EDI, EBP
    INC     EAX

@MainCalculationLoop_Continue:

    DEC     ECX
    JNZ     @MainCalculationLoop

  {
    We are done with loop, store quotient and remainder.

    Quotient is in EDX:EAX, remainder is in EDI:ESI.
  }

    // store quotient
    POP     ECX
    MOV     dword ptr [ECX], EAX
    MOV     dword ptr [ECX + 4], EDX
    // store remainder
    POP     ECX
    MOV     dword ptr [ECX], ESI
    MOV     dword ptr [ECX + 4], EDI

    JMP     @RoutineEnd

@Fast32BitDivision:

  {
    Here, the dividend is in EAX, EDX is zero and divisor is in EBX. Pointers
    to remainder and quotient are pushed on stack (in that order).
  }

    DIV     EBX                 // EAX := EDX:EAX div EBX     EDX := EDX:EAX mod EBX

    POP     ECX
    MOV     dword ptr [ECX], EAX
    MOV     dword ptr [ECX + 4], 0
    POP     ECX
    MOV     dword ptr [ECX], EDX
    MOV     dword ptr [ECX + 4], 0

@RoutineEnd:

    POP     EBP
    POP     EDI
    POP     ESI    
    POP     EBX

{$ENDIF}
end;
{$ELSE}
{$IF Declared(NativeUInt64E)}
begin
Quotient := Dividend div Divisor;
Remainder := Dividend - UInt64(Quotient * Divisor);
end;
{$ELSE}

  procedure RCL1(var Value: UInt32; var Carry: Boolean);
  var
    TempCarry:  Boolean;
  begin
    TempCarry := (Value and $80000000) <> 0;
    If Carry then
      Value := UInt32(Value shl 1) or UInt32(1)
    else
      Value := UInt32(Value shl 1);
    Carry := TempCarry;
  end;

var
  ShiftRegister:  array[0..3] of UInt32;
  i:              Integer;
  Carry:          Boolean;
begin
If Divisor <> 0 then
  begin
    If (UInt64Rec(Dividend).Hi = 0) and (UInt64Rec(Divisor).Hi = 0) then
      begin
        Quotient := 0;
        Remainder := 0;
        uDivMod(UInt64Rec(Dividend).Lo,UInt64Rec(Divisor).Lo,UInt64Rec(Quotient).Lo,UInt64Rec(Remainder).Lo);
      end
    else If Divisor = 1 then
      begin
        Quotient := Dividend;
        Remainder := 0;
      end
    else
      begin
        ShiftRegister[0] := UInt64Rec(Dividend).Lo;
        ShiftRegister[1] := UInt64Rec(Dividend).Hi;
        ShiftRegister[2] := 0;
        ShiftRegister[3] := 0;
        For i := 0 to 63 do
          begin
            // shift register
            Carry := False;
            RCL1(ShiftRegister[0],Carry);
            RCL1(ShiftRegister[1],Carry);
            RCL1(ShiftRegister[2],Carry);
            RCL1(ShiftRegister[3],Carry);
            // if remainder is greater than divisor...
            If (ShiftRegister[3] > UInt64Rec(Divisor).Hi) or
              ((ShiftRegister[3] = UInt64Rec(Divisor).Hi) and (ShiftRegister[2] >= UInt64Rec(Divisor).Lo)) then
              begin
                // ...subtract divisor from remainder and increment quotient
                Carry := ShiftRegister[2] < UInt64Rec(Divisor).Lo;
                ShiftRegister[2] := ShiftRegister[2] - UInt64Rec(Divisor).Lo;
                If Carry then
                  ShiftRegister[3] := ShiftRegister[3] - UInt64Rec(Divisor).Hi - 1 
                else
                  ShiftRegister[3] := ShiftRegister[3] - UInt64Rec(Divisor).Hi;
                Inc(ShiftRegister[0]);
              end;
          end;
        UInt64Rec(Quotient).Lo := ShiftRegister[0];
        UInt64Rec(Quotient).Hi := ShiftRegister[1];
        UInt64Rec(Remainder).Lo := ShiftRegister[2];
        UInt64Rec(Remainder).Hi := ShiftRegister[3];
      end;
  end
// following is here only to raise a zero-division exception  
else Quotient := UInt32(Dividend) div UInt32(Divisor);
end;
{$IFEND}
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    DivMod - alias functions
-------------------------------------------------------------------------------}

procedure DivMod(Dividend,Divisor: Int8; out Quotient,Remainder: Int8);
begin
iDivMod(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivMod(Dividend,Divisor: Int16; out Quotient,Remainder: Int16);
begin
iDivMod(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivMod(Dividend,Divisor: Int32; out Quotient,Remainder: Int32);
begin
iDivMod(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivMod(Dividend,Divisor: Int64; out Quotient,Remainder: Int64);
begin
iDivMod(Dividend,Divisor,Quotient,Remainder);
end;


{===============================================================================
--------------------------------------------------------------------------------
                          Combined division and ceiling
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivCeil - signed integers
-------------------------------------------------------------------------------}

Function iDivCeil(Dividend,Divisor: Int8): Int8;
var
  Remainder:  Int8;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeil(Dividend,Divisor: Int16): Int16;
var
  Remainder:  Int16;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeil(Dividend,Divisor: Int32): Int32;
var
  Remainder:  Int32;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeil(Dividend,Divisor: Int64): Int64;
var
  Remainder:  Int64;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

{-------------------------------------------------------------------------------
    uDivCeil - unsigned integers
-------------------------------------------------------------------------------}

Function uDivCeil(Dividend,Divisor: UInt8): UInt8;
var
  Remainder:  UInt8;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeil(Dividend,Divisor: UInt16): UInt16;
var
  Remainder:  UInt16;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeil(Dividend,Divisor: UInt32): UInt32;
var
  Remainder:  UInt32;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{
  The line "Result := Result + 1" can, in theory, produce overflow if UInt64 is
  declared only as an alias to Int64.
}
{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function uDivCeil(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Result := Result + 1;
end;
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    DivCeil - alias functions
-------------------------------------------------------------------------------}

Function DivCeil(Dividend,Divisor: Int8): Int8;
begin
Result := iDivCeil(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: Int16): Int16;
begin
Result := iDivCeil(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: Int32): Int32;
begin
Result := iDivCeil(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: Int64): Int64;
begin
Result := iDivCeil(Dividend,Divisor);
end;


{===============================================================================
--------------------------------------------------------------------------------
                           Combined division and floor
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivFloor - signed integers
-------------------------------------------------------------------------------}

Function iDivFloor(Dividend,Divisor: Int8): Int8;
var
  Remainder:  Int8;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloor(Dividend,Divisor: Int16): Int16;
var
  Remainder:  Int16;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloor(Dividend,Divisor: Int32): Int32;
var
  Remainder:  Int32;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloor(Dividend,Divisor: Int64): Int64;
var
  Remainder:  Int64;
begin
iDivMod(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

{-------------------------------------------------------------------------------
    uDivFloor - unsigned integers
-------------------------------------------------------------------------------}

Function uDivFloor(Dividend,Divisor: UInt8): UInt8;
var
  Remainder:  UInt8;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloor(Dividend,Divisor: UInt16): UInt16;
var
  Remainder:  UInt16;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloor(Dividend,Divisor: UInt32): UInt32;
var
  Remainder:  UInt32;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloor(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
end;

{-------------------------------------------------------------------------------
    DivFloor - alias functions
-------------------------------------------------------------------------------}

Function DivFloor(Dividend,Divisor: Int8): Int8;
begin
Result := iDivFloor(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: Int16): Int16;
begin
Result := iDivFloor(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: Int32): Int32;
begin
Result := iDivFloor(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: Int64): Int64;
begin
Result := iDivFloor(Dividend,Divisor);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              64bit ceil and floor                             
--------------------------------------------------------------------------------
===============================================================================}

Function Ceil64(N: Extended): Int64;
begin
Result := Trunc(N);
If Frac(N) > 0 then
  Result := Result + 1;
end;

//------------------------------------------------------------------------------

Function Floor64(N: Extended): Int64;
begin
Result := Trunc(N);
If Frac(N) < 0 then
  Result := Result - 1;
end;


{===============================================================================
--------------------------------------------------------------------------------
                         Is positive integer power of 2                                                      
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iIsPow2 - signed integers
-------------------------------------------------------------------------------}

Function iIsPow2(N: Int8): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
{
  No negative number can be a positive integer power of 2 (we are not dealing
  with complex numbers here!). The same goes for 0 (btw. 2^0 = 1).
}
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iIsPow2(N: Int16): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iIsPow2(N: Int32): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iIsPow2(N: Int64): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
  Result := False;
end;

{-------------------------------------------------------------------------------
    uIsPow2 - unsigned integers
-------------------------------------------------------------------------------}

Function uIsPow2(N: UInt8): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uIsPow2(N: UInt16): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uIsPow2(N: UInt32): Boolean;
begin
If N > 0 then
  Result := N and Pred(N) = 0
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function uIsPow2(N: UInt64): Boolean;
begin
{$IF Declared(NativeUInt64E)}
If N > 0 then
{$ELSE}
If CompareUInt64(N,0) > 0 then
{$IFEND}
  Result := N and (N - 1) = 0
else
  Result := False;
end;
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    IsPow2 - alias functions
-------------------------------------------------------------------------------}

Function IsPow2(N: Int8): Boolean;
begin
Result := iIsPow2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: Int16): Boolean;
begin
Result := iIsPow2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: Int32): Boolean;
begin
Result := iIsPow2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: Int64): Boolean;
begin
Result := iIsPow2(N);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            Integer logarithm base 2
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iIntLog2 - signed integers
-------------------------------------------------------------------------------}

Function iIntLog2(N: Int8): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N              AL              CL           DIL
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   CL, 0
    JLE   @InvalidInput

    MOV   DL, CL
    DEC   DL
    TEST  CL, DL
    JNZ   @InvalidInput

    AND   ECX, $000000FF
    BSF   AX, CX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   DIL, 0
    JLE   @InvalidInput

    MOV   DL, DIL
    DEC   DL
    TEST  DIL, DL
    JNZ   @InvalidInput

    AND   EDI, $000000FF
    BSF   AX, DI
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   AL, 0
    JLE   @InvalidInput

    MOV   DL, AL
    DEC   DL
    TEST  AL, DL
    JNZ   @InvalidInput

    AND   EAX, $000000FF
    BSF   AX, AX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:
    
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N > 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 7 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iIntLog2(N: Int16): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N              AX              CX            DI
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   CX, 0
    JLE   @InvalidInput

    MOV   DX, CX
    DEC   DX
    TEST  CX, DX
    JNZ   @InvalidInput

    BSF   AX, CX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   DI, 0
    JLE   @InvalidInput

    MOV   DX, DI
    DEC   DX
    TEST  DI, DX
    JNZ   @InvalidInput

    BSF   AX, DI
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   AX, 0
    JLE   @InvalidInput

    MOV   DX, AX
    DEC   DX
    TEST  AX, DX
    JNZ   @InvalidInput

    BSF   AX, AX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:
    
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N > 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 15 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iIntLog2(N: Int32): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N             EAX             ECX           EDI
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   ECX, 0
    JLE   @InvalidInput

    MOV   EDX, ECX
    DEC   EDX
    TEST  ECX, EDX
    JNZ   @InvalidInput

    BSF   EAX, ECX

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   EDI, 0
    JLE   @InvalidInput

    MOV   EDX, EDI
    DEC   EDX
    TEST  EDI, EDX
    JNZ   @InvalidInput

    BSF   EAX, EDI

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   EAX, 0
    JLE   @InvalidInput

    MOV   EDX, EAX
    DEC   EDX
    TEST  EAX, EDX
    JNZ   @InvalidInput

    BSF   EAX, EAX

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:
    
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N > 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 31 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iIntLog2(N: Int64): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N          (EBP + 8)          RCX           RDI
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   RCX, 0
    JLE   @InvalidInput

    MOV   RDX, RCX
    DEC   RDX
    TEST  RCX, RDX
    JNZ   @InvalidInput

    BSF   RAX, RCX

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   RDI, 0
    JLE   @InvalidInput

    MOV   RDX, RDI
    DEC   RDX
    TEST  RDI, RDX
    JNZ   @InvalidInput

    BSF   RAX, RDI

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
  
    // load the number
    MOV   EAX, dword ptr [N]
    MOV   EDX, dword ptr [N + 4]

    // test for zero/negative
    CMP   EDX, 0
    JNZ   @NonZeroInputHigh
  {
    Higher 32bits are zero, so the number cannot be negative. We just need to
    check whether lower 32bits are zero (whole number is zero) or not (the
    number is non-zero positive).
  }
    TEST  EAX, EAX
    JZ    @InvalidInput
    JMP   @NonZeroInput
@NonZeroInputHigh:
    // high 32bits are non-zero, check sign...
    JL    @InvalidInput

@NonZeroInput:

    // test for Pow2
    PUSH  EBX

    MOV   EBX, EAX
    MOV   ECX, EDX

    SUB   EBX, 1
    SBB   ECX, 0
    
    TEST  EAX, EBX
    JNZ   @InvalidInputWithCleanup
    TEST  EDX, ECX
    JNZ   @InvalidInputWithCleanup

    // do bit scan
    TEST  EAX, EAX
    JZ    @ScanHigh

    BSF   EAX, EAX
    JMP   @Cleanup

@ScanHigh:

    BSF   EAX, EDX
    ADD   EAX, 32

@Cleanup:

    POP   EBX
    JMP   @RoutineEnd

@InvalidInputWithCleanup:

    POP   EBX

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N > 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 63 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    uIntLog2 - unsigned integers
-------------------------------------------------------------------------------}

Function uIntLog2(N: UInt8): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N              AL              CL           DIL
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  CL, CL
    JZ    @InvalidInput

    MOV   DL, CL
    DEC   DL
    TEST  CL, DL
    JNZ   @InvalidInput

    AND   ECX, $000000FF
    BSF   AX, CX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  DIL, DIL
    JZ    @InvalidInput

    MOV   DL, DIL
    DEC   DL
    TEST  DIL, DL
    JNZ   @InvalidInput

    AND   EDI, $000000FF
    BSF   AX, DI
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  AL, AL
    JZ    @InvalidInput

    MOV   DL, AL
    DEC   DL
    TEST  AL, DL
    JNZ   @InvalidInput

    AND   EAX, $000000FF
    BSF   AX, AX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:
    
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 7 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uIntLog2(N: UInt16): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N              AX              CX            DI
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  CX, CX
    JZ    @InvalidInput

    MOV   DX, CX
    DEC   DX
    TEST  CX, DX
    JNZ   @InvalidInput

    BSF   AX, CX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  DI, DI
    JZ    @InvalidInput

    MOV   DX, DI
    DEC   DX
    TEST  DI, DX
    JNZ   @InvalidInput

    BSF   AX, DI
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  AX, AX
    JZ    @InvalidInput

    MOV   DX, AX
    DEC   DX
    TEST  AX, DX
    JNZ   @InvalidInput

    BSF   AX, AX
    AND   EAX, $0000FFFF

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:
    
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 15 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uIntLog2(N: UInt32): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N             EAX             ECX           EDI
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  ECX, ECX
    JZ    @InvalidInput

    MOV   EDX, ECX
    DEC   EDX
    TEST  ECX, EDX
    JNZ   @InvalidInput

    BSF   EAX, ECX

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  EDI, EDI
    JZ    @InvalidInput

    MOV   EDX, EDI
    DEC   EDX
    TEST  EDI, EDX
    JNZ   @InvalidInput

    BSF   EAX, EDI

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  EAX, EAX
    JZ    @InvalidInput

    MOV   EDX, EAX
    DEC   EDX
    TEST  EAX, EDX
    JNZ   @InvalidInput

    BSF   EAX, EAX

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:
    
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  If (N and Pred(N)) = 0 then
    For i := 0 to 31 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function uIntLog2(N: UInt64): Int32;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
               N          (EBP + 8)          RCX           RDI
          Result             EAX             EAX           EAX
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  RCX, RCX
    JZ    @InvalidInput

    MOV   RDX, RCX
    DEC   RDX
    TEST  RCX, RDX
    JNZ   @InvalidInput

    BSF   RAX, RCX

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  RDI, RDI
    JZ    @InvalidInput

    MOV   RDX, RDI
    DEC   RDX
    TEST  RDI, RDX
    JNZ   @InvalidInput

    BSF   RAX, RDI

    JMP   @RoutineEnd

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    // load the number
    MOV   EAX, dword ptr [N]
    MOV   EDX, dword ptr [N + 4]

    // test for zero
    TEST  EAX, EAX
    JNZ   @NonZeroInput
    TEST  EDX, EDX
    JZ    @InvalidInput

@NonZeroInput:

    // test for Pow2
    PUSH  EBX

    MOV   EBX, EAX
    MOV   ECX, EDX

    SUB   EBX, 1
    SBB   ECX, 0
    
    TEST  EAX, EBX
    JNZ   @InvalidInputWithCleanup
    TEST  EDX, ECX
    JNZ   @InvalidInputWithCleanup

    // do bit scan
    TEST  EAX, EAX
    JZ    @ScanHigh

    BSF   EAX, EAX
    JMP   @Cleanup

@ScanHigh:

    BSF   EAX, EDX
    ADD   EAX, 32

@Cleanup:

    POP   EBX
    JMP   @RoutineEnd

@InvalidInputWithCleanup:

    POP   EBX

@InvalidInput:

    MOV   EAX, -1

@RoutineEnd:

{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  If (N and (N - 1)) = 0 then
    For i := 0 to 63 do
      If (N shr i) and 1 <> 0 then
        begin
          Result := i;
          Break{For i}
        end;
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    IntLog2 - alias functions
-------------------------------------------------------------------------------}

Function IntLog2(N: Int8): Int32;
begin
Result := iIntLog2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: Int16): Int32;
begin
Result := iIntLog2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: Int32): Int32;
begin
Result := iIntLog2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: Int64): Int32;
begin
Result := iIntLog2(N);
end;


{===============================================================================
--------------------------------------------------------------------------------
             Try combined division and modulo by integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iTryDivModPow2 - signed integers
-------------------------------------------------------------------------------}

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AL              CL           DIL
         Divisor              DL              DL           SIL
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   DL, 0
    JLE   @SignalFailure

    MOV   AL, DL
    DEC   AL
    TEST  DL, AL
    JNZ   @SignalFailure

    AND   DX, $FF
    BSF   DX, DX                      // Log2(Divisor)
    JZ    @SignalFailure              // let's be paranoid

    XCHG  RCX, RDX  
    TEST  DL, DL
    JNS   @PositiveDividend

    // dividend is negative...
    MOV   R10B, DL

    // calculate remainder
    AND   DL, AL
    JZ    @StoreRemainder
    MOV   R11B, -1
    SHL   R11B, CL
    OR    DL, R11B

@StoreRemainder:

    MOV   byte ptr [R9], DL

    // calculare quotient
    ADD   AL, R10B
    SAR   AL, CL

    MOV   byte ptr [R8], AL           // store quotient

    JMP   @SignalSuccess

@PositiveDividend:

    AND   AL, DL                      // remainder
    SHR   DL, CL                      // quotient

    MOV   byte ptr [R8], DL           // storing quotient
    MOV   byte ptr [R9], AL           // storing remainder

@SignalSuccess:

    MOV   RAX, 1                      // return true
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX                    // return false

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   SIL, 0
    JLE   @SignalFailure

    MOV   AL, SIL
    DEC   AL
    TEST  SIL, AL
    JNZ   @SignalFailure

    AND   SI, $FF
    BSF   SI, SI                      // Log2(Divisor)
    JZ    @SignalFailure              // let's be paranoid

    XCHG  RCX, RSI
    TEST  DIL, DIL
    JNS   @PositiveDividend

    // dividend is negative...
    MOV   R8B, DIL

    // calculate remainder
    AND   DIL, AL
    JZ    @StoreRemainder
    MOV   R9B, -1
    SHL   R9B, CL
    OR    DIL, R9B

@StoreRemainder:

    MOV   byte ptr [RSI], DIL

    // calculare quotient
    ADD   AL, R8B
    SAR   AL, CL

    MOV   byte ptr [RDX], AL          // store quotient

    JMP   @SignalSuccess

@PositiveDividend:

    AND   AL, DIL                     // remainder
    SHR   DIL, CL                     // quotient

    MOV   byte ptr [RDX], DIL         // storing quotient
    MOV   byte ptr [RSI], AL          // storing remainder

@SignalSuccess:
    
    MOV   RAX, 1                      // return true
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX                    // return false

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX

    CMP   DL, 0
    JLE   @SignalFailure

    MOV   BL, DL
    DEC   BL
    TEST  DL, BL
    JNZ   @SignalFailure

    AND   DX, $FF
    BSF   DX, DX                      // Log2(Divisor)
    JZ    @SignalFailure              // let's be paranoid

    XCHG  ECX, EDX  
    TEST  AL, AL
    JNS   @PositiveDividend

    // dividend is negative...
    PUSH  EDX
    PUSH  EBX

    // calculate remainder
    AND   BL, AL
    JZ    @StoreRemainder
    MOV   DL, -1
    SHL   DL, CL
    OR    BL, DL

@StoreRemainder:

    MOV   EDX, dword ptr [Remainder]
    MOV   byte ptr [EDX], BL

    // calculare quotient
    POP   EBX
    ADD   AL, BL
    SAR   AL, CL

    // store quotient
    POP   EDX
    MOV   byte ptr [EDX], AL

    JMP   @SignalSuccess

@PositiveDividend:

    AND   BL, AL                      // remainder
    SHR   AL, CL                      // quotient

    MOV   byte ptr [EDX], AL          // storing quotient
    MOV   EAX, dword ptr [Remainder]
    MOV   byte ptr [EAX], BL          // storing remainder

@SignalSuccess:
    
    MOV   EAX, 1                      // return true
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX                    // return false

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}

  Function SAR(Value: UInt8; Shift: Integer): UInt8;
  begin
    If Shift <> 0 then
      begin
        If (Value and $80) <> 0 then
          Result := UInt8((Value shr Shift) or (UInt8(-1) shl (8 - Shift)))
        else
          Result := Value shr Shift;
      end
    else Result := Value;
  end;

var
  PowerExp:     Integer;
  PredDivisor:  Int8;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    // if here, divisor is strictly greater than zero
    If Dividend < 0 then
      begin
        // both Q and R are negative
        PredDivisor := Pred(Divisor);
        // Dividend + PredDivisor can and will overflow
        Quotient := Int8(SAR(UInt8(Dividend + PredDivisor),PowerExp));
        Remainder := Dividend and PredDivisor;
        If Remainder <> 0 then
          Remainder := Remainder or Int8(UInt8(-1) shl PowerExp);
      end
    else
      begin
        // both Q and R are positive
        Quotient := Dividend shr PowerExp;
        Remainder := Dividend and Pred(Divisor);
      end;
    Result := True;
  end
else Result := False;
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AX              CX            DI
         Divisor              DX              DX            SI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   DX, 0
    JLE   @SignalFailure

    MOV   AX, DX
    DEC   AX
    TEST  DX, AX
    JNZ   @SignalFailure

    BSF   DX, DX
    JZ    @SignalFailure

    XCHG  RCX, RDX
    TEST  DX, DX
    JNS   @PositiveDividend

    MOV   R10W, DX

    AND   DX, AX
    JZ    @StoreRemainder
    MOV   R11W, -1
    SHL   R11W, CL
    OR    DX, R11W

@StoreRemainder:

    MOV   word ptr [R9], DX

    ADD   AX, R10W
    SAR   AX, CL

    MOV   word ptr [R8], AX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   AX, DX
    SHR   DX, CL

    MOV   word ptr [R8], DX
    MOV   word ptr [R9], AX

@SignalSuccess:

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   SI, 0
    JLE   @SignalFailure

    MOV   AX, SI
    DEC   AX
    TEST  SI, AX
    JNZ   @SignalFailure

    BSF   SI, SI
    JZ    @SignalFailure

    XCHG  RCX, RSI
    TEST  DI, DI
    JNS   @PositiveDividend

    MOV   R8W, DI

    AND   DI, AX
    JZ    @StoreRemainder
    MOV   R9W, -1
    SHL   R9W, CL
    OR    DI, R9W

@StoreRemainder:

    MOV   word ptr [RSI], DI

    ADD   AX, R8W
    SAR   AX, CL

    MOV   word ptr [RDX], AX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   AX, DI
    SHR   DI, CL

    MOV   word ptr [RDX], DI
    MOV   word ptr [RSI], AX

@SignalSuccess:

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX

    CMP   DX, 0
    JLE   @SignalFailure

    MOV   BX, DX
    DEC   BX
    TEST  DX, BX
    JNZ   @SignalFailure

    BSF   DX, DX
    JZ    @SignalFailure

    XCHG  ECX, EDX  
    TEST  AX, AX
    JNS   @PositiveDividend

    PUSH  EDX
    PUSH  EBX

    AND   BX, AX
    JZ    @StoreRemainder
    MOV   DX, -1
    SHL   DX, CL
    OR    BX, DX

@StoreRemainder:

    MOV   EDX, dword ptr [Remainder]
    MOV   word ptr [EDX], BX

    POP   EBX
    ADD   AX, BX
    SAR   AX, CL

    POP   EDX
    MOV   word ptr [EDX], AX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   BX, AX
    SHR   AX, CL

    MOV   word ptr [EDX], AX
    MOV   EAX, dword ptr [Remainder]
    MOV   word ptr [EAX], BX

@SignalSuccess:

    MOV   EAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}

  Function SAR(Value: UInt16; Shift: Integer): UInt16;
  begin
    If Shift <> 0 then
      begin
        If (Value and $8000) <> 0 then
          Result := UInt16((Value shr Shift) or (UInt16(-1) shl (16 - Shift)))
        else
          Result := Value shr Shift;
      end
    else Result := Value;
  end;

var
  PowerExp:     Integer;
  PredDivisor:  Int16;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    If Dividend < 0 then
      begin
        PredDivisor := Pred(Divisor);
        Quotient := Int16(SAR(UInt16(Dividend + PredDivisor),PowerExp));
        Remainder := Dividend and PredDivisor;
        If Remainder <> 0 then
          Remainder := Remainder or Int16(UInt16(-1) shl PowerExp);
      end
    else
      begin
        Quotient := Dividend shr PowerExp;
        Remainder := Dividend and Pred(Divisor);
      end;
    Result := True;
  end
else Result := False;
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend             EAX             ECX           EDI
         Divisor             EDX             EDX           ESI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   EDX, 0
    JLE   @SignalFailure

    MOV   EAX, EDX
    DEC   EAX
    TEST  EDX, EAX
    JNZ   @SignalFailure

    BSF   EDX, EDX
    JZ    @SignalFailure

    XCHG  RCX, RDX
    TEST  EDX, EDX
    JNS   @PositiveDividend

    MOV   R10D, EDX

    AND   EDX, EAX
    JZ    @StoreRemainder
    MOV   R11D, -1
    SHL   R11D, CL
    OR    EDX, R11D

@StoreRemainder:

    MOV   dword ptr [R9], EDX

    ADD   EAX, R10D
    SAR   EAX, CL

    MOV   dword ptr [R8], EAX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   EAX, EDX
    SHR   EDX, CL

    MOV   dword ptr [R8], EDX
    MOV   dword ptr [R9], EAX

@SignalSuccess:

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   ESI, 0
    JLE   @SignalFailure

    MOV   EAX, ESI
    DEC   EAX
    TEST  ESI, EAX
    JNZ   @SignalFailure

    BSF   ESI, ESI
    JZ    @SignalFailure

    XCHG  RCX, RSI
    TEST  EDI, EDI
    JNS   @PositiveDividend

    MOV   R8D, EDI

    AND   EDI, EAX
    JZ    @StoreRemainder
    MOV   R9D, -1
    SHL   R9D, CL
    OR    EDI, R9D

@StoreRemainder:

    MOV   dword ptr [RSI], EDI

    ADD   EAX, R8D
    SAR   EAX, CL

    MOV   dword ptr [RDX], EAX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   EAX, EDI
    SHR   EDI, CL

    MOV   dword ptr [RDX], EDI
    MOV   dword ptr [RSI], EAX

@SignalSuccess:

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX

    CMP   EDX, 0
    JLE   @SignalFailure

    MOV   EBX, EDX
    DEC   EBX
    TEST  EDX, EBX
    JNZ   @SignalFailure

    BSF   EDX, EDX
    JZ    @SignalFailure

    XCHG  ECX, EDX  
    TEST  EAX, EAX
    JNS   @PositiveDividend

    PUSH  EDX
    PUSH  EBX

    AND   EBX, EAX
    JZ    @StoreRemainder
    MOV   EDX, -1
    SHL   EDX, CL
    OR    EBX, EDX

@StoreRemainder:

    MOV   EDX, dword ptr [Remainder]
    MOV   dword ptr [EDX], EBX

    POP   EBX
    ADD   EAX, EBX
    SAR   EAX, CL

    POP   EDX
    MOV   dword ptr [EDX], EAX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   EBX, EAX
    SHR   EAX, CL

    MOV   dword ptr [EDX], EAX
    MOV   EAX, dword ptr [Remainder]
    MOV   dword ptr [EAX], EBX

@SignalSuccess:

    MOV   EAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}

  Function SAR(Value: UInt32; Shift: Integer): UInt32;
  begin
    If Shift <> 0 then
      begin
        If (Value and $80000000) <> 0 then
          Result := UInt32((Value shr Shift) or (UInt32(-1) shl (32 - Shift)))
        else
          Result := Value shr Shift;
      end
    else Result := Value;
  end;

var
  PowerExp:     Integer;
  PredDivisor:  Int32;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    If Dividend < 0 then
      begin
        PredDivisor := Pred(Divisor);
        Quotient := Int32(SAR(UInt32(Dividend + PredDivisor),PowerExp));
        Remainder := Dividend and PredDivisor;
        If Remainder <> 0 then
          Remainder := Remainder or Int32(UInt32(-1) shl PowerExp);
      end
    else
      begin
        Quotient := Dividend shr PowerExp;
        Remainder := Dividend and Pred(Divisor);
      end;
    Result := True;
  end
else Result := False;
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function iTryDivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend          (EBP + 8)          RCX           RDI
         Divisor          (EBP + 16)         RDX           RSI
        Quotient             EAX^             R8^          RDX^
       Remainder             EDX^             R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   RDX, 0
    JLE   @SignalFailure

    MOV   RAX, RDX
    DEC   RAX
    TEST  RDX, RAX
    JNZ   @SignalFailure

    BSF   RDX, RDX
    JZ    @SignalFailure

    XCHG  RCX, RDX
    TEST  RDX, RDX
    JNS   @PositiveDividend

    MOV   R10, RDX

    AND   RDX, RAX
    JZ    @StoreRemainder
    MOV   R11, -1
    SHL   R11, CL
    OR    RDX, R11

@StoreRemainder:

    MOV   qword ptr [R9], RDX

    ADD   RAX, R10
    SAR   RAX, CL

    MOV   qword ptr [R8], RAX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   RAX, RDX
    SHR   RDX, CL

    MOV   qword ptr [R8], RDX
    MOV   qword ptr [R9], RAX

@SignalSuccess:

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    CMP   RSI, 0
    JLE   @SignalFailure

    MOV   RAX, RSI
    DEC   RAX
    TEST  RSI, RAX
    JNZ   @SignalFailure

    BSF   RSI, RSI
    JZ    @SignalFailure

    XCHG  RCX, RSI
    TEST  RDI, RDI
    JNS   @PositiveDividend

    MOV   R8, RDI

    AND   RDI, RAX
    JZ    @StoreRemainder
    MOV   R9, -1
    SHL   R9, CL
    OR    RDI, R9

@StoreRemainder:

    MOV   qword ptr [RSI], RDI

    ADD   RAX, R8
    SAR   RAX, CL

    MOV   qword ptr [RDX], RAX

    JMP   @SignalSuccess

@PositiveDividend:

    AND   RAX, RDI
    SHR   RDI, CL

    MOV   qword ptr [RDX], RDI
    MOV   qword ptr [RSI], RAX

@SignalSuccess:

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX
    PUSH  ESI
    PUSH  EDI

    PUSH  EAX
    PUSH  EDX  

    // load divisor
    MOV   EAX, dword ptr [Divisor]
    MOV   EDX, dword ptr [Divisor + 4]

    // test divisor for zero/negative
    CMP   EDX, 0
    JNZ   @NonZeroDivisorHigh
    TEST  EAX, EAX
    JZ    @SignalFailure
    JMP   @PositiveDivisor

@NonZeroDivisorHigh:

    JL    @SignalFailure

@PositiveDivisor:

    // get log2(divisor)
    MOV   EDI, EAX
    MOV   ESI, EDX

    SUB   EDI, 1
    SBB   ESI, 0

    TEST  EAX, EDI
    JNZ   @SignalFailure
    TEST  EDX, ESI
    JNZ   @SignalFailure

    // do bit scan
    TEST  EAX, EAX
    JZ    @DivisorScanHigh

    BSF   ECX, EAX
    JMP   @DividendCheckSign

@DivisorScanHigh:

    BSF   ECX, EDX
    ADD   ECX, 32

@DividendCheckSign:

    // load dividend
    MOV   EAX, dword ptr [Dividend]
    MOV   EDX, dword ptr [Dividend + 4]

    // it is enough to look at sign of higher dword of the dividend
    TEST  EDX, EDX
    JNS   @PositiveDividend

    // negative dividend, calculate quotient
    ADD   EAX, EDI
    ADC   EDX, ESI

    CMP   CL, 31
    JA    @N_ShiftAbove31

    // shift is 31 or below
    SHRD  EAX, EDX, CL
    SAR   EDX, CL

    JMP   @N_StoreQuotient

@N_ShiftAbove31:

    MOV   EAX, EDX
    
    // fill the EDX with sign bit
    TEST  EDX, EDX
    SETNS DL
    AND   EDX, 1
    DEC   EDX

    MOV   EBX, ECX    // preserve full shift, we will need it for remainder
    AND   CL, 31
    SAR   EAX, CL
    MOV   ECX, EBX

@N_StoreQuotient:

    MOV   EBX, dword ptr [ESP + 4]
    MOV   dword ptr [EBX], EAX
    MOV   dword ptr [EBX + 4], EDX

    // calculare remainder
    AND   EDI, dword ptr [Dividend]
    AND   ESI, dword ptr [Dividend + 4]
    MOV   EBX, EDI
    OR    EBX, ESI
    JZ    @N_StoreRemainder

    MOV   EDX, -1
    CMP   CL, 31
    JA    @N_MaskShiftAbove31
    MOV   EAX, -1
    SHL   EAX, CL
    JMP   @N_ApplyRemainderMask

@N_MaskShiftAbove31:

    XOR   EAX, EAX
    AND   CL, 31
    SHL   EDX, CL

@N_ApplyRemainderMask:

    OR    EDI, EAX
    OR    ESI, EDX

@N_StoreRemainder:

    MOV   EBX, dword ptr [ESP]
    MOV   dword ptr [EBX], EDI
    MOV   dword ptr [EBX + 4], ESI

    JMP   @SignalSuccess

@PositiveDividend:

    // calculate remainder
    AND   EDI, EAX
    AND   ESI, EDX

    //calculate quotient
    CMP   CL, 31
    JA    @P_ShiftAbove31

    // shift is 31 or below
    SHRD  EAX, EDX, CL
    SHR   EDX, CL

    JMP   @P_StoreResults

@P_ShiftAbove31:

    MOV   EAX, EDX
    XOR   EDX, EDX
    AND   CL, 31
    SHR   EAX, CL

@P_StoreResults:

    // quotient
    MOV   ECX, dword ptr [ESP + 4]
    MOV   dword ptr [ECX], EAX
    MOV   dword ptr [ECX + 4], EDX

    // remainder
    MOV   ECX, dword ptr [ESP]
    MOV   dword ptr [ECX], EDI
    MOV   dword ptr [ECX + 4], ESI

@SignalSuccess:

    MOV   EAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX

@RoutineEnd:

    ADD   ESP, 8
    POP   EDI
    POP   ESI
    POP   EBX

{$ENDIF}
end;
{$ELSE}

  Function SAR(Value: UInt64; Shift: Integer): UInt64;
  begin
    If Shift <> 0 then
      begin
        If (Value and $8000000000000000) <> 0 then
          Result := UInt64((Value shr Shift) or (UInt64(-1) shl (64 - Shift)))
        else
          Result := Value shr Shift;
      end
    else Result := Value;
  end;

var
  PowerExp:     Integer;
  PredDivisor:  Int64;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    If Dividend < 0 then
      begin
        PredDivisor := Pred(Divisor);
        Quotient := Int64(SAR(UInt64(Dividend + PredDivisor),PowerExp));
        Remainder := Dividend and PredDivisor;
        If Remainder <> 0 then
          Remainder := Remainder or Int64(UInt64(-1) shl PowerExp);
      end
    else
      begin
        Quotient := Dividend shr PowerExp;
        Remainder := Dividend and Pred(Divisor);
      end;
    Result := True;
  end
else Result := False;
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    uTryDivModPow2 - unsigned integers
-------------------------------------------------------------------------------}

Function uTryDivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AL              CL           DIL
         Divisor              DL              DL           SIL
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  DL, DL
    JZ    @SignalFailure

    MOV   AL, DL
    DEC   AL
    TEST  DL, AL
    JNZ   @SignalFailure

    AND   DX, $FF
    BSF   DX, DX
    JZ    @SignalFailure

    AND   AL, CL
    XCHG  CL, DL
    SHR   DL, CL

    MOV   byte ptr [R8], DL
    MOV   byte ptr [R9], AL

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  SIL, SIL
    JZ    @SignalFailure

    MOV   AL, SIL
    DEC   AL
    TEST  SIL, AL
    JNZ   @SignalFailure

    AND   SI, $FF
    BSF   SI, SI
    JZ    @SignalFailure

    AND   AL, DIL
    XCHG  RCX, RSI
    SHR   DIL, CL

    MOV   byte ptr [RDX], DIL
    MOV   byte ptr [RSI], AL

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX

    TEST  DL, DL
    JZ    @SignalFailure

    MOV   BL, DL
    DEC   BL
    TEST  DL, BL
    JNZ   @SignalFailure

    AND   DX, $FF
    BSF   DX, DX                      // Log2(Divisor)
    JZ    @SignalFailure              // let's be paranoid

    AND   BL, AL                      // calculate remainder
    XCHG  EDX, ECX                    // shift must be in CL
    SHR   AL, CL                      // calculate quotient
    
    MOV   byte ptr [EDX], AL          // store quotient
    MOV   EAX, dword ptr [Remainder]  // load address of remainder
    MOV   byte ptr [EAX], BL          // store remainder

    MOV   EAX, 1                      // return true
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX                    // return false

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp: Int32;
begin
PowerExp := uIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and Pred(Divisor);
    Result := True;
  end
else Result := False;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uTryDivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend              AX              CX            DI
         Divisor              DX              DX            SI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  DX, DX
    JZ    @SignalFailure

    MOV   AX, DX
    DEC   AX
    TEST  DX, AX
    JNZ   @SignalFailure

    BSF   DX, DX
    JZ    @SignalFailure

    AND   AX, CX
    XCHG  CX, DX
    SHR   DX, CL

    MOV   word ptr [R8], DX
    MOV   word ptr [R9], AX

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  SI, SI
    JZ    @SignalFailure

    MOV   AX, SI
    DEC   AX
    TEST  SI, AX
    JNZ   @SignalFailure

    BSF   SI, SI
    JZ    @SignalFailure

    AND   AX, DI
    XCHG  RCX, RSI
    SHR   DI, CL

    MOV   word ptr [RDX], DI
    MOV   word ptr [RSI], AX

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX

    TEST  DX, DX
    JZ    @SignalFailure

    MOV   BX, DX
    DEC   BX
    TEST  DX, BX
    JNZ   @SignalFailure

    BSF   DX, DX
    JZ    @SignalFailure

    AND   BX, AX
    XCHG  EDX, ECX
    SHR   AX, CL

    MOV   word ptr [EDX], AX
    MOV   EAX, dword ptr [Remainder]
    MOV   word ptr [EAX], BX

    MOV   EAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp: Int32;
begin
PowerExp := uIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and Pred(Divisor);
    Result := True;
  end
else Result := False;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uTryDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend             EAX             ECX           EDI
         Divisor             EDX             EDX           ESI
        Quotient             ECX^             R8^          RDX^
       Remainder          (EBP + 8)^          R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  EDX, EDX
    JZ    @SignalFailure

    MOV   EAX, EDX
    DEC   EAX
    TEST  EDX, EAX
    JNZ   @SignalFailure

    BSF   EDX, EDX
    JZ    @SignalFailure

    AND   EAX, ECX
    XCHG  RCX, RDX
    SHR   EDX, CL

    MOV   dword ptr [R8], EDX
    MOV   dword ptr [R9], EAX

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  ESI, ESI
    JZ    @SignalFailure

    MOV   EAX, ESI
    DEC   EAX
    TEST  ESI, EAX
    JNZ   @SignalFailure

    BSF   ESI, ESI
    JZ    @SignalFailure

    AND   EAX, EDI
    XCHG  RCX, RSI
    SHR   EDI, CL

    MOV   dword ptr [RDX], EDI
    MOV   dword ptr [RSI], EAX

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX

    TEST  EDX, EDX
    JZ    @SignalFailure

    MOV   EBX, EDX
    DEC   EBX
    TEST  EDX, EBX
    JNZ   @SignalFailure

    BSF   EDX, EDX
    JZ    @SignalFailure

    AND   EBX, EAX
    XCHG  EDX, ECX
    SHR   EAX, CL

    MOV   dword ptr [EDX], EAX
    MOV   EAX, dword ptr [Remainder]
    MOV   dword ptr [EAX], EBX

    MOV   EAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   EAX, EAX                    

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp: Int32;
begin
PowerExp := uIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and Pred(Divisor);
    Result := True;
  end
else Result := False;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}
Function uTryDivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean;
{$IFNDEF PurePascal}
asm
{ --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                        win32 & lin32       win64         lin64
        Dividend          (EBP + 8)          RCX           RDI
         Divisor          (EBP + 16)         RDX           RSI
        Quotient             EAX^             R8^          RDX^
       Remainder             EDX^             R9^          RCX^
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  -- }
{$IFDEF x64}
  {$IFDEF Windows}

    TEST  RDX, RDX
    JZ    @SignalFailure

    MOV   RAX, RDX
    DEC   RAX
    TEST  RDX, RAX
    JNZ   @SignalFailure

    BSF   RDX, RDX
    JZ    @SignalFailure

    AND   RAX, RCX
    XCHG  RCX, RDX
    SHR   RDX, CL

    MOV   qword ptr [R8], RDX
    MOV   qword ptr [R9], RAX

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    TEST  RSI, RSI
    JZ    @SignalFailure

    MOV   RAX, RSI
    DEC   RAX
    TEST  RSI, RAX
    JNZ   @SignalFailure

    BSF   RSI, RSI
    JZ    @SignalFailure

    AND   RAX, RDI
    XCHG  RCX, RSI
    SHR   RDI, CL

    MOV   qword ptr [RDX], RDI
    MOV   qword ptr [RSI], RAX

    MOV   RAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    XOR   RAX, RAX

@RoutineEnd:

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX
    PUSH  EAX
    PUSH  EDX

    MOV   EAX, dword ptr [Divisor]
    MOV   EDX, dword ptr [Divisor + 4]

    // test for zero
    TEST  EAX, EAX
    JNZ   @NonZeroDivisor
    TEST  EDX, EDX
    JZ    @SignalFailure

@NonZeroDivisor:

    // test for Pow2
    MOV   EBX, EAX
    MOV   ECX, EDX

    SUB   EBX, 1
    SBB   ECX, 0

    TEST  EAX, EBX
    JNZ   @SignalFailure
    TEST  EDX, ECX
    JNZ   @SignalFailure

    // do bit scan
    TEST  EAX, EAX
    JZ    @DivisorScanHigh

    BSF   EAX, EAX
    JZ    @SignalFailure
    JMP   @DivisorScanDone

@DivisorScanHigh:

    BSF   EAX, EDX
    JZ    @SignalFailure
    ADD   EAX, 32

@DivisorScanDone:
{
  So by now the EAX contains Log2(Divisor), ECX:EBX contain (Divisor - 1),
  and EDX can be ignored (contains higher 32bits of divisor).
}
    // load dividend
    PUSH  EDI
    PUSH  ESI

    MOV   EDI, dword ptr [Dividend]
    MOV   ESI, dword ptr [Dividend + 4]

    // calculate and store remainder
    AND   EBX, EDI
    AND   ECX, ESI

    MOV   EDX, dword ptr [ESP + 8]
    MOV   dword ptr [EDX], EBX
    MOV   dword ptr [EDX + 4], ECX

    // calculate quotient
    MOV   ECX, EAX
    CMP   CL, 31
    JA    @ShiftAbove31

    // shift is 31 or below
    SHRD  EDI, ESI, CL
    SHR   ESI, CL

    JMP   @StoreQuotient

@ShiftAbove31:

    MOV   EDI, ESI
    XOR   ESI, ESI
    AND   CL, 31
    SHR   EDI, CL

@StoreQuotient:

    MOV   EAX, dword ptr [ESP + 12]
    MOV   dword ptr [EAX], EDI
    MOV   dword ptr [EAX + 4], ESI

    // cleanup and signal success
    POP   ESI
    POP   EDI
    ADD   ESP, 8    // EDX, EAX    

    MOV   EAX, 1
    JMP   @RoutineEnd

@SignalFailure:

    ADD   ESP, 8    // EDX, EAX
    XOR   EAX, EAX

@RoutineEnd:

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp: Int32;
begin
PowerExp := uIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and (Divisor - 1);
    Result := True;
  end
else Result := False;
end;
{$ENDIF}
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    TryDivModPow2 - alias functions
-------------------------------------------------------------------------------}

Function TryDivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean;
begin
Result := iTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryDivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean;
begin
Result := iTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean;
begin
Result := iTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryDivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean;
begin
Result := iTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of equal types
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iMin - signed integers
-------------------------------------------------------------------------------}

Function iMin(A,B: Int8): Int8;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMin(A,B: Int16): Int16;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMin(A,B: Int32): Int32;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMin(A,B: Int64): Int64;
begin
If A < B then
  Result := A
else
  Result := B;
end;

{-------------------------------------------------------------------------------
    uMin - unsigned integers
-------------------------------------------------------------------------------}

Function uMin(A,B: UInt8): UInt8;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMin(A,B: UInt16): UInt16;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMin(A,B: UInt32): UInt32;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMin(A,B: UInt64): UInt64;
begin
{$IF Declared(NativeUInt64E)}
If A < B then
{$ELSE}
If CompareUInt64(A,B) < 0 then
{$IFEND}
  Result := A
else
  Result := B;
end;

{-------------------------------------------------------------------------------
    fMin - real numbers
-------------------------------------------------------------------------------}

Function fMin(A,B: Extended): Extended;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMin(A,B: Currency): Currency;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMin(A,B: TDateTime): TDateTime;
begin
If A < B then
  Result := A
else
  Result := B;
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of equal types
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iMax - signed integers
-------------------------------------------------------------------------------}

Function iMax(A,B: Int8): Int8;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMax(A,B: Int16): Int16;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMax(A,B: Int32): Int32;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMax(A,B: Int64): Int64;
begin
If A > B then
  Result := A
else
  Result := B;
end;

{-------------------------------------------------------------------------------
    uMax - unsigned integers
-------------------------------------------------------------------------------}

Function uMax(A,B: UInt8): UInt8;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMax(A,B: UInt16): UInt16;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMax(A,B: UInt32): UInt32;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMax(A,B: UInt64): UInt64;
begin
{$IF Declared(NativeUInt64E)}
If A > B then
{$ELSE}
If CompareUInt64(A,B) > 0 then
{$IFEND}
  Result := A
else
  Result := B;
end;

{-------------------------------------------------------------------------------
    fMax - real numbers
-------------------------------------------------------------------------------}

Function fMax(A,B: Extended): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMax(A,B: Currency): Currency;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMax(A,B: TDateTime): TDateTime;
begin
If A > B then
  Result := A
else
  Result := B;
end;

end.





