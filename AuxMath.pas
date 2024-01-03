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
{$DEFINE PurePascal}

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

procedure iDivMod(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivMod(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure uDivMod(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivMod(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                         Is positive integer power of 2                                                      
--------------------------------------------------------------------------------
===============================================================================}

Function iPow2(N: Int8): Boolean; overload;
Function iPow2(N: Int16): Boolean; overload;
Function iPow2(N: Int32): Boolean; overload;
Function iPow2(N: Int64): Boolean; overload;

//------------------------------------------------------------------------------

Function uPow2(N: UInt8): Boolean; overload;
Function uPow2(N: UInt16): Boolean; overload;
Function uPow2(N: UInt32): Boolean; overload;
Function uPow2(N: UInt64): Boolean; overload;

{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of equal types
--------------------------------------------------------------------------------
===============================================================================}

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

{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of equal types
--------------------------------------------------------------------------------
===============================================================================}

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
                       Minimum of differing integer types                       
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
*)

implementation

{===============================================================================
    Internals
===============================================================================}
type
  UInt64Rec = packed record
    case Integer of
      0: (Lo, Hi: UInt32);
      1: (Cardinals: array [0..1] of UInt32);
      2: (Words: array [0..3] of UInt16);
      3: (Bytes: array [0..7] of UInt8);
  end;

//------------------------------------------------------------------------------

{$IF not Declared(NativeUInt64E)}

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
  }

    IDIV    EBX                 // EAX := EDX:EAX div EBX     EDX := EDX:EAX mod EBX

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
Remainder := Dividend - (Quotient * Divisor);
end;
{$ENDIF}

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
        uDivMod(UInt64Rec(Dividend).Lo,UInt64Rec(Divisor).Lo,UInt64Rec(Quotient).Lo,UInt64Rec(Remainder).Lo)
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


{===============================================================================
--------------------------------------------------------------------------------
                         Is positive integer power of 2                                                      
--------------------------------------------------------------------------------
===============================================================================}
const
  PopCountTable: array[UInt8] of UInt8 = (
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8);   

{-------------------------------------------------------------------------------
    iPow2 - signed integers
-------------------------------------------------------------------------------}

Function iPow2(N: Int8): Boolean;
begin
If N > 0 then
  Result := PopCountTable[UInt8(N)] = 1
else
{
  No negative number can be a positive integer power of 2 (we are not dealing
  with complex numbers here!). The same goes for 0 (btw. 2^0 = 1).
}
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iPow2(N: Int16): Boolean;
begin
If N > 0 then
  Result := (PopCountTable[UInt8(N shr 8)] + PopCountTable[UInt8(N)]) = 1
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iPow2(N: Int32): Boolean;
begin
If N > 0 then
  Result := (PopCountTable[UInt8(N shr 24)] + PopCountTable[UInt8(N shr 16)] +
             PopCountTable[UInt8(N shr 8)] + PopCountTable[UInt8(N)]) = 1
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iPow2(N: Int64): Boolean;
begin
If N > 0 then
  Result := (PopCountTable[Int64Rec(N).Bytes[0]] + PopCountTable[Int64Rec(N).Bytes[1]] +
             PopCountTable[Int64Rec(N).Bytes[2]] + PopCountTable[Int64Rec(N).Bytes[3]] +
             PopCountTable[Int64Rec(N).Bytes[4]] + PopCountTable[Int64Rec(N).Bytes[5]] +
             PopCountTable[Int64Rec(N).Bytes[6]] + PopCountTable[Int64Rec(N).Bytes[7]]) = 1
else
  Result := False;
end;

{-------------------------------------------------------------------------------
    uPow2 - unsigned integers
-------------------------------------------------------------------------------}

Function uPow2(N: UInt8): Boolean;
begin
If N > 0 then
  Result := PopCountTable[UInt8(N)] = 1
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uPow2(N: UInt16): Boolean;
begin
If N > 0 then
  Result := (PopCountTable[UInt8(N shr 8)] + PopCountTable[UInt8(N)]) = 1
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uPow2(N: UInt32): Boolean;
begin
If N > 0 then
  Result := (PopCountTable[UInt8(N shr 24)] + PopCountTable[UInt8(N shr 16)] +
             PopCountTable[UInt8(N shr 8)] + PopCountTable[UInt8(N)]) = 1
else
  Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uPow2(N: UInt64): Boolean;
begin
{$IF Declared(NativeUInt64E)}
If N > 0 then
{$ELSE}
If CompareUInt64(N,0) > 0 then
{$IFEND}
  Result := (PopCountTable[UInt64Rec(N).Bytes[0]] + PopCountTable[UInt64Rec(N).Bytes[1]] +
             PopCountTable[UInt64Rec(N).Bytes[2]] + PopCountTable[UInt64Rec(N).Bytes[3]] +
             PopCountTable[UInt64Rec(N).Bytes[4]] + PopCountTable[UInt64Rec(N).Bytes[5]] +
             PopCountTable[UInt64Rec(N).Bytes[6]] + PopCountTable[UInt64Rec(N).Bytes[7]]) = 1
else
  Result := False;
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
