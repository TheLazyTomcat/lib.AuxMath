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

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF Defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
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
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}
{$J-} // typed constants are not writeable

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

const
  DistinctOverloadUInt64 = {$IF Declared(NativeUInt64E)}True{$ELSE}False{$IFEND};
  DistinctOverloadUInt64N = {$IF Declared(NativeUInt64E)}1{$ELSE}0{$IFEND};
{$IF Declared(NativeUInt64E)}
  DistinctOverloadUInt64E = True;
{$IFEND}
(*
  DistinctUTF8Char = True;
  DistinctUTF8String = True;
*)
  DistinctOverloadUnicodeChar = {$IF Declared(UnicodeIsWideE)}False{$ELSE}True{$IFEND};
  DistinctOverloadUnicodeCharN = {$IF Declared(UnicodeIsWideE)}1{$ELSE}1{$IFEND};
{$IF not Declared(UnicodeIsWideE)}
  DistinctOverloadUnicodeCharE = True;
{$IFEND}
  DistinctOverloadUnicodeString = {$IF Declared(UnicodeIsWideE)}False{$ELSE}True{$IFEND};
  DistinctOverloadUnicodeStringN = {$IF Declared(UnicodeIsWideE)}1{$ELSE}1{$IFEND};
{$IF not Declared(UnicodeIsWideE)}
  DistinctOverloadUnicodeStringE = True;
{$IFEND}

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EAMException = class(Exception);

  EAMInvalidOperation = class(EAMException);

{===============================================================================
    Public constants
===============================================================================}
{
  Lowest and highest possible values of selected integer types. They can all be
  obtained using stndard functions Low() and High(), but if anyone wants them
  as constants, there they are...
}
const
  MinInt8 = Int8($80);            // -128
  MaxInt8 = Int8($7F);            // 127
  MinShortInt = MinInt8;
  MaxShortInt = MaxInt8;
  MinShort = MinInt8;
  MaxShort = MaxInt8;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinUInt8 = UInt8(0);
  MaxUInt8 = UInt8($FF);          // 255
  MinByte = MinUInt8;
  MaxByte = MaxUInt8;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinInt16 = Int16($8000);        // -32768
  MaxInt16 = Int16($7FFF);        // 32767
  MinSmallInt = MinInt16;
  MaxSmallInt = MaxInt16;
  MinSmall = MinInt16;
  MaxSmall = MaxInt16;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinUInt16 = UInt16(0);
  MaxUInt16 = UInt16($FFFF);      // 65535
  MinWord = MinUInt16;
  MaxWord = MaxUInt16;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinInt32 = Int32($80000000);    // -2147483648
  MaxInt32 = Int32($7FFFFFFF);    // 2147483647
{$IF SizeOf(LongInt) = 4}
  MinLongInt = MinInt32;
  MaxLongInt = MaxInt32;
  MinLong = MinInt32;
  MaxLong = MaxInt32;
{$IFEND}
{$IF SizeOf(Integer) = 4}
  MinInteger = MinInt32;
  MaxInteger = MaxInt32;
  MinInt = MinInt32;
  MaxInt = MaxInt32;
{$IFEND}

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinUInt32 = UInt32(0);
  MaxUInt32 = UInt32($FFFFFFFF);  // 4294967295
  MinDWord = MinUInt32;
  MaxDWord = MaxUInt32;
{$IF SizeOf(LongWord) = 4}
  MinLongWord = MinUInt32;
  MaxLongWord = MaxUInt32;
{$IFEND}
{$IF SizeOf(Cardinal) = 4}
  MinCardinal = MinUInt32;
  MaxCardinal = MaxUInt32;
{$IFEND}

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinInt64 = Int64($8000000000000000);    // -9223372036854775808
  MaxInt64 = Int64($7FFFFFFFFFFFFFFF);    // 9223372036854775807
  MinQuadInt = MinInt64;
  MaxQuadInt = MaxInt64;
{$IF SizeOf(LongInt) = 8}
  MinLongInt = MinInt64;
  MaxLongInt = MaxInt64;
  MinLong = MinInt64;
  MaxLong = MaxInt64;
{$IFEND}
{$IF SizeOf(Integer) = 8}
  MinInteger = MinInt64;
  MaxInteger = MaxInt64;
  MinInt = MinInt64;
  MaxInt = MaxInt64;
{$IFEND}

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinUInt64 = UInt64(0);
  MaxUInt64 = UInt64($FFFFFFFFFFFFFFFF);  // 18446744073709551615
  MinQuadWord = MinUInt64;
  MaxQuadWord = MaxUInt64;
  MinQWord = MinUInt64;
  MaxQWord = MaxUInt64;
{$IF SizeOf(LongWord) = 8}
  MinLongWord = MinUInt64;
  MaxLongWord = MaxUInt64;
{$IFEND}
{$IF SizeOf(Cardinal) = 8}
  MinCardinal = MinUInt64;
  MaxCardinal = MaxUInt64;
{$IFEND}

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  MinPtrInt = {$IF SizeOf(PtrInt) = 8}MinInt64{$ELSE}MinInt32{$IFEND};
  MaxPtrInt = {$IF SizeOf(PtrInt) = 8}MaxInt64{$ELSE}MaxInt32{$IFEND};

  MinPtrUInt = {$IF SizeOf(PtrUInt) = 8}MinUInt64{$ELSE}MinUInt32{$IFEND};
  MaxPtrUInt = {$IF SizeOf(PtrUInt) = 8}MaxUInt64{$ELSE}MaxUInt32{$IFEND};

//==============================================================================
{
  Special values for half-precision (16bit) floating point number types.

  Since those types are not supported by compiler and mostly not even by
  hardware, they are declared only as structured types of the right size.
  This means, among others, that they cannot be directly used in arithmetics
  or comparisons, and their assignments are not automatically checked for
  invalid encodings and SNaN.
}
const
  Float16Min:         Float16 = ($01,$00);  // 5.96046e-8
  Float16Max:         Float16 = ($FF,$7B);  // 65504
  Float16MinNormal:   Float16 = ($00,$04);  // 6.10351562500000e-5 (lowest possible normalized value)
  Float16MaxDenormal: Float16 = ($FF,$03);  // 6.09755516052246e-5 (highest possible denormalized value)
  Float16QNaN:        Float16 = ($FF,$7F);  // quiet NaN
  Float16SNaN:        Float16 = ($FF,$7D);  // signaled NaN
  Float16NaN:         Float16 = ($FF,$7F);  // quiet NaN
  Float16Infinity:    Float16 = ($00,$7C);  // positive infinity
  Float16Zero:        Float16 = ($00,$00);  // (+)0
  Float16One:         Float16 = ($00,$3C);  // +1.0
  Float16Indefinite:  Float16 = ($00,$FE);  // indefinite quiet NaN

  MinFloat16: Float16 = ($01,$00);  // Float16Min
  MaxFloat16: Float16 = ($FF,$7B);  // Float16Max

  HalfMin:          Half = ($01,$00); // 5.96046e-8
  HalfMax:          Half = ($FF,$7B); // 65504
  HalfMinNormal:    Half = ($00,$04); // 6.10351562500000e-5
  HalfMaxDenormal:  Half = ($FF,$03); // 6.09755516052246e-5
  HalfQNaN:         Half = ($FF,$7F); // quiet NaN
  HalfSNaN:         Half = ($FF,$7D); // signaled NaN
  HalfNaN:          Half = ($FF,$7F); // quiet NaN
  HalfInfinity:     Half = ($00,$7C); // positive infinity
  HalfZero:         Half = ($00,$00); // (+)0
  HalfOne:          Half = ($00,$3C); // +1.0
  HalfIndefinite:   Half = ($00,$FE); // indefinite quiet NaN

  MinHalf: Half = ($01,$00); // HalfMin
  MaxHalf: Half = ($FF,$7B); // HalfMax

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Special values for single-precision (32bit) floating point number types.

  Note on the implementation...

    It is implemented the way it is (ie. as a disgusting hack misusing the fact
    that typed constants are stored in memory) because I know of no other way
    how to put arbitrary bit pattern into float constant (or, in this case,
    static variable). The only other way I am aware of is declaring the symbols
    as functions and returning proper values, but call overhead is no-go for me
    here (yeah inlining and such, but still, it does not always work and so on).

    So, to be completely clear, the special values are declared as global
    variables overlayed on typed constants. The compilers are smart enough
    to recognize it and (when writeable constants option is off, which here it
    explicitly is) treat the variables in code as constants (ie. they cannot be
    assigned to).
}
const
  iFloat32Min:          UInt32 = $00000001; // 1.40129846432482e-45
  iFloat32Max:          UInt32 = $7F7FFFFF; // 3.40282346638529e+38
  iFloat32MinNormal:    UInt32 = $00800000; // 1.17549435082229e-38
  iFloat32MaxDenormal:  UInt32 = $007FFFFF; // 1.17549421069244e-38
  iFloat32QNaN:         UInt32 = $7FFFFFFF; // quiet NaN
  iFloat32SNaN:         UInt32 = $7FBFFFFF; // signaled NaN
  iFloat32NaN:          UInt32 = $7FFFFFFF; // quiet NaN
  iFloat32Infinity:     UInt32 = $7F800000; // positive infinity
  iFloat32Indefinite:   UInt32 = $FFC00000; // indefinite quiet NaN

var
  Float32Min:         Float32 absolute iFloat32Min;
  Float32Max:         Float32 absolute iFloat32Max;
  Float32MinNormal:   Float32 absolute iFloat32MinNormal;
  Float32MaxDenormal: Float32 absolute iFloat32MaxDenormal;
  Float32QNaN:        Float32 absolute iFloat32QNaN;
  Float32SNaN:        Float32 absolute iFloat32SNaN;
  Float32NaN:         Float32 absolute iFloat32NaN;
  Float32Infinity:    Float32 absolute iFloat32Infinity;
  Float32Indefinite:  Float32 absolute iFloat32Indefinite;

  SingleMin:          Single absolute iFloat32Min;
  SingleMax:          Single absolute iFloat32Max;
  SingleMinNormal:    Single absolute iFloat32MinNormal;
  SingleMaxDenormal:  Single absolute iFloat32MaxDenormal;
  SingleQNaN:         Single absolute iFloat32QNaN;
  SingleSNaN:         Single absolute iFloat32SNaN;
  SingleNaN:          Single absolute iFloat32NaN;
  SingleInfinity:     Single absolute iFloat32Infinity;
  SingleIndefinite:   Single absolute iFloat32Indefinite;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Special values for double-precision (64bit) floating point number types.
}
const
  iFloat64Min:          UInt64 = UInt64($0000000000000001); // 4.94065645841247e-324
  iFloat64Max:          UInt64 = UInt64($7FEFFFFFFFFFFFFF); // 1.79769313486232e+308
  iFloat64MinNormal:    UInt64 = UInt64($0010000000000000); // 2.2250738585072014e-308
  iFloat64MaxDenormal:  UInt64 = UInt64($000FFFFFFFFFFFFF); // 2.2250738585072009e-308
  iFloat64QNaN:         UInt64 = UInt64($7FFFFFFFFFFFFFFF); // quiet NaN
  iFloat64SNaN:         UInt64 = UInt64($7FF7FFFFFFFFFFFF); // signaled NaN
  iFloat64NaN:          UInt64 = UInt64($7FFFFFFFFFFFFFFF); // quiet NaN
  iFloat64Infinity:     UInt64 = UInt64($7FF0000000000000); // positive infinity
  iFloat64Indefinite:   UInt64 = UInt64($FFF8000000000000); // indefinite quiet NaN

var
  Float64Min:         Float64 absolute iFloat64Min;
  Float64Max:         Float64 absolute iFloat64Max;
  Float64MinNormal:   Float64 absolute iFloat64MinNormal;
  Float64MaxDenormal: Float64 absolute iFloat64MaxDenormal;
  Float64QNaN:        Float64 absolute iFloat64QNaN;
  Float64SNaN:        Float64 absolute iFloat64SNaN;
  Float64NaN:         Float64 absolute iFloat64NaN;
  Float64Infinity:    Float64 absolute iFloat64Infinity;
  Float64Indefinite:  Float64 absolute iFloat64Indefinite;

  DoubleMin:          Double absolute iFloat64Min;
  DoubleMax:          Double absolute iFloat64Max;
  DoubleMinNormal:    Double absolute iFloat64MinNormal;
  DoubleMaxDenormal:  Double absolute iFloat64MaxDenormal;
  DoubleQNaN:         Double absolute iFloat64QNaN;
  DoubleSNaN:         Double absolute iFloat64SNaN;
  DoubleNaN:          Double absolute iFloat64NaN;
  DoubleInfinity:     Double absolute iFloat64Infinity;
  DoubleIndefinite:   Double absolute iFloat64Indefinite;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Special values for double-extended-precision (80bit) floating point number
  types.
}
type
  TAMFloat80Overlay = packed record
    Mantissa:      UInt64;
    SignExponent:  UInt16;
  end;

const
  iFloat80Min:          TAMFloat80Overlay = (Mantissa: UInt64($0000000000000001); SignExponent: $0000); // 3.64519953188247460253e-4951
  iFloat80Max:          TAMFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $7FFE); // 1.18973149535723176502e+4932
  iFloat80MinNormal:    TAMFloat80Overlay = (Mantissa: UInt64($8000000000000000); SignExponent: $0001); // 3.36210314311209350626e-4932
  iFloat80MaxDenormal:  TAMFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $0000); // 3.36210314311209350590E-4932
  iFloat80QNaN:         TAMFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet NaN
  iFloat80SNaN:         TAMFloat80Overlay = (Mantissa: UInt64($BFFFFFFFFFFFFFFF); SignExponent: $7FFF); // signaled NaN
  iFloat80NaN:          TAMFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet NaN
  iFloat80Infinity:     TAMFloat80Overlay = (Mantissa: UInt64($8000000000000000); SignExponent: $7FFF); // positive infinity
  iFloat80Indefinite:   TAMFloat80Overlay = (Mantissa: UInt64($C000000000000000); SignExponent: $FFFF); // indefinite quiet NaN

{
  Some super-special values for 80bit floats.

  They are not supported by modern hardware and usually produce an exception
  when used (except for pseudo-denormals, they are silently converted to
  correct denormals or normalized values where possible).

    pseudo-denormals ... denormals with integer bit (bit 63) in mantissa set to 1
    unnormals        ... seemingly normalized numbers, but with zero integer bit
    pseudo-NaN       ... values encoded as proper NaN, zero integer bit
    pseudo-Infinity  ... encoded as proper infinity, zero integer bit
}
  iFloat80MinPseudoDenormal:  TAMFloat80Overlay = (Mantissa: UInt64($8000000000000000); SignExponent: $0000); // 3.36210314311209350626e-4932 (lowest possible pseudo-denormal)
  iFloat80MaxPseudoDenormal:  TAMFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $0000); // 6.72420628622418701216e-4932 (highest possible pseudo-denormal)
  iFloat80MinUnnormal:        TAMFloat80Overlay = (Mantissa: UInt64($0000000000000000); SignExponent: $0001); // lowest possible unnormal
  iFloat80MaxUnnormal:        TAMFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $7FFE); // highest possible unnormal
  iFloat80PseudoQNaN:         TAMFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet pseudo-NaN
  iFloat80PseudoSNaN:         TAMFloat80Overlay = (Mantissa: UInt64($3FFFFFFFFFFFFFFF); SignExponent: $7FFF); // signaled pseudo-NaN
  iFloat80PseudoNaN:          TAMFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet pseudo-NaN
  iFloat80PseudoInfinity:     TAMFloat80Overlay = (Mantissa: UInt64($0000000000000000); SignExponent: $7FFF); // pseudo-infinity

var
  Float80Min:         Float80 absolute iFloat80Min;
  Float80Max:         Float80 absolute iFloat80Max;
  Float80MinNormal:   Float80 absolute iFloat80MinNormal;
  Float80MaxDenormal: Float80 absolute iFloat80MaxDenormal;
  Float80QNaN:        Float80 absolute iFloat80QNaN;
  Float80SNaN:        Float80 absolute iFloat80SNaN;
  Float80NaN:         Float80 absolute iFloat80NaN;
  Float80Infinity:    Float80 absolute iFloat80Infinity;
  Float80Indefinite:  Float80 absolute iFloat80Indefinite;

  Float80MinPseudoDenormal: Float80 absolute iFloat80MinPseudoDenormal;
  Float80MaxPseudoDenormal: Float80 absolute iFloat80MaxPseudoDenormal;
  Float80MinUnnormal:       Float80 absolute iFloat80MinUnnormal;
  Float80MaxUnnormal:       Float80 absolute iFloat80MaxUnnormal;
  Float80PseudoQNaN:        Float80 absolute iFloat80PseudoQNaN;
  Float80PseudoSNaN:        Float80 absolute iFloat80PseudoSNaN;
  Float80PseudoNaN:         Float80 absolute iFloat80PseudoNaN;
  Float80PseudoInfinity:    Float80 absolute iFloat80PseudoInfinity;

{$IF SizeOf(Extended) = 10}
  ExtendedMin:          Extended absolute iFloat80Min;
  ExtendedMax:          Extended absolute iFloat80Max;
  ExtendedMinNormal:    Extended absolute iFloat80MinNormal;
  ExtendedMaxDenormal:  Extended absolute iFloat80MaxDenormal;
  ExtendedQNaN:         Extended absolute iFloat80QNaN;
  ExtendedSNaN:         Extended absolute iFloat80SNaN;
  ExtendedNaN:          Extended absolute iFloat80NaN;
  ExtendedInfinity:     Extended absolute iFloat80Infinity;
  ExtendedIndefinite:   Extended absolute iFloat80Indefinite;

  ExtendedMinPseudoDenormal:  Extended absolute iFloat80MinPseudoDenormal;
  ExtendedMaxPseudoDenormal:  Extended absolute iFloat80MaxPseudoDenormal;
  ExtendedMinUnnormal:        Extended absolute iFloat80MinUnnormal;
  ExtendedMaxUnnormal:        Extended absolute iFloat80MaxUnnormal;
  ExtendedPseudoQNaN:         Extended absolute iFloat80PseudoQNaN;
  ExtendedPseudoSNaN:         Extended absolute iFloat80PseudoSNaN;
  ExtendedPseudoNaN:          Extended absolute iFloat80PseudoNaN;
  ExtendedPseudoInfinity:     Extended absolute iFloat80PseudoInfinity;
{$ELSE}
  ExtendedMin:          Extended absolute iFloat64Min;
  ExtendedMax:          Extended absolute iFloat64Max;
  ExtendedMinNormal:    Extended absolute iFloat64MinNormal;
  ExtendedMaxDenormal:  Extended absolute iFloat64MaxDenormal;
  ExtendedQNaN:         Extended absolute iFloat64QNaN;
  ExtendedSNaN:         Extended absolute iFloat64SNaN;
  ExtendedNaN:          Extended absolute iFloat64NaN;
  ExtendedInfinity:     Extended absolute iFloat64Infinity;
  ExtendedIndefinite:   Extended absolute iFloat64Indefinite;
{$IFEND}

{===============================================================================
    Public auxiliary constants
===============================================================================}
const
{
  Highest and lowest integral value that can be stored in 64bit floating point
  number (Double) without losing any information (the numbers have the same
  magnitude, so negated positive limit can be used as low value, but for the
  sake of clarity, both are provided).
}
  AM_INT_DBL_HI = 9007199254740992;    // $0020000000000000
  AM_INT_DBL_LO = -9007199254740992;   // $FFE0000000000000

  // "nicer" alias (to get lower limit, just negate it)
  Flt64MaxInt = AM_INT_DBL_HI;

{
  Highest and lowest integral values that can be stored in Extended floating
  point type, whatever its declaration might be, without losing infromation.
}
  AM_INT_EXT_HI = {$IF SizeOf(Extended) = 10}High(Int64){$ELSE}AM_INT_DBL_HI{$IFEND};
  AM_INT_EXT_LO = {$IF SizeOf(Extended) = 10}Low(Int64){$ELSE}AM_INT_DBL_LO{$IFEND};

  AM_UINT_EXT_HI = {$IF SizeOf(Extended) = 10}MaxUInt64{$ELSE}AM_INT_DBL_HI{$IFEND};

{===============================================================================
    Public auxiliary funtions - declaration
===============================================================================}
{
  Use following two functions to transfer signed 64bit integer value to and
  from floating point number. All compilers supporting type Int64 can do it
  implicitly (or using Trunc), this functions are here to do controlled
  conversion.

  In Int64ToFloat, if the type Extended is declared only as an alias for
  Double (Win64), the limits for transfer (AM_INT_DBL_LO,AM_INT_DBL_HI) are
  in effect. The number must be within those limits otherwise an exception of
  class EAMInvalidOperation is raised.

  For FloatToInt64, the floating point number must not have non-zero fraction,
  must be greater or equal to -2^63 and at the same time smaller than 2^63.
  Also, when type extended is aliased to double, the number must lie within
  limits for loss-less conversion to integer (must be within interval
  [AM_INT_DBL_LO,AM_INT_DBL_HI]). If any mentioned rule is not observed, then
  an exception of class EAMInvalidOperation is raised.
}
Function Int64ToFloat(N: Int64): Extended;

Function FloatToInt64(N: Extended): Int64;

{
  Following functions transfer unsigned 64bit integer value to and from
  floating point number. Newer compilers can do it, but older ones (those
  without full support for type UInt64) might not be capable of doing it.

  Note that both functions are doing the same checks as corresponding functions
  for signed 64bit integers (of course here tweaked for unsigned).
}
Function UInt64ToFloat(N: UInt64): Extended;

Function FloatToUInt64(N: Extended): UInt64;

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

procedure DivMod(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivMod(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivMod(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivMod(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                          Combined division and ceiling
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and Ceil as one optimized operation (the calculation does
  not use floating point unit/numbers, only integers, which ensures that no
  information is lost due to precision problems in large float numbers).
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
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeil(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivCeil(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeil(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeil(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeil(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


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
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloor(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivFloor(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloor(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloor(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloor(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                              64bit ceil and floor
--------------------------------------------------------------------------------
===============================================================================}
{
  Standard Ceil and Floor functions are returning only Integers - that is,
  32bit numbers. Following functions are here for situations where 64bit wide
  integers are required.
  
  Ceil64 and Floor64 are returning Int64, CeilU64 and FloorU64 are returning
  UInt64.

  Note that, if the N is beyond limits for UInt64, the CeilU64 and FloorU64
  will raise an EAMInvalidOperation exception (in Ceil64 and Floor64, this is
  manager by the compiler and the exception would be of class EInvalidOP).
}

Function Ceil64(N: Extended): Int64;

Function Floor64(N: Extended): Int64;

Function CeilU64(N: Extended): UInt64;

Function FloorU64(N: Extended): UInt64;

{===============================================================================
--------------------------------------------------------------------------------
                         Is positive integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns true when given number is a positive integer power of 2 (2^E, where E
  is a positive integer), false otherwise.     
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
{$IF Declared(DistinctOverloadUInt64E)}
Function IsPow2(N: Int64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function IsPow2(N: UInt8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IsPow2(N: UInt16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IsPow2(N: UInt32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function IsPow2(N: UInt64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                            Integer logarithm base 2
--------------------------------------------------------------------------------
===============================================================================}
{
  If the given number is a positive integer power of 2, then IntLog2 will
  return the exponent (effectively Log2(N)).
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
{$IF Declared(DistinctOverloadUInt64E)}
Function IntLog2(N: Int64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function IntLog2(N: UInt8): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IntLog2(N: UInt16): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function IntLog2(N: UInt32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function IntLog2(N: UInt64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


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

Function TryDivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryDivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryDivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
               Combined division and modulo by integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{
  If divisor is a positive integer power of 2, it will perform highly optimized
  integer division and modulo in one operation and return true.
  Otherwise it will perform standard integer division and modulo and return
  false.

    NOTE - the highly optimized division (TryDivModPow2) is always called and
           only when it fails the standard division (DivMod) is called instead.
}

Function iDivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean; overload;
Function iDivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean; overload;
Function iDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean; overload;
Function iDivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean; overload;

//------------------------------------------------------------------------------

Function uDivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean; overload;
Function uDivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean; overload;
Function uDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean; overload;
Function uDivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean; overload;

//------------------------------------------------------------------------------

Function DivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function DivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
           Combined division and ceiling (optimized for pow2 divisor)
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and Ceil as one operation while utilizing functions
  optimized for integral power of two divisor.
  The calculation does not use floating point unit/numbers, only integers.
}

Function iDivCeilPow2(Dividend,Divisor: Int8): Int8; overload;
Function iDivCeilPow2(Dividend,Divisor: Int16): Int16; overload;
Function iDivCeilPow2(Dividend,Divisor: Int32): Int32; overload;
Function iDivCeilPow2(Dividend,Divisor: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uDivCeilPow2(Dividend,Divisor: UInt8): UInt8; overload;
Function uDivCeilPow2(Dividend,Divisor: UInt16): UInt16; overload;
Function uDivCeilPow2(Dividend,Divisor: UInt32): UInt32; overload;
Function uDivCeilPow2(Dividend,Divisor: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function DivCeilPow2(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeilPow2(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivCeilPow2(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeilPow2(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
            Combined division and floor (optimized for pow2 divisor)
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and Floor as one operation while utilizing functions
  optimized for integral power of two divisor.
  The calculation does not use floating point unit/numbers, only integers.
}

Function iDivFloorPow2(Dividend,Divisor: Int8): Int8; overload;
Function iDivFloorPow2(Dividend,Divisor: Int16): Int16; overload;
Function iDivFloorPow2(Dividend,Divisor: Int32): Int32; overload;
Function iDivFloorPow2(Dividend,Divisor: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uDivFloorPow2(Dividend,Divisor: UInt8): UInt8; overload;
Function uDivFloorPow2(Dividend,Divisor: UInt16): UInt16; overload;
Function uDivFloorPow2(Dividend,Divisor: UInt32): UInt32; overload;
Function uDivFloorPow2(Dividend,Divisor: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function DivFloorPow2(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloorPow2(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivFloorPow2(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloorPow2(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
         Combined division and modulo by integer power of 2 (no checks)
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs highly optimized integer division and modulo, while assuming the
  divisor is a positive integer power of 2.

    WARNING - it is caller's resposibility to ensure that the divisor is a
              positive integral power of 2, this is not checked. If the divisor
              is not a power of two, then the results are completely undefined.
}

procedure iDivModPow2NoCheck(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivModPow2NoCheck(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivModPow2NoCheck(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure iDivModPow2NoCheck(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure uDivModPow2NoCheck(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivModPow2NoCheck(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivModPow2NoCheck(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure uDivModPow2NoCheck(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure DivModPow2NoCheck(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NoCheck(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NoCheck(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NoCheck(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure DivModPow2NoCheck(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NoCheck(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NoCheck(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NoCheck(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
// shortened sliases

procedure iDivModPow2NC(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure iDivModPow2NC(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure iDivModPow2NC(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure iDivModPow2NC(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure uDivModPow2NC(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure uDivModPow2NC(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure uDivModPow2NC(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure uDivModPow2NC(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure DivModPow2NC(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NC(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NC(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NC(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure DivModPow2NC(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NC(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NC(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DivModPow2NC(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
      Combined division and ceiling (optimized for pow2 divisor, no checks)     
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and Ceil as one operation while utilizing functions
  optimized for integral power of two divisor without checking.
  The calculation does not use floating point unit/numbers.

    WARNING - it is caller's resposibility to ensure that the divisor is a
              positive integral power of 2, this is not checked. If the divisor
              is not a power of two, then the result is completely undefined.
}

Function iDivCeilPow2NoCheck(Dividend,Divisor: Int8): Int8; overload;
Function iDivCeilPow2NoCheck(Dividend,Divisor: Int16): Int16; overload;
Function iDivCeilPow2NoCheck(Dividend,Divisor: Int32): Int32; overload;
Function iDivCeilPow2NoCheck(Dividend,Divisor: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt8): UInt8; overload;
Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt16): UInt16; overload;
Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt32): UInt32; overload;
Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function DivCeilPow2NoCheck(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NoCheck(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NoCheck(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeilPow2NoCheck(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivCeilPow2NoCheck(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NoCheck(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NoCheck(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeilPow2NoCheck(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function iDivCeilPow2NC(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function iDivCeilPow2NC(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function iDivCeilPow2NC(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function iDivCeilPow2NC(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function uDivCeilPow2NC(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function uDivCeilPow2NC(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function uDivCeilPow2NC(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function uDivCeilPow2NC(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function DivCeilPow2NC(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NC(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NC(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeilPow2NC(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivCeilPow2NC(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NC(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivCeilPow2NC(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivCeilPow2NC(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
       Combined division and floor (optimized for pow2 divisor, no checks)           
--------------------------------------------------------------------------------
===============================================================================}
{
  Performs division and floor as one operation while utilizing functions
  optimized for integral power of two divisor without checking.
  The calculation does not use floating point unit/numbers.

    WARNING - it is caller's resposibility to ensure that the divisor is a
              positive integral power of 2, this is not checked. If the divisor
              is not a power of two, then the result is completely undefined.
}

Function iDivFloorPow2NoCheck(Dividend,Divisor: Int8): Int8; overload;
Function iDivFloorPow2NoCheck(Dividend,Divisor: Int16): Int16; overload;
Function iDivFloorPow2NoCheck(Dividend,Divisor: Int32): Int32; overload;
Function iDivFloorPow2NoCheck(Dividend,Divisor: Int64): Int64; overload;

//------------------------------------------------------------------------------

Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt8): UInt8; overload;
Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt16): UInt16; overload;
Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt32): UInt32; overload;
Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

Function DivFloorPow2NoCheck(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NoCheck(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NoCheck(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloorPow2NoCheck(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivFloorPow2NoCheck(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NoCheck(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NoCheck(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloorPow2NoCheck(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function iDivFloorPow2NC(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function iDivFloorPow2NC(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function iDivFloorPow2NC(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function iDivFloorPow2NC(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function uDivFloorPow2NC(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function uDivFloorPow2NC(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function uDivFloorPow2NC(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function uDivFloorPow2NC(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function DivFloorPow2NC(Dividend,Divisor: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NC(Dividend,Divisor: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NC(Dividend,Divisor: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloorPow2NC(Dividend,Divisor: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function DivFloorPow2NC(Dividend,Divisor: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NC(Dividend,Divisor: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function DivFloorPow2NC(Dividend,Divisor: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function DivFloorPow2NC(Dividend,Divisor: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of same types
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

//------------------------------------------------------------------------------

Function Min(A,B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A,B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A,B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A,B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A,B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A,B: Extended): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of mixed types
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns smaller/lower of the two given values.

  When the function cannot select or return proper value, then an exception of 
  class EAMInvalidOperation is raised. This can happen in following situations:

    - value to be returned cannot fit into result (eg. when comparing Int16 and
      Int8 where 16bit value is -300 and result is of type Int8 - such number 
      simply cannot be stored in signed 8bit integer)

    - negative value is to be returned, but the result type is unsigned integer

    - large positive or negative integer needs to be converted to float of
      limited precision for comparison (eg. on systems where type Extended is
      only an alias for Double), and the conversion would lead to loss
      of information (see constants AM_INT_DBL_LO and AM_INT_DBL_HI for
      applicable limits)

    - floating point number needs to be returned in integer result, but it has
      value beyond what the result type can store (floats can store, though in 
      lower precision, much higher numbers than any integer can)

    - selected value is a floating point number with non-zero fraction but the
      result type is integer
}

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

Function fuMin(A: Extended; B: UInt8): Extended; overload;
Function fuMin(A: Extended; B: UInt16): Extended; overload;
Function fuMin(A: Extended; B: UInt32): Extended; overload;
Function fuMin(A: Extended; B: UInt64): Extended; overload;

//------------------------------------------------------------------------------

Function ifMin(A: Int8; B: Extended): Int8; overload;
Function ifMin(A: Int16; B: Extended): Int16; overload;
Function ifMin(A: Int32; B: Extended): Int32; overload;
Function ifMin(A: Int64; B: Extended): Int64; overload;

//------------------------------------------------------------------------------

Function ufMin(A: UInt8; B: Extended): UInt8; overload;
Function ufMin(A: UInt16; B: Extended): UInt16; overload;
Function ufMin(A: UInt32; B: Extended): UInt32; overload;
Function ufMin(A: UInt64; B: Extended): UInt64; overload;

//==============================================================================

Function Min(A: Int8; B: Int16): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int8; B: Int32): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int8; B: Int64): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: Int16; B: Int8): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int16; B: Int32): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int16; B: Int64): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: Int32; B: Int8): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int32; B: Int16): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int32; B: Int64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Min(A: Int64; B: Int8): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int64; B: Int16): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int64; B: Int32): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Int8; B: UInt8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int8; B: UInt16): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int8; B: UInt32): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int8; B: UInt64): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: Int16; B: UInt8): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int16; B: UInt16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int16; B: UInt32): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int16; B: UInt64): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: Int32; B: UInt8): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int32; B: UInt16): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int32; B: UInt32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int32; B: UInt64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Min(A: Int64; B: UInt8): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int64; B: UInt16): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int64; B: UInt32): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int64; B: UInt64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: UInt8; B: Int8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt8; B: Int16): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt8; B: Int32): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt8; B: Int64): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: UInt16; B: Int8): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt16; B: Int16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt16; B: Int32): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt16; B: Int64): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: UInt32; B: Int8): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt32; B: Int16): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt32; B: Int32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt32; B: Int64): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Min(A: UInt64; B: Int8): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt64; B: Int16): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt64; B: Int32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt64; B: Int64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: UInt8; B: UInt16): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt8; B: UInt32): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt8; B: UInt64): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: UInt16; B: UInt8): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt16; B: UInt32): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt16; B: UInt64): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Min(A: UInt32; B: UInt8): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt32; B: UInt16): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt32; B: UInt64): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Min(A: UInt64; B: UInt8): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt64; B: UInt16): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt64; B: UInt32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Extended; B: Int8): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Extended; B: Int16): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Extended; B: Int32): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Extended; B: Int64): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Extended; B: UInt8): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Extended; B: UInt16): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Extended; B: UInt32): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Extended; B: UInt64): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Int8; B: Extended): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int16; B: Extended): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: Int32; B: Extended): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: Int64; B: Extended): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: UInt8; B: Extended): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt16; B: Extended): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Min(A: UInt32; B: Extended): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Min(A: UInt64; B: Extended): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of same types
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

//------------------------------------------------------------------------------

Function Max(A,B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A,B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A,B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A,B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A,B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A,B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A,B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A,B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A,B: Extended): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of mixed types
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns bigger/higher of the two given values.

  When the function cannot select or return proper value, then an exception of 
  class EAMInvalidOperation is raised. Please refer to description of Min
  operating on mixed types for information about when this can happen.
}

Function iiMax(A: Int8; B: Int16): Int8; overload;
Function iiMax(A: Int8; B: Int32): Int8; overload;
Function iiMax(A: Int8; B: Int64): Int8; overload;

Function iiMax(A: Int16; B: Int8): Int16; overload;
Function iiMax(A: Int16; B: Int32): Int16; overload;
Function iiMax(A: Int16; B: Int64): Int16; overload;

Function iiMax(A: Int32; B: Int8): Int32; overload;
Function iiMax(A: Int32; B: Int16): Int32; overload;
Function iiMax(A: Int32; B: Int64): Int32; overload;

Function iiMax(A: Int64; B: Int8): Int64; overload;
Function iiMax(A: Int64; B: Int16): Int64; overload;
Function iiMax(A: Int64; B: Int32): Int64; overload;

//------------------------------------------------------------------------------

Function iuMax(A: Int8; B: UInt8): Int8; overload;
Function iuMax(A: Int8; B: UInt16): Int8; overload;
Function iuMax(A: Int8; B: UInt32): Int8; overload;
Function iuMax(A: Int8; B: UInt64): Int8; overload;

Function iuMax(A: Int16; B: UInt8): Int16; overload;
Function iuMax(A: Int16; B: UInt16): Int16; overload;
Function iuMax(A: Int16; B: UInt32): Int16; overload;
Function iuMax(A: Int16; B: UInt64): Int16; overload;

Function iuMax(A: Int32; B: UInt8): Int32; overload;
Function iuMax(A: Int32; B: UInt16): Int32; overload;
Function iuMax(A: Int32; B: UInt32): Int32; overload;
Function iuMax(A: Int32; B: UInt64): Int32; overload;

Function iuMax(A: Int64; B: UInt8): Int64; overload;
Function iuMax(A: Int64; B: UInt16): Int64; overload;
Function iuMax(A: Int64; B: UInt32): Int64; overload;
Function iuMax(A: Int64; B: UInt64): Int64; overload;

//------------------------------------------------------------------------------

Function uiMax(A: UInt8; B: Int8): UInt8; overload;
Function uiMax(A: UInt8; B: Int16): UInt8; overload;
Function uiMax(A: UInt8; B: Int32): UInt8; overload;
Function uiMax(A: UInt8; B: Int64): UInt8; overload;

Function uiMax(A: UInt16; B: Int8): UInt16; overload;
Function uiMax(A: UInt16; B: Int16): UInt16; overload;
Function uiMax(A: UInt16; B: Int32): UInt16; overload;
Function uiMax(A: UInt16; B: Int64): UInt16; overload;

Function uiMax(A: UInt32; B: Int8): UInt32; overload;
Function uiMax(A: UInt32; B: Int16): UInt32; overload;
Function uiMax(A: UInt32; B: Int32): UInt32; overload;
Function uiMax(A: UInt32; B: Int64): UInt32; overload;

Function uiMax(A: UInt64; B: Int8): UInt64; overload;
Function uiMax(A: UInt64; B: Int16): UInt64; overload;
Function uiMax(A: UInt64; B: Int32): UInt64; overload;
Function uiMax(A: UInt64; B: Int64): UInt64; overload;

//------------------------------------------------------------------------------

Function uuMax(A: UInt8; B: UInt16): UInt8; overload;
Function uuMax(A: UInt8; B: UInt32): UInt8; overload;
Function uuMax(A: UInt8; B: UInt64): UInt8; overload;

Function uuMax(A: UInt16; B: UInt8): UInt16; overload;
Function uuMax(A: UInt16; B: UInt32): UInt16; overload;
Function uuMax(A: UInt16; B: UInt64): UInt16; overload;

Function uuMax(A: UInt32; B: UInt8): UInt32; overload;
Function uuMax(A: UInt32; B: UInt16): UInt32; overload;
Function uuMax(A: UInt32; B: UInt64): UInt32; overload;

Function uuMax(A: UInt64; B: UInt8): UInt64; overload;
Function uuMax(A: UInt64; B: UInt16): UInt64; overload;
Function uuMax(A: UInt64; B: UInt32): UInt64; overload;

//------------------------------------------------------------------------------

Function fiMax(A: Extended; B: Int8): Extended; overload;
Function fiMax(A: Extended; B: Int16): Extended; overload;
Function fiMax(A: Extended; B: Int32): Extended; overload;
Function fiMax(A: Extended; B: Int64): Extended; overload;

//------------------------------------------------------------------------------

Function fuMax(A: Extended; B: UInt8): Extended; overload;
Function fuMax(A: Extended; B: UInt16): Extended; overload;
Function fuMax(A: Extended; B: UInt32): Extended; overload;
Function fuMax(A: Extended; B: UInt64): Extended; overload;

//------------------------------------------------------------------------------

Function ifMax(A: Int8; B: Extended): Int8; overload;
Function ifMax(A: Int16; B: Extended): Int16; overload;
Function ifMax(A: Int32; B: Extended): Int32; overload;
Function ifMax(A: Int64; B: Extended): Int64; overload;

//------------------------------------------------------------------------------

Function ufMax(A: UInt8; B: Extended): UInt8; overload;
Function ufMax(A: UInt16; B: Extended): UInt16; overload;
Function ufMax(A: UInt32; B: Extended): UInt32; overload;
Function ufMax(A: UInt64; B: Extended): UInt64; overload;

//==============================================================================

Function Max(A: Int8; B: Int16): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int8; B: Int32): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int8; B: Int64): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: Int16; B: Int8): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int16; B: Int32): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int16; B: Int64): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: Int32; B: Int8): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int32; B: Int16): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int32; B: Int64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Max(A: Int64; B: Int8): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int64; B: Int16): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int64; B: Int32): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Int8; B: UInt8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int8; B: UInt16): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int8; B: UInt32): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int8; B: UInt64): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: Int16; B: UInt8): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int16; B: UInt16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int16; B: UInt32): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int16; B: UInt64): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: Int32; B: UInt8): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int32; B: UInt16): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int32; B: UInt32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int32; B: UInt64): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Max(A: Int64; B: UInt8): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int64; B: UInt16): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int64; B: UInt32): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int64; B: UInt64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: UInt8; B: Int8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt8; B: Int16): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt8; B: Int32): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt8; B: Int64): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: UInt16; B: Int8): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt16; B: Int16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt16; B: Int32): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt16; B: Int64): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: UInt32; B: Int8): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt32; B: Int16): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt32; B: Int32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt32; B: Int64): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Max(A: UInt64; B: Int8): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt64; B: Int16): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt64; B: Int32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt64; B: Int64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: UInt8; B: UInt16): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt8; B: UInt32): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt8; B: UInt64): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: UInt16; B: UInt8): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt16; B: UInt32): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt16; B: UInt64): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function Max(A: UInt32; B: UInt8): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt32; B: UInt16): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt32; B: UInt64): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Max(A: UInt64; B: UInt8): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt64; B: UInt16): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt64; B: UInt32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Extended; B: Int8): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Extended; B: Int16): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Extended; B: Int32): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Extended; B: Int64): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Extended; B: UInt8): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Extended; B: UInt16): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Extended; B: UInt32): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Extended; B: UInt64): Extended; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Int8; B: Extended): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int16; B: Extended): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: Int32; B: Extended): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: Int64; B: Extended): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: UInt8; B: Extended): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt16; B: Extended): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Max(A: UInt32; B: Extended): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function Max(A: UInt64; B: Extended): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Smallest value in array
--------------------------------------------------------------------------------
===============================================================================}
{
  Selects smallest value from an array, returns it in output parameter Minimum
  and sets result to an index at which first occurence of this value was found.

  If the array is empty, then -1 is returned and value of Minimum is undefined.
}

Function iMinValue(const Values: array of Int8; out Minimum: Int8): Integer; overload;
Function iMinValue(const Values: array of Int16; out Minimum: Int16): Integer; overload;
Function iMinValue(const Values: array of Int32; out Minimum: Int32): Integer; overload;
Function iMinValue(const Values: array of Int64; out Minimum: Int64): Integer; overload;

//------------------------------------------------------------------------------

Function uMinValue(const Values: array of UInt8; out Minimum: UInt8): Integer; overload;
Function uMinValue(const Values: array of UInt16; out Minimum: UInt16): Integer; overload;
Function uMinValue(const Values: array of UInt32; out Minimum: UInt32): Integer; overload;
Function uMinValue(const Values: array of UInt64; out Minimum: UInt64): Integer; overload;

//------------------------------------------------------------------------------

Function fMinValue(const Values: array of Single; out Minimum: Single): Integer; overload;
Function fMinValue(const Values: array of Double; out Minimum: Double): Integer; overload;
{$IF SizeOf(Extended) = 10}
Function fMinValue(const Values: array of Extended; out Minimum: Extended): Integer; overload;
{$IFEND}

//------------------------------------------------------------------------------

Function MinValue(const Values: array of Int8; out Minimum: Int8): Integer; overload;
Function MinValue(const Values: array of Int16; out Minimum: Int16): Integer; overload;
Function MinValue(const Values: array of Int32; out Minimum: Int32): Integer; overload;
{$IF Declared(DistinctOverloadUInt64E)}
Function MinValue(const Values: array of Int64; out Minimum: Int64): Integer; overload;
{$IFEND}

Function MinValue(const Values: array of UInt8; out Minimum: UInt8): Integer; overload;
Function MinValue(const Values: array of UInt16; out Minimum: UInt16): Integer; overload;
Function MinValue(const Values: array of UInt32; out Minimum: UInt32): Integer; overload;
{$IF Declared(DistinctOverloadUInt64E)}
Function MinValue(const Values: array of UInt64; out Minimum: UInt64): Integer; overload;
{$IFEND}

Function MinValue(const Values: array of Single; out Minimum: Single): Integer; overload;
Function MinValue(const Values: array of Double; out Minimum: Double): Integer; overload;
{$IF SizeOf(Extended) = 10}
Function MinValue(const Values: array of Extended; out Minimum: Extended): Integer; overload;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Largest value in array
--------------------------------------------------------------------------------
===============================================================================}
{
  Selects largest value from an array, returns it in output parameter Maximum
  and sets result to an index at which first occurence of this value was found.

  If the array is empty, then -1 is returned and value of Maximum is undefined.
}

Function iMaxValue(const Values: array of Int8; out Maximum: Int8): Integer; overload;
Function iMaxValue(const Values: array of Int16; out Maximum: Int16): Integer; overload;
Function iMaxValue(const Values: array of Int32; out Maximum: Int32): Integer; overload;
Function iMaxValue(const Values: array of Int64; out Maximum: Int64): Integer; overload;

//------------------------------------------------------------------------------

Function uMaxValue(const Values: array of UInt8; out Maximum: UInt8): Integer; overload;
Function uMaxValue(const Values: array of UInt16; out Maximum: UInt16): Integer; overload;
Function uMaxValue(const Values: array of UInt32; out Maximum: UInt32): Integer; overload;
Function uMaxValue(const Values: array of UInt64; out Maximum: UInt64): Integer; overload;

//------------------------------------------------------------------------------

Function fMaxValue(const Values: array of Single; out Maximum: Single): Integer; overload;
Function fMaxValue(const Values: array of Double; out Maximum: Double): Integer; overload;
{$IF SizeOf(Extended) = 10}
Function fMaxValue(const Values: array of Extended; out Maximum: Extended): Integer; overload;
{$IFEND}

//------------------------------------------------------------------------------

Function MaxValue(const Values: array of Int8; out Maximum: Int8): Integer; overload;
Function MaxValue(const Values: array of Int16; out Maximum: Int16): Integer; overload;
Function MaxValue(const Values: array of Int32; out Maximum: Int32): Integer; overload;
{$IF Declared(DistinctOverloadUInt64E)}
Function MaxValue(const Values: array of Int64; out Maximum: Int64): Integer; overload;
{$IFEND}

Function MaxValue(const Values: array of UInt8; out Maximum: UInt8): Integer; overload;
Function MaxValue(const Values: array of UInt16; out Maximum: UInt16): Integer; overload;
Function MaxValue(const Values: array of UInt32; out Maximum: UInt32): Integer; overload;
{$IF Declared(DistinctOverloadUInt64E)}
Function MaxValue(const Values: array of UInt64; out Maximum: UInt64): Integer; overload;
{$IFEND}

Function MaxValue(const Values: array of Single; out Maximum: Single): Integer; overload;
Function MaxValue(const Values: array of Double; out Maximum: Double): Integer; overload;
{$IF SizeOf(Extended) = 10}
Function MaxValue(const Values: array of Extended; out Maximum: Extended): Integer; overload;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                          Compare values of equal types
--------------------------------------------------------------------------------
===============================================================================}
{
  If A is larger than B, a positive number is returned. If A is smaller than B,
  then negative number is returned. When the two values are equal, zero is
  returned.

  Epsilon is maximal difference between two numbers where they can still be
  considered equal.
}
Function iCompareValue(A,B: Int8): Integer; overload;
Function iCompareValue(A,B: Int16): Integer; overload;
Function iCompareValue(A,B: Int32): Integer; overload;
Function iCompareValue(A,B: Int64): Integer; overload;

//------------------------------------------------------------------------------

Function uCompareValue(A,B: UInt8): Integer; overload;
Function uCompareValue(A,B: UInt16): Integer; overload;
Function uCompareValue(A,B: UInt32): Integer; overload;
Function uCompareValue(A,B: UInt64): Integer; overload;

//------------------------------------------------------------------------------

Function fCompareValue(A,B: Extended; Epsilon: Extended = 0.0): Integer; overload;

//------------------------------------------------------------------------------

Function CompareValue(A,B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A,B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A,B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A,B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A,B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A,B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A,B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A,B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A,B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                        Compare values of differing types
--------------------------------------------------------------------------------
===============================================================================}
{
  Returns positive number when A is bigger than B, negative number when A is
  smaller than B, and zero when they equal.

  For floats versus integers, when the integer cannot be accurately converted
  to float for comparison, an exception of class EAMInvalidOperation will be
  raised.

    NOTE - I am fully aware that some overloads are superfluous and their
           functionality can be achieved by just swapping arguments.
}

Function iiCompareValue(A: Int8; B: Int16): Integer; overload;
Function iiCompareValue(A: Int8; B: Int32): Integer; overload;
Function iiCompareValue(A: Int8; B: Int64): Integer; overload;

Function iiCompareValue(A: Int16; B: Int8): Integer; overload;
Function iiCompareValue(A: Int16; B: Int32): Integer; overload;
Function iiCompareValue(A: Int16; B: Int64): Integer; overload;

Function iiCompareValue(A: Int32; B: Int8): Integer; overload;
Function iiCompareValue(A: Int32; B: Int16): Integer; overload;
Function iiCompareValue(A: Int32; B: Int64): Integer; overload;

Function iiCompareValue(A: Int64; B: Int8): Integer; overload;
Function iiCompareValue(A: Int64; B: Int16): Integer; overload;
Function iiCompareValue(A: Int64; B: Int32): Integer; overload;

//------------------------------------------------------------------------------

Function iuCompareValue(A: Int8; B: UInt8): Integer; overload;
Function iuCompareValue(A: Int8; B: UInt16): Integer; overload;
Function iuCompareValue(A: Int8; B: UInt32): Integer; overload;
Function iuCompareValue(A: Int8; B: UInt64): Integer; overload;

Function iuCompareValue(A: Int16; B: UInt8): Integer; overload;
Function iuCompareValue(A: Int16; B: UInt16): Integer; overload;
Function iuCompareValue(A: Int16; B: UInt32): Integer; overload;
Function iuCompareValue(A: Int16; B: UInt64): Integer; overload;

Function iuCompareValue(A: Int32; B: UInt8): Integer; overload;
Function iuCompareValue(A: Int32; B: UInt16): Integer; overload;
Function iuCompareValue(A: Int32; B: UInt32): Integer; overload;
Function iuCompareValue(A: Int32; B: UInt64): Integer; overload;

Function iuCompareValue(A: Int64; B: UInt8): Integer; overload;
Function iuCompareValue(A: Int64; B: UInt16): Integer; overload;
Function iuCompareValue(A: Int64; B: UInt32): Integer; overload;
Function iuCompareValue(A: Int64; B: UInt64): Integer; overload;

//------------------------------------------------------------------------------

Function uiCompareValue(A: UInt8; B: Int8): Integer; overload;
Function uiCompareValue(A: UInt8; B: Int16): Integer; overload;
Function uiCompareValue(A: UInt8; B: Int32): Integer; overload;
Function uiCompareValue(A: UInt8; B: Int64): Integer; overload;

Function uiCompareValue(A: UInt16; B: Int8): Integer; overload;
Function uiCompareValue(A: UInt16; B: Int16): Integer; overload;
Function uiCompareValue(A: UInt16; B: Int32): Integer; overload;
Function uiCompareValue(A: UInt16; B: Int64): Integer; overload;

Function uiCompareValue(A: UInt32; B: Int8): Integer; overload;
Function uiCompareValue(A: UInt32; B: Int16): Integer; overload;
Function uiCompareValue(A: UInt32; B: Int32): Integer; overload;
Function uiCompareValue(A: UInt32; B: Int64): Integer; overload;

Function uiCompareValue(A: UInt64; B: Int8): Integer; overload;
Function uiCompareValue(A: UInt64; B: Int16): Integer; overload;
Function uiCompareValue(A: UInt64; B: Int32): Integer; overload;
Function uiCompareValue(A: UInt64; B: Int64): Integer; overload;

//------------------------------------------------------------------------------

Function uuCompareValue(A: UInt8; B: UInt16): Integer; overload;
Function uuCompareValue(A: UInt8; B: UInt32): Integer; overload;
Function uuCompareValue(A: UInt8; B: UInt64): Integer; overload;

Function uuCompareValue(A: UInt16; B: UInt8): Integer; overload;
Function uuCompareValue(A: UInt16; B: UInt32): Integer; overload;
Function uuCompareValue(A: UInt16; B: UInt64): Integer; overload;

Function uuCompareValue(A: UInt32; B: UInt8): Integer; overload;
Function uuCompareValue(A: UInt32; B: UInt16): Integer; overload;
Function uuCompareValue(A: UInt32; B: UInt64): Integer; overload;

Function uuCompareValue(A: UInt64; B: UInt8): Integer; overload;
Function uuCompareValue(A: UInt64; B: UInt16): Integer; overload;
Function uuCompareValue(A: UInt64; B: UInt32): Integer; overload;

//------------------------------------------------------------------------------

Function fiCompareValue(A: Extended; B: Int8; Epsilon: Extended = 0.0): Integer; overload;
Function fiCompareValue(A: Extended; B: Int16; Epsilon: Extended = 0.0): Integer; overload;
Function fiCompareValue(A: Extended; B: Int32; Epsilon: Extended = 0.0): Integer; overload;
Function fiCompareValue(A: Extended; B: Int64; Epsilon: Extended = 0.0): Integer; overload;

//------------------------------------------------------------------------------

Function fuCompareValue(A: Extended; B: UInt8; Epsilon: Extended = 0.0): Integer; overload;
Function fuCompareValue(A: Extended; B: UInt16; Epsilon: Extended = 0.0): Integer; overload;
Function fuCompareValue(A: Extended; B: UInt32; Epsilon: Extended = 0.0): Integer; overload;
Function fuCompareValue(A: Extended; B: UInt64; Epsilon: Extended = 0.0): Integer; overload;

//------------------------------------------------------------------------------

Function ifCompareValue(A: Int8; B: Extended; Epsilon: Extended = 0.0): Integer; overload;
Function ifCompareValue(A: Int16; B: Extended; Epsilon: Extended = 0.0): Integer; overload;
Function ifCompareValue(A: Int32; B: Extended; Epsilon: Extended = 0.0): Integer; overload;
Function ifCompareValue(A: Int64; B: Extended; Epsilon: Extended = 0.0): Integer; overload;

//------------------------------------------------------------------------------

Function ufCompareValue(A: UInt8; B: Extended; Epsilon: Extended = 0.0): Integer; overload;
Function ufCompareValue(A: UInt16; B: Extended; Epsilon: Extended = 0.0): Integer; overload;
Function ufCompareValue(A: UInt32; B: Extended; Epsilon: Extended = 0.0): Integer; overload;
Function ufCompareValue(A: UInt64; B: Extended; Epsilon: Extended = 0.0): Integer; overload;

//==============================================================================

Function CompareValue(A: Int8; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int8; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int8; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: Int16; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int16; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int16; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: Int32; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int32; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int32; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function CompareValue(A: Int64; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int64; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int64; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Int8; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int8; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int8; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int8; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: Int16; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int16; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int16; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int16; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: Int32; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int32; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int32; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int32; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function CompareValue(A: Int64; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int64; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int64; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int64; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: UInt8; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt8; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt8; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt8; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: UInt16; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt16; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt16; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt16; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: UInt32; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt32; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt32; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt32; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function CompareValue(A: UInt64; B: Int8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt64; B: Int16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt64; B: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt64; B: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: UInt8; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt8; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt8; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: UInt16; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt16; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt16; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function CompareValue(A: UInt32; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt32; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt32; B: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function CompareValue(A: UInt64; B: UInt8): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt64; B: UInt16): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt64; B: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Extended; B: Int8; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Extended; B: Int16; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Extended; B: Int32; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Extended; B: Int64; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Extended; B: UInt8; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Extended; B: UInt16; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Extended; B: UInt32; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Extended; B: UInt64; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Int8; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int16; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: Int32; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: Int64; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: UInt8; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt16; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function CompareValue(A: UInt32; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(DistinctOverloadUInt64E)}
Function CompareValue(A: UInt64; B: Extended; Epsilon: Extended = 0.0): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

//==============================================================================
(*
type
  TCompareOperation = (
    cmpEqual,cmpNotEqual,cmpLess,cmpNotLess,cmpLessOrEqual,cmpNotLessNorEqual,
    cmpGreater,cmpNotGreater,cmpGreaterOrEqual,cmpNotGreaterNorEqual);

Function iCompareValueOp(A,B: Int8; Operation: TCompareOperation = cmpEqual): Boolean;
begin
case Operation of
  cmpNotEqual:                    Result := iCompareValue(A,B) <> 0;
  cmpLess,cmpNotGreaterNorEqual:  Result := iCompareValue(A,B) < 0;
  cmpLessOrEqual,cmpNotGreater:   Result := iCompareValue(A,B) <= 0;
  cmpGreater,cmpNotLessNorEqual:  Result := iCompareValue(A,B) > 0;
  cmpGreaterOrEqual,cmpNotLess:   Result := iCompareValue(A,B) >= 0;
else
  {cmpEqual}
  Result := iCompareValue(A,B) = 0;
end;
end;

Function iIfThen(Value: Boolean; OnTrue: Int8; OnFalse: Int8 = 0): Int8;
Function iIfThen(Value: Boolean; OnTrue: Int16; OnFalse: Int16 = 0): Int16;
Function iIfThen(Value: Boolean; OnTrue: Int32; OnFalse: Int32 = 0): Int32;
Function iIfThen(Value: Boolean; OnTrue: Int64; OnFalse: Int64 = 0): Int64;

Function uIfThen(Value: Boolean; OnTrue: UInt8; OnFalse: UInt8 = 0): UInt8;
Function uIfThen(Value: Boolean; OnTrue: UInt16; OnFalse: UInt16 = 0): UInt16;
Function uIfThen(Value: Boolean; OnTrue: UInt32; OnFalse: UInt32 = 0): UInt32;
Function uIfThen(Value: Boolean; OnTrue: UInt64; OnFalse: UInt64 = 0): UInt64;

Function fIfThen(Value: Boolean; OnTrue: Extended; OnFalse: Extended = 0.0): Extended;

Function cIfThen(Value: Boolean; OnTrue: AnsiChar; OnFalse: AnsiChar = #0): AnsiChar;
Function cIfThen(Value: Boolean; OnTrue: UTF8Char; OnFalse: UTF8Char = #0): UTF8Char;
Function cIfThen(Value: Boolean; OnTrue: WideChar; OnFalse: WideChar = #0): WideChar;
Function cIfThen(Value: Boolean; OnTrue: UnicodeChar; OnFalse: UnicodeChar = #0): UnicodeChar;
Function cIfThen(Value: Boolean; OnTrue: UCS4Char; OnFalse: UCS4Char = 0): UCS4Char;
Function cIfThen(Value: Boolean; OnTrue: Char; OnFalse: Char = #0): Char;

Function sIfThen(Value: Boolean; const OnTrue: ShortString; const OnFalse: ShortString = ''): ShortString;
Function sIfThen(Value: Boolean; const OnTrue: AnsiString; const OnFalse: AnsiString = ''; UniqueCopy: Boolean = False)): AnsiString;
Function sIfThen(Value: Boolean; const OnTrue: UTF8String; const OnFalse: UTF8String = ''; UniqueCopy: Boolean = False)): UTF8String;
Function sIfThen(Value: Boolean; const OnTrue: WideString; const OnFalse: WideString = ''; UniqueCopy: Boolean = False)): WideString;
Function sIfThen(Value: Boolean; const OnTrue: UnicodeString; const OnFalse: UnicodeString = ''; UniqueCopy: Boolean = False)): UnicodeString;
Function sIfThen(Value: Boolean; const OnTrue: UCS4String; const OnFalse: UCS4String = ''; UniqueCopy: Boolean = False)): UCS4String;
Function sIfThen(Value: Boolean; const OnTrue: String; const OnFalse: String = ''; UniqueCopy: Boolean = False)): String;

Function pIfThen(Value: Boolean; OnTrue: Pointer; OnFalse: Pointer = nil): Pointer;

Function oIfThen(Value: Boolean; OnTrue: TObject; OnFalse: TObject = nil): TObject;
Function oIfThen(Value: Boolean; OnTrue: TClass; OnFalse: TClass = nil): TClass;

Function vIfThen(Value: Boolean; const OnTrue: Variant; const OnFalse: Variant = null): Variant;

procedure bIfThen(Value: Boolean; const OnTrue,OnFalse; Size: TMemSize; out Result);
procedure bIfThen(Value: Boolean; OnTrue,OnFalse: Pointer; Size: TMemSize; Result: Pointer);

Function gIfThen(Value: Boolean; const OnTrue: TGUID; const OnFalse: TGUID = ???): TGUID;

InRange
EnsureRange
*)
implementation

{$IF (SizeOf(Extended) <> 10) and (SizeOf(Extended) <> 8)}
  {$MESSAGE FATAL 'Unsupported size of type Extended.'}
{$IFEND}

{===============================================================================
    Internals
===============================================================================}
const
  iTwoPow64: UInt32 = $5F800000;  // 18446744073709551616 (2^64)
  iTwoPow63: UInt32 = $5F000000;  // 9223372036854775808  (2^63)
var
  TwoPow64: Single absolute iTwoPow64;
  TwoPow63: Single absolute iTwoPow63;

//------------------------------------------------------------------------------
{$IF not Declared(NativeUInt64E)}
type
  UInt64Rec = packed record
    case Integer of
      0: (Lo, Hi: UInt32);
      1: (Cardinals: array [0..1] of UInt32);
      2: (Words: array [0..3] of UInt16);
      3: (Bytes: array [0..7] of UInt8);
  end;
{$IFEND}
//------------------------------------------------------------------------------

Function CompareUInt64(A,B: UInt64): Integer;
begin
{$IF Declared(NativeUInt64E)}
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
{$ELSE}
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
{$IFEND}
end;

{===============================================================================
    Public auxiliary funtions - implementation
===============================================================================}

Function Int64ToFloat(N: Int64): Extended;
begin
{$IF SizeOf(Extended) <> 10}
If (N < AM_INT_DBL_LO) or (N > AM_INT_DBL_HI) then
  raise EAMInvalidOperation.CreateFmt('Int64ToFloat: Integer value (%d) cannot be accurately converted to Extended.',[N]);
{$IFEND}
Result := N;
end;

//------------------------------------------------------------------------------

Function FloatToInt64(N: Extended): Int64;
begin
If Frac(N) <> 0.0 then
  raise EAMInvalidOperation.CreateFmt('FloatToInt64: Floating point value (%g) cannot be stored in Int64.',[N]);
If (N < -TwoPow63) or (N >= TwoPow63) then
  raise EAMInvalidOperation.CreateFmt('FloatToInt64: Floating point value (%g) cannot fit into Int64.',[N]);
{$IF SizeOf(Extended) <> 10}
If (N < AM_INT_DBL_LO) or (N > AM_INT_DBL_HI) then
  raise EAMInvalidOperation.CreateFmt('FloatToInt64: Floating point value (%g) cannot be accurately converted to Int64.',[N]);
{$IFEND}
Result := Trunc(N);
end;

//------------------------------------------------------------------------------

Function UInt64ToFloat(N: UInt64): Extended;
begin
{
  Here we assume that the compiler cannot properly transfer integer bigger than
  High(Int64) into floating point number (note the modern compilers can do it).

  We let the integer to be loaded as if it was signed and then, if it is
  negative, add 2^64 to it, which will bring the float to a proper positive
  value.
}
{$IF SizeOf(Extended) <> 10}
If CompareUInt64(N,AM_INT_DBL_HI) > 0 then
  raise EAMInvalidOperation.CreateFmt('UInt64ToFloat: Integer value (%u) cannot be accurately converted to Extended.',[N]);
{$IFEND}
If (N and UInt64($8000000000000000)) <> 0 then
  Result := Int64(N) + TwoPow64
else
  Result := N;
end;

//------------------------------------------------------------------------------

Function FloatToUInt64(N: Extended): UInt64;
begin
If Frac(N) <> 0.0 then
  raise EAMInvalidOperation.CreateFmt('FloatToUInt64: Floating point value (%g) cannot be stored in UInt64.',[N]);
If (N < 0) or (N >= TwoPow64) then
  raise EAMInvalidOperation.CreateFmt('FloatToUInt64: Floating point value (%g) cannot fit into UInt64.',[N]);
If N >= TwoPow63 then
  N := N - TwoPow64;
{$IF SizeOf(Extended) <> 10}
If (N < AM_INT_DBL_LO) or (N > AM_INT_DBL_HI) then
  raise EAMInvalidOperation.CreateFmt('FloatToUInt64: Floating point value (%g) cannot be accurately converted to UInt64.',[N]);
{$IFEND}
Result := Trunc(N);
end;


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

{$IFDEF AM_OverflowChecks}
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
{$ENDIF}

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
{$IFNDEF FPC}{$IF Defined(AM_OverflowChecks) and (CompilerVersion < 18)}{$Q-}
  procedure iDivMod_CalcRemainder;
  begin
    Remainder := Dividend - (Quotient * Divisor);
  end;
{$Q+}{$IFEND}{$ENDIF}
begin
Quotient := Dividend div Divisor;
{
  Following (the multiplication) sometimes signals overflow even when it should
  not (in D7 at least, where __llmulo is called), therefore, in Delphi older
  than 2006 (because I have no idea when it was corrected, but newer versions
  have full support for UInt64, so I assume it is corrected there already), we
  call nested function where the overflows are explicitly disabled to prevent
  problems.
}
{$IF Declared(iDivMod_CalcRemainder)}
iDivMod_CalcRemainder;
{$ELSE}
Remainder := Dividend - (Quotient * Divisor);
{$IFEND}
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
Remainder := Dividend - UInt32(Quotient * Divisor);
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
{$IFDEF AM_OverflowChecks}{$Q-}{$ENDIF}  
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
{$IFDEF AM_OverflowChecks}{$Q+}{$ENDIF}
{$IFEND}
{$ENDIF}

{-------------------------------------------------------------------------------
    DivMod - common-name overloads
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

//------------------------------------------------------------------------------

procedure DivMod(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8);
begin
uDivMod(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivMod(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16);
begin
uDivMod(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivMod(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32);
begin
uDivMod(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivMod(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64);
begin
uDivMod(Dividend,Divisor,Quotient,Remainder);
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
  The line "Result := Result + 1" can produce overflow if UInt64 is declared
  only as an alias to Int64.
}
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // Result + 1 can overflow
Function uDivCeil(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivMod(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Result := Result + 1;
end;
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}

{-------------------------------------------------------------------------------
    DivCeil - common-name overloads
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

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: Int64): Int64;
begin
Result := iDivCeil(Dividend,Divisor);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function DivCeil(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivCeil(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivCeil(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivCeil(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeil(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivCeil(Dividend,Divisor);
end;
{$IFEND}


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
    DivFloor - common-name overloads
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

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: Int64): Int64;
begin
Result := iDivFloor(Dividend,Divisor);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function DivFloor(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivFloor(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivFloor(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivFloor(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloor(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivFloor(Dividend,Divisor);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                              64bit ceil and floor                             
--------------------------------------------------------------------------------
===============================================================================}

Function Ceil64(N: Extended): Int64;
begin
// no need to check limits, Trunc is doing that
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

//------------------------------------------------------------------------------

Function CeilU64(N: Extended): UInt64;
begin
If (N >= 0) and (N < TwoPow64) then
  begin
    If N >= TwoPow63 then
      Result := UInt64(Trunc(N - TwoPow64))
    else
      Result := Trunc(N);
    If Frac(N) <> 0 then
      Result := Result + 1;
  end
else raise EAMInvalidOperation.Create('CeilU64: Number cannot fit into result.');
end;

//------------------------------------------------------------------------------

Function FloorU64(N: Extended): UInt64;
begin
If (N >= 0) and (N < TwoPow64) then
  begin
    If N >= TwoPow63 then
      Result := UInt64(Trunc(N - TwoPow64))
    else
      Result := Trunc(N);
    {
      Since UInt64 cannot be negative, there is no point in checking fraction
      (result will never be decremented).
    }
  end
else raise EAMInvalidOperation.Create('FloorU64: Number cannot fit into result.');
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

{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // N - 1 can overflow
Function uIsPow2(N: UInt64): Boolean;
begin
If CompareUInt64(N,0) > 0 then
  Result := N and (N - 1) = 0
else
  Result := False;
end;
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}

{-------------------------------------------------------------------------------
    IsPow2 - common-name overloads
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

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: Int64): Boolean;
begin
Result := iIsPow2(N);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function IsPow2(N: UInt8): Boolean;
begin
Result := uIsPow2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: UInt16): Boolean; 
begin
Result := uIsPow2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: UInt32): Boolean;   
begin
Result := uIsPow2(N);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IsPow2(N: UInt64): Boolean; 
begin
Result := uIsPow2(N);
end;  
{$IFEND}


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
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // (N - 1) can overflow
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
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}
{$ENDIF}

{-------------------------------------------------------------------------------
    IntLog2 - common-name overloads
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

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: Int64): Int32;
begin
Result := iIntLog2(N);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function IntLog2(N: UInt8): Int32;
begin
Result := uIntLog2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: UInt16): Int32;
begin
Result := uIntLog2(N);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: UInt32): Int32;
begin
Result := uIntLog2(N);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function IntLog2(N: UInt64): Int32;
begin
Result := uIntLog2(N);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
             Try combined division and modulo by integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    TryDivModPow2 - internal functions
-------------------------------------------------------------------------------}

{$IFDEF PurePascal}

Function SAR_8(Value: UInt8; Shift: Integer): UInt8;
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAR_16(Value: UInt16; Shift: Integer): UInt16;
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAR_32(Value: UInt32; Shift: Integer): UInt32;
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAR_64(Value: UInt64; Shift: Integer): UInt64;
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

{$ENDIF}

{-------------------------------------------------------------------------------
    iTryDivModPow2 - signed integers
-------------------------------------------------------------------------------}

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
var
  PowerExp:     Int32;
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
      {
        Dividend + PredDivisor cannot overflow. Dividend cannot be bigger than
        -1 (because we are in a negative-dividend branch) and PredDivisor is
        always positive and never bigger than 63 (0x3F).
      }
        Quotient := Int8(SAR_8(UInt8(Dividend + PredDivisor),PowerExp));
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
var
  PowerExp:     Int32;
  PredDivisor:  Int16;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    If Dividend < 0 then
      begin
        PredDivisor := Pred(Divisor);
        Quotient := Int16(SAR_16(UInt16(Dividend + PredDivisor),PowerExp));
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
var
  PowerExp:     Int32;
  PredDivisor:  Int32;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    If Dividend < 0 then
      begin
        PredDivisor := Pred(Divisor);
        Quotient := Int32(SAR_32(UInt32(Dividend + PredDivisor),PowerExp));
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
var
  PowerExp:     Int32;
  PredDivisor:  Int64;
begin
PowerExp := iIntLog2(Divisor);
If PowerExp >= 0 then
  begin
    If Dividend < 0 then
      begin
        PredDivisor := (Divisor - 1);
        Quotient := Int64(SAR_64(UInt64(Dividend + PredDivisor),PowerExp));
        Remainder := Dividend and PredDivisor;
        If Remainder <> 0 then
          Remainder := Remainder or Int64(UInt64(-1) shl PowerExp);
      end
    else
      begin
        Quotient := Dividend shr PowerExp;
        Remainder := Dividend and (Divisor - 1);
      end;
    Result := True;
  end
else Result := False;
end;
{$ENDIF}

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
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // (Divisor - 1) can overflow
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
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}
{$ENDIF}

{-------------------------------------------------------------------------------
    TryDivModPow2 - common-name overloads
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

//------------------------------------------------------------------------------

Function TryDivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean;
begin
Result := uTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryDivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean;
begin
Result := uTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean;
begin
Result := uTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryDivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean;
begin
Result := uTryDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;


{===============================================================================
--------------------------------------------------------------------------------
               Combined division and modulo by integer power of 2
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivModPow2 - signed integers
-------------------------------------------------------------------------------}

Function iDivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean;
begin
If not iTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    iDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean;
begin
If not iTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    iDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean;
begin
If not iTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    iDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean;
begin
If not iTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    iDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

{-------------------------------------------------------------------------------
    uDivModPow2 - unsigned integers
-------------------------------------------------------------------------------}

Function uDivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean;
begin
If not uTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    uDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean;
begin
If not uTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    uDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean;
begin
If not uTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    uDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean;
begin
If not uTryDivModPow2(Dividend,Divisor,Quotient,Remainder) then
  begin
    uDivMod(Dividend,Divisor,Quotient,Remainder);
    Result := False;
  end
else Result := True;
end;

{-------------------------------------------------------------------------------
    DivModPow2 - common-name overloads
-------------------------------------------------------------------------------}

Function DivModPow2(Dividend,Divisor: Int8; out Quotient,Remainder: Int8): Boolean;
begin
Result := iDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivModPow2(Dividend,Divisor: Int16; out Quotient,Remainder: Int16): Boolean;
begin
Result := iDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivModPow2(Dividend,Divisor: Int32; out Quotient,Remainder: Int32): Boolean;
begin
Result := iDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivModPow2(Dividend,Divisor: Int64; out Quotient,Remainder: Int64): Boolean;
begin
Result := iDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

//------------------------------------------------------------------------------

Function DivModPow2(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8): Boolean;
begin
Result := uDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivModPow2(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16): Boolean;
begin
Result := uDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivModPow2(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32): Boolean;
begin
Result := uDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivModPow2(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64): Boolean;
begin
Result := uDivModPow2(Dividend,Divisor,Quotient,Remainder);
end;


{===============================================================================
--------------------------------------------------------------------------------
            Combined division and ceil (optimized for pow2 divisor)
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivCeilPow2 - signed integers
-------------------------------------------------------------------------------}

Function iDivCeilPow2(Dividend,Divisor: Int8): Int8;
var
  Remainder:  Int8;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2(Dividend,Divisor: Int16): Int16;
var
  Remainder:  Int16;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2(Dividend,Divisor: Int32): Int32;
var
  Remainder:  Int32;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2(Dividend,Divisor: Int64): Int64;
var
  Remainder:  Int64;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

{-------------------------------------------------------------------------------
    uDivCeilPow2 - unsigned integers
-------------------------------------------------------------------------------}

Function uDivCeilPow2(Dividend,Divisor: UInt8): UInt8;
var
  Remainder:  UInt8;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2(Dividend,Divisor: UInt16): UInt16;
var
  Remainder:  UInt16;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2(Dividend,Divisor: UInt32): UInt32;
var
  Remainder:  UInt32;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // Result + 1 can overflow 
Function uDivCeilPow2(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Result := Result + 1;
end;
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}

{-------------------------------------------------------------------------------
    DivCeilPow2 - common-name overloads
-------------------------------------------------------------------------------}

Function DivCeilPow2(Dividend,Divisor: Int8): Int8;
begin
Result := iDivCeilPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2(Dividend,Divisor: Int16): Int16;
begin
Result := iDivCeilPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2(Dividend,Divisor: Int32): Int32;
begin
Result := iDivCeilPow2(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2(Dividend,Divisor: Int64): Int64;
begin
Result := iDivCeilPow2(Dividend,Divisor);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function DivCeilPow2(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivCeilPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivCeilPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivCeilPow2(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivCeilPow2(Dividend,Divisor);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
            Combined division and floor (optimized for pow2 divisor)
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivFloorPow2 - signed integers
-------------------------------------------------------------------------------}

Function iDivFloorPow2(Dividend,Divisor: Int8): Int8;
var
  Remainder:  Int8;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2(Dividend,Divisor: Int16): Int16;
var
  Remainder:  Int16;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2(Dividend,Divisor: Int32): Int32;
var
  Remainder:  Int32;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2(Dividend,Divisor: Int64): Int64;
var
  Remainder:  Int64;
begin
iDivModPow2(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

{-------------------------------------------------------------------------------
    uDivFloorPow2 - unsigned integers
-------------------------------------------------------------------------------}

Function uDivFloorPow2(Dividend,Divisor: UInt8): UInt8;
var
  Remainder:  UInt8;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2(Dividend,Divisor: UInt16): UInt16;
var
  Remainder:  UInt16;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2(Dividend,Divisor: UInt32): UInt32;
var
  Remainder:  UInt32;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivModPow2(Dividend,Divisor,Result,Remainder);
end;

{-------------------------------------------------------------------------------
    DivFloorPow2 - common-name overloads
-------------------------------------------------------------------------------}

Function DivFloorPow2(Dividend,Divisor: Int8): Int8;
begin
Result := iDivFloorPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2(Dividend,Divisor: Int16): Int16;
begin
Result := iDivFloorPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2(Dividend,Divisor: Int32): Int32;
begin
Result := iDivFloorPow2(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2(Dividend,Divisor: Int64): Int64;
begin
Result := iDivFloorPow2(Dividend,Divisor);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function DivFloorPow2(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivFloorPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivFloorPow2(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivFloorPow2(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivFloorPow2(Dividend,Divisor);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
         Combined division and modulo by integer power of 2 (no checks)          
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    DivModPow2NoCheck - internal functions
-------------------------------------------------------------------------------}

{$IFDEF PurePascal}

Function BSF_8(N: UInt8): Integer;
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  For i := 0 to 7 do
    If ((N shr i) and 1) <> 0 then
      begin
        Result := i;
        Break{For i};
      end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSF_16(N: UInt16): Integer;
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  For i := 0 to 15 do
    If ((N shr i) and 1) <> 0 then
      begin
        Result := i;
        Break{For i};
      end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSF_32(N: UInt32): Integer;
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  For i := 0 to 31 do
    If ((N shr i) and 1) <> 0 then
      begin
        Result := i;
        Break{For i};
      end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSF_64(N: UInt64): Integer;
var
  i:  Integer;
begin
Result := -1;
If N <> 0 then
  For i := 0 to 63 do
    If ((N shr i) and 1) <> 0 then
      begin
        Result := i;
        Break{For i};
      end;
end;

{$ENDIF}

{-------------------------------------------------------------------------------
    iDivModPow2NoCheck - signed integers
-------------------------------------------------------------------------------}

procedure iDivModPow2NoCheck(Dividend,Divisor: Int8; out Quotient,Remainder: Int8); overload;
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

    MOV   AL, CL
    AND   DX, $FF
    BSF   CX, DX
    DEC   DL

    TEST  AL, AL
    JNS   @PositiveDividend

    MOV   R10B, AL
    ADD   AL, DL
    SAR   AL, CL

    AND   DL, R10B
    JZ    @StoreResults
    MOV   R10B, -1
    SHL   R10B, CL
    OR    DL, R10B
    JMP   @StoreResults

@PositiveDividend:

    AND   DL, AL
    SHR   AL, CL

@StoreResults:

    MOV   byte ptr [R8], AL
    MOV   byte ptr [R9], DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    AND   SI, $FF
    BSF   CX, SI
    DEC   SIL

    TEST  DIL, DIL
    JNS   @PositiveDividend

    MOV   R8B, DIL
    ADD   DIL, SIL
    SAR   DIL, CL

    AND   SIL, R8B
    JZ    @StoreResults
    MOV   R8B, -1
    SHL   R8B, CL
    OR    SIL, R8B
    JMP   @StoreResults

@PositiveDividend:

    AND   SIL, DIL
    SHR   DIL, CL

@StoreResults:    

    MOV   byte ptr [RDX], DIL
    MOV   byte ptr [RAX], SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX
    PUSH  ECX

    AND   DX, $FF
    BSF   CX, DX
    DEC   DL

    TEST  AL, AL
    JNS   @PositiveDividend

    MOV   BL, AL
    ADD   AL, DL
    SAR   AL, CL

    AND   DL, BL
    JZ    @StoreResults
    MOV   BL, -1
    SHL   BL, CL
    OR    DL, BL
    JMP   @StoreResults

@PositiveDividend:

    AND   DL, AL
    SHR   AL, CL

@StoreResults:

    POP   ECX
    MOV   byte ptr [ECX], AL
    MOV   ECX, dword ptr [Remainder]
    MOV   byte ptr [ECX], DL

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp:     Int32;
  PredDivisor:  Int8;
begin
PowerExp := BSF_8(UInt8(Divisor));
If Dividend < 0 then
  begin
    PredDivisor := Pred(Divisor);
    Quotient := Int8(SAR_8(UInt8(Dividend + PredDivisor),PowerExp));
    Remainder := Dividend and PredDivisor;
    If Remainder <> 0 then
      Remainder := Remainder or Int8(UInt8(-1) shl PowerExp);
  end
else
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and Pred(Divisor);
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivModPow2NoCheck(Dividend,Divisor: Int16; out Quotient,Remainder: Int16); overload;
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

    MOV   AX, CX
    BSF   CX, DX
    DEC   DX

    TEST  AX, AX
    JNS   @PositiveDividend

    MOV   R10W, AX
    ADD   AX, DX
    SAR   AX, CL

    AND   DX, R10W
    JZ    @StoreResults
    MOV   R10W, -1
    SHL   R10W, CL
    OR    DX, R10W
    JMP   @StoreResults

@PositiveDividend:

    AND   DX, AX
    SHR   AX, CL

@StoreResults:

    MOV   word ptr [R8], AX
    MOV   word ptr [R9], DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    BSF   CX, SI
    DEC   SI

    TEST  DI, DI
    JNS   @PositiveDividend

    MOV   R8W, DI
    ADD   DI, SI
    SAR   DI, CL

    AND   SI, R8W
    JZ    @StoreResults
    MOV   R8W, -1
    SHL   R8W, CL
    OR    SI, R8W
    JMP   @StoreResults

@PositiveDividend:

    AND   SI, DI
    SHR   DI, CL

@StoreResults:    

    MOV   word ptr [RDX], DI
    MOV   word ptr [RAX], SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX
    PUSH  ECX

    BSF   CX, DX
    DEC   DX

    TEST  AX, AX
    JNS   @PositiveDividend

    MOV   BX, AX
    ADD   AX, DX
    SAR   AX, CL

    AND   DX, BX
    JZ    @StoreResults
    MOV   BX, -1
    SHL   BX, CL
    OR    DX, BX
    JMP   @StoreResults

@PositiveDividend:

    AND   DX, AX
    SHR   AX, CL

@StoreResults:

    POP   ECX
    MOV   word ptr [ECX], AX
    MOV   ECX, dword ptr [Remainder]
    MOV   word ptr [ECX], DX

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp:     Int32;
  PredDivisor:  Int16;
begin
PowerExp := BSF_16(UInt16(Divisor));
If Dividend < 0 then
  begin
    PredDivisor := Pred(Divisor);
    Quotient := Int16(SAR_16(UInt16(Dividend + PredDivisor),PowerExp));
    Remainder := Dividend and PredDivisor;
    If Remainder <> 0 then
      Remainder := Remainder or Int16(UInt16(-1) shl PowerExp);
  end
else
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and Pred(Divisor);
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivModPow2NoCheck(Dividend,Divisor: Int32; out Quotient,Remainder: Int32); overload;
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

    MOV   EAX, ECX
    BSF   ECX, EDX
    DEC   EDX

    TEST  EAX, EAX
    JNS   @PositiveDividend

    MOV   R10D, EAX
    ADD   EAX, EDX
    SAR   EAX, CL

    AND   EDX, R10D
    JZ    @StoreResults
    MOV   R10D, -1
    SHL   R10D, CL
    OR    EDX, R10D
    JMP   @StoreResults

@PositiveDividend:

    AND   EDX, EAX
    SHR   EAX, CL

@StoreResults:

    MOV   dword ptr [R8], EAX
    MOV   dword ptr [R9], EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    BSF   ECX, ESI
    DEC   ESI

    TEST  EDI, EDI
    JNS   @PositiveDividend

    MOV   R8D, EDI
    ADD   EDI, ESI
    SAR   EDI, CL

    AND   ESI, R8D
    JZ    @StoreResults
    MOV   R8D, -1
    SHL   R8D, CL
    OR    ESI, R8D
    JMP   @StoreResults

@PositiveDividend:

    AND   ESI, EDI
    SHR   EDI, CL

@StoreResults:    

    MOV   dword ptr [RDX], EDI
    MOV   dword ptr [RAX], ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX
    PUSH  ECX

    BSF   ECX, EDX
    DEC   EDX

    TEST  EAX, EAX
    JNS   @PositiveDividend

    MOV   EBX, EAX
    ADD   EAX, EDX
    SAR   EAX, CL

    AND   EDX, EBX
    JZ    @StoreResults
    MOV   EBX, -1
    SHL   EBX, CL
    OR    EDX, EBX
    JMP   @StoreResults

@PositiveDividend:

    AND   EDX, EAX
    SHR   EAX, CL

@StoreResults:

    POP   ECX
    MOV   dword ptr [ECX], EAX
    MOV   ECX, dword ptr [Remainder]
    MOV   dword ptr [ECX], EDX

    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp:     Int32;
  PredDivisor:  Int32;
begin
PowerExp := BSF_32(UInt32(Divisor));
If Dividend < 0 then
  begin
    PredDivisor := Pred(Divisor);
    Quotient := Int32(SAR_32(UInt32(Dividend + PredDivisor),PowerExp));
    Remainder := Dividend and PredDivisor;
    If Remainder <> 0 then
      Remainder := Remainder or Int32(UInt32(-1) shl PowerExp);
  end
else
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and Pred(Divisor);
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivModPow2NoCheck(Dividend,Divisor: Int64; out Quotient,Remainder: Int64); overload;
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

    MOV   RAX, RCX
    BSF   RCX, RDX
    DEC   RDX

    TEST  RAX, RAX
    JNS   @PositiveDividend

    MOV   R10, RAX
    ADD   RAX, RDX
    SAR   RAX, CL

    AND   RDX, R10
    JZ    @StoreResults
    MOV   R10, -1
    SHL   R10, CL
    OR    RDX, R10
    JMP   @StoreResults

@PositiveDividend:

    AND   RDX, RAX
    SHR   RAX, CL

@StoreResults:

    MOV   qword ptr [R8], RAX
    MOV   qword ptr [R9], RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    BSF   RCX, RSI
    DEC   RSI

    TEST  RDI, RDI
    JNS   @PositiveDividend

    MOV   R8, RDI
    ADD   RDI, RSI
    SAR   RDI, CL

    AND   RSI, R8
    JZ    @StoreResults
    MOV   R8, -1
    SHL   R8, CL
    OR    RSI, R8
    JMP   @StoreResults

@PositiveDividend:

    AND   RSI, RDI
    SHR   RDI, CL

@StoreResults:    

    MOV   qword ptr [RDX], RDI
    MOV   qword ptr [RAX], RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EBX
    PUSH  ESI
    PUSH  EDI

    PUSH  EAX
    PUSH  EDX  

    // load divisor
    MOV   EDI, dword ptr [Divisor]
    MOV   ESI, dword ptr [Divisor + 4]

    // do bit scan
    TEST  EDI, EDI
    JZ    @DivisorScanHigh

    BSF   ECX, EDI
    JMP   @DividendCheckSign

@DivisorScanHigh:

    BSF   ECX, ESI
    ADD   ECX, 32

@DividendCheckSign:

    SUB   EDI, 1
    SBB   ESI, 0

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
    
    // fill EDX with sign bit
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

    JMP   @RoutineEnd

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

@RoutineEnd:

    ADD   ESP, 8
    POP   EDI
    POP   ESI
    POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  PowerExp:     Int32;
  PredDivisor:  Int64;
begin
PowerExp := BSF_64(UInt64(Divisor));
If Dividend < 0 then
  begin
    PredDivisor := (Divisor - 1);
    Quotient := Int64(SAR_64(UInt64(Dividend + PredDivisor),PowerExp));
    Remainder := Dividend and PredDivisor;
    If Remainder <> 0 then
      Remainder := Remainder or Int64(UInt64(-1) shl PowerExp);
  end
else
  begin
    Quotient := Dividend shr PowerExp;
    Remainder := Dividend and (Divisor - 1);
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    uDivModPow2NoCheck - unsigned integers
-------------------------------------------------------------------------------}

procedure uDivModPow2NoCheck(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8);
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

    MOV   AL, CL
    AND   DX, $FF
    BSF   CX, DX
    DEC   DL
    AND   DL, AL
    SHR   AL, CL

    MOV   byte ptr [R8], AL
    MOV   byte ptr [R9], DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    AND   SI, $FF
    BSF   CX, SI
    DEC   SIL
    AND   SIL, DIL
    SHR   DIL, CL

    MOV   byte ptr [RDX], DIL
    MOV   byte ptr [RAX], SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  ECX

    AND   DX, $FF
    BSF   CX, DX
    DEC   DL
    AND   DL, AL
    SHR   AL, CL

    POP   ECX
    MOV   byte ptr [ECX], AL
    MOV   ECX, dword ptr [Remainder]
    MOV   byte ptr [ECX], DL

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend shr BSF_8(Divisor);
Remainder := Dividend and Pred(Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivModPow2NoCheck(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16);
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

    MOV   AX, CX
    BSF   CX, DX
    DEC   DX
    AND   DX, AX
    SHR   AX, CL

    MOV   word ptr [R8], AX
    MOV   word ptr [R9], DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    BSF   CX, SI
    DEC   SI
    AND   SI, DI
    SHR   DI, CL

    MOV   word ptr [RDX], DI
    MOV   word ptr [RAX], SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  ECX

    BSF   CX, DX
    DEC   DX
    AND   DX, AX
    SHR   AX, CL

    POP   ECX
    MOV   word ptr [ECX], AX
    MOV   ECX, dword ptr [Remainder]
    MOV   word ptr [ECX], DX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend shr BSF_16(Divisor);
Remainder := Dividend and Pred(Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivModPow2NoCheck(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32);
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

    MOV   EAX, ECX
    BSF   ECX, EDX
    DEC   EDX
    AND   EDX, EAX
    SHR   EAX, CL

    MOV   dword ptr [R8], EAX
    MOV   dword ptr [R9], EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    BSF   ECX, ESI
    DEC   ESI
    AND   ESI, EDI
    SHR   EDI, CL

    MOV   dword ptr [RDX], EDI
    MOV   dword ptr [RAX], ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  ECX

    BSF   ECX, EDX
    DEC   EDX
    AND   EDX, EAX
    SHR   EAX, CL

    POP   ECX
    MOV   dword ptr [ECX], EAX
    MOV   ECX, dword ptr [Remainder]
    MOV   dword ptr [ECX], EDX

{$ENDIF}
end;
{$ELSE}
begin
Quotient := Dividend shr BSF_32(Divisor);
Remainder := Dividend and Pred(Divisor);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivModPow2NoCheck(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64);
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

    MOV   RAX, RCX
    BSF   RCX, RDX
    DEC   RDX
    AND   RDX, RAX
    SHR   RAX, CL

    MOV   qword ptr [R8], RAX
    MOV   qword ptr [R9], RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    MOV   RAX, RCX
    BSF   RCX, RSI
    DEC   RSI
    AND   RSI, RDI
    SHR   RDI, CL

    MOV   qword ptr [RDX], RDI
    MOV   qword ptr [RAX], RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    PUSH  EDI
    PUSH  ESI

    PUSH  EDX
    PUSH  EAX

    MOV   EDI, dword ptr [Divisor]
    MOV   ESI, dword ptr [Divisor + 4]

    TEST  EDI, EDI
    JZ    @ScanDivisorHigh
    BSF   ECX, EDI
    JZ    @RoutineEnd       // error
    JMP   @LoadDividend

@ScanDivisorHigh:

    BSF   ECX, ESI
    JZ    @RoutineEnd       // error
    ADD   ECX, 32

@LoadDividend:

    MOV   EAX, dword ptr [Dividend]
    MOV   EDX, dword ptr [Dividend + 4]

    SUB   EDI, 1
    SBB   ESI, 0

    AND   EDI, EAX
    AND   ESI, EDX

    CMP   CL, 31
    JA    @DividendShiftAbove31
    SHRD  EAX, EDX, CL
    SHR   EDX, CL
    JMP   @StoreResults

@DividendShiftAbove31:

    MOV   EAX, EDX
    XOR   EDX, EDX
    AND   CL, 31
    SHR   EAX, CL

@StoreResults:

    MOV   ECX, dword ptr [ESP]
    MOV   dword ptr [ECX], EAX
    MOV   dword ptr [ECX + 4], EDX

    MOV   ECX, dword ptr [ESP + 4]
    MOV   dword ptr [ECX], EDI
    MOV   dword ptr [ECX + 4], ESI

@RoutineEnd:

    ADD   ESP, 8
    POP   ESI
    POP   EDI

{$ENDIF}
end;
{$ELSE}
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // Divisor - 1 can overflow
begin
Quotient := Dividend shr BSF_64(Divisor);
Remainder := Dividend and (Divisor - 1);
end;
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}
{$ENDIF}

{-------------------------------------------------------------------------------
    DivModPow2NoCheck - common-name overloads
-------------------------------------------------------------------------------}

procedure DivModPow2NoCheck(Dividend,Divisor: Int8; out Quotient,Remainder: Int8);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NoCheck(Dividend,Divisor: Int16; out Quotient,Remainder: Int16);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NoCheck(Dividend,Divisor: Int32; out Quotient,Remainder: Int32);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NoCheck(Dividend,Divisor: Int64; out Quotient,Remainder: Int64);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

//------------------------------------------------------------------------------

procedure DivModPow2NoCheck(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NoCheck(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NoCheck(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NoCheck(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

{-------------------------------------------------------------------------------
    *DivModPow2NC - shortened sliases
-------------------------------------------------------------------------------}

procedure iDivModPow2NC(Dividend,Divisor: Int8; out Quotient,Remainder: Int8);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivModPow2NC(Dividend,Divisor: Int16; out Quotient,Remainder: Int16);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivModPow2NC(Dividend,Divisor: Int32; out Quotient,Remainder: Int32);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure iDivModPow2NC(Dividend,Divisor: Int64; out Quotient,Remainder: Int64);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

//------------------------------------------------------------------------------

procedure uDivModPow2NC(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivModPow2NC(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivModPow2NC(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure uDivModPow2NC(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

//------------------------------------------------------------------------------

procedure DivModPow2NC(Dividend,Divisor: Int8; out Quotient,Remainder: Int8);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: Int16; out Quotient,Remainder: Int16);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: Int32; out Quotient,Remainder: Int32);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: Int64; out Quotient,Remainder: Int64);
begin
iDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: UInt8; out Quotient,Remainder: UInt8);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: UInt16; out Quotient,Remainder: UInt16);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: UInt32; out Quotient,Remainder: UInt32);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DivModPow2NC(Dividend,Divisor: UInt64; out Quotient,Remainder: UInt64);
begin
uDivModPow2NoCheck(Dividend,Divisor,Quotient,Remainder);
end;


{===============================================================================
--------------------------------------------------------------------------------
      Combined division and ceiling (optimized for pow2 divisor, no checks)     
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivCeilPow2NoCheck - signed integers
-------------------------------------------------------------------------------}

Function iDivCeilPow2NoCheck(Dividend,Divisor: Int8): Int8;
var
  Remainder:  Int8;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2NoCheck(Dividend,Divisor: Int16): Int16;
var
  Remainder:  Int16;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2NoCheck(Dividend,Divisor: Int32): Int32;
var
  Remainder:  Int32;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2NoCheck(Dividend,Divisor: Int64): Int64;
var
  Remainder:  Int64;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result > 0) and (Remainder <> 0) then
  Inc(Result);
end;

{-------------------------------------------------------------------------------
    uDivCeilPow2NoCheck - unsigned integers
-------------------------------------------------------------------------------}

Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt8): UInt8;
var
  Remainder:  UInt8;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt16): UInt16;
var
  Remainder:  UInt16;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt32): UInt32;
var
  Remainder:  UInt32;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Inc(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q-}{$IFEND} // Result + 1 can overflow
Function uDivCeilPow2NoCheck(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If Remainder <> 0 then
  Result := Result + 1;
end;
{$IF not Declared(NativeUInt64E) and Defined(AM_OverflowChecks)}{$Q+}{$IFEND}

{-------------------------------------------------------------------------------
    DivCeilPow2NoCheck - common-name overloads
-------------------------------------------------------------------------------}

Function DivCeilPow2NoCheck(Dividend,Divisor: Int8): Int8;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NoCheck(Dividend,Divisor: Int16): Int16;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NoCheck(Dividend,Divisor: Int32): Int32;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NoCheck(Dividend,Divisor: Int64): Int64;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function DivCeilPow2NoCheck(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NoCheck(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NoCheck(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NoCheck(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}

{-------------------------------------------------------------------------------
    *DivCeilPow2NC - shortened sliases
-------------------------------------------------------------------------------}

Function iDivCeilPow2NC(Dividend,Divisor: Int8): Int8;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2NC(Dividend,Divisor: Int16): Int16;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2NC(Dividend,Divisor: Int32): Int32;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivCeilPow2NC(Dividend,Divisor: Int64): Int64;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

//------------------------------------------------------------------------------

Function uDivCeilPow2NC(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2NC(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2NC(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivCeilPow2NC(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

//------------------------------------------------------------------------------

Function DivCeilPow2NC(Dividend,Divisor: Int8): Int8;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: Int16): Int16;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: Int32): Int32;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: Int64): Int64;
begin
Result := iDivCeilPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivCeilPow2NC(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivCeilPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
       Combined division and floor (optimized for pow2 divisor, no checks)           
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iDivFloorPow2NoCheck - signed integers
-------------------------------------------------------------------------------}

Function iDivFloorPow2NoCheck(Dividend,Divisor: Int8): Int8;
var
  Remainder:  Int8;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2NoCheck(Dividend,Divisor: Int16): Int16;
var
  Remainder:  Int16;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2NoCheck(Dividend,Divisor: Int32): Int32;
var
  Remainder:  Int32;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2NoCheck(Dividend,Divisor: Int64): Int64;
var
  Remainder:  Int64;
begin
iDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
If (Result < 0) and (Remainder <> 0) then
  Dec(Result);
end;

{-------------------------------------------------------------------------------
    uDivFloorPow2NoCheck - unsigned integers
-------------------------------------------------------------------------------}

Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt8): UInt8;
var
  Remainder:  UInt8;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt16): UInt16;
var
  Remainder:  UInt16;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt32): UInt32;
var
  Remainder:  UInt32;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2NoCheck(Dividend,Divisor: UInt64): UInt64;
var
  Remainder:  UInt64;
begin
uDivModPow2NoCheck(Dividend,Divisor,Result,Remainder);
end;

{-------------------------------------------------------------------------------
    DivFloorPow2NoCheck - common-name overloads
-------------------------------------------------------------------------------}

Function DivFloorPow2NoCheck(Dividend,Divisor: Int8): Int8;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NoCheck(Dividend,Divisor: Int16): Int16;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NoCheck(Dividend,Divisor: Int32): Int32;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NoCheck(Dividend,Divisor: Int64): Int64;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function DivFloorPow2NoCheck(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NoCheck(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NoCheck(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NoCheck(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}

{-------------------------------------------------------------------------------
    *DivCeilPow2NC - shortened sliases
-------------------------------------------------------------------------------}

Function iDivFloorPow2NC(Dividend,Divisor: Int8): Int8;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2NC(Dividend,Divisor: Int16): Int16;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2NC(Dividend,Divisor: Int32): Int32;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iDivFloorPow2NC(Dividend,Divisor: Int64): Int64;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

//------------------------------------------------------------------------------

Function uDivFloorPow2NC(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2NC(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2NC(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uDivFloorPow2NC(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

//------------------------------------------------------------------------------

Function DivFloorPow2NC(Dividend,Divisor: Int8): Int8;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: Int16): Int16;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: Int32): Int32;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: Int64): Int64;
begin
Result := iDivFloorPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: UInt8): UInt8;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: UInt16): UInt16;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: UInt32): UInt32;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DivFloorPow2NC(Dividend,Divisor: UInt64): UInt64;
begin
Result := uDivFloorPow2NoCheck(Dividend,Divisor);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of same types
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
If CompareUInt64(A,B) < 0 then
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

{-------------------------------------------------------------------------------
    Min - common-name overloads
-------------------------------------------------------------------------------}

Function Min(A,B: Int8): Int8;
begin
Result := iMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A,B: Int16): Int16;
begin
Result := iMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A,B: Int32): Int32;
begin
Result := iMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A,B: Int64): Int64;
begin
Result := iMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A,B: UInt8): UInt8;
begin
Result := uMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A,B: UInt16): UInt16;
begin
Result := uMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A,B: UInt32): UInt32;
begin
Result := uMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A,B: UInt64): UInt64;
begin
Result := uMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A,B: Extended): Extended;
begin
Result := fMin(A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Minimum of mixed types
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iiMin - signed integer & signed integer
-------------------------------------------------------------------------------}

Function iiMin(A: Int8; B: Int16): Int8;
begin
If B >= Int16(Low(Int8)) then
  begin
    If Int16(A) < B then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMin: Value of B (Int16: %d) is too low for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int8; B: Int32): Int8;
begin
If B >= Int32(Low(Int8)) then
  begin
    If Int32(A) < B then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMin: Value of B (Int32: %d) is too low for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int8; B: Int64): Int8;
begin
If B >= Int64(Low(Int8)) then
  begin
    If Int64(A) < B then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMin: Value of B (Int64: %d) is too low for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int16; B: Int8): Int16;
begin
If A < Int16(B) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int16; B: Int32): Int16;
begin
If B >= Int32(Low(Int16)) then
  begin
    If Int32(A) < B then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMin: Value of B (Int32: %d) is too low for Int16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int16; B: Int64): Int16;
begin
If B >= Int64(Low(Int16)) then
  begin
    If Int64(A) < B then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMin: Value of B (Int64: %d) is too low for Int16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int32; B: Int8): Int32;
begin
If A < Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int32; B: Int16): Int32;
begin
If A < Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int32; B: Int64): Int32;
begin
If B >= Int64(Low(Int32)) then
  begin
    If Int64(A) < B then
      Result := A
    else
      Result := Int32(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMin: Value of B (Int64: %d) is too low for Int32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int64; B: Int8): Int64;
begin
If A < Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int64; B: Int16): Int64;
begin
If A < Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMin(A: Int64; B: Int32): Int64;
begin
If A < Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

{-------------------------------------------------------------------------------
    iuMin - signed integer & unsigned integer
-------------------------------------------------------------------------------}

Function iuMin(A: Int8; B: UInt8): Int8;
begin
If (A < 0) or (B > UInt8(High(Int8))) or (UInt8(A) < B) then
  Result := A
else
  Result := Int8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int8; B: UInt16): Int8;
begin
If (A < 0) or (B > UInt16(High(Int8))) or (UInt16(A) < B) then
  Result := A
else
  Result := Int8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int8; B: UInt32): Int8;
begin
If (A < 0) or (B > UInt32(High(Int8))) or (UInt32(A) < B) then
  Result := A
else
  Result := Int8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int8; B: UInt64): Int8;
begin
If (A < 0) or (CompareUInt64(B,UInt64(High(Int8))) > 0) or (CompareUInt64(UInt64(UInt8(A)),B) < 0) then
  Result := A
else
  Result := Int8(B);
end;   

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int16; B: UInt8): Int16;
begin
// note - B can never be bigger than what result can accomodate
If A < Int16(B) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int16; B: UInt16): Int16;
begin
If (A < 0) or (B > UInt16(High(Int16))) or (UInt16(A) < B) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int16; B: UInt32): Int16;
begin
If (A < 0) or (B > UInt32(High(Int16))) or (UInt32(A) < B) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int16; B: UInt64): Int16;
begin
If (A < 0) or (CompareUInt64(B,UInt64(High(Int16))) > 0) or (CompareUInt64(UInt64(UInt16(A)),B) < 0) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int32; B: UInt8): Int32;
begin
If A < Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int32; B: UInt16): Int32;
begin
If A < Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int32; B: UInt32): Int32;
begin
If (A < 0) or (B > UInt32(High(Int32))) or (UInt32(A) < B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int32; B: UInt64): Int32;
begin
If (A < 0) or (CompareUInt64(B,UInt64(High(Int32))) > 0) or (CompareUInt64(UInt64(UInt32(A)),B) < 0) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int64; B: UInt8): Int64;
begin
If A < Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int64; B: UInt16): Int64;
begin
If A < Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int64; B: UInt32): Int64;
begin
If A < Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMin(A: Int64; B: UInt64): Int64;
begin
If (A < 0) or (CompareUInt64(B,UInt64(High(Int64))) > 0) or (CompareUInt64(UInt64(A),B) < 0) then
  Result := A
else
  Result := Int64(B);
end;

{-------------------------------------------------------------------------------
    uiMin - unsigned integer & signed integer
-------------------------------------------------------------------------------}

Function uiMin(A: UInt8; B: Int8): UInt8;
begin
If B >= 0 then
  begin
    If A < UInt8(B) then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int8: %d) is too low for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt8; B: Int16): UInt8;
begin
If B >= 0 then
  begin
    If (B > Int16(High(UInt8))) or (Int16(A) < B) then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int16: %d) is too low for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt8; B: Int32): UInt8;
begin
If B >= 0 then
  begin
    If (B > Int32(High(UInt8))) or (Int32(A) < B) then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int32: %d) is too low for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt8; B: Int64): UInt8;
begin
If B >= 0 then
  begin
    If (B > Int64(High(UInt8))) or (Int64(A) < B) then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int64: %d) is too low for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt16; B: Int8): UInt16;
begin
If B >= 0 then
  begin
    If A < UInt16(B) then
      Result := A
    else
      Result := UInt16(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int8: %d) is too low for UInt16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt16; B: Int16): UInt16;
begin
If B >= 0 then
  begin
    If A < UInt16(B) then
      Result := A
    else
      Result := UInt16(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int16: %d) is too low for UInt16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt16; B: Int32): UInt16;
begin
If B >= 0 then
  begin
    If (B > Int32(High(UInt16))) or (Int32(A) < B) then
      Result := A
    else
      Result := UInt16(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int32: %d) is too low for UInt16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt16; B: Int64): UInt16;
begin
If B >= 0 then
  begin
    If (B > Int64(High(UInt16))) or (Int64(A) < B) then
      Result := A
    else
      Result := UInt16(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int64: %d) is too low for UInt16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt32; B: Int8): UInt32;
begin
If B >= 0 then
  begin
    If A < UInt32(B) then
      Result := A
    else
      Result := UInt32(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int8: %d) is too low for UInt32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt32; B: Int16): UInt32;
begin
If B >= 0 then
  begin
    If A < UInt32(B) then
      Result := A
    else
      Result := UInt32(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int16: %d) is too low for UInt32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt32; B: Int32): UInt32;
begin
If B >= 0 then
  begin
    If A < UInt32(B) then
      Result := A
    else
      Result := UInt32(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int32: %d) is too low for UInt32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt32; B: Int64): UInt32;
begin
If B >= 0 then
  begin
    If (B > Int64(High(UInt32))) or (Int64(A) < B) then
      Result := A
    else
      Result := UInt32(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int64: %d) is too low for UInt32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt64; B: Int8): UInt64;
begin
If B >= 0 then
  begin
    If CompareUInt64(A,UInt64(UInt8(B))) < 0 then
      Result := A
    else
      Result := UInt64(UInt8(B));
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int8: %d) is too low for UInt64.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt64; B: Int16): UInt64;
begin
If B >= 0 then
  begin
    If CompareUInt64(A,UInt64(UInt16(B))) < 0 then
      Result := A
    else
      Result := UInt64(UInt16(B));
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int16: %d) is too low for UInt64.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt64; B: Int32): UInt64;
begin
If B >= 0 then
  begin
    If CompareUInt64(A,UInt64(UInt32(B))) < 0 then
      Result := A
    else
      Result := UInt64(UInt32(B));
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int32: %d) is too low for UInt64.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMin(A: UInt64; B: Int64): UInt64;
begin
If B >= 0 then
  begin
    If CompareUInt64(A,UInt64(B)) < 0 then
      Result := A
    else
      Result := UInt64(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMin: Value of B (Int32: %d) is too low for UInt64.',[B]);
end;

{-------------------------------------------------------------------------------
    uuMin - unsigned integer & unsigned integer
-------------------------------------------------------------------------------}

Function uuMin(A: UInt8; B: UInt16): UInt8;
begin
{
  No more comparisons and checks is needed here, if B is larger than 
  High(UInt8), then it cannot be returned as minimum.
}
If UInt16(A) < B then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt8; B: UInt32): UInt8;
begin
If UInt32(A) < B then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt8; B: UInt64): UInt8;
begin
If CompareUInt64(UInt64(A),B) < 0 then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt16; B: UInt8): UInt16;
begin
If A < UInt16(B) then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt16; B: UInt32): UInt16;
begin
If UInt32(A) < B then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt16; B: UInt64): UInt16;
begin
If CompareUInt64(UInt64(A),B) < 0 then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt32; B: UInt8): UInt32;
begin
If A < UInt32(B) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt32; B: UInt16): UInt32;
begin
If A < UInt32(B) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt32; B: UInt64): UInt32;
begin
If CompareUInt64(UInt64(A),B) < 0 then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt64; B: UInt8): UInt64;
begin
If CompareUInt64(A,UInt64(B)) < 0 then
  Result := A
else
  Result := UInt64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt64; B: UInt16): UInt64;
begin
If CompareUInt64(A,UInt64(B)) < 0 then
  Result := A
else
  Result := UInt64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMin(A: UInt64; B: UInt32): UInt64;
begin
If CompareUInt64(A,UInt64(B)) < 0 then
  Result := A
else
  Result := UInt64(B);
end;

{-------------------------------------------------------------------------------
    fiMin - float & signed integer
-------------------------------------------------------------------------------}

Function fiMin(A: Extended; B: Int8): Extended;
begin
{
  [U]Int8 through [U]Int32 can be converted to Extended without losing
  information, even when it is declared only as an alias for Double.
}
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiMin(A: Extended; B: Int16): Extended;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiMin(A: Extended; B: Int32): Extended;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiMin(A: Extended; B: Int64): Extended;
begin
{
  By checking whether the float is smaller than the integer can ever be, we
  can avoid potential exception in further code if B is out of limits for
  accurate conversion to float.
}
If A >= -TwoPow63 then
  begin
    If A < Int64ToFloat(B) then   // raises exception if B is out of limits
      Result := A
    else
      Result := B;  // if here, then B was already checked for limits
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    fuMin - float & unsigned integer
-------------------------------------------------------------------------------}

Function fuMin(A: Extended; B: UInt8): Extended;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuMin(A: Extended; B: UInt16): Extended;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuMin(A: Extended; B: UInt32): Extended;
begin
If A < B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuMin(A: Extended; B: UInt64): Extended;
begin
If A >= 0 then
  begin
    If A < UInt64ToFloat(B) then
      Result := A
    else
      Result := UInt64ToFloat(B);
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    ifMin - signed integer & float
-------------------------------------------------------------------------------}

Function ifMin(A: Int8; B: Extended): Int8;
begin
If B < A then
  begin
    If (B < Low(Int8)) or (B > High(Int8)) then
      raise EAMInvalidOperation.CreateFmt('ifMin: Value of B (Extended: %g) cannot fit into Int8.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ifMin: Value of B (Extended: %g) cannot be stored in Int8.',[B])
    else
      Result := Int8(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifMin(A: Int16; B: Extended): Int16;
begin
If B < A then
  begin
    If (B < Low(Int16)) or (B > High(Int16)) then
      raise EAMInvalidOperation.CreateFmt('ifMin: Value of B (Extended: %g) cannot fit into Int16.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ifMin: Value of B (Extended: %g) cannot be stored in Int16.',[B])
    else
      Result := Int16(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifMin(A: Int32; B: Extended): Int32;
begin
If B < A then
  begin
    If (B < Low(Int32)) or (B > High(Int32)) then
      raise EAMInvalidOperation.CreateFmt('ifMin: Value of B (Extended: %g) cannot fit into Int32.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ifMin: Value of B (Extended: %g) cannot be stored in Int32.',[B])
    else
      Result := Int32(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifMin(A: Int64; B: Extended): Int64;
begin
If B < TwoPow63 then
  begin
    If B < Int64ToFloat(A) then
      Result := FloatToInt64(B) // does checks for fraction and limits
    else
      Result := A;
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    ufMin - unsigned integer & float
-------------------------------------------------------------------------------}

Function ufMin(A: UInt8; B: Extended): UInt8;
begin
If B < A then
  begin
    If (B < 0) or (B > High(UInt8)) then
      raise EAMInvalidOperation.CreateFmt('ufMin: Value of B (Extended: %g) cannot fit into UInt8.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ufMin: Value of B (Extended: %g) cannot be stored in UInt8.',[B])
    else
      Result := UInt8(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufMin(A: UInt16; B: Extended): UInt16;
begin
If B < A then
  begin
    If (B < 0) or (B > High(UInt16)) then
      raise EAMInvalidOperation.CreateFmt('ufMin: Value of B (Extended: %g) cannot fit into UInt16.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ufMin: Value of B (Extended: %g) cannot be stored in UInt16.',[B])
    else
      Result := UInt16(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufMin(A: UInt32; B: Extended): UInt32;
begin
If B < A then
  begin
    If (B < 0) or (B > High(UInt32)) then
      raise EAMInvalidOperation.CreateFmt('ufMin: Value of B (Extended: %g) cannot fit into UInt32.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ufMin: Value of B (Extended: %g) cannot be stored in UInt32.',[B])
    else
      Result := UInt32(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufMin(A: UInt64; B: Extended): UInt64;
begin
If B < TwoPow64 then
  begin
    If B < UInt64ToFloat(A) then
      Result := FloatToUInt64(B) // performs all checks
    else
      Result := A;
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    Min - common-name overloads
-------------------------------------------------------------------------------}

Function Min(A: Int8; B: Int16): Int8;
begin
Result := iiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int8; B: Int32): Int8;
begin
Result := iiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int8; B: Int64): Int8;
begin
Result := iiMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: Int8): Int16;
begin
Result := iiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: Int32): Int16;
begin
Result := iiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: Int64): Int16;
begin
Result := iiMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: Int8): Int32;
begin
Result := iiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: Int16): Int32;
begin
Result := iiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: Int64): Int32;
begin
Result := iiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: Int8): Int64;
begin
Result := iiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: Int16): Int64;
begin
Result := iiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: Int32): Int64;
begin
Result := iiMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Int8; B: UInt8): Int8;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int8; B: UInt16): Int8;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int8; B: UInt32): Int8;
begin
Result := iuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int8; B: UInt64): Int8;
begin
Result := iuMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: UInt8): Int16;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: UInt16): Int16;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: UInt32): Int16;
begin
Result := iuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: UInt64): Int16;
begin
Result := iuMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: UInt8): Int32;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: UInt16): Int32;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: UInt32): Int32;
begin
Result := iuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: UInt64): Int32; 
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: UInt8): Int64;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: UInt16): Int64;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: UInt32): Int64;
begin
Result := iuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: UInt64): Int64;
begin
Result := iuMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: UInt8; B: Int8): UInt8;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt8; B: Int16): UInt8;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt8; B: Int32): UInt8;
begin
Result := uiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt8; B: Int64): UInt8;
begin
Result := uiMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: Int8): UInt16;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: Int16): UInt16;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: Int32): UInt16;
begin
Result := uiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: Int64): UInt16;
begin
Result := uiMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: Int8): UInt32;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: Int16): UInt32;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: Int32): UInt32;
begin
Result := uiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: Int64): UInt32;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: Int8): UInt64;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: Int16): UInt64;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: Int32): UInt64;
begin
Result := uiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: Int64): UInt64;
begin
Result := uiMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: UInt8; B: UInt16): UInt8;
begin
Result := uuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt8; B: UInt32): UInt8; 
begin
Result := uuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt8; B: UInt64): UInt8;  
begin
Result := uuMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: UInt8): UInt16;  
begin
Result := uuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: UInt32): UInt16; 
begin
Result := uuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: UInt64): UInt16; 
begin
Result := uuMin(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: UInt8): UInt32;
begin
Result := uuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: UInt16): UInt32;
begin
Result := uuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: UInt64): UInt32;
begin
Result := uuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: UInt8): UInt64; 
begin
Result := uuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: UInt16): UInt64;
begin
Result := uuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: UInt32): UInt64;
begin
Result := uuMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Extended; B: Int8): Extended;
begin
Result := fiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Extended; B: Int16): Extended;
begin
Result := fiMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Extended; B: Int32): Extended;
begin
Result := fiMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Extended; B: Int64): Extended;
begin
Result := fiMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Extended; B: UInt8): Extended;
begin
Result := fuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Extended; B: UInt16): Extended;
begin
Result := fuMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Extended; B: UInt32): Extended;
begin
Result := fuMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Extended; B: UInt64): Extended;
begin
Result := fuMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: Int8; B: Extended): Int8;
begin
Result := ifMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int16; B: Extended): Int16;
begin
Result := ifMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int32; B: Extended): Int32;
begin
Result := ifMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: Int64; B: Extended): Int64;
begin
Result := ifMin(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Min(A: UInt8; B: Extended): UInt8;
begin
Result := ufMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt16; B: Extended): UInt16;
begin
Result := ufMin(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt32; B: Extended): UInt32;
begin
Result := ufMin(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Min(A: UInt64; B: Extended): UInt64;
begin
Result := ufMin(A,B);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of same types
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
If CompareUInt64(A,B) > 0 then
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

{-------------------------------------------------------------------------------
    Max - common-name overloads
-------------------------------------------------------------------------------}

Function Max(A,B: Int8): Int8;
begin
Result := iMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A,B: Int16): Int16;
begin
Result := iMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A,B: Int32): Int32;
begin
Result := iMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A,B: Int64): Int64;
begin
Result := iMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A,B: UInt8): UInt8;
begin
Result := uMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A,B: UInt16): UInt16;
begin
Result := uMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A,B: UInt32): UInt32;
begin
Result := uMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A,B: UInt64): UInt64;
begin
Result := uMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A,B: Extended): Extended;
begin
Result := fMax(A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Maximum of mixed types
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iiMax - signed integer & signed integer
-------------------------------------------------------------------------------}

Function iiMax(A: Int8; B: Int16): Int8;
begin
If B <= Int16(High(Int8)) then
  begin
    If Int16(A) > B then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMax: Value of B (Int16: %d) is too high for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int8; B: Int32): Int8;
begin
If B <= Int32(High(Int8)) then
  begin
    If Int32(A) > B then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMax: Value of B (Int32: %d) is too high for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int8; B: Int64): Int8;
begin
If B <= Int64(High(Int8)) then
  begin
    If Int64(A) > B then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMax: Value of B (Int64: %d) is too high for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int16; B: Int8): Int16;
begin
If A > Int16(B) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int16; B: Int32): Int16;
begin
If B <= Int32(High(Int16)) then
  begin
    If Int32(A) > B then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMax: Value of B (Int32: %d) is too high for Int16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int16; B: Int64): Int16;
begin
If B <= Int64(High(Int16)) then
  begin
    If Int64(A) > B then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMax: Value of B (Int64: %d) is too high for Int16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int32; B: Int8): Int32;
begin
If A > Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int32; B: Int16): Int32;
begin
If A > Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int32; B: Int64): Int32;
begin
If B <= Int64(High(Int32)) then
  begin
    If Int64(A) < B then
      Result := A
    else
      Result := Int32(B);
  end
else raise EAMInvalidOperation.CreateFmt('iiMax: Value of B (Int64: %d) is too high for Int32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int64; B: Int8): Int64;
begin
If A > Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int64; B: Int16): Int64;
begin
If A > Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiMax(A: Int64; B: Int32): Int64;
begin
If A > Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

{-------------------------------------------------------------------------------
    iuMax - signed integer & unsigned integer
-------------------------------------------------------------------------------}

Function iuMax(A: Int8; B: UInt8): Int8;
begin
If B <= UInt8(High(Int8)) then
  begin
    If A > Int8(B) then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt8: %u) is too high for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int8; B: UInt16): Int8;
begin
If B <= UInt16(High(Int8)) then
  begin
    If A > Int8(B) then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt16: %u) is too high for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int8; B: UInt32): Int8;
begin
If B <= UInt32(High(Int8)) then
  begin
    If A > Int8(B) then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt32: %u) is too high for Int8.',[Int64(B)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int8; B: UInt64): Int8;
begin
If CompareUInt64(B,UInt64(High(Int8))) <= 0 then
  begin
    If A > Int8(B) then
      Result := A
    else
      Result := Int8(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt64: %u) is too high for Int8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int16; B: UInt8): Int16;
begin
If A > Int16(B) then
  Result := A
else
  Result := Int16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int16; B: UInt16): Int16;
begin
If B <= UInt16(High(Int16)) then
  begin
    If A > Int16(B) then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt16: %u) is too high for Int16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int16; B: UInt32): Int16;
begin
If B <= UInt32(High(Int16)) then
  begin
    If A > Int16(B) then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt32: %u) is too high for Int16.',[Int64(B)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int16; B: UInt64): Int16;
begin
If CompareUInt64(B,UInt32(High(Int16))) <= 0 then
  begin
    If A > Int16(B) then
      Result := A
    else
      Result := Int16(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt64: %u) is too high for Int16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int32; B: UInt8): Int32;
begin
If A > Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int32; B: UInt16): Int32;
begin
If A > Int32(B) then
  Result := A
else
  Result := Int32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int32; B: UInt32): Int32;
begin
If B <= UInt32(High(Int32)) then
  begin
    If A > Int32(B) then
      Result := A
    else
      Result := Int32(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt32: %u) is too high for Int32.',[Int64(B)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int32; B: UInt64): Int32;
begin
If CompareUInt64(B,UInt64(High(Int32))) <= 0 then
  begin
    If A > Int32(B) then
      Result := A
    else
      Result := Int32(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt64: %u) is too high for Int32.',[Int64(B)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int64; B: UInt8): Int64;
begin
If A > Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int64; B: UInt16): Int64;
begin
If A > Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int64; B: UInt32): Int64;
begin
If A > Int64(B) then
  Result := A
else
  Result := Int64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuMax(A: Int64; B: UInt64): Int64;
begin
If CompareUInt64(B,UInt64(High(Int64))) <= 0 then
  begin
    If A > Int64(B) then
      Result := A
    else
      Result := Int64(B);
  end
else raise EAMInvalidOperation.CreateFmt('iuMax: Value of B (UInt64: %u) is too high for Int64.',[B]);
end;

{-------------------------------------------------------------------------------
    uiMax - unsigned integer & signed integer
-------------------------------------------------------------------------------}

Function uiMax(A: UInt8; B: Int8): UInt8;
begin
If (B < 0) or (A > UInt8(B)) then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt8; B: Int16): UInt8;
begin
If B <= Int16(High(UInt8)) then
  begin
    If Int16(A) > B then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMax: Value of B (Int16: %d) is too high for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt8; B: Int32): UInt8;
begin
If B <= Int32(High(UInt8)) then
  begin
    If (B < 0) or (Int32(A) > B) then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMax: Value of B (Int32: %d) is too high for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt8; B: Int64): UInt8;
begin
If B <= Int64(High(UInt8)) then
  begin
    If (B < 0) or (Int64(A) > B) then
      Result := A
    else
      Result := UInt8(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMax: Value of B (Int64: %d) is too high for UInt8.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt16; B: Int8): UInt16;
begin
If (B < 0) or (A > UInt16(B)) then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt16; B: Int16): UInt16;
begin
If (B < 0) or (A > UInt16(B)) then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt16; B: Int32): UInt16;
begin
If B <= Int32(High(UInt16)) then
  begin
    If (B < 0) or (Int32(A) > B) then
      Result := A
    else
      Result := UInt16(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMax: Value of B (Int32: %d) is too high for UInt16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt16; B: Int64): UInt16;
begin
If B <= Int64(High(UInt16)) then
  begin
    If (B < 0) or (Int64(A) > B) then
      Result := A
    else
      Result := UInt16(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMax: Value of B (Int64: %d) is too high for UInt16.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt32; B: Int8): UInt32;
begin
If (B < 0) or (A > UInt32(B)) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt32; B: Int16): UInt32;
begin
If (B < 0) or (A > UInt32(B)) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt32; B: Int32): UInt32;
begin
If (B < 0) or (A > UInt32(B)) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt32; B: Int64): UInt32;
begin
If B <= Int64(High(UInt32)) then
  begin
    If (B < 0) or (Int64(A) > B) then
      Result := A
    else
      Result := UInt32(B);
  end
else raise EAMInvalidOperation.CreateFmt('uiMax: Value of B (Int64: %d) is too high for UInt32.',[B]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt64; B: Int8): UInt64;
begin
If (B < 0) or (CompareUInt64(A,UInt64(UInt8(B))) > 0) then
  Result := A
else
  Result := UInt64(UInt8(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt64; B: Int16): UInt64;
begin
If (B < 0) or (CompareUInt64(A,UInt64(UInt16(B))) > 0) then
  Result := A
else
  Result := UInt64(UInt16(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt64; B: Int32): UInt64;
begin
If (B < 0) or (CompareUInt64(A,UInt64(UInt32(B))) > 0) then
  Result := A
else
  Result := UInt64(UInt32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiMax(A: UInt64; B: Int64): UInt64;
begin
If (B < 0) or (CompareUInt64(A,UInt64(B)) > 0) then
  Result := A
else
  Result := UInt64(B);
end;

{-------------------------------------------------------------------------------
    uuMax - unsigned integer & unsigned integer
-------------------------------------------------------------------------------}

Function uuMax(A: UInt8; B: UInt16): UInt8;
begin
If UInt16(A) > B then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt8; B: UInt32): UInt8;
begin
If UInt32(A) > B then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt8; B: UInt64): UInt8;
begin
If CompareUInt64(UInt64(A),B) > 0 then
  Result := A
else
  Result := UInt8(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt16; B: UInt8): UInt16;
begin
If A > UInt16(B) then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt16; B: UInt32): UInt16;
begin
If UInt32(A) > B then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt16; B: UInt64): UInt16;
begin
If CompareUInt64(UInt64(A),B) > 0 then
  Result := A
else
  Result := UInt16(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt32; B: UInt8): UInt32;
begin
If A > UInt32(B) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt32; B: UInt16): UInt32;
begin
If A > UInt32(B) then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt32; B: UInt64): UInt32;
begin
If CompareUInt64(UInt64(A),B) > 0 then
  Result := A
else
  Result := UInt32(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt64; B: UInt8): UInt64;
begin
If CompareUInt64(A,UInt64(B)) > 0 then
  Result := A
else
  Result := UInt64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt64; B: UInt16): UInt64;
begin
If CompareUInt64(A,UInt64(B)) > 0 then
  Result := A
else
  Result := UInt64(B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuMax(A: UInt64; B: UInt32): UInt64;
begin
If CompareUInt64(A,UInt64(B)) > 0 then
  Result := A
else
  Result := UInt64(B);
end;

{-------------------------------------------------------------------------------
    fiMax - float & signed integer
-------------------------------------------------------------------------------}

Function fiMax(A: Extended; B: Int8): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiMax(A: Extended; B: Int16): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiMax(A: Extended; B: Int32): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiMax(A: Extended; B: Int64): Extended;
begin
If A < TwoPow63 then
  begin
    If A > Int64ToFloat(B) then
      Result := A
    else
      Result := B;
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    fuMax - float & unsigned integer
-------------------------------------------------------------------------------}

Function fuMax(A: Extended; B: UInt8): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuMax(A: Extended; B: UInt16): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuMax(A: Extended; B: UInt32): Extended;
begin
If A > B then
  Result := A
else
  Result := B;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuMax(A: Extended; B: UInt64): Extended;
begin
If A < TwoPow64 then
  begin
    If A > UInt64ToFloat(B) then
      Result := A
    else
      Result := UInt64ToFloat(B);
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    ifMax - signed integer & float
-------------------------------------------------------------------------------}

Function ifMax(A: Int8; B: Extended): Int8;
begin
If B > A then
  begin
    If (B < Low(Int8)) or (B > High(Int8)) then
      raise EAMInvalidOperation.CreateFmt('ifMax: Value of B (Extended: %g) cannot fit into Int8.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ifMax: Value of B (Extended: %g) cannot be stored in Int8.',[B])
    else
      Result := Int8(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifMax(A: Int16; B: Extended): Int16;
begin
If B > A then
  begin
    If (B < Low(Int16)) or (B > High(Int16)) then
      raise EAMInvalidOperation.CreateFmt('ifMax: Value of B (Extended: %g) cannot fit into Int16.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ifMax: Value of B (Extended: %g) cannot be stored in Int16.',[B])
    else
      Result := Int16(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifMax(A: Int32; B: Extended): Int32;
begin
If B > A then
  begin
    If (B < Low(Int32)) or (B > High(Int32)) then
      raise EAMInvalidOperation.CreateFmt('ifMax: Value of B (Extended: %g) cannot fit into Int32.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ifMax: Value of B (Extended: %g) cannot be stored in Int32.',[B])
    else
      Result := Int32(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifMax(A: Int64; B: Extended): Int64;
begin
If B >= -TwoPow63 then
  begin
    If B > Int64ToFloat(A) then
      Result := FloatToInt64(B)
    else
      Result := A;
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    ufMax - unsigned integer & float
-------------------------------------------------------------------------------}

Function ufMax(A: UInt8; B: Extended): UInt8;
begin
If B > A then
  begin
    If B > High(UInt8) then
      raise EAMInvalidOperation.CreateFmt('ufMax: Value of B (Extended: %g) cannot fit into UInt8.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ufMax: Value of B (Extended: %g) cannot be stored in UInt8.',[B])
    else
      Result := UInt8(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufMax(A: UInt16; B: Extended): UInt16;
begin
If B > A then
  begin
    If B > High(UInt16) then
      raise EAMInvalidOperation.CreateFmt('ufMax: Value of B (Extended: %g) cannot fit into UInt16.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ufMax: Value of B (Extended: %g) cannot be stored in UInt16.',[B])
    else
      Result := UInt16(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufMax(A: UInt32; B: Extended): UInt32;
begin
If B > A then
  begin
    If B > High(UInt32) then
      raise EAMInvalidOperation.CreateFmt('ufMax: Value of B (Extended: %g) cannot fit into UInt32.',[B])
    else If Frac(B) <> 0 then
      raise EAMInvalidOperation.CreateFmt('ufMax: Value of B (Extended: %g) cannot be stored in UInt32.',[B])
    else
      Result := UInt32(Trunc(B));
  end
else Result := A;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufMax(A: UInt64; B: Extended): UInt64;
begin
If B >= 0 then
  begin
    If B > UInt64ToFloat(A) then
      Result := FloatToUInt64(B)
    else
      Result := A;
  end
else Result := A;
end;

{-------------------------------------------------------------------------------
    Max - common-name overloads
-------------------------------------------------------------------------------}

Function Max(A: Int8; B: Int16): Int8;
begin
Result := iiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int8; B: Int32): Int8;
begin
Result := iiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int8; B: Int64): Int8;
begin
Result := iiMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: Int8): Int16;
begin
Result := iiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: Int32): Int16;
begin
Result := iiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: Int64): Int16;
begin
Result := iiMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: Int8): Int32;
begin
Result := iiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: Int16): Int32;
begin
Result := iiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: Int64): Int32;
begin
Result := iiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: Int8): Int64;
begin
Result := iiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: Int16): Int64;
begin
Result := iiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: Int32): Int64;
begin
Result := iiMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Int8; B: UInt8): Int8;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int8; B: UInt16): Int8;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int8; B: UInt32): Int8;
begin
Result := iuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int8; B: UInt64): Int8;
begin
Result := iuMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: UInt8): Int16;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: UInt16): Int16;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: UInt32): Int16;
begin
Result := iuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: UInt64): Int16;
begin
Result := iuMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: UInt8): Int32;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: UInt16): Int32;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: UInt32): Int32;
begin
Result := iuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: UInt64): Int32; 
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: UInt8): Int64;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: UInt16): Int64;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: UInt32): Int64;
begin
Result := iuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: UInt64): Int64;
begin
Result := iuMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: UInt8; B: Int8): UInt8;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt8; B: Int16): UInt8;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt8; B: Int32): UInt8;
begin
Result := uiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt8; B: Int64): UInt8;
begin
Result := uiMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: Int8): UInt16;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: Int16): UInt16;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: Int32): UInt16;
begin
Result := uiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: Int64): UInt16;
begin
Result := uiMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: Int8): UInt32;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: Int16): UInt32;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: Int32): UInt32;
begin
Result := uiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: Int64): UInt32;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: Int8): UInt64;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: Int16): UInt64;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: Int32): UInt64;
begin
Result := uiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: Int64): UInt64;
begin
Result := uiMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: UInt8; B: UInt16): UInt8;
begin
Result := uuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt8; B: UInt32): UInt8; 
begin
Result := uuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt8; B: UInt64): UInt8;  
begin
Result := uuMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: UInt8): UInt16;  
begin
Result := uuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: UInt32): UInt16; 
begin
Result := uuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: UInt64): UInt16; 
begin
Result := uuMax(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: UInt8): UInt32;
begin
Result := uuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: UInt16): UInt32;
begin
Result := uuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: UInt64): UInt32;
begin
Result := uuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: UInt8): UInt64; 
begin
Result := uuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: UInt16): UInt64;
begin
Result := uuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: UInt32): UInt64;
begin
Result := uuMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Extended; B: Int8): Extended;
begin
Result := fiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Extended; B: Int16): Extended;
begin
Result := fiMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Extended; B: Int32): Extended;
begin
Result := fiMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Extended; B: Int64): Extended;
begin
Result := fiMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Extended; B: UInt8): Extended;
begin
Result := fuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Extended; B: UInt16): Extended;
begin
Result := fuMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Extended; B: UInt32): Extended;
begin
Result := fuMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Extended; B: UInt64): Extended;
begin
Result := fuMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: Int8; B: Extended): Int8;
begin
Result := ifMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int16; B: Extended): Int16;
begin
Result := ifMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int32; B: Extended): Int32;
begin
Result := ifMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: Int64; B: Extended): Int64;
begin
Result := ifMax(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function Max(A: UInt8; B: Extended): UInt8;
begin
Result := ufMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt16; B: Extended): UInt16;
begin
Result := ufMax(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt32; B: Extended): UInt32;
begin
Result := ufMax(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Max(A: UInt64; B: Extended): UInt64;
begin
Result := ufMax(A,B);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Smallest value in array
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iMinValue - signed integers
-------------------------------------------------------------------------------}

Function iMinValue(const Values: array of Int8; out Minimum: Int8): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Int8;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMinValue(const Values: array of Int16; out Minimum: Int16): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Int16;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMinValue(const Values: array of Int32; out Minimum: Int32): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Int32;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMinValue(const Values: array of Int64; out Minimum: Int64): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Int64;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

{-------------------------------------------------------------------------------
    uMinValue - unsigned integers
-------------------------------------------------------------------------------}

Function uMinValue(const Values: array of UInt8; out Minimum: UInt8): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: UInt8;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMinValue(const Values: array of UInt16; out Minimum: UInt16): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: UInt16;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMinValue(const Values: array of UInt32; out Minimum: UInt32): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: UInt32;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMinValue(const Values: array of UInt64; out Minimum: UInt64): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: UInt64;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If CompareUInt64(Values[i],MinVal) < 0 then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

{-------------------------------------------------------------------------------
    fMinValue - real numbers
-------------------------------------------------------------------------------}

Function fMinValue(const Values: array of Single; out Minimum: Single): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Single;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMinValue(const Values: array of Double; out Minimum: Double): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Double;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;

{$IF SizeOf(Extended) = 10}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMinValue(const Values: array of Extended; out Minimum: Extended): Integer;
var
  i:      Integer;
  MinIdx: Integer;
  MinVal: Extended;
begin
If Length(Values) > 0 then
  begin
    MinIdx := Low(Values);
    MinVal := Values[MinIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] < MinVal then
        begin
          MinIdx := i;
          MinVal := Values[i];
        end;
    Minimum := MinVal;
    Result := MinIdx;
  end
else Result := -1;
end;
{$IFEND}

{-------------------------------------------------------------------------------
    MinValue - common-name overloads
-------------------------------------------------------------------------------}

Function MinValue(const Values: array of Int8; out Minimum: Int8): Integer;
begin
Result := iMinValue(Values,Minimum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of Int16; out Minimum: Int16): Integer;
begin
Result := iMinValue(Values,Minimum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of Int32; out Minimum: Int32): Integer;
begin
Result := iMinValue(Values,Minimum);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of Int64; out Minimum: Int64): Integer;
begin
Result := iMinValue(Values,Minimum);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of UInt8; out Minimum: UInt8): Integer;
begin
Result := uMinValue(Values,Minimum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of UInt16; out Minimum: UInt16): Integer;
begin
Result := uMinValue(Values,Minimum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of UInt32; out Minimum: UInt32): Integer;
begin
Result := uMinValue(Values,Minimum);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of UInt64; out Minimum: UInt64): Integer;
begin
Result := uMinValue(Values,Minimum);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of Single; out Minimum: Single): Integer;
begin
Result := fMinValue(Values,Minimum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of Double; out Minimum: Double): Integer;
begin
Result := fMinValue(Values,Minimum);
end;

{$IF SizeOf(Extended) = 10}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MinValue(const Values: array of Extended; out Minimum: Extended): Integer;
begin
Result := fMinValue(Values,Minimum);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                             Largest value in array
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iMaxValue - signed integers
-------------------------------------------------------------------------------}

Function iMaxValue(const Values: array of Int8; out Maximum: Int8): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Int8;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMaxValue(const Values: array of Int16; out Maximum: Int16): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Int16;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMaxValue(const Values: array of Int32; out Maximum: Int32): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Int32;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iMaxValue(const Values: array of Int64; out Maximum: Int64): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Int64;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

{-------------------------------------------------------------------------------
    uMaxValue - unsigned integers
-------------------------------------------------------------------------------}

Function uMaxValue(const Values: array of UInt8; out Maximum: UInt8): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: UInt8;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMaxValue(const Values: array of UInt16; out Maximum: UInt16): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: UInt16;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMaxValue(const Values: array of UInt32; out Maximum: UInt32): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: UInt32;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uMaxValue(const Values: array of UInt64; out Maximum: UInt64): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: UInt64;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If CompareUInt64(Values[i],MaxVal) > 0 then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

{-------------------------------------------------------------------------------
    fMaxValue - real numbers
-------------------------------------------------------------------------------}

Function fMaxValue(const Values: array of Single; out Maximum: Single): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Single;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMaxValue(const Values: array of Double; out Maximum: Double): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Double;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;

{$IF SizeOf(Extended) = 10}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fMaxValue(const Values: array of Extended; out Maximum: Extended): Integer;
var
  i:      Integer;
  MaxIdx: Integer;
  MaxVal: Extended;
begin
If Length(Values) > 0 then
  begin
    MaxIdx := Low(Values);
    MaxVal := Values[MaxIdx];
    For i := Succ(Low(Values)) to High(Values) do
      If Values[i] > MaxVal then
        begin
          MaxIdx := i;
          MaxVal := Values[i];
        end;
    Maximum := MaxVal;
    Result := MaxIdx;
  end
else Result := -1;
end;
{$IFEND}

{-------------------------------------------------------------------------------
    MaxValue - common-name overloads
-------------------------------------------------------------------------------}

Function MaxValue(const Values: array of Int8; out Maximum: Int8): Integer;
begin
Result := iMaxValue(Values,Maximum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of Int16; out Maximum: Int16): Integer;
begin
Result := iMaxValue(Values,Maximum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of Int32; out Maximum: Int32): Integer;
begin
Result := iMaxValue(Values,Maximum);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of Int64; out Maximum: Int64): Integer;
begin
Result := iMaxValue(Values,Maximum);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of UInt8; out Maximum: UInt8): Integer;
begin
Result := uMaxValue(Values,Maximum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of UInt16; out Maximum: UInt16): Integer;
begin
Result := uMaxValue(Values,Maximum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of UInt32; out Maximum: UInt32): Integer;
begin
Result := uMaxValue(Values,Maximum);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of UInt64; out Maximum: UInt64): Integer;
begin
Result := uMaxValue(Values,Maximum);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of Single; out Maximum: Single): Integer;
begin
Result := fMaxValue(Values,Maximum);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of Double; out Maximum: Double): Integer;
begin
Result := fMaxValue(Values,Maximum);
end;

{$IF SizeOf(Extended) = 10}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MaxValue(const Values: array of Extended; out Maximum: Extended): Integer;
begin
Result := fMaxValue(Values,Maximum);
end;
{$IFEND}


{===============================================================================
--------------------------------------------------------------------------------
                          Compare values of equal types
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iCompareValue - signed integers
-------------------------------------------------------------------------------}

Function iCompareValue(A,B: Int8): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iCompareValue(A,B: Int16): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iCompareValue(A,B: Int32): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iCompareValue(A,B: Int64): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

{-------------------------------------------------------------------------------
    uCompareValue - unsigned integers
-------------------------------------------------------------------------------}

Function uCompareValue(A,B: UInt8): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uCompareValue(A,B: UInt16): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uCompareValue(A,B: UInt32): Integer;
begin
If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uCompareValue(A,B: UInt64): Integer;
begin
Result := CompareUInt64(A,B);
end;

{-------------------------------------------------------------------------------
    fCompareValue - real numbers
-------------------------------------------------------------------------------}

Function fCompareValue(A,B: Extended; Epsilon: Extended = 0.0): Integer;
begin
If Epsilon <> 0.0 then
  begin
    If Abs(A - B) <= Abs(Epsilon) then
      Result := 0
    else If A < B then
      Result := -1
    else
      Result := +1;
  end
else If A > B then
  Result := +1
else If A < B  then
  Result := -1
else
  Result := 0;
end;

{-------------------------------------------------------------------------------
    CompareValue - common-name overloads
-------------------------------------------------------------------------------}

Function CompareValue(A,B: Int8): Integer;
begin
Result := iCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: Int16): Integer;
begin
Result := iCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: Int32): Integer;
begin
Result := iCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: Int64): Integer;
begin
Result := iCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: UInt8): Integer;
begin
Result := uCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: UInt16): Integer;
begin
Result := uCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: UInt32): Integer;
begin
Result := uCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: UInt64): Integer;
begin
Result := uCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A,B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;


{===============================================================================
--------------------------------------------------------------------------------
                        Compare values of differing types
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    iiCompareValue - signed integer & signed integer
-------------------------------------------------------------------------------}

Function iiCompareValue(A: Int8; B: Int16): Integer;
begin
Result := iCompareValue(Int16(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int8; B: Int32): Integer;
begin
Result := iCompareValue(Int32(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int8; B: Int64): Integer;
begin
Result := iCompareValue(Int64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int16; B: Int8): Integer;
begin
Result := iCompareValue(A,Int16(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int16; B: Int32): Integer;
begin
Result := iCompareValue(Int32(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int16; B: Int64): Integer;
begin
Result := iCompareValue(Int64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int32; B: Int8): Integer;
begin
Result := iCompareValue(A,Int32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int32; B: Int16): Integer;
begin
Result := iCompareValue(A,Int32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int32; B: Int64): Integer;
begin
Result := iCompareValue(Int64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int64; B: Int8): Integer;
begin
Result := iCompareValue(A,Int64(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int64; B: Int16): Integer;
begin
Result := iCompareValue(A,Int64(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iiCompareValue(A: Int64; B: Int32): Integer;
begin
Result := iCompareValue(A,Int64(B));
end;

{-------------------------------------------------------------------------------
    iuCompareValue - signed integer & unsigned integer
-------------------------------------------------------------------------------}

Function iuCompareValue(A: Int8; B: UInt8): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt8(A),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int8; B: UInt16): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt16(A),B)
else
  Result := -1;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int8; B: UInt32): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt32(A),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int8; B: UInt64): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt64(UInt8(A)),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int16; B: UInt8): Integer;
begin
Result := iCompareValue(A,Int16(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int16; B: UInt16): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt16(A),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int16; B: UInt32): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt32(A),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int16; B: UInt64): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt64(UInt16(A)),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int32; B: UInt8): Integer;
begin
Result := iCompareValue(A,Int32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int32; B: UInt16): Integer;
begin
Result := iCompareValue(A,Int32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int32; B: UInt32): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt32(A),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int32; B: UInt64): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt64(UInt32(A)),B)
else
  Result := -1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int64; B: UInt8): Integer;
begin
Result := iCompareValue(A,Int64(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int64; B: UInt16): Integer;
begin
Result := iCompareValue(A,Int64(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int64; B: UInt32): Integer;
begin
Result := iCompareValue(A,Int64(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function iuCompareValue(A: Int64; B: UInt64): Integer;
begin
If A >= 0 then
  Result := uCompareValue(UInt64(A),B)
else
  Result := -1;
end;

{-------------------------------------------------------------------------------
    uiCompareValue - unsigned integer & signed integer
-------------------------------------------------------------------------------}

Function uiCompareValue(A: UInt8; B: Int8): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt8(B))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt8; B: Int16): Integer;
begin
Result := iCompareValue(Int16(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt8; B: Int32): Integer;
begin
Result := iCompareValue(Int32(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt8; B: Int64): Integer;
begin
Result := iCompareValue(Int64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt16; B: Int8): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt16(B))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt16; B: Int16): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt16(B))
else
  Result := +1;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt16; B: Int32): Integer;
begin
Result := iCompareValue(Int32(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt16; B: Int64): Integer;
begin
Result := iCompareValue(Int64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt32; B: Int8): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt32(B))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt32; B: Int16): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt32(B))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt32; B: Int32): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt32(B))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt32; B: Int64): Integer;
begin
Result := iCompareValue(Int64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt64; B: Int8): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt64(UInt8(B)))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt64; B: Int16): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt64(UInt16(B)))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt64; B: Int32): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt64(UInt32(B)))
else
  Result := +1;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uiCompareValue(A: UInt64; B: Int64): Integer;
begin
If B >= 0 then
  Result := uCompareValue(A,UInt64(B))
else
  Result := +1;
end;

{-------------------------------------------------------------------------------
    uuCompareValue - unsigned integer & unsigned integer
-------------------------------------------------------------------------------}

Function uuCompareValue(A: UInt8; B: UInt16): Integer;
begin
Result := uCompareValue(UInt16(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt8; B: UInt32): Integer;
begin
Result := uCompareValue(UInt32(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt8; B: UInt64): Integer;
begin
Result := uCompareValue(UInt64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt16; B: UInt8): Integer;
begin
Result := uCompareValue(A,UInt16(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt16; B: UInt32): Integer;
begin
Result := uCompareValue(UInt32(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt16; B: UInt64): Integer;
begin
Result := uCompareValue(UInt64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt32; B: UInt8): Integer;
begin
Result := uCompareValue(A,UInt32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt32; B: UInt16): Integer;
begin
Result := uCompareValue(A,UInt32(B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt32; B: UInt64): Integer;
begin
Result := uCompareValue(UInt64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt64; B: UInt8): Integer;
begin
Result := uCompareValue(UInt64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt64; B: UInt16): Integer;
begin
Result := uCompareValue(UInt64(A),B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function uuCompareValue(A: UInt64; B: UInt32): Integer;
begin
Result := uCompareValue(UInt64(A),B);
end;

{-------------------------------------------------------------------------------
    fiCompareValue - real number & signed integer
-------------------------------------------------------------------------------}

Function fiCompareValue(A: Extended; B: Int8; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiCompareValue(A: Extended; B: Int16; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiCompareValue(A: Extended; B: Int32; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fiCompareValue(A: Extended; B: Int64; Epsilon: Extended = 0.0): Integer;
begin
{
  Compare float with values the integer cannot ever have to avoid possible
  exceptions raised in Int64ToFloat...
}
If Epsilon <> 0.0 then
  begin
    Epsilon := Abs(Epsilon);
    If A >= (TwoPow63 + Epsilon) then
      Result := +1
    else If A < (-TwoPow63 - Epsilon) then
      Result := -1
    else
      Result := fCompareValue(A,Int64ToFloat(B),Epsilon);
  end
else
  begin
    If A >= TwoPow63 then
      Result := +1
    else If A < -TwoPow63 then
      Result := -1
    else
      Result := fCompareValue(A,Int64ToFloat(B));
  end;
end;

{-------------------------------------------------------------------------------
    fuCompareValue - real number & signed integer
-------------------------------------------------------------------------------}

Function fuCompareValue(A: Extended; B: UInt8; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuCompareValue(A: Extended; B: UInt16; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuCompareValue(A: Extended; B: UInt32; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function fuCompareValue(A: Extended; B: UInt64; Epsilon: Extended = 0.0): Integer;
begin
If Epsilon <> 0.0 then
  begin
    Epsilon := Abs(Epsilon);
    If A >= (TwoPow64 + Epsilon) then
      Result := +1
    else If A < -Epsilon then
      Result := -1
    else
      Result := fCompareValue(A,UInt64ToFloat(B),Epsilon);
  end
else
  begin
    If A >= TwoPow64 then
      Result := +1
    else If A < 0 then
      Result := -1
    else
      Result := fCompareValue(A,UInt64ToFloat(B));
  end;
end;

{-------------------------------------------------------------------------------
    ifCompareValue - signed integer & real number
-------------------------------------------------------------------------------}

Function ifCompareValue(A: Int8; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifCompareValue(A: Int16; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifCompareValue(A: Int32; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ifCompareValue(A: Int64; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
If Epsilon <> 0.0 then
  begin
    Epsilon := Abs(Epsilon);
    If B >= (TwoPow63 + Epsilon) then
      Result := -1
    else If B < (-TwoPow63 - Epsilon) then
      Result := +1
    else
      Result := fCompareValue(Int64ToFloat(A),B,Epsilon);
  end
else
  begin
    If B >= TwoPow63 then
      Result := -1
    else If B < -TwoPow63 then
      Result := +1
    else
      Result := fCompareValue(Int64ToFloat(A),B);
  end;
end;

{-------------------------------------------------------------------------------
    ufCompareValue - unsigned integer & real number
-------------------------------------------------------------------------------}

Function ufCompareValue(A: UInt8; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufCompareValue(A: UInt16; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufCompareValue(A: UInt32; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := fCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ufCompareValue(A: UInt64; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
If Epsilon <> 0.0 then
  begin
    Epsilon := Abs(Epsilon);
    If B >= (TwoPow64 + Epsilon) then
      Result := -1
    else If B < -Epsilon then
      Result := +1
    else
      Result := fCompareValue(UInt64ToFloat(A),B,Epsilon);
  end
else
  begin
    If B >= TwoPow64 then
      Result := -1
    else If B < 0 then
      Result := +1
    else
      Result := fCompareValue(UInt64ToFloat(A),B);
  end;
end;

{-------------------------------------------------------------------------------
    CompareValue - common-name overloads
-------------------------------------------------------------------------------}

Function CompareValue(A: Int8; B: Int16): Integer;
begin
Result := iiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int8; B: Int32): Integer;
begin
Result := iiCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int8; B: Int64): Integer;
begin
Result := iiCompareValue(A,B);
end;

{$IFEND}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: Int8): Integer;
begin
Result := iiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: Int32): Integer;
begin
Result := iiCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: Int64): Integer;
begin
Result := iiCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: Int8): Integer;
begin
Result := iiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: Int16): Integer;
begin
Result := iiCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: Int64): Integer;
begin
Result := iiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: Int8): Integer;
begin
Result := iiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: Int16): Integer;
begin
Result := iiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: Int32): Integer;
begin
Result := iiCompareValue(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Int8; B: UInt8): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int8; B: UInt16): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int8; B: UInt32): Integer;
begin
Result := iuCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int8; B: UInt64): Integer;
begin
Result := iuCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: UInt8): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: UInt16): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: UInt32): Integer;
begin
Result := iuCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: UInt64): Integer;
begin
Result := iuCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: UInt8): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: UInt16): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: UInt32): Integer;
begin
Result := iuCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: UInt64): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: UInt8): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: UInt16): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: UInt32): Integer;
begin
Result := iuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: UInt64): Integer;
begin
Result := iuCompareValue(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: UInt8; B: Int8): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt8; B: Int16): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt8; B: Int32): Integer;
begin
Result := uiCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt8; B: Int64): Integer;
begin
Result := uiCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: Int8): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: Int16): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: Int32): Integer;
begin
Result := uiCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: Int64): Integer;
begin
Result := uiCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: Int8): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: Int16): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: Int32): Integer;
begin
Result := uiCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: Int64): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: Int8): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: Int16): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: Int32): Integer;
begin
Result := uiCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: Int64): Integer;
begin
Result := uiCompareValue(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: UInt8; B: UInt16): Integer;
begin
Result := uuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt8; B: UInt32): Integer;
begin
Result := uuCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt8; B: UInt64): Integer;
begin
Result := uuCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: UInt8): Integer;
begin
Result := uuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: UInt32): Integer;
begin
Result := uuCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: UInt64): Integer;
begin
Result := uuCompareValue(A,B);
end;
{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: UInt8): Integer;
begin
Result := uuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: UInt16): Integer;
begin
Result := uuCompareValue(A,B);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: UInt64): Integer;
begin
Result := uuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: UInt8): Integer;
begin
Result := uuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: UInt16): Integer;
begin
Result := uuCompareValue(A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: UInt32): Integer;
begin
Result := uuCompareValue(A,B);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Extended; B: Int8; Epsilon: Extended = 0.0): Integer;
begin
Result := fiCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Extended; B: Int16; Epsilon: Extended = 0.0): Integer;
begin
Result := fiCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Extended; B: Int32; Epsilon: Extended = 0.0): Integer;
begin
Result := fiCompareValue(A,B,Epsilon);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Extended; B: Int64; Epsilon: Extended = 0.0): Integer;
begin
Result := fiCompareValue(A,B,Epsilon);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Extended; B: UInt8; Epsilon: Extended = 0.0): Integer;
begin
Result := fuCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Extended; B: UInt16; Epsilon: Extended = 0.0): Integer;
begin
Result := fuCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Extended; B: UInt32; Epsilon: Extended = 0.0): Integer;
begin
Result := fuCompareValue(A,B,Epsilon);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Extended; B: UInt64; Epsilon: Extended = 0.0): Integer;
begin
Result := fuCompareValue(A,B,Epsilon);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: Int8; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ifCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int16; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ifCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int32; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ifCompareValue(A,B,Epsilon);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: Int64; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ifCompareValue(A,B,Epsilon);
end;
{$IFEND}

//------------------------------------------------------------------------------

Function CompareValue(A: UInt8; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ufCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt16; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ufCompareValue(A,B,Epsilon);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt32; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ufCompareValue(A,B,Epsilon);
end;

{$IF Declared(DistinctOverloadUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(A: UInt64; B: Extended; Epsilon: Extended = 0.0): Integer;
begin
Result := ufCompareValue(A,B,Epsilon);
end;
{$IFEND}

end.





