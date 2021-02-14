-module(cerlc).

%%
%% Generate custom configured, Cyclic Redundancy Check (CRC) calculation functions
%%

-export([init/1, calc_crc/2]).

-record(cerlc, {
  bits :: 8 | 16 | 32 | 64, 
  reducer :: function(), 
  init_value :: non_neg_integer(), 
  final_xor_value :: non_neg_integer()
}).

-spec init(atom() | tuple()) -> crc_defn.
init(CrcDef) when is_atom(CrcDef) ->
  init(name_to_config(CrcDef));

% Generate 8-bit CRC function.
% Function is same for normal and reflected cases
init({8, Polynomial, InitValue, FinalXorValue, Reflected}) ->
  Table = gen_table(8, Polynomial, Reflected),

  #cerlc{
    bits = 8,
    init_value = InitValue,
    final_xor_value = FinalXorValue,
    reducer = fun (CurByte, CurCrc) ->
      % Table tuple is indexed from 1, not 0
      Index = (CurCrc bxor CurByte) + 1,
      element(Index, Table)
    end
  };

% Generate 16, 32, or 64-bit CRC function for un-reflected case
init({Bits, Polynomial, InitValue, FinalXorValue, false}) ->
  Table = gen_table(Bits, Polynomial, false),
  Shift = bits_to_shift(Bits),
  Mask = bits_to_mask(Bits),

   #cerlc{
    bits = Bits,
    init_value = InitValue,
    final_xor_value = FinalXorValue,
    reducer = fun (CurByte, CurCrc) ->
      % Table tuple is indexed from 1, not 0
      Index = (((CurCrc bsr Shift) bxor CurByte) band 16#FF) + 1,
      ((CurCrc bsl 8) bxor element(Index, Table)) band Mask
    end
  };

% Generate 16, 32, or 64-bit CRC functions for reflected case
init({Bits, Polynomial, InitValue, FinalXorValue, true}) ->
  Table = gen_table(Bits, Polynomial, true),
  Mask = bits_to_mask(Bits),
  
  #cerlc{
    bits = Bits,
    init_value = InitValue,
    final_xor_value = FinalXorValue,
    reducer = fun (CurByte, CurCrc) ->
      % Table tuple is indexed from 1, not 0
      Index = ((CurCrc bxor CurByte) band 16#FF) + 1,
      ((CurCrc bsr 8) bxor element(Index, Table)) band Mask
    end
  }.

%%
%% Calculate the CRC of a binary or list of data bytes
%%
-spec calc_crc(binary() | list(), #cerlc{}) -> non_neg_integer().
calc_crc(Data, Info) when is_binary(Data) ->
    calc_crc(binary_to_list(Data), Info);

calc_crc(Data, Info) when is_list(Data) ->
    lists:foldl(Info#cerlc.reducer, Info#cerlc.init_value, Data) bxor Info#cerlc.final_xor_value.

% Generate CRC look-up table for un-reflected and reflected cases
gen_table(Bits, Polynomial, false) -> 
  HiBit = bits_to_hibit(Bits),
  Mask = bits_to_mask(Bits),
  Shift = bits_to_shift(Bits),
  gen_table(HiBit, Mask, Shift, Polynomial, [], 0);

gen_table(Bits, Polynomial, true) -> 
  PolynomialR = reflect(Bits, Polynomial),
  Mask = bits_to_mask(Bits),
  gen_table_r(Mask, PolynomialR, [], 0).

% Generate un-reflected CRC lookup table
gen_table(_HiBit, _Mask, _Shift, _Polynomial, Table, 256) -> 
  list_to_tuple(lists:reverse(Table));

gen_table(HiBit, Mask, Shift, Polynomial, Table, Divident) ->
  CurrByte = curr_byte(HiBit, Polynomial, (Divident bsl Shift), 0),
  gen_table(HiBit, Mask, Shift, Polynomial, [(CurrByte band Mask) | Table], Divident + 1).

curr_byte(_HiBit, _Polynomial, CurrByte, 8) -> CurrByte;

curr_byte(HiBit, Polynomial, CurrByte, Index) ->
  NextByte = case (CurrByte band HiBit) of
    0 -> CurrByte bsl 1;
    _ -> (CurrByte bsl 1) bxor Polynomial
  end,
  curr_byte(HiBit, Polynomial, NextByte, Index + 1).

% Generate reflected CRC lookup table
gen_table_r(_Mask, _Polynomial, Table, 256) -> 
  list_to_tuple(lists:reverse(Table));

gen_table_r(Mask, Polynomial, Table, Divident) ->
  CurrByte = curr_byte_r(Polynomial, Divident, 0),
  gen_table_r(Mask, Polynomial, [(CurrByte band Mask) | Table], Divident + 1).

curr_byte_r(_Polynomial, CurrByte, 8) -> CurrByte;

curr_byte_r(Polynomial, CurrByte, Index) ->
  NextByte = case (CurrByte band 1) of
    0 -> CurrByte bsr 1;
    1 -> (CurrByte bsr 1) bxor Polynomial
  end,
  curr_byte_r(Polynomial, NextByte, Index + 1).

% Convert CRC name to config parameters
name_to_config(crc8) -> {8, 16#07, 16#00, 16#00, false};
name_to_config(crc8_sae_j1850) -> {8, 16#1D, 16#FF, 16#FF, false};
name_to_config(crc8_sae_j1850_zero) -> {8, 16#1D, 16#00, 16#00, false};
name_to_config(crc8_8h2f) -> {8, 16#2F, 16#FF, 16#FF, false};
name_to_config(crc8_cdma2000) -> {8, 16#9B, 16#FF, 16#00, false};
name_to_config(crc8_darc) -> {8, 16#39, 16#00, 16#00, true};
name_to_config(crc8_dvb_s2) -> {8, 16#D5, 16#00, 16#00, false};
name_to_config(crc8_ebu) -> {8, 16#1D, 16#FF, 16#00, true};
name_to_config(crc8_icode) -> {8, 16#1D, 16#FD, 16#00, false};
name_to_config(crc8_itu) -> {8, 16#07, 16#00, 16#55, false};
name_to_config(crc8_maxim) -> {8, 16#31, 16#00, 16#00, true};
name_to_config(crc8_sensirion) -> {8, 16#31, 16#FF, 16#00, false};
name_to_config(crc8_rohc) -> {8, 16#07, 16#FF, 16#00, true};
name_to_config(crc8_wcdma) -> {8, 16#9B, 16#00, 16#00, true};
name_to_config(crc16_ccitt_zero) -> {16, 16#1021, 16#0000, 16#0000, false};
name_to_config(crc16_arc) -> {16, 16#8005, 16#0000, 16#0000, true};
name_to_config(crc16_aug_ccitt) -> {16, 16#1021, 16#1D0F, 16#0000, false};
name_to_config(crc16_buypass) -> {16, 16#8005, 16#0000, 16#0000, false};
name_to_config(crc16_ccitt_false) -> {16, 16#1021, 16#FFFF, 16#0000, false};
name_to_config(crc16_cdma2000) -> {16, 16#C867, 16#FFFF, 16#0000, false};
name_to_config(crc16_dds_110) -> {16, 16#8005, 16#800D, 16#0000, false};
name_to_config(crc16_dect_r) -> {16, 16#0589, 16#0000, 16#0001, false};
name_to_config(crc16_dect_x) -> {16, 16#0589, 16#0000, 16#0000, false};
name_to_config(crc16_dnp) -> {16, 16#3D65, 16#0000, 16#FFFF, true};
name_to_config(crc16_en_13757) -> {16, 16#3D65, 16#0000, 16#FFFF, false};
name_to_config(crc16_genibus) -> {16, 16#1021, 16#FFFF, 16#FFFF, false};
name_to_config(crc16_maxim) -> {16, 16#8005, 16#0000, 16#FFFF, true};
name_to_config(crc16_mcrf4xx) -> {16, 16#1021, 16#FFFF, 16#0000, true};
name_to_config(crc16_riello) -> {16, 16#1021, 16#B2AA, 16#0000, true};
name_to_config(crc16_t10_dif) -> {16, 16#8BB7, 16#0000, 16#0000, false};
name_to_config(crc16_teledisk) -> {16, 16#A097, 16#0000, 16#0000, false};
name_to_config(crc16_tms37157) -> {16, 16#1021, 16#89EC, 16#0000, true};
name_to_config(crc16_usb) -> {16, 16#8005, 16#FFFF, 16#FFFF, true};
name_to_config(crc16_a) -> {16, 16#1021, 16#C6C6, 16#0000, true};
name_to_config(crc16_kermit) -> {16, 16#1021, 16#0000, 16#0000, true};
name_to_config(crc16_modbus) -> {16, 16#8005, 16#FFFF, 16#0000, true};
name_to_config(crc16_x_25) -> {16, 16#1021, 16#FFFF, 16#FFFF, true};
name_to_config(crc16_xmodem) -> {16, 16#1021, 16#0000, 16#0000, false};
name_to_config(crc32) -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#FFFFFFFF, true};
name_to_config(crc32_bzip2) -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#FFFFFFFF, false};
name_to_config(crc32_c) -> {32, 16#1EDC6F41, 16#FFFFFFFF, 16#FFFFFFFF, true};
name_to_config(crc32_d) -> {32, 16#A833982B, 16#FFFFFFFF, 16#FFFFFFFF, true};
name_to_config(crc32_mpeg2) -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#00000000, false};
name_to_config(crc32_posix) -> {32, 16#04C11DB7, 16#00000000, 16#FFFFFFFF, false};
name_to_config(crc32_q) -> {32, 16#814141AB, 16#00000000, 16#00000000, false};
name_to_config(crc32_jamcrc) -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#00000000, true};
name_to_config(crc32_xfer) -> {32, 16#000000AF, 16#00000000, 16#00000000, false};
name_to_config(crc64_ecma_182) -> {64, 16#42F0E1EBA9EA3693, 16#0, 16#0, false};

name_to_config(crc64_go_iso) ->
  {64, 16#000000000000001B, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF, true};

name_to_config(crc64_we) ->
  {64, 16#42F0E1EBA9EA3693, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF, false};

name_to_config(crc64_xz) ->
  {64, 16#42F0E1EBA9EA3693, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF, true}.

%
% Utility functions
%

reflect(Bits, Value) ->
  reflect(Bits, Value, 0, 0).

reflect(Bits, _Value, Result, Index) when Index == Bits -> Result;

reflect(Bits, Value, Result, Index) ->
  NextResult = case (Value band (1 bsl Index)) of
    0 -> Result;
    _ -> Result bor (1 bsl ((Bits - 1) - Index))
  end,
  reflect(Bits, Value, NextResult, Index + 1).

bits_to_mask(Bits) -> (1 bsl Bits) - 1.

bits_to_hibit(Bits) -> 1 bsl (Bits - 1).

bits_to_shift(Bits) -> Bits - 8.
