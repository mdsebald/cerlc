-module(gen_cerlc).

% Pretend the cerlc module and functions exist already, to suppress compiler errors
% They won't actually be created, until gen_module() is executed
-import(cerlc, [crc/1, crc/2]).

-export([gen_module/0,
         gen_table/0]).

-on_load(gen_module/0).

gen_module() ->
  M = gen_crc_fun(ct_expand:term(gen_table())),
  merl:compile_and_load(M, [native]),
  % must return ok, for -on_load() directive to succeed
  ok.

gen_crc_fun({8, Table, InitValue, FinalXorValue, _Reflected}) ->
  codegen:gen_module(
    cerlc, 
    [{crc,1}, {crc,2}],
    [
      {crc, 
      fun
        (Data) -> crc({'$var', InitValue}, Data)
      end},
      {crc, 
      fun
        (Crc, [X | Rem]) ->
          % table() tuple is indexed from 1, not 0
          Index = (Crc bxor X) + 1,
          NextCrc = element(Index, {'$var', Table}),
          crc(NextCrc, Rem);
        (Crc, []) -> Crc bxor {'$var', FinalXorValue}
      end}
    ]
  );

gen_crc_fun({Bits, Table, InitValue, FinalXorValue, false}) ->
  Mask = bits_to_mask(Bits),
  codegen:gen_module(
    cerlc, 
    [{crc,1}, {crc,2}],
    [
      {crc, 
      fun
        (Data) -> crc({'$var', InitValue}, Data)
      end},
      {crc, 
      fun
        (Crc, [X | Rem]) ->
          % table() tuple is indexed from 1, not 0
          Index = ((Crc bsr 8) bxor X) + 1,
          NextCrc = ((Crc bsl 8) bxor element(Index, {'$var', Table})) band {'$var', Mask},
          crc(NextCrc, Rem);
        (Crc, []) -> Crc bxor {'$var', FinalXorValue}
      end}
    ]
  );

gen_crc_fun({Bits, Table, InitValue, FinalXorValue, true}) ->
  Mask = bits_to_mask(Bits),
  codegen:gen_module(
    cerlc, 
    [{crc,1}, {crc,2}],
    [
      {crc, 
      fun
        (Data) -> crc({'$var', InitValue}, Data)
      end},
      {crc, 
      fun
        (Crc, [X | Rem]) ->
          % table() tuple is indexed from 1, not 0
          Index = ((Crc bxor X) band 16#FF) + 1,
          NextCrc = ((Crc bsr 8) bxor element(Index, {'$var', Table})) band {'$var', Mask},
          crc(NextCrc, Rem);
        (Crc, []) -> Crc bxor {'$var', FinalXorValue}
      end}
    ]
  ).

%%
%% Generate CRC lookup table
%%

gen_table() ->
  {Bits, Polynomial, InitValue, FinalXorValue, Reflected} = get_config(),
  {Bits, gen_table(Bits, Polynomial, Reflected), InitValue, FinalXorValue, Reflected}.

gen_table(Bits, Polynomial, false) -> 
  HiBit = bits_to_hibit(Bits),
  Mask = bits_to_mask(Bits),
  Shift = bits_to_shift(Bits),
  gen_table(HiBit, Mask, Shift, Polynomial, [], 0);

gen_table(Bits, Polynomial, true) -> 
  PolynomialR = reflect(Bits, Polynomial),
  Mask = bits_to_mask(Bits),
  gen_table_r(Mask, PolynomialR, [], 0).

%%
%% Generate un-reflected CRC lookup table
%%

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

%%
%% Generate reflected CRC lookup table
%%

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

%%
%% Get crc configuration
%%

get_config() ->
  {ok, Terms} = file:consult("cerlc.cfg"),

  case proplists:get_value(crc_name, Terms) of
    undefined -> 
      {
        proplists:get_value(bits, Terms),
        proplists:get_value(polynomial, Terms),
        proplists:get_value(init_value, Terms),
        proplists:get_value(final_xor_value, Terms),
        proplists:get_value(reflected, Terms)
      };
    CrcName -> name_to_config(CrcName)
  end.

name_to_config("CRC8") -> {8, 16#07, 16#00, 16#00, false};
name_to_config("CRC8_SAE_J1850") -> {8, 16#1D, 16#FF, 16#FF, false};
name_to_config("CRC8_SAE_J1850_ZERO") -> {8, 16#1D, 16#00, 16#00, false};
name_to_config("CRC8_8H2F") -> {8, 16#2F, 16#FF, 16#FF, false};
name_to_config("CRC8_CDMA2000") -> {8, 16#9B, 16#FF, 16#00, false};
name_to_config("CRC8_DARC") -> {8, 16#39, 16#00, 16#00, true};
name_to_config("CRC8_DVB_S2") -> {8, 16#D5, 16#00, 16#00, false};
name_to_config("CRC8_EBU") -> {8, 16#1D, 16#FF, 16#00, true};
name_to_config("CRC8_ICODE") -> {8, 16#1D, 16#FD, 16#00, false};
name_to_config("CRC8_ITU") -> {8, 16#07, 16#00, 16#55, false};
name_to_config("CRC8_MAXIM") -> {8, 16#31, 16#00, 16#00, true};
name_to_config("CRC8_ROHC") -> {8, 16#07, 16#FF, 16#00, true};
name_to_config("CRC8_WCDMA") -> {8, 16#9B, 16#00, 16#00, true};
name_to_config("CRC16_CCIT_ZERO") -> {16, 16#1021, 16#0000, 16#0000, false};
name_to_config("CRC16_ARC") -> {16, 16#8005, 16#0000, 16#0000, true};
name_to_config("CRC16_AUG_CCITT") -> {16, 16#1021, 16#1D0F, 16#0000, false};
name_to_config("CRC16_BUYPASS") -> {16, 16#8005, 16#0000, 16#0000, false};
name_to_config("CRC16_CCITT_FALSE") -> {16, 16#1021, 16#FFFF, 16#0000, false};
name_to_config("CRC16_CDMA2000") -> {16, 16#C867, 16#FFFF, 16#0000, false};
name_to_config("CRC16_DDS_110") -> {16, 16#8005, 16#800D, 16#0000, false};
name_to_config("CRC16_DECT_R") -> {16, 16#0589, 16#0000, 16#0001, false};
name_to_config("CRC16_DECT_X") -> {16, 16#0589, 16#0000, 16#0000, false};
name_to_config("CRC16_DNP") -> {16, 16#3D65, 16#0000, 16#FFFF, true};
name_to_config("CRC16_EN_13757") -> {16, 16#3D65, 16#0000, 16#FFFF, false};
name_to_config("CRC16_GENIBUS") -> {16, 16#1021, 16#FFFF, 16#FFFF, false};
name_to_config("CRC16_MAXIM") -> {16, 16#8005, 16#0000, 16#FFFF, true};
name_to_config("CRC16_MCRF4XX") -> {16, 16#1021, 16#FFFF, 16#0000, true};
name_to_config("CRC16_RIELLO") -> {16, 16#1021, 16#B2AA, 16#0000, true};
name_to_config("CRC16_T10_DIF") -> {16, 16#8BB7, 16#0000, 16#0000, false};
name_to_config("CRC16_TELEDISK") -> {16, 16#A097, 16#0000, 16#0000, false};
name_to_config("CRC16_TMS37157") -> {16, 16#1021, 16#89EC, 16#0000, true};
name_to_config("CRC16_USB") -> {16, 16#8005, 16#FFFF, 16#FFFF, true};
name_to_config("CRC16_A") -> {16, 16#1021, 16#C6C6, 16#0000, true};
name_to_config("CRC16_KERMIT") -> {16, 16#1021, 16#0000, 16#0000, true};
name_to_config("CRC16_MODBUS") -> {16, 16#8005, 16#FFFF, 16#0000, true};
name_to_config("CRC16_X_25") -> {16, 16#1021, 16#FFFF, 16#FFFF, true};
name_to_config("CRC16_XMODEM") -> {16, 16#1021, 16#0000, 16#0000, false};
name_to_config("CRC32") -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#FFFFFFFF, true};
name_to_config("CRC32_BZIP2") -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#FFFFFFFF, false};
name_to_config("CRC32_C") -> {32, 16#1EDC6F41, 16#FFFFFFFF, 16#FFFFFFFF, true};
name_to_config("CRC32_D") -> {32, 16#A833982B, 16#FFFFFFFF, 16#FFFFFFFF, true};
name_to_config("CRC32_MPEG2") -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#00000000, false};
name_to_config("CRC32_POSIX") -> {32, 16#04C11DB7, 16#00000000, 16#FFFFFFFF, false};
name_to_config("CRC32_Q") -> {32, 16#814141AB, 16#00000000, 16#00000000, false};
name_to_config("CRC32_JAMCRC") -> {32, 16#04C11DB7, 16#FFFFFFFF, 16#00000000, true};
name_to_config("CRC32_XFER") -> {32, 16#000000AF, 16#00000000, 16#00000000, false};
name_to_config("CRC64") -> {64, 16#42F0E1EBA9EA3693, 16#00, 16#00, false};
name_to_config("CRC64_GO_ISO") -> {64, 16#000000000000001B, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF, true};
name_to_config("CRC64_WE") -> {64, 16#42F0E1EBA9EA3693, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF, false};
name_to_config("CRC64_XZ") -> {64, 16#42F0E1EBA9EA3693, 16#FFFFFFFFFFFFFFFF, 16#FFFFFFFFFFFFFFFF, true}.

%%
%% Utility functions
%%

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
