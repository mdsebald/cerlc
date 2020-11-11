%%
%% Implement CRC-8
%%
%% Input reflected: no
%% Result reflected: no
%% Polynomial: 0x07
%% Initial Value: 0x0
%% Final Xor Value: 0x0
%%

-module(crc8).

-compile(native).

-export([crc/1, crc/2, table/0]).

-spec crc(binary() | [byte()]) -> byte().
crc(Data) when is_binary(Data) ->
  crc(binary_to_list(Data));

crc(Data) when is_list(Data) ->
  crc(16#0, Data).

-spec crc(byte(), [byte()]) -> byte().
crc(Crc, [X | Rem]) ->
  % table() tuple is indexed from 1, not 0
  Index = (Crc bxor X) + 1,
  NextCrc = element(Index, table()),
  crc(NextCrc, Rem);

crc(Crc, []) -> Crc.

-spec table() -> tuple().
table() ->
{
  16#00, 16#07, 16#0E, 16#09, 16#1C, 16#1B, 16#12, 16#15,
  16#38, 16#3F, 16#36, 16#31, 16#24, 16#23, 16#2A, 16#2D,
  16#70, 16#77, 16#7E, 16#79, 16#6C, 16#6B, 16#62, 16#65,
  16#48, 16#4F, 16#46, 16#41, 16#54, 16#53, 16#5A, 16#5D,
  16#E0, 16#E7, 16#EE, 16#E9, 16#FC, 16#FB, 16#F2, 16#F5,
  16#D8, 16#DF, 16#D6, 16#D1, 16#C4, 16#C3, 16#CA, 16#CD,
  16#90, 16#97, 16#9E, 16#99, 16#8C, 16#8B, 16#82, 16#85,
  16#A8, 16#AF, 16#A6, 16#A1, 16#B4, 16#B3, 16#BA, 16#BD,
  16#C7, 16#C0, 16#C9, 16#CE, 16#DB, 16#DC, 16#D5, 16#D2,
  16#FF, 16#F8, 16#F1, 16#F6, 16#E3, 16#E4, 16#ED, 16#EA,
  16#B7, 16#B0, 16#B9, 16#BE, 16#AB, 16#AC, 16#A5, 16#A2,
  16#8F, 16#88, 16#81, 16#86, 16#93, 16#94, 16#9D, 16#9A,
  16#27, 16#20, 16#29, 16#2E, 16#3B, 16#3C, 16#35, 16#32,
  16#1F, 16#18, 16#11, 16#16, 16#03, 16#04, 16#0D, 16#0A,
  16#57, 16#50, 16#59, 16#5E, 16#4B, 16#4C, 16#45, 16#42,
  16#6F, 16#68, 16#61, 16#66, 16#73, 16#74, 16#7D, 16#7A,
  16#89, 16#8E, 16#87, 16#80, 16#95, 16#92, 16#9B, 16#9C,
  16#B1, 16#B6, 16#BF, 16#B8, 16#AD, 16#AA, 16#A3, 16#A4,
  16#F9, 16#FE, 16#F7, 16#F0, 16#E5, 16#E2, 16#EB, 16#EC,
  16#C1, 16#C6, 16#CF, 16#C8, 16#DD, 16#DA, 16#D3, 16#D4,
  16#69, 16#6E, 16#67, 16#60, 16#75, 16#72, 16#7B, 16#7C,
  16#51, 16#56, 16#5F, 16#58, 16#4D, 16#4A, 16#43, 16#44,
  16#19, 16#1E, 16#17, 16#10, 16#05, 16#02, 16#0B, 16#0C,
  16#21, 16#26, 16#2F, 16#28, 16#3D, 16#3A, 16#33, 16#34,
  16#4E, 16#49, 16#40, 16#47, 16#52, 16#55, 16#5C, 16#5B,
  16#76, 16#71, 16#78, 16#7F, 16#6A, 16#6D, 16#64, 16#63,
  16#3E, 16#39, 16#30, 16#37, 16#22, 16#25, 16#2C, 16#2B,
  16#06, 16#01, 16#08, 16#0F, 16#1A, 16#1D, 16#14, 16#13,
  16#AE, 16#A9, 16#A0, 16#A7, 16#B2, 16#B5, 16#BC, 16#BB,
  16#96, 16#91, 16#98, 16#9F, 16#8A, 16#8D, 16#84, 16#83,
  16#DE, 16#D9, 16#D0, 16#D7, 16#C2, 16#C5, 16#CC, 16#CB,
  16#E6, 16#E1, 16#E8, 16#EF, 16#FA, 16#FD, 16#F4, 16#F3
}.
