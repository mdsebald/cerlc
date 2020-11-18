%%
%% Implement CRC-16/AUG CCITT 
%%
%% Input reflected: no
%% Result reflected: no
%% Polynomial: 0x1021
%% Initial Value: 0x1D0F
%% Final Xor Value: 0x0
%%

-module(crc16_aug_ccitt).

-compile(native).

-compile({inline,[table/0]}).

-export([crc/1, crc/2]).

-type uint16() :: 0..16#FFFF.

-spec crc(binary() | [byte()]) -> uint16().
crc(Data) when is_binary(Data) ->
  crc(binary_to_list(Data));

crc(Data) when is_list(Data) ->
  crc(16#1D0F, Data).

-spec crc(uint16(), [byte()]) -> uint16().
crc(Crc, [X | Rem]) ->
  % table() tuple is indexed from 1, not 0
  Index = ((Crc bsr 8) bxor X) + 1,
  NextCrc = ((Crc bsl 8) bxor element(Index, table())) band 16#FFFF,
  crc(NextCrc, Rem);

crc(Crc, []) -> Crc.

-spec table() -> tuple().
table() ->
{
  16#0000, 16#1021, 16#2042, 16#3063, 16#4084, 16#50A5, 16#60C6, 16#70E7,
  16#8108, 16#9129, 16#A14A, 16#B16B, 16#C18C, 16#D1AD, 16#E1CE, 16#F1EF,
  16#1231, 16#0210, 16#3273, 16#2252, 16#52B5, 16#4294, 16#72F7, 16#62D6,
  16#9339, 16#8318, 16#B37B, 16#A35A, 16#D3BD, 16#C39C, 16#F3FF, 16#E3DE,
  16#2462, 16#3443, 16#0420, 16#1401, 16#64E6, 16#74C7, 16#44A4, 16#5485,
  16#A56A, 16#B54B, 16#8528, 16#9509, 16#E5EE, 16#F5CF, 16#C5AC, 16#D58D,
  16#3653, 16#2672, 16#1611, 16#0630, 16#76D7, 16#66F6, 16#5695, 16#46B4,
  16#B75B, 16#A77A, 16#9719, 16#8738, 16#F7DF, 16#E7FE, 16#D79D, 16#C7BC,
  16#48C4, 16#58E5, 16#6886, 16#78A7, 16#0840, 16#1861, 16#2802, 16#3823,
  16#C9CC, 16#D9ED, 16#E98E, 16#F9AF, 16#8948, 16#9969, 16#A90A, 16#B92B,
  16#5AF5, 16#4AD4, 16#7AB7, 16#6A96, 16#1A71, 16#0A50, 16#3A33, 16#2A12,
  16#DBFD, 16#CBDC, 16#FBBF, 16#EB9E, 16#9B79, 16#8B58, 16#BB3B, 16#AB1A,
  16#6CA6, 16#7C87, 16#4CE4, 16#5CC5, 16#2C22, 16#3C03, 16#0C60, 16#1C41,
  16#EDAE, 16#FD8F, 16#CDEC, 16#DDCD, 16#AD2A, 16#BD0B, 16#8D68, 16#9D49,
  16#7E97, 16#6EB6, 16#5ED5, 16#4EF4, 16#3E13, 16#2E32, 16#1E51, 16#0E70,
  16#FF9F, 16#EFBE, 16#DFDD, 16#CFFC, 16#BF1B, 16#AF3A, 16#9F59, 16#8F78,
  16#9188, 16#81A9, 16#B1CA, 16#A1EB, 16#D10C, 16#C12D, 16#F14E, 16#E16F,
  16#1080, 16#00A1, 16#30C2, 16#20E3, 16#5004, 16#4025, 16#7046, 16#6067,
  16#83B9, 16#9398, 16#A3FB, 16#B3DA, 16#C33D, 16#D31C, 16#E37F, 16#F35E,
  16#02B1, 16#1290, 16#22F3, 16#32D2, 16#4235, 16#5214, 16#6277, 16#7256,
  16#B5EA, 16#A5CB, 16#95A8, 16#8589, 16#F56E, 16#E54F, 16#D52C, 16#C50D,
  16#34E2, 16#24C3, 16#14A0, 16#0481, 16#7466, 16#6447, 16#5424, 16#4405,
  16#A7DB, 16#B7FA, 16#8799, 16#97B8, 16#E75F, 16#F77E, 16#C71D, 16#D73C,
  16#26D3, 16#36F2, 16#0691, 16#16B0, 16#6657, 16#7676, 16#4615, 16#5634,
  16#D94C, 16#C96D, 16#F90E, 16#E92F, 16#99C8, 16#89E9, 16#B98A, 16#A9AB,
  16#5844, 16#4865, 16#7806, 16#6827, 16#18C0, 16#08E1, 16#3882, 16#28A3,
  16#CB7D, 16#DB5C, 16#EB3F, 16#FB1E, 16#8BF9, 16#9BD8, 16#ABBB, 16#BB9A,
  16#4A75, 16#5A54, 16#6A37, 16#7A16, 16#0AF1, 16#1AD0, 16#2AB3, 16#3A92,
  16#FD2E, 16#ED0F, 16#DD6C, 16#CD4D, 16#BDAA, 16#AD8B, 16#9DE8, 16#8DC9,
  16#7C26, 16#6C07, 16#5C64, 16#4C45, 16#3CA2, 16#2C83, 16#1CE0, 16#0CC1,
  16#EF1F, 16#FF3E, 16#CF5D, 16#DF7C, 16#AF9B, 16#BFBA, 16#8FD9, 16#9FF8,
  16#6E17, 16#7E36, 16#4E55, 16#5E74, 16#2E93, 16#3EB2, 16#0ED1, 16#1EF0
}.

%% ====================================================================
%% Tests
%% ====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_list_test() ->
  ?assert(crc("") == 16#1D0F).

empty_bin_test() ->
  ?assert(crc(<<>>) == 16#1D0F).

std_list_test() ->
  ?assert(crc("123456789") == 16#E5CC).

bin_with_crc_test() ->
  ?assert(crc(<<16#31,16#32,16#33,16#34,16#35,16#36,16#37,16#38,16#39,16#E5,16#CC>>) == 0).

-endif.
