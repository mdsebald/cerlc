-module(cerlc_tests).

-include_lib("eunit/include/eunit.hrl").

empty_list_test() ->
  ?assert(cerlc:crc("") == 16#1D0F).

% empty_bin_test() ->
%   ?assert(cerlc:crc(<<>>) == 16#1D0F).

std_list_test() ->
  ?assert(cerlc:crc("123456789") == 16#E5CC).

% bin_with_crc_test() ->
%   ?assert(cerlc:crc(<<16#31,16#32,16#33,16#34,16#35,16#36,16#37,16#38,16#39,16#E5,16#CC>>) == 0).
