-module(cerlc_tests).

-include_lib("eunit/include/eunit.hrl").

f8_bit_un_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc8),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#F4).

f8_bit_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc8_rohc),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#D0).

f8_bit_Sensirion_CRC_test() ->
  CrcDefn = cerlc:init(crc8_sensirion),
  ?assert(cerlc:calc_crc([16#BE, 16#EF], CrcDefn) == 16#92).

f16_bit_un_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc16_aug_ccitt),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#E5CC).

f16_bit_un_reflected_check_CRC_test() ->
  CrcDefn = cerlc:init(crc16_aug_ccitt),

  ?assert(cerlc:calc_crc(
            [16#31, 16#32, 16#33, 16#34, 16#35, 16#36, 16#37, 16#38, 16#39, 16#E5, 16#CC],
            CrcDefn
          ) == 0).

f16_bit_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc16_usb),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#B4C8).

f32_bit_un_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc32_bzip2),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#FC891918).

f32_bit_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc32),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#CBF43926).

f64_bit_un_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc64_ecma_182),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#6C40DF5F0B497347).

f64_bit_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init(crc64_go_iso),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#B90956C775A41001).

binary_to_list_conversion_test() ->
  CrcDefn = cerlc:init(crc64_go_iso),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#B90956C775A41001).

f16_bit_random_reflected_generate_CRC_test() ->
  CrcDefn = cerlc:init({16, 16#1234, 0, 0, true}),
  ?assert(cerlc:calc_crc("123456789", CrcDefn) == 16#F13).

f16_bit_random_reflected_check_CRC_test() ->
  CrcDefn = cerlc:init({16, 16#1234, 0, 0, true}),

  ?assert(cerlc:calc_crc(
            [16#31, 16#32, 16#33, 16#34, 16#35, 16#36, 16#37, 16#38, 16#39, 16#13, 16#0F],
            CrcDefn
          ) == 0).
