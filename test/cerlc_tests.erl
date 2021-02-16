-module(cerlc_tests).

-include_lib("eunit/include/eunit.hrl").

% Test each predefined CRC algorithm
crc8_test() -> crc_test_common(crc8, 16#F4).
crc8_sae_j1850_test() -> crc_test_common(crc8_sae_j1850, 16#4B).
crc8_sae_j1850_zero_test() -> crc_test_common(crc8_sae_j1850_zero, 16#37).
crc8_8h2f_test() -> crc_test_common(crc8_8h2f, 16#DF).
crc8_cdma2000_test() -> crc_test_common(crc8_cdma2000, 16#DA).
crc8_darc_test() -> crc_test_common(crc8_darc, 16#15).
crc8_dvb_s2_test() -> crc_test_common(crc8_dvb_s2, 16#BC).
crc8_ebu_test() -> crc_test_common(crc8_ebu, 16#97).
crc8_icode_test() -> crc_test_common(crc8_icode, 16#7E).
crc8_itu_test() -> crc_test_common(crc8_itu, 16#A1).
crc8_maxim_test() -> crc_test_common(crc8_maxim, 16#A1).
crc8_sensirion_test() -> crc_test_common(crc8_sensirion, 16#92, [16#BE, 16#EF]).
crc8_rohc_test() -> crc_test_common(crc8_rohc, 16#D0).
crc8_wcdma_test() -> crc_test_common(crc8_wcdma, 16#25).
crc16_ccitt_zero_test() -> crc_test_common(crc16_ccitt_zero, 16#31C3).
crc16_arc_test() -> crc_test_common(crc16_arc, 16#BB3D).
crc16_aug_ccitt_test() -> crc_test_common(crc16_aug_ccitt, 16#E5CC).
crc16_buypass_test() -> crc_test_common(crc16_buypass, 16#FEE8).
crc16_ccitt_false_test() -> crc_test_common(crc16_ccitt_false, 16#29B1).
crc16_cdma2000_test() -> crc_test_common(crc16_cdma2000, 16#4C06).
crc16_dds_110_test() -> crc_test_common(crc16_dds_110, 16#9ECF).
crc16_dect_r_test() -> crc_test_common(crc16_dect_r, 16#007E).
crc16_dect_x_test() -> crc_test_common(crc16_dect_x, 16#007F).
crc16_dnp_test() -> crc_test_common(crc16_dnp, 16#EA82).
crc16_en_13757_test() -> crc_test_common(crc16_en_13757, 16#C2B7).
crc16_genibus_test() -> crc_test_common(crc16_genibus, 16#D64E).
crc16_maxim_test() -> crc_test_common(crc16_maxim, 16#44C2).
crc16_mcrf4xx_test() -> crc_test_common(crc16_mcrf4xx, 16#6F91).
crc16_riello_test() -> crc_test_common(crc16_riello, 16#63D0).
crc16_t10_dif_test() -> crc_test_common(crc16_t10_dif, 16#D0DB).
crc16_teledisk_test() -> crc_test_common(crc16_teledisk, 16#0FB3).
crc16_tms37157_test() -> crc_test_common(crc16_tms37157, 16#26B1).
crc16_usb_test() -> crc_test_common(crc16_usb, 16#B4C8).
crc16_a_test() -> crc_test_common(crc16_a, 16#BF05).
crc16_kermit_test() -> crc_test_common(crc16_kermit, 16#2189).
crc16_modbus_test() -> crc_test_common(crc16_modbus, 16#4B37).
crc16_x_25_test() -> crc_test_common(crc16_x_25, 16#906E).
crc16_xmodem_test() -> crc_test_common(crc16_xmodem, 16#31C3).
crc32_test() -> crc_test_common(crc32, 16#CBF43926).
crc32_bzip2_test() -> crc_test_common(crc32_bzip2, 16#FC891918).
crc32_c_test() -> crc_test_common(crc32_c, 16#E3069283).
crc32_d_test() -> crc_test_common(crc32_d, 16#87315576).
crc32_mpeg2_test() -> crc_test_common(crc32_mpeg2, 16#0376E6E7).
crc32_posix_test() -> crc_test_common(crc32_posix, 16#765E7680).
crc32_q_test() -> crc_test_common(crc32_q, 16#3010BF7F).
crc32_jamcrc_test() -> crc_test_common(crc32_jamcrc, 16#340BC6D9).
crc32_xfer_test() -> crc_test_common(crc32_xfer, 16#BD0BE338).
crc64_ecma_182_test() -> crc_test_common(crc64_ecma_182, 16#6C40DF5F0B497347).
crc64_go_iso_test() -> crc_test_common(crc64_go_iso, 16#B90956C775A41001).
crc64_we_test() -> crc_test_common(crc64_we, 16#62EC59E3F1A4F00A).
crc64_xz_test() -> crc_test_common(crc64_xz, 16#995DC9BBDF1939FA).

% Test all bit widths, un-reflected and reflected, with made up parameters
crc8_custom_config_test() -> crc_test_common({8, 16#12, 16#34, 16#56, false}, 16#36).
crc8_r_custom_config_test() -> crc_test_common({8, 16#12, 16#34, 16#56, true}, 16#22).
crc16_custom_config_test() -> crc_test_common({16, 16#1234, 16#5678, 16#9ABC, false}, 16#420C).
crc16_r_custom_config_test() -> crc_test_common({16, 16#1234, 16#5678, 16#9ABC, true}, 16#AE4A).
crc32_custom_config_test() -> crc_test_common({32, 16#12345678, 16#9ABCDEF0, 16#12345678, false}, 16#D6FCC8B0).
crc32_r_custom_config_test() -> crc_test_common({32, 16#12345678, 16#9ABCDEF0, 16#12345678, true}, 16#1357BA22).
crc64_custom_config_test() -> crc_test_common({64, 16#123456789ABCDEF0, 16#123456789ABCDEF0, 16#123456789ABCDEF0, false}, 16#980704F6123B6210).
crc64_r_custom_config_test() -> crc_test_common({64, 16#123456789ABCDEF0, 16#123456789ABCDEF0, 16#123456789ABCDEF0, true}, 16#1B851603B23D082C).

crc_test_common(CrcConfig, ExpectedCrc) ->
  crc_test_common(CrcConfig, ExpectedCrc, "123456789").

crc_test_common(CrcConfig, ExpectedCrc, TestData) ->
  CrcDefn = cerlc:init(CrcConfig),
  ?assertEqual(ExpectedCrc, cerlc:calc_crc(TestData, CrcDefn)).
