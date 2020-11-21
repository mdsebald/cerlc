# cerlc

## A Native Erlang Library of CRC Algorithms

### Goals
    - Implemented in native Erlang. No NIFs or other depenencies required.
    - Efficient.
    - One algorithm per module.

### Current Algorithms and Associated Modules
    - CRC-8             crc8.erl
    - CRC-16/AUG-CCITT  crc16_aug_ccitt.erl
    - CRC-16/MODBUS     crc16_modbus.erl
    - CRC-16/USB        crc16_usb.erl
    - CRC-32            crc32.erl
    - CRC-32/C          crc32_c.erl

### Build
```
    $ rebar3 compile
```

### Test
```
    $ rebar3 eunit
```

### Use

#### Erlang example
```
    CrcValue = crc16_aug_ccitt:crc(Data),
```

#### Elixir example
```
    crc_value = :crc16_aug_ccitt.crc(data)
```

### A Word on Performance

There are too many variables to definitively declare one implementation method is always faster than another in all environments. However, in benchmarking various CRC implementations, I have found in general, the performance rank of various CRC implementation methods, from fastest to slowest is as follows:

1. Erlang Built In Function (BIF) (Only CRC-32 algorithm available)
2. Erlang/Elixir with Natively Implemented Functions in 'C' (NIF's)
3. Native Erlang/Elixir with Native Compilation enabled (Also known as: HiPE)
    1. In some cases, HiPE is faster than a NIF implementation
4. Native Erlang/Elixir compiled with OTP-24, which introduced the Just In Time compiler (JIT)
    1. HiPE is not available in OTP-24 and later
5. Native Erlang/Elixir without HiPE or JIT compilation

#### Benchmarking Various CRC implementations

For comparison purposes only.  Your environment may produce different results.

[Crc Benchmark Source](https://github.com/mdsebald/CrcBenchmarks)

##### Benchmark Environment:

OS: WSL2, Ubuntu 18.04, on 64 bit Windows 10 Processor: Intel(R) Core(TM) i5-6300U CPU @ 2.40GHz, 2501 Mhz, 2 Core(s), 4 Logical Processor(s) 8 GB Ram

##### Benchmark CRC routines:
  1. Erlang BIF crc32()
  2. Erlang implementation for CRC32-C using NIF's
  3. Elixir configurable CRC-32 implemented using NIF's
  4. Native Erlang CRC-32 (cerlc library)
  5. Native Erlang CRC-32/C (cerlc library)

##### Benchmark run 3 times, using a 100 bytes of random data each time
  1. OTP-23 with HiPE enabled
  2. OTP-24 uses JIT compiler
  3. OTP-23 without HiPE enabled

##### OTP-23 with HiPE
```
iex(2)> CrcBenchmarks.run_benchmark_crc32(100)
*** &:erlang.crc32/1 ***
1.2 sec     8M iterations   0.16 μs/op

*** &:crc32cer.nif/1 ***
1.2 sec     8M iterations   0.16 μs/op

*** &CRC.crc_32/1 ***
1.5 sec     1M iterations   1.51 μs/op

*** &:crc32.crc/1 ***
1.9 sec     2M iterations   0.93 μs/op

*** &:crc32_c.crc/1 ***
1.0 sec     1M iterations   1.0 μs/op
```

##### OTP-24 with JIT compilation
```
iex(6)> CrcBenchmarks.run_benchmark_crc32(100)
*** &:erlang.crc32/1 ***
1.3 sec     8M iterations   0.16 μs/op

*** &:crc32cer.nif/1 ***
1.3 sec     8M iterations   0.17 μs/op

*** &CRC.crc_32/1 ***
1.5 sec     1M iterations   1.47 μs/op

*** &:crc32.crc/1 ***
1.8 sec     1M iterations   1.77 μs/op

*** &:crc32_c.crc/1 ***
1.8 sec     1M iterations   1.76 μs/op
```

##### OTP-23 without HiPE
```
iex(2)> CrcBenchmarks.run_benchmark_crc32(100)
*** &:erlang.crc32/1 ***
1.3 sec     8M iterations   0.16 μs/op

*** &:crc32cer.nif/1 ***
1.2 sec     8M iterations   0.15 μs/op

*** &CRC.crc_32/1 ***
1.3 sec     1M iterations   1.28 μs/op

*** &:crc32.crc/1 ***
1.0 sec   262K iterations   4.12 μs/op

*** &:crc32_c.crc/1 ***
1.0 sec   262K iterations   3.92 μs/op
```

As expected, the BIF and NIF implementations' performance remain pretty constant in all of the runs, as they are not affected by HiPE or JIT.

### Sources of Truth
[Javascript CRC Calculator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)

[crccalc](https://crccalc.com/)

### Don't see the CRC algorithm you need?
    Open an issue and I'll add it.

### Thanks!
