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
    TODO

### Sources of Truth
[Javascript CRC Calculator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)

[crccalc](https://crccalc.com/)

### Don't see the CRC algorithm you need?
    Open an issue and I'll add it.

### Thanks!
