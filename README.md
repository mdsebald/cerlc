# cerlc

## A Custom Configurable Cyclic Redundancy Check (CRC), Function Generator
 
[![Erlang CI](https://github.com/mdsebald/cerlc/workflows/Erlang%20CI/badge.svg)](https://github.com/mdsebald/cerlc/actions)

### Implemented in native Erlang. No NIFs or depenencies required.

#### Select from over 50 predefined CRC algorithms.  
    See src/cerlc.erl for complete list

#### OR Generate custom CRC algorithms by manually specifying the parameters. 
    bit width, polynomial, initial value, final XOR value, and data reflected or not 

### Build
```
    $ rebar3 compile
```

### Test
```
    $ rebar3 eunit
```

### Use

#### In an Erlang application
```erlang

% Add to list of dependencies in rebar.config
{deps, [
    {cerlc, "0.2.0"}
]}.


% Example: Using a predefined CRC algorithm
CrcDefn = cerlc:init(crc16_aug_ccitt),

Crc = cerlc:calc_crc(Data, CrcDefn)


% Example: Creating a custom CRC algorithm
% Custom CRC parameters: {Bits, Polynomial, InitValue, FinalXorValue, Reflected}
CustomDefn = cerlc:init({8, 16#1234, 0, 16#FF, false}),

Crc = cerlc:calc_crc(Data, CustomDefn)


% Example: Using a macro to evaluate init() function at compile time
-define(CRC_DEFN, cerlc:init(crc16_aug_ccitt)).

% Data may be a binary or list of bytes
Crc = cerlc:calc_crc(Data, ?CRC_DEFN),

```

#### In an Elixir application
```elixir

# Add to list of dependencies in mix.exs
def deps do
  [
    {:cerlc, "~> 0.2.0"}
  ]
end


# Example: Using a predefined CRC algorithm
crc8_defn = :cerlc.init(:crc8)

crc = :cerlc.calc_crc(data, crc8_defn)


# Example: Creating a custom CRC algorithm
# Custom CRC parameters: {bits, polynomial, init_value, final_xor_value, reflected}
custom_defn = cerlc:init({8, 0x1234, 0, 0xFF, false}),

crc = :cerlc.calc_crc(data, custom_defn)


# Example: Using a module attribute to evaluate init() function at compile time
@crc32_defn :cerlc.init(:crc32_c)

crc = :cerlc.calc_crc(data, @crc32_defn)

```

### Performance

cerlc is slower than Erlang's built in crc32() function and CRC's implemented using 'C' language NIF's.

cerlc is useful for quickly generating CRC's with little code and no other dependencies.

### Sources of CRC Definitions
[Javascript CRC Calculator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)

[crccalc](https://crccalc.com/)
