# cerlc

## A Custom Configurable Cyclic Redundancy Check (CRC), Function Generator
 
[![Erlang CI](https://github.com/mdsebald/cerlc/workflows/Erlang%20CI/badge.svg)](https://github.com/mdsebald/cerlc/actions)

### Implemented in native Erlang. No NIFs or depenencies required.

#### Select from over 50 predefined CRC algorithms.  
    See src/cerlc.erl for complete list

#### Generate custom CRC algorithms by manually specifying the parameters. 
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

% Example Code
CrcDefn = cerlc:init(:crc16_aug_ccitt),

% Data may be a binary or list of bytes
Crc = cerlc:calc_crc(Data, CrcDefn)

```

#### In an Elixir application
```elixir

# Add to list of dependencies in mix.exs
def deps do
  [
    {:cerlc, "~> 0.2.0"}
  ]
end

# Example Code
crc_defn = :cerlc.init(:crc32_c)

# data may be a binary of list of bytes
crc = :cerlc.calc_crc(data, crc_defn)

```

### A Word on Performance

cerlc is much slower than Erlang's built in crc32() function and CRC's implemented using 'C' language NIF's. 
cerlc is useful for quickly generating CRC's with little code and no other dependencies, 
and the CRC calculation is not in a critical path.

### Sources of CRC Definitions
[Javascript CRC Calculator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)

[crccalc](https://crccalc.com/)
