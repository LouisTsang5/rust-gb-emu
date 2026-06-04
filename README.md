# Rust Gameboy Emulator

## Building the emulator

```sh
cargo build --release --bin gb-emu
```

## Running the emulator

Substitute <ROM_FILE> with a ROM binary file 

```sh
./gb-emu [<ROM_FILE>]
```

## Reference Materials

### Hardware Spec
- https://gbdev.io/pandocs

### Test ROMs

Currently passes all tests in **cpu_instrs** and **instr_timing**

- https://github.com/retrio/gb-test-roms
