# sml-dice

[![CI](https://github.com/sjqtentacles/sml-dice/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-dice/actions/workflows/ci.yml)

Pure Standard ML **RPG dice notation**: parser (`2d6`, `3d6+2`, `4d6kh3`), exact probability distributions via convolution, expected value, and seeded sampling via vendored `sml-prng`.

No clock, no I/O, no non-determinism. Byte-identical under **MLton** and **Poly/ML**.

Numeric parsing is precision-safe and portable: dice numbers are converted through `IntInf.fromString` and bounds-checked against the fixed limit `2147483647` (2^31-1), so out-of-range counts/sides (e.g. `9999999999d6`) return `NONE` on both compilers instead of raising `Overflow` on 32-bit MLton or silently succeeding on 63-bit Poly/ML.

## Running `make example` prints:

```
=== Dice ===
2d6 distribution (value:count):
   2 | *
   3 | **
   4 | ***
   5 | ****
   6 | *****
   7 | ******
   8 | *****
   9 | ****
  10 | ***
  11 | **
  12 | *
E[2d6] = 7/1

3d6+2 expected value: 25/2

Seeded rolls of 3d6 (seed=99):
  9
  13
  9
  14
  8

4d6kh3 (drop lowest, common for D&D stats):
  9
  13
  9
  14
  13
  16
```

## API

```sml
datatype expr = Const | Roll | Add | Sub | KeepHigh | KeepLow

val parse         : string -> expr option
val distribution  : expr -> (int * int) list   (* (value, count) sorted *)
val expectedValue : expr -> int * int           (* exact fraction num/den *)
val roll          : expr -> Prng.SplitMix64.state -> int * Prng.SplitMix64.state
```

Supported notation: `NdM`, `d6` (= `1d6`), `+`, `-`, `NdMkh<k>` (keep highest k), `NdMkl<k>` (keep lowest k).

## Build & test

```sh
make test && make test-poly && make example
```

## Tests

**24 deterministic checks**: parse round-trips, 2d6 distribution (1,6,1 counts for 2,7,12), E[3d6]=21/2, E[3d6+2]=25/2, Const distribution, seeded roll determinism, and out-of-range parse boundary cases (`>2^31-1` counts/sides return `NONE`, never raise, on both compilers).

## License

MIT. See [LICENSE](LICENSE).
