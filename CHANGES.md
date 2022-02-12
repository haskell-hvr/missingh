### 1.4.3.1

_Andreas Abel, 2022-02-12_

- Remove spurious dependency on `random`.
- Bump upper bounds on `base` and `time`.
- Tested with GHC 7.0 - 9.2.

### 1.4.3.0

_Herbert Valerio Riedel, 2020-04-09_

- New tuple construction helpers `Data.Tuple.Utils.dup` and `Data.Tuple.Utils.triple`
- Close sockets on connection failure in `connectTCPAddr` ([#50](https://github.com/haskell-hvr/missingh/issues/50))

### 1.4.2.1

_Herbert Valerio Riedel, 2019-05-20_

- GHC 7.0 only compat release

## 1.4.2.0

_Herbert Valerio Riedel, 2019-05-14_

- Fix regression (introduced in 1.4.1.0 release) in `Data.Compression.Inflate`
- Drop redundant dependency on `HUnit`
- Add more explicit `SafeHaskell` annotations to modules; all modules
  except for `System.Debian` are now explicitly either `Safe` or `Trustworthy`
- Add support for `network-3.0` and `network-3.1`

## 1.4.1.0

_John Goerzen, 2018-10-13_

- Support for GHC 8.6.1 / `base-4.12` ([#45](https://github.com/haskell-hvr/missingh/issues/45))

### 1.4.0.1

_John Goerzen, 2016-06-15_

- Restore compatibility with GHC 7.4.2

# 1.4.0.0

_John Goerzen, 2016-06-29_

- Removal of `Data.Hash.CRC32.Posix` and `System.Time.Utils.ParseDate`

- Added explicit `SafeHaskell` annotations to modules
