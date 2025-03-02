# 1.6.0.2

_Andreas Abel, 2025-03-02_

- Drop support for GHC 7.10
- Allow `containers < 1`
- Tested with GHC 8.0 - 9.12.1

# 1.6.0.1

_Andreas Abel, 2023-09-11_

- Repair build on Windows with GHC 9.6 (`directory-1.3.8.*`):
  Revoke `Safe` status of these modules:
  * `System.IO.WindowsCompat`
  * `System.IO.PlafCompat`
- Tested with GHC 7.10 - 9.8 alpha1.

# 1.6.0.0

_Andreas Abel, 2023-01-14_

- Allow `directory-1.3.8.0` which is no longer `Safe` Haskell.
  Thus, we need to revoke `Safe` status of the following modules:
  * `Data.MIME.Types`
  * `Network.Email.Sendmail`
  * `System.IO.HVFS`
  * `System.IO.HVFS.Combinators`
  * `System.IO.HVFS.InstanceHelpers`
  * `System.Path`
  * `System.Path.Glob`
  * `System.Path.NameManip`
- Allow `unix-2.8.0.0` (enables `directory-1.3.8.0`).
- Tested with GHC 7.10 - 9.6 alpha1.

### 1.5.0.1

_Andreas Abel, 2022-03-14_

- Repair build on Windows (regression in 1.5.0.0, [#59](https://github.com/haskell-hvr/missingh/issues/59)).
- Tested with GHC 7.10 - 9.2.

# 1.5.0.0

_Andreas Abel, 2022-02-12_

- Dropped support for GHC ≤ 7.8.
- Support `mtl-2.3`: removed `Error` instance for `BinPackerError` and `GZipError`.
- Use `sortOn` in `packLargeFirst` ([#41](https://github.com/haskell-hvr/missingh/issues/41)).
- Fix warnings for `-Wall` and `-Wcompat`.
- Hardened code by making all imports explicit.
- Tested with GHC 7.10 - 9.2.

### 1.4.3.1

_Andreas Abel, 2022-02-12_

- Remove spurious dependency on `random`.
- Bump upper bounds on `base` and `time`.
- Tested with GHC 7.0 - 9.2.

## 1.4.3.0

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
