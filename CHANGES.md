### 1.4.2.0

- Fix regression (introduced in 1.4.1.0 release) in `Data.Compression.Inflate`
- Drop redundant dependency on `HUnit`
- Add more explicit `SafeHaskell` annotations to modules; all modules
  except for `System.Debian` are now explicitly either `Safe` or `Trustworthy`
- Add support for `network-3.0` and `network-3.1`

## 1.4.1.0

- Support for GHC 8.6.1 / `base-4.12` ([#45](https://github.com/haskell-hvr/missingh/issues/45))

### 1.4.0.1

- Restore compatibility with GHC 7.4.2

# 1.4.0.0

- Removal of `Data.Hash.CRC32.Posix` and `System.Time.Utils.ParseDate`

- Added explicit `SafeHaskell` annotations to modules
