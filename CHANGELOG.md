### 1.3.2.0

_2025-08-30, Andreas Abel_

- Drop flag `network--GT-3_0_0` and support for `network < 3`
- Allow newer `time`
- Tested with GHC 8.0 - 9.14 alpha1

#### 1.3.1.2

_2025-03-11, Andreas Abel_

- Thoroughly drop support for GHC 7
- Allow newer `containers`
- Tested with GHC 8.0 - 9.12.1

#### 1.3.1.1

_2024-04-15, Andreas Abel_

- Drop support for GHC 7
- Tested with GHC 8.0 - 9.10

### 1.3.1.0

_2019-10-07, Herbert Valerio Riedel_

- Evaluate message before taking lock in simple handler ([#49](https://github.com/haskell-hvr/hslogger/pull/49))
- Define `Typeable`, `Data`, `Generic` and `NFData` instances for `System.Log.Priority` ([#43](https://github.com/haskell-hvr/hslogger/pull/43))

## 1.3.0.0

_2019-04-15, Herbert Valerio Riedel_

- **[semantic change]** Messages are encoded as UTF-8 (previously the encoding was locale dependent) for the Syslog and Growl backends
- Add support for `network-3.0`; remove redundant dependency on `directory` and `process`
