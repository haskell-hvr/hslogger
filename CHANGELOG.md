See also https://pvp.haskell.org/faq

#### 1.3.1.1 *(patch)*

- Drop support for GHC 7
- Tested with GHC 8.0 - 9.10

### 1.3.1.0 *(minor)*

- Evaluate message before taking lock in simple handler ([#49](https://github.com/haskell-hvr/hslogger/pull/49))
- Define `Typeable`, `Data`, `Generic` and `NFData` instances for `System.Log.Priority` ([#43](https://github.com/haskell-hvr/hslogger/pull/43))

## 1.3.0.0 *(major)*

- **[semantic change]** Messages are encoded as UTF-8 (previously the encoding was locale dependent) for the Syslog and Growl backends
- Add support for `network-3.0`; remove redundant dependency on `directory` and `process`
