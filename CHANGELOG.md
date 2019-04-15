See also https://pvp.haskell.org/faq

## 1.3.0.0 *(major)*

- **[semantic change]** Messages are encoded as UTF-8 (previously the encoding was locale dependent) for the Syslog and Growl backends
- Add support for `network-3.0`; remove redundant dependency on `directory` and `process`
