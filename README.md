# Import scripts for LastPass

Included is a shell script (`export_pass.sh`) for converting password entries
from the `pass` unix password manager to a flat text file and a Haskell
program (`ExportPass.hs`) for converting such flat text files to the CSV
import format that LastPass wants.

To run the Haskell program, install GHC and cabal (e.g. from the [Haskell
Platform](https://www.haskell.org/platform/)) and try the following:

```
cabal sandbox init
cabal install attoparsec
alias ghc-sandbox='ghc -no-user-package-db \
                       -package-db .cabal-sandbox/*-packages.conf.d'

ghc-sandbox -e :main ExportPass.hs < mySecretPasswords.txt
```

Note that `ExportPass` reads from `stdin` and writes to `stdout`.
