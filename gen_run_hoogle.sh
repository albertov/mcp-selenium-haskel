#!/usr/bin/env bash

nix build .#hoogle --no-link
echo '#!/usr/bin/env bash' > ./run_hoogle.sh
echo exec "$(nix path-info .#hoogle)"/bin/hoogle --json '"$@"' >> ./run_hoogle.sh
chmod +x ./run_hoogle.sh
