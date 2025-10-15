#!/usr/bin/env bash
set -euo pipefail

# конфіг Homebrew (ARM64)
export PATH="/opt/homebrew/opt/libpq/bin:/opt/homebrew/bin:$PATH"
export PKG_CONFIG_PATH="/opt/homebrew/opt/libpq/lib/pkgconfig:/opt/homebrew/opt/openssl@3/lib/pkgconfig:${PKG_CONFIG_PATH:-}"

# інклюди/ліби для компілятора C
export CPPFLAGS="-I/opt/homebrew/opt/libpq/include -I/opt/homebrew/opt/openssl@3/include"
export LDFLAGS="-L/opt/homebrew/opt/libpq/lib -L/opt/homebrew/opt/openssl@3/lib"
export CPATH="/opt/homebrew/opt/libpq/include:/opt/homebrew/opt/openssl@3/include:${CPATH:-}"
export LIBRARY_PATH="/opt/homebrew/opt/libpq/lib:/opt/homebrew/opt/openssl@3/lib:${LIBRARY_PATH:-}"

# для лінкування під час виконання
export DYLD_LIBRARY_PATH="/opt/homebrew/opt/libpq/lib:/opt/homebrew/opt/openssl@3/lib:${DYLD_LIBRARY_PATH:-}"

cabal clean || true
cabal build -v1

# опційний запуск (./build.sh run)
if [[ "${1-}" == "run" ]]; then
  echo "Запуск"
  cabal run
fi
