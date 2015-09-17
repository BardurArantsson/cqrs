#!/bin/bash -xe
D="$(dirname $0)"

# Helper to initialize and perform a build in $CWD
build() {
    cabal sandbox init
    for src in $*; do
        cabal sandbox add-source "$src"
    done
    cabal install --enable-tests --dependencies-only
    cabal configure
    cabal build
}

# Build the 'core' package
(cd "${D}/cqrs-core" && build)

# Build the 'testkit' package
(cd "${D}/cqrs-testkit" && build "../cqrs-core")

# Build the 'memory' package
(cd "${D}/cqrs-memory" && build "../cqrs-core" "../cqrs-testkit")

# Build the 'postgresql' package
(cd "${D}/cqrs-postgresql" && build "../cqrs-core" "../cqrs-testkit")

# Build the 'example' package
(cd "${D}/cqrs-example" && build "../cqrs-core" "../cqrs-memory")
