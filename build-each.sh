#!/bin/bash -xe
D="$(dirname $0)"

build_it() {
    local DEPS="$*"
    cabal sandbox init
    for DEP in $DEPS; do
        cabal sandbox add-source "../${DEP}"
    done
    cabal install --enable-tests --dependencies-only
    cabal configure
    cabal build
}

build() {
    # Extract parameters
    local NAME=$1
    shift
    local DEPS="$@"
    # Build in the sub-project directory
    (cd "${D}/${NAME}" && build_it "$DEPS")
}

# Build all the packages
build cqrs-core
build cqrs-testkit    cqrs-core
build cqrs-memory     cqrs-core cqrs-testkit
build cqrs-postgresql cqrs-core cqrs-testkit
build cqrs-example    cqrs-core cqrs-memory
