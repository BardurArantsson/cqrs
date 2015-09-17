#!/bin/bash -xe
D=$(dirname $0)

# Make sure we're running in the project's root directory
cd "${D}"

# Make sure we have a sandbox for all the projects. This will
# overwrite cabal.sandbox.config if it exists.
cabal sandbox init

# Build everything as a single unit. This ensures that all the
# dependencies are semi-consistent across the projects.
cabal install --enable-tests ./cqrs-core ./cqrs-testkit ./cqrs-memory ./cqrs-postgresql ./cqrs-example
