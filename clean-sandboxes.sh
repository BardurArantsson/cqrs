#!/bin/bash -x
D="$(dirname $0)"

# Make sure we're running in the project's root directory
cd "${D}"

# Remove all sandboxes that have potentially been created by the
# build-*.sh scripts
for d in "${D}" "${D}/cqrs-core" "${D}/cqrs-testkit" "${D}/cqrs-memory" "${D}/cqrs-postgresql" "${D}/cqrs-example"; do
  (cd "${d}" && cabal sandbox delete)
done
