#!/bin/bash -xe
D="$(dirname $0)"

# Make sure we're running in the project's root directory
cd "${D}"

# Remove all the dist folders the have potentially been created by the
# build-*.sh scripts
for d in "${D}/cqrs-core" "${D}/cqrs-testkit" "${D}/cqrs-memory" "${D}/cqrs-postgresql" "${D}/cqrs-example"; do
  rm -rf ${d}/dist
done
