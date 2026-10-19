#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

check() {
    name=$1
    user=$2
    stat "$name" > /dev/null && cd "$name" && git pull && cd .. \
	    || git clone https://github.com/$user/$name.git
}

cd "$SCRIPT_DIR"

check "JSON-Schema-Test-Suite" "json-schema-org"
check "jmespath.test" "jmespath"
check "JSONTestSuite" "nst"

