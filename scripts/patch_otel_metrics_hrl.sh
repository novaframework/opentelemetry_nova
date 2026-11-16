#!/bin/sh
# opentelemetry_experimental 0.6.0 (the latest release) types a record field
# in include/otel_metrics.hrl as eqwalizer:dynamic() without depending on
# eqwalizer_support, so Dialyzer reports an unknown type in every module that
# includes the header. Upstream replaced it with dynamic() after the release
# (open-telemetry/opentelemetry-erlang#1036). Until that is published, this
# makes the same replacement in this repo's own _build.
#
# It fails when the locked version is not 0.6.0 and the header no longer
# names the type: the fix has been released and this script and its
# pre_hook in rebar.config must be removed.
set -eu

header=_build/default/lib/opentelemetry_experimental/include/otel_metrics.hrl
patched_version='{pkg,<<"opentelemetry_experimental">>,<<"0.6.0">>}'

# Built as a dependency: the header is not in this directory's _build.
[ -f "$header" ] || exit 0

if grep -q 'eqwalizer:dynamic()' "$header"; then
    sed 's/eqwalizer:dynamic()/dynamic()/g' "$header" > "$header.tmp"
    mv "$header.tmp" "$header"
elif ! grep -qF "$patched_version" rebar.lock 2>/dev/null; then
    echo "opentelemetry_nova: $header no longer names eqwalizer:dynamic() and" >&2
    echo "rebar.lock does not pin opentelemetry_experimental 0.6.0. Remove" >&2
    echo "scripts/patch_otel_metrics_hrl.sh and its pre_hook from rebar.config." >&2
    exit 1
fi
