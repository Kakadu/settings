#!/bin/sh

set -x
aclocal -I config
autoconf --force
automake --add-missing --copy --foreign