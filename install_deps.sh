#!/bin/bash
if [[ $(type stack) ]]; then
    echo "Nothing to be done for setup"
else
    echo "Setting up stack..."
    curl -sSL https://get.haskellstack.org/ | sh
fi
