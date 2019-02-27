#!/bin/bash
if [[ $(type stack) ]]; then
    echo "Nothing to be done for setup"
else
    echo "Setting up stack..."
    wget -qO- https://get.haskellstack.org/ | sh
fi
