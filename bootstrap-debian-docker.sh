#!/usr/bin/env bash

# Script dependencies
function install_script_dependencies() {
  if [ "$(grep -Ei 'debian|buntu|mint' /etc/*release)" ]; then
     apt-get update && apt-get install curl xz-utils sudo -y
  fi
}

install_script_dependencies
