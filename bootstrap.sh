#!/usr/bin/env bash
echo "Bootstrapping the homies!"


# We need Nix to download all the applications we need
function install_nix() {
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
}

# Configure Nix so we can use experimental features
function configure_nix() {
    echo "Configuring nix"
    NIX_CONFIG_PATH=$HOME/.config/nix
    if [ ! -d "$NIX_CONFIG_PATH" ]; then
        echo "$NIX_CONFIG_PATH doesn't exist, creating it."
        mkdir -p $NIX_CONFIG_PATH
    fi
    touch $NIX_CONFIG_PATH/nix.conf
    CONFIG_STRING="experimental-features = nix-command flakes"
    if grep -R "$CONFIG_STRING" $NIX_CONFIG_PATH/nix.conf;then
        echo "Experimental features already enabled"
    else
        echo "Enabling experimental features"
        echo "$CONFIG_STRING" >> $NIX_CONFIG_PATH/nix.conf
    fi
}

function install_dependencies() {
    echo "Installing dependencies"
    nix profile install nixpkgs#just
    nix profile install nixpkgs#ripgrep

}

function done_bootstrapping() {
    echo "Completed bootstrap!"
    echo "Now you can run `just` to install the rest"
}

install_nix
configure_nix
install_dependencies
