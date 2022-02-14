#!/usr/bin/env bash
echo "Bootstrapping the homies!"


# We need Nix to download all the applications we need
function install_nix() {
    echo "Downloading the nix installer..."
    curl -s -L --output /tmp/nix_installer.sh https://nixos.org/nix/install
    echo "Installing nix..."
    chmod +x /tmp/nix_installer.sh
    # We use single user mode for debian since it is supported
    if [ "$(grep -Ei 'debian|buntu|mint' /etc/*release)" ]; then
       yes | /tmp/nix_installer.sh --no-daemon
    else
       yes | /tmp/nix_installer.sh --daemon
    fi
    echo "Activating the Nix environment"

    nix_env_path1=/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    if [ -f "$nix_env_path1" ]; then
        source $nix_env_path1
    fi

    nix_env_path2=~/.nix-profile/etc/profile.d/nix.sh
    if [ -f "$nix_env_path2" ]; then
        source $nix_env_path2
    fi
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
}

function done_bootstrapping() {
    echo "Completed bootstrap!"
    echo "Now you can run `just` to install the rest"
}

install_nix
configure_nix
install_dependencies
