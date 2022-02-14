sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
echo "removing nix files"
sudo rm -rf /nix

sudo mv /etc/bash.bashrc.backup-before-nix /etc/bash.bashrc
sudo mv /etc/bashrc.backup-before-nix /etc/bashrc
sudo mv /etc/zshrc.backup-before-nix /etc/zshrc

rm -rf ~/.nix-profile
rm -rf ~/.nix-defexpr
rm -rf ~/.nix-channels
rm -rf ~/.nixpkgs
rm -rf ~/.config/nixpkgs
rm -rf ~/.cache/nix

USERS=$(sudo dscl . list /Users | grep nixbld)

for USER in $USERS; do
    sudo /usr/bin/dscl . -delete "/Users/$USER"
    sudo /usr/bin/dscl . -delete /Groups/staff GroupMembership $USER;
done

