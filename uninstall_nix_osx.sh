echo "Deleting all Nix installed packages"
nix-env -e '.*'
nix profile remove '.*'

echo "Running nix GC"
nix profile wipe-history
nix-collect-garbage --delete-old
nix store gc
nix store optimise

if grep -R "nix" /etc/synthetic.conf
then
    sudo sed -i '' "s/nix//g" /etc/synthetic.conf
    echo "Removed nix from /etc/synthetic.conf"
    echo "/nix will be read-only until a reboot"
fi

echo "Unloading the daemon"

if [ -f /Library/LaunchDaemons/org.nixos.nix-daemon.plist ]; then
    sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
    sudo rm /Library/LaunchDaemons/org.nixos.nix-daemon.plist
fi

if [ -f /etc/profile.backup-before-nix ]; then
    sudo mv /etc/profile.backup-before-nix /etc/profile
fi

if [ -f /etc/bashrc.backup-before-nix ]; then
    sudo mv /etc/bashrc.backup-before-nix /etc/bashrc
fi

if [ -f /etc/zshrc.backup-before-nix ]; then
    sudo mv /etc/zshrc.backup-before-nix /etc/zshrc
fi

echo "Checking if /etc/fstab has been cleaned up"
if grep -R "/nix" /etc/fstab
then
    echo "You still have a /nix entry in /etc/fstab"
    echo "Please remove it with 'sudo vifs'"
    echo "You will have to reboot after this step"
    exit 1
fi

echo "Checking if the disk was removed"

if diskutil apfs list|grep "Nix Store" -B 3
then
    echo "You still have a Nix Store volumn in 'diskutil apfs list'"
    echo "Please remove it with 'diskutil apfs deleteVolume <volumeDevice>'"
    UUID=$(diskutil apfs list|grep "Nix Store" -B 3|head -n1|awk '{print $4}')
    echo "> suggested: diskutil apfs deleteVolume $UUID"
    exit 1
fi


rm -rf $HOME/.nix-*
rm -rf $HOME/.config/nixpkgs
rm -rf $HOME/.cache/nix
rm -rf $HOME/.nixpkgs

if [ -L $HOME/Applications ]; then
  rm $HOME/Applications
fi

sudo rm -rf /etc/nix /nix


USERS=$(sudo dscl . list /Users | grep nixbld)

for USER in $USERS; do
    sudo /usr/bin/dscl . -delete "/Users/$USER"
    sudo /usr/bin/dscl . -delete /Groups/staff GroupMembership $USER;
done

sudo /usr/bin/dscl . -delete "/Groups/nixbld"
sudo rm -rf /var/root/.nix-*
sudo rm -rf /var/root/.cache/nix

