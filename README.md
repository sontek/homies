# Introduction
This is the repository that has all the scripts for
bootstrapping a development environment on my computers.

- `bootstrap.sh` - This will install the bare minimum
                   necessary for doing the provisioning
- `justfile` - This is the main script for defining all
               the available options. `just help`

# Bootstrapping
We need to get some base tooling installed so that the
rest of the tooling can depend on it, so the first step
is to run `./bootstrap.sh`

# Installing
To get everything installed and provisioned you can run:

```
just install
```

# Mac screenshots on a Linux box over SSH
When I ssh from a Mac into a Linux box, the Linux box mounts the Mac's
screenshots folder at the same path, so a screenshot dragged into the
terminal pastes a path (`/Users/<you>/screenshots/...`) that works there too.
The mount comes up automatically on each SSH login; the configs are in
`dotfiles/ssh/.ssh/config` (`RemoteForward`, `SendEnv`, `mac-tunnel`) and
`dotfiles/zsh/.config/zsh/.zshrc`.

One-time setup on each Mac:

```
# System Settings → General → Sharing → turn on Remote Login, then:
mkdir -p ~/screenshots
defaults write com.apple.screencapture location ~/screenshots
# Without the floating thumbnail, a drag gives the real file, not a temp copy
defaults write com.apple.screencapture show-thumbnail -bool false
killall SystemUIServer

# Let the shared key log in, but only through the tunnel
echo "from=\"127.0.0.1,::1\" $(cat ~/.ssh/id_ed25519.pub)" >> ~/.ssh/authorized_keys
```

To use a folder other than `~/screenshots`, export `MAC_SCREENSHOTS` on the
Mac before `.zshrc` runs. Use a folder outside `~/Desktop` and `~/Documents`:
macOS blocks `sshd` from those without Full Disk Access.

One-time setup on each Linux box (`<you>` is the Mac username):

```
sudo apt install -y sshfs
sudo mkdir -p /Users/<you>/screenshots
sudo chown -R "$USER": /Users
```

Add the Linux box's name to the `Host` line carrying `RemoteForward` in the SSH
config. If the mount doesn't appear, check
`~/.cache/mac-screenshots-mount.log` on the Linux box.

# Vim
To update the plugins used in nvim:

```
:PackerUpdate
```

# Fonts
I currently use JetBrainsMono Nerd Font patched from here:

- https://www.nerdfonts.com/font-downloads
