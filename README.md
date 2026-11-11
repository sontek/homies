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

# Mac screenshots on a Linux box
When I ssh from a Mac into a Linux box, a screenshot dragged into the terminal
pastes a Mac path (`/Users/<you>/screenshots/...`). To make that path work on
the Linux box too, Mutagen keeps a copy of the Mac's screenshots folder there,
at the same path. Sync runs one way, Mac to box, over ordinary ssh to the box,
so the Mac doesn't need Remote Login.

Point macOS screenshots at the folder, once per Mac:

```
mkdir -p ~/screenshots
defaults write com.apple.screencapture location ~/screenshots
# Without the floating thumbnail, a drag gives the real file, not a temp copy
defaults write com.apple.screencapture show-thumbnail -bool false
killall SystemUIServer
```

On the Linux box, create the same path once (`<you>` is the Mac username):

```
sudo mkdir -p /Users/<you>/screenshots
sudo chown -R "$USER": /Users
```

Then start the sync from the Mac, naming the box as you would to ssh:

```
just sync-screenshots <host>
just sync-screenshots <host> ~/some/other/folder   # a folder other than ~/screenshots
```

It runs in the background and resumes after a reboot. Check it with
`mutagen sync list`, and stop it with `mutagen sync terminate screenshots-<host>`.

# Vim
To update the plugins used in nvim:

```
:PackerUpdate
```

# Fonts
I currently use JetBrainsMono Nerd Font patched from here:

- https://www.nerdfonts.com/font-downloads
