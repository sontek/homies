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

# Vim
To update the plugins used in nvim:

```
:PackerUpdate
```

# Fonts
I currently use JetBrainsMono Nerd Font patched from here:

- https://www.nerdfonts.com/font-downloads
