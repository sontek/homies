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

# Claude Jail

`claude-jail` runs Claude Code in a secure Docker Desktop sandbox with project isolation and optional custom environments.

## Quick Start

```bash
# Run Claude Code in sandbox (current directory)
claude-jail

# Or use the alias
cj
```

Claude will run in an isolated Docker container with:
- Your project mounted at current directory
- SSH keys and Git credentials available
- AWS credentials mounted (if `~/.aws` exists)
- Agent skills from `~/code/sontek/agent-skills`
- Development tools from nix/mise automatically mounted

## Custom Docker Environments

For projects needing specific dependencies (Python packages, Node modules, etc.), create `.claude/Dockerfile`:

```bash
# Copy a template to your project
cp ~/code/sontek/homies/docker/claude-templates/python.Dockerfile .claude/Dockerfile

# claude-jail automatically builds and caches it
claude-jail
```

**Available templates:**
- `python.Dockerfile` - Python 3.12 + uv/Poetry + Just + AWS CLI
- `nodejs.Dockerfile` - Node.js 20 + npm/pnpm + AWS CLI
- `golang.Dockerfile` - Go 1.21 + AWS CLI

See [docker/claude-templates/README.md](docker/claude-templates/README.md) for details.

## Ralph Autonomous Workflow

`ralph` orchestrates Claude Code through structured phases automatically:

```bash
ralph "implement user authentication"
```

Ralph workflow:
1. **PLAN** - Analyze requirements and existing code
2. **IMPLEMENT** - Write code and tests
3. **TEST** - Run tests, linters, type checkers (auto-discovers project tools)
4. **REVIEW** - Manual code review for bugs and quality
5. **COMMIT** - Create git commit (auto-skipped if tests/review failed)

**Features:**
- Automatic task retry (skips after 3 failures to prevent infinite loops)
- Phase 5 (COMMIT) auto-skipped if Phase 3 or 4 failed
- Decision logging in `.ralph/decisions.md`
- All artifacts archived in `.ralph/runs/ralph-YYYYMMDD-HHMMSS/`

## Documentation

- [CLAUDE_JAIL_DOCKER.md](CLAUDE_JAIL_DOCKER.md) - Full claude-jail documentation
- [CLAUDE_DOCKERFILE_GUIDE.md](CLAUDE_DOCKERFILE_GUIDE.md) - Custom Docker image guide
- [docker/claude-templates/](docker/claude-templates/) - Reusable templates for common stacks
- [docs/archive/](docs/archive/) - Historical implementation and design docs

## Shell Helpers

```bash
docker-status         # Check if Docker Desktop is running
claude-containers     # List Claude sandbox containers
claude-cleanup        # Remove stopped containers
ralph-status          # Show Ralph state
ralph-log             # Show Ralph audit logs
ralph-clean           # Clean Ralph state/logs
ralph-help            # Show Ralph usage
```
