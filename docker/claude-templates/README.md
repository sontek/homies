# Claude Sandbox Docker Templates

Reusable Docker templates for common project stacks with Claude Code and Ralph.

## Available Templates

| Template | Includes | Use Case |
|----------|----------|----------|
| `python.Dockerfile` | Python 3.12, uv/Poetry, Just, AWS CLI | Python projects (choose uv or Poetry) |
| `nodejs.Dockerfile` | Node.js 20, npm, pnpm, AWS CLI | JavaScript/TypeScript projects |
| `golang.Dockerfile` | Go 1.21, AWS CLI | Go projects |

## Quick Start

1. **Copy template to your project:**
   ```bash
   cp ~/code/sontek/homies/docker/claude-templates/python.Dockerfile .claude/Dockerfile
   ```

2. **Customize if needed:**
   ```dockerfile
   # Edit .claude/Dockerfile to add project-specific dependencies
   RUN apt-get install -y libpq-dev  # Example: PostgreSQL client
   ```

3. **Run claude-jail or ralph:**
   ```bash
   claude-jail  # Automatically builds and caches custom image
   ```

## What's Included in All Templates

- **Base**: Official `docker/sandbox-templates:claude-code`
- **AWS CLI v2**: For CodeArtifact and AWS services
- **Layer caching**: Dependencies cached separately from source code
- **Smart rebuilds**: Only rebuilds when dependency files change

## Template Details

### Python Template

**python.Dockerfile:**
- Python 3.12
- **Option 1 (default):** uv (fast Python package manager)
- **Option 2 (commented):** Poetry (traditional dependency manager)
- Just (command runner)
- AWS CLI v2
- Installs from `pyproject.toml` + `uv.lock` or `poetry.lock`

Choose uv or Poetry by uncommenting the appropriate section.

### Node.js Template

**nodejs.Dockerfile:**
- Node.js 20 LTS
- npm and pnpm package managers
- AWS CLI v2
- Installs from `package.json` and `package-lock.json`

### Go Template

**golang.Dockerfile:**
- Go 1.21
- AWS CLI v2
- Downloads modules from `go.mod` and `go.sum`

## Customizing Templates

### Adding System Packages

```dockerfile
USER root
RUN apt-get update && apt-get install -y \
    postgresql-client \
    redis-tools \
    && rm -rf /var/lib/apt/lists/*
USER agent
```

### Adding Python Tools

```dockerfile
USER agent
RUN pip install --user ruff mypy black
```

### Adding Node.js Global Packages

```dockerfile
USER agent
RUN npm install -g typescript tsx
```

## How It Works

1. **Automatic detection**: `claude-jail` checks for `.claude/Dockerfile`
2. **Hash-based caching**: Generates hash from dependency files
3. **Smart rebuilds**: Only rebuilds when dependencies change
4. **Fast startups**: Cached images load instantly

**Dependency files watched:**
- Python: `pyproject.toml`, `uv.lock`, `requirements.txt`
- Node: `package.json`, `package-lock.json`
- Rust: `Cargo.toml`, `Cargo.lock`
- Go: `go.mod`, `go.sum`

## Benefits

### Fast Iteration
- First build: ~1-2 minutes
- Cached runs: Instant
- Dependency changes: ~30 seconds (layer cache)

### Clean Isolation
- No global package pollution
- Each project has its own environment
- Source code always up-to-date (mounted at runtime)

### Works with Ralph
- Phase 3 (TEST) can run tests, linters, type checkers
- AWS credentials automatically mounted
- No "missing dependency" errors

## See Also

- [CLAUDE_JAIL_DOCKER.md](../../CLAUDE_JAIL_DOCKER.md) - Main documentation
- [CLAUDE_DOCKERFILE_GUIDE.md](../../CLAUDE_DOCKERFILE_GUIDE.md) - Advanced customization
