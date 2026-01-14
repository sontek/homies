# Claude Code Docker Sandbox

Docker-based isolation for Claude Code with autonomous workflow orchestration.

## Quick Start

```bash
# Run Claude Code in Docker sandbox
cd ~/code/your-project
claude-jail

# Run autonomous workflow
ralph "implement user authentication"
```

## Prerequisites

- Docker Desktop 4.50+ with `docker sandbox` support
- Claude Code with active subscription (Pro, Max, Team, or Enterprise)
- Skills at `~/code/sontek/agent-skills` (or set `AGENT_SKILLS_PATH`)

## claude-jail: Docker Sandbox Wrapper

Runs Claude Code in Docker Desktop's secure sandbox with project-specific customization.

### Basic Usage

```bash
# Run in current directory
claude-jail

# Force fresh container (picks up env var changes)
claude-jail --fresh

# With custom prompt
claude-jail -p "Your task here"
```

### Features

**Custom Docker Images**

Create `.claude/Dockerfile` in your project to add dependencies:

```dockerfile
FROM docker/sandbox-templates:claude-code

USER root
RUN apt-get update && apt-get install -y build-essential && rm -rf /var/lib/apt/lists/*

# Install Just (command runner)
RUN curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

USER agent

# Install uv (Python package manager)
RUN curl -LsSf https://astral.sh/uv/install.sh | sh

# Copy dependency files and install (cached layer)
COPY --chown=agent:agent pyproject.toml uv.lock /tmp/deps/
WORKDIR /tmp/deps
RUN /home/agent/.local/bin/uv sync --frozen || echo "Warning: uv sync failed (likely needs AWS auth), will retry at runtime"

WORKDIR /home/agent
```

See `docker/claude-templates/` for Python, Node.js, and Golang examples.

`claude-jail` automatically:
- Detects `.claude/Dockerfile`
- Generates hash of dependency files (pyproject.toml, package.json, etc.)
- Builds image tagged as `claude-sandbox-{project}:{hash}`
- Caches image (rebuilds only when dependencies change)

**AWS Credentials**

AWS credentials are automatically mounted and passed:
- `~/.aws` directory mounted (read-write for SSO token refresh)
- Environment variables: `AWS_PROFILE`, `AWS_REGION`, `AWS_DEFAULT_REGION`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`
- `AWS_SDK_LOAD_CONFIG=1` set (required for SSO)
- Container automatically recreated when `AWS_PROFILE` changes

```bash
# Use AWS SSO
AWS_PROFILE=Sandbox-John claude-jail
```

Inside Claude, AWS CLI works:
```bash
aws sts get-caller-identity
just pkg-login  # CodeArtifact authentication
```

**Git Credentials**

Automatically configured from your environment:
- `GIT_AUTHOR_NAME` and `GIT_AUTHOR_EMAIL` passed as env vars
- Falls back to `git config user.name` and `git config user.email`
- SSH keys mounted at `~/.ssh` (read-only)

**GitHub CLI**

`GITHUB_TOKEN` automatically passed:
```bash
gh pr create
gh issue list
```

**Docker Socket**

Docker commands available via `sudo`:
```bash
sudo docker ps
sudo docker-compose up
```

**Skills**

Skills from `$AGENT_SKILLS_PATH` mounted at `/skills` (read-only).

First time: Register marketplace inside Claude:
```
/plugin marketplace add /skills
```

Then restart `claude-jail`. Skills persist in Docker volume.

### Environment Variables

```bash
# AWS credentials
AWS_PROFILE=Sandbox-John         # AWS profile to use
AWS_REGION=us-east-1             # AWS region
AWS_ACCESS_KEY_ID=...            # Direct credentials (alternative to profile)
AWS_SECRET_ACCESS_KEY=...
AWS_SESSION_TOKEN=...

# Git credentials
GIT_AUTHOR_NAME="Your Name"
GIT_AUTHOR_EMAIL="you@example.com"

# GitHub
GITHUB_TOKEN=ghp_...             # GitHub personal access token

# Skills
AGENT_SKILLS_PATH=~/code/sontek/agent-skills
```

### Container Reuse

By default, `claude-jail` reuses the existing container for your workspace. This preserves:
- Claude authentication (no re-login)
- Skills marketplace registration
- Faster startup

Use `--fresh` to force a new container when:
- Environment variables changed (e.g., switching `AWS_PROFILE`)
- Want clean container state

**Note:** Container is automatically recreated when `AWS_PROFILE` changes, so you rarely need `--fresh` manually.

## ralph: Autonomous Workflow

Autonomous workflow orchestration using Claude Code with phase-based execution.

### Usage

```bash
# Basic usage
ralph "implement user authentication"

# From file
ralph tasks/task1.md

# From stdin
echo "fix the login bug" | ralph

# Configuration
RALPH_MAX_ITERATIONS=30 ralph "complex task"
```

### How It Works

Ralph uses **phase-specific prompts** with iterative improvement loops:

```
Phase 1: PLAN          → Create 9-phase implementation plan
Phase 2: IMPLEMENT     → Write code and tests
Phase 3: REVIEW        → Code review, add issues to Phase 4 if found
Phase 4: FIX           → Fix Phase 3 issues
                       ↓ (loops back to Phase 3 until clean)
Phase 5: TEST          → Run tests/linters, add failures to Phase 6
Phase 6: FIX           → Fix Phase 5 failures
                       ↓ (loops back to Phase 5 until passing)
Phase 7: FINAL-REVIEW  → Final review, add issues to Phase 8
Phase 8: FIX           → Fix Phase 7 issues
                       ↓ (loops back to Phase 7 until clean)
Phase 9: COMMIT        → Create git commit
```

**Living Document Plan**

The plan at `.ralph/runs/ralph-YYYYMMDD-HHMMSS/plan.md` evolves during execution:
- Review phases dynamically add fix tasks
- Test phases add failure fix tasks
- Each phase adds re-review/re-test tasks if issues found

**Example Flow:**

```
Iteration 1 - Phase 1: PLAN
  → Creates plan.md with 9 phases
  → Phase complete → advance to Phase 2

Iteration 2 - Phase 2: IMPLEMENT
  → Implements first feature
  → Marks task complete
  → More Phase 2 tasks remain, stay in Phase 2

Iteration 3 - Phase 2: IMPLEMENT
  → Implements second feature
  → All Phase 2 tasks complete → advance to Phase 3

Iteration 4 - Phase 3: REVIEW
  → Reviews code, finds 2 bugs
  → Adds 2 fix tasks to Phase 4
  → Adds re-review task to Phase 3
  → Phase complete → advance to Phase 4

Iteration 5 - Phase 4: FIX
  → Fixes first bug
  → More fixes remain, stay in Phase 4

Iteration 6 - Phase 4: FIX
  → Fixes second bug
  → All Phase 4 tasks complete → advance to Phase 3

Iteration 7 - Phase 3: REVIEW (re-review)
  → Re-reviews code, no issues found
  → Does NOT add re-review task
  → Phase complete → advance to Phase 5

Iteration 8 - Phase 5: TEST
  → Runs pytest, all tests pass
  → Does NOT add re-test task
  → Phase complete → advance to Phase 7

Iteration 9 - Phase 7: FINAL-REVIEW
  → Final review, no issues
  → Phase complete → advance to Phase 9

Iteration 10 - Phase 9: COMMIT
  → Creates git commit
  → Workflow complete
```

### State Management

Ralph tracks state in `.ralph/`:

```
.ralph/
├── state.json              # Current phase, iteration count
├── decisions.md            # Autonomous decisions made
├── audit/                  # Execution logs
└── runs/                   # Per-run artifacts
    └── ralph-20260108-143022/
        ├── plan.md         # Living document plan
        ├── state.json      # Final state
        ├── todos.json      # Claude's task list
        └── decisions.md    # Decisions made
```

### Configuration

```bash
RALPH_MAX_ITERATIONS=30      # Default: 20
RALPH_COST_LIMIT=15.00       # Default: 10.00 USD (not yet implemented)
RALPH_AUTO_APPROVE_PUSH=true # Skip push approval (default: false)
RALPH_CLAUDE_COMMAND=claude  # Use native claude instead of claude-jail (default: claude-jail)
```

**Choosing between `claude` and `claude-jail`:**

- **`claude-jail` (default)**: Runs in Docker sandbox with custom images, AWS SSO, isolated environment
- **`claude`**: Uses native Claude Code, faster startup, no Docker required

```bash
# Use Docker sandbox (default)
ralph "implement feature"

# Use native Claude
RALPH_CLAUDE_COMMAND=claude ralph "implement feature"

# Or set persistently
export RALPH_CLAUDE_COMMAND=claude
ralph "implement feature"
```

### .gitignore

```gitignore
# Ralph artifacts (never commit)
.ralph/

# Claude state (optional - can be useful for collaboration)
.claude/
```

## Custom Dockerfiles

### Template Structure

```dockerfile
# MUST extend official base image
FROM docker/sandbox-templates:claude-code

# Install system packages as root
USER root
RUN apt-get update && apt-get install -y \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install tools as agent
USER agent
RUN curl -LsSf https://astral.sh/uv/install.sh | sh

# Copy ONLY dependency files (for layer caching)
COPY --chown=agent:agent pyproject.toml uv.lock /tmp/deps/
WORKDIR /tmp/deps
RUN /home/agent/.local/bin/uv sync --frozen

# Reset to default workdir
WORKDIR /home/agent

# MUST end as agent user
USER agent
```

### Critical Rules

**DO:**
- ✅ Extend `docker/sandbox-templates:claude-code`
- ✅ Copy only dependency files (not source code)
- ✅ End as `USER agent`
- ✅ Clean up apt cache: `&& rm -rf /var/lib/apt/lists/*`

**DON'T:**
- ❌ Use different base image (breaks Claude Code)
- ❌ Copy source code (it's mounted at runtime)
- ❌ Set `WORKDIR` to project directory
- ❌ Modify Claude Code installation
- ❌ End as root user

### Cache Invalidation

Image rebuilds when these files change:
- `pyproject.toml`, `uv.lock`, `requirements.txt` (Python)
- `package.json`, `package-lock.json` (Node.js)
- `Cargo.toml`, `Cargo.lock` (Rust)
- `go.mod`, `go.sum` (Go)
- `.claude/Dockerfile` itself

### PATH Setup

For tools to be accessible in Claude Code's bash commands:

```dockerfile
# Use ENV PATH directive
ENV PATH="/home/agent/.local/bin:/usr/local/bin:${PATH}"

# Use BASH_ENV for non-interactive shells
RUN echo 'export PATH="/home/agent/.local/bin:/usr/local/bin:$PATH"' > /home/agent/.bash_env
ENV BASH_ENV=/home/agent/.bash_env
```

**Don't use** `.bashrc`, `.profile`, or `/etc/profile.d/` - they're not sourced in non-interactive shells.

## Troubleshooting

### Tools not found in container

If `just --version` fails inside Claude:

1. Check tools are installed in Dockerfile
2. Verify PATH is set via `ENV PATH` and `BASH_ENV`
3. Force container recreation: `claude-jail --fresh`

### AWS credentials not working

```bash
# Check env var is set on host
echo $AWS_PROFILE

# Verify it's passed to container
docker inspect <container-id> | jq '.[0].Config.Env' | grep AWS

# Container recreates automatically when AWS_PROFILE changes
# If not, use: claude-jail --fresh
```

### Container using wrong Docker image

Container was created before custom Dockerfile. Remove it:

```bash
# List sandboxes
docker sandbox ls

# Remove old container
docker sandbox rm <sandbox-id>

# Or use --fresh flag
claude-jail --fresh
```

### Skills not showing

1. Register marketplace (one-time):
   ```
   /plugin marketplace add /skills
   ```

2. Restart claude-jail

3. Verify mount:
   ```bash
   docker exec <container-id> ls -la /skills
   ```

## Shell Helpers

From `dotfiles/zsh/.config/zsh/claude-jail.zsh`:

```bash
cj                    # Alias for claude-jail
docker-status         # Check if Docker Desktop is running
claude-containers     # List Claude sandbox containers
claude-cleanup        # Remove stopped containers
```

## Architecture

**What claude-jail does:**
- Wraps `docker sandbox run` command
- Builds custom images from `.claude/Dockerfile`
- Mounts AWS credentials, SSH keys, skills
- Passes environment variables (AWS, Git, GitHub)
- Auto-recreates when credentials change

**What ralph does:**
- Orchestrates 9-phase workflow with iterative loops
- Uses phase-specific prompts (not one large prompt)
- Manages phase transitions automatically
- Tracks state in `.ralph/state.json`
- Archives each run to `.ralph/runs/`

**Authentication:**
- Claude: OAuth via browser (credentials in Docker volume)
- AWS: SSO or static credentials from `~/.aws`
- Git: SSH keys + environment variables
- GitHub: `GITHUB_TOKEN` environment variable
