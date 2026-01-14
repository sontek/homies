# Claude Docker Sandbox Integration
# Part of homies dotfiles: https://github.com/sontek/homies

# Exports
export AGENT_SKILLS_PATH="${HOME}/code/sontek/agent-skills"
export CLAUDE_PLUGIN_PATH="/skills"

# Add homies bin to PATH if not already there
HOMIES_BIN="${HOME}/code/sontek/homies/bin"
[[ ":$PATH:" != *":${HOMIES_BIN}:"* ]] && export PATH="${HOMIES_BIN}:${PATH}"

# Aliases
alias cj='claude-jail'

# Helper: Check Docker Desktop status
docker-status() {
    if docker info >/dev/null 2>&1; then
        echo "✓ Docker Desktop is running"
        docker version --format '  Version: {{.Server.Version}}'
    else
        echo "✗ Docker Desktop is not running"
        echo "  Start Docker Desktop to use Claude sandbox"
    fi
}

# Helper: Show Claude sandbox containers
claude-containers() {
    echo "Claude sandbox containers:"
    docker ps -a --filter ancestor=docker/sandbox-templates:claude-code --format "table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.CreatedAt}}"
}

# Helper: Clean up stopped Claude containers
claude-cleanup() {
    local count=$(docker ps -a --filter ancestor=docker/sandbox-templates:claude-code --filter status=exited -q | wc -l | tr -d ' ')

    if [ "$count" -eq 0 ]; then
        echo "No stopped Claude containers to clean up"
        return 0
    fi

    echo "Found $count stopped Claude container(s)"
    read -q "REPLY?Remove them? (y/n) "
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        docker container prune -f --filter ancestor=docker/sandbox-templates:claude-code
        echo "✓ Cleaned up stopped containers"
    else
        echo "Cancelled"
    fi
}

# Ralph helpers
ralph-status() {
    if [ -f ".claude/ralph-state.json" ]; then
        echo "Ralph state:"
        cat .claude/ralph-state.json | jq '{
            total_iterations,
            cost_usd,
            started_at,
            updated_at,
            prompt: (.prompt | if length > 100 then .[:100] + "..." else . end)
        }'
    else
        echo "No Ralph state found in current directory"
        echo "Run ralph first: ralph \"your prompt\""
    fi
}

ralph-log() {
    if [ -d ".claude/audit" ]; then
        echo "Recent Ralph audit logs:"
        ls -lt .claude/audit/ | head -10
    else
        echo "No audit logs found"
    fi
}

ralph-clean() {
    if [ ! -f ".claude/ralph-state.json" ] && [ ! -d ".claude/audit" ]; then
        echo "No Ralph state or logs to clean"
        return 0
    fi

    echo "This will delete:"
    [ -f ".claude/ralph-state.json" ] && echo "  - .claude/ralph-state.json"
    [ -d ".claude/audit" ] && echo "  - .claude/audit/ ($(ls .claude/audit/ 2>/dev/null | wc -l | tr -d ' ') files)"

    read -q "REPLY?Continue? (y/n) "
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -f .claude/ralph-state.json
        rm -rf .claude/audit/
        echo "✓ Cleaned Ralph state and logs"
    else
        echo "Cancelled"
    fi
}

# Helper: Show Ralph help
ralph-help() {
    cat <<'EOF'
Ralph - Autonomous Claude Code workflow

Usage:
  ralph "your prompt"              # Run autonomous workflow
  ralph tasks/task.md              # Use prompt from file
  echo "task" | ralph              # Read from stdin
  ralph --resume                   # Resume from checkpoint

Environment variables:
  RALPH_MAX_ITERATIONS=20          # Max loop iterations (default: 20)
  RALPH_COST_LIMIT=10.00           # Max cost in USD (default: 10.00)
  RALPH_AUTO_APPROVE_PUSH=false    # Skip push approval (default: false)
  RALPH_CHECKPOINT_DIR=.claude     # State directory
  RALPH_AUDIT_DIR=.claude/audit    # Audit logs

Examples:
  ralph "Add user authentication"
  ralph docs/implementation.md
  RALPH_MAX_ITERATIONS=30 ralph "complex task"

Helper commands:
  ralph-status    # Show current Ralph state
  ralph-log       # Show audit logs
  ralph-clean     # Clean up state and logs
  ralph-help      # Show this help
EOF
}
