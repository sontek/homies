#!/usr/bin/env bash
# aoe-lock.sh — serialize expensive commands across aoe worktrees with flock.
#
# Registered as a PreToolUse(Bash) hook. When a Bash command matches a "lane"
# pattern below, the hook rewrites it to run under an flock mutex so that only
# one job per lane runs at a time on this host. flock holds the lock for the
# lifetime of the wrapped process and the kernel releases it on exit/crash/kill,
# so there is no stale-lock cleanup. A queued command blocks until the lock is
# free; in practice the agent's own command timeout is the upper bound on the wait.
#
# Lanes are independent: a test and a deploy can run concurrently, but two tests
# (or two deploys) queue behind each other. Edit the *_RE patterns to tune what
# gets serialized. To limit this to aoe sessions only, gate on $AOE_INSTANCE_ID
# near the top (see note).
#
# Fail-open by design: if anything is missing or unexpected, the command passes
# through unchanged (the hook prints nothing and exits 0).

input=$(cat)

emit_passthrough() { exit 0; }  # no output => Claude runs the original command

# Only act on Bash tool calls.
command -v jq >/dev/null 2>&1 || emit_passthrough
tool=$(printf '%s' "$input" | jq -r '.tool_name // empty')
[ "$tool" = "Bash" ] || emit_passthrough

# Need flock; without it we cannot lock, so pass through untouched.
command -v flock >/dev/null 2>&1 || emit_passthrough

cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // empty')
[ -n "$cmd" ] || emit_passthrough

# To restrict locking to aoe sessions only, uncomment:
#   [ -n "${AOE_INSTANCE_ID:-}" ] || emit_passthrough

LOCK_DIR="/tmp/aoe-locks"

# Idempotency: never double-wrap a command we already rewrote.
case "$cmd" in
  *"$LOCK_DIR"*) emit_passthrough ;;
esac

# --- Lane patterns (extended regex, case-insensitive). Tune these. -------------
# Test lane: any pytest invocation (incl. `python -m pytest`, `uv run pytest`),
# or a `just test` / `just tests` recipe.
TEST_RE='(^|[^[:alnum:]_.-])(pytest|py\.test)([^[:alnum:]_]|$)|(^|[^[:alnum:]_.-])just([[:space:]]+[^&;|]*)?[[:space:]]+tests?([^[:alnum:]_]|$)'
# Deploy lane: scaffolded but intentionally empty for now. Add patterns later,
# e.g. DEPLOY_RE='terraform[[:space:]]+apply|(^|[[:space:]])just[[:space:]]+deploy'
DEPLOY_RE=''
# ------------------------------------------------------------------------------

lane=""
if printf '%s' "$cmd" | grep -Eiq "$TEST_RE"; then
  lane="pytest"
elif [ -n "$DEPLOY_RE" ] && printf '%s' "$cmd" | grep -Eiq "$DEPLOY_RE"; then
  lane="deploy"
fi

[ -n "$lane" ] || emit_passthrough   # not an expensive command

mkdir -p "$LOCK_DIR" 2>/dev/null || emit_passthrough
lockfile="$LOCK_DIR/$lane.lock"

# Wrap the original so flock owns it for its whole lifetime; %q shell-quotes it for bash -c re-exec.
# (%q emits bash ANSI-C $'...' quoting, so the rewrite assumes a bash-compatible executor — Claude's is.)
wrapped="flock -x $lockfile bash -c $(printf '%q' "$cmd")"

# Rewrite the tool input. updatedInput must restate the full tool_input object.
printf '%s' "$input" | jq --arg c "$wrapped" '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    permissionDecision: "allow",
    updatedInput: (.tool_input + { command: $c })
  }
}'
