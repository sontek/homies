#!/usr/bin/env bash
# Repair Nix multi-user build users on macOS.
#
# After a macOS upgrade, the OS can reclaim low UIDs that Nix's build users used
# (e.g. _nixbld1..7 sat at UIDs 301..307, which macOS later handed to its own
# daemons like _modelmanagerd). The `nixbld` group still lists every _nixbldN,
# but some of those users no longer exist, so any from-source Nix build fails:
#     error: the user '_nixbld1' in the group 'nixbld' does not exist
#
# This recreates each missing _nixbld user at a FREE UID and restarts the daemon.
# Idempotent: existing users are left untouched. Safe to re-run after the next
# macOS upgrade if it happens again.
set -euo pipefail

# Re-exec under sudo if not root (one password prompt).
if [ "$(id -u)" -ne 0 ]; then
  echo "elevating with sudo..."
  exec sudo "$0" "$@"
fi

GROUP="nixbld"
GID="$(dscl . -read "/Groups/$GROUP" PrimaryGroupID 2>/dev/null | awk '{print $2}')"
if [ -z "$GID" ]; then
  echo "error: group '$GROUP' not found — is multi-user Nix installed?" >&2
  exit 1
fi

# Expected build users = whatever the group lists (e.g. _nixbld1.._nixbld32).
members="$(dscl . -read "/Groups/$GROUP" GroupMembership 2>/dev/null \
  | tr ' ' '\n' | grep -E '^_nixbld[0-9]+$' | sort -u)"
if [ -z "$members" ]; then
  echo "error: no _nixbld members listed in group '$GROUP'" >&2
  exit 1
fi

# Start scanning for free UIDs just above the highest existing _nixbld UID.
max_uid=0
for u in $members; do
  # `|| uid=""` so a missing user's failed read doesn't trip `set -e`.
  uid="$(dscl . -read "/Users/$u" UniqueID 2>/dev/null | awk '{print $2}')" || uid=""
  if [ -n "$uid" ] && [ "$uid" -gt "$max_uid" ]; then
    max_uid="$uid"
  fi
done
next=$(( max_uid > 0 ? max_uid + 1 : 350 ))

created=0
for u in $members; do
  if dscl . -read "/Users/$u" >/dev/null 2>&1; then
    continue   # exists — leave it alone
  fi
  # advance to the next genuinely-free UID
  while dscl . -search /Users UniqueID "$next" 2>/dev/null | grep -q .; do
    next=$(( next + 1 ))
  done
  uid="$next"; next=$(( next + 1 ))
  n="${u#_nixbld}"
  echo "creating $u (UID $uid)"
  dscl . -create "/Users/$u" UniqueID "$uid"
  dscl . -create "/Users/$u" PrimaryGroupID "$GID"
  dscl . -create "/Users/$u" NFSHomeDirectory /var/empty
  dscl . -create "/Users/$u" UserShell /usr/bin/false
  dscl . -create "/Users/$u" RealName "Nix build user $n"
  created=$(( created + 1 ))
done

echo "recreated $created build user(s) (gid $GID)."

# Restart the daemon so it picks up the new users (label varies by installer,
# so detect it instead of guessing).
label="$(launchctl list 2>/dev/null | awk '/nix-daemon/ {print $3; exit}')"
if [ -n "$label" ]; then
  echo "restarting system/$label"
  launchctl kickstart -k "system/$label" || true
else
  echo "note: nix-daemon launchd label not found — if builds still fail, reboot."
fi

echo "done."
