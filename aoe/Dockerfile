# Slim, Claude-only developer-machine image for Agent of Empires sandboxes.
#
# WHY NOT `FROM aoe-sandbox`: the AOE base bundles ~12 agent CLIs (Hermes alone is
# 2.85GB) plus a Rust/bun toolchain — ~8.7GB before we add anything, and you can't
# `rm` it back out (union FS keeps the parent's bytes). This image instead starts
# from clean Ubuntu and reproduces ONLY the Claude pieces AOE needs, cutting ~7GB.
#
# AOE execs the agent by bare binary name (src/cockpit/agent_registry.rs):
#   tmux mode    -> `claude`             (Claude Code CLI)
#   cockpit/ACP  -> `claude-agent-acp`   (Zed adapter, npm @agentclientprotocol/...)
# Both are installed below. SELECTING ANY OTHER AGENT IN AOE WILL FAIL in this
# image — that's the deliberate trade for the size cut. The ACP adapter is pinned
# to the major AOE's cockpit expects; rebuild when AOE bumps it.
#
# AOE overrides the entrypoint to `sleep infinity` and runs everything via
# `docker exec` (no guaranteed login shell), so tool PATH comes from ENV/profile.d
# below, not a login shell. Pin a digest in production.
FROM ubuntu:26.04
ENV DEBIAN_FRONTEND=noninteractive

# --- One apt layer: base OS tooling + build deps + docker CLI + psql + the
# Chromium runtime libs Playwright needs (the 26.04 t64 set, verified via ldd).
# docker.io/docker-compose-v2 give the CLI only; it drives the host daemon over
# the mounted socket (see config.example.toml). Acquire::Retries rides out the
# occasional archive.ubuntu.com index hiccup.
RUN apt-get update -o Acquire::Retries=3 && apt-get install -y --no-install-recommends \
      ca-certificates curl git gnupg unzip xz-utils \
      build-essential pkg-config libssl-dev zlib1g-dev \
      jq fzf ripgrep openssh-client tmux \
      postgresql-client docker.io docker-compose-v2 \
      libxcomposite1 libxdamage1 libxfixes3 libxrandr2 libxkbcommon0 \
      libasound2t64 libatk1.0-0t64 libatk-bridge2.0-0t64 libatspi2.0-0t64 \
      libcairo2 libcups2t64 libdbus-1-3 libgbm1 libglib2.0-0t64 \
      libnspr4 libnss3 libpango-1.0-0 \
 && rm -rf /var/lib/apt/lists/*

# --- Node (system): the Claude ACP adapter + the npx-driven diagram skills run on
# it. nodesource 22, matching what aoe-sandbox shipped; mise node@24 layers on top
# for project work (mise shims take PATH precedence; the adapter still resolves).
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - \
 && apt-get install -y nodejs \
 && rm -rf /var/lib/apt/lists/*

# --- Claude: CLI (`claude`, standalone binary) + ACP adapter (`claude-agent-acp`).
# IS_SANDBOX=1 lets Claude Code use --dangerously-skip-permissions as root, and
# the dirs are where AOE seeds mounted credentials.
RUN curl -fsSL https://claude.ai/install.sh | bash
ENV PATH="/root/.local/bin:${PATH}"
RUN npm install -g @agentclientprotocol/claude-agent-acp@^0.39.0 \
 && mkdir -p /root/.claude /root/.ssh
ENV IS_SANDBOX=1

# --- Git: rewrite GitHub SSH remotes -> HTTPS so AOE's GH_TOKEN credential helper
# authenticates push/pull without an SSH key in the sandbox. System-level
# (/etc/gitconfig) so it applies to BOTH Claude's git (which uses
# GIT_CONFIG_GLOBAL=~/.sandbox-gitconfig) and plain terminal git; the system file
# is always read regardless of GIT_CONFIG_GLOBAL, and layers with AOE's seeded
# token helper. Forward the token via `[sandbox] environment = ["GH_TOKEN"]`.
RUN git config --system url."https://github.com/".insteadOf "git@github.com:" \
 && git config --system --add url."https://github.com/".insteadOf "ssh://git@github.com/"

# --- dev-sandbox tools you actually use, re-added (gh, uv+uvx, pnpm) ---
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
      -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
 && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
 && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
      > /etc/apt/sources.list.d/github-cli.list \
 && apt-get update && apt-get install -y gh && rm -rf /var/lib/apt/lists/* \
 && curl -LsSf https://astral.sh/uv/install.sh | sh \
 && npm install -g pnpm

# --- AWS CLI v2 (self-contained installer; not a single managed binary) ---
RUN curl -fsSL "https://awscli.amazonaws.com/awscli-exe-linux-$(uname -m).zip" -o /tmp/awscliv2.zip \
 && unzip -q /tmp/awscliv2.zip -d /tmp \
 && /tmp/aws/install \
 && rm -rf /tmp/aws /tmp/awscliv2.zip

# --- mise: owns the version-managed toolchain per repo mise.toml ---
RUN curl -fsSL https://mise.run | sh \
 && ln -sf /root/.local/bin/mise /usr/local/bin/mise

# mise SHIMS on PATH (not `mise activate`): resolve tools in non-interactive
# `docker exec` shells with no per-shell hook. Shims dir first so repo-pinned
# versions win; system node/uv stay as fallback.
ENV PATH="/root/.local/share/mise/shims:/root/.local/bin:${PATH}"

# Ship sane global defaults (latest stable as of 2026-06). Repos override
# per-project via their own mise.toml; `mise install` (the on_create hook) fetches
# whatever they pin. node@24 is backward-compatible with the node22 the Claude
# adapter runs under, so the shim shadowing it is safe.
RUN mise use -g \
      python@3.14 \
      node@24 \
      terraform \
 && mise reshim \
 && mise ls

# --- CLIs we always want at latest, installed directly (not version-pinned) ---
# ast-grep (+ sg), crane, tilt, just, and sentry-cli (hard dep of the `sentry`
# skill). NOTE: no inline `#` comments inside the RUN below — backslash line
# continuation joins it into one shell line, so a `#` would eat the rest.
RUN set -eux; \
    ARCH="$(uname -m)"; \
    curl -fsSL "https://github.com/ast-grep/ast-grep/releases/latest/download/app-${ARCH}-unknown-linux-gnu.zip" -o /tmp/sg.zip; \
    unzip -o /tmp/sg.zip -d /usr/local/bin; \
    rm /tmp/sg.zip; \
    case "$ARCH" in x86_64) CRANE_ARCH=x86_64 ;; aarch64) CRANE_ARCH=arm64 ;; *) CRANE_ARCH="$ARCH" ;; esac; \
    curl -fsSL "https://github.com/google/go-containerregistry/releases/latest/download/go-containerregistry_Linux_${CRANE_ARCH}.tar.gz" -o /tmp/crane.tgz; \
    tar -xzf /tmp/crane.tgz -C /usr/local/bin crane; \
    rm /tmp/crane.tgz; \
    curl -fsSL https://raw.githubusercontent.com/tilt-dev/tilt/master/scripts/install.sh | bash; \
    curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin; \
    curl -sL https://sentry.io/get-cli/ | sh

# For interactive terminals, also activate mise.
RUN printf 'eval "$(/usr/local/bin/mise activate bash)"\n' > /etc/profile.d/zz-mise.sh \
 && chmod +x /etc/profile.d/zz-mise.sh

# --- Playwright (e2e) + headless Chromium, shared with the Mermaid skills ---
# Ubuntu 26.04 is too new for Playwright's platform map, so `--with-deps` refuses
# to pick a browser build or install deps. Two-part workaround that builds + runs:
#   1. PLAYWRIGHT_HOST_PLATFORM_OVERRIDE pins it to the (forward-compatible) 24.04
#      build. arm64 default; for an amd64 build pass
#      --build-arg PLAYWRIGHT_PLATFORM=ubuntu24.04.
#   2. The chromium runtime libs are already apt-installed above (the 26.04 t64
#      set), and PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=1 stops install/launch
#      from throwing on the unrecognized OS.
# A repo pinning a different @playwright/test version still works: it fetches its
# matching browser into the same dir (no root; deps + override already in place).
# The symlink gives Puppeteer (draw-mermaid-diagram / draw-infra-diagram) a stable
# chromium so it skips its own ~100MB download. `npx` resolves via the mise
# node@24 shims on PATH above.
ARG PLAYWRIGHT_PLATFORM=ubuntu24.04-arm64
ENV PLAYWRIGHT_BROWSERS_PATH=/ms-playwright \
    PLAYWRIGHT_HOST_PLATFORM_OVERRIDE=${PLAYWRIGHT_PLATFORM} \
    PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=1
RUN npx -y playwright@latest install chromium \
 && ln -sf "$(ls -d /ms-playwright/chromium-*/chrome-linux/chrome | head -n1)" /usr/local/bin/chromium \
 && chmod -R a+rx /ms-playwright
ENV PUPPETEER_EXECUTABLE_PATH=/usr/local/bin/chromium \
    PUPPETEER_SKIP_DOWNLOAD=1

WORKDIR /workspace
