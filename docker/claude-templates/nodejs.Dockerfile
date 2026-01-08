# Node.js + npm/pnpm + AWS CLI Template
#
# Usage: Copy to your project as .claude/Dockerfile
#   cp ~/code/sontek/homies/docker/claude-templates/nodejs.Dockerfile .claude/Dockerfile

FROM docker/sandbox-templates:claude-code

USER root

# Install Node.js and npm
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get update && apt-get install -y \
    nodejs \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install pnpm (alternative package manager)
RUN npm install -g pnpm

# Install AWS CLI v2
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
    && unzip awscliv2.zip \
    && ./aws/install \
    && rm -rf aws awscliv2.zip

USER agent

# Copy Node dependency files for layer caching
COPY --chown=agent:agent package.json package-lock.json* /tmp/deps/
WORKDIR /tmp/deps

# Install Node dependencies
# Use npm ci for lockfile-based installation
RUN npm ci || echo "Warning: npm ci failed, will retry at runtime"

# Alternative: Use pnpm instead
# RUN pnpm install --frozen-lockfile || echo "Warning: pnpm install failed"

USER agent
