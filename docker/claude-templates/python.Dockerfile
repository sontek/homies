# Python 3.12 + uv/Poetry + Just + AWS CLI Template
#
# Usage: Copy to your project as .claude/Dockerfile
#   cp ~/code/sontek/homies/docker/claude-templates/python.Dockerfile .claude/Dockerfile
#
# Then uncomment either uv or Poetry section below.

FROM docker/sandbox-templates:claude-code

USER root

# Add deadsnakes PPA for Python 3.12
RUN apt-get update && apt-get install -y software-properties-common \
    && add-apt-repository ppa:deadsnakes/ppa -y \
    && apt-get update

# Install Python 3.12 and system dependencies
RUN apt-get install -y \
    build-essential \
    python3.12 \
    python3.12-dev \
    python3.12-venv \
    python3-pip \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Make python3.12 the default
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.12 1 \
    && update-alternatives --install /usr/bin/python python /usr/bin/python3.12 1

# Install AWS CLI v2
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
    && unzip awscliv2.zip \
    && ./aws/install \
    && rm -rf aws awscliv2.zip

# Install just (command runner)
RUN curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

USER agent

# ===== Option 1: Use uv (recommended for modern projects) =====
# Install uv (fast Python package manager)
RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/home/agent/.cargo/bin:${PATH}"

# Copy Python dependency files for layer caching
COPY --chown=agent:agent pyproject.toml uv.lock* /tmp/deps/
WORKDIR /tmp/deps

# Install Python dependencies with uv
RUN uv sync --frozen || echo "Warning: uv sync failed, will retry at runtime"

# ===== Option 2: Use Poetry (uncomment if using Poetry) =====
# Install Poetry
# RUN pip install --user poetry
# ENV PATH="/home/agent/.local/bin:${PATH}"

# Copy Python dependency files for layer caching
# COPY --chown=agent:agent pyproject.toml poetry.lock* /tmp/deps/
# WORKDIR /tmp/deps

# Install Python dependencies with Poetry
# RUN poetry install --no-root || echo "Warning: poetry install failed, will retry at runtime"

# Reset to agent user (Claude expects this)
USER agent

# Note: Source code is mounted at runtime, not copied into image
# Note: AWS credentials are mounted at runtime from ~/.aws
