# Go + AWS CLI Template
#
# Usage: Copy to your project as .claude/Dockerfile
#   cp ~/code/sontek/homies/docker/claude-templates/golang.Dockerfile .claude/Dockerfile

FROM docker/sandbox-templates:claude-code

USER root

# Install Go
RUN curl -LO https://go.dev/dl/go1.21.5.linux-amd64.tar.gz \
    && rm -rf /usr/local/go \
    && tar -C /usr/local -xzf go1.21.5.linux-amd64.tar.gz \
    && rm go1.21.5.linux-amd64.tar.gz
ENV PATH="/usr/local/go/bin:${PATH}"

# Install AWS CLI v2
RUN apt-get update && apt-get install -y \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
    && unzip awscliv2.zip \
    && ./aws/install \
    && rm -rf aws awscliv2.zip

USER agent

# Copy Go dependency files for layer caching
COPY --chown=agent:agent go.mod go.sum* /tmp/deps/
WORKDIR /tmp/deps

# Download Go dependencies
RUN go mod download || echo "Warning: go mod download failed, will retry at runtime"

USER agent
