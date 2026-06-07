# Sandboxing Claude

I give coding agents broad permissions, and they can be steered by whatever they
read (prompt injection). So I don't trust the agent; I trust the box it runs in.
Each project gets its own Colima Linux VM and the agent lives inside it. The VM
is the boundary.

```
just vm-up <project>
```

## Why a VM is the right boundary

**It's a real virtual machine, not a container.** Colima runs it under Apple's
Virtualization framework. Worst case, a compromised agent becomes root inside the
VM, and that is where it stops: the hypervisor boundary keeps it off the macOS
host, the kernel, and anything I didn't mount. A container that shares the host
kernel wouldn't give me that.

**Only the project is mounted, nothing else.** The VM gets `~/code/<project>`
plus `~/code/sontek` (my dotfiles and tooling), and that is it. No `~`, no
`~/.ssh`, no `~/.aws`, no other projects. An escape sees the code it is working
on and none of my secrets.

**No host Docker socket.** The VM runs its own Docker daemon, so `just dev`,
compose, and tilt are containers inside the VM. I never pass in the host socket;
that would let the agent run a privileged container that mounts `/` and takes the
Mac. It can't reach the host daemon.

**Git without a key in the VM.** I forward the host SSH agent in, so `git push`
uses my keys without the private key ever touching the VM disk. Nothing to
exfiltrate.

**AWS without my real credentials.** The VM inherits no AWS access. I run
`aws sso login` inside it against a sandbox account, which gives a short-lived,
scoped token that stays in the VM. My `~/.aws` (prod profiles, long-lived keys)
is never mounted, so the agent can't escalate into anything I didn't hand it.

## What an escape can still reach

I'm not pretending it's airtight. Inside those limits a compromised agent can
read or change the mounted project and my `~/code/sontek` tooling, and while I'm
connected with the agent forwarded it can use that agent to act as me on GitHub.
When I want it tighter:

- A fine-grained, repo-scoped `GH_TOKEN` over HTTPS instead of agent forwarding.
- Sandbox AWS accounts only; I never SSO into prod from an agent's VM.
- One project per VM, so a bad one doesn't touch the rest of my work.

## Short version

The agent lives in a disposable Linux VM that sees its project and my tooling,
runs its own Docker, borrows my SSH agent without holding the key, and only has
the AWS access I log into inside it. A bad day for the agent is a bad day for
that VM, not for my Mac, my keys, or my cloud accounts.
