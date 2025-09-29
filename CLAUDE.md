# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview
This is a personal Emacs configuration using `use-package` for package management. The configuration focuses on Python development with Eglot LSP, Ruby, Elixir, and general programming tools.

## Commands
- **Start Emacs server**: Already configured to start automatically via `(server-start)` in init.el:27
- **Reload configuration**: `M-x eval-buffer` while in init.el
- **Install packages**: Packages are auto-installed via `use-package` with `:ensure t`

## Key Architecture
### Package Management
- Uses `use-package` with MELPA repository
- Auto-installs packages on first load via `use-package-always-ensure t`
- Custom packages in `site-lisp/` directory (e.g., asdf-vm integration)

### Language Server Configuration (Eglot)
- Python: Uses `pylsp` with Ruff and Black formatters (config at init.el:154-163)
- Elixir/Heex: Custom language server at `/Users/amackera/elixir-ls-v0.29.3/language_server.sh`
- JavaScript/TypeScript: Uses `typescript-language-server`
- JSON: Uses `vscode-json-language-server`
- Ruby: Uses `solargraph`

### Key Bindings
- `C-c p` - Projectile commands
- `C-x v` - Open vterm terminal
- `C-c l` - Claude Code commands
- `C-c c` - Org capture
- `C-c a` - Org agenda
- `C-;` - Comment/uncomment line

### Environment Management
- Uses `exec-path-from-shell` to sync shell environment variables
- Custom asdf-vm integration via `site-lisp/asdf-vm/asdf-vm.el`
- Syncs PATH, ASDF_DIR, ASDF_DATA_DIR, PYENV_ROOT, NVM_DIR

### UI Configuration
- Theme: Kaolin Galaxy
- Font: Fira Code-12
- Completion: Vertico + Orderless
- Git interface: Magit
- Terminal: vterm