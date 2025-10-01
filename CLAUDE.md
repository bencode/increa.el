# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**increa.el** is an Emacs Lisp package providing GitHub Copilot-like code completion using LLM APIs (Qwen-Coder or Claude). It displays ghost text completions asynchronously without requiring LSP servers.

## Architecture

### Module Organization

Four core modules with clear separation of concerns:

- **increa.el**: Main orchestration - manages minor mode, idle timers, completion state, and coordinates other modules
- **increa-api.el**: API communication layer - uses `request.el` for async HTTP requests to LLM providers (Qwen/Claude)
- **increa-context.el**: Context extraction - intelligently gathers code context (max 100KB by default), builds prompts, cleans responses
- **increa-overlay.el**: Ghost text display - manages Emacs overlays for showing/accepting completions

### Completion Flow

```
User types → Post-command hook → Idle timer (0.5s) → increa-complete
  → increa--get-context (smart context extraction, max 100KB)
  → increa--build-prompt (structured prompt with <cursor> marker)
  → increa--call-api (async HTTP via request.el)
  → Callback → increa--trim-completion (cleanup)
  → increa--set-overlay-text (display as ghost text)
  → User presses TAB/C-<return> → increa-accept-completion (insert text)
```

### Context Extraction Strategy

Similar to copilot.el, uses character-based limits instead of line counts:

- **Default**: 100,000 characters (configurable via `increa-context-max-chars`)
- **Small files**: Send entire file content
- **Large files**: Extract centered around cursor (50KB before, 50KB after)
- **Edge cases**: Adjust window if cursor is near file beginning/end

This approach provides better context for LLM while avoiding token limits.

### State Management

- **Buffer-local variables** (`defvar-local`) track completion state per buffer
- **Completion guards**: `increa--completion-in-progress` prevents concurrent requests
- **Position tracking**: `increa--last-trigger-point` validates completion context
- **Timer lifecycle**: Single idle timer per buffer, cleaned up on mode disable

### Smart Triggering

Completions only trigger when:
- Not in minibuffer or read-only buffer
- No completion in progress or already visible
- Point not at beginning of buffer
- **Not inside strings** (checked via `syntax-ppss` nth 3)
- **Not inside comments** (checked via `syntax-ppss` nth 4)

This logic is centralized in `increa--should-trigger-p` (increa.el).

## Code Style Conventions

- **Lexical binding**: All files use `;;; -*- lexical-binding: t; -*-`
- **Namespace**: Public functions use `increa-`, private use `increa--` (double-dash)
- **Customization**: All user options use `defcustom` with `:group 'increa`
- **Function focus**: Keep functions single-purpose and focused

## Configuration

### API Setup

```elisp
;; Choose provider
(setq increa-api-provider 'qwen)  ; or 'claude

;; API credentials (required)
(setq increa-api-key "YOUR_API_KEY")

;; Qwen-specific
(setq increa-model "qwen3-coder-flash")  ; or "qwen3-coder-plus"
(setq increa-api-endpoint "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions")

;; Regional endpoints for Qwen:
;; - Beijing (default): dashscope.aliyuncs.com
;; - Singapore: dashscope-intl.aliyuncs.com
```

### Completion Behavior

```elisp
(setq increa-idle-delay 0.5)             ; Auto-trigger delay (seconds)
(setq increa-enable-auto-complete t)     ; Enable auto-completion
(setq increa-context-max-chars 100000)   ; Context size limit (characters)
(setq increa-max-tokens 500)             ; Max completion length
(setq increa-temperature 0.2)            ; LLM temperature (0.0-1.0)
(setq increa-api-timeout 10)             ; Request timeout (seconds)
```

### Keybindings

- `C-c i` or `C-c C-i`: Manually trigger completion
- `TAB`: Accept visible completion (when overlay present)
- `C-<return>`: Accept completion (Ctrl+Enter)
- `C-g`: Dismiss completion

## Testing and Debugging

- **No automated tests**: Manual testing required in various language buffers
- **Error logging**: Check `*Messages*` buffer for API errors and state changes
- **Test scenarios**: Try completions in Python, JavaScript, Emacs Lisp buffers
- **Debug mode**: Check `increa--completion-in-progress` and overlay state with `describe-variable`

## Dependencies

- **Emacs**: Minimum version 27.1
- **Required package**: [request.el](https://github.com/tkf/emacs-request) for HTTP requests
- **External**: Requires Qwen (Dashscope) or Claude API key with network access

## Installation

```elisp
;; Install request.el first
(package-install 'request)

;; Or in Doom Emacs:
;; Add to packages.el: (package! request)
;; Then run: doom sync

;; Install increa.el
(add-to-list 'load-path "/path/to/increa.el")
(require 'increa)
(setq increa-api-provider 'qwen)
(setq increa-api-key "YOUR_API_KEY")
(global-increa-mode 1)
```

## Prompt Engineering

The prompt structure in `increa--build-prompt` (increa-context.el) is carefully designed:
- Clear role definition: "You are a code completion assistant"
- Explicit output constraints: "Only output code, no explanations"
- Context provided: language, code before/after with `<cursor>` marker
- Rules emphasize: no repetition of prefix, concise output, style matching

When modifying prompts, maintain this structure and test across multiple languages.