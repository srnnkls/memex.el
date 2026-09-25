# memex.el

Search indexed agent conversation history.

## About

memex.el is an Emacs client for [memex](https://github.com/nicosuave/memex), which indexes the
conversation history of coding agents such as Claude Code and Codex. It lists the sessions memex
indexed, searches them from the minibuffer as you type, and renders a whole session as a foldable
transcript in which each tool call and its result form one entry.

A transcript opens on the conversation. The person's and the agent's turns are shown whole, every
tool call is a one-line heading whose glyph and colour say what it was for, and text the harness
injected into a turn is out of the way. Four keys bring each kind of entry in or out, so a session
of a thousand records reads as the questions and answers it was about.

Reach for it when you need to find what an agent said, did or was told in an earlier session, read
a subagent's work after the fact, or get back to a session: in the viewer, in an Org note, or, with
[herdr.el](https://github.com/srnnkls/herdr.el), in the agent's own terminal.

## Installation

memex.el needs Emacs 29.1 or newer and the `memex` binary on `exec-path`. Install memex as its
[README](https://github.com/nicosuave/memex#install) describes and build an index with
`memex index`. [The memex backend](GUIDE.md#the-memex-backend) covers what memex.el asks of it.

The package depends on `magit-section`, `transient` and [lectio](https://github.com/srnnkls/lectio),
which renders an agent's markdown. Put a checkout of this repository and of lectio on the load
path:

```elisp
(use-package memex
  :load-path "~/projects/memex.el"
  :commands (memex-search memex-search-messages memex-search-sessions
             memex-view-session memex-usage))

(use-package memex-status
  :load-path "~/projects/memex.el"
  :commands (memex-status memex-project-status))
```

Each integration is a file of its own and loads only where its partner does:

```elisp
(use-package memex-anchor :after memex-view)  ; `a' in a transcript opens the live agent
(use-package memex-herdr :after herdr)        ; resume in herdr, keys in herdr's dashboard
(use-package memex-org :after org)            ; `memex:' links and capture
(use-package memex-embark :after embark)      ; actions on memex candidates
(use-package memex-evil :after evil)          ; normal-state keys in the viewer
```

[Consult](https://github.com/minad/consult) is optional. With it, search runs as you type and
previews the highlighted hit; without it, search reads one query and lists what memex answers.

## Getting started

Open the dashboard and read a session:

1. `M-x memex-status` lists the indexed sessions, newest activity first: age, message count,
   agent, repository and subject.
2. Move to a row and press `RET`. The transcript takes the frame, with point on the last message.
3. Press `n` and `p` to walk the records, `T` to show every tool call whole and `T` again to fold
   the calls back, and `t` to open or close every tool's input and output at once.
4. Press `q` to return to the dashboard.

Then search across sessions:

1. Run `M-x memex-search` and type `flaky test`. Each row is one session, with an excerpt of its
   hit.
2. With consult installed, press `C-SPC` to preview the highlighted session around the hit.
3. Press `M-r`, then `c` and `RET`, to add tool calls to the roles the search looks in.
4. Press `RET` on a row. The transcript opens at the hit, with the query highlighted.

From the dashboard, `s` on a row searches that session alone, and `s` off a row searches every
session listed.

## Commands

| Command | Does |
| --- | --- |
| `memex-status` | list the indexed sessions |
| `memex-project-status` | list the sessions of the current project |
| `memex-search` | search the index, one row per session or per message |
| `memex-search-sessions` | search, one row per session |
| `memex-search-messages` | search, one row per message |
| `memex-view-session` | read the transcript of a recent session |
| `memex-usage` | report token usage, for one project with a prefix argument |
| `memex-anchor-show` | open the live agent running a record's session, at that record |
| `memex-herdr-resume` | resume a session in a herdr tab |
| `memex-herdr-switch-session` | read the transcript of an agent herdr runs |
| `memex-org-capture` | capture a record as an Org excerpt |

In the minibuffer of a search, `M-m` cycles lexical, semantic and hybrid retrieval, `M-g` switches
between sessions and messages, `M-r` chooses roles, `M-t` asks for every role, and `M-.` searches
the highlighted session alone. In the dashboard, `?` opens a menu of its keys; in a transcript, `f`
opens a menu of what each kind of entry shows. [REFERENCE.md](REFERENCE.md#keys) lists every key.

## Concepts

| Term | Meaning |
| --- | --- |
| *record* | one message or tool event memex indexed, named by its `doc_id` |
| *session* | one conversation, named by a `session_id` together with its `source_path` |
| *source* | the agent that recorded a session, such as `claude` or `codex` |
| *role* | what a record is: `user`, `assistant`, `tool_use` or `tool_result` |
| *mode* | how a search matches: `lexical`, `semantic` or `hybrid` |
| *scope* | the sessions a search is limited to, or none for the whole index |
| *entry* | a record as the viewer draws it, with a tool call joined to its result |
| *kind* | what an entry is to a reader: `human`, `assistant`, `tool` or `system` |
| *state* | how much of a kind a transcript shows: `show`, `collapse` or `hide` |

## Documentation

- [GUIDE.md](GUIDE.md) explains how memex.el works, starting at
  [How memex.el works](GUIDE.md#how-memexel-works).
- [REFERENCE.md](REFERENCE.md) lists every command, key, user option, face and hook.

## Development

The package is built and tested with [Eask](https://emacs-eask.github.io/). lectio is not in a
package archive, so link a checkout of it before installing the other dependencies:

```sh
eask link add lectio ~/projects/lectio
eask install-deps
eask run script test          # every suite under ERT
eask lint checkdoc --strict   # docstrings
eask compile                  # byte-compile, warnings as errors
```

Three tests ask the installed memex whether it still speaks the wire format the fixtures record:
the recorded-payload contract, the API round trip and the core `ping`. Each skips itself when
`memex` is not on `PATH`. The core and API suites give a stub process 10 seconds and extend that
while memex is still running, up to 300 seconds, so an outer timeout in CI has to sit above 300
seconds.

Where the code lives:

- `memex-core.el`: the transport, `memex-rpc`, and the error symbols.
- `memex-api.el`: one function per operation memex's `rpc` command answers.
- `memex-completion.el`: `memex-read-record`, `memex-read-session` and `memex-read-project`.
- `memex.el`: search.
- `memex-status.el`: the session dashboard.
- `memex-entry.el`: pairing a call with its result, and classifying the pair.
- `memex-view.el`: the transcript viewer.
- `memex-usage.el`: the token usage report.
- `memex-herdr.el`, `memex-anchor.el`: the bridges to herdr and to a live agent's terminal.
- `memex-org.el`, `memex-embark.el`, `memex-evil.el`: the Org, embark and evil integrations.
- `memex-markdown.el`: obsolete aliases for the renderer that moved to lectio.
