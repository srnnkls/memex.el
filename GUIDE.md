# The memex.el guide

Every command, key, option and face is listed in [REFERENCE.md](REFERENCE.md). This guide explains
how the pieces fit and walks through them in the order you will use them.

## Contents

- [How memex.el works](#how-memexel-works)
- [The memex backend](#the-memex-backend)
- [The session dashboard](#the-session-dashboard)
- [Searching](#searching)
- [Reading a transcript](#reading-a-transcript)
- [Back to a live agent](#back-to-a-live-agent)
- [Token usage](#token-usage)
- [Org links and capture](#org-links-and-capture)
- [Completion, embark and icons](#completion-embark-and-icons)
- [Evil](#evil)
- [When something looks wrong](#when-something-looks-wrong)
- [Where to look next](#where-to-look-next)

## How memex.el works

memex reads the transcripts coding agents leave on disk and keeps an index of them. memex.el asks
that index questions and draws the answers in Emacs. A few terms carry the model:

- A *record* is one thing memex indexed: a message, a tool call or a tool result. Its `doc_id`
  names it.
- A *session* is one conversation. A `session_id` names a session only together with its
  `source_path`, the transcript file it was read from, so memex.el always passes the two as a pair.
- The *source* is the agent that recorded a session, such as `claude` or `codex`.
- A record's *role* is `user`, `assistant`, `tool_use` or `tool_result`. Most of an index is
  `tool_use` and `tool_result`.
- A search's *mode* is `lexical`, `semantic` or `hybrid`, and its *scope* is the sessions it is
  limited to. An empty scope searches the whole index.
- In the viewer, an *entry* is a record as a reader sees it: a tool call and its result drawn as
  one. Each entry is of one *kind*: `human`, `assistant`, `tool`, or `system` for whatever the
  harness wrote. A transcript shows each kind in one *state*: `show`, `collapse` or `hide`.

Everything starts from one of three places. The dashboard (`memex-status`) lists sessions,
search (`memex-search`) finds records, and the selectors behind `memex-view-session` and
`memex-org-capture` pick from recent records. All three lead into the transcript viewer, and from
there, with herdr, back to the agent.

## The memex backend

memex.el talks to the [memex](https://github.com/nicosuave/memex) binary and nothing else. It needs:

- The binary on `exec-path`, under the name or path `memex-executable` holds (`"memex"`). Every
  request checks for it first and signals `memex-error` when it is missing.
- An index. memex.el passes no `--root`, so memex uses its default data directory, `~/.memex`,
  and reads its configuration from `~/.memex/config.toml`. Run `memex index` once to build the
  index; memex.el asks memex to reindex before `g` redraws a transcript.
- The `rpc` subcommand, speaking protocol 1. Each request starts `memex rpc` in
  `temporary-file-directory`, writes one JSON envelope to its stdin, reads one response from its
  stdout and lets the process exit. There is no server to manage.

A request looks like this:

```json
{"protocol": 1,
 "request": {"op": "search",
             "spec": {"query": "flaky test", "limit": 20, "mode": "lexical",
                      "recency_weight": 1.0, "recency_half_life_days": 30.0}}}
```

The response carries the same `protocol` and a `response` object tagged by `kind`. A response under
another protocol version signals `memex-protocol-error` and reports what `memex --version` prints,
and a `kind` of `error` signals `memex-rpc-error` with memex's message. A process that exits
non-zero or prints something that is not JSON signals `memex-transport-error` with its stderr.

The operations memex.el uses are `ping`, `search`, `recent`, `sessions`, `session_count`,
`session`, `session_page`, `session_batch`, `show`, `index`, `usage`, `usage_activity` and
`session_activity`. memex-api.el has one function for each.

Two features need more than the index holds by default:

- Semantic and hybrid search need memex to have embedded the transcripts. See memex's own README
  for the embedding model and how to index with it.
- The token usage report needs memex's token tracking, which memex turns off by default. Enable
  it in `~/.memex/config.toml`.

The herdr bridge also runs `memex sessions --json-array` directly, because the command that resumes
a session is not part of the `rpc` surface. See [Back to a live agent](#back-to-a-live-agent).

## The session dashboard

`M-x memex-status` lists the sessions memex indexed, newest activity first, in `*memex status*`.
`M-x memex-project-status` does the same for the project `default-directory` belongs to, in a
dashboard named after the project. Each row shows how long ago the session was active, how many
messages it holds, the agent that recorded it, the repository and the subject. `TAB` expands a
row into everything memex holds for the session, including the command that resumes it.

The heading counts the rows and, once memex has counted them, the sessions that match in all, as
in `Sessions 20/412`. It also names the order and any filters in force.

| Key | Does |
| --- | --- |
| `RET` | open the transcript |
| `o` | open the transcript in another window |
| `r` | resume the session in a herdr tab |
| `w` | copy the command that resumes the session |
| `s` | search the session at point, or every session listed |
| `S` | search menu: scope and mode |
| `f` | narrow what memex is asked for |
| `P` | switch between this project's sessions and every session |
| `O` | order the rows |
| `L` | set how many sessions to ask for |
| `g` | ask memex again |
| `?` | a menu of these keys |
| `q` | quit |

`n`, `p`, `TAB`, `1` to `4` and `M-1` to `M-4` come from `magit-section-mode-map`.

### Narrowing and ordering

`f` narrows the request itself: by project, directory, source, origin or activity since a date, with
`L` for how many rows and `DEL` to clear. memex applies its limit after its filters, so a narrowed
dashboard of 20 rows holds 20 matching sessions rather than the matching few of the newest 20.

`O` orders the rows memex answered with and sends no request. memex answers newest first and offers
no other order, so ordering by message count shows the longest of the sessions you asked for, not
the longest memex knows. In the `O` menu, a second press of the same order reverses it, `f` flips
the current order and `DEL` goes back to memex's.

The origin filter chooses among `regular`, `interactive`, `subagent` and `all`. memex's default
leaves out permission reviews; `memex-status-origin` sets what every dashboard asks for.

### Searching from the dashboard

A search from the dashboard is scoped to what point stands on. On a row it searches that session;
anywhere else it searches every session listed, so narrowing the dashboard is also how you choose
the sessions a search runs over. `S` opens a menu with the same choice, a search of the whole
index, and a key for each mode.

## Searching

`M-x memex-search` reads a query and lists what memex answers. `memex-search-sessions` lists one
row per session, `memex-search-messages` one row per record, and `memex-search` starts as
`memex-search-group-by-session` says, which is sessions. With a prefix argument each asks for the
mode first; without one the mode is lexical.

A row shows the project, the source and role or tool, and an excerpt of the record cut around the
first query term, with the terms highlighted. The margin carries how many hits a session row stands
for and how long ago the record was written. `RET` opens the transcript at the hit and highlights
the query there with `hi-yellow`; `M-x unhighlight-regexp` takes the highlight down.

With [consult](https://github.com/minad/consult) installed, the search runs as you type: each change
to the query cancels the request in flight. A semantic query waits for `memex-search-debounce`
seconds of quiet, since memex embeds every query it is sent. Without consult, memex.el reads one
query, fetches once and lists the answer under the same keys.

### Narrowing a running search

These keys restart the search from the minibuffer, keeping the query and every other narrowing:

| Key | Does |
| --- | --- |
| `M-m` | cycle lexical, semantic and hybrid |
| `M-g` | switch between one row per session and one per message |
| `M-r` | choose the roles the search asks for |
| `M-t` | ask for every role, or go back to `memex-search-roles` |
| `M-.` | search the highlighted row's session alone, one row per message |

The prompt names every narrowing in force. `memex lexical messages [session] [user+assistant]:`
is a lexical search for messages, inside one session, among what the person and the agent wrote.

### Roles

A search starts from `memex-search-roles`, which is `user` and `assistant`: most of the index is
tool traffic, and a query is usually put to the conversation. `M-r` opens a menu that stays up
while you build the set:

| Key | Does |
| --- | --- |
| `u` | `user` in or out |
| `a` | `assistant` in or out |
| `c` | `tool_use` in or out |
| `r` | `tool_result` in or out |
| `SPC` | every role |
| `DEL` | back to `memex-search-roles` |
| `RET` | restart the search with this set |
| `q` | cancel |

memex applies the roles inside the index, before its limit, so a page of results is a page of
those roles. A memex without `roles` in its search spec ignores the set and answers with every role.

### Sessions and their hits

A search grouped by session asks memex for more hits than it shows and folds them into one row per
session, keeping memex's ranking. `memex-search-session-hit` picks which hit a session row opens at
and previews: `newest` (the default) or `best`. It chooses among the hits memex returned; it does
not rank the sessions differently or fetch every hit in the index.

### Previews

With consult, `C-SPC` shows the highlighted row's session in `*memex session preview*`, around the
hit and with the query highlighted. The preview goes where `display-buffer` would put a transcript,
and in the window the search was called from where nothing says otherwise. It draws
`memex-search-preview-context` records on each side of the hit, and fetches each session once per
search. `memex-search-preview-key` sets the key, and `memex-search-auto-preview` makes the preview
follow the selection. Leaving the search takes the preview down and gives the window back.

### Width

A search row is wide. `memex-search-project-width`, `memex-search-identity-width` and
`memex-search-snippet-width` cap its columns, and the excerpt also fits the narrowest window the
minibuffer is shown in. With vertico, a window of its own suits the category:

```elisp
(with-eval-after-load 'vertico-multiform
  (add-to-list 'vertico-multiform-categories '(memex-record buffer)))
```

## Reading a transcript

A transcript is a `magit-section` tree in `memex-session-mode`: each entry is a section, and each
tool's input and output a section inside it. It opens with point on the last message, or on the
record a search or link pointed at, and takes the frame under `memex-view-display-action`. The
header line names the project, the number of entries, how many reported an error, the time span
and any kind not shown whole.

### Headings

Every heading is a glyph, a label, the time of day and, for a tool call, a description, each at a
fixed column, so a run of entries reads down one edge:

```
 ▌ user       09:14:02
 ▌ ✳ claude   09:14:10
 ▸ bash       09:15:31  Find checkpoints
 ▪ read       09:15:40  config.yaml
 ◂ edit       09:16:05  memex-view.el
 ⁄ skill      09:16:12  rfc-contract-audit
 ▹ agent      09:17:48  reviewer: the search changes
 ▴ bash       09:18:20  Verify RED  Ran 7 tests, 0 as expected, 7 unexpected
 ⊘ edit       09:18:33  memex-entry.el
```

A person's or an agent's turn takes `▌` and no description, since the text follows. An agent's
turn also carries the mark of its source from `memex-view-source-marks`: a Nerd Font logo where a
font covers it, and otherwise `✳` for Claude and `⌬` for Codex.

A tool call takes the shape of what it was for, and its colour says the same:

| Glyph | Colour | The call |
| --- | --- | --- |
| `▸` | green | ran something |
| `▪` | cyan | looked something up |
| `▪` | magenta | went out to the network |
| `◂` | orange | changed a file |
| `▹` | purple | handed work to another agent |
| `⁄` | purple | invoked a skill |
| `·` | `memex-view-tool` | a tool of no known class |

Three outcomes take the glyph column instead. `▴` marks a call whose output reported something
wrong, and the line that said so follows the description. `⊘` marks a call that never ran because
it was denied or interrupted, and `○` a call whose result is not in the index yet.

memex records no exit status, so `▴` is read from the output, which `memex-entry-failure` matches.
Tools in `memex-entry-quoting` return what they fetched rather than a report, so their
output is never read for failure. `e` and `M-e` move to the next and previous entry marked `▴`.

### Kinds and states

A transcript shows the person and the agent whole, folds every tool call to its heading, and hides
what the harness injected: skill bodies, slash commands, file mentions and task notifications. It
tells an injected `user` record from a person's by `memex-entry-injected`, a regexp over how the
record opens. Roles other than `user` and `assistant` count as `system` too.

| Key | Does |
| --- | --- |
| `U` | show every `human` entry whole, or put them back |
| `A` | show every `assistant` entry whole, or put them back |
| `T` | show every `tool` entry whole, or put them back |
| `S` | show every `system` entry whole, or put them back |
| `f` | a menu that moves each kind between `show`, `collapse` and `hide` |

A kind that opens whole has nowhere to go back to, so its key hides it. In the `f` menu, `u`, `a`,
`t` and `s` cycle one kind each, `SPC` shows everything and `DEL` returns to the opening states.
`memex-view-initial-states` sets the states a transcript opens in.

Hidden entries stay in the buffer as invisible text, so `isearch` and `occur` still reach them. A
search from the viewer that lands on a hidden entry brings its kind back as headings.

### Moving and acting

| Key | Does |
| --- | --- |
| `n`, `p` | next and previous record |
| `e`, `M-e` | next and previous entry that reported an error |
| `t` | fold or unfold every tool's input and output at once |
| `d` | show or hide each entry's duration, `doc_id` and counts |
| `RET` | open the tool field at point in a buffer of its own |
| `w` | copy what the entry was given: its command, or its first argument |
| `s` | search this session and jump to the hit you pick |
| `a` | open the live agent at this record (needs memex-anchor.el) |
| `g` | reindex, then redraw the transcript in place |
| `q` | kill the transcript and close the window it opened |

`TAB`, `S-TAB` and `1` to `4` fold sections as in Magit, and `imenu` lists every entry by its
description.

`g` asks memex to rescan its sources first, since the index holds only what memex last read. Filters
and details survive the redraw, and point on the last record follows the session to its new end.

`q` closes a window only if the viewer opened it. A window it borrowed, such as a popup holding a
terminal, goes back to showing what it showed before.

### Rendering

Messages from the roles in `memex-view-markdown-roles` are rendered as markdown through lectio, up
to `memex-view-markdown-limit` characters. Tool input and output are fontified in the mode their
tool and file path imply, up to `memex-view-fontify-limit` characters, and a tool's output shows
`memex-view-output-lines` lines before the rest folds away. A long session is drawn in chunks of
`memex-view-chunk-size` entries, newest first, while Emacs is idle.

memex indexes no reasoning. Claude stores its thinking blocks empty and Codex encrypts its
reasoning items, so neither reaches a transcript.

## Back to a live agent

These features need [herdr.el](https://github.com/srnnkls/herdr.el), which controls persistent herdr
terminal workspaces. Without it, `memex-anchor-show` opens the indexed transcript, and a resume
refuses with an error unless the session has no resume command to run.

### Resuming a session

`r` in the dashboard, `M-x memex-herdr-resume`, and the embark action `r` take a session back to a
live agent. If herdr already runs an agent reporting that session, memex.el attaches its terminal.
Otherwise it opens a herdr tab in the session's working directory and starts the agent from the
command memex recorded for it, waiting up to `memex-herdr-start-timeout` milliseconds for it to
come up.

memex's `rpc` surface carries no resume command, so memex.el reads it from
`memex sessions --json-array`, which has no session filter. It reads the newest
`memex-resume-lookup-limit` sessions of the source and matches the one asked for. A session whose
transcript file is gone, or for which memex recorded no resume command, opens in the viewer.

### Reading a record in the agent's terminal

`a` in a transcript, or `M-x memex-anchor-show`, opens the agent still running the record's session
and puts point where its terminal drew the record. memex.el finds the pane whose agent reports the
same session ID or transcript path; for an older agent that reports neither, it takes the only agent
of that source in the session's directory. A session nobody runs is resumed, and one that cannot be
resumed opens in the viewer.

The match is made on letters alone, since a terminal strips markdown, adds glyphs and breaks lines.
`memex-anchor-window` is how many consecutive letters must match. A record the terminal has scrolled
out of its history cannot be found there, and the viewer remains the complete history.

`memex-anchor-target` chooses between the terminal (the default) and the indexed transcript for a
session herdr runs. `memex-anchor-takeover` decides whether the attached terminal accepts input
while its Emacs window has focus.

### From herdr to memex

memex-herdr.el adds memex to herdr's own interfaces when both are loaded:

- In herdr's transient, `v` shows the transcript of the agent attached in the current buffer
  (`memex-herdr-open-agent-session`).
- In herdr's dashboard, `s` searches the sessions of the agent, herd or list at point, and `m`
  opens `memex-herdr-dispatch`, a menu of scope, mode, transcript and resume. Neither key replaces a
  binding you made.
- `M-x memex-herdr-switch-session` reads one of this workspace's agents and shows its transcript;
  with a prefix argument it offers every running agent.

memex-herdr.el also changes where a search, a link or an embark action opens a transcript: under
`memex-herdr-display-action`, which leaves the choice to `display-buffer`, instead of taking the
frame.

## Token usage

`M-x memex-usage` reports token usage in `*memex usage*`: totals and pricing context, then each
source with its tokens, cost and cache misses, then whatever a machine warned about. With a prefix
argument it asks for a project and reports into `*memex usage: PROJECT*`. The report needs memex's
token tracking to be on.

## Org links and capture

memex-org.el adds a `memex:` link type. A link names the session and the record, and following it
opens the transcript at that record:

```org
[[memex:SESSION-ID:SOURCE-PATH:DOC-ID][memex my-project assistant #1234]]
```

`M-x memex-org-capture` reads a recent record and turns it into a link followed by the record's text
in a quote block. With `memex-org-capture-template` set to the key of an `org-capture` template, the
excerpt goes through that template; without one it goes to the kill ring and an Org buffer.

## Completion, embark and icons

Search candidates, and the selectors that read a record, a session or a project, carry the
completion categories `memex-record`, `memex-session` and `memex-project`. Each candidate carries
its record, so other packages can act on it.

memex-embark.el binds these actions on a record or session candidate:

| Key | Does |
| --- | --- |
| `t` | open the transcript at the record |
| `r` | resume the session |
| `v` | show the record's fields in `*memex record*` |
| `y` | copy the record's `doc_id` |
| `u` | report token usage for the record's project |
| `c` | capture the record as an Org excerpt |

A project candidate offers `u` alone. `t` and `r` go through memex-herdr.el, so load it as well.
When marginalia is loaded, memex-embark.el registers memex's annotations for the record categories,
and when `nerd-icons-completion` is loaded it gives each category an icon.

## Evil

memex-evil.el gives the viewer's commands keys in evil normal state, where the plain keys belong to
evil. Motion stays evil's own.

| Key | Does |
| --- | --- |
| `M-n`, `M-p` | next and previous record |
| `g TAB` | fold or unfold every tool's input and output |
| `g f` | the kinds menu |
| `g U`, `g A`, `g T`, `g S` | toggle one kind |
| `g s` | search this session |
| `g r` | resume in herdr |
| `g a` | open the live agent at this record |
| `q`, `g Q` | quit the transcript |

## When something looks wrong

- `memex executable not found`: `memex-executable` does not name a program on `exec-path`. On
  macOS, a GUI Emacs may not inherit the shell's `PATH`.
- `memex speaks another protocol`: the installed memex answers under a protocol other than 1.
  Upgrade memex or memex.el so the two agree.
- A search finds nothing you know is there: check the roles in the prompt, then press `M-t` to ask
  for every role. A transcript newer than memex's last scan is not in the index until
  `memex index` runs, or `g` in a transcript.
- Semantic or hybrid search finds nothing: memex has not embedded the transcripts.
- The usage report is empty: memex's token tracking is off.
- `r` says memex found no session in the last N sessions: raise `memex-resume-lookup-limit`.
- A transcript shows fewer entries than expected: the header line names every kind not shown
  whole. Press `f` then `SPC` to show everything.
- A person's question is missing from a transcript: `memex-entry-injected` took it for harness text.
  Press `S` to show `system` entries, and narrow the regexp.
- Tofu where an agent's mark should be: the frame has no Nerd Font. Set `memex-view-nerd-font` to
  `nil`.
- A request that fails after it started, such as a search, a preview or the usage report, reports
  in the echo area rather than signalling an error, because the answer arrives in a process
  sentinel. Check `*Messages*`.

## Where to look next

- [REFERENCE.md](REFERENCE.md) lists every command, key, user option, face and hook.
- memex's own [README](https://github.com/nicosuave/memex) covers indexing, embedding models,
  remote machines and token tracking.
- `memex-api.el` documents each operation's fields in its function docstrings.
