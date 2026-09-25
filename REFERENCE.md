# memex.el reference

Every command, key, user option, face, hook and Lisp entry point. For how they fit together, read
[GUIDE.md](GUIDE.md).

## Contents

- [Keys](#keys)
- [Commands](#commands)
- [User options](#user-options)
- [Faces](#faces)
- [Hooks](#hooks)
- [Buffers](#buffers)
- [Lisp interface](#lisp-interface)
- [Errors](#errors)
- [Obsolete names](#obsolete-names)

## Keys

### Search minibuffer

`memex-search-map`, active while a search reads its query.

| Key | Command |
| --- | --- |
| `M-m` | `memex-search-cycle-mode` |
| `M-g` | `memex-search-toggle-grouping` |
| `M-r` | `memex-search-select-roles` |
| `M-t` | `memex-search-toggle-roles` |
| `M-.` | `memex-search-in-selected-session` |

With consult, `memex-search-preview-key` (`C-SPC`) previews the highlighted candidate.

### Roles menu

`memex-search-select-roles`, opened with `M-r`.

| Key | Command |
| --- | --- |
| `u` | `memex-search-toggle-role-user` |
| `a` | `memex-search-toggle-role-assistant` |
| `c` | `memex-search-toggle-role-tool-use` |
| `r` | `memex-search-toggle-role-tool-result` |
| `SPC` | `memex-search-select-every-role` |
| `DEL` | `memex-search-select-default-roles` |
| `RET` | `memex-search-apply-selected-roles` |
| `q` | cancel |

### Dashboard

`memex-status-mode-map`, whose parent is `magit-section-mode-map`.

| Key | Command |
| --- | --- |
| `RET` | `memex-status-visit` |
| `o` | `memex-status-visit-other-window` |
| `r` | `memex-status-resume` |
| `w` | `memex-status-copy-command` |
| `s` | `memex-status-search` |
| `S` | `memex-status-search-dispatch` |
| `f` | `memex-status-filter` |
| `P` | `memex-status-toggle-project` |
| `O` | `memex-status-sort` |
| `L` | `memex-status-set-limit` |
| `g` | `memex-status-refresh` |
| `?` | `memex-status-dispatch` |
| `q` | `quit-window` |

`memex-status-dispatch` (`?`) offers the same keys, and `?` in it closes it.

`memex-status-filter` (`f`):

| Key | Command |
| --- | --- |
| `p` | `memex-status-narrow-project` |
| `d` | `memex-status-narrow-directory` |
| `k` | `memex-status-narrow-source` |
| `o` | `memex-status-narrow-origin` |
| `s` | `memex-status-narrow-since` |
| `L` | `memex-status-set-limit` |
| `DEL` | `memex-status-clear-narrowing` |

`memex-status-sort` (`O`):

| Key | Order |
| --- | --- |
| `r` | last active |
| `s` | started |
| `n` | messages |
| `p` | project |
| `k` | source |
| `l` | label |
| `x` | any order in `memex-status-sorts` (`memex-status-sort-by`) |
| `f` | reverse the current order (`memex-status-sort-reverse`) |
| `DEL` | memex's own order (`memex-status-sort-clear`) |

Choosing the order already in force reverses it.

`memex-status-search-dispatch` (`S`):

| Key | Searches |
| --- | --- |
| `s` | the session at point, or every session listed (`memex-status-search`) |
| `l` | every session listed (`memex-status-search-listed`) |
| `g` | the whole index (`memex-status-search-globally`) |
| `x` | as `s`, lexical |
| `m` | as `s`, semantic |
| `y` | as `s`, hybrid |

### Transcript

`memex-session-mode-map`. `memex-session-mode` derives from `magit-section-mode`, so its section
keys (`TAB`, `S-TAB`, `1` to `4`, `M-1` to `M-4`, `^`) work too.

| Key | Command |
| --- | --- |
| `n` | `memex-view-next-record` |
| `p` | `memex-view-previous-record` |
| `e` | `memex-view-next-problem` |
| `M-e` | `memex-view-previous-problem` |
| `t` | `memex-view-toggle-tool-content` |
| `d` | `memex-view-toggle-details` |
| `f` | `memex-view-filter` |
| `U` | `memex-view-toggle-human` |
| `A` | `memex-view-toggle-assistant` |
| `T` | `memex-view-toggle-tool` |
| `S` | `memex-view-toggle-system` |
| `w` | `memex-view-copy-command` |
| `RET` | `memex-view-visit-payload` |
| `s` | `memex-view-search-in-session` |
| `a` | `memex-anchor-show`, once memex-anchor.el is loaded |
| `g` | `memex-view-refresh` |
| `q` | `memex-view-quit` |

`memex-view-filter` (`f`):

| Key | Command |
| --- | --- |
| `u` | `memex-view-cycle-human` |
| `a` | `memex-view-cycle-assistant` |
| `t` | `memex-view-cycle-tool` |
| `s` | `memex-view-cycle-system` |
| `SPC` | `memex-view-show-everything` |
| `DEL` | `memex-view-reset-states` |
| `q` | done |

### Transcript under evil

memex-evil.el binds these in normal state in `memex-session-mode-map` once evil loads.

| Key | Command |
| --- | --- |
| `M-n` | `memex-view-next-record` |
| `M-p` | `memex-view-previous-record` |
| `g TAB` | `memex-view-toggle-tool-content` |
| `g f` | `memex-view-filter` |
| `g U` | `memex-view-toggle-human` |
| `g A` | `memex-view-toggle-assistant` |
| `g T` | `memex-view-toggle-tool` |
| `g S` | `memex-view-toggle-system` |
| `g s` | `memex-view-search-in-session` |
| `g r` | `memex-herdr-resume` |
| `g a` | `memex-anchor-show` |
| `q`, `g Q` | `memex-view-quit` |

### Embark

`memex-embark-record-map` serves the `memex-record` and `memex-session` categories, and
`memex-embark-project-map` the `memex-project` category. Both take `embark-general-map` as parent.

| Key | Action | Maps |
| --- | --- | --- |
| `t` | `memex-embark-open-transcript` | record |
| `r` | `memex-embark-resume` | record |
| `v` | `memex-embark-show-record` | record |
| `y` | `memex-embark-copy-record-id` | record |
| `u` | `memex-embark-usage` | record, project |
| `c` | `memex-embark-org-capture` | record |

`t` and `r` call into memex-herdr.el.

### herdr

memex-herdr.el adds these where herdr.el is loaded:

| Where | Key | Command |
| --- | --- | --- |
| `herdr-transient` | `v` | `memex-herdr-open-agent-session` |
| `herdr-status-mode-map` | `s` | `memex-herdr-search` |
| `herdr-status-mode-map` | `m` | `memex-herdr-dispatch` |
| `herdr-status-dispatch` | `s`, `m` | the same two, in a Search group |

A dashboard key someone else already bound is left alone.

`memex-herdr-dispatch`:

| Key | Command |
| --- | --- |
| `s` | `memex-herdr-search` |
| `g` | `memex-herdr-search-globally` |
| `S` | as `s`, semantic |
| `L` | as `s`, lexical |
| `H` | as `s`, hybrid |
| `RET` | `memex-herdr-transcript-at-point` |
| `r` | `memex-herdr-resume-at-point` |

## Commands

With a prefix argument, the search commands read a mode; without one the mode is `lexical`.

### Search (memex.el)

| Command | Does |
| --- | --- |
| `memex-search` | search, grouped as `memex-search-group-by-session` says; open the pick and return its record |
| `memex-search-sessions` | search with one candidate per session |
| `memex-search-messages` | search with one candidate per record |
| `memex-search-cycle-mode` | restart the running search in the next mode |
| `memex-search-toggle-grouping` | restart it grouped the other way |
| `memex-search-select-roles` | choose the roles it asks for |
| `memex-search-toggle-roles` | restart it asking for every role, or for `memex-search-roles` |
| `memex-search-in-selected-session` | restart it within the highlighted candidate's session |

### Dashboard (memex-status.el)

| Command | Does |
| --- | --- |
| `memex-status` | show the dashboard of every session |
| `memex-project-status` | show the dashboard of the current project's sessions |
| `memex-status-refresh` | ask memex again; also `revert-buffer` |
| `memex-status-visit` | open the transcript of the session at point |
| `memex-status-visit-other-window` | the same, in another window |
| `memex-status-resume` | resume the session at point in herdr |
| `memex-status-copy-command` | copy the session's resume command |
| `memex-status-search` | search the session at point, or every session listed |
| `memex-status-search-listed` | search every session listed |
| `memex-status-search-globally` | search the whole index |
| `memex-status-narrow-project` | ask only for a project's sessions |
| `memex-status-narrow-directory` | ask only for sessions under a directory |
| `memex-status-narrow-source` | ask only for one source's sessions |
| `memex-status-narrow-origin` | ask for `regular`, `interactive`, `subagent` or `all` sessions |
| `memex-status-narrow-since` | ask only for sessions active since a date or RFC 3339 time |
| `memex-status-toggle-project` | switch between the project's sessions and every session |
| `memex-status-clear-narrowing` | drop every filter |
| `memex-status-set-limit` | set how many sessions to ask for, 1 to 500 |
| `memex-status-sort-by` | order the rows by a name in `memex-status-sorts` |
| `memex-status-sort-reverse` | reverse the order |
| `memex-status-sort-clear` | return to memex's order |
| `memex-status-dispatch`, `memex-status-filter`, `memex-status-sort`, `memex-status-search-dispatch` | the menus above |

### Transcript (memex-view.el)

| Command | Does |
| --- | --- |
| `memex-view-session` | read a session from recent records and show its transcript |
| `memex-view-refresh` | reindex, then redraw the transcript in place; also `revert-buffer` |
| `memex-view-quit` | kill the transcript and close only a window opened for it |
| `memex-view-next-record`, `memex-view-previous-record` | move by record |
| `memex-view-next-problem`, `memex-view-previous-problem` | move to an entry that reported an error |
| `memex-view-follow-end` | move to the last message, with the end in view (unbound) |
| `memex-view-toggle-tool-content` | fold or unfold every tool field |
| `memex-view-toggle-details` | show or hide durations, `doc_id`s and counts |
| `memex-view-toggle-human`, `-assistant`, `-tool`, `-system` | show a kind whole, or return it to its opening state |
| `memex-view-cycle-human`, `-assistant`, `-tool`, `-system` | move a kind to its next state (menu suffixes) |
| `memex-view-show-everything` | show every kind whole |
| `memex-view-reset-states` | return every kind to `memex-view-initial-states` |
| `memex-view-filter` | the kinds menu |
| `memex-view-visit-payload` | open the tool field at point in a buffer of its own |
| `memex-view-copy-command` | copy the entry's `command` argument, or its first |
| `memex-view-search-in-session` | search this session and jump to the chosen hit |

### Usage (memex-usage.el)

| Command | Does |
| --- | --- |
| `memex-usage` | report token usage; with a prefix argument, read a project and report on it |

### herdr (memex-herdr.el)

| Command | Does |
| --- | --- |
| `memex-herdr-resume` | resume a session in herdr: the transcript's in a viewer, else one read from recent records |
| `memex-herdr-switch-session` | read one of this workspace's agents and show its transcript; every agent with a prefix argument |
| `memex-herdr-open-agent-session` | show the transcript of the agent attached in the current buffer |
| `memex-herdr-search` | search the sessions of the herdr dashboard section at point |
| `memex-herdr-search-globally` | search the whole index |
| `memex-herdr-transcript-at-point` | show the transcript of the agent at point in herdr's dashboard |
| `memex-herdr-resume-at-point` | resume the session of the agent at point |
| `memex-herdr-dispatch` | the herdr menu above |

### Anchor (memex-anchor.el)

| Command | Does |
| --- | --- |
| `memex-anchor-show` | open the live agent running a record's session, at the record: the record at point in a transcript, else one read from recent records |

### Org (memex-org.el)

| Command | Does |
| --- | --- |
| `memex-org-capture` | read a recent record and capture it as an Org excerpt |

## User options

`M-x customize-group RET memex` reaches all of them.

### Transport (memex-core.el, memex-completion.el)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-executable` | string | `"memex"` | name of, or path to, the memex binary |
| `memex-completion-timeout` | number | `10.0` | seconds a selector waits for memex |
| `memex-completion-width` | positive integer | `64` | width a backend string is cut to in a candidate |
| `memex-completion-recent-limit` | natnum | `500` | recent records the selectors draw from |

### Search (memex.el)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-search-group-by-session` | boolean | `t` | one candidate per session rather than per record |
| `memex-search-roles` | set of roles | `("user" "assistant")` | roles a search starts with; nil for every role |
| `memex-search-session-hit` | `newest` or `best` | `newest` | which hit a session candidate opens at and previews |
| `memex-search-debounce` | number | `0.4` | seconds of quiet before a semantic query is sent, with consult |
| `memex-search-preview-key` | key or `any` | `"C-SPC"` | key that previews the highlighted candidate |
| `memex-search-auto-preview` | boolean | `nil` | preview follows the selection |
| `memex-search-preview-context` | natnum | `25` | records a preview draws on each side of the hit |
| `memex-search-highlight-function` | function or nil | `memex-search-highlight-with-hi-lock` | marks the query in a preview or opened transcript; called with the window and the query |
| `memex-search-snippet-width` | natnum | `160` | widest excerpt |
| `memex-search-project-width` | natnum | `22` | widest project column |
| `memex-search-identity-width` | natnum | `22` | widest source and role column |

### Dashboard (memex-status.el, group `memex-status`)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-status-limit` | natnum | `20` | sessions a dashboard asks for |
| `memex-status-origin` | nil, `all`, `interactive` or `subagent` | `nil` | sessions asked for; nil is memex's default, without permission reviews |
| `memex-status-sorts` | alist | recent, started, messages, project, source, label | orders `O` offers: name to key function and natural direction |
| `memex-status-buffer-name` | string | `"*memex status*"` | dashboard buffer name; project dashboards append the project |
| `memex-status-display-action` | `display-buffer` action | `((display-buffer-reuse-window display-buffer-same-window))` | how the dashboard is shown |
| `memex-status-label-width` | natnum | `64` | widest subject |
| `memex-status-project-width` | natnum | `16` | widest repository name |

### Transcript (memex-view.el)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-view-initial-states` | alist of kind to state | `((human . show) (assistant . show) (tool . collapse) (system . hide))` | how much of each kind a transcript opens showing |
| `memex-view-details` | boolean | `nil` | open with durations, `doc_id`s and counts shown |
| `memex-view-display-action` | `display-buffer` action | `(display-buffer-full-frame)` | how a transcript is shown |
| `memex-view-markdown-roles` | list of strings | `("user" "assistant")` | roles whose text is rendered as markdown |
| `memex-view-markdown-limit` | natnum | `20000` | longest message rendered as markdown, in characters |
| `memex-view-fontify-tool-content` | boolean | `t` | fontify tool input and output in the mode of their tool and path |
| `memex-view-fontify-limit` | natnum | `40000` | longest tool field fontified, in characters |
| `memex-view-output-lines` | natnum | `12` | lines of tool output shown before the rest folds |
| `memex-view-chunk-size` | natnum | `200` | entries drawn per chunk, newest chunk first |
| `memex-view-fill-delay` | number | `0.05` | idle seconds between chunks |
| `memex-view-heading-clock` | boolean | `t` | headings show the time of day |
| `memex-view-heading-width` | natnum | `78` | longest heading description |
| `memex-view-heading-indent` | natnum | `1` | columns between the fold indicator and a heading's glyph |
| `memex-view-label-width` | natnum | `11` | column heading descriptions start at |
| `memex-view-metadata-width` | natnum | `12` | column metadata values align at |
| `memex-view-indent` | natnum | `2` | columns a section is indented from its parent |
| `memex-view-message-padding` | natnum | `3` | pixels between the window edge and each entry, graphical frames only |
| `memex-view-source-marks` | alist | `claude`: Nerd Font `nf-cod-claude`, then `✳`; `codex`: `nf-cod-openai`, then `⌬` | mark and face drawn before an agent's name, by source; the first displayable candidate wins |
| `memex-view-nerd-font` | `auto`, `t` or `nil` | `auto` | whether a mark may be a Nerd Font glyph; `auto` on graphical frames only |

### Entries (memex-entry.el, group `memex-entry`)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-entry-failure` | regexp | test failures, `Traceback`, `panicked at`, `command not found`, `error:` at line start, and similar | output that marks a call `▴` |
| `memex-entry-skipped` | regexp | output opening with `[Request interrupted by user` or the denial message | output that marks a call `⊘` |
| `memex-entry-injected` | regexp | text opening with a tag, a caveat, a skill's base directory or a continuation note | how a `user` record the harness wrote opens |
| `memex-entry-quoting` | list of strings | `("Read" "Glob" "Grep" "NotebookRead" "ToolSearch" "WebFetch" "WebSearch")` | tools whose output is never read for failure |
| `memex-entry-principal` | alist | Bash, BashOutput, KillShell to `command`; Write to `content`; Edit to `new_string`; MultiEdit to `edits`; NotebookEdit to `new_source` | the argument that always gets a section of its own |
| `memex-entry-payload-threshold` | natnum | `200` | length past which any argument gets a section of its own |

### herdr (memex-herdr.el)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-resume-lookup-limit` | natnum | `5000` | recent sessions read to find a resume command |
| `memex-herdr-start-timeout` | natnum | `20000` | milliseconds herdr waits for a resumed agent |
| `memex-herdr-display-action` | `display-buffer` action | `nil` | how a transcript opened through the bridge is shown |

### Anchor (memex-anchor.el, group `memex-anchor`)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-anchor-target` | `terminal` or `transcript` | `terminal` | where a record whose live pane is known opens |
| `memex-anchor-takeover` | boolean | `t` | the attached terminal accepts input while its window has focus |
| `memex-anchor-window` | natnum | `45` | consecutive letters that must match on screen |

### Org (memex-org.el)

| Option | Type | Default | Meaning |
| --- | --- | --- | --- |
| `memex-org-capture-template` | string or nil | `nil` | `org-capture` template key; nil puts the excerpt on the kill ring and in a buffer |

## Faces

### Dashboard

| Face | Used for |
| --- | --- |
| `memex-status-label` | a session's subject |
| `memex-status-project` | a repository, and the field names of an expanded row |
| `memex-status-path` | a directory |
| `memex-status-meta` | size, age, source and separators |
| `memex-status-narrowing` | the filters in the heading |

### Transcript

| Face | Used for |
| --- | --- |
| `memex-view-human` | the ground of a person's turn |
| `memex-view-assistant` | the ground of an agent's turn |
| `memex-view-tool-ground` | the ground of a tool call |
| `memex-view-system` | the ground of harness text |
| `memex-view-human-label` | the `user` label |
| `memex-view-agent-label` | an agent's name |
| `memex-view-source-claude` | Claude's mark |
| `memex-view-source-codex` | Codex's mark |
| `memex-view-nerd-glyph` | a Nerd Font mark, scaled to the text |
| `memex-view-tool-run` | a tool that ran something |
| `memex-view-tool-read` | a tool that looked something up |
| `memex-view-tool-write` | a tool that changed a file |
| `memex-view-tool-agent` | a tool that handed work to an agent, or a skill |
| `memex-view-tool-net` | a tool that went to the network |
| `memex-view-tool` | a tool of no known class |
| `memex-view-warning` | a call that reported an error, and the error count |
| `memex-view-ok` | a kind shown whole, in the kinds menu |

A search highlights the query with `hi-yellow` through `memex-search-highlight-with-hi-lock`.

## Hooks

| Hook | Called with | When |
| --- | --- | --- |
| `memex-view-shown-functions` | the transcript buffer | after a transcript is shown with point on its record |

## Buffers

| Buffer | Holds |
| --- | --- |
| `*memex status*` | the dashboard; `*memex status: PROJECT*` for a project |
| `*memex session ID (PATH)*` | a transcript, one per session |
| `*memex session preview*` | the search preview |
| `*memex usage*` | the usage report; `*memex usage: PROJECT*` for a project |
| `*memex record*` | a record's fields, from embark's `v` |
| `*memex excerpt*` | an Org excerpt, without a capture template |

## Lisp interface

### Reading from the minibuffer (memex-completion.el)

| Function | Returns |
| --- | --- |
| `memex-read-record` `&optional prompt records` | a record alist chosen from recent records, or from RECORDS |
| `memex-read-session` `&optional prompt records` | the newest record of a chosen session |
| `memex-read-project` `&optional prompt records` | a project name |

Each fetches synchronously, waiting up to `memex-completion-timeout` seconds.

### Opening

| Function | Does |
| --- | --- |
| `memex-search-in-sessions` `scope &optional mode initial` | search the sessions SCOPE names, a list of plists of `:source`, `:session-id` and `:source-path`; nil searches everything |
| `memex-view-session` `session-id source-path &optional doc-id display` | show a transcript at DOC-ID; DISPLAY, if given, is called with the buffer instead of `display-buffer` |
| `memex-view-record-buffer` `record name` | render one record alone into buffer NAME |
| `memex-herdr-open-session` `session-id source-path &optional doc-id` | show a transcript through the herdr bridge's display |
| `memex-herdr-open-agent` `agent` | show the transcript of a herdr agent row |
| `memex-herdr-session-scope` `reference &optional directory` | the scope element for a herdr `agent_session` reference |
| `memex-org-link` `record` | an Org link string to RECORD |
| `memex-org-follow` `path` | open a `memex:` link path |

### Requests (memex-core.el, memex-api.el)

`memex-rpc` `op fields callback &optional errback` runs one request and returns its process;
`memex-cancel-rpc` `process` abandons it without calling either callback.

memex-api.el wraps each operation. Each function takes its required arguments, then a callback, then
keyword arguments including `:errback`, and returns the request process.

| Function | Operation |
| --- | --- |
| `memex-api-ping` | `ping` |
| `memex-api-search` | `search` |
| `memex-api-recent` | `recent` |
| `memex-api-sessions` | `sessions`, at most `memex-api-max-sessions` (500) |
| `memex-api-session-count` | `session_count` |
| `memex-api-session` | `session` |
| `memex-api-session-page` | `session_page`, at most `memex-api-max-session-page-size` (500) records |
| `memex-api-session-batch` | `session_batch`, at most `memex-api-max-session-batch-size` (32) pages |
| `memex-api-show` | `show` |
| `memex-api-index` | `index` |
| `memex-api-usage` | `usage` |
| `memex-api-usage-activity` | `usage_activity` |
| `memex-api-session-activity` | `session_activity` |

## Errors

| Error | Signalled when |
| --- | --- |
| `memex-error` | the binary is missing or a request does not encode; parent of the rest |
| `memex-rpc-error` | memex answered with an error |
| `memex-transport-error` | memex exited non-zero or answered with something other than a response |
| `memex-protocol-error` | memex answered under another protocol version |
| `memex-api-limit-error` | a request exceeds one of memex's caps; also a `user-error` |

## Obsolete names

memex-markdown.el keeps aliases for the renderer that moved to lectio in 0.2.0:
`memex-markdown-render`, `memex-markdown-render-all`, `memex-markdown-html` and the face
`memex-markdown-code`, which point at `lectio-render`, `lectio-render-all`, `lectio-html` and
`lectio-code`.
