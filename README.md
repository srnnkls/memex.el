# memex.el

Emacs client for [memex](https://github.com/nicosuave/memex), which indexes the
conversation history of coding agents. Speaks memex's `rpc` command — whose wire
is a protocol-1 envelope, `{"protocol":1,"request":{"op":…}}`, and not JSON-RPC —
puts the index up in the minibuffer and in Emacs buffers, and takes an indexed
session back to a live agent.

```
M-x memex-status        list the sessions memex indexed, newest first
M-x memex-search        search the index, as you type where consult is installed
M-x memex-view-session  read a whole session's transcript in a buffer
M-x memex-usage         report memex's token usage
M-x memex-herdr-resume  resume the session in a herdr tab
M-x memex-anchor-show   read the record where the live agent drew it
```

## Dashboard

`memex-status` lists the sessions memex indexed, newest activity first: one
collapsible row per session over its age, size, source, repository and
subject, expanding into the identity memex holds for it.

| Key | Action |
| --- | --- |
| `RET` / `o` | Read the transcript |
| `r` | Resume it in a herdr tab |
| `w` | Copy the command that resumes it |
| `s` | Search — the session at point, or every session listed |
| `S` | Search menu: scope and retrieval mode |
| `f` | Narrow what memex is asked for |
| `O` | Order the rows |
| `L` | How many rows to ask for |
| `g` | Refresh |
| `?` | A menu of these same keys |

`n`, `p`, `TAB` and `M-1`..`M-4` come from `magit-section-mode-map`.

`f` narrows the *request* — source, project, directory, activity since, and
the origin subset — because memex applies a limit after its filters, so
narrowing in the buffer would leave fewer rows than asked for. `O` orders
what came back and spends no request: memex answers newest first and offers
no sort key, so ordering by size shows the largest of the sessions asked
for, not the largest memex knows. `L` is how wide that window is, capped at
`memex-api-max-sessions`.

Search is scoped to what point stands for. On a row that is one session; off
a row it is every session listed, which makes a narrowed dashboard a way to
pick the sessions a search runs over.

## Search

`memex-search-messages` lists individual matches; `memex-search-sessions`
lists one match per session. `memex-search` starts according to
`memex-search-group-by-session` (sessions by default).

A search is narrowed from the minibuffer it is read in, where `memex-search-map`
is active:

| Key | Action |
| --- | --- |
| `M-m` | Cycle lexical, semantic and hybrid retrieval |
| `M-g` | Switch between matching messages and sessions |
| `M-r` | Put roles in or out of the search at one key each |
| `M-t` | Ask for every role, or go back to the chosen set |
| `M-.` | Search the messages of the selected candidate's session |

Each restarts the search, keeping the query, the retrieval mode, the grouping,
the session scope and the roles. The prompt names every narrowing in force, so
`memex lexical messages [user+assistant]:` is a query put to the conversation.

`memex-search-roles` is the set a search starts from, `("user" "assistant")` by
default: four fifths of the index is tool traffic, and a query is nearly always
put to the conversation rather than to the calls that carried it out. `M-r`
opens a menu with a key per role — `u` user, `a` assistant, `c` tool_use,
`r` tool_result — that stays up while the set is put together; `SPC` takes every
role, `DEL` goes back to `memex-search-roles`, and `RET` restarts the search
once. `M-t` swings between the set and all of it. memex takes the roles as a set and applies them in the
index, so the page that comes back is a page of those roles rather than a page
of the corpus with the rest dropped. This needs a memex built with `roles` in
its search spec; against an older binary the set is ignored and every role comes
back. Selecting a candidate opens the transcript at the match, including tool
results.

Run a search command again to search the whole index. Without Consult, search
fetches once before selection, under the same keys.

Rows show project, source/role, and a highlighted match excerpt.
`memex-search-project-width`, `memex-search-identity-width`, and
`memex-search-snippet-width` cap their display widths; excerpts also fit the
narrowest window the minibuffer is shown in, which is the vertico-buffer
window where that is up and the miniwindow otherwise. Line-break escapes and
terminal colors are removed from candidate text only.

The margin carries how many hits a session row stands for and how long ago the
record was written, right-aligned under Marginalia. With Consult, the
highlighted candidate is drawn whole in `*memex preview*` — the excerpt in the
row is a window into it — at `memex-search-preview-key`, `C-SPC` by default.
Each preview renders a record, so the selection does not drag one along on its
way to the row that was wanted; set `memex-search-auto-preview` for a preview
that follows the selection.

`memex-search-session-hit` chooses the session's opening match and excerpt:
`newest` (default) or `best`. This selects among returned hits; it does not
change session ranking or fetch every match in the index.

## Layers

| File | What it is |
| --- | --- |
| `memex-core.el` | RPC transport — `memex-rpc`, `memex-executable`, errors, customs |
| `memex-api.el` | one wrapper per operation of memex's RPC surface, twelve of them |
| `memex-completion.el` | `memex-read-record`, `memex-read-session`, `memex-read-project` |
| `memex.el` | `memex-search`, over core, api, completion, view and usage — a leaf no other module requires |
| `memex-status.el` | the session dashboard, a magit-section list |
| `memex-view.el` | the whole-session transcript viewer, a magit-section tree |
| `memex-usage.el` | the token usage report |
| `memex-herdr.el` | the resume bridge — an indexed session back into a herdr tab |
| `memex-anchor.el` | the scrollback bridge — an indexed record back to the live agent's screen |
| `memex-markdown.el` | markdown to HTML to shr |
| `memex-org.el` | the `memex` link type and capture out of a record |
| `memex-embark.el` | embark actions on memex candidates |
| `memex-evil.el` | evil normal-state bindings for the viewer |
| `memex-tests.el` | the aggregate suite and the live contract |

## Install

```elisp
(use-package memex
  :load-path "~/projects/memex.el"
  :commands (memex-search memex-search-messages memex-search-sessions
             memex-view-session memex-usage))

(use-package memex-status
  :load-path "~/projects/memex.el"
  :commands memex-status)

(use-package memex-herdr :after herdr :commands memex-herdr-resume)
(use-package memex-anchor :after herdr :commands memex-anchor-show)
(use-package memex-org :after org)
(use-package memex-embark :after embark)
(use-package memex-evil :after evil)
```

Everything under the first block is optional, and so is every package it names.
`Package-Requires` is `((emacs "29.1"))` on eleven of the twelve files;
`memex-view.el` also requires `magit-section`, which the transcript is built out
of. herdr, embark, evil, consult and marginalia are each probed for at call time
or wired through `with-eval-after-load`, so an Emacs missing one of them loses that
layer and nothing besides. `nerd-icons-completion` is probed the same way and
gives each memex category its icon. A search row is wide, so it is worth giving
the category a window of its own:

```elisp
(with-eval-after-load 'vertico-multiform
  (add-to-list 'vertico-multiform-categories '(memex-record buffer)))
```
 `memex-org.el` is the exception — it requires `ol`,
`org` and `org-capture` at load time — and Org ships with Emacs. `memex-search`
reads a query and fetches once without consult, and searches as you type with it.

The memex binary itself is a real requirement — `memex-executable` names it.
Nothing else is: an agent's markdown is rendered by `memex-markdown.el` and shr.
Pandoc was used for this and dropped — it covers GFM whole, but costs around a
second per session whatever the session holds, and rendering 964 records went
from 1.68s to 0.66s without it. Tables, footnotes and task lists are the price
and show as their source.

The viewer is a magit-section tree: each turn is a section and each tool field a
section inside it, so `TAB`, `S-TAB` and `1`-`4` behave the way they do in
magit. `t` folds every tool field in the session at once, `n`/`p` move a whole
record at a time, and `a` reads the record point is on in the session that is
still running it — `g a` under evil, where `a` and `j` belong to normal state.
`memex-anchor-target` says whether that means the indexed transcript or the
agent's own terminal; it uses the live terminal by default. Memex joins the pane
whose agent reports the same session ID or transcript path, without guessing
identity from terminal content. Ghostel, vterm and Eat remain live observers,
take writable control and shared PTY geometry only while a selected Emacs
window has focus, then release both so Herdr's foreground client can reflow the
same terminal. The turn's header stays put in the header
line while you scroll through it, and a tool's input and output are fontified
in the mode its tool and file path imply.

A transcript opens on the conversation: what the person and the agent wrote is
shown whole and laid on grounds of its own, the calls that carried it out are a
heading each, and what the harness injected into somebody's turn — skill bodies,
slash commands, file mentions, task notifications — is filtered out. `U`, `A`,
`T` and `S` ask for the person, the agent, the calls or the harness whole and put
them back as they opened; `f` opens the transient that moves any of the four
between whole, heading and gone. Under evil these are `g U`, `g A`, `g T`, `g S`
and `g f`, since normal state spends the bare keys on its own verbs — load
`memex-evil.el` and every viewer key is under `g`. The header line names
whatever is not whole and
`memex-view-initial-states` sets what a transcript opens on. Filtering is
`buffer-invisibility-spec` rather than a second render, so the text never leaves
the buffer and a search still reaches it.

Every heading is a glyph, a label and a description at fixed columns, so a run
of entries reads down an edge rather than as a paragraph:

```
▌ user
▌ ✳ claude
▸ bash       Find checkpoints
▪ read       config.yaml
◂ edit       memex-view.el
⁄ skill      rfc-contract-audit
▹ agent      reviewer: the search changes
▴ bash       Verify RED    Ran 7 tests, 0 as expected, 7 unexpected
⊘ edit       memex-entry.el
```

A call is drawn as what the tool was for, and direction is the mnemonic: `▸`
points out of the session at work that left it, `◂` points back in at the
session changing your own files, so a transcript answers what it touched down
one column. `▪` only looked. `▹` is hollow because work handed to an agent
comes back with a transcript of its own, and `⁄` is the slash a skill is
invoked with. Two outcomes take the column from the tool: `▴`, the one glyph
anywhere pointing up, so trouble is found without reading, and `⊘` for a call
that never ran because you denied or interrupted it.

Anyone talking takes `▌` instead, one lane down the buffer for the prose, and
the mark of the agent who wrote a turn rides with its name rather than in the
lane. The name and the mark are padded together, so the clock holds its column
whether or not there is a mark.

Each kind of entry is laid on a ground of its own and the same classification
colours the tool — green ran something, blue looked something up, orange
changed a file, purple handed work elsewhere. What a call took, when it ran and
what it is called are there under `d`, not on every line by default.

Nothing memex indexes is reasoning: Claude stores its thinking blocks empty and
codex encrypts its reasoning items, so neither survives into the index and there
is no such thing to filter. The four kinds above are what a transcript is made
of. Telling a person's turn from an injected one is `memex-entry-injected`, a
regexp over what the record opens with — measured at 34 kept and 113 filtered
over 147 `user` records from 25 sessions, losing no question and leaking three
prose injections through.

## Tests

```bash
emacs -Q --batch -L . -l memex-tests.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l memex-<module>-tests.el -f ert-run-tests-batch-and-exit
```

The first loads all twelve per-module suites and runs everything, and every oracle
in them holds there: the ones asserting a module is absent measure that around
their own load or unbind the bridge for the branch under test. The second runs a
single suite while you work on that module and localises a failure the aggregate
reports; it is also how the herdr suite is run by hand with memex stripped from
`PATH`, and the evil suite with nothing else loaded beside it.

The waits are finite but load-aware. `memex-core-tests--wait` and
`memex-api-tests--wait` give a stub 10 seconds and push that deadline back for as
long as a memex process is still running, to a ceiling of 300 seconds; the
selector tests pin `memex-completion-timeout` at 300 seconds. So load stretches
the suite rather than making it lie, up to those bounds: the aggregate settles at
around 30 seconds and took over 6 minutes on a machine coming down from load
average 50. An outer timeout in CI has to sit above the 300-second inner ceiling,
or it pre-empts the test instead of bounding the command.

The fixtures record memex's wire format and pin what the client makes of it.
Three tests ask the installed memex whether it still sends that shape — the
recorded-payload contract, the API request round-trip and the core `ping` — and
each skips itself when memex is not on PATH, so a contributor without the binary
still gets a clean run of 3 skipped.
