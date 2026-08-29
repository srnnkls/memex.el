# memex.el

Emacs client for [memex](https://github.com/nicosuave/memex), which indexes the
conversation history of coding agents. Speaks memex's `rpc` command — whose wire
is a protocol-1 envelope, `{"protocol":1,"request":{"op":…}}`, and not JSON-RPC —
puts the index up in the minibuffer and in Emacs buffers, and takes an indexed
session back to a live agent.

```
M-x memex-search        search the index, as you type where consult is installed
M-x memex-view-session  read a whole session's transcript in a buffer
M-x memex-usage         report memex's token usage
M-x memex-herdr-resume  resume the session in a herdr tab
M-x memex-anchor-show   read the record where the live agent drew it
```

## Layers

| File | What it is |
| --- | --- |
| `memex-core.el` | RPC transport — `memex-rpc`, `memex-executable`, errors, customs |
| `memex-api.el` | one wrapper per operation of memex's RPC surface, eleven of them |
| `memex-completion.el` | `memex-read-record`, `memex-read-session`, `memex-read-project` |
| `memex.el` | `memex-search`, over core, api, completion, view and usage — a leaf no other module requires |
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
  :commands (memex-search memex-view-session memex-usage))

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
layer and nothing besides. `memex-org.el` is the exception — it requires `ol`,
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
slash commands, file mentions, task notifications — is filtered out. `f` opens
the transient that moves any of the four between whole, heading and gone; the
header line names whatever is not whole. Filtering is `buffer-invisibility-spec`
rather than a second render, so the text never leaves the buffer and a search
still reaches it.

Every heading is a glyph, a label and a description at fixed columns, so a run
of entries reads down an edge rather than as a paragraph:

```
● you
● claude
✔ bash       Find checkpoints
✔ read       config.yaml
▲ bash       Verify RED    Ran 7 tests, 0 as expected, 7 unexpected
```

Each kind of entry is laid on a ground of its own and each tool wears the colour
of what it does — green ran something, blue looked something up, orange changed
a file, purple handed work to another agent. What a call took, when it ran and
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
