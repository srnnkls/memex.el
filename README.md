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
```

## Layers

| File | What it is |
| --- | --- |
| `memex-core.el` | RPC transport — `memex-rpc`, `memex-executable`, errors, customs |
| `memex-api.el` | one wrapper per operation of memex's RPC surface, eleven of them |
| `memex-completion.el` | `memex-read-record`, `memex-read-session`, `memex-read-project` |
| `memex.el` | `memex-search`, over core, api, completion, view and usage — a leaf no other module requires |
| `memex-view.el` | the whole-session transcript viewer |
| `memex-usage.el` | the token usage report |
| `memex-herdr.el` | the resume bridge — an indexed session back into a herdr tab |
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
(use-package memex-org :after org)
(use-package memex-embark :after embark)
(use-package memex-evil :after evil)
```

Everything under the first block is optional, and so is every package it names.
`Package-Requires` is `((emacs "29.1"))` on all ten files and nothing else:
herdr, embark, evil, consult and marginalia are each probed for at call time or
wired through `with-eval-after-load`, so an Emacs missing one of them loses that
layer and nothing besides. `memex-org.el` is the exception — it requires `ol`,
`org` and `org-capture` at load time — and Org ships with Emacs. `memex-search`
reads a query and fetches once without consult, and searches as you type with it.

The memex binary itself is a real requirement — `memex-executable` names it.

## Tests

```bash
emacs -Q --batch -L . -l memex-tests.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l memex-<module>-tests.el -f ert-run-tests-batch-and-exit
```

The first loads all ten per-module suites and runs everything, and every oracle
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
