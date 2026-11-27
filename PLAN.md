# ECB Modernization Plan

Status as of 2026-11-26, based on byte-compiling and batch-running ECB 2.52
under GNU Emacs 30.2.

Baseline: all 33 files byte-compile with no errors (116 warnings), autoloads
generate, `ecb-activate`/`ecb-deactivate` work, all 26 built-in layouts switch
cleanly, and `ecb-advice-test.el` produces its expected transcript.

The steps below are ordered so that each one is independently reviewable and
leaves the tree green. Every step ends with `make ecb autoloads` on Emacs 30
and the test suite from step 0 passing.

---

## Step 0: Unit test proposal

**Status: done (2026-11-26, revalidated on Emacs 31.1 2026-11-27).** `test/`
holds an ERT suite of 502 tests across 16 files; `make test` runs it in
about 20-30 s on Emacs 30.2 and 31.1, from source or from `.elc`. 26 tests
are marked `:expected-result :failed` (two of them only on Emacs >= 31,
decided by a runtime probe rather than a version check); each one
documents a real bug in its docstring and flips to an unexpected *pass* when
the bug is fixed, so the suite stays green today and the fix steps below
cannot regress silently.

| File | Tests | Known-bug tests |
|---|---|---|
| `tree-buffer-test.el` | 53 | 0 |
| `ecb-util-test.el` | 51 | 2 |
| `ecb-common-browser-test.el` | 39 | 3 |
| `ecb-test.el` (activation, hooks, sync) | 27 | 1 |
| `ecb-layout-test.el` / `ecb-layout-defs-test.el` | 49 / 10 | 3 / 2 |
| `ecb-file-browser-test.el` | 55 | 2 |
| `ecb-method-browser-test.el` | 51 | 0 |
| `ecb-upgrade-test.el` | 47 | 1 |
| `ecb-advice-scenario-test.el` (port of `ecb-advice-test.el`) | 4 | 0 |
| `ecb-compilation-test.el` | 17 | 2 |
| `ecb-navigate-test.el` | 17 | 2 |
| `ecb-mode-line-test.el` | 10 | 0 |
| `ecb-symboldef-test.el` (+ analyse) | 16 | 1 |
| `ecb-misc-modules-test.el` (help, face, tod, speedbar, compatibility, winman, create-layout, buffertab, multiframe, jde, eshell, wrappers, autogen) | 44 | 6 |
| `ecb-integration-test.el` | 12 | 1 |

Known-bug tests (grep `:expected-result :failed` for the full docstrings):

- `ecb-help`: `ecb-problem-report-list-all-variables` uses the removed
  `user-variable-p` (Step 2).
- `ecb-layout`: `ecb-window-select` is passed a window object when maximizing
  from the compile window (Step 2); `ecb-split-edit-window-after-start`
  deletes the compile window; left-right layouts report an inconsistent
  maximize state.
- `ecb-common-browser`: `ecb-add-to-advice-set` mutates before rejecting an
  illegal class; `ecb-enable-ecb-advice` logs on every call.
- `ecb-util`: `ecb-some` ignores extra sequences; `ecb-merge-face-into-text`
  returns the text without the face.
- `ecb-file-browser`: `ecb-cache-directory-contents-not` does not accept a
  regexp list as documented; unsorted history is not most-recent-first.
- `ecb-method-browser` / `ecb-speedbar`: `ecb-get-tags-for-non-semantic-files`
  uses `cl-return` outside a `cl-block`, so any grouped imenu index (e.g.
  Ruby, Makefile) throws `no-catch` and empties the methods buffer.
- `ecb-compilation`: the predicate branch of `ecb-compilation-buffer-p` is a
  `dolist` with no result form, so `ecb-compilation-predicates` never match
  and lambda predicates error.
- `ecb-navigate`: `ecb-nav-remove-node` drops the tail when removing a
  middle node; `ecb-nav-dump-history` calls `int-to-string` on the sentinel.
- `ecb-upgrade`: layout number 20 maps to the nonexistent "speedbar1".
- `ecb-symboldef`: `ecb-symboldef-get-doc-for-vsymbol` returns "".
- `ecb-buffertab`: `ecb-buffertab-make-menu` has the `add-to-list` APPEND
  argument outside the call, so the result is not a keymap.
- `ecb-multiframe`: `ecb-multiframe-setup-buffer-name` references the free
  variable `frame`.
- `ecb-semantic-wrapper`: alist maps to `semantic-tag-type-superclass`
  (real name is plural), leaving a void alias.
- `ecb-autogen`: top-level `(set 'find-file-hook nil)` clobbers the user's
  hook list whenever the file is loaded.
- `ecb.el`: `ecb-find-file-hook` adds the self-removing
  `ecb-apply-default-tag-filter` to `post-command-hook` and deactivation
  never removes it.

Found by revalidating on Emacs 31.1 (both are regressions caused by Emacs
changes, and both hit users with default settings):

- `ecb-layout`: `ecb-other-window-behavior` defaults to the symbol `all`
  and `ecb-get-other-window` tests it with `functionp` before its
  `cl-case`. Emacs 31 added a builtin function `all`, so `C-x o`
  (`other-window`) signals `wrong-number-of-arguments` in every ECB session
  until the user customizes the option. Fix: check the known symbols before
  `functionp`, or rename the default. Promote to Step 2.
- `ecb-speedbar`: `ecb-speedbar-activate` calls `speedbar-disable-update`
  before the speedbar buffer exists; Emacs 31's `speedbar-set-timer` runs
  inside `speedbar-buffer`, so the `left-dir-plus-speedbar` layout (and any
  user layout with a speedbar window) fails to draw. Fix: create the buffer
  first or guard the call. Promote to Step 2.

Emacs 31 also changed `cl-block`'s expansion, so the `cl-return` bug in
`ecb-get-tags-for-non-semantic-files` now surfaces as `void-variable`
instead of `no-catch`; the tests assert the user-visible outcome (empty
methods buffer) and are unaffected.

Test-runner note: `test/run-tests.el` sets
`native-comp-enable-subr-trampolines` to nil. Tests stub primitives with
`cl-letf`, and on a native-comp Emacs that would JIT-compile a trampoline
at test time, which needs a working C toolchain and is unrelated to what is
being tested.

Not covered by tests (needs a window system or third-party packages):
mouse/popup handlers, image icons, the interactive drawing part of
`ecb-create-layout`, JDEE, escreen/winring, speedbar-in-ECB. Those modules
have load and graceful-degradation tests only.

The original proposal follows.

ECB has effectively no tests. `ecb-advice-test.el` is a manual "compare the
*Messages* buffer to a comment block" script. Before changing anything, build
a real suite so later steps (especially the advice migration) can be verified
mechanically.

### Framework and layout

- **ERT** (built into Emacs; no external dependency). Tests run with
  `emacs -Q --batch -L . -L test -l ert -l test/<file> -f ert-run-tests-batch-and-exit`.
- New directory `test/`, one file per source module:

  ```
  test/
    ecb-test-helpers.el        shared fixtures and macros
    tree-buffer-test.el
    ecb-util-test.el
    ecb-common-browser-test.el
    ecb-file-browser-test.el
    ecb-method-browser-test.el
    ecb-layout-test.el
    ecb-layout-defs-test.el
    ecb-compilation-test.el
    ecb-navigate-test.el
    ecb-upgrade-test.el
    ecb-advice-test.el         (rewritten from the existing manual script)
    ecb-semantic-wrapper-test.el
    ecb-speedbar-test.el
    ecb-eshell-test.el
    ecb-symboldef-test.el
    ecb-analyse-test.el
    ecb-cycle-test.el
    ecb-help-test.el
    ecb-face-test.el
    ecb-tod-test.el
    ecb-compatibility-test.el
    ecb-winman-support-test.el
    ecb-create-layout-test.el
    ecb-mode-line-test.el
    ecb-integration-test.el    end-to-end activate/drive/deactivate
    fixtures/
      project-a/               small source tree: .el, .c, .py, nested dirs,
                               hidden files, symlinks, an empty dir
      project-b/               second source path for multi-path tests
      custom-old-2.40.el       saved customizations in pre-upgrade format
      custom-old-2.50.el
  ```

- `Makefile` gains `test` (runs everything) and `test-unit` (everything except
  `ecb-integration-test.el`, for fast local loops). `make test` is what CI
  runs in step 1.
- Move `ecb-advice-test.el` out of `ecb_LISP_EL` in `ecb-makedef.mk` so test
  code is not shipped in the distribution tarball.

### Shared helpers (`test/ecb-test-helpers.el`)

- `ecb-test-with-temp-project (var &rest body)` — copies `fixtures/project-a`
  into a temp dir, binds `var` to it, cleans up afterwards. All filesystem
  tests use this; nothing touches the real home directory.
- `ecb-test-with-ecb (&rest body)` — binds `ecb-source-path` to the temp
  project, disables tip-of-the-day, upgrade prompts, and idle timers, calls
  `ecb-activate`, runs body, guarantees `ecb-deactivate` via `unwind-protect`.
  Also binds `ecb-compile-window-height` to a small number so compile-window
  tests work in the 80x24 batch frame.
- `ecb-test-with-layout (name &rest body)` — switches to layout `name` inside
  `ecb-test-with-ecb`.
- `ecb-test-window-tree ()` — returns a canonical sexp describing the current
  frame's window layout (buffer names + sizes) so layout tests can compare
  against expected structures.
- `ecb-test-tree-buffer-lines (buffer-name)` — returns the visible lines of
  a tree buffer with text properties stripped, for golden-output assertions.
- `ecb-test-with-custom-file (fixture &rest body)` — points `custom-file` at
  a copy of a fixture for upgrade tests.
- `ecb-test-no-user-input` — a `cl-letf` wrapper that makes `y-or-n-p`,
  `yes-or-no-p`, `read-string`, `completing-read` return canned values or
  signal, so no test can block on stdin (which is what hung the first manual
  sweep).

### What to test, per module

Priorities: **P1** is needed before touching that module in later steps;
**P2** is wanted before 2.6 release; **P3** is nice-to-have.

#### `tree-buffer.el` (P1, ~117 functions, foundation of every ECB window)

Pure data layer, no window required:
- `tree-node-new`, `tree-node-add-children`, parent/child links, `at-beginning`
  ordering.
- `tree-node-find-child-by-data/name`, `-by-name`, `-remove-child-by-data/name`,
  `tree-node-search-subtree-by-data`, `-by-id`, `tree-node-map-subtree`
  traversal order.
- `tree-node-indent-level`, `tree-node-update`, expandable/shrink-name flags.
- `tree-buffer-remove-elt`, id generation (`tree-node-id-init`, uniqueness).

Rendering layer, uses a real buffer in batch:
- Create a tree buffer with `tree-buffer-create`, add a known tree, call
  `tree-buffer-update`; assert exact line contents and indentation (golden).
- Expand/collapse a node, assert line count and `tree-buffer-get-node-at-point`
  at each line.
- `tree-buffer-find-displayed-node-by-data/name`,
  `tree-buffer-search-displayed-node-list`.
- Name-start column / point helpers (`tree-buffer-get-node-name-start-column`,
  `-start-point`, `-end-point`) against known indentation.
- Highlighting: `tree-buffer-highlight-node-by-data/name` sets an overlay on
  the right line; `tree-buffer-remove-highlight` clears it.
- Incremental search (`tree-buffer-incr-searchpattern`) narrows the displayed
  node list correctly for prefix and substring modes.
- Image icon fallback: `tree-buffer-add-image-icon-maybe` degrades to ASCII
  when `display-images-p` is nil (always true in batch).

#### `ecb-util.el` (P1, ~107 functions, mostly pure)

- List/sequence helpers: `ecb-member`, `ecb-subseq`,
  `ecb-delete-first-occurence-from-list`, `ecb-member-of-symbol/value-list`,
  `ecb-copy-list`, `ecb-set-difference`, `ecb-filter`, `ecb-last`,
  `ecb-first`, `ecb-nth` style helpers.
- String helpers: `ecb-string=`, `ecb-string<`, `ecb-string-trim`,
  `ecb-string-make-singular`, regexp list matching
  (`ecb-match-regexp-list`).
- `ecb-option-get-value` for each supported `:type` and for values set via
  `customize` vs `setq`.
- `ecb-normalize-number` boundary cases (fractions vs absolute, zero,
  negative).
- Filename helpers: `ecb-fix-filename` (trailing slash, env-var substitution,
  `~` expansion, Windows drive letters, double slashes), `ecb-file-exists-p`
  on remote-looking paths, `ecb-directory-sep-char` handling.
- `ecb-with-readonly-buffer`, `ecb-with-dedicated-window`,
  `ecb-exec-in-window` macros: body runs, restores state on non-local exit.
- Timer helpers: `ecb-run-with-idle-timer`/`ecb-cancel-timer` register and
  deregister (assert on `timer-idle-list` membership, do not wait).
- Stack/ring helpers in `ecb-navigate.el` get their own file (below).

#### `ecb-common-browser.el` (P1)

- Advice-set machinery (**the gate for step 4**):
  `defecb-advice-set`, `defecb-advice` (legacy), `ecb-add-to-advice-set`
  (new), `ecb-enable-advices`, `ecb-disable-advices`,
  `ecb-with-original-adviced-function-set`, `ecb-with-ecb-advice`,
  permanent sets, `ecb-always-disabled-advices` refusal to enable, nested
  `with-original` calls (the "outmost-caller" bookkeeping). Assert via
  `advice-member-p` / `ad-is-active` rather than parsing messages.
- Error cases: unknown advice set, illegal advice class (must *not* mutate
  the set before erroring — currently it does).
- Mixed-format sets: a set containing both a legacy `(fn . class)` entry and
  a new `(fn :class advice-fn)` entry enables/disables both. This is what
  keeps step 4 safe while it is half done.
- `ecb-fix-filename` lives here too; see above.
- Buffer registry: `defecb-tree-buffer-creator`, `ecb-ecb-buffer-registry-*`
  (name lookup, set-fcn lookup, tree-buffer-p), `ecb-buffer-is-ecb-buffer-of-current-layout-p`.
- `defecb-autocontrol/sync-function` generates a function that respects
  `ecb-basic-buffer-sync` option semantics (`t`, `nil`, list of modes,
  `'always`) — table-driven test.
- `ecb-window-select` accepts buffer, buffer-name, and (after the fix) a
  window; returns nil when not displayed.
- Popup-menu entry generation (`ecb-create-menu-user-ext-type`,
  `ecb-menu-entry-*`) returns well-formed easymenu lists for the documented
  option shapes.

#### `ecb-file-browser.el` (P1, ~130 functions)

Against the temp fixture project:
- Directory tree build: top-level nodes equal `ecb-source-path` entries;
  aliases from `(path . alias)` entries show the alias.
- `ecb-excluded-directories-regexps`, `ecb-source-file-regexps`,
  `ecb-sources-exclude-cvsignore` filtering; hidden files; symlink to a dir.
- `ecb-prescan-directories-for-emptyness` marks an empty dir unexpandable.
- `ecb-expand-directory-nodes` with level 0/1/2 on the fixture produces the
  expected visible lines; add a test with a `max-depth` guard once it exists.
- Sources buffer content for a selected dir; sort order per
  `ecb-sources-sort-method` (`name`, `extension`, nil) and
  `ecb-sources-sort-ignore-case`.
- `ecb-set-selected-directory` / `ecb-set-selected-source` update
  `ecb-path-selected-directory`/`-source` and highlight the right node.
- History: add, `ecb-clear-history`, `ecb-history-sort-method`,
  `ecb-history-exclude-file-regexps`, bucketing by mode/dir/extension
  (`ecb-history-make-buckets`), max length.
- Caches: `ecb-files-and-subdirs-cache-*`, `ecb-sources-cache-*`,
  `ecb-remote-path-cache-*`, `ecb-vc-cache-*` add/get/remove/clear; cache
  invalidation when a dir is touched (`ecb-directory-update-speedbar` path).
- `ecb-remote-path` parsing for tramp (`/ssh:host:/p`,
  `/ssh:user@host#22:/p`, method-less), ange-ftp style, and a plain local path
  (returns `NOT-REMOTE` in the cache).
- `ecb-get-file-info-text` format for a known file (`cl-letf` the
  attributes).
- VC support hooks: `ecb-vc-dir-managed-by-*` detection against a fixture
  with a fake `.git`/`.svn` dir; `ecb-vc-state` mapping to faces/icons with a
  stubbed `vc-state`.
- Node-info/tooltip text (`ecb-mouse-over-directory-node`, `-source-node`,
  `-history-node`) for each `ecb-*-show-node-info` setting.
- Popup menu actions that are pure: delete/create/rename helpers with the
  user prompt stubbed to confirm, against the temp dir.

#### `ecb-method-browser.el` (P1, ~117 functions)

Use `semantic/bovine/el` on the fixture `.el` files (always available) plus
`imenu` and etags fallbacks:
- `ecb-rebuild-methods-buffer` on a known file yields expected tag names and
  nesting; golden lines.
- `ecb-show-tags` option: filter by tag class per major mode, `collapsed` /
  `expanded` / `flattened` / `hidden` display modes, `ecb-tag-display-function`
  variants, `ecb-type-tag-display`, `ecb-type-tag-expansion`.
- Bucket grouping (`ecb-add-tag-bucket`, `ecb-bucket-node-display`),
  `ecb-methods-nodes-expand-spec` / `-collapse-spec`, `ecb-expand-methods-nodes`
  levels.
- Sorting: `ecb-methods-sort-method`? (only if it exists) and
  `ecb-post-process-semantic-taglist` hooks run in order.
- Filters: `ecb-methods-filter-regexp`, `-protection`, `-tag-class`,
  `-function`, `-current-type`, stacking, `ecb-methods-filter-delete-last`,
  `ecb-methods-filter-none`. Assert displayed tag set after each.
- Fallback paths: a buffer with no semantic support uses imenu
  (`ecb-non-semantic-methods-initial-expand`, `ecb-process-non-semantic-files`),
  a file with a TAGS table uses etags; `ecb-non-semantic-exclude-modes`.
- Sync behaviour: `ecb-method-buffer-sync` on a buffer change, cache hit vs
  rebuild (`ecb-methods-buffer-needs-rebuild-p`), narrowing
  (`ecb-methods-buffer-sync` with `ecb-method-buffer-needs-rebuild`).
- `ecb-tag-visit-post-actions` and `ecb-jump-to-tag` move point to the tag
  start; `ecb-tag-visit-smart-tag-start`, `-recenter`, `-highlight-tag-header`
  individually.
- Current-tag tracking: `ecb-highlight-tag-with-point` modes (`nil`,
  `highlight`, `highlight-scroll`) select the right node when point moves.
- `ecb-get-real-curr-tag`, `ecb-get-tag-name` /
  `ecb-get-tag-parent-names` for nested types.

#### `ecb-layout.el` (P1, ~127 functions)

Everything runs in the batch frame (80x24; override with
`set-frame-size` where a test needs more room):
- `ecb-layout-switch` to every entry in `ecb-available-layouts`; for each,
  assert `ecb-test-window-tree` matches a stored golden sexp (generated once,
  reviewed, committed). This locks the layout engine.
- `ecb-redraw-layout-full`, `-quickly`, `ecb-redraw-layout-preserving-compwin-state`.
- Window size persistence: `ecb-store-window-sizes`, `ecb-restore-window-sizes`,
  `ecb-restore-default-window-sizes`, `ecb-layout-window-sizes` per layout,
  fractional vs absolute sizes.
- `ecb-toggle-ecb-windows`, `ecb-hide-ecb-windows`, `ecb-show-ecb-windows`;
  `ecb-windows-hidden-p` state machine.
- Maximize: `ecb-maximize-ecb-buffer` for each ECB buffer, with point in an
  edit window, in an ECB window, and **in the compile window** (the
  `ecb-window-select` bug), `ecb-undo-maximize-ecb-buffer`,
  `ecb-cycle-maximized-ecb-buffers`, `ecb-maximize-next-after-maximized-select`.
- Compile window: `ecb-toggle-compile-window`, `-height`,
  `ecb-compile-window-state` (`hidden`/`visible`), `ecb-compile-window-temporally-enlarge`
  variants, `ecb-enlarged-compilation-window-max-height`,
  `ecb-compile-window-width` (`frame` vs `edit-window`), persistent vs
  non-persistent (`ecb-compile-window-height` nil).
- Edit-window operations under advice: `split-window-vertically`,
  `-horizontally`, `delete-window`, `delete-other-windows`,
  `other-window`, `switch-to-buffer-other-window`, `display-buffer`,
  `balance-windows` behave per the docstrings when the ECB frame is active,
  and are unchanged in a second frame. These are the highest-value tests for
  step 4; write them before converting `ecb-layout.el`.
- `ecb-canonical-edit-windows-list`, `ecb-canonical-ecb-windows-list`,
  `ecb-point-in-edit-window-number`, `ecb-where-is-point` classification for
  every window type.
- `ecb-layout-define` with a user-defined layout (from `ecb-examples.el`)
  registers and renders; `ecb-layout-undefine` removes it.
- `ecb-ignore-special-display`, `ecb-ignore-display-buffer-function`,
  `ecb-ignore-pop-up-frames` option matrix via `display-buffer`.
- `ecb-split-edit-window-after-start` and `ecb-layout-always-operate-in-edit-window`.
- `ecb-handle-major-mode-visibilty` with `ecb-major-modes-show-or-hide`.
- Mode-line / divider: `ecb-fix-window-size`, `ecb-mode-line-prefixes`,
  `ecb-mode-line-data` via `ecb-mode-line.el` tests.

#### `ecb-layout-defs.el` (P1)

- Each `ecb-layout-define` form registers a layout; `ecb-buildin-layouts`
  equals the set of all defined names; layout types (`left`, `right`, `top`,
  `left-right`) match `ecb-layout-get-type`.

#### `ecb-compilation.el` (P2)

- `ecb-compilation-buffer-p` for `ecb-compilation-buffer-names`,
  `ecb-compilation-major-modes`, `ecb-compilation-predicates`, both match and
  no-match; `ecb-compilation-get-buffers` ordering;
  `ecb-compilation-registered-buffer-p`.

#### `ecb-navigate.el` (P2)

- `ecb-nav-add-item`, `ecb-nav-goto-previous`, `-next`, boundary messages,
  invalidation when a buffer is killed (`ecb-nav-is-valid`), max history,
  `ecb-nav-position-goto` restores point.

#### `ecb-upgrade.el` (P1 before any option is renamed)

- `ecb-upgradable-option-alist` driven: for every entry, load a custom-file
  fixture with the old name/value, run `ecb-upgrade-options`, assert the new
  option has the converted value and the old one is reported in
  `ecb-upgraded-options`. Run the upgrade twice and assert idempotence.
- `ecb-not-compatible-options` detection for a value of the wrong `:type`.
- `ecb-check-not-compatible-options` renames/warns; `ecb-reset-not-compatible-options`.
- Version comparison helpers (`ecb-package-version-list<`,
  `ecb-package-version-str2list`) across `2.40`, `2.50beta2`, `2.52`, and
  cedet-style versions.
- `ecb-download-*` and web functions: smoke only, network stubbed.

#### `ecb-advice-test.el` → ERT (P1)

- Port each `LOC-n` block of the existing script into an `ert-deftest` that
  asserts the sequence of calls via a recording list instead of `message`.
  Keep the legacy transcript test until step 4 is complete, then delete it.

#### `ecb-semantic-wrapper.el`, `ecb-semantic.el`, `ecb-cedet-wrapper.el` (P2)

- Each `ecb--semantic-*` wrapper delegates to the right `semantic-*` function
  with the same args (use `cl-letf` to capture). This decides whether the
  wrapper layer can be deleted (TODO item).
- `ecb-cedet-required-version` checks accept the bundled CEDET and reject an
  absurdly old version string.

#### `ecb-speedbar.el` (P2)

- `ecb-speedbar-activate`/`-deactivate` create and remove the speedbar
  buffer, add it to the layout; `ecb-speedbar-update-contents` on a directory
  and a file; `ecb-speedbar-before-activate` restores `speedbar-frame`
  bindings. Advised `speedbar-*` functions behave per docstring.

#### `ecb-eshell.el` (P2)

- `ecb-eshell-recenter`, `ecb-eshell-current-buffer-sync` changes eshell's
  `default-directory` to the selected source's dir without echoing the `cd`;
  `ecb-eshell-fit-window-to-output` resizes within
  `ecb-eshell-enlarge-when-eshell` limits; integration hooks installed and
  removed.

#### `ecb-symboldef.el`, `ecb-analyse.el` (P2)

- `ecb-symboldef-find-definition` for an Elisp function and variable in the
  fixture; `ecb-symboldef-find-functions` dispatch per mode; the temp-buffer
  doc rendering without `eieio-help-mode-augmentation-maybee`.
- `ecb-analyse-buffer-sync` on a known Elisp buffer fills the expected
  buckets (`Context`, `Prefix`, `Completions`, `Local Variables`);
  `ecb-analyse-collapsed-buckets` respected; `ecb-analyse-fontified-buckets`.

#### `ecb-cycle.el`, `ecb-help.el`, `ecb-face.el`, `ecb-tod.el`,
`ecb-mode-line.el`, `ecb-compatibility.el`, `ecb-winman-support.el`,
`ecb-create-layout.el`, `ecb-buffertab.el`, `ecb-multiframe.el`,
`ecb-jde.el` (P2/P3)

- `ecb-cycle-through-compilation-buffers` with 0, 1, 3 compile buffers,
  wraps around, respects a hidden compile window.
- `ecb-help`: `ecb-submit-problem-report` builds a buffer containing every
  option group (this is the `user-variable-p` regression test);
  `ecb-show-help` gives a clear error when `info-help/` is absent.
- `ecb-face`: every `ecb-*-face` defcustom has exactly one `:type` and
  resolves through `ecb-face-default`; `ecb-face-default` height/inherit
  arguments.
- `ecb-tod`: cursor file round-trip in a temp `ecb-tip-of-the-day-file`,
  wraparound at end of tips.
- `ecb-mode-line`: `ecb-mode-line-format` output for each ECB buffer with
  `ecb-mode-line-data` variants (`sel-dir`, `sel-source`, a lambda).
- `ecb-compatibility`: `bs-show`, `Electric-pop-up-window`, `one-window-p`
  advice behaviour in the ECB frame and in another frame.
- `ecb-winman-support`: winring/escreen hooks register and deregister;
  behaviour with the packages absent (`featurep` guards).
- `ecb-create-layout`: non-interactive core (`ecb-create-layout-generate-lisp`
  style functions) produce a valid `ecb-layout-define` form from a recorded
  split sequence; `ecb-create-layout-save-layout` writes to a temp
  `ecb-create-layout-file`.
- `ecb-buffertab`: menu construction only.
- `ecb-multiframe`: with a second frame created in batch, `ecb-activate` in
  frame 2 leaves frame 1 untouched; deactivate restores both.
- `ecb-jde`: loads without JDE; every entry point errors cleanly when JDE is
  absent.

#### `ecb.el` (P1)

- `ecb-activate` → `ecb-minor-mode` is t, hooks installed (list them and
  assert membership), advices enabled, idle timers registered, `ecb-frame`
  set; `ecb-deactivate` undoes every one of those and restores the saved
  window configuration (`ecb-last-window-config-before-deactivation`).
- Activate twice is a no-op; deactivate twice is a no-op; activate with
  `ecb-source-path` nil falls back sensibly.
- `ecb-activate-hook`, `ecb-before-activate-hook`, `ecb-deactivate-hook`,
  `ecb-before-deactivate-hook` run in order; a hook that signals does not
  leave ECB half-active (`ecb-activate` currently has an `unwind-protect`
  for this — test it).
- `ecb-minor-mode` keymap: every binding in `ecb-common-map` under
  `ecb-key-map` prefix resolves to a bound command.
- Byte-compile sanity: `ecb-byte-compile` compiles every file in
  `ecb_LISP_EL` without `Error` lines (this doubles as the "did every file
  load" test).

#### `ecb-integration-test.el` (P1, slow)

The hand-run sweep made permanent:
1. activate with the fixture project;
2. open a fixture file, assert directories/sources/methods/history contents;
3. switch through every layout;
4. compile, cycle compilation buffers, maximize each ECB window from the
   compile window, undo;
5. toggle windows, compile-window height, expand directory nodes to level 2;
6. run `ecb-submit-problem-report` to a buffer;
7. deactivate; assert the frame has exactly the pre-activation windows and
   no ECB timers or advices remain.

### Coverage goal and sequencing

- Target: every `defun` in the P1 modules has at least one test that calls it
  directly or through a documented entry point; P2 modules have entry-point
  coverage; P3 modules have load + smoke coverage. Measure with
  `testcover.el` or `undercover.el` on the P1 files and record the number in
  CI output (do not gate on it initially).
- Order of writing: helpers → `tree-buffer` → `ecb-util` → `ecb-common-browser`
  (advice) → `ecb.el` activation → integration sweep. That is the minimum set
  needed before step 2. Then `ecb-layout`, `ecb-file-browser`,
  `ecb-method-browser`, `ecb-upgrade`, then the rest in step order.
- Golden outputs (tree lines, window trees) are stored as data under
  `test/golden/` with a `make test-update-golden` target that regenerates
  them; a regenerated diff must be reviewed like code.
- Tests run on Emacs 29 and 30 from day one (step 1 matrix); anything that
  only passes on one version is a bug or a test bug, not a skip.

### Known constraints

- Batch Emacs has one 80x24 frame and no display. Everything in ECB that was
  exercised during the survey worked under those conditions; tests that
  truly need a window system (`ecb-create-layout` interactive parts, image
  icons, mouse handlers) are limited to their non-interactive cores.
- Nothing may read stdin. The helper macro stubs all prompts; CI runs with
  `</dev/null` so a missed prompt fails fast instead of hanging.
- Tests must never touch `~`: `ecb-source-path`, `custom-file`,
  `ecb-tip-of-the-day-file`, `ecb-create-layout-file`, and
  `user-emacs-directory` are all bound to temp locations by the helpers.

---

## Step 1: CI

**Status: done.** `.github/workflows/build.yml` runs on push to `master` and
on pull requests, with a matrix of Emacs 29.4, 30.1 and 31.1 via
`purcell/setup-emacs`. Each job does `make ecb autoloads` (hard byte-compile
errors fail the job; warnings are printed but not fatal until step 5), then
`make test` against the `.elc` files, then `make clean && make test` from
source. `scorecard.yml` is unchanged.

Follow-ups once the first run is green:
- Consider adding `snapshot` to the matrix as a non-blocking job.
- `ecb-advice-test.el` was removed from `ecb_LISP_EL` in `ecb-makedef.mk`
  because its scenario now lives in `test/ecb-advice-scenario-test.el`; the
  file itself can be deleted in step 6.

## Step 2: Fix the two confirmed crashes

- `ecb-help.el:389` — `user-variable-p` was removed from Emacs; use
  `custom-variable-p`. Breaks `ecb-submit-problem-report`.
- `ecb-layout.el:3553`, `:3623`, `:3662` — `ecb-window-select` takes a
  buffer-or-name but is passed the window object `ecb-compile-window`.
  Crashes any maximize of an ECB window while point is in the compile
  window. Fix with one shared helper used at all three sites.
- Both have tests from step 0 that fail before and pass after.

## Step 3: Small defcustom / constant-list fixes

- `ecb-face.el:155` — `ecb-sources-general-face` has two `:type` keywords;
  drop the stray `:type 'face`.
- `ecb-buffertab.el:69` — `ecb-buffertab-map` is a `defcustom` holding a
  keymap with no `:type`; make it a `defvar`.
- `ecb-help.el:365-391` — `sort`/`delete` on quoted literal lists; build
  them with `list` or copy first.

## Step 4: Finish the advice migration (bulk of the work; one PR per file)

- Tidy `ecb-enable-ecb-advice2` / `ecb-add-to-advice-set`
  (`ecb-common-browser.el:720-800`): remove the leftover `message` that
  prints on every activation, validate the advice class before mutating the
  set, fix the error message that prints the literal symbol `advice-class`,
  drop the bogus `'calling` debug event at registration time.
- Convert the remaining ~70 `defadvice` forms to named functions plus
  `ecb-add-to-advice-set`, in this order: `ecb-compatibility.el`,
  `ecb-speedbar.el`, `ecb-winman-support.el`, `ecb-eshell.el`,
  `ecb-create-layout.el`, `ecb-file-browser.el`, `ecb-method-browser.el`,
  then `ecb-layout.el` last (~45 advices including `delete-window`,
  `split-window`, `display-buffer`; the riskiest).
- `ecb-with-original-adviced-function-set` and `ecb-with-ecb-advice` must
  keep handling both entry formats until the last file is converted; the
  mixed-format test from step 0 is the gate.
- Then delete `defecb-advice` and the legacy branch of `ecb-enable-ecb-advice`,
  and port `ecb-advice-test.el` fully to the new mechanism.

## Step 5: Lexical binding and dead code

- Add `lexical-binding: t` to the 21 remaining files one at a time, fixing
  free-variable warnings as they surface. `ecb-analyse.el` first: its
  `cl-defmethod` use is unsupported without it.
- Remove dead guarded branches for functions that no longer exist in Emacs:
  `eieio-help-mode-augmentation-maybee` (`ecb-symboldef.el:297`),
  `find-tag-internal`, `vc-load-vc-hooks`, `tramp-file-name-path`,
  `set-face-parent`, and XEmacs-only `modeline-map`, `vertical-divider-map`,
  `function-at-point`. Replace obsolete `eshell-bol` (`ecb-eshell.el:286`)
  with `beginning-of-line`.
- Each removal lets a `silentcomp-defun` go, which lets the byte-compiler
  start reporting real missing functions. Goal: `silentcomp.el` shrinks to
  near nothing.
- Register `ecb--semantic-tag` with `cl-deftype` (or use `semantic-tag`
  directly) to remove the unknown-type warnings in `ecb-analyse.el` and
  `ecb-method-browser.el`.
- Flip CI to treat warnings as errors.

## Step 6: Repo hygiene

- Delete `make.bat` (hardcoded to Emacs 22.3 and self-described as unusable),
  `ecb2/` (unfinished `jn-*` rewrite), and the obsolete `html/` site; update
  `TODO` / `TODO.org`.
- `ecb-show-help`: point the user at `make online-help` instead of a bare
  "does not exist" error.
- `ecb-expand-directory-nodes`: warn (or require confirmation) when the root
  is `~` or `/`; it ran for over two minutes on a home directory.

---

## Sizing

- Step 0: the P1 subset (helpers, `tree-buffer`, `ecb-util`, advice
  machinery, `ecb.el`, integration sweep) is roughly a week of focused work;
  the full P1 list including layout/file/method browsers is several weeks and
  can proceed in parallel with steps 1-3 as long as the P1 subset lands
  first.
- Steps 1-3: an evening, one or two PRs.
- Step 4: several PRs over a few weeks.
- Steps 5-6: incremental, can trail behind.
