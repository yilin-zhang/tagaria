# Tagaria

Tagaria manages textual tags inside directory silos. It works with ordinary
text and does not depend on Org or Markdown. Requires Emacs 29.1 or newer.

## Setup

```elisp
(use-package tagaria
  :load-path "~/.emacs.d/site-lisp/tagaria"
  :commands (tagaria-list tagaria-switch-silo tagaria-insert
             tagaria-search tagaria-migrate-database tagaria-minor-mode)
  :hook ((org-mode . tagaria-minor-mode)
         (markdown-mode . tagaria-minor-mode))
  :custom
  (tagaria-directory "~/notes/"))
```

`tagaria-directory` is the default silo. When it is nil, Tagaria uses the
nearest parent containing `.tagaria.eld`; `tagaria-switch-silo` changes the
default dynamically. The first scan initializes a new silo. Markdown support
requires the separately installed `markdown-mode` package.

The default syntax is `@{example-tag}`. Customize `tagaria-tag-regexp` and
`tagaria-format-function` together to use another syntax.

## Data

Each tag has an optional multiline description. Related tags are stored as a
validated symmetric mapping:

```elisp
(:tags (("alpha" :desc "First line\nSecond line") ("beta")))
 :related (("alpha" "beta") ("beta" "alpha")))
```

List summaries display description line breaks as `↵`; the detail page keeps
the original line breaks. Run `tagaria-migrate-database` once for an older
database: it creates a backup, keeps `:desc` and a valid top-level `:related`
mapping, and discards other legacy fields.

## Use

`tagaria-list` opens the silo list. `RET` enters a tag in the same window and
`^` returns to the list. The detail page shows the silo path followed by
foldable Description, Related Tags, and Occurrences sections. Related tags are
clickable and open in the same detail view.

`e` edits the description in the minibuffer; `E` opens a multiline edit buffer.
The latter uses `text-mode` by default and is configurable through
`tagaria-description-edit-mode-function`. `a` adds a related tag and `x`
removes one. `RET` or `mouse-1` on a displayed description edits it directly.
`r` renames, `g` refreshes, and `D` deletes the tag. In the list,
`d` removes all textual references while preserving the tag; in detail, `d`
removes the occurrence at point. Use `C-h m` for the complete map.

In `tagaria-minor-mode`, `C-c @` and `mouse-1` open the tag at point. Org uses
its native `C-c C-o` dispatch; Markdown binds `C-c C-o` only on highlighted
Tagaria tags. Moving through occurrences previews the source in another
ordinary window; `RET` enters that existing source window.

Reference deletion preserves surrounding text sensibly; customize
`tagaria-delete-separator-function` for adjacent-script behavior. Rename and
bulk deletion scan the silo, preview or confirm destructive work, and create
recoverable backups under `.tagaria-backups/`. `tagaria-backup-keep` controls
retention.

Tagaria is GPLv3-or-later; see [LICENSE](LICENSE).
