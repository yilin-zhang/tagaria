# Tagaria 🏷️

Tagaria manages inline tags in any text format. It provides a searchable tag
list, descriptions, related tags, and references without depending on Org or
Markdown. It requires Emacs 29.1 or newer.

## Setup

```elisp
(use-package tagaria
  :load-path "~/.emacs.d/site-lisp/tagaria"
  :commands (tagaria-list
             tagaria-switch-realm
             tagaria-insert
             tagaria-search
             tagaria-migrate-database
             tagaria-minor-mode)
  :hook ((org-mode . tagaria-minor-mode)
         (markdown-mode . tagaria-minor-mode))
  :custom
  (tagaria-directory "~/notes/"))
```

A realm is a directory tree managed as one tag collection. Tagaria scans the
text files below its root for references, and stores the tags, descriptions,
and relations in `.tagaria.eld` at that root.

`tagaria-directory` sets the default realm. Use `tagaria-switch-realm` to
change it during a session. If no default is configured, Tagaria looks for the
nearest parent directory containing `.tagaria.eld`.

`tagaria-minor-mode` highlights tags and enables tag navigation. Tagaria does
not add it to any major-mode hook by itself; the example above enables it in
Org and Markdown buffers. Markdown integration requires `markdown-mode`.

## Use

The default tag syntax is `@{example-tag}`.

1. Run `M-x tagaria-insert` to insert an existing tag or create a new one.
2. Run `M-x tagaria-list` to scan the current realm and open its tag list.
   The scan also discovers tags that were typed into files manually and creates
   `.tagaria.eld` when the realm is new.
3. Press `RET` on a tag to open its detail view, then press `^` to return.

The List View shows every tag with its description, reference count, and
related tags. The Detail View shows one tag's full description, related tags,
and every reference. Move through the references to preview their source in
another window; press `RET` to enter that source window. Section headings in
the Detail View can be folded with `TAB`.

| Key | List View | Detail View |
| --- | --- | --- |
| `RET` / `o` | Open the item at point | Open the item at point |
| `^` | — | Return to the List View |
| `c` | Create a tag | — |
| `e` | Edit the description in the minibuffer | Edit the description in the minibuffer |
| `E` | Edit the description in a text buffer | Edit the description in a text buffer |
| `a` | Add a related tag | Add a related tag |
| `x` | Remove a related tag | Remove a related tag |
| `d` | Remove all references to the tag | Remove the reference at point |
| `D` | Delete the tag and all its references | Delete the tag and all its references |
| `r` | Rename the tag | Rename the tag |
| `g` | Refresh the List View | Refresh the Detail View |
| `s` | Search tags | — |
| `TAB` | — | Fold or expand a section |
| `q` | Quit Tagaria | Quit Tagaria |

Descriptions and related tags can also be opened with `mouse-1`. Multiline
descriptions are edited in `text-mode` by default; customize
`tagaria-desc-edit-mode` to use another major mode.

In a buffer using `tagaria-minor-mode`, `C-c @` or `mouse-1` opens the tag at
point. Org also supports its usual `C-c C-o`. In Markdown, `C-c C-o` works when
point is on a highlighted Tagaria tag.

Customize `tagaria-tag-regexp` and `tagaria-format-function` together to use a
different tag syntax.

## Data Storage

You normally do not need to edit `.tagaria.eld` by hand. Its format is Emacs
Lisp data:

```elisp
(:tags (("alpha" :desc "First line\nSecond line")
        ("beta"))
 :related (("alpha" "beta")))
```

Descriptions are optional and may contain multiple lines. Related tags are
undirected, so each pair is stored only once.

Run `M-x tagaria-migrate-database` to convert an older database. Tagaria makes
a backup before migration. Rename and deletion operations also create backups
under `.tagaria-backups/`; customize `tagaria-backup-keep` to control how many
are retained.

Tagaria is GPLv3-or-later; see [LICENSE](LICENSE).
