# dired-preview for Emacs

+ Package name (GNU ELPA): `dired-preview` (⚠️ not available yet;
  public testing phase as of 2023-06-25)
+ Git repo on SourceHut: <https://git.sr.ht/~protesilaos/dired-preview>
  - Mirrors:
    + GitHub: <https://github.com/protesilaos/dired-preview>
    + GitLab: <https://gitlab.com/protesilaos/dired-preview>
+ Mailing list: <https://lists.sr.ht/~protesilaos/general-issues>
+ Backronym: Directories Invariably Render Everything Decently; PDFs
  Require Extra Viewing Instructions for Emacs to Work.

* * *

This is a simple package to automatically preview in a side window the
file at point in Dired buffers.  Preview windows are closed when they
are no longer relevant, while preview buffers are killed if they have
not been used for other purposes beside previewing.

Enable the `dired-preview-mode` in the current Dired buffer and then
perform the regular up/down motions (`n` or `p` with default key
bindings) to call the commands `dired-preview-next-file`,
`dired-preview-previous-file`.  Those will trigger the preview.

The previewed file is displayed in a side window if its size is below
the number specified in the user option `dired-preview-max-size`.
Previews are shown subject to a small delay, per ther user option
`dired-preview-delay`.

Files matching the `dired-preview-ignored-extensions-regexp` are not
previewed.  The default value of that user option includes multimedia,
PDFs, and EPUBs.

[ In the future, we may find ways to quickly preview any file type
  without affecting the performance of Emacs.  Though `dired-preview`
  is designed to have no external dependencies, so such an ambition
  may not be realisable (e.g. produce a thumbnail out of a video). ]

To set up `dired-preview-mode` in every Dired buffer, set it up thus:

```elisp
(add-hook 'dired-mode-hook #'dired-preview-mode)
```

I took inspiration for `dired-preview` from the now unmaintained
`peep-dired` package by Adam Sokolnicki: <https://github.com/asok/peep-dired>.
My original plan was to volunteer to maintain `peep-dired` but I
decided to write it my own way: it was easier for me, plus I can
implement only what I consider necessary without upsetting existing
users.
