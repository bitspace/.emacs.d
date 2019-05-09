# .emacs.d
New emacs config

Based on DOOM emacs, but starting from vanilla without the evil focus.

I did not simply use DOOM emacs because I am not a vimmer. Emacs
keybindings are muscle memory after having used emacs for > 25 years.

TODO
Convert markdown docs to org-mode

NOTES
* Docs assume 120 character width.
* I've set it up with a `lisp` directory inside `.emacs.d`. The first package I load after setting up `package.el`
  is `better-defaults.el`. If you create the `lisp` directory in `.emacs.d` and then install `better-defaults` per
  https://github.com/technomancy/better-defaults then it should load. I included the `lisp` subdirectory in `.gitignore`

