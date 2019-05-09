# .emacs.d
New emacs config

Based on DOOM emacs, but starting from vanilla without the evil focus.

I did not simply use DOOM emacs because I am not a vimmer. Emacs
keybindings are muscle memory after having used emacs for > 25 years.

TODO
* Convert markdown docs to org-mode
* Check packages loaded by old default config and see if there are newer/better alternatives.
  * Examples
  	* smartparens instead of (ido? paredit? electric-pair-mode?)
	* enh-ruby-mode
	* markdown-mode
	* gfm-mode
	* ivy, conunsel, swiper
	* projectile (DOOM loads it super early, it seems, to get its keybindings as early as possible)
	* dired: dired-k, ranger, icons, all-the-icons-dired
	* eshell, eshell-up, eshell-z, shrink-path
	* imenu, imenu-anywhere, imenu-list
	* term, multi-term
	* vc: git-link, git-timemachine, gitconfig-mode, gitignore-mode - and of course, magit
* When bringing in config from DOOM emacs, make sure that if a dependency is brought in, if it replaces
  the functionality of a package loaded in the old config, remove the old integration and any droppings. Example:
  `better-defaults` turns on `ido-mode`, but only if `helm` is not already enabled. DOOM has configs for `company`,
  `helm`, `ido` and `ivy`.
* Look at DOOM's native regex playground implementation in `$DOOM_EMACSD/modules/app/regex`.
* Look into WoMan for reading man pages (emacs-specific, GNU package replacement for man)

NOTES
* Docs assume 120 character width.
* I've set it up with a `lisp` directory inside `.emacs.d`. The first package I load after setting up `package.el`
  is `better-defaults.el`. If you create the `lisp` directory in `.emacs.d` and then install `better-defaults` per
  https://github.com/technomancy/better-defaults then it should load. I included the `lisp` subdirectory in
  `.gitignore`
* I'm loading a theme from ~/.emacs.d/themes. Create that directory, drop your color theme file there, and tweak your
  emacs config to load that instead of the Darkula color theme I'm loading.
  
