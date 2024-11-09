# typewriter-roll-mode
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

This minor mode attempts to remove distraction of seeing the previous lines of
text while dumping an uninterrupted stream of thoughts, hence preventing focus
jumping to revisions whether to handle typos, spacing or complete rewording and
losing such a thought in the process.

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then simply `M-x typewriter-roll-mode`.

### Enable for file extensions

It might be useful to auto-enable the mode for certain files or patterns. One
of such methods is updating `auto-mode-alist`:

```emacs-lisp
(add-to-list 'auto-mode-alist
             '("\\.my-quick-note.txt\\'" . typewriter-roll-mode))
```

For every file with `.my-quick-note.txt` extension.

[melpa-badge]: http://melpa.org/packages/typewriter-roll-mode-badge.svg
[melpa-package]: http://melpa.org/#/typewriter-roll-mode
[melpa-stable-badge]: http://stable.melpa.org/packages/typewriter-roll-mode-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/typewriter-roll-mode
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
