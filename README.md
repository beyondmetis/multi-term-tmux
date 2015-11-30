# multi-term-tmux
Seemless integration of tmux with emacs using multi-term. This project borrows ideas from multi-term-ext which is meant for persistent screen connections both locally and over ssh. 

## Supported platforms

This package has been tested with GNU emacs 24.5.1 and tmux 2.1 using Arch Linux

## Currently Implemented 
'multi-term-tmux-new' switches the current buffer to the local tmux session labeled "slave". If a local session named "slave" does not exist, it is created. In this way, sessions are independent of emacs and nearly seemlessly integrated.

## TODO

- Add the ability to disconnect all tmux views outside of tmux
- Configure tmux to look more seemless with emacs
- Implement ssh tmux connections
- Disconnection options for using external terminals and detection of output spamming

## License

This package is under the GNU General Public License.

Copyright (C) 2015 Todd Goodall.
