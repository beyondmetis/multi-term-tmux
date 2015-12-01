# multi-term-tmux
Seemless integration of tmux with emacs using multi-term. This project borrows ideas from multi-term-ext which is meant for persistent screen connections both locally and over ssh. 

## Supported platforms

This package has been tested with GNU emacs 24.5.1 and tmux 2.1 using Arch Linux

## Currently Implemented 
'multi-term-tmux-open (&optional session-name buffer-name)' creates an emacs buffer 'buffer-name' with the local tmux session. If the local tmux session 'session-name' does not exist, it is created.

'multi-term-tmux-remote-open (user+host &optional session-name buffer-name)' creates an emacs buffer 'buffer-name' with the remote tmux session. If the remote tmux session 'session-name' does not exist, it is created.

The default 'session-name' is set by parameter multi-term-tmux-name which itself defaults to 'slave'. 'buffer-name' is set to &lt;machine&gt;-tmux-&lt;session name&gt;

## TODO

- Add the ability to disconnect all tmux views outside of tmux
- Configure tmux to look more seemless with emacs
- Better handling of parameters and user configurations
- Disconnection options for using external terminals and detection of output spamming

## License

This package is under the GNU General Public License.

Copyright (C) 2015 Todd Goodall.
