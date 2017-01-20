# eval-proto

eval-proto is an emacs package which aims to help speed up prototyping through
micro-development.

Suppose you're working on a large project. I frequently find that developing
within the large project itself is difficult and error-prone, especially when I
need to quickly try out ideas in an unfamiliar language. To do this I typically
prototype on a small scale in a temporary directory and execute it as I edit,
but that requires me to switch contexts outside of emacs. This is less than
ideal.

Enter eval-proto. eval-proto allows you to run whatever code you want in your
favorite scripting language and prints the output immediately in your
minibuffer. This makes it easy to quickly prototype your ideas, right within
emacs. How cool is that?

# Setup

All you have to do is bind a key to `eval-proto/eval`. eval-proto will
automatically pick up the shebang from your file and run the interpreter you
specify.

# Example

GIF coming soon...
