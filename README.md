My Dot Files
============

Pieced together from  [Howard Abrams][0] and [Harry R. Schwartz][1].

Mu and Mu4e needs ti be installed first.

On Linux:

``` sh
mkdir ~/programs
cd ~/programs
git clone https://github.com/djcb/mu.git 

```

On Mac:

``` sh

brew install mu --with-emacs --HEAD

```

To install 

``` sh
./install.sh 

```

run emacs :

``` sh
emacsclient -c --alternate-editor= <FILE> 

```

Stop the client:

``` sh
emacsclient --eval "(kill-emacs)" 

```



[0]: https://github.com/howardabrams/dot-files
[1]: https://github.com/hrs/dotfiles
