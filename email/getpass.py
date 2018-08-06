#! /usr/bin/env python3
import subprocess
import os
def mailpasswd(account):
    _USERNAME = os.getenv("USER") 
    _HOME = os.path.expanduser('~'+_USERNAME)
    path = "%(homedir)s/.mail-%(account)s-passwd.gpg"
    path %= {'homedir':_HOME,'account':account}
    return subprocess.check_output(["gpg2", "--quiet", "--batch", "-d", path]).strip()


#
# need to greate new gpg key with :
# gpg2 --full-gen-key
# then encrupt a file containing the password with:
# cat blah.txt | gpg2 --encrypt --recipient timo.lassmann@telethonkids.org.au -o ~/.mail-office365-passwd.gpg
# remove blah.txt - check history for password - inzsh simple editing .zsh_history works to remove passwords.  
#  path = "/home/user/.mail-%(account)s-passwd.gpg"
