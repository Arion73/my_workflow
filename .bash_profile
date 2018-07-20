export PROJECT=$HOME/documents/myproject/

# set color
export CLICOLOR=1

# set mysql
PATH=$PATH:/usr/local/mysql/bin

# set python virtualenv
# avoid installing packages into the system python env if not activate virtualenv
export PIP_REQUIRE_VIRTUALENV=true
# when pip, system activate virtualenv automatically
export PIP_RESPECT_VIRTUALENV=true
# set python virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
source /usr/local/bin/virtualenvwrapper.sh

# set Emacs Flyspell Lang
export LANG=en_US.UTF-8

## startup emacs server
alias em='emacs --daemon'
## startup spacemacs
alias spacemacs='emacs -q -l ~/.spacemacs.d/init.el'
## startup spacemacs server
alias sm='emacs --daemon -q -l ~/.spacemacs.d/init.el'
## startup emacs or spacemacs client
alias cm='emacsclient -c '' "$@"'

cd $PROJECT
workon all
