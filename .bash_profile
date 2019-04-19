export PROJECT=$HOME/documents/myproject/

# set color
export CLICOLOR=1

# set mysql
PATH=$PATH:/usr/local/mysql/bin

# Homebrew git path
export PATH="/usr/local/bin:$PATH"

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

## pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

## Go env
export GOROOT=/usr/local/Cellar/go/1.12.4/libexec
export GOPATH=$HOME/documents/myproject/GO
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

## Java path
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH=$JAVA_HOME/bin:$PATH
export CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar


cd $PROJECT
workon python3.7
