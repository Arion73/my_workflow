export PROJECT_HOME=$HOME/documents/MyProject/

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

cd $PROJECT_HOME
workon all
