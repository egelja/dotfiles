#!/bin/bash

# Fix poetry and venvwrapper
export VIRTUALENVWRAPPER_PYTHON=`which python3.11`
export PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring

# Gurobi
export GUROBI_HOME="/opt/gurobi/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/nino/.miniforge3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/nino/.miniforge3/etc/profile.d/conda.sh" ]; then
        . "/home/nino/.miniforge3/etc/profile.d/conda.sh"
    else
        export PATH="/home/nino/.miniforge3/bin:$PATH"
    fi
fi
unset __conda_setup
conda activate firesim
# <<< conda initialize <<<

