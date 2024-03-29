#!/bin/bash

# Fix poetry and venvwrapper
export VIRTUALENVWRAPPER_PYTHON=`which python3.11`
export PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring

# Gurobi
export GUROBI_HOME="/opt/gurobi/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"
