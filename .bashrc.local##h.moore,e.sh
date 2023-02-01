#!/bin/bash
alias scratch="cd /nfs-scratch/nvm6986"

# Gurobi
export GUROBI_HOME="/nfs-scratch/nvm6986/gurobi/linux64"
export PATH="${PATH}:${GUROBI_HOME}/bin"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${GUROBI_HOME}/lib"

# CC
mail-tests-done() {
    echo "Please check moore for details." | mailx -s "Tests Done!" nikola@u.northwestern.edu
}
