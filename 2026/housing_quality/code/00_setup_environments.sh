#!/bin/bash

set -e  # exit immediately if any command fails

# ---- Resolve project root regardless of where this script is invoked from ----
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"   # .../housing_quality/code
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"                   # .../housing_quality

echo "Project root resolved to: $PROJECT_DIR"

# ---- Set up python virtual environment ----
conda create -p "$PROJECT_DIR/.conda" -y

# Source conda's shell functions so `conda activate` works in a non-interactive script
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate "$PROJECT_DIR/.conda"

conda install --file "$PROJECT_DIR/requirements.txt" -y

# ---- Run R setup inside the project directory ----
Rscript -e "
setwd('$PROJECT_DIR')

if (!requireNamespace('renv', quietly = TRUE)) {
  install.packages('renv')
}

renv::init(bare = TRUE)

required_pkgs <- c(
  'tidyverse',   # data wrangling
  'survey',      # svrepdesign / svyglm / svymle -- primary modeling engine
  'MASS',        # glm.nb() -- NB starting values + diagnostic proxy fit
  'DHARMa',      # simulation-based residual diagnostics (on the MASS fit)
  'car',         # vif() for the collinearity check
  'broom'        # tidy() where supported
)

renv::install(required_pkgs)
renv::snapshot(prompt = FALSE)
"

echo "Setup complete. Project initialized at $PROJECT_DIR with renv and required packages."