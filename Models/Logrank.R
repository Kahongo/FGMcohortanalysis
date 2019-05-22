# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------
# written by Kathrin Weny
# last updated: 1 March 2019

setwd("C:/Users/weny/Google Drive/2018/FGM/02 -Trend modelling/FGM-trend-analysis/Codes")

# Setup -------------------------------------------------------------------

source("Setup.R")

# Create dataset ----------------------------------------------------------

source("Dataprocessing/DHS.R")                                # processed DHS surveys and creates DHS dataset for mothers
source("Dataprocessing/MICS.R")                               # processed MICS surveys and creates MICS dataset for mothers
source("Dataprocessing/DHSd.R")                               # processed DHS surveys and creates DHS dataset for daughters
source("Dataprocessing/MICSd.R")                              # processed MICS surveys and creates MICS dataset for daughters

# Source dataset ----------------------------------------------------------

source("combine_datasets.R")                   # combines all datasets

# Survival analysis -------------------------------------------------------

source("survival_analysis_September2018.R")    # Logrank test and hierarchical Cox regression

# Data visualizations -----------------------------------------------------

source("KM_figures.R")                         # Kaplan Meier estimates



