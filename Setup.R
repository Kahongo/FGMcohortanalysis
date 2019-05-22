# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------


# written by Kathrin Weny

# File description, purpose of code, inputs and output --------------------
# This code creates a cross-sectional data sets of all DHS with FGM self-reported data. 
# Reads in the data, filters for the relevant indicators: 
# clustering and stratification due to the survey design,
# age (single years, year of birth and age group), residence (rural/urban), knowledge about FGM,
# if available, if the woman has experienced FGM, age at which she experienced it,
# and if she thinks, the practice should continue, if available

# the code only includes datasets with FGM status and age-at-FGM.

# The output of this codes is the df_DHS data frame with all cross-sectional DHS FGM data 

# The data has been downloaded from https://dhsprogram.com/data/
# ICF International. 1990-2018.
# Demographic and Health Surveys (various) [Datasets]. Calverton, Maryland: ICF International [Distributor], 2018.

# Source codes, libraries, global options and working directory -----------
library(tidyverse)
library(survey)
library(foreign)
library(reshape2)
library(survival)
library(survminer)
library(readxl)
library(grid)
library(gridExtra)
library(ggpubr)
library(devtools)
library(gtable)
library(survey)
library(coxme)
library(devtools)
library(data.table)
library(ggthemes)
library(jtools)
library(scales)
library(sjPlot)
library(sjmisc)
library(snakecase)
library(lme4)
library(effects)
library(xlsx)
library(survminer)
library(lattice)
library(Greg)
library(coxphw)
library(xlsx)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(survey)
library(foreign)
library(reshape2)
library(survminer)
library(data.table)
library(stringr)
library(wesanderson)
library(ggthemes)
library(tmap)
library(leaflet)
library(rio)
library(tmaptools)
library(rgdal)
library(readxl)
library(ggplot2,sp,raster,maps,mapdata,maptools,ggmap,rgeos, map)
library(broom)
library(survminer)

install_github("renlund/ucR")

confint.coxme <- function(object, parm=NULL, level=0.95, ..., more=FALSE){
  if(!is.null(parm)) warning("[confint.coxme] argument 'parm' doesn't do anything for this method")
  if(level != 0.95) warning("[confint.coxme] 'level' will be 0.95 regardless of what argument you give it. Ha!")
  co <- object$coef
  se <- sqrt(diag(stats::vcov(object)))
  m <- matrix(c(co - 2*se, co + 2*se), ncol=2)
  colnames(m) <- c("2.5 %", "97.5 %")
  rownames(m) <- names(co)
  if(more){
    p <- 2*stats::pnorm(abs(co/se), lower.tail=F)
    m <- cbind(m, co, p)
    rownames(m)[3:4] <- c("coef", "p")
  }
  return (m)
} # https://github.com/renlund/ucR/blob/master/R/confint_coxme.r

#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/survminer", build_vignettes = FALSE)

options(scipen = 999)  # disable scientific notation in R

# If a primary sapling unit has only a single observation, R will crash
# option adjust calculates conservative standard errors
# reference: http://faculty.washington.edu/tlumley/survey/example-lonely.html
options(survey.lonely.psu = "adjust")

# Function definitions ----------------------------------------------------

ReadSingleSAV <- function(x){
  # reads in one DTA file from a given directory
  # 
  # Args:
  #  x = list index of file to be read into R
  #
  # Returns:
  #  single dataset
  
  data <- read.spss(listsav[x], to.data.frame = TRUE)
  
  return(data)
  
}

CalculateTimetoEvent <- function(x,y,z){
  # calculates the time (years) that has passed until a certain event(FGM) occurs
  # and in the case, there was no event, uses proxy (age)
  #
  #Args:
  #   x = event indicator variable
  #   y = age at wich event occured
  #   z = substitute in case no event
  #
  #Returns:
  # new dataframe with time to event column
  
  time <- ifelse(x == 0, z, ifelse(x == 1, y, NA))
  
  return(time)
  
}


plot.fun2 <- function(MODEL, ...){
  plot(survfit(MODEL, newdata = data.frame(year_birth = 1960:2017)),
       log = "x", fun = function(S) log(-log(S)),
       lty = 1:2, xlim = c(1,35), ylim = c(-4,1.5), ...
  )
}

# Create color pallettes --------------------------------------------------

colfunc  <- colorRampPalette(c("blue", "green", "olivedrab", "yellow"))
colfunc1 <- colorRampPalette(c("blue", "green", "yellow"))
colfunc2 <- colorRampPalette(c("peachpuff", "sandybrown", "orangered"))
colfunc3 <- colorRampPalette(c("tomato4", "sandybrown", "orangered"))
