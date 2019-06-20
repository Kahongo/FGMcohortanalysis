# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny

# File description, purpose of code, inputs and output --------------------

# This code creates a cross-sectional data sets of all MICS with FGM self-reported data. 
# Reads in the data, filters for the relevant indicators: 
# clustering and stratification due to the survey design,
# age (single years, year of birth and age group), residence (rural/urban), knowledge about FGM,
# if available, if the woman has experienced FGM, age at which she experienced it,
# and if she thinks, the practice should continue, if available

# the code only includes datasets with FGM status and age-at-FGM.

# The output of this codes is the df_MICS data frame with all cross-sectional MICS FGM data 

# The data has been downloaded from INSERT MICS SOURCE

# Petite enfance: best source I have file:///C:/Users/Kathrin%20Weny/Downloads/ga_ind_13_chap2.pdf mics
# seesms to be from 3-6

# Working directory

setwd("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/MICS")

listsav <- dir(pattern = "*.sav")

# Executed statements -----------------------------------------------------

# read in population data

population <- read.csv("population.csv")

# Benin 2014 --------------------------------------------------------------

i <- 1 
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Benin"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                           
                  "WM6Y",
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","WM6M","wscore",   
                  "HH6",  "HH7",                                            
                  "WB4", "WB5", "WB9", "welevel",                     
                  "windex5",                                          
                  "FG1",                                              
                  "FG3",
                  "FG7",
                  "FG22"))                                           

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$stratification <- NA
tmp$ed_single_years <- NA
tmp$prim_samp_unit <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size
# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(tmp$fgm_age)

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total

# Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

df_MICS <- tmp

# Central African Republic 2010 -------------------------------------------

i <- 2 

tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Central African Republic"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",
                  "WB1Y",
                  "HH1" ,"HH2","WMWEIGHT","WDOB","WM1","WDOM",        # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                       # rural/urban
                  "WB4", "WB5", "CEB", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",
                  "FG7", 
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm

tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)


#NAs

tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total

#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)


# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))


# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Chad 2010 ---------------------------------------------------------------

i <- 4
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Chad"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","CEB","WM1","MSTATUS",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                             # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",
                  "FG7", 
                  "FG17"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm

tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))


tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))


tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)


tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)


tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))


total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total


#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Cote d'Ivoire 2016 ------------------------------------------------------

i <- 5
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Cote d'Ivoire"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM5","WM7",           # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                  # education indicators, MICS only has one in FGM survey
                  "windex5",                                         # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
# in this case, the regions are the strata: https://mics-surveys-prod.s3.amazonaws.com/MICS3/West%20and%20Central%20Africa/C%C3%B4te%20d%27Ivoire/2006/Final/Cote%20d%27Ivoire%202006%20MICS_French.pdf

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp$year <- 2016
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total


#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Gambia 2010  -----------------------------------------------

i <- 6

tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Gambia"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","WMWEIGHT","LN","WM5","WM7",           # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7A",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                  # education indicators, MICS only has one in FGM survey
                  "windex5",                                         # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
# in this case, the regions are the strata: https://mics-surveys-prod.s3.amazonaws.com/MICS3/West%20and%20Central%20Africa/C%C3%B4te%20d%27Ivoire/2006/Final/Cote%20d%27Ivoire%202006%20MICS_French.pdf

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm

tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "During infancy", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total

#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Ghana 2011 --------------------------------------------------------------

i <- 7
tmp <- ReadSingleSAV(i) 

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

# add country column
tmp$country <- "Ghana"
a <- as.data.frame(attr(tmp, "variable.labels"))

# Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","WM4","WM1","WM8",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                             # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm

tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)
tmp$fgm_age <- ifelse(tmp$fgm_age == "During infancy", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total


#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Guinea 2016 --------------------------------------------------------------

i <- 8
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Guinea"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

# Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","WM11M","WB1M","WM5",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                             # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp$year <- 2016 # has to be hardcoded, as converstion fails
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm

tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No"| tmp$fgm_know == "N?o", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total


#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Guinea-Bissau 2014 ------------------------------------------------------
i <- 10
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Guinea-Bissau"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Sim" | tmp$FG2 == "Sim", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","WDOB","WM7","WDOI",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm

tmp$fgm_know <- ifelse(tmp$fgm_know == "Não", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Sim", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Não" , 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)



#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Iraq 2011 ---------------------------------------------------------------

i <- 11
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Iraq"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","WDOB","WM7","WDOI",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "region",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))


tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs

tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "During infancy", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total


#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)
plyr::count(tmp$fgm_age) 


# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Mali 2009 - 2010 --------------------------------------------------------

i <- 12
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Mali"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","WDOB","WM7","WDOI",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG17"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs

tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total

#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)
plyr::count(tmp$fgm_age) 

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Mali 2015 --------------------------------------------------------

i <- 13
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Mali"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WDOI",
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM1","HH4",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",
                  "FG7", 
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit  <- NA
tmp$stratification  <- NA
tmp$ed_single_years <- NA
tmp$year            <- 2015

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total

#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Mauritania 2011 ---------------------------------------------------------

i <- 14
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Mauritania"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","WMWEIGHT","LN", "WM6D","WDOI",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "wlthind5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA
tmp$year <- 2011 # brutal fix, check data set in SPSS later

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$region) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "0", tmp$fgm_age) # 7 March 2019: Code all of those who could not remember as cut at birth (due to shape of survival function)
tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

# add time to event column
tmp$age_years <- as.numeric(as.character(tmp$age_years))

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

tmp$time <- as.numeric(as.character(tmp$time))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Mauritania 2015 --------------------------------------------------------

i <- 15

tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Mauritania"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM7","HH4",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit  <- NA
tmp$stratification  <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "moins d'un ans", "0", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Nigeria 2011 ------------------------------------------------------------

i <- 16

tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Nigeria"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM4","WM5",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                             # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

plyr::count(tmp$fgm)

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <-as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "During infancy", "0", tmp$fgm_age) #### Changed it for Analysis!

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))
total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total

#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Nigeria 2017 ------------------------------------------------------------

i <- 17
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Nigeria"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

# select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            # country, age (continous and group)
                  "WM6Y",                                             # year of interview
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM6M","WM5",   # sample design V004: enumeration area / V101: Region / awfacut: all woman facto regions
                  "HH6", "HH7",                                              # rural/urban
                  "WB4", "WB5", "WB7", "welevel",                     # education indicators, MICS only has one in FGM survey
                  "windex5",                                          # should be wealth v190 but is placeholder to keep number of columns, Sudan doesnt seem to have wealth quintile back then
                  "FG1",                                              # should be knowledge, but placeholer is now type for first wave of DHS
                  "FG3",                                              # 
                  "FG7",
                  "FG22"))                                            # approval/disapproval

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

# add time to event column
tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Sierra Leone 2010 -------------------------------------------------------

i <- 18
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Sierra Leone"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                           
                  "WM6Y",                                             
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WB1M","WM5",           # LN and WM5 are placeholders for missing data
                  "HH6", "HH7",                                             
                  "WB4", "WB5", "WB7", "welevel",                      # WB 7 is a placeholder for missing data
                  "windex5",                                          
                  "FG1",                                              
                  "FG3",                                              
                  "FG7",
                  "FG22"))                                          

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables

tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "During infancy", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total
#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)
plyr::count(tmp$fgm_age) 

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))


# Sierra Leone 2017 -------------------------------------------------------

i <- 19
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Sierra Leone"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB4","WAGE",                           
                  "WM6Y",                                             
                  "WB3Y",
                  "HH1" ,"HH2","wmweight","LN","WB3M","WM5",           # LN and WM5 are placeholders for missing data
                  "HH6", "HH7",                                             
                  "WB10A", "WB5", "WB7", "welevel",                      # WB 7 is a placeholder for missing data
                  "windex5",                                          
                  "FG1",                                              
                  "FG3",                                              
                  "FG7",
                  "FG24"))                                          

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "NO", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "YES", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "YES",  1,
                  ifelse(tmp$fgm == "NO", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

# add time to event column

tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Sudan 2014 --------------------------------------------------------------
# only post-secession data set

i <- 22
tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Sudan"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes", "Yes", "No") # no probing question

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            
                  "WM6Y",                                             
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM11M","WM5",   # LN and WM5 are placeholders for missing data
                  "HH6", "HH7",                                             
                  "WB4", "WB5", "WB7", "welevel",             # WB7 placeholder for missing data        
                  "windex5",                                          
                  "FG1",                                              
                  "FG3",                                             
                  "FG7",
                  "FG22"))                                            
# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

# add time to event column
tmp$age_years <- as.numeric(as.character(tmp$age_years))
tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Togo 2010 ---------------------------------------------------------------

i <- 23

tmp <- ReadSingleSAV(i) 

# add country column
tmp$country <- "Togo"
a <- as.data.frame(attr(tmp, "variable.labels"))

#table(tmp$FG1) # Knows of FGM
#table(tmp$FG2) # Knows of FGM (probed)

tmp$FG1 <- ifelse(tmp$FG1 == "Yes" | tmp$FG2 == "Yes", "Yes", "No")

#Select variables
tmp <- tmp %>%
  dplyr::select(c("country", "WB2","WAGE",                            
                  "WM6Y",                                             
                  "WB1Y",
                  "HH1" ,"HH2","wmweight","LN","WM11M","WM5",    # LN and WM5 placeholders for missing data
                  "HH6","HH7",                                              
                  "WB4", "WB5", "WB7", "welevel",              # WB7 placeholder for missing data        
                  "windex5",                                          
                  "FG1",                                              
                  "FG3",                                              
                  "FG7",
                  "FG22"))                                            

# rename
names(tmp) <- c("country", "age_years", "age_groups",
                "year", 
                "year_birth",
                "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                "residence", "region",
                "highest_ed_level", "years_ed_for_highest", "ed_single_years", "ed_detail", 
                "wealth",
                "fgm_know", 
                "fgm",
                "fgm_age",
                "support")

tmp$prim_samp_unit <- NA
tmp$stratification <- NA
tmp$ed_single_years <- NA

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
tmp$strata <- paste(tmp$residence,tmp$strata) # before this tmp$strata was only the administrative region

# create unique cluster and strata variables
tmp <- tmp %>%
  mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
# therefore: rescale/denormalize weights to population size

# count unweighted surveyed 15-49 year olds
tmp$n_survey <- nrow(tmp)

# get total population of 15-49 year olds at mid-year of the survey
tmp <- merge(population, tmp, by = c("country", "year"))

# denormalize weight
tmp <- tmp %>%
  mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)

# recode fgm
tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                       ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))

tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim",  1,
                  ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao", 0, NA))

tmp$fgm <- ifelse(is.na(tmp$fgm) & (tmp$fgm_know == 0), 0, tmp$fgm)

#NAs
tmp$fgm_age <- as.character(tmp$fgm_age)

tmp$fgm_age <- ifelse(tmp$fgm_age == "Pendant la petite enfance", "95", tmp$fgm_age)

tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))

total<-length(which(tmp$fgm_age >=  0 & tmp$fgm_age < 6))

# %0
at0 <- length(which(tmp$fgm_age==0))/total
# %1
at1 <- length(which(tmp$fgm_age==1))/total
# %2
at2 <- length(which(tmp$fgm_age==2))/total
# %3
at3 <- length(which(tmp$fgm_age==3))/total
# %4
at4 <- length(which(tmp$fgm_age==4))/total
# %5
at5 <- length(which(tmp$fgm_age==5))/total
#Test
sum(at0, at1, at2, at3, at4, at5) # must be 1

x0_5 <- sample(0:5, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2, at3, at4, at5))

tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_5, tmp$fgm_age)

# add time to event column
tmp$age_years <- as.numeric(as.character(tmp$age_years))
tmp$time <- ifelse(tmp$fgm == 1, tmp$fgm_age,
                   ifelse(tmp$fgm == 0, tmp$age_years, NA))

# Add to data frame
df_MICS <- bind_rows(mutate_all(df_MICS, as.character), mutate_all(tmp, as.character))

# Prepare dataframe -------------------------------------------------------

df_MICS$fgm <- ifelse(is.na(df_MICS$fgm), 0, df_MICS$fgm) 

############ Test #######################
#test <- df_MICS %>%
#filter(country == "Sudan")
#test$wgt <- as.numeric(test$wgt)
#mics_design_d  <- svydesign(id             = ~ cluster_number+hh_number,
#                           weight         = ~wgt,
#                          data           = test, 
#                         nest = T)

#svymean(~fgm, mics_design_d, na.rm=T)
#svytable(~fgm, mics_design_d)

#sum(2448.565, 15853.435 )
##########################################
# recode year of birth

df_MICS$year_birth <- as.numeric(as.character(df_MICS$year_birth))

df_MICS$cohort5 <- ifelse(1955 <= df_MICS$year_birth & df_MICS$year_birth < 1959, 1,
                          ifelse(1960 <=   df_MICS$year_birth &   df_MICS$year_birth < 1965, 2, 
                                 ifelse(1965 <=   df_MICS$year_birth &   df_MICS$year_birth < 1970, 3,
                                        ifelse(1970 <=   df_MICS$year_birth &   df_MICS$year_birth < 1975, 4,
                                               ifelse(1975 <=   df_MICS$year_birth &   df_MICS$year_birth < 1980, 5,
                                                      ifelse(1980 <=   df_MICS$year_birth &   df_MICS$year_birth < 1985, 6,
                                                             ifelse(1985 <=   df_MICS$year_birth &   df_MICS$year_birth < 1990, 7,
                                                                    ifelse(1990 <=   df_MICS$year_birth &   df_MICS$year_birth < 1995,8,
                                                                           ifelse(1995 <=   df_MICS$year_birth &   df_MICS$year_birth < 2000, 9,
                                                                                  ifelse(2000 <=   df_MICS$year_birth &   df_MICS$year_birth < 2005, 10,
                                                                                         ifelse(2005 <=   df_MICS$year_birth &   df_MICS$year_birth < 2010, 11,
                                                                                                ifelse(2010 <=   df_MICS$year_birth &   df_MICS$year_birth < 2015, 12, NA))))))))))))  


# add cohort index
df_MICS$cohort10 <- ifelse(1960 <=   df_MICS$year_birth &   df_MICS$year_birth < 1970, 1, 
                           ifelse(1970 <=   df_MICS$year_birth &   df_MICS$year_birth < 1980, 2,
                                  ifelse(1980 <=   df_MICS$year_birth &   df_MICS$year_birth < 1990, 3,
                                         ifelse(1990 <=   df_MICS$year_birth &   df_MICS$year_birth < 2000, 4,
                                                ifelse(2000 <=   df_MICS$year_birth &   df_MICS$year_birth < 2010, 5,
                                                       ifelse(2010 <=   df_MICS$year_birth &   df_MICS$year_birth < 2020, 6, NA))))))


df_MICS$survey <- "MICS" 

df_MICS$time <- as.numeric(df_MICS$time) # weights need to be numeric
df_MICS$re_wgt <- as.numeric(df_MICS$re_wgt) # weights need to be numeric
df_MICS$fgm <- as.numeric(df_MICS$fgm) # weights need to be numeric


setwd("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets")
saveRDS(df_MICS, file = "survival_data_MICS_July2019.rds")



