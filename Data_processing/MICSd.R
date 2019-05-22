# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny

# File description, purpose of code, inputs and output --------------------

# This code creates a cross-sectional data sets of all DHS with FGM reported by mothers 
# with respect to their daugthers. 
# Reads in the data, filters for the relevant indicators: 
# clustering and stratification due to the survey design,
# age (single years, year of birth and age group), residence (rural/urban), knowledge about FGM,
# if available, if the woman has experienced FGM, age at which she experienced it,
# and if she thinks, the practice should continue, if available

# the code only includes datasets with FGM status and age-at-FGM.

# The output of this codes is the df_MICSd data frame with all cross-sectional MICS FGM data 

# The data has been downloaded from INSERT MICS SOURCE

setwd("C:/Users/weny/Google Drive/2018/FGM/02 -Trend modelling/01- Data/MICS/daughters")

# Executed statements ----------------------------------------------------------

# read in population data
population <- read.csv("population.csv")

# Benin 2014 -------------------------------------------------------------------

survey <- "Benin_2014"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM
fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "WM6Y", "fgm_status", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))


fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Benin"
df$year    <- 2014 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- df

# Central African Republic 2010 ------------------------------------------------

survey <- "Central_African_Republic_2010"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
hl <- read.spss(paste(survey,"_hl.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(hl, "variable.labels"))

hl <- hl %>%
  select(c("HH1", "HH2", "HL4", "HL6", "HL8", "HL9"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

hl$LN <- ifelse(!is.na(as.numeric(as.character(hl$HL8))), as.numeric(as.character(hl$HL8)), (as.numeric(as.character(hl$HL9))))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, hl, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$HL6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(HL4) == 2)    # select only girls
#filter(as.numeric(BH5) == 1)   # this is not a birth recode

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Central African Republic"
df$year    <- 2010

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = WMWEIGHT*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Cote dIvoire 2016 -------------------------------------------------------------------

survey <- "Cote d'Ivoire_2016"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

wm$WM6Y <- 2016 #wrongly coded in recode

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) == 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)


fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "WM6Y" , "fgm_status", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Cote d'Ivoire"
df$year    <- 2016 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Gambia 2010 -------------------------------------------------------------------

survey <- "Gambia_2010"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
hl <- read.spss(paste(survey,"_hl.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(hl, "variable.labels"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

hl <- hl %>%
  select(c("HH1", "HH2", "HL4", "HL6", "HL8", "HL9"))

hl$LN <- ifelse(!is.na(as.numeric(as.character(hl$HL8))), as.numeric(as.character(hl$HL8)), (as.numeric(as.character(hl$HL9))))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "WM6Y", "HH6", "HH7A")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "WM6Y", "HH6", "HH7A")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, hl, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$HL6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(HL4) == 2)    # select only girls
#filter(as.numeric(BH5) == 1)   # this is not a birth recode

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7A"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

colnames(df)[9] <- "HH7"

# add country column
df$country <- "Gambia"
df$year    <- 2010

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = WMWEIGHT*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Ghana 2011  -------------------------------------------------------------------

survey <- "Ghana_2011"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) == 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "No") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Ghana"
df$year    <- 2011 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Guinea 2016 -------------------------------------------------------------------

survey <- "Guinea_2016"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

wm$WM6Y <- 2016 #wrongly coded in recode

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) == 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Guinea"
df$year    <- 2016 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Guinea-Bissau 2014 -----------------------------------------------------------

survey <- "Guinea_Bissau_2014"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) == 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "N?o") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Guinea-Bissau"
df$year    <- 2014 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Mali 2015 --------------------------------------------------------------------

survey <- "Mali_2015"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("WM1", "WM2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("WM1", "WM2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

wm$WM6Y <- 2015 # for some reason the year of the interview is not in the dataset

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("WM1", "WM2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("WM1", "WM2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("WM1", "WM2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("WM1", "WM2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("WM1", "WM2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Mali"
df$year    <- 2015 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Mauritania 2011  --------------------------------------------------------------

survey <- "Mauritania_2011"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

wm$WM6Y <- 2011 # omitted to include in recode

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16", "FG16U"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0) %>%
  mutate(FG16U = NA)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "WMWEIGHT", "FG13", "FG15", "FG16", "fgm_status", "FG16U", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Mauritania"
df$year    <- 2011 

# Recode variables of interest 

df$FG16 <- ifelse(df$FG16U == "JOUR", round(as.numeric(as.character(df$FG16))/365, 0),
                  ifelse(df$FG16U == "MOIS", round(as.numeric(as.character(df$FG16))/12, 0), 
                         ifelse(df$FG16U == "ANNEE", round(as.numeric(as.character(df$FG16)), 0), 
                                ifelse(df$FG16U == "NSP" | df$FG16U == "MISSING", NA, NA))))

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = WMWEIGHT*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Mauritania 2015  --------------------------------------------------------------

survey <- "Mauritania_2015"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "Non") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0) 

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Mauritania"
df$year    <- 2015 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Nigeria 2011 -----------------------------------------------------------------

survey <- "Nigeria_2011"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
hl <- read.spss(paste(survey,"_hl.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(hl, "variable.labels"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

hl <- hl %>%
  select(c("HH1", "HH2", "HL4", "HL6", "HL8", "HL9" ))

hl$LN <- ifelse(!is.na(as.numeric(as.character(hl$HL8))), as.numeric(as.character(hl$HL8)), (as.numeric(as.character(hl$HL9))))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, hl, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$HL6

fgtemp <- fgtemp %>%
  mutate(FG15 = "No") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(HL4) == 2)    # select only girls
#filter(as.numeric(BH5) == 1)   # this is not a birth recode

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Nigeria"
df$year    <- 2011

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Nigeria 2017 -----------------------------------------------------------------

survey <- "Nigeria_2017"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "No") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0) 

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Nigeria"
df$year    <- 2017 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Sierra Leone 2010 ------------------------------------------------------------

survey <- "Sierra_Leone_2010"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
hl <- read.spss(paste(survey,"_hl.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(hl, "variable.labels"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

hl <- hl %>%
  select(c("HH1", "HH2", "HL4", "HL6", "HL8", "HL9" ))

hl$LN <- ifelse(!is.na(as.numeric(as.character(hl$HL8))), as.numeric(as.character(hl$HL8)), (as.numeric(as.character(hl$HL9))))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, hl, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$HL6

fgtemp <- fgtemp %>%
  mutate(FG15 = "No") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(HL4) == 2)    # select only girls
#filter(as.numeric(BH5) == 1)   # this is not a birth recode

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Sierra Leone"
df$year    <- 2010

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Sierra Leone 2017 ------------------------------------------------------------

survey <- "Sierra_Leone_2017"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG15", "FG17", "FG18"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG15 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG17 = "No") %>%
  mutate(FG18 = NA)%>%
  mutate(fgm_status = 0) 

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG15", "FG17", "FG18", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG17) == 1, 1, 
                        ifelse(as.numeric(fg$FG17) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG15)) < 15 & as.numeric(as.character(FG15)) >= 0)

colnames(df)[12] <- "FG16" # age at FGM
colnames(df)[11] <- "FG15" # FGM or not
colnames(df)[10] <- "FG13" # age of daugther

# add country column
df$country <- "Sierra Leone"
df$year    <- 2017 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Sudan 2014 -------------------------------------------------------------------

survey <- "Sudan_2014"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
bh <- read.spss(paste(survey,"_bh.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(bh, "variable.labels"))

bh <- bh %>%
  select(c("HH1", "HH2", "LN", "BHLN", "BH3", "BH5", "BH6"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG1) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, bh, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$BH6

fgtemp <- fgtemp %>%
  mutate(FG15 = "No") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0) 

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG1) >= 2) %>%
  filter(as.numeric(BH3) == 2) %>%
  filter(as.numeric(BH5) == 1)

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Sudan"
df$year    <- 2014 

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Togo 2010 ---------------------------------------------------------------

survey <- "Togo_2010"

fg <- read.spss(paste(survey,"_fg.sav", sep =""), to.data.frame = TRUE) 
wm <- read.spss(paste(survey,"_wm.sav", sep =""), to.data.frame = TRUE)
hl <- read.spss(paste(survey,"_hl.sav", sep =""), to.data.frame = TRUE)

a <- as.data.frame(attr(fg, "variable.labels"))
b <- as.data.frame(attr(wm, "variable.labels"))
c <- as.data.frame(attr(hl, "variable.labels"))

fg <- fg %>%
  select(c("HH1", "HH2", "LN", "FG13", "FG15", "FG16"))

hl <- hl %>%
  select(c("HH1", "HH2", "HL4", "HL6", "HL8", "HL9" ))

hl$LN <- ifelse(!is.na(as.numeric(as.character(hl$HL8))), as.numeric(as.character(hl$HL8)), (as.numeric(as.character(hl$HL9))))

# Create dataframe with only women who have not heard of FGM

fgtemp <- wm %>%
  filter(as.numeric(FG2) >= 2) %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# Create dataframe with only women who have heard of FGM
fgtemp1 <- wm %>%
  filter(as.numeric(FG2) == 1 | as.numeric(FG1) == 1)%>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "WM6Y", "HH6", "HH7")) 

# add FG15 (age) to fgtemp
fgtemp <- merge(fgtemp, hl, by=c("HH1", "HH2", "LN"))
fgtemp$FG13 <- fgtemp$HL6

fgtemp <- fgtemp %>%
  mutate(FG15 = "No") %>%
  mutate(FG16 = NA)%>%
  mutate(fgm_status = 0)

fgtemp2 <- fgtemp %>%
  filter(as.numeric(FG2) >= 2) %>%
  filter(as.numeric(HL4) == 2)    # select only girls
#filter(as.numeric(BH5) == 1)   # this is not a birth recode

fgtemp2 <- fgtemp2 %>%
  select(c("HH1", "HH2", "LN", "FG1", "FG2", "wmweight", "FG13", "FG15", "FG16", "fgm_status", "WM6Y", "HH6", "HH7"))

# FGM dataset
fg <- merge(fgtemp1, fg, by =c("HH1", "HH2", "LN"))

fg$fgm_status <- ifelse(as.numeric(fg$FG15) == 1, 1, 
                        ifelse(as.numeric(fg$FG15) == 2, 0, NA))

df <- rbind(fg, fgtemp2)

df <- df %>%
  filter(as.numeric(as.character(FG13)) < 15 & as.numeric(as.character(FG13)) >= 0)

# add country column
df$country <- "Togo"
df$year    <- 2010

# Recode variables of interest 

df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$FG13)),
                  ifelse(df$fgm_status == 1, as.numeric(as.character(df$FG16)), NA))

df$time <- ifelse(df$time == 98 | df$time == 98, NA, df$time)

# count unweighted surveyed 45-19 year olds
df$n_survey <- nrow(df)

# get total population of 15-49 year olds at mid-year of the survey
df <- merge(population, df, by = c("country", "year"))

# denormalize weight
df <- df %>%
  mutate(re_wgt = wmweight*(as.numeric(pop)*1000)/n_survey)

df_MICSd <- bind_rows(mutate_all(df_MICSd, as.character), mutate_all(df, as.character))

# Prepare dataset ---------------------------------------------------------

# Need to recreate stratification from survey data: see MICS 2000 report for explication of stratification
df_MICSd$strata <- paste(df_MICSd$HH6, df_MICSd$HH7) # tmp$strata is only the administrative region

# create unique cluster and strata variables
df_MICSd <- df_MICSd %>%
  mutate(cluster_number_unique = paste(HH1,country,year, sep="")) %>%
  mutate(strata_unique = paste(strata,country,year, sep=""))

# create year of birth of girls
df_MICSd <- df_MICSd %>%
  mutate(year_birth = as.numeric(df_MICSd$WM6Y) - as.numeric(df_MICSd$FG13))

# add cohort index
df_MICSd$cohort10 <- ifelse(1960 <= df_MICSd $year_birth & df_MICSd $year_birth < 1970, 1, 
                            ifelse(1970 <= df_MICSd $year_birth & df_MICSd $year_birth < 1980, 2,
                                   ifelse(1980 <= df_MICSd $year_birth & df_MICSd $year_birth < 1990, 3,
                                          ifelse(1990 <= df_MICSd $year_birth & df_MICSd $year_birth < 2000, 4,
                                                 ifelse(2000 <= df_MICSd $year_birth & df_MICSd $year_birth < 2010, 5,
                                                        ifelse(2010 <= df_MICSd $year_birth & df_MICSd $year_birth < 2020, 6, NA))))))

# add cohort index
df_MICSd$cohort5 <- ifelse(1955 <= df_MICSd$year_birth & df_MICSd$year_birth < 1959, 1,
                           ifelse(1960 <= df_MICSd$year_birth & df_MICSd$year_birth < 1965, 2, 
                                  ifelse(1965 <= df_MICSd$year_birth & df_MICSd$year_birth < 1970, 3,
                                         ifelse(1970 <= df_MICSd$year_birth & df_MICSd$year_birth < 1975, 4,
                                                ifelse(1975 <= df_MICSd$year_birth & df_MICSd$year_birth < 1980, 5,
                                                       ifelse(1980 <= df_MICSd$year_birth & df_MICSd$year_birth < 1985, 6,
                                                              ifelse(1985 <= df_MICSd$year_birth & df_MICSd$year_birth < 1990, 7,
                                                                     ifelse(1990 <= df_MICSd$year_birth & df_MICSd$year_birth < 1995, 8,
                                                                            ifelse(1995 <= df_MICSd$year_birth & df_MICSd$year_birth < 2000, 9,
                                                                                   ifelse(2000 <= df_MICSd$year_birth & df_MICSd$year_birth < 2005, 10,
                                                                                          ifelse(2005 <= df_MICSd$year_birth & df_MICSd$year_birth < 2010, 11,
                                                                                                 ifelse(2010 <= df_MICSd$year_birth & df_MICSd$year_birth < 2020, 12,NA))))))))))))


# recode weight variable due to inconsistent coding
df_MICSd$wmweight <- ifelse(!is.na(df_MICSd$wmweight), df_MICSd$wmweight, 
                            df_MICSd$WMWEIGHT)

# Select variables from combined data file
names(df_MICSd)
df_MICSd$wscore <- NA
df_MICSd$welevel<- NA
df_MICSd$WM6D   <- NA
df_MICSd$WM6M   <- NA
df_MICSd$WM6Y   <- NA
df_MICSd$windex5 <- NA
df_MICSd$CEB    <- NA
df_MICSd$FG20   <- NA
df_MICSd$year_vis   <- NA

df_MICSd <- df_MICSd %>%
  dplyr::select(c("country", "year",
                  "pop",
                  "FG13","wscore",                                         # age in single years (FG13), wscore is a placeholder for age groups
                  "year_birth",
                  "HH1", "HH2", "wmweight", "FG1", "strata", "FG2",        # sample design, FG1 and FG2 are placeholders
                  "HH6",                                                   # rural/urban
                  "HH7",                                                   # governorate, needed for recreation of strata
                  "welevel", "WM6D", "WM6M", "WM6Y",                       # education indicators, placeholrders
                  "windex5",                                               # wealth quintile
                  "CEB",                                                   # knowledge of FGM, placeholder
                  "fgm_status",                                            # FGM or not
                  "FG16",                                                  # age at FGM
                  "FG20",                                                  # approval/disapproval of FGM, placeholder
                  "cluster_number_unique", "strata_unique",                 
                  "n_survey", "re_wgt", "year_vis", "time", 
                  "cohort10", "cohort5"))

# rename columns
names(df_MICSd) <- c("country",              
                     "year",                 
                     "pop",                  
                     "age_years",            
                     "age_groups",           
                     "year_birth",          
                     "cluster_number",       
                     "hh_number",            
                     "wgt",                  
                     "prim_samp_unit",       
                     "strata",               
                     "stratification",       
                     "residence",            
                     "region",               
                     "highest_ed_level",     
                     "years_ed_for_highest", 
                     "ed_single_years",      
                     "ed_detail",            
                     "wealth",               
                     "fgm_know",             
                     "fgm",                  
                     "fgm_age",              
                     "support",              
                     "cluster_number_unique",
                     "strata_unique",        
                     "n_survey",             
                     "re_wgt",               
                     "year_vis",             
                     "time",                 
                     "cohort10",
                     "cohort5")

df_MICSd$survey <- "MICS" 

saveRDS(df_MICSd, file = "survival_data_MICS_daughters_February2019.rds")

setwd("C:/Users/weny/Google Drive/2018/FGM/02 -Trend modelling/FGM-trend-analysis/Codes")
