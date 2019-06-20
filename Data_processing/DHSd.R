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

# The data has been downloaded from https://dhsprogram.com/data/
# ICF International. 1990-2018.
# Demographic and Health Surveys (various) [Datasets]. Calverton, Maryland: ICF International [Distributor], 2018.

# Working directory

setwd("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/DHS/daugthers")

listsav <- dir(pattern = "*.SAV")

# Executed statements -----------------------------------------------------

# Read in population data for denormalization of weights ------------------

population <- read.csv("population.csv")

# Create empty data frame to store data -----------------------------------

df_DHSd <- data.frame(matrix(NA, nrow = 0, ncol = 29))

# Start loop through all DHS ----------------------------------------------

for (i in 1:length(listsav)){ 
  
  # if limited memory only read in one DHS stat file at a time
  wide <- ReadSingleSAV(i) 
  a <- as.data.frame(attr(wide, "variable.labels"))
  # wide <- read.dta("burkina_faso.DTA", convert.factors = FALSE)
  if(i ==6){ #convert to Gregorian calendar for Ethiopia (i ==6)
    wide$vg008 <-  wide$V008+92
    wide$vg007 <- floor((wide$vg008-1)/12)
    wide$vg006 <- wide$vg008-12*wide$V007
    wide$V007 <- wide$vg007+1900
  }
  
  # select variables and create dataset for birth reshape and FGM reshape
  wide_allchildren <- wide %>%
    dplyr::select(c("CASEID",
                    "V000",                         # country code and phase
                    "V001","V002",                  # cluster and household number
                    "V007","V008",                  # year of interView, Date of interview (CMC)
                    "V005","V021","V022","V023",    # sample design
                    "V024","V025",                  # region and place of residence (rural/urban)
                    "G100", "G101",
                    "BIDX.01","BIDX.02","BIDX.03","BIDX.04","BIDX.05","BIDX.06","BIDX.07","BIDX.08","BIDX.09","BIDX.10","BIDX.11","BIDX.12","BIDX.13","BIDX.14","BIDX.15","BIDX.16","BIDX.17","BIDX.18","BIDX.19","BIDX.20",
                    "B4.01","B4.02","B4.03","B4.04","B4.05","B4.06","B4.07","B4.08","B4.09","B4.10","B4.11","B4.12","B4.13","B4.14","B4.15","B4.16","B4.17","B4.18","B4.19","B4.20",  # sex of child
                    "B5.01","B5.02","B5.03","B5.04","B5.05","B5.06","B5.07","B5.08","B5.09","B5.10","B5.11","B5.12","B5.13","B5.14","B5.15","B5.16","B5.17","B5.18","B5.19","B5.20",
                    "B2.01","B2.02","B2.03","B2.04","B2.05","B2.06","B2.07","B2.08","B2.09","B2.10","B2.11","B2.12","B2.13","B2.14","B2.15","B2.16","B2.17","B2.18","B2.19","B2.20",  # year of Birth
                    "B8.01","B8.02","B8.03","B8.04","B8.05","B8.06","B8.07","B8.08","B8.09","B8.10","B8.11","B8.12","B8.13","B8.14","B8.15","B8.16","B8.17","B8.18","B8.19","B8.20",
                    "B3.01","B3.02","B3.03","B3.04","B3.05","B3.06","B3.07","B3.08","B3.09","B3.10","B3.11","B3.12","B3.13","B3.14","B3.15","B3.16","B3.17","B3.18","B3.19","B3.20")) # age of child (CMC)
  
  wide_fgm <- wide %>%
    dplyr::select(c("CASEID", 
                    "GIDX.01","GIDX.02","GIDX.03","GIDX.04","GIDX.05","GIDX.06","GIDX.07","GIDX.08","GIDX.09","GIDX.10","GIDX.11","GIDX.12","GIDX.13","GIDX.14","GIDX.15","GIDX.16","GIDX.17","GIDX.18","GIDX.19","GIDX.20", 
                    "G121.01","G121.02","G121.03","G121.04","G121.05","G121.06","G121.07","G121.08","G121.09","G121.10","G121.11","G121.12","G121.13","G121.14","G121.15","G121.16","G121.17","G121.18","G121.19","G121.20",  # FGM status
                    "G122.01","G122.02","G122.03","G122.04","G122.05","G122.06","G122.07","G122.08","G122.09","G122.10","G122.11","G122.12","G122.13","G122.14","G122.15","G122.16","G122.17","G122.18","G122.19","G122.20")) # year of cutting
  
  # reshape FGM module
  long_fgm <- reshape(wide_fgm, varying = c(2:61), direction = "long", idvar = "CASEID", sep = ".", timevar = "order")
  long_fgm <- long_fgm %>%
    filter(!is.na(GIDX))
  # create ID for merge
  long_fgm <- long_fgm %>%
    mutate(id.match = paste(CASEID, GIDX, sep = ""))
  
  # reshape ALL CHILDREN
  long_allchildren <- reshape(wide_allchildren, varying = c(15:134), direction = "long", idvar = "CASEID", sep = ".", timevar="order")
  long_allchildren <- long_allchildren %>%
    filter(!is.na(BIDX))
  
  # create ID for merge
  long_allchildren <- long_allchildren %>%
    mutate(id.match = paste(CASEID, BIDX, sep = "")) 
  
  
  if(i ==6){ # convert to Gregorian calendar for Ethiopia (i==6)
    long_allchildren$vg008 <-    long_allchildren$B3+92 # V011: Date of birth (CMC)
    long_allchildren$vg007 <-    floor(( long_allchildren$vg008-1)/12)
    long_allchildren$vg006 <-    long_allchildren$vg008-12* long_allchildren$B2 # V010: Respondent's year of birth
    long_allchildren$B2    <-    long_allchildren$vg007+1900
  }
  
  # merge data sets
  df <- merge(long_fgm, long_allchildren, by = "id.match")
  
  df <- df %>%
    filter(as.numeric(as.character(B8)) < 15) %>% # only include daugthers under 15
    select(c("V000", "V001", "V002", "V005", "V007", "V008", "V021", "V022", "V023", "V024",
             "V025", "B4", "B2", "B8", "B3", "G121", "G122", "G100", "G101")) 
  
  
  if(!all(is.na(long_allchildren$G101))){
    long_allchildren$G100 <- ifelse(as.numeric(long_allchildren$G101) == 2 | as.numeric(long_allchildren$G100) == 2, 
                                    2, long_allchildren$G100)
  }
  
  if(i == 5){ 
    long_allchildren <- long_allchildren %>%
      filter(as.numeric(G100) == 2) %>%
      filter(as.numeric(B4) == 2) %>%
      filter(as.numeric(B5) == 2) %>%
      filter(as.numeric(B8) < 15) %>%
      mutate(G121 = "No") %>%
      mutate(G122 = NA) %>%
      select(c("V000", "V001", "V002", "V005", "V007", "V008", "V021", "V022", "V023", "V024",
               "V025", "B4", "B2", "B8", "B3", "G121", "G122", "G100", "G101")) 
  }

  if(i != 5){
  # non FGM birth recode
  long_allchildren <- long_allchildren %>%
    filter(as.numeric(G100) == 1) %>%
    filter(as.numeric(B4) == 2) %>%
    filter(as.numeric(B5) == 2) %>%
    filter(as.numeric(B8) < 15) %>%
    mutate(G121 = "No") %>%
    mutate(G122 = NA) %>%
    select(c("V000", "V001", "V002", "V005", "V007", "V008", "V021", "V022", "V023", "V024",
             "V025", "B4", "B2", "B8", "B3", "G121", "G122", "G100", "G101")) 
  }
  
  # append to complete dataset
  df <- rbind(df, long_allchildren)
  
  # Re-calculate NAs, distinguish between no age given, and respondent indicating 'during infancy'
  
  # recode fgm to numeric variable
  
  df$fgm_status <- ifelse(df$G121 == "Yes" , 1,
                          ifelse(df$G121 == "No", 0, NA))
  
  df$wgt <- df$V005/1000000
  
  # Test ################################################################################################
  #mics_design_d  <- svydesign(id         = ~V021, # primary sampling units shoudl be same as cluster_number
  #                           strata         = ~V022, 
  #                          weight         = ~wgt,
  #                         data           = df, 
  #                        nest = T)
  
  #svymean(~fgm_status, mics_design_d, na.rm=T)
  #svytable(~fgm_status, mics_design_d)
  #svytable(~G121, mics_design_d)
  # Test ################################################################################################
  
  df$G122 <- ifelse(df$G122 == "During first week" |
                      df$G122 == "After first week and before the first year", 0, df$G122)
  
  df$G122 <- ifelse(df$G122 == "During infancy" |
                      df$G122 == "At birth/during infancy**", "95", df$G122)
  
  df$G122 <- as.numeric(as.character(df$G122))
  
  total<-length(which(df$G122 >= 0 & df$G122<=2))
  
  # %3
  at0 <- length(which(df$G122==0))/total
  # %4
  at1 <- length(which(df$G122==1))/total
  # %5
  at2 <- length(which(df$G122==2))/total
  
  #Test
  sum(at0, at1, at2) # must be 1
  
  x0_2 <- sample(0:2, size=length(which(df$G122==95)), replace=TRUE, prob=c(at0, at1, at2))
  
  df$G122 <- ifelse(df$G122 == 95, x0_2, df$G122)
  
  df$G122 <- ifelse(df$G122 > 95 , NA, df$G122)
  
  # add time to event column
  df$time <- ifelse(df$fgm_status == 0, as.numeric(as.character(df$B8)),
                    ifelse(df$fgm_status == 1, as.numeric(as.character(df$G122)), NA))
  
  
  # add id column that is 1 for each recrod in order to count unweighted and weighted population sizes
  df$id <- 1
  
  # create country variable
  df$country <- substr(as.character(df$V000), 0, 2)
  
  #add uniuqe cluster_numbers
  df$year <- as.character(df$V007)
  
  df <- df %>%
    mutate(cluster_number_unique = paste(V001,country,year, sep="")) %>%
    mutate(strata_unique = paste(V022, country, year, sep="")) %>%
    mutate(survey_id = i)
  
  # Recode country name 
  df$country <- ifelse(df$country == "BJ", "Benin",
                       ifelse(df$country == "BF", "Burkina Faso",
                              ifelse(df$country == "SD", "Sudan",
                                     ifelse(df$country == "CM", "Cameroon",
                                            ifelse(df$country == "CF", "Central African Republic", 
                                                   ifelse(df$country == "TD", "Chad",
                                                          ifelse(df$country == "CI", "Cote d'Ivoire", 
                                                                 ifelse(df$country == "EG", "Egypt", 
                                                                        ifelse(df$country == "ET", "Ethiopia",
                                                                               ifelse(df$country == "GM", "Gambia",
                                                                                      ifelse(df$country == "GH", "Ghana",
                                                                                             ifelse(df$country == "GN", "Guinea", 
                                                                                                    ifelse(df$country == "KE", "Kenya",
                                                                                                           ifelse(df$country == "ML", "Mali",
                                                                                                                  ifelse(df$country == "NI", "Niger",
                                                                                                                         ifelse(df$country == "NG", "Nigeria",
                                                                                                                                ifelse(df$country == "SN", "Senegal",
                                                                                                                                       ifelse(df$country == "SL", "Sierra Leone",
                                                                                                                                              ifelse(df$country == "TZ", "United Republic of Tanzania",
                                                                                                                                                     ifelse(df$country == "TG", "Togo",
                                                                                                                                                            ifelse(df$country == "UG", "Uganda", 
                                                                                                                                                                   ifelse(df$country == "YE", "Yemen",
                                                                                                                                                                          ifelse(df$country == "LB", "Liberia", df$country)))))))))))))))))))))))
  
  # if year is wrongly coded sapply(df$v007[1], nchar)
  if (sapply(as.character(df$V007[1]), nchar) < 4){
    df$V007 <- ifelse(df$V007 == 99, 1999, 
                      ifelse(df$V007 == 98, 1998,
                             ifelse(df$V007 == 97, 1997,
                                    ifelse(df$V007 == 96, 1996,
                                           ifelse(df$V007 == 95, 1995,
                                                  ifelse(df$V007 == 94, 1994,
                                                         ifelse(df$V007 == 93, 1993,
                                                                ifelse(df$V007 == 92, 1992,
                                                                       ifelse(df$V007 == 91, 1991,
                                                                              ifelse(df$V007 == 90, 1990,
                                                                                     ifelse(df$V007 == 89, 1989,
                                                                                            ifelse(df$V007 == 88, 1988, df$V007))))))))))))
  }
  
  df$n_survey <- nrow(df)
  
  # get total population of 15-19 year olds at mid-year of the survey
  df <- merge(population, df, by = c("country", "year"))
  
  # denormalize weight
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-9-49
  # https://rpubs.com/corey_sparks/27276
  
  df <- df %>%
    mutate(V005 = as.numeric(V005)/1000000)
  
  df <- df %>%
    mutate(re_wgt = V005*(as.numeric(pop)*1000)/n_survey)
  
  # add cohort index
  df$cohort10 <- ifelse(1960 <=   df$B2 &   df$B2 < 1970, 1, 
                        ifelse(1970 <=   df$B2 &   df$B2 < 1980, 2,
                               ifelse(1980 <=   df$B2 &   df$B2 < 1990, 3,
                                      ifelse(1990 <=   df$B2 &   df$B2 < 2000, 4,
                                             ifelse(2000 <=   df$B2 &   df$B2 < 2010, 5,
                                                    ifelse(2010 <=   df$B2 &   df$B2 < 2020, 6, NA))))))
  
  df$cohort5 <- ifelse(1955 <= df$B2 & df$B2 < 1959, 1,
                       ifelse(1960 <= df$B2 &  df$B2 < 1965, 2, 
                              ifelse(1965 <=   df$B2 &   df$B2 < 1970, 3,
                                     ifelse(1970 <=   df$B2 &   df$B2 < 1975, 4,
                                            ifelse(1975 <=   df$B2 &   df$B2 < 1980, 5,
                                                   ifelse(1980 <=   df$B2 &   df$B2 < 1985,6,
                                                          ifelse(1985 <=   df$B2 &   df$B2 < 1990,7,
                                                                 ifelse(1990 <=   df$B2 &   df$B2 < 1995, 8,
                                                                        ifelse(1995 <=   df$B2 &   df$B2 < 2000, 9,
                                                                               ifelse(2000 <=   df$B2 &   df$B2 < 2005, 10,
                                                                                      ifelse(2005 <=   df$B2 &   df$B2 < 2010, 11,
                                                                                             ifelse(2010 <=   df$B2 &   df$B2 < 2020, 12,NA))))))))))))
  # select and reorder columns
  df <- df %>%
    select(c("country", "year", "pop",
             "B8", "B2", "V001",
             "V002", "V005", "V021", 
             "V022", "V023", 
             "V025", "V024", 
             "fgm_status", "G122", 
             "cluster_number_unique", "strata_unique", 
             "survey_id", "n_survey", "re_wgt", 
             "time", "cohort10", "cohort5"))
  
  # name columns
  names(df) <- c("country", "year", "pop", 
                 "age_years", "year_birth", "cluster_number", 
                 "hh_number", "wgt", "prim_samp_unit", 
                 "strata", "stratification", 
                 "residence", "region", 
                 "fgm", "fgm_age", 
                 "cluster_number_unique", "strata_unique", "survey_id", 
                 "n_survey", "re_wgt",  
                 "time", "cohort10", "cohort5")
  
  if(df$country[1] == "Kenya"){# North eastern province has only be added after 1998 DHS in Kenya
    # for 2008_09 and 2014 we need to excluded if we want to assure 
    # comparability
    df <- df%>%
      filter(region != "North Eastern")
  }
  
  # Append data
  df_DHSd <- bind_rows(mutate_all(df_DHSd, as.character), mutate_all(df, as.character))
}

df_DHSd$survey <- "DHS" # add column of data source for later merge with MICS data

setwd("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets")

saveRDS(df_DHSd, file = "survival_data_DHS_daughters_July2019.rds")

