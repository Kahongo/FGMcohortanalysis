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

# Working directory

setwd("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/DHS")

listsav <- dir(pattern = "*.SAV")

# Executed statements -----------------------------------------------------

# Read in population data for denormalization of weights ------------------

population <- read.csv("population.csv")

# Create empty data frame to store data -----------------------------------

df_DHS <- data.frame(matrix(NA, nrow = 0, ncol = 29))

# Start loop through all DHS ----------------------------------------------

for(i in 1:length(listsav)){ 
  # Read in data ----------------------------------------------------------
  
  tmp <- ReadSingleSAV(i)
  a <- as.data.frame(attr(tmp, "variable.labels"))
  
  # Extract countries and DHS phases (wave I - VII) ------------------------
  if(i == 16){ # convert to Greogrian calendar
    tmp$vg008 <-  tmp$V008+92
    tmp$vg007 <- floor((tmp$vg008-1)/12)
    tmp$vg006 <- tmp$vg008-12*tmp$V007
    tmp$V007 <- tmp$vg007+1900
    
    tmp$vg008 <-  tmp$V011+92
    tmp$vg007 <- floor((tmp$vg008-1)/12)
    tmp$vg006 <- tmp$vg008-12*tmp$V010
    tmp$V010 <- tmp$vg007+1900
  }
  
  if (i != 41 & i != 15){ # i = 40 Tanzania 1996 and i = 15 Egypt 2015 - non standard DHS surveys
    tmp$country <- substr(as.character(tmp$V000), 0, 2)
    tmp$wave <- substr(tmp$V000, 3, 3)
  } 
  
  if(i == 15){ # Egypt 2015 coded totally different - lack the country column
    tmp$country <- "Egypt"
  }
  
  if(i == 41){ # Tanzania 1996 is coded differently and variable are code v instead of Vm R unfortunately is case sensitive
    tmp$country <- substr(as.character(tmp$v000), 0, 2)
    tmp$wave <- substr(tmp$v000, 3, 3)
  }
  
  # Egypt 2015 --------------------------------------------------------------
  # non-standard DHS survey and its own thing: https://userforum.dhsprogram.com/index.php?t=msg&goto=14324&S=Google
  if (tmp$country[1] == "Egypt"){ # dataset does not contain country column
    
    # Select variables from combined data file
    tmp <- tmp %>%
      dplyr::select(c("country", "A110C","AGEND",                              # country, age (continous and group)
                      "AINTY",                                                 # year of interview
                      "A109Y",                                                 # year of birth
                      "CRELAT" ,"HNUMBER","WEIGHT","HPSU","CRESP","ATEAM", # sample design
                      "ATYPE",                                                 # rural/urban
                      "AGOVERN",                                               # governorate, needed for recreation of strata
                      "A508",                                                  # knowledge of FGM
                      "A503",                                                  # FGM or not
                      "A504",                                                  # age at FGM
                      "A511"))                                                 # approval/disapproval of FGM
    
    # rename columns
    names(tmp) <- c("country", "age_years", "age_groups",
                    "year", 
                    "year_birth",
                    "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                    "residence", "region",
                    "fgm_know", 
                    "fgm",
                    "fgm_age",
                    "support")
    
    test <- tmp %>%
      filter(fgm == "No")
    
    # Create stratification: https://userforum.dhsprogram.com/index.php?t=tree&th=4622&goto=8481&S=39796439d8b4cca149d1e3c9b2778a7f#page_top
    tmp$strata <- paste(tmp$residence,tmp$governorate)
    tmp$year_birth <- as.numeric(as.character(tmp$year_birth))
    
    # set columns with missing data to NA
    tmp$cluster_number <- NA
    tmp$stratification <- NA
  } 
  
  # DHS wave 2: 1988-1993 - approximate date --------------------------------
  # No datasets with FGM data
  # DHS wave 3: 1992-1997 - approximate date --------------------------------
  if (!is.null(tmp$wave)){ # code needs to jump over this section if data was already processed and wave no longer exists as column
    test <- c(tmp$wave == 3)[1] # select countries in the third wave
    if (test == TRUE) {
      if (tmp$country[1] == "KE"){ # Kenya 1998
        
        name <- tmp$country[1]
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                   
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025",
                          "V024",
                          "S1001",  # FGM 'in community', not have you heard of FGM, placeholder                                     
                          "S1002",
                          "S1003",
                          "S1012"))                         
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(tmp$year_birth)
        tmp$fgm_know <- NA
      }
      
      if (tmp$country[1] == "GN"){ # Guinea 1998-1999
        
        name <- tmp$country[1]
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                   
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025",
                          "V024",
                          "V901",        # no probing questions, 'heard of FGM' only asked once                              
                          "V902",
                          "V906",
                          "V922"))                                     
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(tmp$year_birth)
      }
      if (tmp$country[1] == "TZ"){ # Tanzania 1996
        
        name <- tmp$country[1]
        
        # Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "v012","v013",                    
                          "v007",
                          "v010",
                          "v001" ,"v002","v005","v021","v022","v023",  
                          "v025", "v024",
                          "v105",                                      # placeholder, no knowledge of fGM, women circumcised in the area
                          "s1003",
                          "s1004",
                          "v004"))                                     # placeholder, no data for approval
        
        tmp$s1003 <- ifelse(tmp$s1003 == "Never circumcised", 0, 
                            ifelse(is.na(tmp$s1003), NA, 1))
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        
        # set missing variables to NA
        tmp$fgm_know <- NA
        tmp$support  <- NA
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(tmp$year_birth)
      }
      if (tmp$country[1] == "BF"){ # Burkina Faso 1999
        
        name <- tmp$country[1]
        
        # Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025", "V024",                                    
                          "S901",      # no probing question asked, however asked "knows of FGM"                                
                          "S903",
                          "S904",
                          "S916"))                                     
        
        # Recode S903 variables, as nonesense in data
        tmp$S903 <- ifelse(tmp$S903 == "Not circumcised", 0, ifelse(is.na(tmp$S903), NA, 1))
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(as.character(tmp$year_birth))
        
      }
      if (tmp$country[1] == "CF"){ # Central African Republic 1994-1995
        
        name <- tmp$country[1]
        
        tmp <- tmp %>%
          filter(SPREF != "Vakaga") # Vakaga was exluded in subsequent surveys and also needs to be excluded here
        
        # Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025", "V024",                              
                          "V004",                                      # placeholder, there is no data on if every heard of FGM
                          "S1001", 
                          "S1002",
                          "S1005"))                                    
        
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        
        # Set missing variables to NA
        tmp$fgm_know  <- NA
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(tmp$year_birth)
      }
      if (tmp$country[1] == "CI"){
        if(tmp$V007[1] == 94){ # Cote d'Ivoire 1994, there were two DHs' phase III in Cote divoire
          
          name <- tmp$country[1]
          
          # Select variables
          tmp <- tmp %>%
            dplyr::select(c("country", "V012","V013",                   
                            "V007",  
                            "V010",
                            "V001" ,"V002","V005","V021","V022","V023",  
                            "V025", "V024",
                            "V004",                                      # placeholder, there is no data if every heard of FGM
                            "S229", 
                            "S231",
                            "V008"))                                     # placeholder, there is no data on approval/disapproval
          
          
          
          # rename
          names(tmp) <- c("country", "age_years", "age_groups",
                          "year", 
                          "year_birth",
                          "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                          "residence", "region",
                          "fgm_know", 
                          "fgm",
                          "fgm_age",
                          "support")
          
          # Set missing variables to NA 
          tmp$fgm_know <- NA
          tmp$support  <- NA
          tmp$year_birth <- paste(19,tmp$year_birth, sep="")
          tmp$year_birth <- as.numeric(as.character(tmp$year_birth))
        }
      } 
      if(!is.null(tmp$wave)){
        if (tmp$country[1] == "CI"){
          if(tmp$V007[1] == 1999 | tmp$V007[1] == 1998){ # Cote d'Ivoire 1999, there were two DHs' phase III in Cote divoire
            
            name <- tmp$country[1]
            
            #Select variables
            tmp <- tmp %>%
              dplyr::select(c("country", "V012","V013",                    
                              "V007",  
                              "V010",
                              "V001" ,"V002","V005","V021","V022","V023",  
                              "V025",  "V024",                                  
                              "S901",   # heard of FGM, but no probing question                                   
                              "S902",
                              "S904",
                              "S916"))                                     
            
            # rename
            names(tmp) <- c("country", "age_years", "age_groups",
                            "year", 
                            "year_birth",
                            "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                            "residence", "region",
                            "fgm_know", 
                            "fgm",
                            "fgm_age",
                            "support")
            
            tmp$year_birth <- paste(19,tmp$year_birth, sep="")
            tmp$year_birth <- as.numeric(as.character(tmp$year_birth))
          }
        }}
      if (tmp$country[1] == "ML"){ # Mali 1995-1996
        
        name <- tmp$country[1]
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025",  "V024" ,    
                          "V004",                                      # placeholder, there is no data on if every heard of FGM
                          "S551",
                          "S553",
                          "S560"))
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        
        # Set missing variables to NA
        tmp$fgm_know <- NA  
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(as.character(tmp$year_birth))
      }
      if (tmp$country[1] == "NI"){ # Niger 1998
        
        name <- tmp$country[1]
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025",  "V024",                                    
                          "S551",                  # Heard of circumcision, but not probing question                                   
                          "S552",
                          "S554",
                          "S556"))                                     
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        tmp$year_birth <- paste(19,tmp$year_birth, sep="")
        tmp$year_birth <- as.numeric(as.character(tmp$year_birth))
        
      }   }   }
  
  # DHS wave 4: 1997-2003 - approximate date --------------------------------
  
  if (!is.null(tmp$wave)){ # code needs to jump over this section if data was already processed and wave no longer exists as column
    test <- c(tmp$wave == 4)[1] # select countries in the first wave
    
    if (test == TRUE) {
      
      if (tmp$country[1] == "CM"){ # Cameroon 2004
        
        # only women who were selected
        tmp <- tmp %>%
          filter(SELIG1 == "Yes")
        
        #table(tmp$S1001) # Knows of FGM
        #table(tmp$S1002) # Knows of FGM (probed)
        
        tmp$S1001 <- ifelse(tmp$S1001 == "Yes" | tmp$S1002 == "Yes", "Yes", "No")
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",
                          "V010",
                          "V001" ,"V002","SWEIGHT2","V021","V022","V023",  
                          "V025", "V024",                                  
                          "S1001",                                     
                          "S1003",
                          "S1007",
                          "S1023"))                                   
        
        # rename 
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
      }
      
      if (tmp$country[1] == "TD"){ # Chad 2004
        
        #table(tmp$S1001) # Knows of FGM
        #table(tmp$S1002) # Knows of FGM (probed)
        
        tmp$S1001 <- ifelse(tmp$S1001 == "Yes" | tmp$S1002 == "Yes", "Yes", "No")
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                   
                          "V007",   
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025", "V024",                                     
                          "S1001",                                     
                          "S1003",
                          "S1007", 
                          "S1023"))                                
        
        # rename 
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        
        tmp$fgm_age <- as.character(tmp$fgm_age)
        
        tmp$fgm_age <- ifelse(tmp$fgm_age == "100" | tmp$fgm_age == "101" | tmp$fgm_age == "199" | tmp$fgm_age == "200" |
                                tmp$fgm_age == "1 month", 0, 
                              ifelse(tmp$fgm_age == "201" | tmp$fgm_age == "One year", 1, tmp$fgm_age))
      }
      
      if (tmp$country[1] == "BJ"){ # Benin 2001
        
        name <- tmp$country[1]
        
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",  
                          "V025",  "V024",                                   
                          "FG100",  # no probing question asked                                  
                          "FG103",
                          "FG107",
                          "FG123"))                                   
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
        
      }
      
      if (tmp$country[1] == "ML"){ # Mali 2001
        
        name <- tmp$country[1]
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                    
                          "V007",  
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023", 
                          "V025", "V024",                                   
                          "FG100",   # no probing question                                 
                          "FG103", 
                          "FG107", 
                          "FG123"))                                   
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
      }
      
      
      if (tmp$country[1] == "BF" | tmp$country[1] == "SN" | tmp$country[1] == "TZ" | tmp$country[1] == "GN"){ 
        #Burkina Faso 2003/Senegal 2005/Tanzania 2004/Guinea 2005
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                   
                          "V007",    
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023", 
                          "V025", "V024",                                     
                          "FG100",  # no probing question                                 
                          "FG103",
                          "FG107",
                          "FG123"))                                   
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
      }
      
      if (tmp$country[1] == "NG"){ # Nigeria
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                   
                          "V007",  
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023", 
                          "V025",  "V024",                                     
                          "FG100",    # no probing question                                   
                          "FG103", 
                          "FG107",
                          "FG123"))                                   
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
      } 
      
      
      if(tmp$country[1] != "CM" & # I believe this part might be cut, all countries are expcetions
         tmp$country[1] != "TD" &
         tmp$country[1] != "BJ" &
         tmp$country[1] != "EG" &
         tmp$country[1] != "ET" &
         tmp$country[1] != "ML" &
         tmp$country[1] != "KE" &
         tmp$country[1] != "GH" &
         tmp$country[1] != "BF" &
         tmp$country[1] != "NG" &
         tmp$country[1] != "SN" &
         tmp$country[1] != "TZ" &
         tmp$country[1] != "GN"){
        #Select variables
        tmp <- tmp %>%
          dplyr::select(c("country", "V012","V013",                  
                          "V007", 
                          "V010",
                          "V001" ,"V002","V005","V021","V022","V023",
                          "V025", "V024",                                     
                          "FG101",                                    
                          "FG103", 
                          "FG107",
                          "FG123"))                                  
        
        # rename
        names(tmp) <- c("country", "age_years", "age_groups",
                        "year", 
                        "year_birth",
                        "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                        "residence", "region",
                        "fgm_know", 
                        "fgm",
                        "fgm_age",
                        "support")
      }
    }
  }
  
  
  
  # DHS wave 5: 2003-2008 & wave 6: 2008-2013 & wave 7: 2013-2018 -----------
  
  if (!is.null(tmp$wave)){ # code needs to jump over this section if data was already processed and wave no longer exists as column
    test <- c(tmp$wave == 5 | tmp$wave == 6 | tmp$wave == 7)[1] # select countries in the first wave
    
    if (test == TRUE) { 
      
      if(!is.na(tmp$V007[1])){
        if (tmp$country[1] == "EG" & tmp$V007[1] == 2008){
          
          tmp <- tmp %>%
            dplyr::select(c("country", "V012","V013",                  
                            "V007",                                
                            "V010",
                            "V001" ,"V002","V005","V021","V022","V023", 
                            "V025", "SGOVERN",                                     
                            "G100",    # no probing question                         
                            "G102",                                  
                            "G106", 
                            "G119"))                             
          
          # Remove North and South Sainai due to later lack of coverage of the region
          tmp <- tmp %>%
            filter(SGOVERN != "South Sainai" & SGOVERN != "North Sainai" )
          
          # rename
          names(tmp) <- c("country", "age_years", "age_groups",
                          "year", 
                          "year_birth",
                          "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                          "residence", "region",
                          "fgm_know", 
                          "fgm",
                          "fgm_age",
                          "support")
          
          
        }
        
      }
      
      
      if (tmp$country[1] == "CI"){ # Cote d'Ivoire 2011-12
        if (!is.null(tmp$V007)){
          if(tmp$V007[1] == 2011 | tmp$V007[1] == 2012){ 
            
            #table(tmp$S1001) # Knows of FGM
            #table(tmp$S1002) # Knows of FGM (probed)
            
            tmp$G100 <- ifelse(tmp$G100 == "Yes" | tmp$G101 == "Yes", "Yes", "No")
            
            tmp <- tmp %>%
              dplyr::select(c("country", "V012","V013",                   
                              "V007",
                              "V010",
                              "V001" ,"V002","V005","V021","V022","V023", 
                              "V025",   "V024",                                  
                              "G100",                                   
                              "G102",
                              "G106",
                              "G119"))                                  
            
            # rename
            names(tmp) <- c("country", "age_years", "age_groups",
                            "year", 
                            "year_birth",
                            "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                            "residence", "region",
                            "fgm_know", 
                            "fgm",
                            "fgm_age",
                            "support")
          }
        }
      }
      
      
      
      if(!is.null(tmp$V007)){
        if (tmp$country[1] != "CI" & tmp$country[1] != "EG"){ # all others, changed LB to EG (20 Feb)
          
          #table(tmp$S1001) # Knows of FGM
          #table(tmp$S1002) # Knows of FGM (probed)
          
          tmp$G100 <- ifelse(tmp$G100 == "Yes" | tmp$G101 == "Yes", "Yes", "No")
          
          tmp <- tmp %>%
            dplyr::select(c("country", "V012","V013",                  
                            "V007",                                  
                            "V010",
                            "V001" ,"V002","V005","V021","V022","V023", 
                            "V025",  "V024",
                            "G100",                                   
                            "G102",                                  
                            "G106",
                            "G119"))                                
          
          # rename
          names(tmp) <- c("country", "age_years", "age_groups",
                          "year", 
                          "year_birth",
                          "cluster_number", "hh_number", "wgt", "prim_samp_unit", "strata", "stratification",
                          "residence", "region",
                          "fgm_know", 
                          "fgm",
                          "fgm_age",
                          "support")
        }
      }
    }
  }
  
  # create unique cluster and strata variables
  
  tmp <- tmp %>%
    mutate(cluster_number_unique = paste(cluster_number,country,year, sep="")) %>%
    mutate(strata_unique = paste(strata,country,year, sep=""))# %>%
  #mutate(survey_id = i)
  
  
  # Recode country name 
  tmp$country <- ifelse(tmp$country == "BJ", "Benin",
                        ifelse(tmp$country == "BF", "Burkina Faso",
                               ifelse(tmp$country == "SD", "Sudan",
                                      ifelse(tmp$country == "CM", "Cameroon",
                                             ifelse(tmp$country == "CF", "Central African Republic", 
                                                    ifelse(tmp$country == "TD", "Chad",
                                                           ifelse(tmp$country == "CI", "Cote d'Ivoire", 
                                                                  ifelse(tmp$country == "EG", "Egypt", 
                                                                         ifelse(tmp$country == "ET", "Ethiopia",
                                                                                ifelse(tmp$country == "GM", "Gambia",
                                                                                       ifelse(tmp$country == "GH", "Ghana",
                                                                                              ifelse(tmp$country == "GN", "Guinea", 
                                                                                                     ifelse(tmp$country == "KE", "Kenya",
                                                                                                            ifelse(tmp$country == "ML", "Mali",
                                                                                                                   ifelse(tmp$country == "NI", "Niger",
                                                                                                                          ifelse(tmp$country == "NG", "Nigeria",
                                                                                                                                 ifelse(tmp$country == "SN", "Senegal",
                                                                                                                                        ifelse(tmp$country == "SL", "Sierra Leone",
                                                                                                                                               ifelse(tmp$country == "TZ", "United Republic of Tanzania",
                                                                                                                                                      ifelse(tmp$country == "TG", "Togo",
                                                                                                                                                             ifelse(tmp$country == "UG", "Uganda", 
                                                                                                                                                                    ifelse(tmp$country == "YE", "Yemen",
                                                                                                                                                                           ifelse(tmp$country == "LB", "Liberia", tmp$country)))))))))))))))))))))))
  
  # if year is wrongly coded sapply(tmp$year[1], nchar)
  if (sapply(as.character(tmp$year[1]), nchar) < 4){
    tmp$year <- ifelse(tmp$year == 99, 1999, 
                       ifelse(tmp$year == 98, 1998,
                              ifelse(tmp$year == 97, 1997,
                                     ifelse(tmp$year == 96, 1996,
                                            ifelse(tmp$year == 95, 1995,
                                                   ifelse(tmp$year == 94, 1994,
                                                          ifelse(tmp$year == 93, 1993,
                                                                 ifelse(tmp$year == 92, 1992,
                                                                        ifelse(tmp$year == 91, 1991,
                                                                               ifelse(tmp$year == 90, 1990,
                                                                                      ifelse(tmp$year == 89, 1989,
                                                                                             ifelse(tmp$year == 88, 1988, tmp$year))))))))))))
  }
  
  if(tmp$country[1] == "Kenya"){ # North eastern province has only be added after 1998 DHS in Kenya
    # for 2008_09 and 2014 we need to excluded if we want to assure 
    # comparability
    tmp <- tmp %>%
      filter(region != "Northeastern")%>%
      filter(region != "North Eastern")
  }
  
  # pooled analysis: if no further adjustments of weights, surveys with higher sample size have more weight
  # therefore: rescale/denormalize weights to population size
  
  # count unweighted surveyed 15-19 year olds
  tmp$n_survey <- nrow(tmp)
  
  # get total population of 15-44 year olds at mid-year of the survey
  tmp <- merge(population, tmp, by = c("country", "year"))
  
  # denormalize weight
  # https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-9-49
  # https://rpubs.com/corey_sparks/27276
  
  # filter women in right age group
  tmp <- tmp %>%
    filter(age_years < 50)
  
  tmp <- tmp %>%
    mutate(wgt = as.numeric(wgt)/1000000)
  
  tmp <- tmp %>%
    mutate(re_wgt = wgt*(as.numeric(pop)*1000)/n_survey)
  
  # recode fgm
  tmp$fgm_know <- ifelse(tmp$fgm_know == "Non" | tmp$fgm_know == "No", tmp$fgm_know == 0, 
                         ifelse(tmp$fgm_know == "Yes" | tmp$fgm_know == "Oui", 1, NA))
  
  table(tmp$fgm)
  
  tmp$fgm <- ifelse(tmp$fgm == "Oui" | tmp$fgm == "Yes" | tmp$fgm == "Sim" | tmp$fgm == 1, 1,
                    ifelse(tmp$fgm == "Non" | tmp$fgm == "No" | tmp$fgm == "Nao" | tmp$fgm == 0, 0, NA))
  
  
  tmp$fgm <- ifelse(tmp$fgm_know == 0, 0, tmp$fgm)
  
  tmp$wgt <- tmp$wgt/1000000
  
  # test to compare with survey - added February 8 2019
  #mics_design_d  <- svydesign(id         = ~cluster_number, # primary sampling units shoudl be same as cluster_number
  #                           strata         = ~strata, 
  #                          weight         = ~wgt,
  #                         data           = tmp, 
  #                        nest = T)
  #svymean(~fgm, mics_design_d, na.rm=T)
  #svytable(~fgm, mics_design_d)
  ###############################
  
  #NAs
  tmp$fgm_age <- as.character(tmp$fgm_age)
  
  if(tmp$country[1] == "Cote d'Ivoire" & (tmp$year[1] == 1999 | tmp$year[1] == 1998)){
    
    tmp$fgm_age <- ifelse(tmp$fgm_age == "Early neonatal", "0", tmp$fgm_age)
  }
  
  if(paste(tmp$country[1],tmp$year[1], sep="") != "Chad2004"){
    tmp$fgm_age <- ifelse(tmp$fgm_age == "During infancy" | tmp$fgm_age == "During Infancy",  "95", tmp$fgm_age)
    
    tmp$fgm_age <- ifelse(tmp$fgm_age == "During first week" |
                            tmp$fgm_age == "After first week and before the first year", 0, tmp$fgm_age)
    
    
    tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))
    
    
    total<-length(which(tmp$fgm_age >= 0 & tmp$fgm_age<=2))
    
    # %3
    at0 <- length(which(tmp$fgm_age==0))/total
    # %4
    at1 <- length(which(tmp$fgm_age==1))/total
    # %5
    at2 <- length(which(tmp$fgm_age==2))/total
    
    #Test
    sum(at0, at1, at2) # must be 1
    
    x0_2 <- sample(0:2, size=length(which(tmp$fgm_age==95)), replace=TRUE, prob=c(at0, at1, at2))
    
    tmp$fgm_age <- ifelse(tmp$fgm_age == 95, x0_2, tmp$fgm_age)
    
    tmp$fgm_age <- ifelse(tmp$fgm_age > 95 , NA, tmp$fgm_age)
  }
  
  
  # Chad 2004 might have to be exluded if the bellow assumption do not hold.
  if(tmp$country[1] == "Chad" & tmp$year[1] == 2004){
    tmp$fgm_age <- ifelse(tmp$fgm_age == "Before 5 years or so", "95", tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == "Around 5-9 years", "96", tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == "Around at 10 years or more", "97", tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == "1 month", 0, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == "One year", 1, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 102, 2, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 103, 3, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 107, 7, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 108, 8, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 109, 9, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 110, 10, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 111, 11, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 112, 12, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 202, 2, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 203, 3, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 204, 4, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 205, 5, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 206, 6, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 207, 7, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 208, 8, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 209, 9, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 210, 10, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 211, 11, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 212, 12, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 213, 13, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 214, 14, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 215, 15, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 216, 16, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 217, 17, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 218, 18, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 219, 19, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 220, 20, tmp$fgm_age)
    tmp$fgm_age <- ifelse(tmp$fgm_age == 223, 23, tmp$fgm_age)
    
    tmp$fgm_age <- as.numeric(as.character(tmp$fgm_age))
    
    total<-length(which(tmp$fgm_age >= 0 & tmp$fgm_age<=5))
    
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
    
    total<-length(which(tmp$fgm_age >= 6 & tmp$fgm_age<=9))
    
    # %0
    at6 <- length(which(tmp$fgm_age==6))/total
    # %1
    at7 <- length(which(tmp$fgm_age==7))/total
    # %2
    at8 <- length(which(tmp$fgm_age==8))/total
    # %3
    at9 <- length(which(tmp$fgm_age==9))/total
    
    #Test
    sum(at6, at7, at8, at9) # must be 1
    
    x6_9 <- sample(6:9, size=length(which(tmp$fgm_age==96)), replace=TRUE, prob=c(at6, at7, at8, at9))
    
    tmp$fgm_age <- ifelse(tmp$fgm_age == 96, x6_9, tmp$fgm_age)
    
    total<-length(which(tmp$fgm_age >= 10 & tmp$fgm_age < 21))
    
    # %0
    at10 <- length(which(tmp$fgm_age==10))/total
    # %1
    at11 <- length(which(tmp$fgm_age==11))/total
    # %2
    at12 <- length(which(tmp$fgm_age==12))/total
    # %3
    at13 <- length(which(tmp$fgm_age==13))/total
    # %0
    at14 <- length(which(tmp$fgm_age==14))/total
    # %1
    at15 <- length(which(tmp$fgm_age==15))/total
    # %2
    at16 <- length(which(tmp$fgm_age==16))/total
    # %3
    at17 <- length(which(tmp$fgm_age==17))/total
    # %0
    at18 <- length(which(tmp$fgm_age==18))/total
    # %1
    at19 <- length(which(tmp$fgm_age==19))/total
    # %2
    at20 <- length(which(tmp$fgm_age==20))/total
    
    #Test
    sum(at10, at11, at12, at13, at14, at15, at16, at17, at18, at19, at20) # must be 1
    
    x10_up <- sample(10:20, size=length(which(tmp$fgm_age==97)), replace=TRUE, prob=c(at10, at11, at12, at13, at14, at15, at16, at17, at18, at19, at20))
    
    tmp$fgm_age <- ifelse(tmp$fgm_age == 97, x10_up, tmp$fgm_age)
    
    tmp$fgm_age <- ifelse(tmp$fgm_age > 90 , NA, tmp$fgm_age)
    
  }
  
  # add time to event column
  
  tmp$time <- CalculateTimetoEvent(tmp$fgm, tmp$fgm_age, tmp$age_years)
  
  
  # add cohort index
  tmp$cohort10 <- ifelse(1960 <=   tmp$year_birth &   tmp$year_birth < 1970, 1, 
                         ifelse(1970 <=   tmp$year_birth &   tmp$year_birth < 1980, 2,
                                ifelse(1980 <=   tmp$year_birth &   tmp$year_birth < 1990, 3,
                                       ifelse(1990 <=   tmp$year_birth &   tmp$year_birth < 2000, 4,
                                              ifelse(2000 <=   tmp$year_birth &   tmp$year_birth < 2010, 5,
                                                     ifelse(2010 <=   tmp$year_birth &   tmp$year_birth < 2020, 6, NA))))))
  
  tmp$cohort5 <- ifelse(1955 <= tmp$year_birth & tmp$year_birth < 1960, 1,
                        ifelse(1960 <=   tmp$year_birth &   tmp$year_birth < 1965, 2, 
                               ifelse(1965 <=   tmp$year_birth &   tmp$year_birth < 1970, 3,
                                      ifelse(1970 <=   tmp$year_birth &   tmp$year_birth < 1975, 4,
                                             ifelse(1975 <=   tmp$year_birth &   tmp$year_birth < 1980, 5,
                                                    ifelse(1980 <=   tmp$year_birth &   tmp$year_birth < 1985, 6,
                                                           ifelse(1985 <=   tmp$year_birth &   tmp$year_birth < 1990, 7,
                                                                  ifelse(1990 <=   tmp$year_birth &   tmp$year_birth < 1995, 8,
                                                                         ifelse(1995 <=   tmp$year_birth &   tmp$year_birth < 2000, 9,
                                                                                ifelse(2000 <=   tmp$year_birth &   tmp$year_birth < 2005, 10,
                                                                                       ifelse(2005 <=   tmp$year_birth &   tmp$year_birth < 2010, 11,
                                                                                              ifelse(2010 <=   tmp$year_birth &   tmp$year_birth < 2020, 12, NA))))))))))))  
  
  # append data 
  df_DHS <- rbind(df_DHS, tmp)
} 

df_DHS$survey <- "DHS" # add column of data source for later merge with MICS data

# save dataset
saveRDS(df_DHS, file = "survival_data_DHS_February2019.rds")

setwd("C:/Users/weny/Google Drive/2018/FGM/02 -Trend modelling/FGM-trend-analysis/Codes")

