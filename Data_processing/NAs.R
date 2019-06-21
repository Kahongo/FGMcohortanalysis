# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny
# created 20 June 2019

# File description, purpose of code, inputs and output --------------------

# This code scopes the datasets and saves the percentage of women 15-49 years who report 'during infancy'

# In DHS ------------------------------------------------------------------

setwd("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/DHS")

listsav     <- dir(pattern = "*.SAV")

listsav <- listsav[-c(4, 9, 11, 15, 18, 21, 24, 27, 41)] # Remove Burkina Faso 1998/99 
                                                     # Chad 2004 (different categories - see below), 
                                                     # Cote dIvoire 1994 (no 'during infancy')
                                                     # Egypt 2015 (no 'during infancy')
                                                     # Guinea 1999 (no 'during infancy')
                                                     # Kenya 1998 (no 'during infancy')
                                                     # Mali 1995/96 (no 'during infancy')
                                                     # Niger 1998 (no 'during infancy')
                                                     # Tanzania 1996 (no 'during infancy')
                                                     

results.dhs     <- data.frame(matrix(NA, nrow = length(listsav), ncol = 2))
results.dhs[,1] <- listsav 

for(i in 1:length(listsav)){

  tmp <- ReadSingleSAV(i) 
  a <- as.data.frame(attr(tmp, "variable.labels"))

if(i == 1 | i == 4 | i == 14 | i == 18 | i == 22 | i == 25 | i == 33){ # 1 = Benin.2001/ 2= Burkina Faso.2003 \ Guinea 2005 / Mali 2001
  tmp <- tmp %>%
    filter(FG103 == "Yes" | FG103 == "Oui") %>%
    filter(!is.na(FG103))

  b <- as.data.frame(count(tmp$FG107 == "During infancy" | tmp$FG107 == "During Infancy" ))[2,2]/
    sum(as.data.frame(count(tmp$FG107 == "During infancy" | tmp$FG107 == "During Infancy" ))[2,2],
        as.data.frame(count(tmp$FG107 == "During infancy" | tmp$FG107 == "During Infancy" ))[1,2])*100
}
  
  if(i == 6 ){ # Cameroon.2004
    tmp <- tmp %>%
      filter(S1003 == "Yes" | S1003 == "Oui") %>%
      filter(!is.na(S1003))
    
    b <- as.data.frame(count(tmp$S1007 == "During infancy" | tmp$S1007 == "During Infancy" ))[2,2]/
      sum(as.data.frame(count(tmp$S1007 == "During infancy" | tmp$S1007 == "During Infancy" ))[2,2],
          as.data.frame(count(tmp$S1007 == "During infancy" | tmp$S1007 == "During Infancy" ))[1,2])*100
  }
  
  if(i == 7){ # CAR.1994/95
    tmp <- tmp %>%
      filter(S1001 == "Yes" | S1001 == "Oui") %>%
      filter(!is.na(S1001))
    
    b <- as.data.frame(count(tmp$S1002 == "During infancy" | tmp$S1002 == "During Infancy" ))[2,2]/
      sum(as.data.frame(count(tmp$S1002 == "During infancy" | tmp$S1002 == "During Infancy" ))[2,2],
          as.data.frame(count(tmp$S1002 == "During infancy" | tmp$S1002 == "During Infancy" ))[1,2])*100
  }

  if(i == 9){ # Cote d'Ivoire 1998/99
    tmp <- tmp %>%
      filter(S902 == "Yes" | S902 == "Oui") %>%
      filter(!is.na(S902)) %>%
      filter(!is.na(S904))
    
    b <- as.data.frame(count(tmp$S904 == "Early neonatal"))[2,2]/
      sum(as.data.frame(count(tmp$S904 == "Early neonatal"))[2,2],
          as.data.frame(count(tmp$S904 == "Early neonatal"))[1,2])*100
  }
  
  

  if (i != 1 & i != 4 & i != 6 & i != 7 & i != 9 & i != 14 & i != 16 & i != 18 & i != 22 & i != 25 & i != 33){

tmp <- tmp %>%
  filter(G102 == "Yes" | G102 == "Oui") %>%
  filter(!is.na(G102))

b <- as.data.frame(count(tmp$G106 == "During infancy"| tmp$G106 == "During Infancy"))[2,2]/
  sum(as.data.frame(count(tmp$G106 == "During infancy"| tmp$G106 == "During Infancy" ))[2,2],
      as.data.frame(count(tmp$G106 == "During infancy"| tmp$G106 == "During Infancy" ))[1,2])*100
  }
  
  results.dhs[i,2] <- b
  
}

# Chad 2004: different categories

listsav     <- dir(pattern = "*.SAV")

tmp <- ReadSingleSAV(8) 

tmp <- tmp %>%
  filter(S1003 == "Yes") # nrow 3071

table(tmp$S1007) # Dont know (135) | Around at 10 years or more (506) | Around 5-9 years (698) | Before 5 years or so (82)

sum(506, 698, 82)/(3071-135)

# In MICS -----------------------------------------------------------------

setwd("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/MICS")

listsav <- dir(pattern = "*.sav")

results.mics     <- data.frame(matrix(NA, nrow = length(listsav), ncol = 2))
results.mics[,1] <- listsav

# Mali 2009/10
tmp <- ReadSingleSAV(11) 
a <- as.data.frame(attr(tmp, "variable.labels"))

tmp <- tmp %>%
  filter(FG3 == "Yes" | FG3 == "Oui") %>%
  filter(!is.na(FG3))

table(tmp$FG7)
b <- as.data.frame(count(tmp$FG7 == "Pendant la petite enfance"))[2,2]/
  sum(as.data.frame(count(tmp$FG7 == "Pendant la petite enfance" ))[2,2],
      as.data.frame(count(tmp$FG7 == "Pendant la petite enfance" ))[1,2])*100


# Chad 2000

tmp <- ReadSingleSAV(3) 
a <- as.data.frame(attr(tmp, "variable.labels"))

tmp <- tmp %>%
  filter(CU5 == "Yes" | CU5 == "Oui") %>%
  filter(!is.na(CU5))

table(tmp$CU6)



