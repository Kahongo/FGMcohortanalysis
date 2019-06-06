
# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny

memory.limit(size=56000)

# Set up data set ------------------------------------------

survival_data <- readRDS("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/survival_data_February2019.rds", refhook = NULL)

survey1 <- survival_data %>%
  filter(!is.na(fgm)) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(cohort5))%>%
  filter(!is.na(cohort10)) %>%
  filter(!is.na(year_birth))

survey1$cohort10 <- as.factor(survey1$cohort10)
survey1$cohort5 <- as.factor(survey1$cohort5)
survey1$cohort5 = factor(survey1$cohort5,levels(survey1$cohort5)[c(4:11,1:3)]) # no more cohort 1 which is 1955-59 and has no equivalent for cohort 10
survey1$cohort5 = relevel(survey1$cohort5, ref="2")
survey1$cohort10 = relevel(survey1$cohort10, ref="1")

# convert in correct format
survey1$re_wgt <- as.numeric(survey1$re_wgt) 
survey1$time <- as.numeric(survey1$time)
survey1$fgm <- as.numeric(survey1$fgm) 
survey1$year_birth <- as.numeric(survey1$year_birth)

survey1 <- survey1 %>%
  filter( re_wgt != 0)#one weight in Mauritania = 0

survey1$country <- as.factor(survey1$country)

countries <- unique(survey1$country)

# Bring in complex survey format ------------------------------------------

survey <-      svydesign(id             = ~cluster_number_unique, 
                         strata         = ~strata_unique, 
                         weight         = ~re_wgt,
                         data           = survey1, 
                         nest           = T)
# Logistic regression -----------------------------------------------------

# Model A: Logistic model with interaction on year and country 
gc()
logistic <-  svyglm(fgm~ year_birth*country, family=quasibinomial, design=survey)
save(logistic, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/logistic.model.results.rda")

# Coxmodel corresponding to logistic model --------------------------------
gc()
cox <- svycoxph(Surv(time, fgm>0)~ year_birth*country, survey)
save(cox, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/coxmodel.rda")

# Test proportionality assumption -----------------------------------------
gc()
zph <- cox.zph(cox)
save(zph, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/zph.rda")

# Time split --------------------------------------------------------------
# Source: I : https://www.r-bloggers.com/dealing-with-non-proportional-hazards-in-r/
# Source: II: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6015946/
gc()

survey.split <- survey1
survey.split$time <- as.numeric(survey.split$time)
survey.split <- timeSplitter(survey.split, by = 5,
                             event_var = "fgm",
                             event_start_status = 0,
                             time_var = "time")

cox.split <- coxph(Surv(Start_time, Stop_time, fgm==1)~ year_birth*country + #svycoxph not available with splitter
                     country:Start_time, survey.split)
save(cox.split, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.split.rda")

# Strata ------------------------------------------------------------------
# same as fitting different baseline hazard functions
# http://myweb.uiowa.edu/pbreheny/7210/f15/notes/11-17.pdf
# http://rstudio-pubs-static.s3.amazonaws.com/5096_0880aaaf0df94f3b8533a1c024738246.html
gc()

# cox.strata <- svycoxph(Surv(time, fgm==1)~ year_birth*country -country + strata(country), survey)
# save(cox.strata, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.rda")
# ==> not able to create a curve for models that contain an interaction without the lower order effect

# cox.strata.II <- svycoxph(Surv(time, fgm==1)~ year_birth*strata(country), survey)
# save(cox.strata.II, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.II.rda")

#covs <- data.frame(country= "Guinea", year_birth = 1960:2017)
#summary(survfit(cox.strata.II, newdata = covs, type = "aalen")) # Source Tutorial: Survival Estimation for Cox Regression
# Models with Time-Varying Coefficients Using SAS and R

# cox.strata.III <- coxph(Surv(time, fgm==1)~ year_birth*strata(country), survey1, robust=TRUE, weights = as.numeric(re_wgt))
# save(cox.strata.III, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.III.rda")

# cox.strata.IV <- svycoxph(Surv(time, fgm==1)~ year_birth:strata(country), survey)
# save(cox.strata.IV, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.IV.rda")

#cox.strata.V <- svycoxph(Surv(time, fgm==1)~ year_birth + strata(country), survey)
#save(cox.strata.V, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.V.rda")

cox.strata.VI <- svycoxph(Surv(time, fgm==1)~ year_birth:country + year_birth + strata(country), survey)
save(cox.strata.VI, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.VI.rda")

zph.cox.strata.VI  <- cox.zph(cox.strata.VI)
save(zph.cox.strata.VI, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/zph.cox.strata.VI.rda")

# Test if cox.strata.VI is the same with interaction model
survey3 <- survey

survey3$year.country.inter = interaction(survey3$country, survey3$year_birth, drop = TRUE)

survey <-      svydesign(id             = ~cluster_number_unique, 
                         strata         = ~strata_unique, 
                         weight         = ~re_wgt,
                         data           = survey3, 
                         nest           = T)

cox.strata.VI.test <- svycoxph(Surv(time, fgm==1)~ year.country.inter + year_birth + strata(country), survey)

save(cox.strata.VI.test, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.VI.test.rda")


# Run Cox model for 1994-now ----------------------------------------------

survey2 <- survey1 %>%
  filter(year_birth > 1989)

table(is.na(survey2$time))

# Creating the interaction with the interaction function, the interaction function computes the factor identical to what 
# is obtained by specificting an interaction term in a model (see: The R Primer, SEcond edition, By Claus Thorn Ekstrom)
survey2$year.country.inter = interaction(survey2$country, survey2$year_birth, drop = TRUE)

survey <-      svydesign(id             = ~cluster_number_unique, 
                         strata         = ~strata_unique, 
                         weight         = ~re_wgt,
                         data           = survey2, 
                         nest           = T)

cox.strata.VI.a <- svycoxph(Surv(time, fgm==1)~ year_birth*strata(country), survey)

save(cox.strata.VI.a, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.VI.a.rda")

#cox.strata.VI.b <- svycoxph(Surv(time, fgm==1)~ year_birth:country + year_birth + strata(country), survey)
#save(cox.strata.VI.b, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.VI.b.rda")
# ==> failed to run. 

# Model VII ---------------------------------------------------------------

#cox.strata.VII <- svycoxph(Surv(time, fgm==1)~ year_birth:country:time + time + year_birth + strata(country), survey)
#save(cox.strata.VII, file = "test.cox.time.rda") #, file = "G:/My Drive/2019/1- FGM/02- Trend estimates/Results/results.cox.strata.VII.rda")

#zph.cox.strata.VII <- cox.zph(cox.strata.VII)

# model per country

for(i in countries){
  
  temp.model <- svycoxph(Surv(time, fgm==1)~ year_birth, design = subset(survey, country == i))
  
  filename <- paste("G:/My Drive/2019/1- FGM/02- Trend estimates/Results/", i, ".results.rda", sep ="")
  
  save(temp.model, file = filename)
  
  
}

load("G:/My Drive/2019/1- FGM/02- Trend estimates/Results/Nigeria.timeinter.results.rda", verbose=T)

ggcoxzph(cox.zph(temp.model))

# model per country - time interaction

for(i in countries){
  
  temp.model <- svycoxph(Surv(time, fgm==1)~ year_birth*time, design = subset(survey, country == i))
  
  filename <- paste("G:/My Drive/2019/1- FGM/02- Trend estimates/Results/", i, ".timeinter.results.rda", sep ="")
  
  save(temp.model, file = filename)
  
}

# Assess model fit

extractAIC(cox)
extractAIC(cox.strata.V)
extractAIC(cox.strata.VI)

AIC(cox)
AIC(cox.strata.V)
AIC(cox.strata.VI)

anova(cox, cox.strata.V)

# Hierarchical model  --------------------------------

# I: Global

rancox.fit5 <-  coxme(Surv(time,fgm) ~ year_birth + (1|country) + (year_birth|country),
                      ties="efron",
                      data = survey1,
                      weights = re_wgt)

save(rancox.fit5, file="coxme.results.rda")

rancox.fit5$frail

AIC(rancox.fit5)
AIC(cox.strata.VI)


model.frailty.gaussian <- coxph(Surv(time,fgm) ~ year_birth + country*year_birth + 
                                  frailty(country, distribution="gaussian"), data=survey1)

plot(survfit(model.frailty.gaussian, newdata = data.frame(country = "Yemen", year_birth= 1960:2017)))


save(model.frailty.gaussian, file="coxph.frailty.gaussina.rda")


model.frailty.gamma <- coxph(Surv(time,fgm) ~ year_birth + country*year_birth + 
                               frailty(country, distribution="gamma"), data=survey1)

save(model.frailty.gamma, file="coxph.frailty.gamma.rda")



summary(model.frailty.gamma )

temp.world        <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("country", "data", "lower", "upper"))
temp.world[1,1]   <- "Global"
temp.world[1,2]   <- as.data.frame(summary(rancox.fit5)$coef)
temp.world[1,3:4] <- confint(rancox.fit5)

# II: Global

rancox.fit5 <-  coxme(Surv(time,fgm) ~ year_birth + (1|country/cluster_number_unique) + (1 + year_birth|country)+
                        strata(strata_unique),
                      ties="efron",
                      data = survey1,
                      weights = re_wgt)

summary(rancox.fit5)

# National level

coxme.results <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("country", "data", "lower", "upper"))

survey2 <- survey2 %>%
  filter(country != "Cameroon") # needs to be removed as no non missing cases

countries <- unique(survey1$country)


for(i in countries){
  
  data_for_model <- survey1 %>%
    filter(country == i) %>%
    filter(re_wgt != 0) #one weight in Mauritania = 0
  
  rancox.fit5.temp <-  coxme(Surv(time,fgm) ~ year_birth + (1|cluster_number_unique) + strata(strata_unique) ,
                             ties="efron",
                             data = data_for_model,
                             weights = re_wgt)
  
  temp        <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("country", "data", "lower", "upper"))
  temp[1,1]   <- i
  temp[1,2]   <- as.data.frame(summary(rancox.fit5.temp)$coef)
  temp[1,3:4] <- confint(rancox.fit5.temp)
  
  coxme.results <- rbind(coxme.results, temp)
  
}

coxme.results <- rbind(coxme.results, temp.world)

# the coefficients from a coxmodel have to be exponentialized in order to retrieve the relative odds
coxme.results <- coxme.results %>%
  mutate(point.estimate = exp(data)) %>%
  mutate(lower.estimate = exp(lower)) %>%
  mutate(upper.estimate = exp(upper)) %>%
  select(c("country", "point.estimate", "lower.estimate", "upper.estimate"))

write.csv(coxme.results, file="G:/My Drive/2019/1- FGM/02- Trend estimates/Results/coxme.results.csv")














