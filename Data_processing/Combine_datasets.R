# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------
# written by Kathrin Weny

# File description, purpose of code, inputs and output --------------------

DHS   <- readRDS("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets/survival_data_DHS_July2019.rds", refhook = NULL)
DHSd  <- readRDS("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets/survival_data_DHS_daughters_July2019.rds", refhook = NULL)

MICS  <- readRDS("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets/survival_data_MICS_July2019.rds", refhook = NULL)
MICSd <- readRDS("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets/survival_data_MICS_daughters_July2019.rds", refhook = NULL)

# MICS datasets -----------------------------------------------------------

MICS <- dplyr::bind_rows(dplyr::mutate_all(MICSd, as.character), 
                         dplyr::mutate_all(MICS, as.character)) 

# DHS datasets ------------------------------------------------------------

DHS <- dplyr::bind_rows(dplyr::mutate_all(DHSd, as.character), 
                        dplyr::mutate_all(DHS, as.character)) 

# Full dataset ------------------------------------------------------------

data <- dplyr::bind_rows(dplyr::mutate_all(MICS, as.character), 
                         dplyr::mutate_all(DHS, as.character)) 

data$time <- ifelse(data$time == 99 | data$time == 98, NA, data$time)

# Plot NAs for age/time ---------------------------------------------------

data1 <- data %>%
  filter(!is.na(fgm)) %>%
  group_by(as.numeric(age_years)) %>%
  dplyr::mutate(new = n())

data_na_fgmage <- data1 %>%
  filter(fgm == 1)%>%
  filter(is.na(fgm_age)) 

data_na_fgmage$age_years<- as.factor(data_na_fgmage$age_years)
data_na_fgmage$age_years = factor(data_na_fgmage$age_years,levels(data_na_fgmage$age_years)[c(1:2,13,24,35,46:50,3:12,14:23,25:34,36:45)]) 

data_na_fgmage <- data_na_fgmage %>%
  dplyr::mutate(new1 = n())

data_na_fgmage <- data_na_fgmage %>%
  mutate(percentage = new1/new )

data_na_fgmage$age_years <- as.numeric(as.character(data_na_fgmage$age_years))
data_na_fgmage <- data_na_fgmage[order(data_na_fgmage$age_years),] 

ggplot(data_na_fgmage, aes(x=age_years, y = percentage, size = new1)) +
  geom_point(color= "red3") + theme_bw(base_size = 32) +
  scale_y_continuous(breaks = seq(0,0.04,0.02), 
                     labels = c("0 %", "2%", "4%"))+
  scale_x_continuous(breaks = seq(0, length(unique(data_na_fgmage$age_years)), by = 5))+
  theme(legend.position = c(0.8, 0.25))+
  
  labs(title= "Proportion of survey responses for which \n age at FGM is not reported", x = "Current age of woman/girl",
       y = "Missing age-at-FGM, %", size = "# of observations")+
  theme(title = element_text(face = "bold"), 
        plot.background = element_rect(fill = "gainsboro"))

# Ungroup data ------------------------------------------------------------

data <- ungroup(data)

# Unify residence coding --------------------------------------------------

data$residence <- ifelse(data$residence == 1, "Urban", 
                         ifelse(data$residence == 2, "Rural", 
                                ifelse(data$residence == "Urbain", "Urban", 
                                       ifelse(data$residence == "Urbano", "Urban",
                                              ifelse(data$residence == "Urban", "Urban", 
                                                     ifelse(data$residence == "Rural", "Rural",NA))))))

# Select bar minimum of necessary columns ---------------------------------

data <- data %>%
  dplyr::select(c("country", "year_birth", "age_years",
                  "cohort10", "cohort5", "residence",
                  "re_wgt", "wgt",
                  "fgm", 
                  "time",
                  "cluster_number_unique",
                  "strata_unique",
                  "survey", "year"))

# safe dataset --------------------------------------------------

df <- data 

setwd("G:/My Drive/2019/1- FGM/02- Trend estimates/FGMcohortanalysis/Datasets")

saveRDS(df, file = "survival_data_February2019.rds")


