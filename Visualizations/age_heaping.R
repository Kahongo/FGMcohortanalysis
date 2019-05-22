# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny

# File description, purpose of code, inputs and output --------------------

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(foreign)  
library(lodown)  
library(rdhs) 
library(DemoTools)
library(DDM)      
library(devtools)
library(Pyramid)

survival_data <- readRDS("C:/Users/weny/Google Drive/2018/FGM/02 -Trend modelling/01- Data/survival_data_February2019.rds", refhook = NULL)

survival_data$re_wgt <- as.numeric(survival_data$re_wgt) # weights need to be numeric
survival_data$time <- as.numeric(survival_data$time) # weights need to be numeric
survival_data$fgm <- as.numeric(survival_data$fgm) # weights need to be numeric

# Kaplan-Meier estimates --------------------------------------------------

survival_data$cohort5 <- as.factor(survival_data$cohort5)
survival_data$cohort5 = factor(survival_data$cohort5,levels(survival_data$cohort5)[c(1, 4:11, 2:3)]) 
survival_data$cohort10 <- as.factor(survival_data$cohort10)

survival_data <- survival_data %>%
  filter(country !="Somaliland") %>%
  filter(country !="Somalia North-Eastern Zone")

survival_data$time <- ifelse(survival_data$time == 99 | survival_data$time == 98, NA, survival_data$time)

# Single year age groups --------------------------------------------------
plot_data <- survival_data %>%
  filter(!is.na(time))
plot_data$time <- as.factor(plot_data$time)

plot_data_total <- plot_data %>%
  group_by(time)%>%
  dplyr::mutate(new=n()) %>%
  select(c(time, new))
plot_data_total <- unique(plot_data_total)

plot_data <- plot_data %>%
  group_by(time, fgm)%>%
  dplyr::mutate(new = n()) %>%
  select(c(time, new, fgm))
plot_data <- unique(plot_data)

plot_data$fgm <- as.factor(plot_data$fgm)
levels(plot_data$fgm) <- c("No", "Yes")

plot_data <-  plot_data [,c(3,1,2)]  

plot_data_fgm <- plot_data %>%
  filter(fgm == "Yes")

plot_data_nofgm <- plot_data %>%
  filter(fgm == "No")

# Visualization -----------------------------------------------------------

plot_data$time <- as.numeric(as.character(plot_data$time))

ggplot(plot_data , aes(time, new, fill = fgm)) +     
  geom_col() +
  scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000)+
  scale_x_continuous(breaks = seq(0,49, by =5))+
  
  theme_bw(base_size = 32) +
  
  scale_fill_manual(values = c("peachpuff3", "red3"))+
  
  labs(title= "Age heaping in FGM dataset", x = "Age at FGM or current Age",
       y = "Nr of women and girls", fill = "Underwent FGM")+
  theme(title = element_text(face = "bold"), 
        plot.background = element_rect(fill = "gainsboro"),
        legend.position = c(0.8, 0.75))


ggplot(plot_data_fgm , aes(time, new)) +     
  geom_col() +
  scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000)+
  theme_bw() +
  labs(title= "Age heaping in FGM dataset", x = "Time to FGM",
       y = "Nr of women and girls")

ggplot(plot_data_nofgm , aes(time, new)) +     
  geom_col() +
  scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000)+
  theme_bw() +
  labs(title= "Age heaping in FGM dataset", x = "Age at censoring",
       y = "Nr of women and girls")

# Single year age groups by cohorts --------------------------------------------------
plot_data <- survival_data %>%
  filter(!is.na(time))
plot_data$time <- as.numeric(plot_data$time)

plot_data_total <- plot_data %>%
  group_by(time)%>%
  dplyr::mutate(new=n()) %>%
  select(c(time, new, cohort10))
plot_data_total <- unique(plot_data_total)

plot_data <- plot_data %>%
  group_by(time, fgm)%>%
  dplyr::mutate(new = n()) %>%
  select(c(time, new, fgm, cohort10))
plot_data <- unique(plot_data)

plot_data$fgm <- as.factor(plot_data$fgm)
levels(plot_data$fgm) <- c("No", "Yes")


plot_data_fgm <- plot_data %>%
  filter(fgm == "Yes")

plot_data_nofgm <- plot_data %>%
  filter(fgm == "No")


# Visualization -----------------------------------------------------------

plot_data_fgm$cohort10 <- as.factor(plot_data_fgm$cohort10)

plot_data <- plot_data %>%
  filter(!is.na(cohort10))

plot_data$cohort10 <- ordered(plot_data$cohort10,
                              levels = c(1:6),
                              labels = c("1960 -1969",
                                         "1970 -1979",
                                         "1980 -1989",
                                         "1990 -1999", 
                                         "2000 -2009", 
                                         "after 2009")) 

ggplot(plot_data , aes(time, new, fill = fgm)) +     
  geom_col() +
  facet_wrap(~cohort10) +
  scale_y_continuous(name = "Count (thousands)", labels = function(y) y / 1000)+
  
  theme_bw() +
  
  scale_x_continuous(breaks=seq(0, 49, 5))+
  
  scale_fill_manual(values = c("peachpuff3", "red3"))+
  
  labs(title= "Age heaping in FGM dataset", x = "Age at FGM or current Age",
       y = "Nr of women and girls", fill = "Underwent FGM")

# Age ratio ---------------------------------------------------------------

# FGM

#plot_data_fgm <- plot_data_fgm %>%
# select(-cohort10)

#plot_data_fgm <- dplyr::distinct(plot_data_fgm)

plot_data_fgm <- plot_data_fgm[order(plot_data_fgm$time),] 
unique(plot_data_fgm$time)
fgm <- as.numeric(plot_data_fgm$new)

num.fgm <- 3 * fgm[2:(length(fgm)-1)] # nominator

denom.fgm <- as.numeric(fgm[1:(length(fgm)-2)]) + # denominator
  fgm[2:(length(fgm)-1)] + 
  fgm[3:(length(fgm))]

ageRatio.fgm <- data.frame(matrix(vector(), 43, 2,
                                  dimnames=list(c(), c("age.ratio", "time"))),
                           stringsAsFactors=F)

ageRatio.fgm$age.ratio <- num.fgm/denom.fgm

ageRatio.fgm$time <- c(as.character(1:40), as.character(42:44))

ageRatio.fgm[sapply(ageRatio.fgm, is.character)] <- lapply(ageRatio.fgm[sapply(ageRatio.fgm, is.character)], 
                                                           as.factor)
# No FGM
#plot_data_nofgm <- plot_data_nofgm %>%
# select(-cohort10)

#plot_data_nofgm <- dplyr::distinct(plot_data_nofgm)

plot_data_nofgm <- plot_data_nofgm[order(plot_data_nofgm$time),] 

nofgm <- as.numeric(plot_data_nofgm$new)

num.nofgm <- 3 * nofgm[2:(length(nofgm)-1)] # nominator

denom.nofgm <- as.numeric(nofgm[1:(length(nofgm)-2)]) + # denominator
  nofgm[2:(length(nofgm)-1)] + 
  nofgm[3:(length(nofgm))]

ageRatio.nofgm <- data.frame(matrix(vector(), 48, 2,
                                    dimnames=list(c(), c("age.ratio", "time"))),
                             stringsAsFactors=F)

ageRatio.nofgm$age.ratio <- num.nofgm/denom.nofgm

ageRatio.nofgm$time <- c(as.character(1:48))

ageRatio.nofgm[sapply(ageRatio.nofgm, is.character)] <- lapply(ageRatio.nofgm[sapply(ageRatio.nofgm, is.character)], 
                                                               as.factor)


# Total
#plot_data_total <- plot_data_total %>%
#  select(-cohort10)

#plot_data_total <- dplyr::distinct(plot_data_total)

plot_data_total <- plot_data_total[order(plot_data_total$time),] 

Total <- as.numeric(plot_data_total$new)

num.Total <- 3 * Total[2:(length(Total)-1)] # nominator

denom.Total <- as.numeric(Total[1:(length(Total)-2)]) + # denominator
  Total[2:(length(Total)-1)] + 
  Total[3:(length(Total))]

ageRatio.Total <- data.frame(matrix(vector(), 48, 2,
                                    dimnames=list(c(), c("age.ratio", "time"))),
                             stringsAsFactors=F)

ageRatio.Total$age.ratio <- num.Total/denom.Total

ageRatio.Total$time <- c(as.character(1:48))

ageRatio.Total[sapply(ageRatio.Total, is.character)] <- lapply(ageRatio.Total[sapply(ageRatio.Total, is.character)], 
                                                               as.factor)

plot_data <- merge(ageRatio.Total, ageRatio.nofgm, by= "time", all=TRUE) 

plot_data <- merge(plot_data, ageRatio.fgm, by= "time", all=TRUE) 

names(plot_data) <- c("time", "age.ratio.total", "age.ratio.no.fgm", "age.ratio.fgm")

# Visualization -----------------------------------------------------------
plot_data$time = factor(plot_data$time, levels(plot_data$time)[c(1, 12, 23, 34, 45:49, 2:11, 13:22, 24:33, 35:44)]) 

plot_data <- plot_data %>%
  filter(as.numeric(time) < 16 )

ggplot(data = plot_data) +
  geom_line(aes(y = age.ratio.total, x = time, group = 1, color = "Total"), size = .8) +
  geom_line(aes(y = age.ratio.no.fgm, x = time, group = 1, color = "No FGM"), size = .8) +
  geom_line(aes(y = age.ratio.fgm, x = time, group = 1, color = "FGM"), size = .8) +
  geom_hline(yintercept=1) +
  geom_hline(yintercept=1.1, linetype="dashed") +
  geom_hline(yintercept=0.9, linetype="dashed") +
  labs(x = "Years", y = "Age ratio", color = "By FGM status")+
  ggtitle("Age Ratio - Different Data Sets") +
  ylim(0, 2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="aliceblue"), axis.line = element_line(colour="gray59"), 
        strip.text = element_text(size=15),
        strip.background =element_rect(fill="white"),
        panel.spacing.x = unit(3,"lines")) 


