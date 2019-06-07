
# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny

# File description, purpose of code, inputs and output --------------------

setwd("G:/My Drive/2019/1- FGM/02- Trend estimates/PaperI/Visualizations")

# Load data ---------------------------------------------------------------

survival_data <- readRDS("G:/My Drive/2018/FGM/02 -Trend modelling/01- Data/survival_data_February2019.rds", refhook = NULL)

survival_data$re_wgt <- as.numeric(survival_data$re_wgt) # weights need to be numeric
survival_data$time <- as.numeric(survival_data$time) # weights need to be numeric
survival_data$fgm <- as.numeric(survival_data$fgm) # weights need to be numeric

# Set up data set --------------------------------------------------

survival_data$cohort5 <- as.factor(survival_data$cohort5)
survival_data$cohort5 = factor(survival_data$cohort5,levels(survival_data$cohort5)[c(5:12, 2:4)]) 
survival_data$cohort5 <- relevel(survival_data$cohort5, ref = 2)
survival_data$cohort5 <- ordered(survival_data$cohort5,
                                 levels = c(2:12),
                                 labels = c("1960 -1964",
                                            "1965 -1969",
                                            "1970 -1974", 
                                            "1975 -1979",
                                            "1980 -1984",
                                            "1985 -1989",
                                            "1990 -1994", 
                                            "1995 -1999",
                                            "2000 -2004", 
                                            "2005 -2009",
                                            "after 2009"))

survival_data$cohort10 <- as.factor(survival_data$cohort10)
survival_data$cohort10 <- ordered(survival_data$cohort10,
                                  levels = c(1:6),
                                  labels = c("1960 -1969",
                                             "1970 -1979",
                                             "1980 -1989",
                                             "1990 -1999", 
                                             "2000 -2009", 
                                             "after 2009")) 

survival_data <- survival_data %>%
  filter(!is.na(fgm)) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(cohort5))%>%
  filter(!is.na(cohort10)) %>%
  filter(time != 98)%>%
  filter(time != 99)

# 11 observations where age at cutting > than age of girl in years
survival_data <- survival_data%>%
  filter(as.numeric(time) <= as.numeric(age_years))

# Figure 2a (10-year age cohorts) -----------------------------------------

# Create visualization
data_for_plot <- survival_data 

cohort10 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort10 , data=data_for_plot ,
                    weight= as.numeric(re_wgt))

figure2a.curve <- ggsurvplot(fit = cohort10, data = survival_data, censor = F,
                             legend.title = "Cohort born...", legend=c(0.85, 0.25),
                             font.legend = c(13, "plain", "black"), #alpha = "strata", 
                             
                             font.title = c(18, "bold"),
                             font.x     = c(13), 
                             font.y     = c(13),
                             
                             palette = c("midnightblue", 
                                         "royalblue1", "mediumorchid4", "maroon4", 
                                         "firebrick3",
                                         "firebrick1"),
                             
                             legend.labs = c("1960 -1969",
                                             "1970 -1979",
                                             "1980 -1989",
                                             "1990 -1999", 
                                             "2000 -2009", 
                                             "after 2009"),
                             
                             ylab = "Probabily of not experiencing FGM", 
                             xlab = "Time in years of life") +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- figure2a.curve$plot
g1 <- ggplotGrob(curv_facet)

# Create table
tbl_facet <- as.data.frame(table(data_for_plot$cohort10))
names(tbl_facet) <- c("Cohort", "Unweighted count \n of women/girls \n per cohrot")

tt <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 7))),
  bg_params = list(fill=c( "white"),
                   alpha = rep(c(1,0.5), each=5))))

g2 <- tableGrob(tbl_facet, rows=NULL, theme = tt)

g2$heights <- c(unit(1/2*nrow(g2)+0.4,"null") , rep(unit(1/2*nrow(g2),"null"), nrow(g2)-1))


g2 <- gtable_add_grob(g2,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 2, b = nrow(g2), l = 1, r = ncol(g2))

g2 <- gtable_add_grob(g2,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 1, l = 1, r = ncol(g2))

# Export figure 2a

jpeg("Figure2a.jpg", width = 7.5, height = 5, units = 'in', res = 400)

grid.arrange(g1, g2,  widths = 2:0.5, heights=unit(c(4.5,2), c("in", "mm")))

dev.off()


# Figure 2b (5-year age cohorts) ------------------------------------------

# Create visualization
data_for_plot <- survival_data

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

figure2b.curve <-ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                            legend.title = "Cohort born...", legend=c("right"),
                            font.legend = c(13, "plain", "black"), #alpha = "strata", 
                            
                            font.title = c(18, "bold"),
                            font.x     = c(13), 
                            font.y     = c(13),
                            
                            
                            palette = c("midnightblue", "blue4", "blue3", "blue1",
                                        "royalblue1", "mediumorchid4", "maroon4", 
                                        "firebrick4","firebrick3","firebrick2",
                                        "firebrick1"),
                            
                            legend.labs = c("1960 -1964",
                                            "1965 -1969",
                                            "1970 -1974",
                                            "1975 -1979",
                                            "1980 -1984",
                                            "1985 -1989",
                                            "1990 -1994",
                                            "1995 -1999",
                                            "2000 -2004",
                                            "2005 -2009",
                                            "after 2009"),
                            ylab = "Probabily of not experiencing FGM", 
                            xlab = "Time in years of life") +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- figure2b.curve$plot
g1 <- ggplotGrob(curv_facet)

# Create table
tbl_facet <- as.data.frame(table(data_for_plot$cohort5))
names(tbl_facet) <- c("Cohort", "Unweighted count \n of women/girls \n per cohrot")

tt <- ttheme_default(core=list(
  fg_params=list(fontface=c(rep("plain", 7))),
  bg_params = list(fill=c( "white"),
                   alpha = rep(c(1,0.5), each=5))))

g2 <- tableGrob(tbl_facet, rows=NULL, theme = tt)

g2$heights <- c(unit(1/2*nrow(g2)+7.5,"null") , rep(unit(1/2*nrow(g2),"null"), nrow(g2)-1))

g2 <- gtable_add_grob(g2,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 2, b = nrow(g2), l = 1, r = ncol(g2))

g2 <- gtable_add_grob(g2,
                      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                      t = 1, l = 1, r = ncol(g2))
# Export figure 2b

jpeg("Figure2b.jpg", width = 7.5, height = 5, units = 'in', res = 400)

grid.arrange(g1, g2,  ncol=2, heights=unit(c(4.5,2), c("in", "mm")), widths = c(1,0.5))

dev.off()

# Figure 6 a and b (Guinea and Kenya) ------------------------------------------

data_for_plot <- survival_data %>%
  filter(country == "Guinea")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend= "none",
                  font.legend = c(6, "plain", "black"), #alpha = "strata", 
                  
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g1 <- ggplotGrob(curv_facet)


data_for_plot <- survival_data %>%
  filter(country == "Kenya")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend.title = "Women/girls born in...", legend=c("right"),
                  font.legend = c(10, "plain", "black"), #alpha = "strata", 
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  legend.labs = legend.names,
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g2 <- ggplotGrob(curv_facet)

# Export 

jpeg("Figure6aandb.jpg", width = 7.5, height = 3.5, units = 'in', res = 400)

grid.arrange(g1, g2, nrow = 1, ncol = 2,   widths = c(0.75, 1))

dev.off()

# Figure 3 a and b (Rural vs Urban) ------------------------------------------

data_for_plot <- survival_data %>%
  filter(residence == "Urban")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend= "none",
                  font.legend = c(6, "plain", "black"), #alpha = "strata", 
                  
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g1 <- ggplotGrob(curv_facet)


data_for_plot <- survival_data %>%
  filter(residence == "Rural")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend.title = "Women/girls born in...", legend=c("right"),
                  font.legend = c(10, "plain", "black"), #alpha = "strata", 
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  legend.labs = legend.names,
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g2 <- ggplotGrob(curv_facet)

# Export figure

jpeg("Figure3aandb.jpg", width = 7.5, height = 3.5, units = 'in', res = 1500)

grid.arrange(g1, g2, nrow = 1, ncol = 2,   widths = c(0.75, 1))

dev.off()


# Figure 4 a- k -----------------------------------------------------------

cohorts <- unique(survival_data$cohort5)
plots <- list()

for(i in cohorts){
  
  data_for_plot <- survival_data %>%
    filter(cohort5 == i)
  
  
  cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~residence , data=data_for_plot,
                     weight= as.numeric(re_wgt))
  
  plot<- ggsurvplot(fit = cohort5, data = data_for_plot, censor = F,
                    title = paste("Born",i), legend=c("none"),
                    
                    size = 0.4,
                    
                    font.title = c(8, "bold"),
                    font.x     = c(8), 
                    font.y     = c(8),
                    
                    legend.labs = c("Rural", "Urban"),
                    
                    palette = c("midnightblue", "firebrick3"),
                    
                    xlim = c(0,50),
                    
                    ylab = "Probabily of not experiencing FGM", 
                    xlab = "Time in years of life", font.tickslab = c(6, "plain", "black"))
  
  
  plots[[i]] <- plot
  
  
}

plot <- arrange_ggsurvplots(plots[c(11,6,7,1,9,10,4,3,8,5,2)], ncol=3, nrow=4)

ggsave("Figure4a.jpeg", plot, height = 10, dpi=450)


# Figure 6a (Mauritania, Nigeria, Ethiopia) ------------------------------------------

data_for_plot <- survival_data %>%
  filter(country == "Ethiopia")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend= "none",
                  font.legend = c(6, "plain", "black"), title = "Ethiopia",
                  
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g1 <- ggplotGrob(curv_facet)

data_for_plot <- survival_data %>%
  filter(country == "Mauritania")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend= "none",
                  font.legend = c(6, "plain", "black"), title = "Mauritania",
                  
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g2 <- ggplotGrob(curv_facet)

data_for_plot <- survival_data %>%
  filter(country == "Kenya")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend.title = "Women/girls born in...", legend=c("right"),
                  font.legend = c(10, "plain", "black"), title = "Kenya", 
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  legend.labs = legend.names,
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g3 <- ggplotGrob(curv_facet)

# Export 

jpeg("Figure6a.jpg", width = 7.5, height = 3.5, units = 'in', res = 400)

grid.arrange(g1, g2, g3, nrow = 1, ncol = 3, widths = c(0.6,0.6,1))

dev.off()


# Figure 6b (Gambia,Guinea, Guinea-Bissau) ------------------------------------------

data_for_plot <- survival_data %>%
  filter(country == "Gambia")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend= "none",
                  font.legend = c(6, "plain", "black"), title = "Gambia",
                  
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g1 <- ggplotGrob(curv_facet)

data_for_plot <- survival_data %>%
  filter(country == "Guinea")

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend= "none",
                  font.legend = c(6, "plain", "black"), title = "Guinea",
                  
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("midnightblue", "blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) 

curv_facet <- plot$plot
g2 <- ggplotGrob(curv_facet)

data_for_plot <- survival_data %>%
  filter(country == "Guinea-Bissau") %>%
  filter(cohort5 != "1960 -1964") # only 34 observations

data_for_plot$cohort5 <- factor(data_for_plot$cohort5)

cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                   weight= as.numeric(re_wgt))

legend.names <- levels(data_for_plot$cohort5)

plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                  legend.title = "Women/girls born in...", legend=c("right"),
                  font.legend = c(10, "plain", "black"), title = "Guinea-Bissau", 
                  size = 0.4,
                  
                  font.title = c(10, "bold"),
                  font.x     = c(8), 
                  font.y     = c(8),
                  
                  palette = c("blue4", "blue3", "blue1",
                              "royalblue1", "mediumorchid4", "maroon4", 
                              "firebrick4","firebrick3","firebrick2",
                              "firebrick1"),
                  
                  legend.labs = legend.names,
                  
                  ylab = "Probabily of not experiencing FGM", 
                  xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
  guides(color = guide_legend(reverse = TRUE))

curv_facet <- plot$plot
g3 <- ggplotGrob(curv_facet)

# Export figure 2b

jpeg("Figure6b.jpg", width = 7.5, height = 3.5, units = 'in', res = 400)

grid.arrange(g1, g2, g3, nrow = 1, ncol = 3, widths = c(0.6,0.6,1))

dev.off()
# Figure 3 PAAPoster ------------------------------------------------------

cohorts <- unique(survival_data$cohort10)
plots <- list()


for(i in cohorts){
  
  data_for_plot <- survival_data %>%
    filter(cohort10 == i)
  
  
  cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~residence , data=data_for_plot,
                     weight= as.numeric(re_wgt))
  
  plot<-    ggsurvplot(fit = cohort5, data = data_for_plot, censor = F,
                       title = paste("Born",i), legend=c("none"),
                       
                       size =2,
                       
                       font.title = c(32, "bold"),
                       font.x     = c(32), 
                       font.y     = c(32),
                       
                       legend.labs = c("Rural", "Urban"),
                       
                       palette = c("midnightblue", "firebrick3"),
                       
                       xlim = c(0,40),
                       
                       ylab = "Prob. of no FGM", 
                       xlab = "Age (years)", font.tickslab = c(16, "plain", "black"))
  
  
  plots[[i]] <- plot
  
  
}

plot <- arrange_ggsurvplots(plots[c(4,1,3,2)], ncol=2, nrow=2)
#plot <- arrange_ggsurvplots(plots[c(6,3,5,1,4,2)], ncol=3, nrow=2)

ggsave("Figure3_PAAPoster.jpeg", plot, height = 4, dpi=1200)

# National level -------------------------------------------------------------------

# Export for Annex A ---------------------------------------------

setwd("C:/Users/weny/Google Drive/2019/1- FGM/02- Trend estimates/Visualizations/Annex A")

countries <- unique(survival_data$country)

plots <- list()

for(i in countries){
  
  data_for_plot <- survival_data %>%
    filter(country == i)
  
  data_for_plot$cohort5 <- factor(data_for_plot$cohort5)
  
  cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                     weight= as.numeric(re_wgt))
  
  legend.names <- levels(data_for_plot$cohort5)
  
  plot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                    legend.title = "Women/girls born in...", legend=c("right"),
                    font.legend = c(6, "plain", "black"), #alpha = "strata", 
                    title = paste(i), 
                    size = 0.4,
                    
                    font.title = c(10, "bold"),
                    font.x     = c(8), 
                    font.y     = c(8),
                    
                    palette = c("midnightblue", "blue4", "blue3", "blue1",
                                "royalblue1", "mediumorchid4", "maroon4", 
                                "firebrick4","firebrick3","firebrick2",
                                "firebrick1"),
                    
                    legend.labs = legend.names,
                    
                    ylab = "Probabily of not experiencing FGM", 
                    xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
    guides(color = guide_legend(reverse = TRUE))
  
  plots[[i]] <- plot
  
}

plots <- plots[order(names(plots))]

pdf(paste("Annex A",".pdf", sep=""), onefile=T, paper="a4", height = 9)

arrange_ggsurvplots(plots[1:4],  ncol = 2, nrow = 2)
arrange_ggsurvplots(plots[5:8],  ncol = 2, nrow = 2)
arrange_ggsurvplots(plots[9:12],  ncol = 2, nrow = 2)
arrange_ggsurvplots(plots[13:16],  ncol = 2, nrow = 2)
arrange_ggsurvplots(plots[17:20],  ncol = 2, nrow = 2)
arrange_ggsurvplots(plots[21:24],  ncol = 2, nrow = 2)

dev.off()


# Export for country profiles ------------------------------------------------------

setwd("C:/Users/weny/Google Drive/2019/1- FGM/05- Country profiles/Visualizations for designer/KM curves/")
countries <- unique(survival_data$country)

for(i in countries){
  
  data_for_plot <- survival_data %>%
    filter(country == i)
  
  data_for_plot$cohort5 <- factor(data_for_plot$cohort5)
  
  cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort5 , data=data_for_plot,
                     weight= as.numeric(re_wgt))
  
  legend.names <- levels(data_for_plot$cohort5)
  
  survplot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                        legend.title = "Women/girls born in...", legend=c("right"),
                        font.legend = c(12, "plain", "black"), #alpha = "strata", 
                        title = "Kaplan Meier estimates - 5 year age cohorts",
                        size = 0.4,
                        
                        font.title = c(16, "bold"),
                        font.x     = c(12), 
                        font.y     = c(12),
                        
                        
                        palette = c("midnightblue", "blue4", "blue3", "blue1",
                                    "royalblue1", "mediumorchid4", "maroon4", 
                                    "firebrick4","firebrick3","firebrick2",
                                    "firebrick1"),
                        
                        legend.labs = legend.names,
                        
                        ylab = "Probabily of not experiencing FGM", 
                        xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
    guides(color = guide_legend(reverse = TRUE))
  
  ggsave(file = paste(i, ".pdf"), print(survplot), onefile=FALSE)
  
}


# Figures for country profiles and fact sheets ----------------------------

# Trend

setwd("G:/My Drive/2019/1- FGM/05- Country profiles/Trend")

list.countries <- unique(survival_data$country)

for(i in list.countries){
  
  data_for_plot <- survival_data %>%
    filter(country == i)
  
  data_for_plot$cohort5 <- factor(data_for_plot$cohort10)
  
  cohort5 <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~cohort10 , data=data_for_plot,
                     weight= as.numeric(re_wgt))
  
  legend.names <- levels(data_for_plot$cohort5)
  
  survplot<- ggsurvplot(fit = cohort5, data = survival_data, censor = F,
                        legend.title = "Women/girls born in...", legend=c("right"),
                        font.legend = c(12, "plain", "black"), #alpha = "strata", 
                        title = "Kaplan Meier estimates - 5 year age cohorts",
                        size = 0.4,
                        
                        font.title = c(16, "bold"),
                        font.x     = c(12), 
                        font.y     = c(12),
                        
                        
                        palette = c("midnightblue", "blue1",
                                    "maroon4", "mediumorchid4",
                                    "firebrick2",
                                    "firebrick1"),
                        
                        legend.labs = legend.names,
                        
                        ylab = "Probabily of not experiencing FGM", 
                        xlab = "Time in years of life", font.tickslab = c(6, "plain", "black")) +
    guides(color = guide_legend(reverse = TRUE))
  
  ggsave(file = paste(i, ".pdf"), print(survplot), onefile=FALSE)
  
}

# Age at FGM

setwd("G:/My Drive/2019/1- FGM/05- Country profiles/Age at FGM")

# Only keep latest survey
data.latest <- survival_data %>%
      filter((country == "Burkina Faso" & year == 2010) |
            (country == "Nigeria" & (year == 2017 | year == 2016)) |
            (country == "Senegal" & year == 2017) | 
            (country == "Egypt" & year == 2015) |
            (country == "Sudan" & year == 2014) |
            (country == "Kenya" & year == 2014) | 
            (country == "Ethiopia") |
            (country == "Benin" & (year ==2012 | year == 2011))|
            (country == "Central African Republic" | year == 2010) |
            (country == "Cote d'Ivoire" | year == 2016)|
            (country == "Gambia" | year == 2013) |
            (country == "Ghana" | year == 2011) |
            (country == "Guinea" | year == 2016) |
            (country == "Guinea-Bissau" | year == 2014)|
            (country == "Iraq" | year == 2011) |
            (country == "Kenya" | year == 2014)|
            (country == "Mali" | year == 2015) |
            (country == "Niger" | year == 2012) |
            (country == "Nigeria" | year == 2017) |
            (country == "Senegal" | year == 2017) |
            (country == "Sierra Leone" | year == 2017) |
            (country == "United Republic of Tanzania" | (year == 2015 | year == 2016)) |
            (country == "Togo" | (year == 2013 | year == 2014))|
            (country == "Yemen" | year == 2013)|
            (country == "Chad" | (year == 2014 | year == 2015)))

list.countries <- c("Benin", "Burkina Faso", "Central African Republic",
                    "Chad", "Cote d'Ivoire", "Egypt", "Ethiopia", 
                    "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                    "Iraq", "Kenya", "Mali", "Mauritania", "Niger", "Nigeria", 
                    "Senegal", "Sierra Leone", "Sudan", "Togo", 
                    "United Republic of Tanzania", "Yemen")

results <- data.frame(matrix(, nrow = 0, ncol = 4))

for(i in list.countries){

  data <- data.latest %>%
    filter(country == i) %>%
    filter(fgm >0) 
  
  survival.curve <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~1, 
                      data,
                      weight= as.numeric(re_wgt))
  
  mc <- data.frame(q = c(.25, .5, .75),
                   km = quantile(survival.curve))
  
  temp.results <- data.frame(i, t(as.data.frame(quantile(survival.curve)[1])))
  
  survplot <- ggsurvplot(survival.curve, xlab = "Time (years)", ylab = "Cumulative event",
                         censor = F, data, palette = "brown", conf.int = F, 
                         font.title=c(24, "bold", "brown"),
                         xlim=c(0,20))$plot +
    geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
    geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2) +
    scale_x_continuous(breaks = seq(0, 20, 1))+
    
    labs(title = data$country[1], subtitle = paste("Source:", data$survey[1],data$year[1]))+
    
    theme(legend.position = "none", plot.subtitle = element_text(size = 8))
  
  ggsave(file = paste(i, ".pdf"), print(survplot), onefile=FALSE)
  
  results <- rbind(results, temp.results)
 
}

names(results) <- c("country", "25% cut", "50% cut", "75% cut")
write.csv(results, file = "age.at.fgm.csv")

# Figures for ASRHR supplement --------------------------------------------

setwd("G:/My Drive/2019/1- FGM/11- Ad hoc tasks/ARHR supplement")

# Only keep latest survey
data.latest <- survival_data %>%
  filter(country == "Kenya" & year == 2014) 

survival.curve <- survfit(Surv(as.numeric(time), as.numeric(fgm)==1) ~1, 
                          data.latest,
                          weight= as.numeric(re_wgt))

survplot <- ggsurvplot(fit = survival.curve, data = survival_data, censor = F, conf.int =F,
           fun = "cumhaz", legend = "none",
           
           font.title = c(18, "bold"),
           font.x     = c(13), 
           font.y     = c(13))+
  labs(title = "Kenya - FGM risk at each year of life", 
       subtitle = paste("Source:", data.latest$survey[1],data.latest$year[1]),
       y = "Probability of experiencing FGM at a given age \n if no FGM up this age (cumulative hazard function)",
       x = "Age of girl")


ggsave(file = paste("Kenya.jpg"), print(survplot))





