
# Copyright statement comment ---------------------------------------------

# @UNFPA

# Author comment ----------------------------------------------------------

# written by Kathrin Weny

setwd("C:/Users/weny/Google Drive/2019/1- FGM/02- Trend estimates/Results")
theme_set(theme_sjplot())

# Logistic model ----------------------------------------------------------

load("logistic.model.results.rda")

plot_model(logistic, type = "pred", terms = c("year_birth", "country"), 
           colors = colfunc(24),
           legend.title = " ")+
  scale_x_discrete(limits=seq(from = 1960, to=2020, by=5), labels =seq(from = 1960, to=2020, by=5)) +
  labs(x = "Year of birth", y= "Probability of FGM", title = " ")+
  set_theme( 
    base = theme_classic(), 
    axis.title.size = .9,
    axis.textsize = .9,
    legend.size = 1.25,
    legend.title.size = .8,
    geom.label.size = 3)

# slow decline: Guinea-Bissau, Senegal, Mauritania, Nigeria
plot_model(logistic, type = "pred", terms = c("year_birth", "country"), legend.title = " ",
           colors=c("grey", "grey", "grey", "grey", "grey", "grey",                 # 6
                    "navy", "grey", "grey", "grey", "grey", "grey",                 # 12
                    "grey", "grey", "royalblue3", "grey", "grey", "grey",           # 18
                    "grey", "grey", "grey", "grey", "dodgerblue1", "blue"))+        # 24
  scale_x_discrete(limits=seq(from = 1960, to=2020, by=5), labels =seq(from = 1960, to=2020, by=5)) +
  labs(x = "Year of birth", y= "Probability of FGM", title = " ")

# rapid decline
plot_model(logistic, type = "pred", terms = c("year_birth", "country"), legend.title = " ",
           colors=c("grey", "grey", "grey", "grey", "grey", "red",                  # 6
                    "grey", "grey", "grey", "grey", "yellow", "orange",             # 12
                    "grey", "grey", "grey", "grey", "grey", "darkred",              # 18
                    "grey", "grey", "grey", "grey", "grey", "grey"))+               # 24
  scale_x_discrete(limits=seq(from = 1960, to=2020, by=5), labels =seq(from = 1960, to=2020, by=5)) +
  labs(x = "Year of birth", y= "Probability of FGM", title = " ")

# rapid vs fast
plot_model(logistic, type = "pred", terms = c("year_birth", "country"), legend.title = " ",
           colors=c("grey", "grey", "grey", "grey", "grey", "red",                  # 6
                    "grey", "grey", "grey", "grey", "grey", "grey",             # 12
                    "grey", "grey", "grey", "grey", "grey", "grey",              # 18
                    "grey", "grey", "grey", "grey", "grey", "blue"))+               # 24
  scale_x_discrete(limits=seq(from = 1960, to=2020, by=5), labels =seq(from = 1960, to=2020, by=5)) +
  labs(x = "Year of birth", y= "Probability of FGM", title = " ")

# Simple cox model --------------------------------------------------------

load("cox.model.results.rda")

layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
legend_image <- as.raster(matrix(colfunc1(58), ncol=1))

plot(survfit(cox, newdata = data.frame(country = "Yemen", year_birth= 1960:2017)), 
     col = rev(colfunc1(58)), xlim=c(0,60), axes = F,
     xlab = "Time in years of life",
     ylab = "Probability of not experiencing FGM")
lines(survfit(cox, newdata = data.frame(country = "Guinea", year_birth= 1960:2017)), col = rev(colfunc1(58)))

rasterImage(legend_image, 51, 0.30, 53, 0.70)
text(x=55, y = seq(0.33,0.67,l=5), labels = round(seq(1960, 2017,l=5),0))

text(x=10, y = 1, labels = "Yemen")
text(x=10, y = 0.6, labels = "Guinea")

axis(side=2, seq(0, 1, by=0.25))
axis(side=1, seq(0, 50, by=10))

# Proprotionality assumption
options(scipen=0) # scientific notation in R
load("zph.rda")

plot.new()
plot(zph[9],lwd=2, ylim=c(-2000, 2000),  xlab = "Coefficient for Gambia")

abline(0,0, col=1,lty=3,lwd=2)
abline(h= cox$coef[9], col=3, lwd=2, lty=2)
legend("bottomright",
       legend=c('Reference line for null effect',
                "Average hazard over time",
                "Time-varying hazard"),
       lty=c(3,2,1), col=c(1,3,1), lwd=2)


# Stratified cox model ----------------------------------------------------

load(file = "zph.cox.strata.VI.rda")

ggcoxzph(zph.cox.strata.VI[6])

plot.new()
plot(zph.cox.strata.VI[6],lwd=2, ylim=c(-5, 5))

abline(0,0, col=1,lty=3,lwd=2)

abline(h= zph.cox.strata.V$coef[1], col=3, lwd=2, lty=2)

legend("bottomright",
       legend=c('Reference line for null effect',
                "Average hazard over time",
                "Time-varying hazard"),
       lty=c(3,2,1), col=c(1,3,1), lwd=2)


plot.new()

layout(matrix(ncol=1), width = c(2,1),height = c(1,1))
legend_image <- as.raster(matrix(colfunc1(58), ncol=1))
temp.model$y
load("C:/Users/weny/Google Drive/2019/1- FGM/02- Trend estimates/Results/Guinea.results.rda", verbose=T)
plot(survfit(temp.model, newdata = data.frame(year_birth= 1960:2017)), 
     col = rev(colfunc1(58)), xlim=c(0,60), axes = F,
     xlab = "Time in years of life",
     ylab = "Probability of not experiencing FGM")

load("C:/Users/weny/Google Drive/2019/1- FGM/02- Trend estimates/Results/Yemen.results.rda", verbose=T)
lines(survfit(temp.model, newdata = data.frame(year_birth= 1960:2017)), col = rev(colfunc1(58)))

rasterImage(legend_image, 51, 0.30, 53, 0.70)
text(x=55, y = seq(0.33,0.67,l=5), labels = round(seq(1960, 2017,l=5),0))

text(x=12, y = 1, labels = "Yemen")
text(x=12, y = 0.6, labels = "Guinea")

axis(side=2, seq(0, 1, by=0.25))
axis(side=1, seq(0, 50, by=10))

# big countries


pdf("Annex C.pdf", onefile=T, paper="a4")
plot.new()
legend_image <- as.raster(matrix(colfunc1(58), ncol=1))

list.plots <- list()

par(mfrow = c(2, 2))

for(i in countries){
  
  filename <- paste(i,".results.rda", sep="")
  
  load( filename, verbose = TRUE)
  
  plot(survfit(temp.model, newdata = data.frame(year_birth= 1960:2017)), main = i,
       col = rev(colfunc1(58)), xlim=c(0,60), axes = F,
       xlab = "Time in years of life",
       ylab = "Probability of not experiencing FGM")
  
  rasterImage(legend_image, 51, 0.30, 53, 0.70)
  text(x=57, y = seq(0.33,0.67,l=5), labels = round(seq(1960, 2017,l=5),0), cex=0.7)
  
  axis(side=2, seq(0, 1, by=0.25))
  axis(side=1, seq(0, 50, by=10))
  
  
}


dev.off()





