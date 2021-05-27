library(tidyverse)
library(qwraps2)
options(qwraps2_markup = "latex") 

theme_set(theme_gray())

data_height = read.csv2('/Users/osx/Documents/TU Dortmund/ICS/project2/Height_Data.csv',sep=';')

ggplot(data_height, aes(x=Height)) + geom_histogram(bins = 12,color='white',fill='red',alpha=0.5) + facet_wrap(~ Sport)
ggplot(data_height, aes(x=Sport, y=Height, fill=Sport)) +geom_boxplot()


our_summary1 <-
  list("Player Height (Basketball)" =
         list("min"       = ~ min(Height),
              "median (25%,75%)"    = ~ median_iqr(Height),
              "max"       = ~ max(Height),
              "mean (sd)" = ~ qwraps2::mean_sd(Height))
       
  )

data_height_basketball <- subset(data_height, Sport == 'basketball')

our_summary1 <-
  list("Player Height (Handball)" =
         list("min"       = ~ min(Height),
              "median (25%,75%)"    = ~ median_iqr(Height),
              "max"       = ~ max(Height),
              "mean (sd)" = ~ qwraps2::mean_sd(Height))
       
  )

data_height_handball <- subset(data_height, Sport == 'handball')


our_summary1 <-
  list("Player Height (Ice Hockey)" =
         list("min"       = ~ min(Height),
              "median (25%,75%)"    = ~ median_iqr(Height),
              "max"       = ~ max(Height),
              "mean (sd)" = ~ qwraps2::mean_sd(Height))
       
  )

data_height_ice_hockey <- subset(data_height, Sport == 'ice hockey')

our_summary1 <-
  list("Player Height (Soccer)" =
         list("min"       = ~ min(Height),
              "median (25%,75%)"    = ~ median_iqr(Height),
              "max"       = ~ max(Height),
              "mean (sd)" = ~ qwraps2::mean_sd(Height))
       
  )

data_height_soccer <- subset(data_height, Sport == 'soccer')

our_summary1 <-
  list("Player Height (Volleyball)" =
         list("min"       = ~ min(Height),
              "median (25%,75%)"    = ~ median_iqr(Height),
              "max"       = ~ max(Height),
              "mean (sd)" = ~ qwraps2::mean_sd(Height))
       
  )

data_height_volleyball <- subset(data_height, Sport == 'volleyball')


our_summary1 <-
  list("Player Height (Waterpolo)" =
         list("min"       = ~ min(Height),
              "median (25%,75%)"    = ~ median_iqr(Height),
              "max"       = ~ max(Height),
              "mean (sd)" = ~ qwraps2::mean_sd(Height))
       
  )

data_height_waterpolo <- subset(data_height, Sport == 'water polo')



whole <- summary_table(data_height_basketball, our_summary1)
whole
  
whole <- summary_table(data_height_handball, our_summary1)
whole



whole <- summary_table(data_height_ice_hockey, our_summary1)
whole

whole <- summary_table(data_height_soccer, our_summary1)
whole

whole <- summary_table(data_height_volleyball, our_summary1)
whole

whole <- summary_table(data_height_waterpolo, our_summary1)
whole



data_height$Sport <- as.factor(data_height$Sport)
levels(data_height$Sport)
anova <- aov(Height ~ Sport, data = data_height)
summary(anova)
pairwise.t.test(data_height$Height, data_height$Sport,
                p.adjust.method = "bonferroni")



par(mfrow=c(2,3))
qqnorm(data_height$Height[data_height$Sport=="soccer"], pch = 1, frame = FALSE, main = "Soccer", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="soccer"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="basketball"], pch = 1, frame = FALSE, main = "Basketball", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="basketball"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="handball"], pch = 1, frame = FALSE, main = "Handball", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="handball"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="water polo"], pch = 1, frame = FALSE, main = "Water Polo", ylab = "Heights of the Players [cm]" )
qqline(data_height$Height[data_height$Sport=="water polo"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="volleyball"], pch = 1, frame = FALSE,  main = "Volleyball", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="volleyball"], col = "steelblue", lwd = 2)

qqnorm(data_height$Height[data_height$Sport=="ice hockey"], pch = 1, frame = FALSE, main = "Ice Hockey", ylab = "Heights of the Players [cm]")
qqline(data_height$Height[data_height$Sport=="ice hockey"], col = "steelblue", lwd = 2)

var(data_height$Height[data_height$Sport=="soccer"])
var(data_height$Height[data_height$Sport=="water polo"])
var(data_height$Height[data_height$Sport=="ice hockey"])
var(data_height$Height[data_height$Sport=="basketball"])
var(data_height$Height[data_height$Sport=="handball"])
var(data_height$Height[data_height$Sport=="volleyball"])

par(mfrow=c(1,1))
boxplot(data_height$Height~data_height$Sport, col = c("gold","gray","royalblue1", "Orange1", "palegreen4", "mediumpurple1"), xaxt="n", xlab="" , ylab=("Heigth [cm]"))
grid(NA, 6, col = "gray", lty = "dotted",lwd = par("lwd"))
axis(1, at = c(1,2,3,4,5,6),  c("Basketball", "Handball", "Ice hockey", "Soccer", "Volleyball","Water polo") , tick=FALSE , cex=0.1)
abline(v=1.5,lty=1, col="grey")
abline(v=2.5,lty=1, col="grey")
abline(v=3.5,lty=1, col="grey")
abline(v=4.5,lty=1, col="grey")
abline(v=5.5,lty=1, col="grey")
