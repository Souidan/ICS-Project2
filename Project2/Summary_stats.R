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




