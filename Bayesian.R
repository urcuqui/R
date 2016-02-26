install.packages("dplyr", repos = "http://mran.revolutionanalytics.com")
library(ggplot2); data(mpg)
str(mpg)
mpg$mpg <- factor(mpg$hwy>median(mpg$hwy,na.rm = T), labels=c("Bad","Good"))
p.bad =(table(mpg$mpg)/dim(mpg)[1])[1]
p.good =(table(mpg$mpg)/dim(mpg)[1])[2]
mpg$year <- as.factor(mpg$year); mpg$cyl <- as.factor(mpg$cyl)
install.packages(dplyr)
library(plyr)
bad.cases = length(mpg$mpg[mpg$mpg=="Bad"])
good.cases = length(mpg$mpg[mpg$mpg=="Good"])
library(dplyr)
lh_drv <- summarize(group_by(mpg,mpg,drv),count=n())
lh_drv <- mutate(lh_drv, freq= ifelse(mpg=="Bad", count/bad.cases, count/good.cases))
lh_year <- summarize(group_by(mpg,mpg,year),count=n())
lh_year <- mutate(lh_year, freq=ifelse(mpg=="Bad", count/bad.cases, count/good.cases))
lh_cyl <- summarize(group_by(mpg,mpg,cyl),count=n())
lh_cyl <- mutate(lh_cyl,
                 freq = ifelse(mpg=="Bad", count/bad.cases, count/good.cases))
lh_class <- mutate(lh_class,
                   freq = ifelse(mpg=="Bad", count/bad.cases, count/good.cases))
lh_displ <- summarize(group_by(mpg,mpg),
                      mean=mean(displ), std=sd(displ))
tlh_bad <- lh_drv$freq[lh_drv$mpg=="Bad" & lh_drv$drv=="f"] *
  dnorm(2.8,lh_displ$mean[lh_displ$mpg=="Bad"],lh_displ$std[lh_displ$mpg=="Bad"]) *
  lh_year$freq[lh_year$mpg=="Bad" & lh_year$year=="2008"] *
  lh_cyl$freq[lh_cyl$mpg=="Bad" & lh_cyl$cyl=="6"] *
  lh_class$freq[lh_class$mpg=="Bad" & lh_class$class=="midsize"]
tlh_bad <- round(tlh_bad, 6)
tlh_bad
tlh_good <- lh_drv$freq[lh_drv$mpg=="Good" & lh_drv$drv=="f"] *
  dnorm(2.8,lh_displ$mean[lh_displ$mpg=="Good"],lh_displ$std[lh_displ$mpg=="Good"]) *
  lh_year$freq[lh_year$mpg=="Good" & lh_year$year=="2008"] *
  lh_cyl$freq[lh_cyl$mpg=="Good" & lh_cyl$cyl=="6"] *
  lh_class$freq[lh_class$mpg=="Good" & lh_class$class=="midsize"]
tlh_good <- round(tlh_good, 6)
tlh_good

unpost_bad <- tlh_bad*p.bad

#Fast way
library(klaR)
