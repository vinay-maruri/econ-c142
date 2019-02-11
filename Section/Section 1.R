
#############
# Section 1 #
#############

#Install packages (only once)

install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("statar")
install.packages("readr")
#Call libraries

library(haven)
library(dplyr)
library(statar)
library(ggplot2)

#Set Working Directory
setwd("C:\Users\EndlessWormhole\Desktop\Spring 2019\Econ C142\problem set 1")

# Load dataset
data("mtcars")
head(mtcars,4)

# Browse dataset
names(mtcars)
mtcars # View(mtcars)
head(mtcars) #or tail(mtcars)
head(mtcars,3)
head(mtcars$mpg,3)

# Subset of dataset
data_small <- select(mtcars,mpg,hp,wt)
data_small

# Summary Statistics
summ_mpg <- summarize(data_small, mean(mpg), median(mpg), sd(mpg))
summ_mpg

# Figures
hist(data_small$mpg,
     main = "Mpg Histogram",
     xlab = "Miles per Galon")
abline(v=19,col="blue",lwd=2)

plot(data_small$hp,data_small$mpg,
     main = "Scatter Plot HP and Mpg",
     xlab = "HP",
     ylab = "Mpg")
#abline(lm(data_small$hp~data_small$wt), col="red") # regression line (y~x) 
#lines(lowess(data_small$hp,data_small$mpg), col="blue") # lowess line (x,y)

# Identify Quantiles of MPG

quantile(data_small$mpg)
quantile(data_small$mpg, seq(0,1,by=.1))
quantile(data_small$mpg, seq(.2,1,by=.2))

#Subset of data within each quantile
q2_mpg  <- subset(data_small, mpg <= 15.2, select=c(hp,mpg))
q4_mpg  <- subset(data_small, mpg > 15.2 & mpg <=17.92, select=c(hp,mpg))
q6_mpg  <- subset(data_small, mpg > 17.92 & mpg <=21, select=c(hp,mpg))
q8_mpg  <- subset(data_small, mpg > 21 & mpg <=24.08, select=c(hp,mpg))
q10_mpg <- subset(data_small, mpg > 24.08, select=c(hp,mpg))

#Mean hp within each quantile.
hp_q2mpg <-summarize(q2_mpg, mean(hp))
hp_q4mpg <-summarize(q4_mpg, mean(hp))
hp_q6mpg <-summarize(q6_mpg, mean(hp))
hp_q8mpg <-summarize(q8_mpg, mean(hp))
hp_q10mpg <-summarize(q10_mpg, mean(hp))

#Mean mpg within each quantile.
mpg_q2mpg <-summarize(q2_mpg, mean(mpg))
mpg_q4mpg <-summarize(q4_mpg, mean(mpg))
mpg_q6mpg <-summarize(q6_mpg, mean(mpg))
mpg_q8mpg <-summarize(q8_mpg, mean(mpg))
mpg_q10mpg <-summarize(q10_mpg, mean(mpg))

Qhp  <- c(hp_q2mpg,hp_q4mpg,hp_q6mpg,hp_q8mpg,hp_q10mpg)
Qmpg  <- c(mpg_q2mpg,mpg_q4mpg,mpg_q6mpg,mpg_q8mpg,mpg_q10mpg)

#Plot 1
plot(Qhp, Qmpg,
     main = "Mean HP against Mean Mpg in the Quantile (Binscatter)",
     xlab = "HP",
     ylab = "Mpg")

#Plot 2
plot(data_small$hp,data_small$mpg,
     main = "Scatter Plot HP and Mpg",
     col="red",
     xlab = "HP",
     ylab = "Mpg")
points(Qhp, Qmpg,
     main = "Scatter Plot: Mean HP by Quantile of Mpg",
     col="blue",
     xlab = "HP",
     ylab = "Mpg")

#Alternative (faster): use ad-hoc packages
g <- ggplot(data_small, aes(x = hp , y = mpg)) 
g + stat_binmean(n = 5)

