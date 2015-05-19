getwd()
setwd("~/Documents/R/UR/Data/")
reddit <- read.csv ("reddit.csv", header = TRUE, sep = ",")
summary(reddit)
str(reddit)

pf <- read.csv("pseudo_facebook.tsv", sep = "\t")
names(pf)
str(pf)
count <- subset(pf, gender == "male")
nrow(subset(pf,www_likes != 0))
by(pf$www_likes,pf$gender, sum)

library(ggplot2)
library(dplyr)

#friend_count ####
table(pf$gender)
by(pf$friend_count,pf$gender, summary)
ggplot(data = pf , aes(x=friend_count)) + geom_histogram()

qplot(x= friend_count, data = pf)

qplot(x= friend_count, data = subset(pf, !is.na(gender)), binwidth = 25) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50)) + 
  facet_wrap(~gender)

qplot(x= friend_count, y = ..count../sum(..count..), data = subset(pf, !is.na(gender)), binwidth = 10, 
  geom = 'freqpoly', color = gender , xlab = 'Friend Count',
  ylab = 'Percentage of users with that friend count') + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))

ggplot(aes(x = friend_count, y = ..count../sum(..count..)), data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender), binwidth=10, ) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  xlab('Friend Count') + 
  ylab('Percentage of users with that friend count')


qplot(x = gender, y = friend_count , data = subset(pf,!is.na(gender)), geom = 'boxplot') +
      coord_cartesian(ylim = c(0,250))

qplot(x = gender, y = friend_count , data = subset(pf,!is.na(gender)), geom = 'boxplot', ylim = c(0,250))
# www_likes ####
qplot(x= www_likes, data = pf, color = gender) 

ggplot(aes(x = www_likes), data = pf) +
  geom_freqpoly(aes(color = gender)) + 
  scale_x_log10() #scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7))

#Tenure ####
qplot(x= tenure/365, data = subset(pf, !is.na(gender)), binwidth = .25, 
      xlab = ("Number of years using Facebook") , ylab = ("Number of users in sample") ,
      color = I('black'), fill = I('#099DD9')) + 
  scale_x_continuous(breaks = seq(1,7,1), lim = c(0,7))
ggplot(aes(x = tenure / 365), data = pf) +
  geom_histogram(color = 'black', fill = '#F79420') + 
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) + 
  xlab('Number of years using Facebook') + 
  ylab('Number of users in sample')

#dob_day ####
ggplot(data = pf, aes(x = dob_day)) + 
  geom_histogram() + 
  scale_x_discrete(breaks = 1:31) +
  facet_wrap(~dob_month)

#ages ####
qplot(x= age, data = pf , binwidth = 1 ,
      xlab = ("Age") , ylab = ("Number of users in sample") ,
      color = I('black'), fill = I('#099DD9')) #+ 
  scale_x_discrete(breaks = seq(0,113,5))

ggplot(aes(x = age), data = pf) + 
  geom_histogram(binwidth = 1, fill = '#5760AB') + 
  scale_x_discrete(breaks = seq(0, 113, 5))


# Multiple plot ####
library(gridExtra)
p1 = qplot(x= friend_count, data = pf , color = I('black'), fill = I('#099DD9'))
p2 = qplot(x= log10(friend_count), data = pf, color = I('black'), fill = I('#F79420'))
p3 = qplot(x= sqrt(friend_count), data = pf, color = I('black'), fill = I('#5760AB'))

grid.arrange ( p1,p2,p3,ncol = 2)

ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram(binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

#Explore 2 variables ####
ggplot(aes(x=age, y=friend_count), data = pf) +
  geom_jitter(alpha = 1/20, position = position_jitter(h=0)) +
  xlim(13,90) +
  coord_trans(y = "sqrt")

pf.fc_by_age <- pf %>%
  group_by(age) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = mean(friend_count),
            n=n()) %>%
  arrange(age)

head(pf.fc_by_age)

ggplot (aes(x=age, y=friend_count_mean), data = pf.fc_by_age) +
  geom_line()
