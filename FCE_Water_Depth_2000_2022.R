getwd()

library(WaveletComp)
library(dplyr)
library(matrixStats)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(viridis)
setwd("C:/Users/kande/Dropbox/Kenny_workfolder/Chapter 3 - Long Term/Data/Time Series Datasets")
dom<-read.csv("FCE.Level.2000.2022.csv")
dom <- dom %>% mutate(date = mdy(date))
dom$Year = as.factor(dom$Year)
###########################################################################################################
#### Figure ?? Water Depth over time ######################################################################
###########################################################################################################
s2<- subset(dom, Site == "SRS2")
s4<- subset(dom, Site == "SRS4")
s6<- subset(dom, Site == "SRS6")
t2<- subset(dom, Site == "TSPh2")
t3<- subset(dom, Site == "TSPh3")
t7<- subset(dom, Site == "TSPh7")
t9<- subset(dom, Site == "TSPh9")
t10<- subset(dom, Site == "TSPh10")

ps2 <- ggplot(s2, aes(x=date, y=level)) +
  geom_line() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  ylab("Water Level (cm)") + xlab("")+
  stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 140)
ps2

ps4 <- ggplot(s4, aes(x=date, y=level)) +
  geom_line() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  ylab("Water Level (cm)") + xlab("")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 70)
ps4

ps6 <- ggplot(s6, aes(x=date, y=level)) +
  geom_line() + xlab("")+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  ylab("Water Level (cm)") + xlab("")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 15)
ps6

pt2 <- ggplot(t2, aes(x=date, y=level)) +
  geom_line() +   xlab("")+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  ylab("") + xlab("")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 135)
pt2

pt3 <- ggplot(t3, aes(x=date, y=level)) +
  geom_line() +   xlab("")+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+ ylab("") + xlab("")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 28)
pt3

pt7 <- ggplot(t7, aes(x=date, y=level)) +
  geom_line() + xlab("")+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+ ylab("") + xlab("")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 17)
pt7

pt9 <- ggplot(t9, aes(x=date, y=level)) +
  geom_line() + xlab("")+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  ylab("Water Level (cm)") + xlab("Year")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 15)
pt9

pt10 <- ggplot(t10, aes(x=date, y=level)) +
  geom_line() + xlab("")+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  ylab("") + xlab("Year")+
  stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)+theme_classic()+
  stat_regline_equation(label.y = 15)
pt10

ggarrange(ps2,pt2,ps4,pt3,ps6,pt7,pt9,pt10, nrow = 4,ncol=2, common.legend = TRUE, legend="bottom", labels =c("A","B","C","D","E","F","G","H") )
ggarrange(ps2,pt2,ps6,pt7,pt10,pt9, nrow = 3,ncol=2, common.legend = TRUE, legend="bottom", labels =c("A","B","C","D","E","F"))

