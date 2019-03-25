---
title: "ketamine-table2"
author: "Yanan Ren"
date: "2016Äê9ÔÂ21ÈÕ"
output: word_document
---


```{r results = 'asis'} 
## read dataset ##
ketamine <- read.csv('D:/Biostatistics/2016FallRA/ketamine.csv',header=T, sep=',')
nrow(ketamine)
ncol(ketamine)

## mean at baseline ##

mean(as.numeric(as.character(ketamine$bl_cdrs_comb_total_raw_summary_score[ketamine$bl_cdrs_comb_total_raw_summary_score!="NULL"])))

mean(as.numeric(as.character(ketamine$bl_bdi_total_score
[ketamine$bl_bdi_total_score!="NULL"])))

mean(as.numeric(as.character(ketamine$bl_madrs_total
[ketamine$bl_madrs_total!="NULL"])))

mean(as.numeric(as.character(ketamine$bl_shaps_total[ketamine$bl_shaps_total!="NULL"])))

## sd at baseline ##
sd(as.numeric(as.character(ketamine$bl_cdrs_comb_total_raw_summary_score[ketamine$bl_cdrs_comb_total_raw_summary_score!="NULL"])))

sd(as.numeric(as.character(ketamine$bl_bdi_total_score
[ketamine$bl_bdi_total_score!="NULL"])))

sd(as.numeric(as.character(ketamine$bl_madrs_total
[ketamine$bl_madrs_total!="NULL"])))

sd(as.numeric(as.character(ketamine$bl_shaps_total[ketamine$bl_shaps_total
!="NULL"])))


## mean at week 6 ##
mean(as.numeric(as.character(ketamine$ptx_cdrs_comb_total_raw_summary_score
[ketamine$ptx_cdrs_comb_total_raw_summary_score!="NULL"])))

mean(as.numeric(as.character(ketamine$ptx_bdi_total_score
[ketamine$ptx_bdi_total_score!="NULL"])))

mean(as.numeric(as.character(ketamine$ptx_madrs_total
[ketamine$ptx_madrs_total!="NULL"])))

mean(as.numeric(as.character(ketamine$ptx_shaps_total[ketamine$ptx_shaps_total!="NULL"])))

## sd at week 6 ##
sd(as.numeric(as.character(ketamine$ptx_cdrs_comb_total_raw_summary_score
[ketamine$ptx_cdrs_comb_total_raw_summary_score!="NULL"])))

sd(as.numeric(as.character(ketamine$ptx_bdi_total_score
[ketamine$ptx_bdi_total_score!="NULL"])))

sd(as.numeric(as.character(ketamine$ptx_madrs_total
[ketamine$ptx_madrs_total!="NULL"])))

sd(as.numeric(as.character(ketamine$ptx_shaps_total[ketamine$ptx_shaps_total!="NULL"])))

## difference between week 6 and baseline ##
diff<-c("grid","bl_cdrs_comb_total_raw_summary_score","ptx_cdrs_comb_total_raw_summary_score","bl_bdi_total_score","ptx_bdi_total_score","bl_madrs_total","ptx_madrs_total","bl_shaps_total","ptx_shaps_total")
ketaminesub<-ketamine[diff]
ncol(ketaminesub)
head(ketaminesub)
ketaminesub[ketaminesub[,]=="NULL"]<-NA
View(ketaminesub)

ketaminesub1<-na.omit(ketaminesub[,1:3])
diff_cdrs<-as.numeric(as.character(ketaminesub1[,2]))-as.numeric(as.character(ketaminesub1[,3]))
diff_cdrs
mean(diff_cdrs)
sd(diff_cdrs)
## one sample t-test for diff with Ho: mu=0 Ha=greater ##
t.test(diff_cdrs, mu=0, alternative="greater", conf.level=0.95)

ketaminesub2<-na.omit(ketaminesub[,4:5])
diff_bdi<-as.numeric(as.character(ketaminesub2[,1]))-as.numeric(as.character(ketaminesub2[,2]))
diff_bdi
mean(diff_bdi)
sd(diff_bdi)
## one sample t-test for diff with Ho: mu=0 Ha=greater ##
t.test(diff_bdi, mu=0, alternative="greater", conf.level=0.95)

ketaminesub3<-na.omit(ketaminesub[,6:7])
diff_madrs<-as.numeric(as.character(ketaminesub3[,1]))-as.numeric(as.character(ketaminesub3[,2]))
diff_madrs
mean(diff_madrs)
sd(diff_madrs)
## one sample t-test for diff with Ho: mu=0 Ha=greater ##
t.test(diff_madrs, mu=0, alternative="greater", conf.level=0.95)

ketaminesub4<-na.omit(ketaminesub[,8:9])
diff_shaps<-as.numeric(as.character(ketaminesub4[,1]))-as.numeric(as.character(ketaminesub4[,2]))
diff_shaps
mean(diff_shaps)
sd(diff_shaps)
## one sample t-test for diff with Ho: mu=0 Ha=greater ##
t.test(diff_shaps, mu=0, alternative="greater", conf.level=0.95)
t.test(diff_shaps, mu=0, alternative="less", conf.level=0.95)
t.test(diff_shaps, mu=0, alternative="two.sided", conf.level=0.95)



## Table 2. Primary Outcomes ##
Rawscore <- c("CDRS","BDI","MADRS","SHAPS")
BL.mean<-c(61.3,25.0,29.8,7.0)
BL.SD<-c(15.3,11.5,5.0,3.2)
Baseline <-paste(BL.mean,"¡À",BL.SD, sep=" ")
Week_6.mean<-c(46.7,15.0,19.1,5.7)
Week_6.SD<-c(15.2,12.2,10.7,5.0)
Week_6<-paste(Week_6.mean,"¡À",Week_6.SD)
Diff.week6_BL<-c(14.3,6.3,8.2,-1)
Diff.SD<-c(13.0,5.3,8.3,3.2)
Diff<-paste(Diff.week6_BL,"¡À",Diff.SD)
p.value<-c(0.021,0.049,0.030,0.714)

table2<-data.frame(Rawscore,Baseline,Week_6,Diff,p.value)

table2<-data.frame(Rawscore,BL.mean,BL.SD,Week_6.mean,Week_6.SD,Diff.week6_BL,p.value)

library(knitr)
kable(table2, caption = "Table 2. Primary Outcomes")
```

