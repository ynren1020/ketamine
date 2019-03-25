---
title: "BDI-prepost-ttest"
author: "Yanan Ren"
date: "2016Äê10ÔÂ5ÈÕ"
output: word_document
---

```{r results = 'asis',echo=FALSE} 
## read dataset ##
ketamine <- read.csv('D:/Biostatistics/2016FallRA/ketamine.csv',header=T, sep=',')
nrow(ketamine)
ncol(ketamine)

##BDI subset ##

bdi<-c("grid","bl_bdi_total_score","ptx_bdi_total_score","post_inf1_bdi_total_score","pre_inf1_bdi_total_score","post_inf2_bdi_total_score","pre_inf2_bdi_total_score","post_inf3_bdi_total_score","pre_inf3_bdi_total_score","post_inf4_bdi_total_score","pre_inf4_bdi_total_score","post_inf5_bdi_total_score","pre_inf5_bdi_total_score","post_inf6_bdi_total_score","pre_inf6_bdi_total_score")
ketaminebdi<-ketamine[bdi]
ncol(ketaminebdi)
nrow(ketaminebdi)
head(ketaminebdi)
ketaminebdi[ketaminebdi[,]=="NULL"]<-NA
View(ketaminebdi)
## bdi infusion 1 ##
ketaminebdi1<-na.omit(ketaminebdi[,4:5])
diff_bdi1<-as.numeric(as.character(ketaminebdi1[,2]))-as.numeric(as.character(ketaminebdi1[,1]))
diff_bdi1
mean(diff_bdi1)
sd(diff_bdi1)
## one sample t-test for diff with Ho: mu=0 Ha=greater ## p.value=0.013
t.test(diff_bdi1, mu=0, alternative="greater", conf.level=0.95)

## bdi infusion2 ##
ketaminebdi2<-na.omit(ketaminebdi[,6:7])
diff_bdi2<-as.numeric(as.character(ketaminebdi2[,2]))-as.numeric(as.character(ketaminebdi2[,1]))
diff_bdi2
mean(diff_bdi2)
sd(diff_bdi2)
## one sample t-test for diff with Ho: mu=0 Ha=greater ## p.value=0.053
t.test(diff_bdi2, mu=0, alternative="greater", conf.level=0.95)

## bdi infusion3 ##
ketaminebdi3<-na.omit(ketaminebdi[,8:9])
diff_bdi3<-as.numeric(as.character(ketaminebdi3[,2]))-as.numeric(as.character(ketaminebdi3[,1]))
diff_bdi3
mean(diff_bdi3)
sd(diff_bdi3)
## one sample t-test for diff with Ho: mu=0 Ha=greater ## p.value=0.020
t.test(diff_bdi3, mu=0, alternative="greater", conf.level=0.95)

## bdi infusion4 ##
ketaminebdi4<-na.omit(ketaminebdi[,10:11])
diff_bdi4<-as.numeric(as.character(ketaminebdi4[,2]))-as.numeric(as.character(ketaminebdi4[,1]))
diff_bdi4
mean(diff_bdi4)
sd(diff_bdi4)
## one sample t-test for diff with Ho: mu=0 Ha=greater ## p.value=0.557
t.test(diff_bdi4, mu=0, alternative="greater", conf.level=0.95)

## bdi infusion5 ##
ketaminebdi5<-na.omit(ketaminebdi[,12:13])
diff_bdi5<-as.numeric(as.character(ketaminebdi5[,2]))-as.numeric(as.character(ketaminebdi5[,1]))
diff_bdi5
mean(diff_bdi5)
sd(diff_bdi5)
## one sample t-test for diff with Ho: mu=0 Ha=greater ## p.value=0.161
t.test(diff_bdi5, mu=0, alternative="greater", conf.level=0.95)

## bdi infusion6 ##
ketaminebdi6<-na.omit(ketaminebdi[,14:15])
diff_bdi6<-as.numeric(as.character(ketaminebdi6[,2]))-as.numeric(as.character(ketaminebdi6[,1]))
diff_bdi6
mean(diff_bdi6)
sd(diff_bdi6)
## one sample t-test for diff with Ho: mu=0 Ha=greater ## p.value=0.036
t.test(diff_bdi6, mu=0, alternative="greater", conf.level=0.95)



```

