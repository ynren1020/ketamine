##spearman correlation ##
library(Hmisc)
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

perchange<-(bl_cdrs_comb_total_raw_summary_score-ptx_cdrs_comb_total_raw_summary_score)/(bl_cdrs_comb_total_raw_summary_score-17)
perchange 

plot(age_at_baseline,perchange)
plot(sex,perchange)
plot(depression_age_of_onset,perchange)
plot(depression_txs_tried,perchange)
plot(bl_cdrs_comb_total_raw_summary_score,perchange)
plot(bl_bdi_total_score,perchange)
plot(bl_madrs_total,perchange)
plot(bl_teps_consummatory_total,perchange)
plot(bl_teps_anticipatory_total,perchange)
plot(bl_shaps_total,perchange)
plot(pre_inf1_cadss_total,perchange)
plot(pre_inf1_cadss_observer_total,perchange)

corsub<-c("sex","age_at_baseline","iq_2","height","weight", "depression_age_of_onset","depression_txs_tried","bl_cdrs_comb_total_raw_summary_score","bl_bdi_total_score","ptx_bdi_total_score","bl_madrs_total","ptx_madrs_total","bl_teps_consummatory_total","bl_teps_anticipatory_total","bl_shaps_total","pre_inf1_cadss_total","pre_inf1_cadss_observer_total","post_inf1_cadss_total","post_inf1_cadss_observer_total")
ketaminecorsub<-ketamine[corsub]
ketaminecorsub$perchange<-perchange
ncol(ketaminecorsub)
View(ketaminecorsub)
ketaminecorsub$cadssdiff<-ketaminecorsub[,16]-ketaminecorsub[,18]
ketaminecorsub$cadssobdiff<-ketaminecorsub[,17]-ketaminecorsub[,19]
ketaminecorsub$bdidiff<-ketaminecorsub[,9]-ketaminecorsub[,10]
ketaminecorsub$madrsdiff<-ketaminecorsub[,11]-ketaminecorsub[,12]
ketaminecorsub$BMI<-ketaminecorsub[,5]*0.45/((ketaminecorsub[,4]*0.025)^2)
ketaminecorsub$dose<-c(30.5,34.5,25,30,26.5,57,42,45)
ketaminecorsub$dose_per_actual_body_weight<-c(0.275,0.341,0.256,0.396,0.501,0.502,0.501,0.501)
##Summary statistic BMI IQ txs-tried##
mean(ketaminecorsub[,25])  ##BMI
sd(ketaminecorsub[,25])
mean(ketaminecorsub[,3])   ##IQ
sd(ketaminecorsub[,3])
mean(ketaminecorsub[,7])    ##txs-tried
sd(ketaminecorsub[,7])
mean(ketaminecorsub[,6])    ##age on set
sd(ketaminecorsub[,6])
median(ketaminecorsub[,6])
range(ketaminecorsub[,6])

##histgram##
hist(ketaminecorsub[,25],main="Histogram of BMI",xlab="BMI")
library(ggplot2)
binsize<-diff(range(ketaminecorsub[,25]))/5
ggplot(data=ketaminecorsub, aes(ketaminecorsub[,25])) + 
  geom_histogram(binwidth=binsize,
                 col="black", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram for BMI") +
  labs(x="BMI", y="Count") + 
  xlim(c(10,52)) + 
  ylim(c(0,3))


##histgram perchange##
hist(perchange,main="Histogram of CDRS percent change scores",xlab="CDRS")
png(file="histogram4.png",width=1000, height=651)
library(ggplot2)
binsize<-diff(range(perchange))/5
ggplot(data=ketaminecorsub, aes(ketaminecorsub[,20])) + 
  geom_histogram(binwidth=binsize,
                 col="black", 
                 fill="blue", 
                 alpha = .2) + 
  labs(title="Histogram of CDRS percent change scores",cex.main=3.5) +
  labs(x="CDRS percent change", y="Count",cex.lab=3.5,cex.axis=3.5) + 
  xlim(c(-0.5,1)) + 
  ylim(c(0,3))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title =element_text(size=24, face='bold'))
dev.off()


##regression on sex##
ketaminecorsub[,1]<-as.factor(ketaminecorsub[,1]-1)
ketaminecorsub[,1]
fit.lm1<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + sex, data=ketaminecorsub)
summary(fit.lm1)
## regression on medication ##
ketaminecorsub$prozac<-as.factor(c(rep(1,5),0,1,1))
ketaminecorsub$zoloft<-as.factor(c(1,0,1,0,1,1,0,0))
ketaminecorsub$wellbutrin<-as.factor(c(1,1,0,0,rep(1,4)))
ketaminecorsub$seroquel<-as.factor(c(0,1,0,0,1,1,0,0))
ketaminecorsub$abilify<-as.factor(c(1,rep(0,5),1,0))
ketaminecorsub$cymbalta<-as.factor(c(0,1,1,rep(0,5)))
ketaminecorsub$gabapentin<-as.factor(c(rep(0,4),1,rep(0,3)))
ketaminecorsub$cita<-as.factor(c(0,0,0,1,rep(0,4)))
ketaminecorsub$effexor<-as.factor(c(rep(0,6),1,0))
ketaminecorsub$adderall<-as.factor(c(0,1,rep(0,6)))
ketaminecorsub$modafinil<-as.factor(c(0,0,0,1,rep(0,4)))
ketaminecorsub$clonipine<-as.factor(c(1,rep(0,7)))
ketaminecorsub$lamictal<-as.factor(c(rep(0,7),1))
##Model##
fit.lm.p<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + prozac, data=ketaminecorsub)
summary(fit.lm.p)

fit.lm.z<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + zoloft, data=ketaminecorsub)
summary(fit.lm.z)

fit.lm.w<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + wellbutrin, data=ketaminecorsub)
summary(fit.lm.w)

fit.lm.s<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + seroquel, data=ketaminecorsub)
summary(fit.lm.s)

fit.lm.a<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + abilify, data=ketaminecorsub)
summary(fit.lm.a)

fit.lm.c<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + cymbalta, data=ketaminecorsub)
summary(fit.lm.c)

fit.lm.g<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + gabapentin, data=ketaminecorsub)
summary(fit.lm.g)

fit.lm.cita<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + cita, data=ketaminecorsub)
summary(fit.lm.cita)

fit.lm.e<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + effexor, data=ketaminecorsub)
summary(fit.lm.e)

fit.lm.add<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + adderall, data=ketaminecorsub)
summary(fit.lm.add)

fit.lm.m<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + modafinil, data=ketaminecorsub)
summary(fit.lm.m)

fit.lm.clon<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + clonipine, data=ketaminecorsub)
summary(fit.lm.clon)

fit.lm.l<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + lamictal, data=ketaminecorsub)
summary(fit.lm.l)

##model on categories##
ketaminecorsub$ssri<-as.factor(c(1,0,rep(1,4),0,0))
ketaminecorsub$others<-as.factor(c(rep(1,3),0,rep(1,4)))
ketaminecorsub$stimu<-as.factor(c(rep(c(0,1),2),rep(0,4)))
ketaminecorsub$anti<-as.factor(c(1,1,0,0,1,1,1,0))
#ketaminecorsub$benzo<-as.factor(c(1,0,rep(1,4),0,0))
ketaminecorsub$seisure<-as.factor(c(rep(0,4),1,0,0,1))
View(ketaminecorsub)

fit.lm.ssri<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + ssri, data=ketaminecorsub)
summary(fit.lm.ssri)
fit.lm.others<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + others, data=ketaminecorsub)
summary(fit.lm.others)
fit.lm.stimu<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + stimu, data=ketaminecorsub)
summary(fit.lm.stimu)
fit.lm.anti<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + anti, data=ketaminecorsub)
summary(fit.lm.anti)
fit.lm.seisure<-lm(perchange~bl_cdrs_comb_total_raw_summary_score + seisure, data=ketaminecorsub)
summary(fit.lm.seisure)

confint(fit.lm.ssri)
confint(fit.lm.others)
confint(fit.lm.stimu)
confint(fit.lm.anti)
confint(fit.lm.clon)
confint(fit.lm.seisure)


## Newly added##
library("PerformanceAnalytics")
#correlation between perchange and cadssdiff,bdidiff,and madrsdiff
ketcorsub1<-ketaminecorsub[, c(24,23,22,21,20)]
chart.Correlation(ketcorsub1, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

#correlation between perchange in cdrs and bl-bdi
ketcorsub2 <- ketaminecorsub[, c(9,20)]
chart.Correlation(ketcorsub2, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

#correlation between perchange in cdrs and BMI
ketcorsub3 <- ketaminecorsub[, c(25,20)]
chart.Correlation(ketcorsub3, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

#correlation between perchange in cdrs and txs-tried
ketcorsub4 <- ketaminecorsub[, c(7,20)]
chart.Correlation(ketcorsub4, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

#correlation between perchange in cdrs and IQ
ketcorsub5 <- ketaminecorsub[, c(3,20)]
chart.Correlation(ketcorsub5, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

#correlation between perchange in cdrs and age on set
ketcorsub6 <- ketaminecorsub[, c(6,20)]
chart.Correlation(ketcorsub6, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

#correlation between perchange in cdrs and dose and dose per ABW
ketcorsub7 <- ketaminecorsub[, c(26,27,20)]
chart.Correlation(ketcorsub7, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

####################
without6 <- ketaminecorsub[, c(2,5,7,11,12,13)]
chart.Correlation(without6, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")
#chart.Correlation(without6, histogram=TRUE, pch=19)

without16<-ketaminecorsub[, c(8,9,10,13)]
chart.Correlation(without16, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

without67<-ketaminecorsub[, c(6,13)]
chart.Correlation(without67, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

withoutmore<-ketaminecorsub[, c(3,13)]   
chart.Correlation(withoutmore, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS") ##0.98
cor(c(15,12,14,13),c(0.411,0.293,0.303,0.241))  ##0.752
#new<-data.frame(ageonset=c(15,12,14,13),perchange=c(0.411,0.293,0.303,0.241))
#chart.Correlation(new, histogram=TRUE, pch=19)

withouttxs<-ketaminecorsub[, c(4,13)]
chart.Correlation(withouttxs, histogram=TRUE, method="spearman", main="Spearman Correlation with percent change in CDRS")

##iq_2, BMI ## ???
ketaminecorsub$iq<-ketamine["iq_2"]

height<-ketamine["height"]
weight<-ketamine["weight"]
ketaminecorsub$BMI<-weight*0.45/((height*0.025)^2)
View(ketaminecorsub)
ketamineiqbmi<-ketaminecorsub[,13:15]
View(ketamineiqbmi)
#ketamineiqbmi<-ketamineiqbmi[!is.na(ketamineiqbmi[,])]

iq<-ketamineiqbmi[c(1:5,7,8), c(1,2)]
colnames(iq)
cor(iq$perchange,iq$iq)  #-0.1936
iqmatrix<-matrix(as.numeric(unlist(iq)),nrow=nrow(iq))  ##good##
rcorr(iqmatrix)  ##p.value=0.6774

bmi<-ketamineiqbmi[c(1:5,7,8,9), c(1,3)]
colnames(bmi)
cor(bmi$perchange,bmi$BMI) ##0.4678
bmimatrix<-matrix(as.numeric(unlist(bmi)),nrow=nrow(bmi))
rcorr(bmimatrix) ##p.value=0.2425

#######################################
cor(na.omit(ketaminecorsub[,12:13])) ##0.387 cadss_ob
cadssobmatrix<-matrix(as.numeric(unlist(na.omit(ketaminecorsub[,12:13]))),nrow=nrow(na.omit(ketaminecorsub[,12:13])))
rcorr(cadssobmatrix)  ##p.value=0.3434

cor(na.omit(ketaminecorsub[,c(11,13)])) ##0.112
cor(na.omit(ketaminecorsub[,c(10,13)])) #-0.243

cor(na.omit(ketaminecorsub[,c(9,13)]))  #0.423
tepsantimatrix<-matrix(as.numeric(unlist(na.omit(ketaminecorsub[,c(9,13)]))),nrow=nrow(na.omit(ketaminecorsub[,c(9,13)])))
rcorr(tepsantimatrix)  ##p.value=0.345

cor(na.omit(ketaminecorsub[,c(8,13)]))  #0.527
tepsmatrix<-matrix(as.numeric(unlist(na.omit(ketaminecorsub[,c(8,13)]))),nrow=nrow(na.omit(ketaminecorsub[,c(8,13)])))
rcorr(tepsmatrix)  ##p.value=0.2247

cor(na.omit(ketaminecorsub[,c(7,13)]))  #-0.147
cor(na.omit(ketaminecorsub[,c(6,13)]))  #0.356
cor(na.omit(ketaminecorsub[,c(5,13)]))  #0.001

cor(na.omit(ketaminecorsub[,c(4,13)]))  #-0.488  txstried
txstriedmatrix<-matrix(as.numeric(unlist(na.omit(ketaminecorsub[,c(4,13)]))),nrow=nrow(na.omit(ketaminecorsub[,c(4,13)])))
rcorr(txstriedmatrix) ##0.3259

cor(na.omit(ketaminecorsub[,c(3,13)]))  #0.981  ageonset
ageonsetmatrix<-matrix(as.numeric(unlist(na.omit(ketaminecorsub[,c(3,13)]))),nrow=nrow(na.omit(ketaminecorsub[,c(3,13)])))
rcorr(ageonsetmatrix)   ##0.0032

cor(na.omit(ketaminecorsub[,c(2,13)]))  #0.057  agebaseline
agematrix<-matrix(as.numeric(unlist(na.omit(ketaminecorsub[,c(2,13)]))),nrow=nrow(na.omit(ketaminecorsub[,c(2,13)])))
rcorr(agematrix)  #0.8934

cor(na.omit(ketaminecorsub[,c(1,13)]))  #0.114 sex?

