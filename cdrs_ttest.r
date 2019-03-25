###### RA Project #####
###### Ketamine #######

## read dataset ##
ketamine <- read.csv('D:/Biostatistics/2016FallRA/ketamine.csv',header=T, sep=',')
head(ketamine)
nrow(ketamine)
ncol(ketamine)
summary(ketamine)



ketamine[,1:5]
ketamine$bl_cdrs_comb_tscore  ##incomplete observations ##

## create data frame for baseline cdrs and post cdrs ##
bl_cdrs_comb_tscore<-c(90,76,75,68,84,64.5,74)
grid_bl<-c(6977,7046,7097,7111,7309,7360,7544,NULL,NULL)
bl_cdrs<-cbind(grid_bl,bl_cdrs_comb_tscore)

ptx_cdrs_comb_tscore<-c(76,81,66,63,75,NULL,55,52,NULL)
grid_ptx<-c(6977,7046,7097,7111,7309,NULL,7544,7549,NULL)
ptx_cdrs<-cbind(grid_ptx,ptx_cdrs_comb_tscore)

## combine baseline and post cdrs by grid ##
diff_cdrs<-merge(bl_cdrs,ptx_cdrs,by.x="grid_bl",by.y="grid_ptx")
diff_cdrs$BMI<-c(34.9,29.3,35.9,30.7,18.7,37.2)
diff_cdrs$IQ<-c(118,119,116,127,97,100)
diff_cdrs$age<-c(17.7,16.8,14.5,16.9,17.3,17)
diff_cdrs$gender<-as.factor(c(1,1,1,2,1,1))
diff_cdrs$bl_madrs<-c(39,26,28,26,32,28)
diff_cdrs$diff<-diff_cdrs$bl_cdrs_comb_tscore-diff_cdrs$ptx_cdrs_comb_tscore

## one sample t-test for diff with Ho: mu=0 Ha=greater ##
t.test(diff_cdrs$diff, mu=0, alternative="greater", conf.level=0.95)

## summary statistic ##
summary(diff_cdrs)

## diagnostic of normal dist assumption ##
hist(diff_cdrs$diff)

## paired t.test ##
t.test(diff_cdrs$bl_cdrs_comb_tscore,diff_cdrs$ptx_cdrs_comb_tscore,paired=T)

## formula to get percent change of cdrs ##
perchange <- (diff_cdrs$bl_cdrs_comb_tscore-diff_cdrs$ptx_cdrs_comb_tscore)/(diff_cdrs$bl_cdrs_comb_tscore-17)

## Correlation analysis ##
cor(diff_cdrs$diff,diff_cdrs$bl_cdrs_comb_tscore)

## All Subsets Regression ##
fit0 <- lm(diff ~ bl_cdrs_comb_tscore,data=diff_cdrs)
# view results # 
summary(fit0)

fit1 <- lm(diff ~ bl_cdrs_comb_tscore + BMI,data=diff_cdrs)
summary(fit1)

fit2 <- lm(diff ~ bl_cdrs_comb_tscore + age,data=diff_cdrs)
summary(fit2)

fit3 <- lm(diff ~ bl_cdrs_comb_tscore + bl_madrs,data=diff_cdrs)
summary(fit3)

fit4 <- lm(diff ~ bl_madrs,data=diff_cdrs)
summary(fit4)



