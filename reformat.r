###### RA Project #####
###### Ketamine #######

## read dataset ##
ketamine <- read.csv('D:/Biostatistics/2016FallRA/ketamine.csv',header=T, sep=',')
head(ketamine)
nrow(ketamine)
ncol(ketamine)
summary(ketamine)
ketamine[,15:24]
ketamine$height
ketamine$weight

## BMI ##
BMI <- function(ketamine){
  a<-ketamine$height
  b<-ketamine$weight
  bmi<-b*0.45/((a*0.025)^2)
  print(bmi)
}


## mean at baseline ##
mean(ketamine$age_at_baseline)
mean(as.numeric(as.character(ketamine$depression_age_of_onset[ketamine$depression_age_of_onset!="NULL"])))
mean(as.numeric(as.character(ketamine$depression_txs_tried[ketamine$depression_txs_tried!="NULL"])))
mean(as.numeric(as.character(ketamine$iq_2[ketamine$iq_2!="NULL"])))
mean(bmi)

## sd at baseline ##
sd(ketamine$age_at_baseline)
sd(as.numeric(as.character(ketamine$depression_age_of_onset[ketamine$depression_age_of_onset!="NULL"])))
sd(as.numeric(as.character(ketamine$depression_txs_tried[ketamine$depression_txs_tried!="NULL"])))
sd(as.numeric(as.character(ketamine$iq_2[ketamine$iq_2!="NULL"])))
sd(bmi)

## Table 1. Baseline Characteristics ##
Variable <- c("Age-yr","Gender(Female)no.%","IQ","BMI","Depression age of onset","Treatment tried")
Mean<-c(16.8,2,113.6,31.4,11.6,5.6)
SD<-c(1.0,22.2,10.9,6.5,4.4,3.4)
table1<-data.frame(Variable,Mean,SD)
library(knitr)
kable(table1, caption = "Table 1. Baseline Characteristics")


