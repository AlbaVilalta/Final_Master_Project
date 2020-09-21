####################################
####### FINAL MASTER PROJECT #######
####################################

###################
# Correlation study
###################

## Load libraries

library(compareGroups)
library(foreign)
library(plyr)

## Load data

setwd("F:/TFM/Alba/Hormones")
base1    <- read.spss("0. Bases de dades/BABLE_OCT_2019.sav",to.data.frame=TRUE, use.value.labels = FALSE)
hormones <- read.spss("0. Bases de dades/Hormones COS.sav",to.data.frame=TRUE, use.value.labels = FALSE)
base <- join(base1, hormones, by = "HISTORIA", type = "inner", match = "all")
base$TMX<- factor(base$TMX, levels=c("0","1"), labels=c("NO","YES"))
base$RTX<- factor(base$RTX, levels=c("0","1"), labels=c("NO","YES"))
base$QIMO<-factor(base$QIMO,levels=c("0","1"), labels=c("NO","YES"))
attach(base)

## Characteristics table (median)

# Baseline characteristics table
taula <- compareGroups( ~ DOLOR_EAV0+ECOS_0+BMDL_0+BMDCF_0+BMDFT_0+Dione4_BASAL+Testo_BASAL+E1_BASAL+E2_BASAL+
                          E1.3G_BASAL+E1.3S_BASAL+TotalE1_BASAL+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,
                        data = base, method = c(MENARQUIA=2,FILLS=2,LACT=2,DOLOR_EAV0=2,ECOS_0=2,
                                                Dione4_BASAL=2,Testo_BASAL=2,E1_BASAL=2,E2_BASAL=2,
                                                E1.3G_BASAL=2,E1.3S_BASAL=2,TotalE1_BASAL=2))
taula_basal_horm <- createTable(taula)
taula_basal_horm
export2html(taula_basal_horm, file = "Taula_Basal_Horm.html")

# 3 months characteristics table 
taula <- compareGroups( ~ DOLOR_EAV3+ECOS_3+Dione4_3M+Testo_3M+E1_3M+E2_3M+E1.3G_3M+E1.3S_3M+TotalE1_3M,data = base, 
                        method = 2)
taula_basal_horm <- createTable(taula)
taula_basal_horm

# 12 months characteristics table
taula <- compareGroups( ~ DOLOR_EAV12+ECOS_12,data = base,method = 2)
taula_basal_horm <- createTable(taula)
taula_basal_horm

## Distributions

# Basal distributions
basal <- compareGroups( ~ DOLOR_EAV0 + ECOS_0 + Dione4_BASAL + Testo_BASAL + E1_BASAL + E2_BASAL +
                          E1.3G_BASAL + E1.3S_BASAL + TotalE1_BASAL, data = base, method = c(DOLOR_EAV0=2,ECOS_0=2))

EAV_B <- plot(basal[1])
ECOS_B<- plot(basal[2]) 
Dione4_B <- plot(basal[3])
Testo_B <- plot(basal[4])
E1_B <- plot(basal[5])
E2_B <- plot(basal[6])
E1.3G_B <- plot(basal[7])
E1.3S_B <- plot(basal[8])
TotalE1_B <- plot(basal[9])

# Separated by batches
Tanda_1 <- base[with(base, Tanda==1),]
Tanda_2 <- base[with(base, Tanda==2),]

basal_tanda_1 <- compareGroups( ~ DOLOR_EAV0 + ECOS_0 + Dione4_BASAL + Testo_BASAL + E1_BASAL + E2_BASAL +
                                  E1.3G_BASAL + E1.3S_BASAL + TotalE1_BASAL, data = Tanda_1, 
                                method = c(DOLOR_EAV0=2,ECOS_0=2))

basal_tanda_2 <- compareGroups( ~ DOLOR_EAV0 + ECOS_0 + Dione4_BASAL + Testo_BASAL + E1_BASAL + E2_BASAL +
                                  E1.3G_BASAL + E1.3S_BASAL + TotalE1_BASAL, data = Tanda_2, 
                                method = c(DOLOR_EAV0=2,ECOS_0=2))

# Batch 1
EAV_BT_1 <- plot(basal_tanda_1[1])
ECOS_BT_1 <- plot(basal_tanda_1[2]) 
Dione4_BT_1 <- plot(basal_tanda_1[3])
Testo_BT_1 <- plot(basal_tanda_1[4])
E1_BT_1 <- plot(basal_tanda_1[5])
E2_BT_1 <- plot(basal_tanda_1[6])
E1.3G_BT_1 <- plot(basal_tanda_1[7])
E1.3S_BT_1 <- plot(basal_tanda_1[8])

# Batch 2
EAV_BT_2 <- plot(basal_tanda_2[1])
ECOS_BT_2 <- plot(basal_tanda_2[2]) 
Dione4_BT_2 <- plot(basal_tanda_2[3])
Testo_BT_2 <- plot(basal_tanda_2[4])
E1_BT_2 <- plot(basal_tanda_2[5])
E2_BT_2 <- plot(basal_tanda_2[6])
TotalE1_BT_2 <- plot(basal_tanda_2[9])

## Differences between batches HORMONES

# Baseline
wilcox.test(Tanda_2$DOLOR_EAV0,Tanda_1$DOLOR_EAV0, paired = FALSE) # p-value = 0.4317  
wilcox.test(Tanda_2$ECOS_0,Tanda_1$ECOS_0, paired = FALSE) # p-value = 0.4556
wilcox.test(Tanda_2$Dione4_BASAL,Tanda_1$Dione4_BASAL, paired = FALSE) # p-value = 0.06319 
wilcox.test(Tanda_2$Testo_BASAL,Tanda_1$Testo_BASAL, paired = FALSE) # p-value = 0.1755 
wilcox.test(Tanda_2$E1_BASAL,Tanda_1$E1_BASAL, paired = FALSE) # p-value = 1.798e-07
wilcox.test(Tanda_2$E2_BASAL,Tanda_1$E2_BASAL, paired = FALSE) # p-value = 0.3846

# 3 months
wilcox.test(Tanda_2$DOLOR_EAV3,Tanda_1$DOLOR_EAV3, paired = FALSE) # p-value = 0.6191  
wilcox.test(Tanda_2$ECOS_3,Tanda_1$ECOS_3, paired = FALSE) # p-value = 0.3512
wilcox.test(Tanda_2$Dione4_3M,Tanda_1$Dione4_3M, paired = FALSE) # p-value = 0.0003848 
wilcox.test(Tanda_2$Testo_3M,Tanda_1$Testo_3M, paired = FALSE) # p-value = 0.8415 
wilcox.test(Tanda_2$E1_3M,Tanda_1$E1_3M, paired = FALSE) # p-value = 2.104e-10
wilcox.test(Tanda_2$E2_3M,Tanda_1$E2_3M, paired = FALSE) # p-value = 0.1594

## Diferences between baseline and 3M HORMONES

wilcox.test(base$Testo_BASAL, base$Testo_3M, paired = TRUE) # p-value = 0.0002059
wilcox.test(base$Dione4_BASAL, base$Dione4_3M, paired = TRUE) # p-value = 8.434e-05
wilcox.test(base$E1_BASAL, base$E1_3M, paired = TRUE) # p-value < 2.2e-16 
wilcox.test(base$E2_BASAL, base$E2_3M, paired = TRUE) # p-value = 2.58e-16 
wilcox.test(base$E1.3G_BASAL, base$E1.3G_3M, paired = TRUE) # p-value = 0.0002946 
wilcox.test(base$E1.3S_BASAL, base$E1.3S_3M, paired = TRUE) # p-value = 3.245e-09 
wilcox.test(base$TotalE1_BASAL, base$TotalE1_3M, paired = TRUE) # p-value = 1.708e-08

## Diferences between EAV/ECOS at baseline and 3M

wilcox.test(base$DOLOR_EAV0, base$DOLOR_EAV3, paired = TRUE) # p-value = 0.0165
wilcox.test(base$ECOS_0, base$ECOS_3, paired = TRUE) # p-value = 2.136e-05

## Diferences between EAV/ECOS at baseline and 12M

wilcox.test(base$DOLOR_EAV0, base$DOLOR_EAV12, paired = TRUE) # p-value = 0.005035
wilcox.test(base$ECOS_0, base$ECOS_12, paired = TRUE) # p-value = 6.622e-05

## Linear Regression

# Absolute VAS/ECOS
base$A3_VAS  <- (base$DOLOR_EAV3-base$DOLOR_EAV0)
base$A3_ECOS <- (base$ECOS_3-base$ECOS_0)

base$A12_VAS  <- (base$DOLOR_EAV12-base$DOLOR_EAV0)
base$A12_ECOS <- (base$ECOS_12-base$ECOS_0)

# Relative HORMONES
base$R_Dione4<- ((base$Dione4_3M- base$Dione4_BASAL)/base$Dione4_BASAL)*100
base$R_Testo <- ((base$Testo_3M - base$Testo_BASAL)/base$Testo_BASAL)*100
base$R_E1    <- ((base$E1_3M - base$E1_BASAL)/base$E1_BASAL)*100
base$R_E2    <- ((base$E2_3M - base$E2_BASAL)/base$E2_BASAL)*100
base$R_E1.3G <- ((base$E1.3G_3M - base$E1.3G_BASAL)/base$E1.3G_BASAL)*100
base$R_E1.3S <- ((base$E1.3S_3M - base$E1.3S_BASAL)/base$E1.3S_BASAL)*100
base$R_TotalE1 <- ((base$TotalE1_3M - base$TotalE1_BASAL)/base$TotalE1_BASAL)*100
base$R_E1[sapply(base$R_E1, is.infinite)] <- NA

## Correlation HORMONES - EVA 0-3

# CRU
lmcru_Dione4<- lm(A3_VAS ~ R_Dione4, data=base,na.exclude = TRUE);summary(lmcru_Dione4) # p-value: 0.9878
lmcru_Testo	<- lm(A3_VAS  ~ R_Testo, data=base,na.exclude = TRUE);summary(lmcru_Testo) # p-value: 0.8141
lmcru_E1	<- lm(A3_VAS ~ R_E1, 	  data=base,na.exclude = TRUE);summary(lmcru_E1) # p-value: 0.4123
lmcru_E2	<- lm(A3_VAS ~ R_E2, 	  data=base,na.exclude = TRUE);summary(lmcru_E2) # p-value: 0.9425
lmcru_E1.3G	<- lm(A3_VAS ~ R_E1.3G,  data=base,na.exclude = TRUE);summary(lmcru_E1.3G) # p-value: 0.4556
lmcru_E1.3S	<- lm(A3_VAS ~ R_E1.3S,  data=base,na.exclude = TRUE);summary(lmcru_E1.3S) # p-value: 0.1658
lmcru_TotalE1<-lm(A3_VAS ~ R_TotalE1,  data=base,na.exclude = TRUE);summary(lmcru_TotalE1) # p-value: 0.3868

# ADJ
lmadj_Dione4<- lm(A3_VAS ~ R_Dione4+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T,data=base); summary(lmadj_Dione4) # p-value: 0.2688
lmadj_Testo	<- lm(A3_VAS  ~ R_Testo+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_Testo) # p-value: 0.2634
lmadj_E1	<- lm(A3_VAS ~ R_E1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1) # p-value: 0.1023
lmadj_E2	<- lm(A3_VAS ~ R_E2+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E2) # p-value: 0.1549
lmadj_E1.3G	<- lm(A3_VAS ~ R_E1.3G+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3G) # p-value: 0.0597
lmadj_E1.3S	<- lm(A3_VAS ~ R_E1.3S+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3S) # p-value: 0.09877
lmadj_TotalE1<-lm(A3_VAS ~ R_TotalE1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_TotalE1) # p-value: 0.1791

# Adjusted OR and 95% CI
exp(cbind("Odds ratio" = coef(lmadj_Dione4), confint.default(lmadj_Dione4, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_Testo), confint.default(lmadj_Testo, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1), confint.default(lmadj_E1, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E2), confint.default(lmadj_E2, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3G), confint.default(lmadj_E1.3G, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3S), confint.default(lmadj_E1.3S, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_TotalE1), confint.default(lmadj_TotalE1, level = 0.95)))

## Correlation HORMONES - ECOS 0-3

#CRU
lmcru_Dione4<- lm(A3_ECOS ~ R_Dione4, data=base,na.exclude = TRUE);summary(lmcru_Dione4) # p-value: 0.1911
lmcru_Testo	<- lm(A3_ECOS  ~ R_Testo, data=base,na.exclude = TRUE);summary(lmcru_Testo) # p-value: 0.6164
lmcru_E1	<- lm(A3_ECOS ~ R_E1, data=base,na.exclude = TRUE);summary(lmcru_E1) # p-value: 0.5955
lmcru_E2	<- lm(A3_ECOS ~ R_E2, data=base,na.exclude = TRUE);summary(lmcru_E2) # p-value: 0.9294
lmcru_E1.3G	<- lm(A3_ECOS ~ R_E1.3G, data=base,na.exclude = TRUE);summary(lmcru_E1.3G) # p-value: 0.6829
lmcru_E1.3S	<- lm(A3_ECOS ~ R_E1.3S, data=base,na.exclude = TRUE);summary(lmcru_E1.3S) # p-value: 0.2565
lmcru_TotalE1<-lm(A3_ECOS ~ R_TotalE1, data=base,na.exclude = TRUE);summary(lmcru_TotalE1) # p-value: 0.4663

#ADJ
lmadj_Dione4<- lm(A3_ECOS ~ R_Dione4+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T,data=base); summary(lmadj_Dione4) # p-value: 0.5594
lmadj_Testo	<- lm(A3_ECOS  ~ R_Testo+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_Testo) # p-value: 0.6482
lmadj_E1	<- lm(A3_ECOS ~ R_E1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1) # p-value: 0.6382
lmadj_E2	<- lm(A3_ECOS ~ R_E2+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E2) # p-value: 0.8334
lmadj_E1.3G	<- lm(A3_ECOS ~ R_E1.3G+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3G) # p-value: 0.08108
lmadj_E1.3S	<- lm(A3_ECOS ~ R_E1.3S+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3S) # p-value: 0.05948
lmadj_TotalE1<-lm(A3_ECOS ~ R_TotalE1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_TotalE1) # p-value: 0.01688

# Adjusted OR and 95% CI
exp(cbind("Odds ratio" = coef(lmadj_Dione4), confint.default(lmadj_Dione4, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_Testo), confint.default(lmadj_Testo, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1), confint.default(lmadj_E1, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E2), confint.default(lmadj_E2, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3G), confint.default(lmadj_E1.3G, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3S), confint.default(lmadj_E1.3S, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_TotalE1), confint.default(lmadj_TotalE1, level = 0.95)))

## Correlation HORMONES - EVA 0-12

# CRU
lmcru_Dione4<- lm(A12_VAS ~ R_Dione4, data=base,na.exclude = TRUE);summary(lmcru_Dione4) # p-value: 0.3651
lmcru_Testo	<- lm(A12_VAS  ~ R_Testo, data=base,na.exclude = TRUE);summary(lmcru_Testo) # p-value: 0.901
lmcru_E1	<- lm(A12_VAS ~ R_E1, 	  data=base,na.exclude = TRUE);summary(lmcru_E1) # p-value: 0.3648
lmcru_E2	<- lm(A12_VAS ~ R_E2, 	  data=base,na.exclude = TRUE);summary(lmcru_E2) # p-value: 0.1124
lmcru_E1.3G	<- lm(A12_VAS ~ R_E1.3G,  data=base,na.exclude = TRUE);summary(lmcru_E1.3G) # p-value: 0.05647
lmcru_E1.3S	<- lm(A12_VAS ~ R_E1.3S,  data=base,na.exclude = TRUE);summary(lmcru_E1.3S) # p-value: 0.3971
lmcru_TotalE1<-lm(A12_VAS ~ R_TotalE1,  data=base,na.exclude = TRUE);summary(lmcru_TotalE1) # p-value: 0.8293

# ADJ
lmadj_Dione4<- lm(A12_VAS ~ R_Dione4+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T,data=base); summary(lmadj_Dione4) # p-value: 0.7903
lmadj_Testo	<- lm(A12_VAS  ~ R_Testo+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_Testo) # p-value: 0.8915
lmadj_E1	<- lm(A12_VAS ~ R_E1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1) # p-value: 0.7672
lmadj_E2	<- lm(A12_VAS ~ R_E2+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E2) # p-value: 0.7116
lmadj_E1.3G	<- lm(A12_VAS ~ R_E1.3G+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3G) # p-value: 0.1876
lmadj_E1.3S	<- lm(A12_VAS ~ R_E1.3S+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3S) # p-value: 0.9013
lmadj_TotalE1<-lm(A12_VAS ~ R_TotalE1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_TotalE1) # p-value: 0.7399

# Adjusted OR and 95% CI
exp(cbind("Odds ratio" = coef(lmadj_Dione4), confint.default(lmadj_Dione4, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_Testo), confint.default(lmadj_Testo, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1), confint.default(lmadj_E1, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E2), confint.default(lmadj_E2, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3G), confint.default(lmadj_E1.3G, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3S), confint.default(lmadj_E1.3S, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_TotalE1), confint.default(lmadj_TotalE1, level = 0.95)))

## Correlation HORMONES - ECOS 0-12

#CRU
lmcru_Dione4<- lm(A12_ECOS ~ R_Dione4, data=base,na.exclude = TRUE);summary(lmcru_Dione4) # p-value: 0.1911
lmcru_Testo	<- lm(A12_ECOS  ~ R_Testo, data=base,na.exclude = TRUE);summary(lmcru_Testo) # p-value: 0.6164
lmcru_E1	<- lm(A12_ECOS ~ R_E1, data=base,na.exclude = TRUE);summary(lmcru_E1) # p-value: 0.5955
lmcru_E2	<- lm(A12_ECOS ~ R_E2, data=base,na.exclude = TRUE);summary(lmcru_E2) # p-value: 0.9294
lmcru_E1.3G	<- lm(A12_ECOS ~ R_E1.3G, data=base,na.exclude = TRUE);summary(lmcru_E1.3G) # p-value: 0.6829
lmcru_E1.3S	<- lm(A12_ECOS ~ R_E1.3S, data=base,na.exclude = TRUE);summary(lmcru_E1.3S) # p-value: 0.2565
lmcru_TotalE1<-lm(A12_ECOS ~ R_TotalE1, data=base,na.exclude = TRUE);summary(lmcru_TotalE1) # p-value: 0.4663

#ADJ
lmadj_Dione4<- lm(A12_ECOS ~ R_Dione4+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T,data=base); summary(lmadj_Dione4) # p-value: 0.6879
lmadj_Testo	<- lm(A12_ECOS  ~ R_Testo+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_Testo) # p-value: 0.828
lmadj_E1	<- lm(A12_ECOS ~ R_E1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1) # p-value: 0.5175
lmadj_E2	<- lm(A12_ECOS ~ R_E2+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E2) # p-value: 0.9467
lmadj_E1.3G	<- lm(A12_ECOS ~ R_E1.3G+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3G) # p-value: 0.1217
lmadj_E1.3S	<- lm(A12_ECOS ~ R_E1.3S+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_E1.3S) # p-value: 0.2086
lmadj_TotalE1<-lm(A12_ECOS ~ R_TotalE1+EDAT+BMI+MENARQUIA+EDAT_MENO+FILLS+LACT+TMX+RTX+QIMO,na.exclude=T, data=base);summary(lmadj_TotalE1) # p-value: 0.781

# Adjusted OR and 95% CI
exp(cbind("Odds ratio" = coef(lmadj_Dione4), confint.default(lmadj_Dione4, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_Testo), confint.default(lmadj_Testo, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1), confint.default(lmadj_E1, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E2), confint.default(lmadj_E2, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3G), confint.default(lmadj_E1.3G, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_E1.3S), confint.default(lmadj_E1.3S, level = 0.95)))
exp(cbind("Odds ratio" = coef(lmadj_TotalE1), confint.default(lmadj_TotalE1, level = 0.95)))

###################
# Persistence study
###################

## Load libraries

library(splines)			
library(survival)			
library(foreign)			
library(survminer)		
library(compareGroups)

## Load data

setwd("F:/TFM/Alba/Tamoxifè")
kmTMX2_BASE <- read.csv("kmTMX2_base.csv", sep = ",", header = TRUE)
kmTMX2_BASE$temps_tamox[kmTMX2_BASE$temps_tamox>60] <- 60 

kmIA2<-read.spss("kmIA2.sav",to.data.frame=TRUE, use.value.labels = FALSE)
kmIA2<-kmIA2[kmIA2$mesInici6==0,]		
kmIA2$tab[kmIA2$tab==" "]<- NA
kmIA2$temps_IA[kmIA2$temps_IA>60] <- 60

## Basal charactersitics TAM (ACTIVES)

TAM_ACTIVES <- kmTMX2_BASE[kmTMX2_BASE$Censura==0,]

TAM_ACTIVES$tab <- factor(TAM_ACTIVES$tab,levels=c("0","1","2"), labels=c("NO","YES","EX"))
TAM_ACTIVES$BP6 <- factor(TAM_ACTIVES$BP6, levels=c("0","1"), labels=c("NO","YES"))
TAM_ACTIVES$charlson <- factor(TAM_ACTIVES$charlson, levels = c("0","1","2","3","4"), labels = c("C0","C1","C2","C3","C4"))

basal_10y <- compareGroups( ~ edat+IMC+tab+ruralitat+qmedea+situacio+charlson+BP6, data = TAM_ACTIVES)
taula_basal <- createTable(basal_10y)
taula_basal
export2html(taula_basal, file = "Taula_Basal_TAM.html")

## Basal charactersitics AI (ACTIVES)

AI_ACTIVES <- kmIA2[kmIA2$Censura==0,]

AI_ACTIVES$tab <- factor(AI_ACTIVES$tab,levels=c("0","1","2"), labels=c("NO","YES","EX"))
AI_ACTIVES$BP6 <- factor(AI_ACTIVES$BP6, levels=c("0","1"), labels=c("NO","YES"))
AI_ACTIVES$charlson <- factor(AI_ACTIVES$charlson, levels = c("0","1","2","3","4"), labels = c("C0","C1","C2","C3","C4"))

basal_10y <- compareGroups( ~ edat+IMC+tab+ruralitat+qmedea+situacio+charlson+BP6, data = AI_ACTIVES)
taula_basal <- createTable(basal_10y)
taula_basal
export2html(taula_basal, file = "Taula_Basal_TAM.html")

## Persistence TAM and AI together

library(data.table)
AI_ACTIVES$drug <- 1
TAM_ACTIVES$drug <- 0
TAM_AI <- rbindlist(list(AI_ACTIVES,TAM_ACTIVES))

surv_TMX_sit <- survfit(Surv(TAM_AI$temps_IA, TAM_AI$Cessation6==1) ~ TAM_AI$drug)
ggsurvplot(surv_TMX_sit, data = TAM_AI, size=1.2,censor=TRUE,xlab="Time (months)",
           ylab="Persistence to \n AHT treatment",legend="top",legend.labs=c("TAM","AI"), 
           palette = c("#DAA520","#DC143C"),linetype = c(1,1), legend.title = "Patients:", 
           break.time.by = 12, risk.table = TRUE, cumevents = TRUE,font.x=c(14),font.y=c(14),
           title = "Persistence to AHT at 5 years of active population")
summary(surv_TMX_sit)

# COX cru
cox_cru<-coxph(Surv(TAM_AI$temps_IA, TAM_AI$Cessation6==1) ~ TAM_AI$drug)
summary(cox_cru,conf.int=0.95)$conf.int

# COX adj
ADJ<-TAM_AI[,c(8,11,15,2,43,44,45,68)]
ADJ$charlson[ADJ$charlson>4] <-4
ADJ$charlson <- as.factor(ADJ$charlson)
ADJ$temporal <- as.character(ADJ$qmedea)
ADJ$temporal[ADJ$ruralitat == "R"] <- "R"
ADJ$temporal[ADJ$temporal=="  "] <- "U "
ADJ$ruralitat<-as.factor(as.character(ADJ$ruralitat))
ADJ$temporal <- factor(ADJ$temporal, ordered = FALSE)
ADJ$temporal = relevel(ADJ$temporal, ref = "U1") # U1 as a reference in temporal variable
cox_adj <- coxph(Surv(temps_IA, Cessation6==1) ~ drug + edat + temporal + charlson, data = ADJ)
summary(cox_adj,conf.int=0.95)$conf.int

## Persistence TAM BP YES/NO

surv_TMX <- survfit(Surv(temps_tamox, Cessation6==1) ~ BP6, data = TAM_ACTIVES)
for (i in c(12,24,36,48,60)){	print(summary(surv_TMX, times=i))}
ggsurvplot(surv_TMX, risk.table = TRUE, palette = c("#808000","#DAA520"), cumevents = TRUE, 
           break.time.by = 12, data = TAM_ACTIVES, 
           ylab="Persistence to \n TAM treatment",legend="top",legend.labs=c("Non-BP-users","BP-users"),
           title = "Persistence to TAM at 5 years \ndepending on BP of active users\n", font.x=c(14),
           font.y=c(14), legend.title="Patients:", xlab="Time (months)")

# cox Cru
cox_cru<-coxph(Surv(temps_tamox, Cessation6==1) ~ BP6, data = TAM_ACTIVES); (summary(cox_cru,conf.int=0.95)$conf.int)

# cox Adj
ADJ<-TAM_ACTIVES[,c(8,11,15,2,43,44,45,17)]
ADJ$charlson[ADJ$charlson>4] <-4
ADJ$charlson <- as.factor(ADJ$charlson)
ADJ$temporal <- as.character(ADJ$qmedea)
ADJ$temporal[ADJ$ruralitat == "R"] <- "R"
ADJ$temporal[ADJ$temporal=="  "] <- "U "
ADJ$ruralitat<-as.factor(as.character(ADJ$ruralitat))
ADJ$temporal <- factor(ADJ$temporal, ordered = FALSE)
ADJ$temporal = relevel(ADJ$temporal, ref = "U1") # U1 as a reference in temporal variable
cox_adj <- coxph(Surv(temps_tamox, Cessation6==1) ~ BP6 + edat + temporal + charlson, data = ADJ)
summary(cox_adj,conf.int=0.95)$conf.int

## Persistence AI BP YES/NO

surv_TMX <- survfit(Surv(temps_IA, Cessation6==1) ~ BP6, data = AI_ACTIVES)
for (i in c(12,24,36,48,60)){	print(summary(surv_TMX, times=i))}
ggsurvplot(surv_TMX, risk.table = TRUE, palette = c("#808000","#DC143C"), cumevents = TRUE, 
           break.time.by = 12, data = AI_ACTIVES, 
           ylab="Persistence to \n AI treatment",legend="top",legend.labs=c("Non-BP-users","BP-users"),
           title = "Persistence to AI at 5 years \ndepending on BP of active users\n", font.x=c(14),
           font.y=c(14), legend.title="Patients:", xlab="Time (months)")

# cox Cru
cox_cru<-coxph(Surv(temps_IA, Cessation6==1) ~ BP6, data = AI_ACTIVES)
summary(cox_cru,conf.int=0.95)$conf.int


# cox Adj
ADJ<-AI_ACTIVES[,c(8,11,15,2,43,44,45,17)]
ADJ$charlson[ADJ$charlson>4] <-4
ADJ$charlson <- as.factor(ADJ$charlson)
ADJ$temporal <- as.character(ADJ$qmedea)
ADJ$temporal[ADJ$ruralitat == "R"] <- "R"
ADJ$temporal[ADJ$temporal=="  "] <- "U "
ADJ$ruralitat<-as.factor(as.character(ADJ$ruralitat))
ADJ$temporal <- factor(ADJ$temporal, ordered = FALSE)
ADJ$temporal = relevel(ADJ$temporal, ref = "U1") # U1 as a reference in temporal variable
cox_adj <- coxph(Surv(temps_IA, Cessation6==1) ~ BP6 + edat + temporal + charlson, data = ADJ)
summary(cox_adj,conf.int=0.95)$conf.int
