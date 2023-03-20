####AlligtorStressManuscript####

setwd("/Users/kaitlynmurphy/Desktop")
getwd()

#install these packages
install.packages('lme4')
install.packages('nlme')
install.packages('ggplot2')
install.packages('tidyverse')

#call on these libraries
library(lme4)
library(nlme)
library(ggplot2)
library(tidyverse)

#Call on datasets
#NOTE: There are multiple used in this script

#Call on main dataset ("Data")
datum=read.csv(file.choose())
head(datum)

#Put body condition in dataset
resultsbc=lm(log(WEIGHTAFTER)~log(FINALTL),data=datum,na.action=na.omit)
summary(resultsbc)

datum4 <- datum[complete.cases(datum[,"WEIGHTAFTER"]),]
datum4$resids <- resid(resultsbc)
summary(datum4)

boxplot(resids~TREAT,datum4)
plot(resids~WEIGHTAFTER,datum4)

#separate by treatment groups 
high <- datum4[datum4$TREAT=="High",]
summary(high)
low <- datum4[datum4$TREAT=="Low",]
summary(low)
control <- datum4[datum4$TREAT=="Control",]
summary(control)

#Call on growth dataset ("Growth")
datum_growth=read.csv(file.choose())
head(datum_growth)

#Call on dataset for hormone figure ("Hormone_Long_Format")
#this dataset has a unique format and is thus separate

cortdatum=read.csv(file.choose())
head(cortdatum)

####ESTRADIOL####

resultsestra=lm(ESTRADIOL~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultsestra)
resultsestra1=lm(ESTRADIOL~relevel(TREAT,ref = "High")+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultsestra1)
plot(ESTRADIOL~TREAT,data=datum)

####CORT####

#Interaction term TREAT:ORDER and SEX:WEIGHTAFTER were non-significant and thus removed
results=lm(CORT~TREAT+ORDER+SEX+WEIGHTAFTER,data=datum,na.action=na.omit)
summary(results)
plot(CORT~TREAT,data=datum)

results1=lm(CORT~E2+ORDER+SEX+WEIGHTAFTER,data=datum,na.action=na.omit)
summary(results1)

#Figure with CORT and ESTRADIOL (Figure 2A)

cortdatum$TREAT_2 = factor(cortdatum$TREAT,c("Control", "Low", "High"))

tiff("Figure2A.tiff", width = 4, height = 4, units = 'in', res = 300)

ggplot(cortdatum, aes(x = TREAT_2, y = CONC, color = VALUE)) + 
  geom_boxplot() +
  #stat_boxplot(geom ='errorbar') +
  scale_color_manual(values=c("grey15", "grey60")) +
  theme_bw() +
  theme(legend.position="right") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
      axis.line.y = element_line(size = 0.5, colour = "black"),
      axis.line = element_line(size=1, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.text.x=element_text(colour="black", size = 10),
      axis.text.y=element_text(colour="black", size = 10)) +
  xlab("Treatment") +
  scale_y_continuous(name = "Plasma concentration (pg/mL)",
                     breaks = seq(0, 3600, 500),
                     limits=c(0, 3600))

dev.off()

####TITERS####

resultstiters=lm(TITER~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultstiters)
plot(TITER~TREAT,data=datum)

resultstiters1=lm(TITER~relevel(TREAT,ref = "High")+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultstiters1)

datum$TREAT_2 = factor(datum$TREAT,c("Control", "Low", "High"))

#Figure 2B

tiff("Figure2B.tiff", width = 4, height = 4, units = 'in', res = 300)

boxplot(TITER~TREAT_2, data=datum,
        col=(c("grey90", "grey60", "gray30")),
        xlab="Treatment", ylab="Antibody Titer",
        outline=FALSE)
stripchart(TITER~TREAT_2, vertical = TRUE, data = datum, 
           method = "jitter", add=TRUE, pch = 20)

dev.off()

####N:L####

resultshl=lm(H.L~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultshl)

resultshl1=lm(H.L~relevel(TREAT,ref = "High")+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultshl1)
plot(H.L~TREAT,data=datum)

datum$TREAT_2 = factor(datum$TREAT,c("Control", "Low", "High"))

#Figure 2C

tiff("Figure2C.tiff", width = 4, height = 4, units = 'in', res = 300)

boxplot(H.L~TREAT_2, data=datum,
        col=(c("grey90", "grey60", "gray30")),
        xlab="Treatment", ylab="N:L Ratio",
        outline=FALSE)
stripchart(H.L~TREAT_2, vertical = TRUE, data = datum, 
           method = "jitter", add=TRUE, pch = 20)

dev.off()

####MORPHOLOGY(HEAD LENGTH, ETC.)####

#Weight
resultsweight=lm(WEIGHTAFTER~TREAT+SEX,data=datum,na.action=na.omit)
summary(resultsweight)
plot(WEIGHTAFTER~TREAT,data=datum)

#Head length
#interaction term TREAT:WEIGHTAFTER not significant (removed from model)
resultshl=lm(FINALHL~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultshl)
plot(FINALHL~TREAT,data=datum)

#Total length
#interaction term TREAT:WEIGHTAFTER not significant (removed from model)
resultstl=lm(FINALTL~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultstl)
plot(FINALTL~TREAT,data=datum)

#Neck Girth
resultsng=lm(FINALNECKGIRTH~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultsng)
plot(FINALNECKGIRTH~TREAT,data=datum)

#Tail Girth
#interaction term TREAT:WEIGHTAFTER not significant (removed from model)
resultstg=lm(FINALTAILGIRTH~TREAT+WEIGHTAFTER+SEX,data=datum,na.action=na.omit)
summary(resultstg)
plot(FINALTAILGIRTH~TREAT,data=datum)

#Body Condition
resultsbc=lm(resids~TREAT+WEIGHTAFTER+SEX,data=datum4,na.action=na.omit)
summary(resultsbc)
resultsbc1=lm(resids~relevel(TREAT,ref = "High")+WEIGHTAFTER+SEX,data=datum4,na.action=na.omit)
summary(resultsbc1)

#body condition graph (Figure 3A)

tiff("Figure3A.tiff", width = 4, height = 6, units = 'in', res = 300)

ggplot() +
  geom_point(data=high, aes(x=WEIGHTBEFORE, y=resids), shape=17, color="grey60") +
  geom_point(data=low, aes(x=WEIGHTBEFORE, y=resids), shape=16, color = "grey40") +
  geom_point(data=control, aes(x=WEIGHTBEFORE, y=resids), shape=15, color= "grey15") +
  geom_smooth(data=high, aes(x=WEIGHTBEFORE, y=resids), method=lm,se=FALSE, color="grey60", linetype="dotted") +
  geom_smooth(data=low, aes(x=WEIGHTBEFORE, y=resids), method=lm,se=FALSE, color="grey40", linetype="dashed") +
  geom_smooth(data=control, aes(x=WEIGHTBEFORE, y=resids), method=lm,se=FALSE, color="grey15", linetype="solid") +
  theme_bw() +
  theme(legend.position="right") +
  scale_x_continuous(name = "Weight at Day 0 (g)",breaks=seq(60,160,20), limits=c(60,160)) +
  scale_y_continuous(name = "Body Condition (residuals)",
                     breaks = seq(-0.15, 0.16,.04),
                     limits=c(-0.15, 0.16)) +
  theme(axis.line.x = element_line(size = 0.75, colour = "black"),
        axis.line.y = element_line(size = 0.75, colour = "black"),
        axis.line = element_line(size=2, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(colour="black", size = 14),
        axis.text.y=element_text(colour="black", size = 14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))

dev.off()
boxplot(WEIGHTAFTER/FINALHL~TREAT, data=datum)

####GROWTH####

#Calculating body condition for this dataset
resultsbc=lm(log(MASS)~log(TL),data=datum_growth,na.action=na.omit)
summary(resultsbc)

datum_growth1 <- datum_growth[complete.cases(datum_growth[,"MASS"]),]
datum_growth1$resids <- resid(resultsbc)
summary(datum_growth1)

#Mass
resultslmemass=lme(MASS~TREATMENT+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmemass)

resultslmemass1=lme(MASS~relevel(TREATMENT,ref = "High")+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmemass1)

#Head Length
resultslmeHL=lme(HL~TREATMENT+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeHL)

resultslmeHL1=lme(HL~relevel(TREATMENT,ref = "High")+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeHL1)

#Total Length
resultslmeTL=lme(TL~TREATMENT+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeTL)

resultslmeTL1=lme(TL~relevel(TREATMENT,ref = "High")+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeTL1)

#Neck Girth
resultslmeNG=lme(NECKGIRTH~TREATMENT+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeNG)

resultslmeNG1=lme(NECKGIRTH~relevel(TREATMENT,ref = "High")+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeNG1)

#Tail Girth
resultslmeTG=lme(TAILGIRTH~TREATMENT+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeTG)

resultslmeTG1=lme(TAILGIRTH~relevel(TREATMENT,ref = "High")+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeTG1)

#Body Condition
resultsresids=lme(resids~TREATMENT+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultsresids)

resultslmeresids1=lme(resids~relevel(TREATMENT,ref = "High")+DD,random=~1|ID,data=datum_growth1,na.action=na.omit)
summary(resultslmeresids1)

#Figure 3B

resultsBC = datum_growth1 %>%
  group_by(factor(TREATMENT,c("Control", "Low", "High")),as.numeric(DD)) %>%
  summarize(mean = mean(resids),
            sd = sd(resids))
colnames(resultsBC) = c("TREATMENT", "DD", "MEAN", "SD")

pd = position_dodge(1)

tiff("Figure3B.tiff", width = 6, height = 8, units = 'in', res = 300)

ggplot(data=resultsBC, aes(x=DD, y=MEAN, color=TREATMENT, linetype=TREATMENT)) +
  geom_point(size=0.5) +
  geom_smooth(method=loess, se=TRUE) +
  scale_color_manual(values = c("grey15", "grey40", "grey60")) +
  scale_linetype_manual(values = c("solid","dashed", "twodash")) +
  scale_y_continuous(name = "Body Condition (residuals)") +
  scale_x_continuous(name = "Calendar Day") + 
  theme(axis.line.x = element_line(size = 0.75, colour = "black"),
        axis.line.y = element_line(size = 0.75, colour = "black"),
        axis.line = element_line(size=2, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(colour="black", size = 14),
        axis.text.y=element_text(colour="black", size =14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16)) +
  guides(color = guide_legend(title = "Treatment", title.hjust = 0.5, hjust = 0.5),
         shape = guide_legend(title = "Treatment", title.hjust = 0.5, hjust = 0.5),
         linetype = guide_legend(title = "Treatment", title.hjust = 0.5, hjust = 0.5)) 

dev.off()
