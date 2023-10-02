# GLM Group 08
rm(list=ls())
install.packages("car")
install.packages("maxLik")
install.packages("DHARMa")
install.packages("vcd")
install.packages("aod")
install.packages("countreg", repos="http://R-Forge.R-project.org")
install.packages("MASS")
install.packages("dplyr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(stats)
library(ggplot2)

library(tidyverse)
library(DHARMa)
library(knitr)
library(jtools)
library(car)
library(maxLik)
library(vcd)
library(countreg)
library(aod)
# Read Data
df <- read.table(file = file.choose(), header = TRUE)
head(df)
df$tto=as.factor(df$tto)
df$gender=as.factor(df$gender)
#EDA
#Exploratory Data Analysis
TotalFalls <- sum(Data$Y)
Meanfall <- data.frame(unclass(mean(Data$Y)), check.names = FALSE)

SortedDatabyGender <- Data%>%
  arrange(Data$gender, Data$BI)

SortedDatabyIntervention <- Data%>%
  arrange(Data$tto, Data$Y)

EducationOnly <- SortedDatabyIntervention[1:50,]
EducationPlusAerobic <- SortedDatabyIntervention[51:100,]
EducationOnly_Summary <- data.frame(unclass(summary(EducationOnly$Y)), check.names = FALSE)
EducationPlusAerobic_Summary <- data.frame(unclass(summary(EducationPlusAerobic$Y)), check.names = FALSE)


TotalFallsbyGender<- Data %>%
  group_by(Data$gender)%>%
  summarize(gendercount = n(), totalfalls= sum(Y)) %>%
  mutate(PercentageFallbyGender=(TotalFallsbyGender$totalfalls/304)*100)

GenderCountperFall<- Data %>%
  group_by(Data$gender, Data$Y)%>%
  summarize(count = n(), total= sum(Y))

FemaleData <- SortedDatabyGender[1:47,]
MaleData <- SortedDatabyGender[48:100,]
MeanforMale <- data.frame(unclass(mean(MaleData$Y)), check.names = FALSE)
MeanforFemale <- data.frame(unclass(mean(FemaleData$Y)), check.names = FALSE)

Female_Summary <- data.frame(unclass(summary(FemaleData)), check.names = FALSE)
Female_Statistics_BI <- data.frame(unclass(summary(FemaleData$BI)), check.names = FALSE)
Female_Statistics_SI <- data.frame(unclass(summary(FemaleData$SI)), check.names = FALSE)
Male_Summary <- data.frame(unclass(summary(MaleData)), check.names = FALSE)
Male_Statistics_BI <- data.frame(unclass(summary(MaleData$BI)), check.names = FALSE)
Male_Statistics_SI <- data.frame(unclass(summary(MaleData$SI)), check.names = FALSE)

dataforpiechart <- data.frame(group= c("Male", "Female"), value=c(46.4, 53.4))
ggplot(dataforpiechart, aes(x = "", y = value, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  labs(x = "", y = "", title = "Percentage fall across Gender \n",
       fill = "Gender") + 
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10)) 

AlltheMeans <- data.frame(Groups= c("Male", "Female", "Total"), Average=c(2.66, 3.47, 3.04))
ggplot(data=AlltheMeans, aes(x=Groups, y=Average)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Average), vjust=1.6, color="white", size=10)+
  labs(x = "", y = "", title = "Average fall across Gender \n")
theme_minimal()

ttoMeans <- data.frame(Groups= c("Eduacation Only", "Education plus Aerobics"), Average=c(4.52,1.56))
ggplot(data=ttoMeans, aes(x=Groups, y=Average)) +
  geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=Average), vjust=1.6, color="white", size=10)+
  labs(x = "", y = "", title = "Average Fall according to Interventions \n")
theme_minimal()

#Fitting Poisson model
model.ps <- glm(Y~tto+gender+BI+SI, data=df, family = poisson(link = 'log'))
summary(model.ps)
# Compare predictions to mean response 
small_df <- df[c('tto','gender', 'BI', 'SI')]
small_df$predicted <- predict(model.ps,small_df,type='response')
# Calculating risk ratio
glm.RR <- function(GLM.RESULT, digits = 2) {
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "Risk Ratio"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}
risk_ratio <- glm.RR(model.ps,3) 
as.data.frame(risk_ratio)

# Ratio of means of response
mu_withaero <- df %>% filter(tto==1) %>% summarise(mean(Y)) %>% unlist()
mu_eduonly <- df %>% filter(tto==0) %>% summarise(mean(Y)) %>% unlist()
mu_withaero/mu_eduonly

# Pearson Chi-Squared
X2.geriatric=sum(residuals(model.ps, type = "pearson")^2)
n.geriatric=dim(df)[1]
p.geriatric=length(coef(model.ps))
data.frame(X2s=X2.geriatric,pvalue=(1-pchisq(X2.geriatric,n.geriatric-p.geriatric)))
# Deviance goodness of fit
Dev.geriatric=summary(model.ps)$deviance
df.geriatric=summary(model.ps)$df.residual
data.frame(Dev=Dev.geriatric, df=df.geriatric, pvalue=(1-pchisq(Dev.geriatric,df.geriatric)))
## Likelihood ratio test
Anova(model.ps, test = "LR",type=3)
## Wald test
Anova(model.ps, test = "Wald",type=3)
# Simulating Residuals and rootgrams for Poisson
sim.model.ps <- simulateResiduals(model.ps, plot = T)
hist(sim.model.ps)
countreg::rootogram(model.ps, ylab='Root Square of Frequency',main='Poisson')
testUniformity(sim.model.ps)
testDispersion(sim.model.ps)
# Fitting Negative Binomial
model.nb <- MASS::glm.nb(Y~tto+gender+BI+SI, data=df)
summary(model.nb)
# Compare predictions to mean response 
small_df1 <- df[c('tto','gender', 'BI', 'SI')]
small_df1$predicted <- predict(model.nb,small_df1,type='response')
small_df1 %>% left_join(df %>% group_by(tto) %>% summarise(observed=mean(Y))) %>% mutate_if(is.numeric, round, 4)
# Calculating risk ratio
risk_ratio <- glm.RR(model.nb,4) 
as.data.frame(risk_ratio)
# Pearson Chi-Squared
X2.geriatric=sum(residuals(model.nb, type = "pearson")^2)
n.geriatric=dim(df)[1]
p.geriatric=length(coef(model.nb))
data.frame(X2s=X2.geriatric,pvalue=(1-pchisq(X2.geriatric,n.geriatric-p.geriatric)))
# Deviance goodness of fit
Dev.geriatric=summary(model.nb)$deviance
df.geriatric=summary(model.nb)$df.residual
data.frame(Dev=Dev.geriatric, df=df.geriatric, pvalue=(1-pchisq(Dev.geriatric,df.geriatric)))
## Likelihood ratio test
Anova(model.nb, test = "LR",type=3)
## Wald test
Anova(model.nb, test = "Wald",type=3)
# Simulating Residuals and rootgrams for negative binomial
countreg::rootogram(model.nb,ylab='Root Square of Frequency',main='Negative Binomial')
sim.model.nb <- simulateResiduals(model.nb, plot = T)
hist(sim.model.nb)
DHARMa::plotQQunif(sim.model.nb)
testUniformity(sim.model.ab)
testDispersion(sim.model.nb)
hist(sim.model.nb)
# Quassi-Likelihood
model.abq <- glm(Y~tto+gender+BI+SI, data=df, family = quasipoisson)
model.abq 
# Model Comparison 
round(data.frame(
  Po=coef(model.ps),QL=coef(model.abq),NB=coef(model.nb),
  se.Po=summary(model.ps)$coefficients[, 2],
  se.QL=summary(model.abq)$coefficients[, 2],
  se.NB=summary(model.nb)$coefficients[, 2]),4) 
 
## Model Comparison with AIC
AIC(model.ps,model.nb,model.abq)

