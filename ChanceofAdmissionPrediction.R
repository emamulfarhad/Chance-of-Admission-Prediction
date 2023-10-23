#Loading all the libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(mice)
library(MASS)
library(caret)
library(ROCR)
library(corrplot)
library(car)
library(GGally)

#Dataset load
adm_df <- read.csv("data/datasets_14872_228180_Admission_Predict.csv")

#check head, tail and data size
head(adm_df)
tail(adm_df)
dim(adm_df)

#summary of dataset
summary(adm_df)

#Serial number does not effect chance of admission
adm_df$Serial.No. = NULL


#checking null value
sum(is.na(adm_df))

#checking duplicate value
sum(duplicated(adm_df))

# structure of dataset
str(adm_df)

# Summary after removing serial number column
summary(adm_df)

# In one figure Scatter plot, Frequency curve and Correlation values in one graph
ggpairs(adm_df)

# View details Gre score
boxplot(adm_df$GRE.Score,col="blue",main="Box plot of GRE Score",horizontal=TRUE,xlab="GRE Score")

hist(adm_df$GRE.Score,col="orange",xlab="GRE Score",ylab="Frequency",
     main="Histogram for GRE Score",labels=TRUE)

plot(adm_df$GRE.Score,adm_df$Chance.of.Admit,col="green",main="GRE Score - Chance of Admission",
     xlab="GRE Score",ylab="Chance of Admission")

# view details TOEFL score
boxplot(adm_df$TOEFL.Score,col="blue",main="Boxplot for TOEFL Score",horizontal=TRUE,xlab="TOEFL Score")

hist(adm_df$TOEFL.Score,col="orange",xlab="TOEFL Score",ylab="Frequency",
     main="Histogram for TOEFL Score",labels=TRUE)

plot(adm_df$TOEFL.Score,adm_df$Chance.of.Admit,col="green",xlab="TOEFL Score",ylab="Chance of Admission",
     main="TOEFL Score - Chance of Admission")

#University Rating
boxplot(adm_df$University.Rating,col="blue",main="Boxplot for University rating",horizontal=TRUE,xlab="University rating")

hist(adm_df$University.Rating,col="orange",xlab="University rating",ylab="Frequency",
     main="Histogram for University rating",labels=TRUE)

plot(adm_df$University.Rating,adm_df$Chance.of.Admit,col="green",xlab="University rating",ylab="Chance of Admission",
     main="University rating - Chance of Admission")

#Statement of Purpose(SOP)
boxplot(adm_df$SOP,col="blue",main="Boxplot for SOP",horizontal=TRUE,xlab="SOP")

hist(adm_df$SOP,col="orange",xlab="SOP",ylab="Frequency",
     main="Histogram for SOP",labels=TRUE)

plot(adm_df$SOP,adm_df$Chance.of.Admit,col="green",xlab="SOP",ylab="Chance of Admission",
     main="SOP - Chance of Admission")

#Letter of Recommendation Strength
boxplot(adm_df$LOR,col="blue",main="Boxplot for Letter of Recommendation",horizontal=TRUE,xlab="LOR")

hist(adm_df$LOR,col="orange",xlab="LOR",ylab="Frequency",
     main="Histogram for LOR",labels=TRUE)

plot(adm_df$LOR,adm_df$Chance.of.Admit,col="green",xlab="LOR",ylab="Chance of Admission",
     main="LOR - Chance of Admission")

#CGPA
boxplot(adm_df$CGPA,col="blue",main="Boxplot for CGPA",horizontal=TRUE,xlab="CGPA")

hist(adm_df$CGPA,col="orange",xlab="CGPA",ylab="Frequency",
     main="Histogram for CGPA",labels=TRUE)

plot(adm_df$CGPA,adm_df$Chance.of.Admit,col="green",xlab="CGPA",ylab="Chance of Admission",
     main="CGPA - Chance of Admission")

#Research Strength
rese<-table(adm_df$Research)
barplot(rese,col=c("darkblue","red"),main="Barplot for Research",xlab="Research")

percen=round(rese/sum(rese)*100)
pleb=paste(c(" No(0)","Yes(1)"),percen,"%",sep=" ")
pie(rese,col=c("red","green"),labels=pleb, main="Research done or not done")

plot(adm_df$Research,adm_df$Chance.of.Admit,col="green",xlab="Research",ylab="Chance of Admission",
     main="Research - Chance of Admission")

#view correlation
corel<-cor(adm_df)
corrplot(corel,method = "number",type = "full")

# Train
set.seed(123)
train.size <- 0.8
train.index <- sample.int(length(adm_df$Chance.of.Admit), round(length(adm_df$Chance.of.Admit) * train.size))
train.sample <- adm_df[train.index,]
valid.sample <- adm_df[-train.index,]
names(adm_df)

# A stepwise selection of variables by backwards elimination
# By considering all candidate variables and eliminate one at the time
# Fit the model and summary - visualise by ploting
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
              SOP+LOR+CGPA+Research, data=train.sample)

# Summary
summary(fit)

#Two-dimensional confidence region plot
crPlots(fit)

# To eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("10", "93", "92")),]
#Re-Fit the Model 2
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
              SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("41", "66", "96")),]
#Re-Fit the Model 3
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("67", "11", "375")),]
#Re-Fit the Model 4
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("40", "104", "414")),]
#Re-Fit the Model 5
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("43", "64", "59")),]
#Re-Fit the Model 6
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("42", "116", "328")),]
#Re-Fit the Model 7
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("117", "359", "484")),]
#Re-Fit the Model 8
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("69", "458", "493")),]
#Re-Fit the Model 9
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+University.Rating+
                  SOP+LOR+CGPA+Research, data=train.sample)
summary(fit)

# Re-eliminate further extremes
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("57", "330", "376")),]

# Now we have seen there is no extreme value
# We will drop University.Rating because there is no significance for this variable
#Re-Fit the Model 10 as final model
fit <- lm(Chance.of.Admit ~ GRE.Score+ TOEFL.Score+
                  SOP+LOR+CGPA+Research, data=train.sample)

summary(fit)

#Two-dimensional confidence region plot
crPlots(fit)

#checking for multi-collinearity with Variance Inflation Factor
# correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)


#Thank you
# Hossain Md Farhad 1634661







# Evaluate the final model
# Find all predicted values for both a training set and a validation set
train.sample$pred.chance.adm <- predict(fit, 
                                        newdata = subset(train.sample, select=c(Chance.of.Admit,GRE.Score,TOEFL.Score,SOP,LOR,CGPA,Research)))
valid.sample$pred.chance.adm <- predict(fit, 
                                        newdata = subset(valid.sample, select=c(Chance.of.Admit,GRE.Score,TOEFL.Score,SOP,LOR,CGPA,Research)))

# The theoretical model performance is defined here as R-Squared
summary(fit)
# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$pred.chance.adm, train.sample$Chance.of.Admit), 2)
train.RMSE <- round(sqrt(mean((train.sample$pred.chance.adm - train.sample$Chance.of.Admit)^2)))
train.MAE <- round(mean(abs(train.sample$pred.chance.adm - train.sample$Chance.of.Admit)))
c(train.corr^2, train.RMSE, train.MAE)

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$pred.chance.adm, valid.sample$Chance.of.Admit), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$pred.chance.adm - valid.sample$Chance.of.Admit)^2)))
valid.MAE <- round(mean(abs(valid.sample$pred.chance.adm - valid.sample$Chance.of.Admit)))
c(valid.corr^2, valid.RMSE, valid.MAE)

# Thank you
# Hossain Md Farhad 1634661