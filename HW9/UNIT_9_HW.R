fitmod <- lm(mpg ~ hp + wt, data=mtcars)
hpseq <- seq(50, 330, by=20)
wtseq <- seq(1.5, 5.4, length=15)
newdata <- expand.grid(hp=hpseq, wt=wtseq)
fit <- predict(fitmod, newdata)
fitmat <- matrix(fit, 15, 15)
image(hpseq, wtseq, fitmat, xlab="HP", ylab="WT")
library(bigsplines)
imagebar(hpseq, wtseq, fitmat, xlab="HP", ylab="WT", zlab="MPG", col=heat.colors(12), ncolor=12)


myfun <- function(x){
  2*sin(sqrt(x[,1]^2+x[,2]^2+.1))/sqrt(x[,1]^2+x[,2]^2+.1)
}
x <- expand.grid(seq(-8,8,l=100),seq(-8,8,l=100))
imagebar(seq(-8,8,l=100),seq(-8,8,l=100),matrix(myfun(x),100,100),
         xlab=expression(italic(x)[1]),ylab=expression(italic(x)[2]),
         zlab=expression(hat(italic(y))))

my_sample_col <- data.frame(sample = rep(c("tumour", "normal"), c(4,2)))
row.names(my_sample_col) <- colnames(data_subset)

my_sample_col <- data.frame(sample = rep(c("Control", "FECD_Rep", "FECD_NR", "Pre_S"), c(8,6,4,4)))

############################################# HW9


library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# question A. Clean and prepare the data:
# 1. Create column for brewery ID that is common to both datasets similar to what you did in the project. So we can merge!
  
#upload the two data files: "Beers.csv"" and "Breweries.csv"
beer <- read.csv("C:/Users/chu001/Documents/Yongjun-Chu files/SMU-data-science-application/Doing-Data_Science/Case_study_1/Beers.csv", sep = ',', header = T)
str(beer)
head(beer)
#convert all empty or space entries to NA
beer[beer=="" | beer == " "] <- NA
#apply(is.na(beer), 2, which)
sapply(beer, function(x) sum(is.na(x)))

brewery <- read.csv("C:/Users/chu001/Documents/Yongjun-Chu files/SMU-data-science-application/Doing-Data_Science/Case_study_1/Breweries.csv", sep = ',', header = T)
str(brewery)
head(brewery)
#convert all empty or space entries to NA
brewery[brewery == "" | brewery == " "] <- NA
sapply(brewery, function(x) sum(is.na(x)))

#remove the extra empty space before the state name in column State
brewery$State <- as.factor(gsub("^\\s+", "", brewery$State))
str(brewery)

colnames(beer)[1] <- "Name_beer"
colnames(beer)[5] <- "Brew_ID"
colnames(brewery)[2] <- "Name_brew"
total = merge(beer, brewery, by.x="Brew_ID", by.y="Brew_ID", all = T)
nrow(total)
head(total)
str(total)

#or use this commnad
total1 <- left_join(beer, brewery, by="Brew_ID")
nrow(total1)

#Create One Dataset that has only Colorado and Texas beers and no IBU NAs
beerCOTX <- total %>% dplyr::filter((State=="CO"|State=="TX") & !is.na(IBU))
nrow(beerCOTX)
sapply(beerCOTX, function(x) sum(is.na(x)))


#or use this following commands
beerCOTX1 <- total %>% filter(grepl("CO|TX", State))
nrow(beerCOTX1)
sapply(beerCOTX1, function(x) sum(is.na(x)))
beerCOTX2 <- beerCOTX1[complete.cases(beerCOTX1[ , 5]),]
sapply(beerCOTX2, function(x) sum(is.na(x)))
nrow(beerCOTX2)

#Order beerCOTX by IBU (ascending)
head(beerCOTX)
beerCOTX <- beerCOTX %>% dplyr::group_by(State) %>% dplyr::arrange(IBU)

#or using "order" command
beerCOTX <- beerCOTX[order(beerCOTX$IBU),]

p <- ggplot(beerCOTS, aes(y=ABV, x=IBU)) + geom_point() + facet_wrap(~State, nrow=2) + theme_minimal() 
p = p +
#  xlab("logFC (FECD_Rep/Con)") +
#  ylab("-log10(adjPValue)") +
  ggtitle(label="ABV vs IBU") + theme(plot.title=element_text(hjust=0.5)) +  theme(legend.position="right")
p


#question C: Model the data
beerTX <- beerCOTX %>% filter(State=="TX") %>% select(ABV, IBU, State)
beerCO <- beerCOTX %>% filter(State=="CO") %>% select(ABV, IBU, State)
plot(beerTX$IBU,beerTX$ABV, pch = 20, col = "red", xlab = "IBU", ylab="ABV", main = "IBU vs. ABV for TEXAS") #scatter plot of y versus x_1
fit = lm(ABV~IBU,data = beerTX) # fit the model to get Beta_hat_0 and Beta_hat_1
summary(fit) # view the parameter estimate table
confint(fit)
preds = predict(fit)
head(preds)
lines(beerTX$IBU,preds,type = "l",lwd = 4, col="blue")

plot(beerCO$IBU,beerCO$ABV, pch = 20, col = "red", xlab = "IBU", ylab="ABV", main = "IBU vs. ABV for COLORADO") #scatter plot of y versus x_1
fit = lm(ABV~IBU,data = beerCO) # fit the model to get Beta_hat_0 and Beta_hat_1
summary(fit) # view the parameter estimate table
confint(fit)
preds = predict(fit)
head(preds)
lines(beerCO$IBU,preds,type = "l",lwd = 4, col="black")

# Address the assumptions of the regression model

#1. Look at distribution of ys for a cross section of xs (conditiona on the xs).
beerTX %>% filter(IBU >19 & IBU < 21) %>% ggplot(aes(x = ABV)) + geom_histogram() #looking OK as normal
beerCO %>% filter(IBU >19 & IBU < 21) %>% ggplot(aes(x = ABV)) + geom_histogram() #not looking good for a normal distr.

#2. These normal distributions have equal standard deviations.  
#Look at the residuals for a cross section of the xs (conditional on the xs).
beerCO$resids = fit$residuals
beerCO %>% filter(IBU >18 & IBU < 22) %>% ggplot(aes(x = resids)) + geom_histogram() #looking not too bad
#standard deviation of residuals.  
sd((beerCO %>% filter(IBU >18 & IBU < 22))$resids)
sd((beerCO %>% filter(IBU >28 & IBU < 32))$resids)
#due to the limited data points, the SD of residuals at different points are not the same.

#3. The means of these normal distributions have a linear relationship with IBU.  
mean_value <- beerCO %>% group_by(IBU) %>% dplyr::summarise(mean_ABV=mean(ABV))
plot(mean_value$IBU, mean_value$mean_ABV, pch = 20, col = "red", xlab = "IBU", ylab="mean_ABV", main = "IBU vs. mean_ABV for CO") 

mean_value <- beerTX %>% group_by(IBU) %>% dplyr::summarise(mean_ABV=mean(ABV))
plot(mean_value$IBU, mean_value$mean_ABV, pch = 20, col = "red", xlab = "IBU", ylab="mean_ABV", main = "IBU vs. mean_ABV for TX") 

#both plots look pretty good in terms of the linearity between mean ABV on each IBU.

#4. Independence (you may assume this one to be true without defense.)  

#to answer this question better, we have to use ggfortigfy package
library(ggfortify)
sapply(beerTX, function(x) sum(is.na(x)))
nrow(beerTX)
fit = lm(ABV~IBU,data = beerTX) 
head(fit)
autoplot(fit, smooth.colour = NA)
#by examing the plots, it appears that normality is not met and equal variance is not met. There is a curveage in residual vs. fitted values plot and a funnel shape in standarized residual vs. fitted values. 

sapply(beerCO, function(x) sum(is.na(x)))
fit = lm(ABV~IBU,data = beerCO) 
nrow(beerCO)
autoplot(fit, smooth.colour = NA)
#by examing the plots, it appears that normality is not met and equal variance is not met. There is a curveage in residual vs. fitted values plot and a funnel shape in standarized residual vs. fitted values. 

#D. Gain inference from the model
#9
fit = lm(ABV~IBU,data = beerTX) # fit the model to get Beta_hat_0 and Beta_hat_1
summary(fit) # view the parameter estimate table
confint(fit)

fit = lm(ABV~IBU,data = beerCO) # fit the model to get Beta_hat_0 and Beta_hat_1
summary(fit) # view the parameter estimate table
confint(fit)

#there is no significant difference based on the 95% CI

#E. Compare two competing models: External Cross Validation

#11.Using the beerCOTX dataframe, add a column to the data that is the square of the IBU column, IBU2
beerCOTX$IBU2 <- (beerCOTX$IBU)^2
head(beerCOTX)

#12. For each state, create a training and test set from the data (60%/40% split respectively).  
#Print a summary of each new data frame. there should be four: TrainingCO, TestCO, TrainingTX, TestTX

#Divide into training and test set ... this one is 60% training 40% test
beerTX <- beerCOTX %>% filter(State=="TX")
beerCO <- beerCOTX %>% filter(State=="CO")
samplesize_TX=nrow(beerTX)
samplesize_CO=nrow(beerCO)

train_perc = 0.60
train_indices_TX = sample(seq(1,samplesize_TX,length = samplesize_TX),round(train_perc*samplesize_TX))
train_indices_CO = sample(seq(1,samplesize_CO,length = samplesize_CO),round(train_perc*samplesize_CO))

TrainingTX = beerTX[train_indices_TX,]
summary(TrainingTX)
TestTX = beerTX[-train_indices_TX,]
summary(TestTX)

TrainingCO = beerCO[train_indices_CO,]
summary(TrainingCO)
TestCO = beerCO[-train_indices_CO,]
summary(TestCO)

#13.  Brewmeisters are curious if the relationship between ABV and IBU is purely linear or quadratic.  

#result for TX
ASEholderTrain1_TX = c()
ASEholderTest1_TX = c()
ASEholderTrain2_TX = c()
ASEholderTest2_TX = c()

fitTrain1 = lm(ABV~IBU, data = TrainingTX)
autoplot(fitTrain1, smooth.colour = NA)
summary(fitTrain1)
# These are the predictions of the model on the data that were used to fit the model.
predsTrain1 = predict(fitTrain1)
# These are the predictions of the model on the data that were NOT used to fit the model.
# This is a better measure of how the model will perform in real life
predsTest1 = predict(fitTrain1, newdata = TestTX)
TestTX$preds1 <- predsTest1
head(TestTX)
# Calculation of the ASE for the Test set for one variable
ASEholderTest1_TX = sum((TestTX$preds1 - TestTX$ABV)^2)/(length(TestTX$ABV))
ASEholderTest1_TX

fitTrain2 = lm(ABV~IBU+IBU2, data = TrainingTX)
autoplot(fitTrain2, smooth.colour = NA)
summary(fitTrain2)
# These are the predictions of the model on the data that were used to fit the model.
predsTrain2 = predict(fitTrain2)
# These are the predictions of the model on the data that were NOT used to fit the model.
# This is a better measure of how the model will perform in real life
predsTest2 = predict(fitTrain2, newdata = TestTX)
TestTX$preds2 <- predsTest2
head(TestTX)
# Calculation of the ASE for the Test set for two variables
ASEholderTest2_TX = sum((TestTX$preds2 - TestTX$ABV)^2)/(length(TestTX$ABV))
ASEholderTest2_TX


#result for CO
ASEholderTrain1_CO = c()
ASEholderTest1_CO = c()
ASEholderTrain2_CO = c()
ASEholderTest2_CO = c()

fitTrain1 = lm(ABV~IBU, data = TrainingCO)
autoplot(fitTrain1, smooth.colour = NA)
summary(fitTrain1)
# These are the predictions of the model on the data that were used to fit the model.
predsTrain1 = predict(fitTrain1)
# These are the predictions of the model on the data that were NOT used to fit the model.
# This is a better measure of how the model will perform in real life
predsTest1 = predict(fitTrain1, newdata = TestCO)
TestCO$preds1 <- predsTest1
head(TestCO)
# Calculation of the ASE for the Test set for one variable
ASEholderTest1_CO = sum((TestCO$preds1 - TestCO$ABV)^2)/(length(TestCO$ABV))
ASEholderTest1_CO

fitTrain2 = lm(ABV~IBU+IBU2, data = TrainingCO)
autoplot(fitTrain2, smooth.colour = NA)
summary(fitTrain2)
# These are the predictions of the model on the data that were used to fit the model.
predsTrain2 = predict(fitTrain2)
# These are the predictions of the model on the data that were NOT used to fit the model.
# This is a better measure of how the model will perform in real life
predsTest2 = predict(fitTrain2, newdata = TestCO)
TestCO$preds2 <- predsTest2
head(TestCO)
# Calculation of the ASE for the Test set for two variables
ASEholderTest2_CO = sum((TestCO$preds2 - TestCO$ABV)^2)/(length(TestCO$ABV))
ASEholderTest2_CO

#the results from both TX and Co suggest that adding the quadratic term of explanatory vairable
#didn't improve the ASE on test dataset from just using the one explanatory varuable. 

#BONUS: Is there another method that you know of that will provide inference as to the significance of the squared IBU term? 
#Please describe your thoughts and provide relevant statistics.  Does this inference agree with the result of your cross validation?  

#just check if the coefficients or IBU and IBU2 are significant using the whole dataset (TX or CO)
fit1 = lm(ABV~IBU+IBU2, data = beerTX)
#autoplot(fitTrain1, smooth.colour = NA)
summary(fit1)
fit12 = lm(ABV~IBU2, data = beerTX)
#autoplot(fitTrain1, smooth.colour = NA)
summary(fit12)

fit2 = lm(ABV~IBU+IBU2, data = beerCO)
autoplot(fitTrain1, smooth.colour = NA)
summary(fit2)
fit122 = lm(ABV~IBU2, data = beerCO)
autoplot(fitTrain1, smooth.colour = NA)
summary(fit12)




