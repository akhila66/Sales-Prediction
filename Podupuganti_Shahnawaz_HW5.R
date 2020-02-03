# --------------------------- librarys
library(anytime)
library(tidyverse)
library(tidyr)
library(dplyr)
library(summarytools)
library(data.table)
library(mltools)
library(caret)
library(DataExplorer)
library(stringr)
library(lubridate)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
#--------------- ----------------  --------------- data loading --------------- --------------- --------------- --------------- ---------------

TRAIN_data <- read.csv('~/Desktop/SEM2/IDE/HW5/Train.csv',header=TRUE)
TEST_data <- read.csv('~/Desktop/SEM2/IDE/HW5/Test.csv',header=TRUE)
dim(TRAIN_data)
dim(TEST_data)

#  --------------- --------------- ---------------data explorations ---------------  --------------- --------------- --------------- --------------- ---------------
str(TRAIN_data)

funModeling::df_status(TRAIN_data)
funModeling::freq((TRAIN_data))

funModeling::plot_num(TRAIN_data)

DataExplorer::plot_density(TRAIN_data)
# checking after N/A and replacing null values 

#TRAIN_data[TRAIN_data== ""] <- NA
#TRAIN_data$bounces[is.na(TRAIN_data$bounces)] <- 0

ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = pageviews, colour = country)) # cato variable


ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = pageviews, colour = continent, shape = deviceCategory)) # cato variable

ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = pageviews, colour = continent, shape = deviceCategory, size = deviceCategory)) # cato variable

ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = visitNumber , colour = continent, shape = deviceCategory, size = deviceCategory)) # cato variable

ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = pageviews , colour = as.factor(bounces), shape = deviceCategory, size = deviceCategory)) # cato variable

ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = visitNumber , colour = as.factor(bounces), shape = deviceCategory, size = deviceCategory)) # cato variable

ggplot(data = TRAIN_data) +
  geom_point(mapping = aes(x = revenue, y = visitNumber , colour = as.factor(bounces), shape = browser)) # cato variable

#  --------------- --------------- --------------- binding both train and test to apply changes togather-------------  --------------- --------------- --------------- --------------- ---------------

TRAIN_data$type<- 0
TEST_data$type <- 1
TEST_data$revenue <- NA
fulldata <- rbind(TRAIN_data,TEST_data)
fulldata[fulldata== ""] <- NA

dim(fulldata) # combinded helps to transform data togather.
head(fulldata)
fulldata %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness


#  --------------- --------------- ---------------data transformation data preparation ---------------  --------------- --------------- --------------- --------------- ---------------
fulldata$date<- as.Date(fulldata$date, "%Y-%m-%d") 
fulldata$month<- as.factor(strftime(fulldata$date,"%m"))
fulldata$year<- as.factor(strftime(fulldata$date,"%Y"))
fulldata$roundweek <- as.factor(weekdays(fulldata$date))
summary(fulldata)
fulldata <- fulldata[,-c(3)] # removing date 

#seperating the numerical and categorical data in to seperate tables
full_cat<- fulldata[ ,sapply(fulldata, is.factor)]
colnames(full_cat)

full_num <- fulldata[ ,!sapply(fulldata, is.factor)]
colnames(full_num)
#Removing columns with >90 data missing
full_num<-full_num[,colMeans(is.na(full_num)) <= .80]
#dim(full_num)
full_cat<-full_cat[,colMeans(is.na(full_cat)) <= .80]
#dim(full_cat)
colnames(full_num)
colnames(full_cat)

# remove unessasary variables
full_cat <- full_cat[ , !(names(full_cat) %in% c('region','metro','city','networkDomain','topLevelDomain'))]

for(i in colnames(full_cat))
{
  full_cat[,i]<- fct_explicit_na(full_cat[,i], na_level = names(sort(summary(full_cat[,i]), decreasing=T))[1])
}
full_cat %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness
View(full_cat)
#for numerical data na to 0 which represent null
full_num$bounces[is.na(full_num$bounces)] <- 0
full_num$newVisits[is.na(full_num$newVisits)] <- 0
full_num$pageviews[is.na(full_num$pageviews)] <- 0
#full_num$adwordsClickInfo.page[is.na(full_num$adwordsClickInfo.page)] <- 0
#full_num$adwordsClickInfo.isVideoAd[is.na(full_num$adwordsClickInfo.isVideoAd)] <- 0

full_cat %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness
full_num %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness

#Using onehot function on categorical variable

catcols <- colnames(full_cat)
for(i in catcols)
{
  print(i)
  
  full_cat[i] <- str_replace_all(full_cat[,i], "[^[:alnum:]]", " ") # to remove special char
  d <- dummyVars(" ~ .", data = full_cat[i])
  onehot <- data.frame(predict(d, newdata = full_cat[i]))
  full_cat <- cbind(full_cat,onehot)
}

dim(full_cat)
full_cat <- full_cat[ , !(names(full_cat) %in% catcols)]
dim(full_cat)

colnames(full_num)
full_num$visitNumber <- log(full_num$visitNumber+1)
full_num$timeSinceLastVisit <- log(full_num$timeSinceLastVisit+1)
full_num$pageviews <- log(full_num$pageviews+1)
full_num %>% mutate_all(is.na) %>% summarise_all(mean) # checking missingness

#M2<-sapply(full_cat,unclass)    # data.frame of all categorical variables now displayed as numeric
summary(full_num)
Cfulldata<-cbind(full_num,full_cat)        # complete data.frame with all variables put together

outtrain <- subset(Cfulldata, type == 0)
outtest <-  subset(Cfulldata, type == 1)
head(outtrain)
#reformulate(colnames(outtrain), "revenue")
corv <- cor((outtrain), log(outtrain$revenue+1)) # non of them has good corealtion
View(corv)

souttrain <- outtrain[ , (names(outtrain) %in% c('revenue','custId','pageviews','sourcemall.googleplex.com','countryUnited.States','subContinentNorthern.America','referralPath.','continentAmericas','channelGroupingReferral','timeSinceLastVisit',
                                                'operatingSystemMacintosh','isTrueDirect','browserChrome','visitNumber','deviceCategorydesktop','bounces',
                                                'newVisits','channelGroupingSocial','continentAsia','sourceyoutube.com','continentEurope','isMobile'))]
souttrain <- data.frame(aggregate(souttrain, by=list(souttrain$custId),FUN=mean))
souttrain <- souttrain[ , !(names(souttrain) %in% c('custId','Group.1'))]
colnames(souttrain)

fit <- lm(log(revenue+1) ~ . , data = souttrain)

summary(fit)
model.summary = summary(fit)$coefficients
model.summary
summary(fit)$r.squared # more is better
barplot(sort(model.summary[model.summary[,4]<0.05,4]), main="independent variables Pr(>|t|) <0.05")
test <- predict(fit, souttrain)
rmse(log(souttrain$revenue+1),test)

library('DAAG')
cv.lm(data = souttrain ,m=3, form.lm = formula(log(revenue+1) ~ .), plotit=TRUE, printit=TRUE, dots = 1)

# using caret pakage
data_ctrl <- trainControl(method = "cv", number = 5)
model_caret <- train(log(revenue+1) ~ .,   # model to fit
                     data = souttrain,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this

model_caret
model_caret$finalModel
model_caret$resample

# --------------------------- for getting outoput file
souttest <- outtest[ , (names(outtest) %in% c('custId','pageviews','sourcemall.googleplex.com','countryUnited.States','subContinentNorthern.America','referralPath.','continentAmericas','channelGroupingReferral','timeSinceLastVisit',
                                             'operatingSystemMacintosh','isTrueDirect','browserChrome','visitNumber','deviceCategorydesktop','bounces',
                                             'newVisits','channelGroupingSocial','continentAsia','sourceyoutube.com','continentEurope','isMobile'))]
souttest <- data.frame(aggregate(souttest, by=list(souttest$custId),FUN=mean))

res <- predict(fit, souttest)
length(res)
length(outtest$custId)
predicted<-data.frame(custId = souttest$custId,predRevenue = res)
predicted %>% select_if(is.numeric) %>% mutate_all(is.na) %>% summarise_all(mean)
#predicted[is.na(predicted)] <- 0
write.csv(predicted, file = paste("~/Desktop/SEM2/IDE/HW5/submit",Sys.time(),".csv"), row.names=FALSE)





# cleaning
rm(list = ls(all.names = TRUE))
gc()




