library(dplyr)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)

setwd("D:/analytics training/Important materials/Intern & Kaggle/Churn Prediction")
mydata <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
apply(apply(mydata,2,is.na),2,sum);nrow(mydata)
str(mydata)
head(mydata)
summary(mydata)
sapply(mydata, function(x) length(unique(x)))
attach(mydata)
View(mydata)
mydata <- mydata[complete.cases(mydata), ]


#Recode the given categorical value

mydata$OnlineSecurity <- recode(mydata$OnlineSecurity,"No internet service" ='No')
mydata$OnlineBackup <- recode(mydata$OnlineBackup,"No internet service" ='No')
mydata$DeviceProtection <- recode(mydata$DeviceProtection,"No internet service" ='No')
mydata$TechSupport <- recode(mydata$TechSupport,"No internet service" ='No')
mydata$StreamingTV <- recode(mydata$StreamingTV,"No internet service" ='No')
mydata$StreamingMovies <- recode(mydata$StreamingMovies,"No internet service" ='No')
mydata$MultipleLines <- recode(mydata$MultipleLines,"No phone service" ='No')

#mydata$Surgery...procedure.name <- as.character (Hospital_Management$Surgery...procedure.name) 
#categorizing tenure into groups
min(mydata$tenure); max(mydata$tenure)
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
mydata$tenure_group <- sapply(mydata$tenure,group_tenure)
mydata$tenure_group <- as.factor(mydata$tenure_group)
mydata$SeniorCitizen <- as.factor(mapvalues(mydata$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))
#Removing variables which are not needed
mydata$customerID <- NULL
mydata$tenure <- NULL

#correlation
library(GGally)
pm <- ggpairs(mydata[,c(17,18)], ggplot2::aes(colour='species'))
pm
#Both monthly charge and total charge are correlated remove one of them
mydata$TotalCharges <- NULL

#Plots
p1 <- ggplot(mydata, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

#Create partition
trainingdata<- createDataPartition(mydata$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- mydata[trainingdata,]
testing<- mydata[-trainingdata,]

LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))
anova(LogModel, test="Chisq")

#Checking accuracy
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

#confusion matrix
table(testing$Churn,fitted.results)
