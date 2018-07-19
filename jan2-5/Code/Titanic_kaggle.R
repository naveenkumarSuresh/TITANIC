
setwd("C:/Users/nandy/Desktop/Kaggle")

titanic.train<-read.csv(file="train.csv",stringsAsFactors = FALSE)

titanic.test<-read.csv(file="test.csv",stringsAsFactors = FALSE)

titanic.train$IsTrainSet<-TRUE
titanic.test$IsTrainSet<-FALSE

#there is no col name Survived in test set

titanic.test$Survived <- NA

names(titanic.test)

titanic.full<- rbind(titanic.train,titanic.test)

#missing value in titanic.full set 
#table(titanic.full$Embarked)


titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

#table (titanic.full$Embarked)

#missing value in Age 

#table(is.na(titanic.full$Age))

#median of titanic.full$Age

Age.median<-median(titanic.full$Age,na.rm = TRUE)

titanic.full[is.na(titanic.full$Age),"Age"]<-Age.median

#table (is.na(titanic.full$Age))

#missing value in titanic.full$Fare

Fare.median<-median(titanic.full$Fare,na.rm = TRUE)

titanic.full[is.na(titanic.full$Fare),"Fare"]<-Fare.median

#table(is.na(titanic.full$Fare))

# str(titanic.full)

#Categorical Casting
#str(titanic.full)

titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)


#splitting the data into train and test back again

titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]

titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

#Catogoric the Survuvied col

titanic.train$Survived<-as.factor(titanic.train$Survived)

#Initial Formula

#randomForest(Survived~.) predict Survived with all other variable

survived.equation<-"Survived~Pclass+Sex+Age+SibSp+Embarked"

survived.formula<-as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)

titanic.model<-randomForest(formula=survived.formula,data=titanic.train,ntree=500,mtry=3,nodesize=0.01*nrow(titanic.test))


features.equation<-"Pclass+Sex+Age+SibSp+Parch+Fare+Embarked"

Survived.Prediction<-predict(titanic.model,newdata=titanic.test)

Survived.Prediction

# Two col PassengerID and Survived

PassengerId<-titanic.test$PassengerId

output.df<-as.data.frame(PassengerId)

output.df$Survived<-Survived.Prediction

write.csv(output.df, file="Kaggle_output", row.names = FALSE)

#The row.names=FALSE means to elimate the row index number
