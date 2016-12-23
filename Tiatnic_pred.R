load("all_data.RData")

all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
all_data$Cabin <- as.character(all_data$Cabin)
all_data$Deck <- NA
all_data$Deck <- substring(all_data$Cabin, 1, 1)
all_data$Deck <- sub("^$", "U", all_data$Deck)
all_data$Deck <- factor(all_data$Deck)


library(rpart)

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

train <- all_data[1:891,]
test <- all_data[892:1309,]

library(randomForest)

str(train)
str(test)

set.seed(111)

my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Title + Deck
                          ,
                          data = train, importance = TRUE, ntree = 1000)

my_prediction <- predict(my_forest, test)


my_solution <-data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

varImpPlot(my_forest)