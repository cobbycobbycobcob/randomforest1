### Decision Trees
install.packages('rpart.plot')
install.packages('randomForest')
library(rpart)
library(rpart.plot)
library(ISLR)
library(ggplot2)
library(caTools)
library(randomForest)
library(dplyr)

str(College)
 df <- College

# Exploratory Analysis
pl1 <- ggplot(df, aes(x = Room.Board, y = Grad.Rate)) +
                 geom_point(aes(colour = Private))
pl1 

pl2 <- ggplot(df, aes(F.Undergrad)) +
  geom_histogram(aes(fill = Private), colour = 'black', bins = 50, position = position_stack(reverse = T))
pl2

?geom_histogram

pl3 <- ggplot(df, aes(Grad.Rate)) +
  geom_histogram(aes(fill = Private), colour = 'black', bins = 50, position = position_stack(reverse = T))
pl3

# Data Correction
filter(df, Grad.Rate > 100)

df['Grad.Rate'][df['Grad.Rate'] == 118] <- 100

df['Cazenovia College', 'Grad.Rate']

# Train Test Split
sample <- sample.split(df$Private, SplitRatio = 0.7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)

### Build a Decision Tree Model

tree <- rpart(Private ~ ., method = 'class', data = train)

# Generate predictions using the decision tree
# Returns probabilities associated with each classification (i.e private or not)
tree.pred <- predict(tree, test)
head(tree.pred)

# Create a new column summarizing the data
tree.pred <- as.data.frame(tree.pred) %>%
  mutate(
    Prediction = ifelse(Yes >= 0.5, 'Yes (Private)', 'No (Public)'),
  )

tree.pred

# Plot tree
prp(tree)

# Plot confusion matrix
table(tree.pred$Prediction, test$Private)

## Create a random forest model

rf.model <- randomForest(Private ~ ., data = train, importance = T)

# Note importance provides a measure of how much information is gained from each split in the decision tree

rf.model$confusion
rf.model$importance

## Use random forest model to make predictions

rf.preds <- predict(rf.model, test)
table(rf.preds, test$Private)
