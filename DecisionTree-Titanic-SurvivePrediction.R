########################################################
##### Importing Data Set####
########################################################
set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)
summary(titanic)
tail(titanic)

#By head and tail, while considering pclass, we see data didn't spread randomly, trained 80% data might not see pclass 3
#Fixing the issue as required for better training and testing data set in the following section, avoides poor prediction
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)
titanic <- titanic[shuffle_index, ]
head(titanic)
summary(titanic)
tail(titanic)

########################################################
##### Cleaning Data Set####
########################################################
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

clean_titanic <- titanic %>%
  # Drop variables
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  #Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>% 
  # without missing data
  na.omit()
head(clean_titanic)
summary(clean_titanic)
tail(clean_titanic)

########################################################
##### Create train/test set####
########################################################
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)
summary(data_train)
summary(clean_titanic)
summary(data_test)

########################################################
##### BUILD the model####
########################################################
#install.packages("rpart.plot")	
library(rpart)
library(rpart.plot)
#
# formula: The function to predict
# data: Specifies the data frame
#method: 			
# "class" for a classification tree 			
# "anova" for a regression tree	
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

#When, Warning message:
#  labs do not fit even at cex 0.15, there may be some overplotting 
#Sample Tree can be visible
https://www.guru99.com/images/r_programming/032918_0938_DecisionTre1.png

#observation
#At the top, it is the overall probability of survival. It shows the proportion of passenger that survived the crash. 41 percent of passenger survived.
#This node asks whether the gender of the passenger is male. If yes, then you go down to the root's left child node (depth 2). 63 percent are males with a survival probability of 21 percent.
#In the second node, you ask if the male passenger is above 3.5 years old. If yes, then the chance of survival is 19 percent.
#You keep on going like that to understand what features impact the likelihood of survival.

########################################################
##### Prediction####
########################################################
predict_unseen <-predict(fit, data_test, type = 'class')
#Confusion Matrics
table_mat <- table(data_test$survived, predict_unseen)
table_mat

########################################################
##### Measure performance####
########################################################
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

########################################################
##### Hyper Parameter Tunning####
########################################################
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 3,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)

#Didn't get better performance, need further actions
Reference:
  https://www.guru99.com/r-decision-trees.html
 
