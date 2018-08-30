rm(list=ls())
set.seed(1)

installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}

needed <- c('randomForest')      
installIfAbsentAndLoad(needed)

###   Data Import   ###

file <- read.csv("Assignment2TrainingData.csv")
file <- na.omit(file)
nocustid <- file[-1]

train <- sample(1:dim(nocustid)[1], dim(nocustid)[1]*.8)
data.train <- nocustid[train,]
data.test <- nocustid[-train,]

###   Functions   ###

confusion <- function(true, pred, title = NULL, type) {
  if (is.null(title)) {
    conf.table <- table(true, pred, dnn = c('True', 'Pred'))
  } else {
    conf.table <- table(true, pred, dnn = c('', title))
  }
  
  acc <- (conf.table[1] + conf.table[4])/sum(conf.table)
  err <- (conf.table[2] + conf.table[3])/sum(conf.table)
  precision <- conf.table[2,2]/sum(conf.table[,2])
  type2 <- conf.table[1,2]/sum(conf.table[2,])  #FP
  type1 <- conf.table[2,1]/sum(conf.table[1,])  #FN
  power <- 1 - type2
  true_pos <- conf.table[2,2]/sum(conf.table[2,])
  true_neg <- conf.table[1,1]/sum(conf.table[1,])
  
  if (type == 1) {
    return(c(type1, type2, true_pos, true_neg, acc))
  } else {
    print(paste('Overall Accuracy:', acc))
    print(paste('Overall Error Rate:', err))
    print(paste('Type 1 Error Rate:', type1))
    print(paste('Type 2 Error Rate:', type2))
    print(paste('Power:', power))
    print(paste('Precision:', precision))
    return(conf.table)
  }
}


expected_cost <- function(vector) {
  false_neg <- vector[1]
  false_pos <- vector[2]
  true_pos <- vector[3]
  true_neg <- vector[4]
  true_churn <- .26448
  retention_rate <- .45
  
  rol <- true_churn * (1 - retention_rate) * true_pos * 13100
  ronl <- true_churn * retention_rate * true_pos * 1600
  rnol <- true_churn * false_neg * 11500
  rnonl <- 0
  nol <- 0
  nonl <- (1 - true_churn) * false_pos * 1600
  nnol <- 0
  nnonl <- (1 - true_churn) * true_neg * 0
  
  total_expected_cost <- rol + ronl + rnol + rnonl + nol + nonl + nnol + nnonl
  return(total_expected_cost)
}

###   Random Forest Model   ###

train <- sample(1:dim(nocustid)[1], dim(nocustid)[1]*.9)
data.train <- nocustid[train,]
data.test <- nocustid[-train,]

best.model <- randomForest(formula = Churn ~., data = data.train, mtry = 4, ntree = 500, importance = T, replace = F)
  
best.pred <- predict(best.model, newdata = data.test, type = 'prob')
best.pred_prob <- 1 - best.pred[,1]

expected <- rep(0, 101)
false_pos <- rep(0, 101)
false_neg <- rep(0, 101)
true_pos <- rep(0, 101)
true_neg <- rep(0, 101)
error <- rep(0, 101)
cutoff_rate <- seq(0, 1, .01)
for (i in 1:101){
  pred <- ifelse(best.pred_prob < cutoff_rate[i], 0, 1)
  conf <- confusion(data.test$Churn, pred, type = 1)
  false_pos[i] <- conf[1]
  false_neg[i] <- conf[2]
  true_pos[i] <- conf[3]
  true_neg[i] <- conf[4]
  error[i]  <- 1-conf[5]
  expected[i] <- expected_cost(conf)
}

print(paste("Minimum Expected Cost: $", min(expected)))
print(paste("Cutoff Value for Minimum Expected Cost: ", cutoff_rate[which.min(expected)]))

###  Table   ###

table <- cbind("Cutoff" = cutoff_rate, "False Positive" = false_pos, "False Negative" = false_neg,
               "True Positive" = true_pos, "True Negative" = true_neg, "Overall Error" = error, "Expected Cost" = expected)

write.csv(table, file = "Assignment2Costs.csv")
