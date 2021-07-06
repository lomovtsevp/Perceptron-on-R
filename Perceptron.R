#lib for class creating
library('R6')

#library for graphics
library(ggplot2)

# random seed
set.seed(42)

#Implementation of perceptron
Perceptron <- R6Class(classname = 'Perceptron',
                                  public = list(
                                    num_features = NA, 
                                    learning_rate = NA, 
                                    t_max = NA,
                                    weights = NA,
                                    b = NA,
                                    t = NA,
                      
                                  initialize = function(num_features, learning_rate, t_max){
                                    self$num_features <-  num_features
                                    self$learning_rate <- learning_rate
                                    self$t_max <- t_max
                                    self$weights <- rep(0, num_features)
                                    self$b <- 0
                                    self$t <- 0
                                  },
                      
                                  fit = function(X, y){
                                    while (self$t < self$t_max){
                                      self$t <- self$t + 1
                                      idx <- sample(c(1:(length(y))), 1)
                                      if ((y[idx] * (X[idx, ] %*% self$weights + self$b)) <= 0){
                                        self$b <- self$b + self$learning_rate * y[idx]
                                        self$weights <- self$weights + self$learning_rate * y[idx] * X[idx, ]
                                        }
                                    }
                                    print(self)
                                    return (self)
                                  },
                                  
                                  predict = function(X_test){
                                    classes <- c()
                                    for (i in c(1:length(X_test[,1]))){
                                      if (((X_test[i, ] %*% self$weights) + self$b) >= 0)
                                        classes[i] <- 1
                                      else
                                        classes[i] <- -1
                                    }
                                    return (classes)
                                  }
                                  ))

#___________________________________________________________TEST PERCEPTRON______________________________________________________________-



#IN: Train Dataframe with 5 features and 1 binary target: 'orange' and 'grapefruit'

#OUT: Learned model "Perceptron" which classify Test data on 1 or -1 (orange or grapefruit).


#_______________________________Data preprocessing______________________________________________________

data <- read.csv('C:/Users/plomo/Downloads/citrus.csv')

data$name <- ifelse(data$name == 'orange', 1, -1)

#__________________________________________Split data on Train and Test_________________________

shuffle_index <- sample(x = 1:nrow(data), replace = F, size = nrow(data))

ratio <- 0.6
train_idx <- shuffle_index[1:(ratio * length(shuffle_index))]
test_idx <- shuffle_index[-(1:(ratio * length(shuffle_index)))]
X_train <- as.matrix(data[train_idx, -1])
X_test <- as.matrix(data[test_idx, -1])
y_train <- as.vector(data$name[train_idx])
y_test <- as.vector(data$name[test_idx])



#____________________________FITTING MODEL AND TEST IT________________________________________

model <- Perceptron$new(num_features=ncol(X_train), learning_rate=0.1, t_max = 4000)
model$fit(X_train, y_train)
predictions <- as.factor(model$predict(X_test))



#_____________________________________Metrics of classification quality_________________________________________
library(caret)

precision <- posPredValue(predictions, as.factor(y_test), positive="1")
recall <- sensitivity(predictions, as.factor(y_test), positive="1")

F1_score <- (2 * precision * recall) / (precision + recall)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

score_test <- calc_acc(y_test, predictions)


print(paste('Аккуратность вашей модели составила: ', ceiling(score_test * 100), '%'))
print(paste('Точность вашей модели составила: ', ceiling(precision*100), '%'))
print(paste('Полнота вашей модели составила:', ceiling(recall*100), '%'))
print(paste('F-мера вашей модели составила: ', ceiling(F1_score*100), '%'))

pred_data <- data.frame(predictions, y_test)

ggplot(pred_data, aes(predictions))+
  geom_bar(aes(ifelse(predictions == 1, 'orange', 'grapefruit')), col = 'white', fill = c('red', 'orange'))+
  xlab("Предсказанное распределение")+
  ylab('Количество фруктов')

ggplot(pred_data, aes(y_test))+
  geom_bar(aes(ifelse(y_test == 1, 'orange', 'grapefruit')), col = 'white', fill = c('red', 'orange'))+
  xlab("Объективное распределение")+
  ylab('Количество фруктов')

