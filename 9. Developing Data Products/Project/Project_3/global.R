library(shiny)
library(randomForest) #randomForest
library(e1071) #Support Vector Machine
library(class) #K-nearest Neighbors
library(caret) #Data partition

data(iris)
set.seed(42)

train_idx <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_iris <- iris[train_idx, ]
test_iris <- iris[-train_idx, ]

knn_train_data <- train_iris[, 1:4]
knn_train_labels <- train_iris$Species

model_rf <- randomForest(Species ~., data = train_iris)

min_sl <- min(iris$Sepal.Length) # Sepal.Length minimum
max_sl <- max(iris$Sepal.Length) # Sepal.Length maximum
min_sw <- min(iris$Sepal.Width) # Sepal.Width minimum
max_sw <- max(iris$Sepal.Width) # Sepal.Width maximum
min_pl <- min(iris$Petal.Length) # Petal.Length minimum
max_pl <- max(iris$Petal.Length) # Petal.Length maximum
min_pw <- min(iris$Petal.Width) # Petal.Width minimum
max_pw <- max(iris$Petal.Width) # Petal.Width maximum

# UI Initial Values (Used for Imputation Check in server.R)
initial_value_sl <- 5.8
initial_value_sw <- 3.1
initial_value_pl <- 3.8
initial_value_pw <- 1.2

# Sepal.Length predict model
rf_reg_sl <- randomForest(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = train_iris)

# Sepal.Width predict model
rf_reg_sw <- randomForest(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, data = train_iris)

# Petal.Length predict model
rf_reg_pl <- randomForest(Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width, data = train_iris)

# Petal.Width predict model
rf_reg_pw <- randomForest(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = train_iris)