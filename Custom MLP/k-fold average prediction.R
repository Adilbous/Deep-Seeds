rm(list=ls())
library(keras)

build_model <-function(){
  model <- keras_model_sequential() %>%
    
    layer_dense(units = 16, activation = "relu",input_shape = c(7808, 16))%>%
    
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 16, activation = "relu", 
                kernel_regularizer =  regularizer_l2(l = 0.02)) %>%
    
    layer_dropout(rate = 0.35) %>%
    
    layer_dense(units = 16, activation = "relu", 
                kernel_regularizer =  regularizer_l2(l = 0.02)) %>%
    
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 8, activation = "relu", 
                kernel_regularizer =  regularizer_l2(l = 0.02)) %>%
    
    layer_dropout(rate = 0.3) %>%
    
    layer_flatten()%>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mse")
  )
}

setwd("/Users/adil/documents/GitHub/DeepGS")
source("Input-Encoding-function.R")

Markers <- Data_Input_extraction("Poplar.Geno.csv")
y <- Data_Output_extraction("Poplar.Pheno.csv",2)

train_size = 450

train_indices = sample(1:train_size)

train_Markers = Markers[train_indices,,]
train_y = y[train_indices,]

test_Markers = Markers[-train_indices,,]
test_y = y[-train_indices,]


k <- 4
indices <- sample(1:nrow(train_Markers))
folds <- cut(indices, breaks = k, labels = FALSE)

num_epochs <- 20
all_scores <- c()

predicted_y_matrix <- matrix( data = c(0), nrow = k, ncol = length(test_y))
average_predicted_y = array( data = c(0), dim = length(test_y))

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_Markers[val_indices, ,]
  val_targets <- train_y[val_indices]
  
  partial_train_data <- train_Markers[-val_indices, ,]
  partial_train_targets <- train_y[-val_indices]
  
  model <- build_model()
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 32, verbose = 1)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 1)
  
  all_scores <- c(all_scores, results$mean_squared_error)
  #all_scores[i] = results$mean_squared_error
  
  predicted_y <- model %>% predict(test_Markers)
  predicted_y_matrix[i,] = t(predicted_y)
  
  # linear_reg = lm(predicted_y ~ test_y)
  # print(summary(linear_reg))
  # plot(predicted_y, test_y)
}

for (j in 1:k){average_predicted_y = average_predicted_y + predicted_y_matrix[j,]}
average_predicted_y = average_predicted_y / k


linear_reg = lm(average_predicted_y ~ test_y)
print(summary(linear_reg))
plot(average_predicted_y, test_y)



