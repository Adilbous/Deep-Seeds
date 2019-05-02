library(keras)


# Code servant à construire le model et à l'appliquer au dataset Poplar avec une méthode k-fold

# Fonction build_model_regression qui crée le modèle de notre réseau de type MLP
build_model_regression <- function(){
  model <- keras_model_sequential() %>%
    
    # L'input shape est de taille c(7808, 16), 7808 pour le nombre de marqueurs génomiques du dataset Poplar et 16
    # du fait du one-hot encoding
    layer_dense(units = 16, input_shape = c(7808,16))%>%
    layer_activation_leaky_relu() %>%
    
    # Le réseau comporte des layers de type "dropout" afin d'éviter l'overfitting. 
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 16, kernel_regularizer =  regularizer_l2(l = 0.02)) %>% 
    layer_activation_leaky_relu() %>%
    
    layer_dropout(rate = 0.35) %>%
    
    layer_dense(units = 16, kernel_regularizer =  regularizer_l2(l = 0.02)) %>%
    layer_activation_leaky_relu() %>%
    
    layer_dropout(rate = 0.2) %>%
    
    layer_dense(units = 8, kernel_regularizer =  regularizer_l2(l = 0.02)) %>%
    layer_activation_leaky_relu() %>%
    
    layer_dropout(rate = 0.3) %>%
    
    layer_flatten() %>%
    
    layer_dense(units = 5)
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mse")
  )
}

# Fonction regression_multiple qui applique le modèle sur les 5 composantes scalaires du dataset Poplar.Pheno, à savoir sur
# HT.ORL, BF.ORL, CIRC.ORL, BS.ORL, BS.SAV. 

regression_multiple <- function(markers, y, k = 4, num_epochs = 20, train_size = 450,
                                batch_size = 32) {
  
  # On crée les matrices de train et test sur les Markers et sur y. Pour cela 
  train_indices = sample(1:train_size)
  
  train_Markers = markers[train_indices,,]
  train_y = y[train_indices,]
  
  test_Markers = markers[-train_indices,,]
  test_y = y[-train_indices,]
  
  # On instancie les indices qui serviront lors du k-fold
  indices <- sample(1:nrow(train_Markers))
  folds <- cut(indices, breaks = k, labels = FALSE)
  all_scores <- c()
  
  # On instancie les array prennant les valeurs y prédites par le modèle pour chacune des valeurs phénotypiques
  # On stocke dans yi_test les valeurs de y à tester pour chaque type de variables phénotypiques. 
  predicted_y1_matrix <- matrix( data = c(0), nrow = k, ncol = dim(test_y)[1])
  average_predicted_y1 = array( data = c(0), dim = dim(test_y)[1])
  y1_test <- test_y[,1]
  dim(y1_test) <- c((length(y1_test)),1)
  
  predicted_y2_matrix <- matrix( data = c(0), nrow = k, ncol = dim(test_y)[1])
  average_predicted_y2 = array( data = c(0), dim = dim(test_y)[1])
  y2_test <- test_y[,2]
  dim(y2_test) <- c((length(y2_test)),1)
  
  predicted_y3_matrix <- matrix( data = c(0), nrow = k, ncol = dim(test_y)[1])
  average_predicted_y3 = array( data = c(0), dim = dim(test_y)[1])
  y3_test <- test_y[,3]
  dim(y3_test) <- c((length(y3_test)),1)
  
  predicted_y4_matrix <- matrix( data = c(0), nrow = k, ncol = dim(test_y)[1])
  average_predicted_y4 = array( data = c(0), dim = dim(test_y)[1])
  y4_test <- test_y[,4]
  dim(y4_test) <- c((length(y4_test)),1)
  
  predicted_y5_matrix <- matrix( data = c(0), nrow = k, ncol = dim(test_y)[1])
  average_predicted_y5 = array( data = c(0), dim = dim(test_y)[1])
  y5_test <- test_y[,5]
  dim(y5_test) <- c((length(y5_test)),1)
  
  
  #k-fold
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    
    # On instancie les éléments qui seront utilisées à chaque fold en tant que valeurs de train et valeurs de test (targets)
    # pour les markers et pour les y.
    val_indices <- which(folds == i, arr.ind = TRUE)
    val_data <- train_Markers[val_indices,,]
    val_targets <- train_y[val_indices,] 
    
    partial_train_data <- train_Markers[-val_indices,,]
    partial_train_targets <- train_y[-val_indices,]
    
    #On train le modèle sur les données retenues pour chaque fold.
    model <- build_model_regression()
    
    model %>% fit(partial_train_data, partial_train_targets,
                  epochs = num_epochs, batch_size = batch_size, verbose = 1)
    
    
    results <- model %>% evaluate(val_data, val_targets, verbose = 1)
    
    all_scores <- c(all_scores, results$mean_squared_error)
    
    predicted_y <- model %>% predict(test_Markers)
    
    
    
    pred_y1 <- predicted_y[,1]
    dim(pred_y1) <- c((length(pred_y1)),1)
    predicted_y1_matrix[i,] = t(pred_y1)
    
    pred_y2 <- predicted_y[,2]
    dim(pred_y2) <- c((length(pred_y2)),1)
    predicted_y2_matrix[i,] = t(pred_y2)
    
    pred_y3 <- predicted_y[,3]
    dim(pred_y3) <- c((length(pred_y3)),1)
    predicted_y3_matrix[i,] = t(pred_y3)
    
    pred_y4 <- predicted_y[,4]
    dim(pred_y4) <- c((length(pred_y4)),1)
    predicted_y4_matrix[i,] = t(pred_y4)
    
    pred_y5 <- predicted_y[,5]
    dim(pred_y5) <- c((length(pred_y5)),1)
    predicted_y5_matrix[i,] = t(pred_y5)
    
  }
  
  
  for (j in 1:k){
    average_predicted_y1 = average_predicted_y1 + predicted_y1_matrix[j,]
    average_predicted_y2 = average_predicted_y2 + predicted_y2_matrix[j,]
    average_predicted_y3 = average_predicted_y3 + predicted_y3_matrix[j,]
    average_predicted_y4 = average_predicted_y4 + predicted_y4_matrix[j,]
    average_predicted_y5 = average_predicted_y5 + predicted_y5_matrix[j,]
  }
  
  average_predicted_y1 = average_predicted_y1 / k
  average_predicted_y2 = average_predicted_y2 / k
  average_predicted_y3 = average_predicted_y3 / k
  average_predicted_y4 = average_predicted_y4 / k
  average_predicted_y5 = average_predicted_y5 / k
  
  linear_reg1 = lm(average_predicted_y1 ~ y1_test)
  linear_reg2 = lm(average_predicted_y2 ~ y2_test)
  linear_reg3 = lm(average_predicted_y3 ~ y3_test)
  linear_reg4 = lm(average_predicted_y4 ~ y4_test)
  linear_reg5 = lm(average_predicted_y5 ~ y5_test)
  
  y_average = cbind(average_predicted_y1, average_predicted_y2, 
                    average_predicted_y3, average_predicted_y4, average_predicted_y5)
  
  colnames(y_average) = colnames(y)
  colnames(test_y) = colnames(y)
  
  results = list( map = colnames(y_average), reg1  = linear_reg1, reg2 = linear_reg2, reg3 = linear_reg3,
                 reg4 = linear_reg4, reg5 = linear_reg5, "y_pred" = as.data.frame(y_average), 
                 "y_test" = as.data.frame(test_y), "MSE" = mean(all_scores))
  return (results)

}


