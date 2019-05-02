
# Code servant à encoder les datasets 

# Téléchargement des librairies
library(readr)
library(dplyr)

###-------------------------------------------------###

# Fonction Data_Input_extraction qui encode les données génomiques de manière "one-hot"

data_input <- function(filename, one.hot = TRUE){
  
  # On lit dans un premier temps le fichier csv.
  Data_Geno<- read.csv(file = filename, header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  # On transforme ce fichier csv en matrice afin de pouvoir le manipuler, sous le nom Data_Extract. Pour cela on ne retient
  # pas la première colonne et la première ligne du dataframe initial qui contiennent le nom des variables et des individus.
  Data_Extract <- as.matrix(Data_Geno[,-1][-1,], rownames.force = TRUE)
  
  
  # Pour chaque élément de la matrice Data_Extract, on associe aux éléments des 2 premières dimensions de
  # Data_Extract_new un array représentant la version one-hot de la valeur du marqueur génomique. 
  # Ainsi à un élément "A/A" de Data_Extract, on associe l'array c(1,,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) dans le tenseur
  if (one.hot == TRUE){
    
    
    # On crée un tenseur d'ordre 3 avec pour unique valeur NA, dont 2 des dimensions sont celles de la matrice Data_Extract
    # et dont la troisième dimension est de taille 16, c'est-à-dire le nombre total de marqueurs génomiques existants sous 
    # la forme "A/A". C'est cette matrice Data_Extract_new que l'on va chercher à transformer et qui sera fourni au réseau. 
    Data_Extract_new = array(NA, dim=c(dim(Data_Extract)[1],dim(Data_Extract)[2],16))
    
    
    for (i in 1:(dim(Data_Extract)[1])){
      for (j in 1:(dim(Data_Extract)[2])){
        if (Data_Extract[i,j] == "A/A"){
          Data_Extract_new[i,j,] = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "A/C") {
          Data_Extract_new[i,j,] = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "A/G") {
          Data_Extract_new[i,j,] = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "A/T") {
          Data_Extract_new[i,j,] = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "C/A") {
          Data_Extract_new[i,j,] = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "C/C") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "C/G"){
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "C/T") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "G/A") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "G/C") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "G/G") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
        }else if (Data_Extract[i,j] == "G/T") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
        }else if (Data_Extract[i,j] == "T/A") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
        }else if (Data_Extract[i,j] == "T/C") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
        }else if (Data_Extract[i,j] == "T/G") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
        }else if (Data_Extract[i,j] == "T/T") {
          Data_Extract_new[i,j,] = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
        }
      }
    }
  }
  
  if (one.hot == FALSE){
    
    Data_Extract_new = array(NA, dim=c(dim(Data_Extract)[1],dim(Data_Extract)[2]))
    
    for (i in 1:(dim(Data_Extract)[1])){
      for (j in 1:(dim(Data_Extract)[2])){
        if (Data_Extract[i,j] == "A/A"){
          Data_Extract_new[i,j] = 1
        }else if (Data_Extract[i,j] == "A/C") {
          Data_Extract_new[i,j] = 2
        }else if (Data_Extract[i,j] == "A/G") {
          Data_Extract_new[i,j] = 3
        }else if (Data_Extract[i,j] == "A/T") {
          Data_Extract_new[i,j] = 4
        }else if (Data_Extract[i,j] == "C/A") {
          Data_Extract_new[i,j] = 5
        }else if (Data_Extract[i,j] == "C/C") {
          Data_Extract_new[i,j] = 6
        }else if (Data_Extract[i,j] == "C/G"){
          Data_Extract_new[i,j] = 7
        }else if (Data_Extract[i,j] == "C/T") {
          Data_Extract_new[i,j] = 8
        }else if (Data_Extract[i,j] == "G/A") {
          Data_Extract_new[i,j] = 9
        }else if (Data_Extract[i,j] == "G/C") {
          Data_Extract_new[i,j] = 10
        }else if (Data_Extract[i,j] == "G/G") {
          Data_Extract_new[i,j] = 11
        }else if (Data_Extract[i,j] == "G/T") {
          Data_Extract_new[i,j] = 12
        }else if (Data_Extract[i,j] == "T/A") {
          Data_Extract_new[i,j] = 13
        }else if (Data_Extract[i,j] == "T/C") {
          Data_Extract_new[i,j] = 14
        }else if (Data_Extract[i,j] == "T/G") {
          Data_Extract_new[i,j] = 15
        }else if (Data_Extract[i,j] == "T/T") {
          Data_Extract_new[i,j] = 16
        }
      }
    }
  }
  
  # Une fois que l'on a complété tous les éléments de Data_Extract_new, on retourne ce tenseur en sortie de la fonction. 
  return(Data_Extract_new)
}


###-------------------------------------------------###

# Fonction Data_Output_extraction qui encode les données phénotypiques scalaires

data_output_regression <- function(filename,i){
  
  # On lit dans un premier temps le fichier csv.
  Data_Pheno <- read.csv(file = filename, header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "") 
  
  # On transforme ce fichier csv en matrice afin de pouvoir le manipuler, sous le nom Data_Extract_Pheno. Pour cela on 
  # ne retient pas la première ligne du dataframe initial qui contient le nom des variables, et on sélectionne 
  # la colonne "i" qui contient la variable à sélectionner. 
  Data_Extract_Pheno <- as.matrix(Data_Pheno[-1,][,i], rownames.force = TRUE)
  
  # On crée une matrice avec pour unique valeur NA, avec la même dimension que la matrice Data_Extract_Pheno.
  Output_Matrix = matrix(NA, nrow = dim(Data_Extract_Pheno))
  
  # On instancie chaque élément de la matrice Output_Matrix par les éléments de la matriceData_Extract_Pheno 
  # que l'on a converti en "numeric". 
  for (k in 1:dim(Data_Extract_Pheno)){
    Output_Matrix[k] = as.numeric(Data_Extract_Pheno[k])
  }
  
  # On stocke dans une matrice Output_Matrix_new tous les éléments non NA de Output_Matrix.
  Output_Matrix_new <- Output_Matrix[!is.na(Output_Matrix)]
  
  # Pour toutes les valeurs NA de Output_Matrix on remplace la valeur NA par la moyenne des valeurs de l'Output_Matrix_new.
  Output_Matrix[is.na(Output_Matrix)] <- mean(Output_Matrix_new)
  
  # On reshape Output_Matrix sous la forme d'une matrice de taille c(length(Output_Matrix),1) afin de retrouver la forme
  # intiale de la matrice Output_Matrix.
  dim(Output_Matrix) <- c(length(Output_Matrix),1)
  
  # On obtient le mean de Output_Matrix.
  mean <- apply(Output_Matrix, 2, mean)
  
  # On obtient l'écart-type de Output_Matrix.
  std <- apply(Output_Matrix, 2, sd)
  
  # On centre et on normalise Output_Matrix.
  Output_Matrix <- scale(Output_Matrix, center = mean, scale = std)
  
  # On retourne la matrice Output_Matrix
  return(Output_Matrix)
}   


###-------------------------------------------------###

# Fonction Data_Output_extraction qui encode les données phénotypiques scalaires
data_output_class <- function(filename,i){
  
  # On lit dans un premier temps le fichier csv.
  Data_Pheno <- read.csv(file = filename, header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "") 
  
  # On transforme ce fichier csv en matrice afin de pouvoir le manipuler, sous le nom Data_Extract_Pheno. Pour cela on 
  # ne retient pas la première ligne du dataframe initial qui contient le nom des variables, et on sélectionne 
  # la colonne "i" qui contient la variable à sélectionner. 
  Data_Extract_Pheno <- as.matrix(Data_Pheno[-1,][,i], rownames.force = TRUE)
  
  # On crée une matrice avec pour unique valeur NA, avec la même dimension que la matrice Data_Extract_Pheno.
  Output_Matrix = matrix(0, nrow = dim(Data_Extract_Pheno))
  
  # On encode les valeurs catégoriques par des scalaires. Pour les valeurs "Very low" et "Low" on associe la valeur 0, 
  # pour la valeur médium on associe la valeur 1, pour les valeurs "High" et "Very high" on associe la valeur 2. 
  for (i in 1:(dim(Data_Extract_Pheno)[1])){
    for (j in 1:(dim(Data_Extract_Pheno)[2])){
      if (Data_Extract_Pheno[i,j] == "Very low"){
        Output_Matrix[i,j] = 0
      }else if (Data_Extract_Pheno[i,j] == "Low") {
        Output_Matrix[i,j] = 0
      }else if (Data_Extract_Pheno[i,j] == "Medium") {
        Output_Matrix[i,j] = 1
      }else if (Data_Extract_Pheno[i,j] == "High") {
        Output_Matrix[i,j] = 2
      }else if (Data_Extract_Pheno[i,j] == "Very high") {
        Output_Matrix[i,j] = 2
      }
    }
  }
  return(as.numeric(Output_Matrix))
}




