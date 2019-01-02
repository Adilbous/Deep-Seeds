library(readr)
library(dplyr)

Data_Input_extraction <- function(s){
  Data_Geno<- read.csv(file = s, header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  Data_Extract <- as.matrix(Data_Geno[,-1][-1,], rownames.force = TRUE)
  Data_Extract1 <- matrix(NA, nrow = dim(Data_Extract)[1], ncol = dim(Data_Extract)[2])
  
  for (i in 1:dim(Data_Extract)[1]){
    for (j in 1:dim(Data_Extract)[2]){
      if (Data_Extract[i,j] == "A/A"){
        Data_Extract1[i,j] = 1
      }else if (Data_Extract[i,j] == "A/C") {
        Data_Extract1[i,j] = 2
      }else if (Data_Extract[i,j] == "A/G") {
        Data_Extract1[i,j] = 3
      }else if (Data_Extract[i,j] == "A/T") {
        Data_Extract1[i,j] = 4
      }else if (Data_Extract[i,j] == "C/A") {
        Data_Extract1[i,j] = 5
      }else if (Data_Extract[i,j] == "C/C") {
        Data_Extract1[i,j] = 6
      }else if (Data_Extract[i,j] == "C/G"){
        Data_Extract1[i,j] = 7
      }else if (Data_Extract[i,j] == "C/T") {
        Data_Extract1[i,j] = 8
      }else if (Data_Extract[i,j] == "G/A") {
        Data_Extract1[i,j] = 9
      }else if (Data_Extract[i,j] == "G/C") {
        Data_Extract1[i,j] = 10
      }else if (Data_Extract[i,j] == "G/G") {
        Data_Extract1[i,j] = 11
      }else if (Data_Extract[i,j] == "G/T") {
        Data_Extract1[i,j] = 12
      }else if (Data_Extract[i,j] == "T/A") {
        Data_Extract1[i,j] = 13
      }else if (Data_Extract[i,j] == "T/C") {
        Data_Extract1[i,j] = 14
      }else if (Data_Extract[i,j] == "T/G") {
        Data_Extract1[i,j] = 15
      }else if (Data_Extract[i,j] == "T/T") {
        Data_Extract1[i,j] = 16
      }
    }
  }

  return(Data_Extract1)
}
                         
Data_Output_extraction <- function(s,i){
  Data_Pheno <- read.csv(file = s, header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "") 
  Data_Extract_Pheno <- as.matrix(Data_Pheno[-1,][,i], rownames.force = TRUE)
  Output_Matrix = matrix(NA, nrow = dim(Data_Extract_Pheno))
  
  for (k in 1:dim(Data_Extract_Pheno)){
    Output_Matrix[k] = as.numeric(Data_Extract_Pheno[k])
  }
  
  mean <- apply(Output_Matrix, 2, mean)
  std <- apply(Output_Matrix, 2, sd)
  Output_Matrix <- scale(Output_Matrix, center = mean, scale = std)
  
  return(Output_Matrix)
}                        
                      
  








