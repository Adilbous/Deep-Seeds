library(readr)
library(dplyr)
setwd("/Users/adil/Desktop/OBT/Projet option/Script")
Data_Geno<- read.csv(file = "Poplar.Geno.csv", header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "") 

Data_Extract <- as.matrix(Data_Geno[,-1][-1,], rownames.force = TRUE)#On retire également la première colonne et la première ligne qui ne contient pas de valeurs

dim(Data_Extract)
conv_data = matrix(NA, nrow = dim(Data_Extract)[1], ncol = dim(Data_Extract)[2])

for (i in 1:dim(Data_Extract)[1]){
  for (j in 1:dim(Data_Extract)[2]){
    if (Data_Extract[i,j] == "A/A"){
    conv_data[i,j] = 1
  }else if (Data_Extract[i,j] == "A/C") {
    conv_data[i,j] = 2
  }else if (Data_Extract[i,j] == "A/G") {
    conv_data[i,j] = 3
  }else if (Data_Extract[i,j] == "A/T") {
    conv_data[i,j] = 4
  }else if (Data_Extract[i,j] == "C/A") {
    conv_data[i,j] = 5
  }else if (Data_Extract[i,j] == "C/C") {
    conv_data[i,j] = 6
  }else if (Data_Extract[i,j] == "C/G"){
    conv_data[i,j] = 7
  }else if (Data_Extract[i,j] == "C/T") {
    conv_data[i,j] = 8
  }else if (Data_Extract[i,j] == "G/A") {
    conv_data[i,j] = 9
  }else if (Data_Extract[i,j] == "G/C") {
    conv_data[i,j] = 10
  }else if (Data_Extract[i,j] == "G/G") {
    conv_data[i,j] = 11
  }else if (Data_Extract[i,j] == "G/T") {
    conv_data[i,j] = 12
  }else if (Data_Extract[i,j] == "T/A") {
    conv_data[i,j] = 13
  }else if (Data_Extract[i,j] == "T/C") {
    conv_data[i,j] = 14
  }else if (Data_Extract[i,j] == "T/G") {
    conv_data[i,j] = 15
  }else if (Data_Extract[i,j] == "T/T") {
    conv_data[i,j] = 16
  }
 }
}
    

Data_Pheno <- read.csv(file = "Poplar.Pheno.csv", header = FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "") 

Data_Extract_Pheno <- as.matrix(Data_Pheno[-1,][,4], rownames.force = TRUE)#On selectionne la colonne BS.ORL et on enlève le premier terme "bs.orl"
Data_Extract_Pheno <- as.numeric(Data_Extract_Pheno) #convert string in numeric 
Data_Extract_Pheno <- (Data_Extract_Pheno - min(Data_Extract_Pheno)) / ( max(Data_Extract_Pheno) - min(Data_Extract_Pheno))




