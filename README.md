# Deep-Seeds

Deep Learning applied to the prediction of plant's phenotype given their genetic markers (Genomic Selection).<br>
As of today, the industry uses a range of statistical and mathematical methods to predict plant varieties preformances and characteristics, such as BLUP.<br> 
This project aims to apply Deep Learning methods to Genomic Selection and to overperform current industry practices.

## Content 

Each file is a step of the project. 
1. DeepGS Library : application of an Open Source R package for Genomic Selection with Deep Learning
2. Multiple Regression MLP : custom MLP for the simultaneous prediction of 5 quantitatives variables (multiple regression)
3. Classification pipeline : features extraction using Partial-Least-Square Regression (PLS) and comparision of 3 classifiers (non-DL and CNN) 

### Poplar Dataset

Poplar Dataset is an Open Source data of 562 samples, each having 7808 genetic markers (input data) and 8 phenotypic variables (labels or output data). <br>
Among those 8 variables, 5 are quantitative and thus a regression problem, and the 3 others are qualitative so linked to a classification problem. <br>

### 1. DeepGS Library 

We use an Open Source R package, DeepGS (https://github.com/cma2015/DeepGS), based on the work of WENLONG MA ET AL. (2018) <br>
This library implements a Convolutional Neural Network (CNN) for Genomic Selection.

- DeepGS Network Poplar dataset.Rmd : R notebook that explains our approach using DeepGS on the Poplar dataset
- Input-Encoding-function-from-1-to-16.R : R script, used in the notebook, to translate the Markers data into a choosen representation 

### 2. Multiple Regression MLP

Custom Multi-Layers Perpectrons (or Dense Neural Network) for the simultaneous prediction of 5 quantitatives variables (multiple regression).<br>

- encoding_functions.R : functions to encode the genetic markers (one hot encoding and scalar encoding)
- model_regression_multiple.R : functions to build the Neural Network and other usefull methods for our model
- plots.R. : functions to plot the regression output and the R2 coefficient
- Notebook_Multivariate_Regression.Rmd : R notebook that uses the functions above to train a model and predict the phenotypic variables on a test set

### 3. Classification pipeline (PLS Regression + CNN) 

Features extraction using Partial-Least-Square Regression (PLS) and comparision of 3 classifiers : Suppot Vector Machines (SVM)), k-nearest neighbor (kNN),  and a Convolutional Neural Network (CNN)  

- Notebook_classification.Rmd : R notebook that explains our approach using PLS and the quoted classifiers on the Poplar dataset

