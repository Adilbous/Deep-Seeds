# Deep-Seeds

Deep Learning applied to the prediction of plant's phenotype given their genetic markers (Genomic Selection).<br>
As of today, the industry uses a range of statistical and mathematical methods to predict plant varieties preformances and characteristics, such as BLUP-RR.<br> 
This undergoing project aims to apply Deep Learning methods to Genomic Selection and to overperform current industry practices.

## Content 

Each file is a step of the project, you can thus follow its evolution from step 1. to...

### Poplar Dataset

Poplar Dataset is an Open Source data of 562 samples, each having 7808 genetic markers (input data) and 8 phenotypic variables (labels or output data). <br>
We use this Dataset in the first steps of this project. 

### 1. DeepGS Library 

We use an Open Source R package, DeepGS (https://github.com/cma2015/DeepGS), based on the work of WENLONG MA ET AL. (2018) <br>
This library implements a Convolutional Neural Network (CNN) for Genomic Selection.

- DeepGS Network Poplar dataset.Rmd : R notebook that explains our approach using DeepGS on the Poplar dataset
- Input-Encoding-function-from-1-to-16.R : R script, used in the notebook, to translate the Markers data into a choosen representation 

### 2. Custom MLP approach (undergoing)

Custom Multi-Layer Perceptron (MLP) or dense neural network applied to Genomic Selection. 
Poplar dataset
