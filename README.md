#Hardware
AMD FX-4350 Quad-Core Processor, 8 GB RAM

#OS:  
Linux Mint 16

#3rd-party software
* R v3.0.1 (base libraries)
* rjson (R command to install: install.packages("rjson"))
* RStan v2.2, step-by-step installation guide: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

Note: The original data files from Kaggle (Output_TargetID_Mapping.csv, RepairTrain.csv, SaleTrain.csv) should be placed in the Data folder given in the SETTINGS.json file before running any of the following scripts.

#Training the model
Run train.R script, which will  
1.  Load dataset custom train.csv, if that file exists using functions from data generation.R. Otherwise first generates the dataset (using original datasets from Kaggle and functions from data generation.R) and then save to the file  
2.  Load stan model code (text specification of the model following somewhat similar syntax as in BUGS/JAGS) and compile the model usingstan model() function from RStan library  
3.  Learn model parameters using optimizing() function from RStan library  
4.  Save parameters  

#Making predictions:
Run predict.R script, which will
1.  Load dataset custom test.csv following similar logic as in the 1. training  
2.  Load learned parameters from the file    
3.  Calculate predictions for module-component-sale times  
4.  Aggregate predictions for module-components  
5.  Merge predictions with Output TargetID Mapping.csv to make submission file  
6.  Save submissions  
