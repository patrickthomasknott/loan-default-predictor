# loan-default-predictor

Develop and evaluate a binomial regression generalised linear model to predict peronal loan defaults using a supplied dataset (BackgroundFiles/data.csv). Then, compare it to an existing model.

- load-default-predictor.R is the code for the initial investigation
- loan-default-predictor-report-generation.Rmd hardcodes the optimal hyper-parameters to summarise the process and results when generating REPORT-loan-default-predictor.html

Code overview:  
- explore and clean data  
- investigate predictive abilities of individual features  
- investigate correlation between feature pairs  
- perform stepwise feature removal and addition, using Gini, AIC and difference of deviance    
- evaluate optimal model in comparison to existing model   

NB this was a group project, but I've removed the other two names from the reports
