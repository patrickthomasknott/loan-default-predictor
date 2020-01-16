library(readr)
library(caret)
library(pROC)
library(ROCR)
dataset <- read_csv("BackgroundFiles/data.csv")

# remove the first two rows
dataset <- dataset[-c(1, 2), ]
# make factors
dataset$term <- as.factor(dataset$term)
dataset$emp_length <- as.factor(dataset$emp_length)
dataset$home_ownership <- as.factor(dataset$home_ownership)
dataset$purpose <- as.factor(dataset$purpose)

#### Word & correlation based predictor reduction----

# useful from words:
dataset <-
  dataset[, c(40, 4, 7, 9, 12, 13, 14, 18,  21, 22, 24, 27,  30, 41)]
# put repay_fail as first column
names(dataset)

# Count NA's for each predictor
missing.count <-
  as.data.frame(matrix(data = NA, ncol = length(dataset)))
colnames(missing.count) <- names(dataset)
for (i in 1:length(dataset)) {
  missing.count[1, i] <- sum(is.na(dataset[, i]))
}

# remove subject with a missing value from annual income
dataset <- dataset[!is.na(dataset$annual_inc), ]

# convert emp_length NA's into a new level & check significance
dataset$emp_length <- addNA(dataset$emp_length)
levels(dataset$emp_length)[12] <- "missing"
dataset$emp_length <- factor(dataset$emp_length,levels(dataset$emp_length)[c(12,1,2,4:11,3)])


summary(glm(repay_fail~emp_length, data = dataset, family = "binomial")) #missing is important
pdf("BackgroundFiles/Boxplot of Annual Income.pdf")
boxplot(dataset$annual_inc, main = "Boxplot of Annual Income")
dev.off()
#### Compare annual income as continuous against factor----
dataset$fac.annual_inc <- cut(dataset$annual_inc, breaks = quantile(dataset$annual_inc, seq(0, 1, .1)))


## compare full model with continuous vs factor annual income
fit.cont.annual_inc <-  glm(paste("repay_fail~", (paste(names(dataset)[-c(1, 15, 16)], collapse = "+"))),data = dataset,na.action = na.exclude,family = "binomial")

fit.fac.annual_inc <-  glm(paste("repay_fail~", (paste(names(dataset)[-c(1, 7, 16)], collapse = "+"))),data = dataset,na.action = na.exclude,family = "binomial")



summary(fit.cont.annual_inc)
summary(fit.fac.annual_inc) # factor AIC better, all but one level significant

### compare predictive ability of continuous vs factor annual income
prediction.set.cont.annual_inc = predict(fit.cont.annual_inc, newdata = dataset, type = "response")
prediction.cont.annual_inc = prediction(prediction.set.cont.annual_inc, dataset$repay_fail)
# Gini
Gini.cont.annual_inc <- 2 * as.numeric(performance(prediction.cont.annual_inc, "auc")@y.values) - 1

prediction.set.fac.annual_inc = predict(fit.fac.annual_inc, newdata = dataset, type = "response")
prediction.fac.annual_inc = prediction(prediction.set.fac.annual_inc, dataset$repay_fail)
# Gini
Gini.fac.annual_inc <- 2 * as.numeric(performance(prediction.fac.annual_inc, "auc")@y.values) - 1

prediction.set.log.annual_inc = predict(fit.log.annual_inc, newdata = dataset, type = "response")
prediction.log.annual_inc = prediction(prediction.set.log.annual_inc, dataset$repay_fail)
# Gini
Gini.log.annual_inc <- 2 * as.numeric(performance(prediction.log.annual_inc, "auc")@y.values) - 1


Gini.cont.annual_inc
Gini.fac.annual_inc

# Drop continuous and log annual income
dataset <- dataset[,-7]


#### Check for correlations of covariates from manually culled data ----
names(dataset[c(2, 4, 8, 9, 10, 11, 12, 13)])
# "loan_amnt""installment" "dti"  "delinq_2yrs""inq_last_6mths" "pub_rec""total_acc" "time_since_first_loan"
covariate.indicator <- c(2, 4, 8, 9, 10, 11, 12, 13)

cmat <-
  cor(dataset[, covariate.indicator], use = "pairwise.complete.obs")
cmat.binary <-
  as.data.frame(matrix(
    as.numeric(abs(cmat) > 0.85),
    length(covariate.indicator),
    length(covariate.indicator)
  ))
colnames(cmat.binary) <- names(dataset[covariate.indicator])
rownames(cmat.binary) <- names(dataset[covariate.indicator])
cmat.binary[rowSums(cmat.binary) > 1, colSums(cmat.binary) > 1]
# 2 predictors are correlated >.85, keep loan_amnt not installmennt



# Remove installment
dataset <- dataset[-4]
covariate.indicator <- c(2,  7:12)
names(dataset)
names(dataset)[covariate.indicator]


#### Univariate p-values----
univariate.pvalues <- vector(length=length(dataset)-1)
for (i in 2:length(names(dataset))) {
  fit <-
    glm(
      paste("repay_fail~", (paste(names(
        dataset
      )[i]))),
      data = dataset,
      na.action = na.exclude,
      family = "binomial"
    )
  univariate.pvalues[i - 1] <- signif(sum(summary(fit)$coefficients[,4]<.05)/(length(summary(fit)$coefficients[,4]))*100, 2)
}

names(univariate.pvalues) <- names(dataset)[-1] # all highly significant
write.csv(univariate.pvalues, file = "BackgroundFiles/percent.significant.univariate.pvalues.csv")

#### Univariate plots----
# Plot Covariates
pdf("BackgroundFiles/univariate.predictor.plots.pdf")
y <- dataset$repay_fail
for (i in covariate.indicator) {
  x <- as.numeric(dataset[[i]])
  ifelse(i == 8, # delinq_2yrs integers 0:11
         g <- cut(x, breaks = c(1:11)),
         ifelse(
           i == 9, # inq_last_6mths integers 0:33
           g <- cut(x, breaks = seq(1, 33, 33 / 10)),
           ifelse(i == 10, # pub_rec integers 0:5
                  g <- cut(x, breaks = seq(0, 5, 1)),
                  g <-cut(x, breaks = quantile(x, seq(0, 100, 10) / 100))
           )))
  ym <- tapply(y, g, mean)
  xm <- tapply(x, g, mean)
  mainTitle <- ifelse(i == 8,paste("Repay Failure Rate of ", names(dataset)[i], "\n 9% Bins", sep = ""),ifelse(i == 10,paste("Repay Failure Rate of ", names(dataset)[i], "\n 20% Bins", sep = ""),paste("Repay Failure Rate of ", names(dataset)[i], "\n 10% Bins", sep = "")))
  # plot(xm,ym,xlab = names(dataset)[i],ylab = "Proportion of Defaulters",main = mainTitle,pch = 19,col = "red")
  ymp <- log(ym / (1 - ym))
  plot(xm,ymp,xlab = names(dataset)[i],ylab = "Logit of Proportion of Defaulters",
       main = ifelse(i==10, paste("Repay Failure Rate of ", names(dataset)[i], "\n 20% Bins, ", "3 Predictions Inf", sep = ""), mainTitle),pch = 19,col = "blue")
  abline(lm(ymp[is.finite(ymp)]~xm[is.finite(ymp)]))
}

# Factor plots
#### Factor plots----
# Factors into tables, each column is percent of subjects of that level who did not, then did refault
y<-dataset$repay_fail
q <- colSums(table(y,dataset$term))
q <- rbind(q,q)
barplot(signif(table(y,dataset$term)/q, 3)*100,col=c("darkblue","white"), ylab = "% Repaid", main = "Percent of Loans Repaid by Loan Term")

q <- colSums(table(y,dataset$emp_length))
q <- rbind(q,q)
barplot(signif(table(y,dataset$emp_length)/q, 3)*100,col=c("darkblue","white"), ylab = "% Repaid", main = "Percent of Loans Repaid by Employment Length")

q <- colSums(table(y,dataset$home_ownership))
q <- rbind(q,q)
barplot(signif(table(y,dataset$home_ownership)/q, 3)*100,col=c("darkblue","white"), ylab = "% Repaid", main = "Percent of Loans Repaid by Home Ownership Status")

q <- colSums(table(y,dataset$purpose))
q <- rbind(q,q)
barplot(signif(table(y,dataset$purpose)/q, 3)*100,col=c("darkblue","white"), ylab = "% Repaid", main = "Percent of Loans Repaid by Loan Purpose", xaxt = "n",sub = "car, credit_card, debt_consolidation, educational, home_improvement, house,\n major_purchase, medical, moving, other, renewable_energy, small_business, vacation") 

q <- colSums(table(y,dataset$fac.annual_inc))
q <- rbind(q,q)
barplot(signif(table(y,dataset$fac.annual_inc)/q, 3)*100,col=c("darkblue","white"), ylab = "% Repaid", main = "Percent of Loans Repaid by Income", xaxt = "n", xlab = "Annual Income in 1000's\n(1.9,30) (30, 37) (37, 44.4) (44.4, 50) (50, 58.6)\n (58.6,65.3) (65.3,75.3) (75.3,90) (90,116) (116,6000)")
dev.off()
#### split into 75% train, 25% test----
train.index <-
  createDataPartition(dataset$repay_fail,  p = .75, list = FALSE)
train <- dataset[train.index,]
train.full <-train # for putting predictors back in after they've been removed from train subset
test <- dataset[-train.index,]
sum(train$repay_fail) / length(train$repay_fail)
sum(test$repay_fail) / length(test$repay_fail)

#### Interactions () ----

Gini.interaction <- matrix(nrow=100, ncol=2)
counter <- 1
for (i in 2:12){
  for (j in (i+1):13){
    fit.model<-glm(paste("repay_fail~", (paste(names(dataset)[i],names(dataset)[j], sep =":"))),data = dataset,na.action = na.exclude, family = "binomial"(link = "logit"))
    
    # predict the testing subset
    prediction.set = predict(fit.model, newdata = dataset, type = "response")
    
    # Gini and ROC plot
    pred1 = prediction(prediction.set, dataset$repay_fail)
    test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
    Gini.interaction[counter, 1] <- paste(names(dataset)[i],names(dataset)[j], sep =":")
    Gini.interaction[counter, 2] <- 2 * test.AUC - 1
    
    counter <- counter +1 
  }
}


write.csv(Gini.interaction, "BackgroundFiles/Interaction Gini.csv")

#### Create function for deciding whether to REMOVE a predictor----
Gini.remove.predictor <-
  function(train,
           Gini.model.summary,
           Gini.model.summary.counter,
           Gini.model.pvalues.list) {
    Gini.train.model.check.df <-
      as.data.frame(matrix(nrow = dim(train)[2] - 1, ncol = 4))
    colnames(Gini.train.model.check.df) <-
      c("Dropped Predictor", "Gini", "AIC", "Column Index")
    
    # store the pvalues of each model so we can pick out the p-values of the best model
    temp.pvalues.list <- list()
    for (i in 2:dim(train)[2]) {
      fit <-
        glm(paste("repay_fail~", (paste(
          names(train)[-c(1, i)], collapse = "+"
        ))),
        data = train,
        na.action = na.exclude)
      prob = predict(fit, type = c("response"))
      g <- roc(repay_fail ~ prob, data = train)
      Gini.train.model.check.df[i - 1, 1] <- names(train)[i]
      # calculate Gini
      Gini.train.model.check.df[i - 1, 2] <- 2 * g$auc - 1
      # AIC
      Gini.train.model.check.df[i - 1, 3] <- fit$aic
      # so we know the position in the list of columns of the predictor to drop
      Gini.train.model.check.df[i - 1, 4] <- i
      temp.pvalues.list[[i]] <- summary(fit)$coefficients[, 4]
    }
    
    Gini.model.summary.counter <- Gini.model.summary.counter + 1
    
    # so largest Gini is at the top
    Gini.train.model.check.df.ascending <-
      Gini.train.model.check.df[order(Gini.train.model.check.df[, 2], decreasing = TRUE),]
    
    #### copy results to Gini.model.summary
    Gini.biggest.index <- Gini.train.model.check.df.ascending[1, 4]
    # copy models predictors
    Gini.model.summary[Gini.model.summary.counter, 1] <-
      paste(names(train)[-c(1, Gini.biggest.index)], collapse = "+")
    # copy models Gini
    Gini.model.summary[Gini.model.summary.counter, 2] <-
      Gini.train.model.check.df.ascending[1, 2]
    # copy AIC
    Gini.model.summary[Gini.model.summary.counter, 3] <-
      Gini.train.model.check.df.ascending[1, 3]
    # copy that this is a predictor REDUCTION to summary
    Gini.model.summary[Gini.model.summary.counter, 4] <-
      "Reduction"
    # what predictor is being assessed
    Gini.model.summary[Gini.model.summary.counter, 5] <-
      Gini.train.model.check.df.ascending[1, 1]
    # add blank lines to the console window for easier navigation
    temp.list <-
      list(
        Gini.model.summary,
        Gini.model.summary.counter,
        Gini.biggest.index,
        Gini.train.model.check.df.ascending[1, 1],
        temp.pvalues.list[[Gini.biggest.index]]
      )
    return(temp.list)
  }
#### Create function for deciding whether to ADD a predictor----
Gini.add.predictor <-
  function(train,
           Gini.model.summary,
           Gini.model.summary.counter,
           Gini.removed.predictors,
           Gini.model.pvalues.list) {
    Gini.train.model.check.df <-
      as.data.frame(matrix(nrow = length(Gini.removed.predictors), ncol = 4))
    colnames(Gini.train.model.check.df) <-
      c("Added Predictor",
        "Gini",
        "AIC",
        "Vector index of added predictor")
    
    # store the pvalues of each model so we can pick out the p-values of the best model
    temp.pvalues.list <- list()
    for (i in 1:length(Gini.removed.predictors)) {
      # build model with one the removed predictors
      fit <-
        glm(
          paste("repay_fail~", (paste(
            names(train)[-1], collapse = "+"
          )), "+", Gini.removed.predictors[i]),
          data = train.full,
          na.action = na.exclude
        )
      
      prob = predict(fit, type = c("response"))
      g <- roc(repay_fail ~ prob, data = train.full)
      # copy added predictor name
      Gini.train.model.check.df[i, 1] <- Gini.removed.predictors[i]
      # calculate and copy Gini
      Gini.train.model.check.df[i, 2] <- 2 * g$auc - 1
      # calculate and copy AIC
      Gini.train.model.check.df[i, 3] <- fit$aic
      #  copy vector index
      Gini.train.model.check.df[i, 4] <- i
      temp.pvalues.list[[i]] <- summary(fit)$coefficients[, 4]
    }
    
    Gini.model.summary.counter <- Gini.model.summary.counter + 1
    
    # so largest Gini is at the top
    Gini.train.model.check.df.ascending <-
      Gini.train.model.check.df[order(Gini.train.model.check.df[, 2], decreasing = TRUE),]
    
    #### copy results to Gini.model.summary
    Gini.biggest.index <- Gini.train.model.check.df.ascending[1, 4]
    # copy models predictors
    Gini.model.summary[Gini.model.summary.counter, 1] <-
      paste((paste(names(train)[-1], collapse = "+")), "+", Gini.removed.predictors[Gini.biggest.index])
    # copy models Gini
    Gini.model.summary[Gini.model.summary.counter, 2] <-
      Gini.train.model.check.df.ascending[1, 2]
    # copy models AIC
    Gini.model.summary[Gini.model.summary.counter, 3] <-
      Gini.train.model.check.df.ascending[1, 3]
    # copy that this is a predictor ADDITION to summary
    Gini.model.summary[Gini.model.summary.counter, 4] <- "Addition"
    # what predictor is being assessed
    Gini.model.summary[Gini.model.summary.counter, 5] <-
      Gini.train.model.check.df.ascending[1, 1]
    temp.list <-
      list(
        Gini.model.summary,
        Gini.model.summary.counter,
        Gini.biggest.index,
        Gini.train.model.check.df.ascending[1, 1],
        temp.pvalues.list[[Gini.biggest.index]]
      )
    return(temp.list)
  }
# temp list elements:
# temp[[1]] = Gini.model.summary
# temp[[2]] = Gini.model.summary.counter
# temp[[3]] = Gini.biggest.index
# temp[[4]] = name of predictor being assessed
# temp[[5]] = list of coefficients and their p-values

#### Gini-based comparison prep----
# Create summary of models and Gini
Gini.model.summary <-
  matrix(
    data = NA,
    nrow = 20,
    ncol = 5,
    byrow = TRUE
  )
colnames(Gini.model.summary) <-
  c(
    "Model",
    "Gini",
    "AIC",
    "Reducing or adding a predictor?",
    "Predictor being assessed"
  )
Gini.model.pvalues.list <- list()
Gini.model.summary.counter <- 1

# to keep track of what we've removed for when we try adding in individual predictors later
Gini.removed.predictors <- vector(mode = "character", length = 0)


# fit the model with all current predictors before we start to drop out one at a time
fit <-
  glm(paste("repay_fail~", (paste(
    names(train)[-1], collapse = "+"
  ))),
  data = train,
  na.action = na.exclude)
prob = predict(fit, type = c("response"))
g <- roc(repay_fail ~ prob, data = train)
Gini.model.summary[1, 1] <- "Full model"
# calculate Gini and copy to summary
Gini.model.summary[1, 2] <- 2 * g$auc - 1
# AIC
Gini.model.summary[1, 3] <- fit$aic
Gini.model.summary[1, 4] <- "Nil"
Gini.model.summary[1, 5] <- "Nil"
Gini.model.pvalues.list[[Gini.model.summary.counter]] <-
  summary(fit)$coefficients[, 4]

#### 1st Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]

#### 2nd Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]
#### 3rd Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]

#### 4th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]

#### 5th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]


#### 1st Gini-based predictor ADDITION cycle  ----

# call the predictor adding function
temp <-
  Gini.add.predictor(train, Gini.model.summary,
                     Gini.model.summary.counter,
                     Gini.removed.predictors,
                     Gini.model.pvalues.list)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# Re-add the predictor (if necessary)

# # If adding it in
# # calculate position of releveant column in train.full
# train.full.index <- match(Gini.removed.predictors[biggest.Gini.index], names( train.full))
# # add the predictor back to the train subset
# train <- append(train, train.full[,train.full.index] )
# # check names of predictors in train subset
# names(train)[-1]
# # Cut out the removed predictors name from the removed.predictor vector
# Gini.removed.predictors <- Gini.removed.predictors[-biggest.Gini.index]






#### 6th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]
#### 7th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]

#### 8th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]

#### 9th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]


#### 10th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]


#### 2nd Gini-based predictor ADDITION cycle  ----

# call the predictor adding function
temp <-
  Gini.add.predictor(train, Gini.model.summary,
                     Gini.model.summary.counter,
                     Gini.removed.predictors,
                     Gini.model.pvalues.list)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# Re-add the predictor (if necessary)

# # If adding it in
# # calculate position of releveant column in train.full
# train.full.index <- match(Gini.removed.predictors[biggest.Gini.index], names( train.full))
# # add the predictor back to the train subset
# train <- append(train, train.full[,train.full.index] )
# # check names of predictors in train subset
# names(train)[-1]
# # Cut out the removed predictors name from the removed.predictor vector
# Gini.removed.predictors <- Gini.removed.predictors[-biggest.Gini.index]






#### 11th Gini-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  Gini.remove.predictor(train, Gini.model.summary, Gini.model.summary.counter)

# update variables outside the function environment
Gini.model.summary <- temp[[1]]
Gini.model.summary.counter <- temp[[2]]
biggest.Gini.index <- temp[[3]]
Gini.model.pvalues.list[[Gini.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
Gini.removed.predictors[length(Gini.removed.predictors) + 1] <-
  names(train[, biggest.Gini.index])
# drop the predictor
train <- train[, -c(biggest.Gini.index)]
# check names of remaining predictors
names(train)[-1]

###### export model building summary, Rdata  & model p-values----
write.csv(Gini.model.summary, "BackgroundFiles/GiniModelBuildingSummary.csv")
capture.output(Gini.model.pvalues.list, file = "BackgroundFiles/GiniModelBuildingPValues.txt")
train <- train.full
### Create function for deciding whether to REMOVE a predictor----
AIC.remove.predictor <-
  function(train,
           AIC.model.summary,
           AIC.model.summary.counter,
           AIC.model.pvalues.list) {
    AIC.train.model.check.df <-
      as.data.frame(matrix(nrow = dim(train)[2] - 1, ncol = 4))
    colnames(AIC.train.model.check.df) <-
      c("Dropped Predictor", "Gini", "AIC", "Column Index")
    
    # store the pvalues of each model so we can pick out the p-values of the best model
    temp.pvalues.list <- list()
    for (i in 2:dim(train)[2]) {
      fit <-
        glm(paste("repay_fail~", (paste(
          names(train)[-c(1, i)], collapse = "+"
        ))),
        data = train,
        na.action = na.exclude)
      prob = predict(fit, type = c("response"))
      g <- roc(repay_fail ~ prob, data = train)
      AIC.train.model.check.df[i - 1, 1] <- names(train)[i]
      # calculate Gini
      AIC.train.model.check.df[i - 1, 2] <- 2 * g$auc - 1
      # AIC
      AIC.train.model.check.df[i - 1, 3] <- fit$aic
      # so we know the position in the list of columns of the predictor to drop
      AIC.train.model.check.df[i - 1, 4] <- i
      temp.pvalues.list[[i]] <- summary(fit)$coefficients[, 4]
    }
    
    AIC.model.summary.counter <- AIC.model.summary.counter + 1
    
    # so largest Gini is at the top
    AIC.train.model.check.df.descending <-
      AIC.train.model.check.df[order(AIC.train.model.check.df[, 3], decreasing = FALSE),]
    
    #### copy results to AIC.model.summary
    AIC.smallest.index <- AIC.train.model.check.df.descending[1, 4]
    # copy models predictors
    AIC.model.summary[AIC.model.summary.counter, 1] <-
      paste(names(train)[-c(1, AIC.smallest.index)], collapse = "+")
    # copy models Gini
    AIC.model.summary[AIC.model.summary.counter, 2] <-
      AIC.train.model.check.df.descending[1, 2]
    # copy AIC
    AIC.model.summary[AIC.model.summary.counter, 3] <-
      AIC.train.model.check.df.descending[1, 3]
    # copy that this is a predictor REDUCTION to summary
    AIC.model.summary[AIC.model.summary.counter, 4] <-
      "Reduction"
    # what predictor is being assessed
    AIC.model.summary[AIC.model.summary.counter, 5] <-
      AIC.train.model.check.df.descending[1, 1]
    
    temp.list <-
      list(
        AIC.model.summary,
        AIC.model.summary.counter,
        AIC.smallest.index,
        AIC.train.model.check.df.descending[1, 1],
        temp.pvalues.list[[AIC.smallest.index]]
      )
    return(temp.list)
  }
### Create function for deciding whether to ADD a predictor----
AIC.add.predictor <-
  function(train,
           AIC.model.summary,
           AIC.model.summary.counter,
           AIC.removed.predictors,
           AIC.model.pvalues.list) {
    AIC.train.model.check.df <-
      as.data.frame(matrix(nrow = length(AIC.removed.predictors), ncol = 4))
    colnames(AIC.train.model.check.df) <-
      c("Added Predictor",
        "Gini",
        "AIC",
        "Vector index of added predictor")
    
    # store the pvalues of each model so we can pick out the p-values of the best model
    temp.pvalues.list <- list()
    for (i in 1:length(AIC.removed.predictors)) {
      # build model with one the removed predictors
      fit <-
        glm(
          paste("repay_fail~", (paste(
            names(train)[-1], collapse = "+"
          )), "+", AIC.removed.predictors[i]),
          data = train.full,
          na.action = na.exclude
        )
      
      prob = predict(fit, type = c("response"))
      g <- roc(repay_fail ~ prob, data = train.full)
      # copy added predictor name
      AIC.train.model.check.df[i, 1] <- AIC.removed.predictors[i]
      # calculate and copy Gini
      AIC.train.model.check.df[i, 2] <- 2 * g$auc - 1
      # calculate and copy AIC
      AIC.train.model.check.df[i, 3] <- fit$aic
      #  copy vector index
      AIC.train.model.check.df[i, 4] <- i
      temp.pvalues.list[[i]] <- summary(fit)$coefficients[, 4]
    }
    
    AIC.model.summary.counter <- AIC.model.summary.counter + 1
    
    # so largest Gini is at the top
    AIC.train.model.check.df.descending <-
      AIC.train.model.check.df[order(AIC.train.model.check.df[, 3], decreasing = FALSE),]
    
    #### copy results to AIC.model.summary
    AIC.smallest.index <- AIC.train.model.check.df.descending[1, 4]
    # copy models predictors
    AIC.model.summary[AIC.model.summary.counter, 1] <-
      paste((paste(names(train)[-1], collapse = "+")), "+", AIC.removed.predictors[AIC.smallest.index])
    # copy models Gini
    AIC.model.summary[AIC.model.summary.counter, 2] <-
      AIC.train.model.check.df.descending[1, 2]
    # copy models AIC
    AIC.model.summary[AIC.model.summary.counter, 3] <-
      AIC.train.model.check.df.descending[1, 3]
    # copy that this is a predictor ADDITION to summary
    AIC.model.summary[AIC.model.summary.counter, 4] <- "Addition"
    # what predictor is being assessed
    AIC.model.summary[AIC.model.summary.counter, 5] <-
      AIC.train.model.check.df.descending[1, 1]
    temp.list <-
      list(
        AIC.model.summary,
        AIC.model.summary.counter,
        AIC.smallest.index,
        AIC.train.model.check.df.descending[1, 1],
        temp.pvalues.list[[AIC.smallest.index]]
      )
    return(temp.list)
  }

train <- train.full # to put back all the predictors removed through the Gini-based cull
########### AIC-based comparison prep----
# Create summary of models and AIC
AIC.model.summary <-
  matrix(
    data = NA,
    nrow = 20,
    ncol = 5,
    byrow = TRUE
  )
colnames(AIC.model.summary) <-
  c(
    "Model",
    "Gini",
    "AIC",
    "Reducing or adding a predictor?",
    "Predictor being assessed"
  )
AIC.model.pvalues.list <- list()
AIC.model.summary.counter <- 1

# to keep track of what we've removed for when we try adding in individual predictors later
AIC.removed.predictors <- vector(mode = "character", length = 0)


# fit the model with all current predictors before we start to drop out one at a time
fit <-
  glm(paste("repay_fail~", (paste(
    names(train)[-1], collapse = "+"
  ))),
  data = train,
  na.action = na.exclude)
prob = predict(fit, type = c("response"))
g <- roc(repay_fail ~ prob, data = train)
AIC.model.summary[1, 1] <- "Full model"
# calculate Gini and copy to summary
AIC.model.summary[1, 2] <- 2 * g$auc - 1
# AIC
AIC.model.summary[1, 3] <- fit$aic
AIC.model.summary[1, 4] <- "Nil"
AIC.model.summary[1, 5] <- "Nil"
AIC.model.pvalues.list[[AIC.model.summary.counter]] <-
  summary(fit)$coefficients[, 4]

########### 1st AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 2nd AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 3rd AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 4th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 5th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 1st AIC-based predictor ADDITION cycle  ----

# call the predictor adding function
temp <-
  AIC.add.predictor(train, AIC.model.summary,
                    AIC.model.summary.counter,
                    AIC.removed.predictors,
                    AIC.model.pvalues.list)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# Re-add the predictor (if necessary)

# # If adding it in
# # calculate position of releveant column in train.full
# train.full.index <- match(AIC.removed.predictors[biggest.AIC.index], names( train.full))
# # add the predictor back to the train subset
# train <- append(train, train.full[,train.full.index] )
# # check names of predictors in train subset
# names(train)[-1]
# # Cut out the removed predictors name from the removed.predictor vector
# AIC.removed.predictors <- AIC.removed.predictors[-biggest.AIC.index]






########### 6th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 7th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 8th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 9th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 10th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


########### 2nd AIC-based predictor ADDITION cycle  ----

# call the predictor adding function
temp <-
  AIC.add.predictor(train, AIC.model.summary,
                    AIC.model.summary.counter,
                    AIC.removed.predictors,
                    AIC.model.pvalues.list)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# Re-add the predictor (if necessary)

# # If adding it in
# # calculate position of releveant column in train.full
# train.full.index <- match(AIC.removed.predictors[biggest.AIC.index], names( train.full))
# # add the predictor back to the train subset
# train <- append(train, train.full[,train.full.index] )
# # check names of predictors in train subset
# names(train)[-1]
# # Cut out the removed predictors name from the removed.predictor vector
# AIC.removed.predictors <- AIC.removed.predictors[-biggest.AIC.index]






########### 11th AIC-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  AIC.remove.predictor(train, AIC.model.summary, AIC.model.summary.counter)

# update variables outside the function environment
AIC.model.summary <- temp[[1]]
AIC.model.summary.counter <- temp[[2]]
biggest.AIC.index <- temp[[3]]
AIC.model.pvalues.list[[AIC.model.summary.counter]] <- temp[[5]]

# copy the removed predictors name to the removed.predictor vector so it can be used later
AIC.removed.predictors[length(AIC.removed.predictors) + 1] <-
  names(train[, biggest.AIC.index])
# drop the predictor
train <- train[, -c(biggest.AIC.index)]
# check names of remaining predictors
names(train)[-1]


##### export model building summary, Rdata  & model p-values----
write.csv(AIC.model.summary, "BackgroundFiles/AICmodelBuildingSummary.csv")
capture.output(AIC.model.pvalues.list, file = "BackgroundFiles/AICmodelBuildingPValues.txt")
train <- train.full # to put back all the predictors removed through the AIC-based cull

#### Create function for deciding whether to REMOVE a predictor----
diffDev.remove.predictor <-
  function(train,
           diffDev.model.summary,
           diffDev.model.summary.counter) {
    # working summary for each for loop
    diffDev.train.model.check.df <-
      as.data.frame(matrix(nrow = dim(train)[2] - 1, ncol = 7))
    colnames(diffDev.train.model.check.df) <- c("Model", "Bigger model deviance","Bigger model d.f", "Smaller model deviance", "Smaller model d.f.","p-value", "Column Index")
    
    # Fit big model
    fit.big <- glm(paste("repay_fail~", (paste(names(train)[-1], collapse = "+"))), data = train, na.action = na.exclude)
    
    # big model deviance
    diffDev.train.model.check.df[i - 1, 2] <- dev.big <- fit.big$deviance
    
    # big model d.f.
    diffDev.train.model.check.df[i - 1, 3] <- df.big <- fit.big$df.null - fit.big$df.residual
    
    # For loop to fit each small model
    for (i in 2:dim(train)[2]) {
      
      fit.small <- glm(paste("repay_fail~", (paste(names(train)[-c(1, i)], collapse = "+"))),data = train, na.action = na.exclude)
      # copy predictor names to working summary
      diffDev.train.model.check.df[i - 1, 1] <- names(train)[i]
      
      # small model deviance
      diffDev.train.model.check.df[i - 1, 4] <- dev.small <- fit.small$deviance
      
      # small model d.f.
      diffDev.train.model.check.df[i - 1, 5] <- df.small <- fit.small$df.null - fit.small$df.residual
      # pvalue
      diffDev.train.model.check.df[i - 1, 6] <- 1-pchisq(dev.small-dev.big,df=df.big-df.small)
      # so we know the position in the list of columns of the predictor to drop
      diffDev.train.model.check.df[i - 1, 7] <- i
    }
    
    diffDev.model.summary.counter <- diffDev.model.summary.counter + 1
    
    # so largest p value is at the top
    diffDev.train.model.check.df.ascending <-
      diffDev.train.model.check.df[order(diffDev.train.model.check.df[, 6], decreasing = TRUE),]
    
    #### copy results to diffDev.model.summary
    diffDev.biggest.index <- diffDev.train.model.check.df.ascending[1, 7]
    # copy models predictors
    diffDev.model.summary[diffDev.model.summary.counter, 1] <-
      paste(names(train)[-c(1, diffDev.biggest.index)], collapse = "+")
    # copy models pvalue
    diffDev.model.summary[diffDev.model.summary.counter, 2] <-
      diffDev.train.model.check.df.ascending[1, 6]
    # copy that this is a predictor REDUCTION to summary
    diffDev.model.summary[diffDev.model.summary.counter, 3] <-
      "Reduction"
    # what predictor is being assessed
    diffDev.model.summary[diffDev.model.summary.counter, 4] <-
      diffDev.train.model.check.df.ascending[1, 1]
    
    temp.list <-
      list(
        diffDev.model.summary,
        diffDev.model.summary.counter,
        diffDev.biggest.index)
    return(temp.list)
  }
#### Create function for deciding whether to ADD a predictor

#### DD-based comparison prep----
train <- train.full
# Create summary of models and p-values
diffDev.model.summary <-
  matrix(
    data = NA,
    nrow = 20,
    ncol = 4,
    byrow = TRUE
  )
colnames(diffDev.model.summary) <-
  c(
    "Model",
    "p-value",
    "Reducing or adding a predictor?",
    "Predictor being assessed"
  )

diffDev.model.summary.counter <- 1

# to keep track of what we've removed for when we try adding in individual predictors later
diffDev.removed.predictors <- vector(mode = "character", length = 0)

#### 1st diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]



#### 2nd diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]



#### 3rd diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]



#### 4th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]



#### 5th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### 6th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### 7th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### 8th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### 9th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### 10th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### 11th diffDev-based predictor REMOVAL cycle  ----

# call the predictor removal function
temp <-
  diffDev.remove.predictor(train, diffDev.model.summary, diffDev.model.summary.counter)

# update variables outside the function environment
diffDev.model.summary <- temp[[1]]
diffDev.model.summary.counter <- temp[[2]]
biggest.diffDev.index <- temp[[3]]


# copy the removed predictors name to the removed.predictor vector so it can be used later
diffDev.removed.predictors[length(diffDev.removed.predictors) + 1] <-
  names(train[, biggest.diffDev.index])
# drop the predictor
train <- train[, -c(biggest.diffDev.index)]
# check names of remaining predictors
names(train)[-1]


#### export model building summary, Rdata  & model p-values----
write.csv(diffDev.model.summary, "BackgroundFiles/DiffDevModelBuildingSummary.csv")

repeatability.mx <- matrix(nrow=1, ncol=10)
for (i in 1:20){
  #### split into 75% train, 25% test----
  train.index <-
    createDataPartition(dataset$repay_fail,  p = .75, list = FALSE)
  train <- dataset[train.index,]
  train.full <-
    train # for putting predictors back in after they've been removed from train subset
  test <- dataset[-train.index,]
  fit.model <-
    glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc,
        data = train.full,
        na.action = na.exclude,
        family = "binomial"(link = "logit"))
  
  # predict the testing subset
  prediction.set = predict(fit.model, newdata = test, type = "response")
  
  # Gini and ROC plot
  pred1 = prediction(prediction.set, test$repay_fail)
  test.GINI <- signif(2*as.numeric(performance(pred1, "auc")@y.values)-1, 4)
  repeatability.mx[i]<-test.GINI
}
#### Repeatability----
avg.repeatability <-100*repeatability.mx/mean(repeatability.mx)
plot(1:20, avg.repeatability, xaxt = "n", xlab = "", ylab = "", main = "Gini as Percent of Average Gini", pch=19, col="red")
abline(100, 0)
#### K10 fold, seven best models----
indexKFOLD <-
  createDataPartition(dataset$repay_fail, times = 10,  p = .75, list = FALSE)

KFOLD.mx<-matrix(nrow=8, ncol=12)

for (k in 1:10){ 
  train.setKFOLD <- dataset[indexKFOLD[,k],]
  test.setKFOLD <- dataset[-indexKFOLD[,k],]
  
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~loan_amnt+term+emp_length+purpose+dti+inq_last_6mths+pub_rec+fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[1 , k+1] <- 2 * test.AUC - 1
  
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~term+emp_length+purpose+dti+inq_last_6mths+pub_rec+fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[2 , k+1] <- 2 * test.AUC - 1
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~term+purpose+dti+inq_last_6mths+pub_rec+fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[3 , k+1] <- 2 * test.AUC - 1
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~term+purpose+inq_last_6mths+pub_rec+fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[ 4, k+1] <- 2 * test.AUC - 1
  
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[ 5, k+1] <- 2 * test.AUC - 1
  
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~term+inq_last_6mths+fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[ 6, k+1] <- 2 * test.AUC - 1
  
  #~~~~~~~~~~~~~~~~~~~~~
  fit.model <- glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc+term:purpose+term:fac.annual_inc, data = train.setKFOLD,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.model, newdata = test.setKFOLD, type = "response")
  pred1 = prediction(prediction.set, test.setKFOLD$repay_fail)
  test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
  KFOLD.mx[7 , k+1] <- 2 * test.AUC - 1
  
  KFOLD.mx[1 , 12] <- mean(KFOLD.mx[1,2:11])
  
}
for (i in 1:7){KFOLD.mx[i,12] <- (KFOLD.mx[i,2]+KFOLD.mx[i,3]+KFOLD.mx[i,4]+KFOLD.mx[i,5]+KFOLD.mx[i,6]+KFOLD.mx[i,7]+KFOLD.mx[i,8]+KFOLD.mx[i,9]+KFOLD.mx[i,10])/10}

print(KFOLD.mx[c(5,7),12])
write.csv(KFOLD.mx, "BackgroundFiles/K.10.FOLD.mx.csv")
#### Create 4 subset of training data for cross validation----
fold.index <- createDataPartition(train.full$repay_fail, times = 4, p = .75, list = FALSE)

#### fit model with train subsets, predict with test subsets
cv.models <- c(5,6,8,9,10,11) # select best models

test.Gini.mx <-matrix(data = NA, nrow = 13, ncol = 6)

for (i in cv.models) { #### can't use model 1
  test.Gini.mx[i, 1] <- paste(Gini.model.summary[i, 1], sep ="")
  for (k in 1:4){ 
    
    # Create the training and testing subsets for each fold
    train.set <- train.full[fold.index[,k],]
    test.set <- train.full[-fold.index[,k],]
    
    fit.cv <-glm( paste("repay_fail~", (paste(Gini.model.summary[i, 1], sep =""))),data = train.set,na.action = na.exclude,family = "binomial")
    prediction.set = predict(fit.cv, newdata = test.set,type = "response")
    
    pred1 = prediction(prediction.set,test.set$repay_fail)
    
    test.Gini.mx[i, k+1] <- as.numeric(signif(2*as.numeric(performance(pred1, "auc")@y.values)-1, 4))
  }
}
test.Gini.mx[12, 1] <-"term+purpose+inq_last_6mths+fac.annual_inc+term:purpose+term:fac.annual_inc"
i=12
for (k in 1:4){ 
  
  # Create the training and testing subsets for each fold
  train.set <- train.full[fold.index[,k],]
  test.set <- train.full[-fold.index[,k],]
  
  fit.cv<-glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc+term:purpose+term:fac.annual_inc,data = train.set,na.action = na.exclude,family = "binomial"(link = "logit"))
  prediction.set = predict(fit.cv, newdata = test.set,type = "response")
  
  pred1 = prediction(prediction.set,test.set$repay_fail)
  
  test.Gini.mx[i, k+1] <- as.numeric(signif(2*as.numeric(performance(pred1, "auc")@y.values)-1, 4))
}

for(i in c(5,6,8,9,10,11, 12)){ test.Gini.mx[i,6]<- mean(as.numeric(test.Gini.mx[i, 2:5]))}
write.csv(test.Gini.mx, "BackgroundFiles/test.Gini.mx.csv")

#### Predicting the test data (Gini summary model #5)----

train.full <- dataset[train.index,]
test <- dataset[-train.index,]
pdf("BackgroundFiles/roc.test.plots.with.interactions.pdf") 
test.test.Gini.mx <- matrix(nrow=20, ncol=2)
row.counter <-1

fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[5, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Using loan amount, term, employment length, purpose, \ndebt to income ratio, inquiries last six months, public records, \nand annual income", xlab ="False positive rate")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)
test.test.Gini.mx[row.counter, 1] <- paste(Gini.model.summary[5, 1], sep ="")
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1
#### Predicting the test data (Gini summary model #6)----
# fit the model to the training subset
fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[6, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Using loan amount, term, employment length, \npurpose, inquiries last six months, public records, and annual income")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)

test.test.Gini.mx[row.counter, 1] <- paste(Gini.model.summary[6, 1], sep ="")
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1
#### Predicting the test data (Gini summary model #8)----
# fit the model to the training subset
fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[8, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Using term, employment length, purpose, \ninquiries last six months, public records, and annual income")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)

test.test.Gini.mx[row.counter, 1] <- paste(Gini.model.summary[8, 1], sep ="")
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1
#### Predicting the test data (Gini summary model #9)----
# fit the model to the training subset
fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[9, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Using term,  purpose, inquiries last six months,\n public records, and annual income")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)

# fit the model to the training subset
fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[9, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred2 = prediction(prediction.set, test$repay_fail)
test.AUC2 <- as.numeric(performance(pred2, "auc")@y.values)
test.Gini2 <- 2 * test.AUC2 - 1
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Using term, employment length, annual income, \npurpose, inquiries in last 6 months and public records ", xlab ="False positive rate")
legend("topleft", legend = c( "Gini = ", signif(test.Gini2, 4)))
abline(0,1)
test.test.Gini.mx[row.counter, 1] <- paste(Gini.model.summary[9, 1], sep ="")
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1
#### Predicting the test data (BEST MODEL :Gini summary model #10)----
# fit the model to the training subset
fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[10, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Best Model\nUsing term, purpose, inquiries last six months, and annual income")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)

test.test.Gini.mx[row.counter, 1] <- paste(Gini.model.summary[10, 1], sep ="")
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1
#### Predicting the test data (Gini summary model #11)----
# fit the model to the training subset
fit.model <-
  glm(
    paste("repay_fail~", (paste(Gini.model.summary[11, 1], sep =""))),
    data = train.full,
    na.action = na.exclude,
    family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Using term, purpose, and inquiries last six months")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)

test.test.Gini.mx[row.counter, 1] <- paste(Gini.model.summary[11, 1], sep ="")
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1
#### Predicting the test data (Gini summary model #10 with two way interactions)----
# fit the model to the training subset
fit.model <-
  glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc+term:fac.annual_inc+term:purpose,
      data = train.full,
      na.action = na.exclude,
      family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7),   main = "Interaction Model\nterm, purpose, inquiries last 6 months, annual income\nterm:annual income, and term:purpose")
legend("topleft", legend = c( "Gini = ", signif(test.Gini, 4)))
abline(0,1)

test.test.Gini.mx[row.counter, 1] <- "term+purpose+inq_last_6mths+fac.annual_inc+term:annual income+term:purpose"
test.test.Gini.mx[row.counter, 2] <-signif(test.Gini, 4)
row.counter <- row.counter +1

###### save test set Gini plots
dev.off()
write.csv(test.test.Gini.mx, "BackgroundFiles/TestingGini.csv")


#### Parameter estimate CI's----
pdf("BackgroundFiles/parameter.CIs.pdf") 
fit<-glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc,data = train.full,na.action = na.exclude,family = "binomial"(link = "logit"))

upper.bound <-summary(fit)$coefficients[,1]+1.96*summary(fit)$coefficients[,2]
lower.bound <-summary(fit)$coefficients[,1]-1.96*summary(fit)$coefficients[,2]

# covariates and 2-level factors, c(2, 16)  2 for term 60 months, 16 inq last 6 months
plot(summary(fit)$coefficients[c(2, 16),1],pch=15, ylim = c(-.15,1.15), col=c("red", "blue", "green"), xaxt = "n", xlab = "", ylab = "", main = "95% Confidence Intervals for Parameter Estimates")
legend("top", c("Term 60 months", "Inquiries last 6 months"), pch=15, col = c("red", "blue"), bty = "n")
lines(upper.bound[c(2, 16)])
lines(lower.bound[c(2, 16)])
abline(0,0, lty="dotted")
abline(1,0, lty="dotted")

# 3-15 for purpose
plot(summary(fit)$coefficients[3:15,1],pch=15, ylim = c(-.5,2), col=1:13, xaxt = "n", xlab = "Base level: Car", ylab = "", main = "95% Confidence Intervals for Parameter Estimates\nPurpose")
legend("topleft", levels(train.full$purpose)[2:6], pch=15, col = 1:5, bty = "n")
legend("top", levels(train.full$purpose)[7:11], pch=15, col = 6:10, bty = "n")
legend("topright", levels(train.full$purpose)[12:14], pch=15, col = 11:13, bty = "n")
lines(upper.bound[3:15])
lines(lower.bound[3:15])
abline(0,0, lty="dotted")
abline(1,0, lty="dotted")

# c("1900,30000", "30000, 37000", "37000, 44400", "44400, 50000", "50000, 58600", "58600,65300", "65300,75300", "75300,90000", "90000,116000", "116000,6000000")

# 17-25 for income bracket
plot(summary(fit)$coefficients[17:25,1],pch=15, ylim = c(-1.2,.5), col=1:9, xaxt = "n", xlab = "Base level: (1900,30000)", ylab = "", main = "95% Confidence Intervals for Parameter Estimates\nIncome Bracket")
legend("topleft", c( "(30000, 37000)", "(37000, 44400)", "(44400, 50000)"), pch=15, col = 1:3, bty = "n")
legend("top", c("(50000, 58600)", "(58600,65300)", "(65300,75300)"), pch=15, col = 4:6, bty = "n")
legend("topright", c("(75300,90000)", "(90000,116000)", "(116000,6000000)"), pch=15, col = 7:9, bty = "n")
lines(upper.bound[17:25])
lines(lower.bound[17:25])
abline(0,0, lty="dotted")
abline(-1,0, lty="dotted")

dev.off()

#### Best Model Test & Train set ROC----
pdf("BackgroundFiles/best model train & test Roc.pdf")
fit.model<-glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc,data = train.full,na.action = na.exclude,family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = test, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, test$repay_fail)
test.AUC <- as.numeric(performance(pred1, "auc")@y.values)
test.Gini <- 2 * test.AUC - 1
test.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf,  text.adj=c(-0.2,1.7),   main = paste("Validation Set ROC, Gini = ", signif(test.Gini, 4)), xlab ="False positive rate")


#train set
fit.model<-glm(repay_fail~term+purpose+inq_last_6mths+fac.annual_inc,data = train.full,na.action = na.exclude,family = "binomial"(link = "logit"))

# predict the testing subset
prediction.set = predict(fit.model, newdata = train.full, type = "response")

# Gini and ROC plot
pred1 = prediction(prediction.set, train.full$repay_fail)
train.full.AUC <- as.numeric(performance(pred1, "auc")@y.values)
train.full.Gini <- 2 * train.full.AUC - 1
train.full.Gini
perf <- performance(pred1, "tpr", "fpr")
plot(perf,  text.adj=c(-0.2,1.7),   main = "Train Set ROC", xlab ="False positive rate")
legend("topleft", legend = c( "Gini = ", signif(train.full.Gini, 4)))
dev.off()
#### End ----
# save.image("344.collected.full.Rdata")