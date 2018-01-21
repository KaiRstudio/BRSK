# file for building models
# For each model:
# Parameter optimization (=Loops over parameters of model)
# Loops over choice of variables
# Training/Testing    
# data splitting, model prediction, cross validation


# ----------------------- Read Data Prep
source("BADSproj.R")
# ----------------------- 


# ----------------------- Packages
if(!require("NeuralNetTools")) install.packages("NeuralNetTools"); library("NeuralNetTools")
if(!require("caret"))         install.packages("caret");        library("caret")



# ----------------------- Random Forest

k <- 5 # Set number of cross validation
set.seed(132)

# - Set controls for rf
model.control <- trainControl(
  method = "cv", 
  number = k, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary, 
  allowParallel = TRUE 
)

#set search grid
rf.parms <- expand.grid(mtry = 1:10)

# Train random forest rf with a 5-fold cross validation 
# ntree set as default value 500
rf.caret <- train(return~., data = train.filtered,
                  method = "rf", ntree = 500, tuneGrid = rf.parms, importance = TRUE,
                  metric = "ROC", trControl = model.control, na.action = na.omit)


# ----------------------- Gradient Boosting




# ----------------------- Neural Networks
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  #repeats = 3, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  returnData = FALSE # The training data will not be included in the ouput training object
)
# Define a search grid of values to test
nn.parms <- expand.grid("decay" = 0.001, 
                        "size" = seq(3, 15, 3))

#Train neural network nn with 5-fold cv
set.seed(123)
nn_caret <- caret::train(BAD~., data = tr,  
                         method = "nnet", maxit = 100, trace = FALSE, # options for nnet function
                         tuneGrid = nn.parms, # parameters to be tested
                         metric = "ROC", trControl = model.control)


# Neural networks work better when the data inputs are on the same scale, e.g. standardized
# Be careful to use the training mean/sd for normalization
normalizer <- caret::preProcess(train, method = c("center", "scale"))
tr <- predict(normalizer, newdata = train)
ts <- predict(normalizer, newdata = test)

nn_raw <- nnet(BAD~., data = train, # the data and formula to be used
               trace = FALSE, maxit = 1000, # general options
               size = 3, # the number of nodes in the model
               decay = 0.001) # regularization parameter similar to lambda in ridge regression

plotnet(nn, max_sp = TRUE)





# ----------------------- Logistic Regression

# Regularized Logistic Regression
indep.var <- model.matrix(return~.-1, daten)
dep.var <- daten$return
lasso <- glmnet(x = indep.var, y = dep.var, family = "binomial", standardize = TRUE,
                alpha = 1, nlambda = 100)
yhat["lasso"] <- as.vector( predict(lasso, newx = x, s = 0.001, type = "response") )

# ----------------------- 


