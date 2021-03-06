
# -------------------------------- BADS Final Assignment Group 45 --------------------------------

#   - Sebastian Gütgemann (589623)  ~ s.guetgemann@gmx.net
#   - Rieke Ackermann     (588611)  ~ rieke.ackermann@web.de
#   - Sena Aydin          (594644)  ~ senaaydin484@gmail.com
#   - Kai Dessau          (559766)  ~ kai-dessau@web.de


# ---------------------------- Run this file to load the whole code ------------------------------

# 1. Load all packages 
check.packages <- function(x){
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){
      install.packages( i , dependencies = TRUE )
      require( i , character.only = TRUE )}}}

check.packages( c("plyr" , "dplyr" , "stringdist", "rpart", "psych", "car", "Amelia",
                    "boot", "caret", "rpart", "rattle", "rpart.plot", "RColorBrewer", "rms",
                    "pROC", "e1071", "randomForest", "hmeasure", "repmis", "ggplot2", "eeptools",
                    "xgboost", "penalized", "neuralnet", "woeBinning", "klaR", "NeuralNetTools", 
                    "glmnet", "mlr", "parallelMap", "nnet", "parallel", "repmis" ))


# 2. Load, prep & clean training/test data
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/BADSproj.R")


# 3. Variable selection by filter & wrapper
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/VarSel.R")


# 4. Parameter tuning and performances
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/Modeling.R")


# 5. Apply cost sensitive learning
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/AdapToCSL.R")


# 6. Load and prep unknown data
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/Unknown_data_prep.R")



# ------------------- Result
# Best model: ** Random Forest with categories based on costs **


# Predict final data: Put in best model and threshold
final.pred <- predict(modelLib[["rf.cat"]], newdata = final, type = "prob")
final.pred.class <- setThreshold(final.pred, cv.emp.th.rf.cat)

#Write csv
output<- data.frame("Order_Item_ID"= final$order_item_id ,"Return" = final.pred.class$data[,3])
write.csv(output, "45.csv", row.names = FALSE )
