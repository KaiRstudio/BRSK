if(!require("repmis")) install.packages("repmis");library("repmis")


# 1. Load, prep & clean training/test data
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/BADSproj.R")


# 2. Variable selection by filter & wrapper
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/VarSel.R")


# 3. Parameter tuning and performances
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/Modeling.R") # uses MODELING code


# 4. Apply cost sensitive learning
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/AdapToCSL.R")


# 5. Load and prep unknown data
source("https://raw.githubusercontent.com/KaiRstudio/BRSK/master/Unknown_data_prep.R")



# ------------------- Result
# Best model: ** Random Forest at the moment **




# ------------------- Final data prediction
# - Use best model to predict unknown data -
