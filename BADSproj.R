#   BADS Final Assignment Group 
#   - Sebastian Gütgemann (589623)  ~ s.guetgemann@gmx.net
#   - Rieke Ackermann     (588611)  ~ rieke.ackermann@web.de
#   - Sena Aydin          (594644)  ~ senaaydin484@gmail.com
#   - Kai Dessau          (559766)  ~ kai-dessau@web.de



# ----------------------- Packages

if(!require("plyr"))          install.packages("plyr");         library("plyr")
if(!require("dplyr"))         install.packages("dplyr");        library("dplyr")
if(!require("stringdist"))    install.packages("stringdist");   library("stringdist")
if(!require("rpart"))         install.packages("rpart");        library("rpart")
if(!require("psych"))         install.packages("psych");        library("psych")
if(!require("car"))           install.packages("car");          library("car")
if(!require("Amelia"))        install.packages("Amelia");       library("Amelia")
if(!require("boot"))          install.packages("boot", dep=T);  library("boot")
if(!require("caret"))         install.packages("caret");        library("caret")
if(!require("rpart"))         install.packages("rpart");        library("rpart")
if(!require("rattle"))        install.packages("rattle");       library("rattle")
if(!require("rpart.plot"))    install.packages("rpart.plot");   library("rpart.plot")
if(!require("RColorBrewer"))  install.packages("RColorBrewer"); library("RColorBrewer")
if(!require("rms"))           install.packages("rms");          library("rms")
if(!require("pROC"))          install.packages("pROC");         library("pROC")
if(!require("e1071"))         install.packages("e1071");        library("e1071")
if(!require("randomForest"))  install.packages("randomForest"); library("randomForest")
if(!require("hmeasure"))      install.packages("hmeasure");     library("hmeasure")
if(!require("repmis"))        install.packages("repmis");       library("repmis")
if(!require("ggplot2"))       install.packages("ggplot2");      library("ggplot2")
if(!require("eeptools"))      install.packages("eeptools");     library("eeptools")
if(!require("xgboost"))       install.packages("xgboost");      library("xgboost")
if(!require("penalized"))     install.packages("penalized");    library("penalized")
if(!require("neuralnet"))     install.packages("neuralnet");    library("neuralnet")

if(!require("woeBinning"))     install.packages("woeBinning");    library("woeBinning")
if(!require("klaR"))     install.packages("klaR");    library("klaR")




# ----------------------- Load Data

githubURL <- "https://raw.githubusercontent.com/KaiRstudio/BRSK/master/BADS_WS1718_known_MODEL_FITTING.csv"
daten <- source_data(githubURL, sha1 ="254e37cf5b7fe121e2e9c8212803cda9415c9de7", header = "auto", sep=",")
#sha1 is the hash to make sure data hasnt changed




# ----------------------- Formatting

formatofdate        <- "%Y-%m-%d"
daten$order_date    <- as.Date(daten$order_date, format = formatofdate)
daten$delivery_date <- as.Date(daten$delivery_date, format = formatofdate)
daten$user_reg_date <- as.Date(daten$user_reg_date, format = formatofdate)
daten$user_dob      <- as.Date(daten$user_dob, format = formatofdate)
daten$order_item_id <- as.factor(daten$order_item_id)
daten$item_id       <- as.factor(daten$item_id)
daten$brand_id      <- as.factor(daten$brand_id)
daten$user_id       <- as.factor(daten$user_id)
daten$user_title    <- as.factor(daten$user_title)
daten$user_state    <- as.factor(daten$user_state)
daten$return        <- as.factor(daten$return)




# ----------------------- General Terms

# - Calculate age -
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  age = to_lt$year - from_lt$year
  ifelse(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1,
    age)} ## why not just use difference between order date and yob?


# - Exclude Outlier by Z-Score -
Z.outlier <- function(x){
  Zscore <- scale(x)
  x[Zscore >3] <- NA
  x[Zscore <(-3)] <- NA
  return(x)}


# - Aggregate color levels -
agg.col <- function (df.col) {
  df.col <- factor(df.col)
  df.col <- ifelse(df.col %in% c("?"), "Other",
                             ifelse(df.col %in% c("petrol", "blau", "blue", "azure", "cobalt blue", "dark navy", "darkblue", "turquoise", "silver", "navy", "aqua", "aquamarine", "baltic blue", "darkblue"), "blue",
                                    ifelse(df.col %in% c("red", "orange", "purple", "currant purple", "pink", "antique pink", "crimson", "bordeaux", "berry", "fuchsia", "perlmutt", "coral", "hibiscus", "magenta", "terracotta", "dark garnet"), "red",
                                           ifelse(df.col %in% c("green", "olive", "oliv", "dark oliv", "mint", "aubergine", "lemon", "nature", "khaki", "avocado", "jade"), "green",
                                                  ifelse(df.col %in% c("mocca", "brwon", "brown", "beige", "kanel", "mahagoni", "copper coin", "cognac", "caramel"),"brown",
                                                         ifelse(df.col %in% c("apricot", "yellow", "vanille", "gold", "ingwer", "white", "mango", "almond", "champagner", "creme", "curry", "ivory", "ocher"), "yellow",
                                                                ifelse(df.col %in% c("grey", "black", "dark grey", "graphite", "iron", "habana", "ebony", "amethyst", "basalt", "ash", "ancient", "anthracite", "denim", "dark denim"), "dark", "Other")))))))
  df.col <- factor(df.col)
}




# ----------------------- Start: Prep Existing Variables

# ---- Dates ----
daten$delivery_date[daten$order_date>daten$delivery_date] <-NA



# ---- Item ID ----



# ---- Size ----
daten$item_size <- factor(toupper(daten$item_size))
levels(daten$item_size) <- c(levels(daten$item_size), "Other")
daten$item_size [daten$item_size %in% names(prop.table(table(daten$item_size))
                         [prop.table(table(daten$item_size))<= 0.01])] <- "Other"
daten$item_size <- factor(daten$item_size)



# ---- Color ----
levels(daten$item_color) <- c(levels(factor(daten$item_color)), "Other")
daten$item_color <- agg.col(daten$item_color)



# ---- Brand ID ----



# ---- Price ----
daten$item_price[daten$item_price <= 0] <- NA
# med_item_price <- median(daten$item_price, na.rm=TRUE)
# daten$item_price[is.na(daten$item_price)] <- med_item_price


# - Scale Price -
#daten$item_price2 <- as.numeric(scale(daten$item_price))
#daten$item_price <- Z.outlier(daten$item_price) #Are we sure that 399.95 is outlier?



# ---- User ID ----





# ---- Title ----
levels(daten$user_title) <- c(levels(factor(daten$user_title)),"Other")
daten$user_title[daten$user_title != "Mrs" & daten$user_title != "Mr"] <- factor("Other")
daten$user_title <- factor(daten$user_title)



# ---- User State ----

# ----------------------- End: Existing variable prep




# ----------------------- Start: New variables

# ---- Delivery Time/Month ----
daten$order_month <- as.factor(months(daten$order_date)) # new column with month of delivery
daten$delivery_time <- Z.outlier(as.numeric(daten$delivery_date - daten$order_date))
daten$regorderdiff <- as.numeric(daten$order_date - daten$user_reg_date) ## makes sense?
# If necessary replace NAs by median
#MED_DEL <- round( median (daten$delivery_time, na.rm =TRUE)) # round( mean (daten$delivery_time, na.rm =TRUE)) gives median of 4 and mean of 11 for delivery time
#daten$delivery_date[is.na(daten$delivery_date)]<- daten$order_date[is.na(daten$delivery_date)] + MED_DEL
#daten$delivery_time[is.na(daten$delivery_time)] <- MED_DEL

# Scaled date variables
#daten$delivery_time2 <- as.numeric(scale(daten$delivery_time))
#daten$regorderdiff2 <- as.numeric(scale(daten$regorderdiff))



# ---- Customer age ----
daten$age <- age(daten$user_dob,daten$order_date)
daten$age <- Z.outlier(daten$age)
#med.age <- round(median(daten$age, na.rm = TRUE))
#daten$age[is.na(daten$age)] <- med.age
#daten$age2 <- as.numeric(scale(daten$age))



# ---- Number of items in a basket ----
daten <- join(daten, count(daten, order_date, user_id),by = c("order_date", "user_id"))



# ---- The number of same item user bought within the same basket ----
daten <- join(daten, count(daten, order_date, user_id, item_id),by = c("order_date", "user_id","item_id"))
names(daten)[names(daten) == "n"] <- "ct_basket_size"
names(daten)[names(daten) == "nn"] <- "ct_same_items"



# ---- Customers past return rates ----

# ----------------------- End: New variables




# ----------------------- Start: Drop non relevant variables

drops <- c("order_item_id",
           "delivery_date",
           "user_reg_date")
daten <- daten[,!(names(daten) %in% drops)]

# ----------------------- End: Drop non relevant variables




# ----------------------- Start: Binning and WoE

# ---- Binning ----
#binning <- woe.binning (df=daten, target.var="return", pred.var=c("delivery_time"), min.perc.class = 0.01)
#woe.binning.plot(binning)
#woe.binning.table(binning)



# ---- WoE ----
# to avoid overfitting: split a woe training set from the overall training set
set.seed(222)
idx.train <- createDataPartition(y = daten$return, p = 0.75, list = FALSE) # Draw a random, stratified sample including p percent of the data
test <-  daten[-idx.train, ] # test set
train <- daten[idx.train, ] # training set
## tapply (woe.train$user_state, woe.train$return, summary)
tapply(train$user_title, train$return, summary)
##if there is the prob of zero in one level for return/non-return, function does not work -> zeroadj
data <- train[, sapply(train, is.factor)]
data <- data[, -1]
woe.values <- woe(return ~ ., data=train, zeroadj=0.1)
## weights for each factor
woe.values$woe
# note: klaR assumes the first level of target to be the target level (WoE refer to no returns)
## transformed version (variables have been replaced with woe)
summary(woe.values$xnew)
## replacement
test.woe <- predict(woe.values, newdata=test, replace=TRUE)
summary(test.woe)
## check for plausibility by plotting weights against their levels
barplot(woe.values$woe$user_state)

# ----------------------- End Binning & WoE



summary(daten)
str(daten)


missmap(daten, main = "Missing values vs observed") # to give a plot of the missing values per variable

# ---- IDEAS & OPEN QUESTIONS ----
# 1)  In case someone buys more stuff at once, the whole thing should obviously not be 
#     discouraged because one or two items are likely to be returned
#     rather try to get the one item out of the cart
# 2)  The likelihood of an item being returned has to be seen in respect to how much
#     would be lost if discouraged and wouldnt have been returned. It is worse if cheap items
#     are returned than expensive ones, because percentage wise the return is more expensive
#     as well as its 3€ plus 10 % ov value.
# 3)  Maybe other categories as alternative to binning? a) dark-light contrasting
#     b) warm-cold contrast c) black-white contrast ... are dark colors returned more in summertime?
#     combining fasion colors with months and returns? colors  can look differently online.
# 4)  likelihood ratio test; AIC; step function (direction: both)
# 5)  Without knowing the item, statements about price or size can hardly be made
# 6)  daten <- daten(na.strings = c("?","not reported") --> question marks and not reported entries havent been replaced with NAs yet
# 7)  review at the end which packages we really need
# 8)  was is on purpose that user title and user state have not been class-transformed?
# 9) do we really want to put all titles other than MRS as "other" ?
# 10) division into rarely returned/never returned should rather not be included
# 11) items with price zero should not be available to be returned as it would costwise not make sense. they should not simply be put as NAs
# 12) text should include that when you are the shopowner and you set up this analysis, some open questions could be answered with more background knowlegde