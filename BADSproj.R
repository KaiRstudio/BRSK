
# -------------------------------- Data Prep training & test ----------------------------
# - Formatting, handling errors, feature engineering, binning/aggregation, data split, WoE -




# ----------------------- Load Data

githubURL <- "https://raw.githubusercontent.com/KaiRstudio/BRSK/master/BADS_WS1718_known_MODEL_FITTING.csv"
daten <- source_data(githubURL, sha1 ="254e37cf5b7fe121e2e9c8212803cda9415c9de7", header = "auto", sep=",")




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

# ---- Calculate age ----
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  age = to_lt$year - from_lt$year
  ifelse(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1,
    age)} 


# ---- Exclude Outlier by Z-Score ----
Z.outlier <- function(x){
  Zscore <- scale(x)
  x[Zscore >3] <- NA
  x[Zscore <(-3)] <- NA
  return(x)}


# ---- Aggregate color levels ----
agg.col <- function (df.col) {
  df.col <- factor(df.col)
  df.col <- ifelse(df.col %in% c("?"), "Other",
                   ifelse(df.col %in% c("petrol", "blau", "blue", "azure", "cobalt blue", "dark navy", "darkblue", "turquoise", "silver", "navy", "aqua", "aquamarine", "baltic blue", "darkblue"), "blue",
                          ifelse(df.col %in% c("red", "orange", "purple", "currant purple", "pink", "antique pink", "crimson", "bordeaux", "berry", "fuchsia", "perlmutt", "coral", "hibiscus", "magenta", "terracotta", "dark garnet"), "red",
                                 ifelse(df.col %in% c("green", "olive", "oliv", "dark oliv", "mint", "aubergine", "lemon", "nature", "khaki", "avocado", "jade"), "green",
                                        ifelse(df.col %in% c("mocca", "brwon", "brown", "beige", "kanel", "mahagoni", "copper coin", "cognac", "caramel"),"brown",
                                               ifelse(df.col %in% c("apricot", "yellow", "vanille", "gold", "ingwer", "white", "mango", "almond", "champagner", "creme", "curry", "ivory", "ocher"), "yellow",
                                                      ifelse(df.col %in% c("grey", "black", "dark grey", "graphite", "iron", "habana", "ebony", "amethyst", "basalt", "ash", "ancient", "anthracite", "denim", "dark denim"), "dark", "Other")))))))
  df.col <- factor(df.col)}




# ----------------------- Start: Prep Existing Variables

# ---- Item_ID ----
levels(daten$item_id) <- c(levels(factor(daten$item_id)),"New")
single_iid<- factor(daten$item_id[daten$item_id %in% names(table(daten$item_id))
                                  [table(daten$item_id) == 1]])
daten$item_id[daten$item_id %in% names(table(daten$item_id))[table(daten$item_id) == 1]] <- factor("New")
daten$item_id <- factor(daten$item_id)


# ---- User_ID ----
levels(daten$user_id) <- c(levels(factor(daten$user_id)),"New")
single_uid <- factor(daten$user_id[daten$user_id %in% names(table(daten$user_id))
                                   [table(daten$user_id) == 1]]) 
daten$user_id[daten$user_id %in% names(table(daten$user_id))[table(daten$user_id) == 1]] <- factor("New")
daten$user_id <- factor(daten$user_id)


# ---- Brand_ID ----
levels(daten$brand_id) <- c(levels(factor(daten$brand_id)),"New")
single_bid <- factor(daten$brand_id[daten$brand_id %in% names(table(daten$brand_id))
                                    [table(daten$brand_id) <= 5]])
daten$brand_id[daten$brand_id %in% names(table(daten$brand_id))[table(daten$brand_id) <= 5]] <- factor("New")
daten$brand_id <- factor(daten$brand_id)


# ---- Dates ----
daten$delivery_date[daten$order_date>daten$delivery_date] <-NA



# ---- Size ----
daten$item_size <- factor(toupper(daten$item_size))
levels(daten$item_size) <- c(levels(daten$item_size), "Other")
daten$item_size [daten$item_size %in% names(prop.table(table(daten$item_size))
                                            [prop.table(table(daten$item_size))<= 0.01])] <- "Other"
daten$item_size <- factor(daten$item_size)



# ---- Color ----
levels(daten$item_color) <- c(levels(factor(daten$item_color)), "Other")
daten$item_color <- agg.col(daten$item_color)



# ---- Price ----
daten$item_price[daten$item_price <= 0] <- NA
med_item_price <- median(daten$item_price, na.rm=TRUE)
daten$item_price[is.na(daten$item_price)] <- med_item_price



# ---- Title ----
levels(daten$user_title) <- c(levels(factor(daten$user_title)),"Other")
daten$user_title[daten$user_title != "Mrs" & daten$user_title != "Mr"] <- factor("Other")
daten$user_title <- factor(daten$user_title)

# ----------------------- End: Prep existing variables




# ----------------------- Start: New variables

# ---- Month of order ----
daten$order_month <- as.factor(months(daten$order_date))


# ---- Days needed to deliver ----
daten$delivery_time <- Z.outlier(as.numeric(daten$delivery_date - daten$order_date))


# ---- Duration of membership ----
daten$regorderdiff <- as.numeric(daten$order_date - daten$user_reg_date)


# ---- Customer age ----
daten$age <- age(daten$user_dob,daten$order_date)
daten$age <- Z.outlier(daten$age)
med.age <- median(daten$age, na.rm = TRUE)
daten$age[is.na(daten$age)] <- med.age


# ---- Number of items in a basket ----
daten <- join(daten, dplyr::count(daten, order_date, user_id),
              by = c("order_date", "user_id"))


# ---- Number of same items bought in the same basket ----
daten <- join(daten, count(daten, order_date, user_id, item_id),
              by = c("order_date", "user_id","item_id"))
names(daten)[names(daten) == "n"] <- "ct_basket_size"
names(daten)[names(daten) == "nn"] <- "ct_same_items"
daten$ct_basket_size <- as.numeric(daten$ct_basket_size)
daten$ct_same_items <- as.numeric(daten$ct_same_items)

# ----------------------- End new variables




# ----------------------- Start: Drop non relevant variables

drops <- c("order_item_id",
           "delivery_date",
           "user_reg_date",
           "user_dob",
           "order_date")
daten <- daten[,!(names(daten) %in% drops)]

# ----------------------- End: Drop non relevant variables




# ----------------------- Start: Binning

# ---- Only significant: delivery_time ----
binning <- woe.binning (df=daten, target.var="return", pred.var=c("delivery_time"), min.perc.class = 0.01)
#woe.binning.plot(binning)
daten$delivery_time <- ifelse(is.na(daten$delivery_time), "Missing",
                              ifelse(daten$delivery_time <= 1, "<=1", ">1"))
daten$delivery_time <- factor(daten$delivery_time)

# ----------------------- End: Binning




# ----------------------- Start: Split Data

# ---- Training & Test ----
set.seed(111)
idx.train <- createDataPartition(y = daten$return, p = 0.75, list = FALSE)
test <-  daten[-idx.train, ] # test set
train <- daten[idx.train, ] # training set

# ---- To avoid overfitting: further data split for WoE ----
set.seed(112)
woe.idx.train <- createDataPartition(y=train$return, p = 0.7, list = FALSE)
train.split <- train[woe.idx.train,] # Set for WoE calculation

# ----------------------- End: Split Data




# ----------------------- Start: WoE

# ---- WoE for all variables ----
woe.values <- woe(return ~ ., data=train.split, zeroadj=0.05)

# ---- WoE only for variables with many factors (all IDs) ----
woe.values_ids <- woe(return ~ item_id+brand_id+user_id, data=train.split, zeroadj=0.05)
# Note: Negative WoE means higher probability to return

#barplot(-1*woe.values$woe$user_state)

# ---- WoE replacement: All categoricals ----
test.woe <- predict(woe.values, newdata=test, replace=TRUE)
train.woe <- predict(woe.values, newdata=train, replace=TRUE)

# ---- WoE replacement: IDs only ----
test.2 <-predict(woe.values_ids, newdata=test, replace=TRUE)
train.2 <-predict(woe.values_ids, newdata=train, replace=TRUE)

# ----------------------- End: WoE




# ----------------------- Start: Normalized data
# - Normalize Data for NN -
normalizer <- caret::preProcess(train[,names(train) %in% c("item_price","regorderdiff","age","ct_basket_size","ct_same_items")],
                                method = c("center", "scale"))

# ---- Normalized dataset for neural network ----
nn.train.woe <- predict(normalizer, newdata = train.woe)
nn.test.woe <- predict(normalizer, newdata = test.woe)
# ---- Adjust return values for Neural Network ----
nn.train.woe$return <- as.factor(ifelse(nn.train.woe$return == 1, 1, -1))
nn.test.woe$return <- as.factor(ifelse(nn.test.woe$return == 1, 1, -1))

# ---- Normalized dataset with categorical variables ----
# - Normalized data with categorical variables didn't show performance improvements -
#test.3 <- predict(normalizer, newdata= test.2)
#train.3 <- predict(normalizer, newdata=train.2)

# ----------------------- End: Normalized data
