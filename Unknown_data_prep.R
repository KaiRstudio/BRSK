# - Read unknown data
githubURL <- "https://raw.githubusercontent.com/KaiRstudio/BRSK/be86d55f0a62bfc0ebeb88a6103d660e4711f68f/BADS_WS1718_class.csv"
nd <- source_data(githubURL, header = "auto", sep=",")


# - Prep unknown data by using respective formulas from original data prep

# ----------------------- Formatting

formatofdate        <- "%Y-%m-%d"
nd$order_date    <- as.Date(nd$order_date, format = formatofdate)
nd$delivery_date <- as.Date(nd$delivery_date, format = formatofdate)
nd$user_reg_date <- as.Date(nd$user_reg_date, format = formatofdate)
nd$user_dob      <- as.Date(nd$user_dob, format = formatofdate)
nd$order_item_id <- as.factor(nd$order_item_id)
nd$item_id       <- as.factor(nd$item_id)
nd$brand_id      <- as.factor(nd$brand_id)
nd$user_id       <- as.factor(nd$user_id)
nd$user_title    <- as.factor(nd$user_title)
nd$user_state    <- as.factor(nd$user_state)
nd$return        <- as.factor(nd$return)




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
  df.col <- factor(df.col)}




# ----------------------- Start: Prep Existing Variables

# ---- Dates ----
nd$delivery_date[nd$order_date>nd$delivery_date] <-NA



# ---- Size ----
nd$item_size <- factor(toupper(nd$item_size))
levels(nd$item_size) <- c(levels(nd$item_size), "Other")
nd$item_size [nd$item_size %in% names(prop.table(table(nd$item_size))
                                            [prop.table(table(nd$item_size))<= 0.01])] <- "Other"
nd$item_size <- factor(nd$item_size)



# ---- Color ----
levels(nd$item_color) <- c(levels(factor(nd$item_color)), "Other")
nd$item_color <- agg.col(nd$item_color)



# ---- Price ----
nd$item_price[nd$item_price <= 0] <- NA
med_item_price <- median(nd$item_price, na.rm=TRUE)
nd$item_price[is.na(nd$item_price)] <- med_item_price



# ---- Title ----
levels(nd$user_title) <- c(levels(factor(nd$user_title)),"Other")
nd$user_title[nd$user_title != "Mrs" & nd$user_title != "Mr"] <- factor("Other")
nd$user_title <- factor(nd$user_title)

# ----------------------- End: Existing variable prep




# ----------------------- Start: New variables

# ---- Date dependent variables ----
nd$order_month <- as.factor(months(nd$order_date)) # new column with month of delivery
nd$delivery_time <- Z.outlier(as.numeric(nd$delivery_date - nd$order_date))
nd$regorderdiff <- as.numeric(nd$order_date - nd$user_reg_date)



# ---- Customer age ----
nd$age <- age(nd$user_dob,nd$order_date)
nd$age <- Z.outlier(nd$age)
med.age <- median(nd$age, na.rm = TRUE)
nd$age[is.na(nd$age)] <- med.age



# ---- Count_Basket_Size ----
nd <- join(nd, dplyr::count(nd, order_date, user_id),
              by = c("order_date", "user_id"))



# ---- Count same Item_ID in basket ----
nd <- join(nd, count(nd, order_date, user_id, item_id),
              by = c("order_date", "user_id","item_id"))
names(nd)[names(nd) == "n"] <- "ct_basket_size"
names(nd)[names(nd) == "nn"] <- "ct_same_items"
nd$ct_basket_size <- as.numeric(nd$ct_basket_size)
nd$ct_same_items <- as.numeric(nd$ct_same_items)

# ----------------------- End new variables




# ----------------------- Start: Drop non relevant variables

drops <- c("order_item_id",
           "delivery_date",
           "user_reg_date",
           "user_dob",
           "order_date")
nd <- nd[,!(names(nd) %in% drops)]

# ----------------------- End: Drop non relevant variables




# ----------------------- Start: Binning

nd$delivery_time <- ifelse(is.na(nd$delivery_time), "Missing",
                              ifelse(nd$delivery_time <= 1, "<=1", ">1"))
nd$delivery_time <- factor(nd$delivery_time)

# ----------------------- End: Binning




# ----------------------- Start: WoE
# - Predict with respective set for Random Forest - 

set.seed(123)
final <- predict(woe.values_Ids, newdata=nd, replace = True)

# ----------------------- End: WoE

