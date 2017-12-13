# GROUP 45 members:
#   - Bastian
#   - Rieke
#   - Sena
#   - Kai Dessau (559766) - kai-dessau@web.de

daten <- read.csv('C:/Users/Administrator/Google Drive/Exports/BADS/BADS_WS1718_known.csv',
                    sep=",",
                    na.strings = c("?","not reported"),
                    colClasses = c("order_date" ="Date",
                                   "delivery_date" = "Date",
                                   "user_reg_date" = "Date",
                                   "user_dob" = "Date",
                                   "order_item_id" = "factor",
                                   "item_id" = "factor",
                                   "brand_id" = "factor" ,
                                   "user_id" = "factor",
                                   "return" = "factor"))

# ---- Packages ----
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
# review at the end which ones we really need

# ---- General Terms ----
formatofdate <- "%Y-%m-%d"

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  age = to_lt$year - from_lt$year
  ifelse(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1,
    age)} #-------I used this function to calculate age so maybe we can use it later on. Therefore I leave it here.

Z.outlier <- function(x){
  Zscore <- scale(x)
  x[Zscore >3] <- NA
  x[Zscore <(-3)] <- NA
  return(x)    
}

#######Converting dates
for (chrVar in c("order_date", "delivery_date", "user_reg_date", "user_dob")) {
 daten[, chrVar] <- as.Date(daten[[chrVar]])
}


# ---- Order Item ID ----
table(is.na(daten$order_item_id))

# ---- Dates ----
daten$delivery_date[daten$order_date>daten$delivery_date] <-NA
daten$delivery_date[daten$delivery_date == "1990-12-31"] <- NA # remove unrealistic times
daten$delivery_time <- daten$delivery_date - daten$order_date #Subtract delivery date from order date so we can see "delivery time"
daten$order_month <- as.factor(months(daten$order_date)) # new column with month of delivery
daten$delivery_time <- Z.outlier(daten$delivery_time) # removing outliers from delivery time
MED_delivery <- round( median (daten$delivery_time, na.rm =TRUE)) # round( mean (daten$delivery_time, na.rm =TRUE)) gives median of 4 and mean of 11 for delivery time
daten$delivery_date[is.na(daten$delivery_date)]<- daten$order_date[is.na(daten$delivery_date)] + MED_DEL
daten$delivery_time[is.na(daten$delivery_time)] <- MED_DEL
daten$regorderdiff <- daten$order_date - daten$user_reg_date

hist(as.numeric(daten$regorderdiff)) # many people that registered and immediately bought, the rest is equally distributed up until 774 days
max(daten$delivery_time, na.rm =TRUE) # delivery has to be timely after order date, the max is 151 days
min(daten$delivery_time, na.rm =TRUE) # check that minimal delivery time is zero
hist(daten$delivery_time) # Histogram of delivery_time
sum(is.na(daten$delivery_date)) # should be zero
# ---- Item ID ----

# ---- Size ----
x$item_size <- factor(toupper(x$item_size))
table_size <- table(x$item_size)
levels(x$item_size) <- c(names(table_size), "Other")
x$item_size[is.na(x$item_size)] <- factor("Other")
x[x$item_size %in% names(table_size)[table_size < 100],"item_size"] <- factor("Other")
x$item_size <- factor(x$item_size)
# ---- Color ----
table_col_clust <- table(x$item_color)
levels(x$item_color) <- c(names(table_col_clust), "Other")
x$item_color[is.na(x$item_color)] <- factor("Other")
x[x$item_color %in% names(table_col_clust)[table_col_clust < 200],"item_color"] <- factor("Other")
x$item_color <- factor(x$item_color)

daten$item_colorcat <- as.factor(daten$item_color) # categorisation is very subjective
daten$item_colorcat[daten$item_colorcat == "?"] <- NA
daten$item_colorcat <- ifelse(daten$item_colorcat %in% c("?"), NA,
                       ifelse(daten$item_colorcat %in% c("petrol", "blau", "blue", "azure", "cobalt blue", "dark navy", "darkblue", "turquoise", "silver", "navy", "aqua", "aquamarine", "baltic blue", "darkblue"), "blue",
                       ifelse(daten$item_colorcat %in% c("red", "orange", "purple", "currant purple", "pink", "antique pink", "crimson", "bordeaux", "berry", "fuchsia", "perlmutt", "coral", "hibiscus", "magenta", "terracotta", "dark garnet"), "red",
                       ifelse(daten$item_colorcat %in% c("green", "olive", "oliv", "dark oliv", "mint", "aubergine", "lemon", "nature", "khaki", "avocado", "jade"), "green",
                       ifelse(daten$item_colorcat %in% c("mocca", "brwon", "brown", "beige", "kanel", "mahagoni", "copper coin", "cognac", "caramel"),"brown",
                       ifelse(daten$item_colorcat %in% c("apricot", "yellow", "vanille", "gold", "ingwer", "white", "mango", "almond", "champagner", "creme", "curry", "ivory", "ocher"), "yellow",
                       ifelse(daten$item_colorcat %in% c("grey", "black", "dark grey", "graphite", "iron", "habana", "ebony", "amethyst", "basalt", "ash", "ancient", "anthracite", "denim", "dark denim"), "dark",
                       ifelse(daten$item_colorcat %in% c("striped", "aviator", "pallid", "opal", "stained", "floral", "ecru", "curled"), "others", daten$item_colorcat)))))))
table(daten$item_colorcat) 
# ---- Brand ID ----

# ---- Price ----
x$item_price[x$item_price==0] <-NA
med.ip <- median(x$item_price, na.rm=TRUE)
x$item_price[is.na(x$item_price)] <- med.ip
Returns$item_price <- Z.outlier(Returns$item_price)

daten$pricecat <- cut(daten$pricecat,c(-1, 0, 20, 50, 100, 200, 400), labels=c(NA, "0-20", "20-50", "50-100", "100-200", "200-400"))
# ---- User ID ----

# ---- Title ----
daten$user_title[daten$user_title == "not reported"] <- NA # remove not reported titles
table(daten$user_title)

table_title <- table(x$user_title)
levels(x$user_title) <- c(names(table_title), "Other")
x$user_title[is.na(x$user_title)] <- factor("Other")
x$user_title[x$user_title == "Company"] <- factor("Other")
x$user_title[x$user_title == "Family"] <- factor("Other")
x$user_title <- factor(x$user_title)
# ---- Age ----
daten$age <- as.numeric(round((daten$order_date - daten$user_dob)/ 365.25))

med.age <- median(x$age, na.rm = TRUE)
x$age[is.na(x$age)] <- med.age
Returns$age <- Z.outlier(Returns$age) 

daten$age2 <- daten$age
daten$age2[is.na(daten$age2)] <- round(median(daten$age2, na.rm = T))
daten$age2 <- ageoutlierclean(daten$age2)

daten$agecat <- recode(daten$agecat, "0:27='18-27';28:37='28-37'; 38:47='38-47'; 48:57='48-57'; 58:67='58-67'; 68:77='68-77'; 78:87='78-87'; 88:100='88-100'")

table(daten$agecat)
summary(daten$age2)
hist(daten$age2)
boxplot(daten$age2)
hist(daten$age)
# ---- User State ----


missmap(daten, main = "Missing values vs observed") # to give a plot of the missing values per variable

# ---- IDEAS ----
# 1)  In case someone buys more stuff at once, the whole thing should obviously not be 
#     discouraged because one or two items are likely to be returned
#     rather try to get the one item out of the cart
# 2)  the likelihood of an item being returned has to be seen in respect to how much
#     would be lost if discouraged and wouldnt have been returned. It is worse if cheap items
#     are returned than expensive ones, because percentage wise the return is more expensive
#     as well as its 3€ plus 10 % ov value.
