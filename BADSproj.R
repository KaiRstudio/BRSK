# GROUP 45 members:
#   - Bastian
#   - Rieke
#   - Sena
#   - Kai Dessau (559766) - kai-dessau@web.de
setwd("~/Humboldt/BADS/BRSK")

daten <- read.csv('~/Humboldt/BADS/BRSK',
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

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1,
    age
  )
} #-------I used this function to calculate age so maybe we can use it later on. Therefore I leave it here.

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
x$delivery_date[x$order_date>x$delivery_date] <-NA
daten$delivery_date[daten$delivery_date == "1990-12-31"] <- NA # remove unrealistic times
sum(is.na(daten$delivery_date)) # should be zero
daten$delivery.time<-daten$delivery_date - daten$order_date #Subtract delivery date from order date so we can see "delivery time" which might be important for prediction.
x$order_month <- as.factor(months(x$order_date))


order$delivery.time<-order$delivery_date - order$order_date
order$delivery.time_factor[is.na(order$delivery.time)]<-"neverReturned"
order$delivery.time_factor[order$delivery.time<0]<-"rarelyReturned"
order$delivery.time_factor[order$delivery.time>=0]<-"returnedHalfTheTime"
order$delivery.time_factor<- as.factor(order$delivery.time_factor)
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


# ---- Brand ID ----

# ---- Price ----
x$item_price[x$item_price==0] <-NA

med.ip <- median(x$item_price, na.rm=TRUE)
x$item_price[is.na(x$item_price)] <- med.ip
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
med.age <- median(x$age, na.rm = TRUE)
x$age[is.na(x$age)] <- med.age
# ---- User State ----


# ---- IDEAS ----
# 1)  In case someone buys more stuff at once, the whole thing should obviously not be 
#     discouraged because one or two items are likely to be returned
#     rather try to get the one item out of the cart
# 2)  the likelihood of an item being returned has to be seen in respect to how much
#     would be lost if discouraged and wouldnt have been returned. It is worse if cheap items
#     are returned than expensive ones, because percentage wise the return is more expensive
#     as well as its 3€ plus 10 % ov value.
