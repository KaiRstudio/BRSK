#   - Sebastian Gütgemann (589623) - s.guetgemann@gmx.net
#   - Rieke Ackermann (588611) - rieke.ackermann@web.de
#   - Sena Aydin (594644) - senaaydin484@gmail.com
#   - Kai Dessau (559766) - kai-dessau@web.de
setwd("~/Humboldt/BADS/BADS")

daten <- read.csv("BADS_WS1718_known.csv",
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
# if(!require("plyr"))          install.packages("plyr");         library("plyr")
# if(!require("dplyr"))         install.packages("dplyr");        library("dplyr")
# if(!require("stringdist"))    install.packages("stringdist");   library("stringdist")
# if(!require("rpart"))         install.packages("rpart");        library("rpart")
# if(!require("psych"))         install.packages("psych");        library("psych")
# if(!require("car"))           install.packages("car");          library("car")
# if(!require("Amelia"))        install.packages("Amelia");       library("Amelia")
# if(!require("boot"))          install.packages("boot", dep=T);  library("boot")
# if(!require("caret"))         install.packages("caret");        library("caret")
# if(!require("rpart"))         install.packages("rpart");        library("rpart")
# if(!require("rattle"))        install.packages("rattle");       library("rattle")
# if(!require("rpart.plot"))    install.packages("rpart.plot");   library("rpart.plot")
# if(!require("RColorBrewer"))  install.packages("RColorBrewer"); library("RColorBrewer")
# if(!require("rms"))           install.packages("rms");          library("rms")
# if(!require("pROC"))          install.packages("pROC");         library("pROC")
# if(!require("e1071"))         install.packages("e1071");        library("e1071")
# if(!require("randomforest"))  install.packages("randomforest"); library("randomforest")
# if(!require("hmeasure"))      install.packages("hmeasure");     library("hmeasure")
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
    age)} 

Z.outlier <- function(x){
  Zscore <- scale(x)
  x[Zscore >3] <- NA
  x[Zscore <(-3)] <- NA
  return(x)    
}



# ---- Order Item ID ----
table(is.na(daten$order_item_id))

# ---- Dates ----
daten$delivery_time <- daten$delivery_date - daten$order_date
daten$delivery.time_factor[is.na(daten$delivery_time)]<-"neverReturned"
daten$delivery.time_factor[daten$delivery_time<0]<-"rarelyReturned"
daten$delivery.time_factor[daten$delivery_time>=0]<-"returnedHalfTheTime" 
daten$delivery.time_factor<- as.factor(daten$delivery.time_factor)
#daten$delivery_date[daten$delivery_date == "1990-12-31"] <- NA # remove unrealistic times
#daten$delivery_date[daten$order_date>daten$delivery_date] <-NA
daten$order_month <- as.factor(months(daten$order_date)) # new column with month of delivery
#daten$delivery_time <- Z.outlier(daten$delivery_time) # removing outliers from delivery time
# MED_delivery <- round( median (daten$delivery_time, na.rm =TRUE)) # round( mean (daten$delivery_time, na.rm =TRUE)) gives median of 4 and mean of 11 for delivery time
# daten$delivery_date[is.na(daten$delivery_date)]<- daten$order_date[is.na(daten$delivery_date)] + MED_DEL
# daten$delivery_time[is.na(daten$delivery_time)] <- MED_DEL
daten$regorderdiff <- daten$order_date - daten$user_reg_date
hist(as.numeric(daten$regorderdiff)) # many people that registered and immediately bought, the rest is equally distributed up until 774 days
#max(daten$delivery_time, na.rm =TRUE) # delivery has to be timely after order date, the max is 151 days
#min(daten$delivery_time, na.rm =TRUE) # check that minimal delivery time is zero
#hist(daten$delivery_time) # Histogram of delivery_time
#sum(is.na(daten$delivery_date)) # should be zero
# ---- Item ID ----

# ---- Size ----
daten$item_size <- factor(toupper(daten$item_size))
table_size <- table(daten$item_size)
levels(daten$item_size) <- c(names(table_size), "Other")
daten$item_size[is.na(daten$item_size)] <- factor("Other")
daten[daten$item_size %in% names(table_size)[table_size < 100],"item_size"] <- factor("Other")
daten$item_size <- factor(daten$item_size)
# ---- Color ----
table_col_clust <- table(daten$item_color)
levels(daten$item_color) <- c(names(table_col_clust), "Other")
daten$item_color[is.na(daten$item_color)] <- factor("Other")
daten[daten$item_color %in% names(table_col_clust)[table_col_clust < 200],"item_color"] <- factor("Other")
daten$item_color <- factor(daten$item_color)

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
daten$pricecat <- daten$item_price
daten$pricecat <- cut(daten$pricecat,c(-1, 0, 20, 50, 100, 200, 400), labels=c(NA, "0-20", "20-50", "50-100", "100-200", "200-400"))

daten$item_price[daten$item_price == 0] <-NA
med_item_price <- median(daten$item_price, na.rm=TRUE)
daten$item_price[is.na(daten$item_price)] <- med_item_price
#daten$item_price <- Z.outlier(daten$item_price) #Are we sure that 399.95 is outlier?

# ---- User ID ----

# ---- Title ----
daten$user_title[daten$user_title == "not reported"] <- NA # remove not reported titles
table(daten$user_title)

daten$user_title<-as.character(daten$user_title)
daten$user_title[daten$user_title!='Mrs']<-'Other'
daten$user_title<-as.factor(daten$user_title)

# ---- Age ----
daten$age <- age(daten$user_dob,daten$order_date)

med.age <- median(daten$age, na.rm = TRUE)
daten$age[is.na(daten$age)] <- med.age
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
# 2)  The likelihood of an item being returned has to be seen in respect to how much
#     would be lost if discouraged and wouldnt have been returned. It is worse if cheap items
#     are returned than expensive ones, because percentage wise the return is more expensive
#     as well as its 3€ plus 10 % ov value.
# 3)  Maybe other categories as alternative to binning? a) dark-light contrasting
#     b) warm-cold contrast c) black-white contrast ... are dark colors returned more in summertime?
#     combining fasion colors with months and returns? colors  can look differently online.
# 4)  likelihood ratio test; AIC; step function (direction: both)
# 5)  Without knowing the item, statements about price or size can hardly be made