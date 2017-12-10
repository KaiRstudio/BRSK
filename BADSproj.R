# GROUP 45 members:
#   - Bastian
#   - Rieke
#   - Sena
#   - Kai Dessau (559766) - kai-dessau@web.de


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

#######Converting dates
for (chrVar in c("order_date", "delivery_date", "user_reg_date", "user_dob")) {
 daten[, chrVar] <- as.Date(daten[[chrVar]])
}


# ---- Order Item ID ----
table(is.na(daten$order_item_id))

# ---- Order Date ----

# ---- Delivery Date ----
daten$delivery_date[daten$delivery_date == "1990-12-31"] <- NA # remove unrealistic times
sum(is.na(daten$delivery_date)) # should be zero

#-----Delivery Time------
#Subtract delivery date from order date so we can see "delivery time" which might be important for prediction.
daten$delivery.time<-daten$delivery_date - daten$order_date

# ---- Item ID ----

# ---- Size ----

# ---- Color ----

# ---- Brand ID ----

# ---- Price ----

# ---- User ID ----

# ---- Title ----
daten$user_title[daten$user_title == "not reported"] <- NA # remove not reported titles
table(daten$user_title)
# ---- Date of birth ----

# ---- User State ----

# ---- Registration Date ----


# ---- IDEAS ----
# 1)  In case someone buys more stuff at once, the whole thing should obviously not be 
#     discouraged because one or two items are likely to be returned
#     rather try to get the one item out of the cart
# 2)  the likelihood of an item being returned has to be seen in respect to how much
#     would be lost if discouraged and wouldnt have been returned. It is worse if cheap items
#     are returned than expensive ones, because percentage wise the return is more expensive
#     as well as its 3€ plus 10 % ov value.
