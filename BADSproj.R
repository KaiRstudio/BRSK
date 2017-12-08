# GROUP 45 members:
#   - Bastian
#   - Rieke
#   - Sena
#   - Kai Dessau (559766) - kai-dessau@web.de


# ---- General Terms ----
formatofdate <- "%Y-%m-%d"

# ---- Order Item ID ----
table(is.na(daten$order_item_id))

# ---- Order Date ----
daten$order_date <- as.Date(daten$order_date, format = formatofdate)
# ---- Delivery Date ----
daten$delivery_date <- as.Date(daten$delivery_date, format = formatofdate)
daten$delivery_date[daten$delivery_date == "1990-12-31"] <- NA # remove unrealistic times
sum(is.na(daten$delivery_date)) # should be zero
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
daten$user_dob <- as.Date(daten$user_dob, format = formatofdate)
# ---- User State ----

# ---- Registration Date ----
daten$user_reg_date <- as.Date(daten$user_reg_date, format = formatofdate)



# ---- IDEAS ----
# 1)  In case someone buys more stuff at once, the whole thing should obviously not be 
#     discouraged because one or two items are likely to be returned
#     rather try to get the one item out of the cart
# 2)  the likelihood of an item being returned has to be seen in respect to how much
#     would be lost if discouraged and wouldnt have been returned. It is worse if cheap items
#     are returned than expensive ones, because percentage wise the return is more expensive
#     as well as its 3€ plus 10 % ov value.
