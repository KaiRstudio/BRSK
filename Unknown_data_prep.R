# - Read unknown data
githubURL <- "https://raw.githubusercontent.com/KaiRstudio/BRSK/master/BADS_WS1718_class_20180115.csv"
nd <- source_data(githubURL, sha1 = "c09db6b674539c097b3d510429007190da761ec1", header = "auto", sep=",")


# - Prep unknown data by using respective formulas from original data prep

# ----------------------- Formatting

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


# ----------------------- Start: Prep Variables

# ---- Check whether IDs are known to dataset ----
# ---- Item_ID ----
levels(nd$item_id) <- c(levels(factor(nd$item_id)),"New")
nd$item_id[!(nd$item_id %in% woe.values$xlevels$item_id)] <- factor("New")
nd$item_id <- factor(nd$item_id)



# ---- User_ID ----
levels(nd$user_id) <- c(levels(factor(nd$user_id)),"New")
nd$user_id[!(nd$user_id %in% factor(woe.values$xlevels$user_id))] <- factor("New")
nd$user_id <- factor(nd$user_id)



# ---- Brand_ID ----
levels(nd$brand_id) <- c(levels(factor(nd$brand_id)),"New")
nd$brand_id[!(nd$brand_id %in% woe.values$xlevels$brand_id)] <- factor("New")
#nd$brand_id[nd$brand_id %in% single_bid || !(nd$brand_id %in% levels(train.split$brand_id))] <- factor("New")
nd$brand_id <- factor(nd$brand_id)



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
nd$order_month <- as.factor(months(nd$order_date))
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

drops <- c("delivery_date",
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

final <- predict(woe.values, newdata=nd, replace = TRUE)

# ----------------------- End: WoE


final <- final[,names(final) %in% c(names(test.woe),"order_item_id")]
