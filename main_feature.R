library("sqldf")
#data_user <- read.csv("tianchi_fresh_comp_train_user.csv",header = TRUE,nrows = 1000)

data_user<-read.csv.sql(file="training/tianchi_user_nono2o_valid_time_slot-1.csv", sql="select * from file",  header=TRUE);
data_user <- as.data.frame(sapply(data_user, function(x) gsub("\"", "", x)))
data_user_unique <- read.csv.sql(file="training/tianchi_user_nono2o_valid_time_slot-1.csv", sql="select user_id,item_id,item_category,max(time) as max_time,min(time) as min_time from file group by user_id, item_id", header=TRUE) 
#data_user<-read.csv(file="training/tianchi_user_nono2o_valid_time_slot-1.csv", header=TRUE);
data_user_unique <- data_user %>% select(-behavior_type,-time) %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% filter(row_number()==1)
data_user_unique <- as.data.frame(sapply(data_user_unique, function(x) gsub("\"", "", x)))
#data_label <- read.csv()
source("feature_user.R")
u_act <- userActivation()
u_weighted_ratio <- UWeightedRatio()
u_view_by_date <- UViewCountByDate() # average(u_buy/u_act)
data_feature <- merge(u_weighted_ratio,u_act)%>% merge(u_view_by_date) %>% join(data_user_unique,by="user_id")



source("item_feature.R")
item_act = itemAcativation()
i_weighted_buy_ratio <- itemBuyRatioWeighted()
data_feature <- join(data_feature,i_weighted_buy_ratio,by = "item_id") %>% join(item_act,by = "item_id") 

source("category_feature.R")
cat_act <- categoryActivation()
c_weighted_buy_ratio <- catetoryBuyRatioWeighted()

data_feature <- join(cat_act,data_feature, by = "item_category") %>% join(c_weighted_buy_ratio, by = "item_category")

source("UI_feature.R")
ui_act <- UIAct()
ui_weighted_ratio <- UIWeightedRatio()

data_feature <- join(data_feature,ui_act,by = c("user_id","item_id")) %>% merge(ui_weighted_ratio,by = c("user_id","item_id"))

source("UC_feature.R")
uc_act <- UCAct()
uc_weighted_ratio <- UCWeightedRatio()

data_feature <- join(data_feature,uc_act,by =c("user_id","item_category")) %>% join(uc_weighted_ratio,by =c("user_id","item_category"))

source("time_feature.R")
ui_latest_act <-UILatestAct(labal_date = "2014-11-25 00")
ui_act_to_buy <- UIActToBuy()

data_feature <- join(data_feature,ui_latest_act,by = c("user_id","item_id")) %>% join(ui_act_to_buy,by = c("user_id","item_id"))

source("cross_feature.R")
ui_act_times_ratio <- UIActTimesRatio()
ui_act_by_uc_act <- UIActByUCAct()
ui_act_time_u_ratio <- UIActTimesURatio()
ui_act_by_u_act <- UIActByUAct()

data_feature <- join(data_feature,ui_act_times_ratio,by = c("user_id","item_id")) %>% join(ui_act_by_uc_act,by = c("user_id","item_id"))
data_feature <- join(data_feature,ui_act_time_u_ratio,by = c("user_id","item_id")) %>% join(ui_act_by_u_act,by = c("user_id","item_id"))
data_feature$ui_act_to_buy[is.na(data_feature$ui_act_to_buy)] <- -1

write.csv(data_feature,file = "training/data_feature1.csv",quote = FALSE)

#%>% merge(ui_act_by_uc_act)
#%>% join(ui_act_by_uc_act,by = c("user_id","item_id")) 
#%>% join(ui_act_by_u_act,by = c("user_id","item_id")) 
#%>% join(ui_act_time_u_ratio, by=c("user_id","item_id")) 
# merge(ui_act_by_uc_act,by = c("user_id","item_id")) 



