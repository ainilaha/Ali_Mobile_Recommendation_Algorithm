
# load data
library(plyr)
library(dplyr)
library(lubridate)
library(stringi)
#data_user <- read.csv("tianchi_fresh_comp_train_user.csv",header = TRUE,nrows = 1000)
#data_user$time <- stri_c(data_user$time,":00:00")

userActivation <- function(){
 usr_act <- data_user %>% group_by(user_id)%>% summarize(user_act = n())
}


userBuyRatio <- function(){
  usr_act <- user_activation()
  user_buy <- data_user %>% filter(behavior_type ==4)%>% group_by(user_id) %>% summarize(user_buy = n())
  buy_ratio <- merge(data_user,ui_act)%>% merge(uc_act) %>% mutate(buy_ratio = user_buy/user_act)
  
}

UWeightedRatio <- function(){
  user_buy <- data_user %>% filter(behavior_type ==4)%>% group_by(user_id) %>% summarize(user_buy = n())
  user_view <- data_user %>% filter(behavior_type ==1)%>% group_by(user_id) %>% summarize(user_view = n())
  user_collect <- data_user %>% filter(behavior_type ==2)%>% group_by(user_id) %>% summarize(user_collect = n())
  user_cart <- data_user %>% filter(behavior_type ==3)%>% group_by(user_id) %>% summarize(user_cart = n())
  weighted_ratio = merge(user_buy,user_view,all = TRUE) %>% merge(user_collect,all = TRUE) %>% merge(user_cart,all = TRUE)
  weighted_ratio[is.na(weighted_ratio)] <- 0
  weighted_ratio <- weighted_ratio %>% mutate(u_w_ratio = user_buy/(user_view+2*user_collect+3*user_cart))%>%mutate(u_w_ratio = ifelse(u_w_ratio == Inf,-1,u_w_ratio))
}

UViewCountByDate <- function(){
  data_user_tmp <- data_user
  data_user_tmp$time <- substr(data_user_tmp$time,1,10)
  user_buy <- data_user_tmp %>% filter(behavior_type == 4 ) %>% group_by_(.dots=list(quote(user_id), quote(time))) %>% summarize(u_buy = n())
  u_act_date <- data_user_tmp %>% group_by_(.dots=list(quote(user_id), quote(time))) %>% summarize(u_act = n())
  u_act_buy_date <- merge(user_buy,u_act_date) %>% group_by(user_id) %>% summarize(u_buy_day_ratio = mean(u_buy/u_act))
}


#user_buy_ratio <- userBuyRatio()
