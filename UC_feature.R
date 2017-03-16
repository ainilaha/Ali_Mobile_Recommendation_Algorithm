
UCAct <- function(){
  uc_act <- data_user %>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_act = n())
}
UCBehaveToView <- function (behave){
  uc_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_buy = n())
  uc_behave <- data_user %>% filter(behavior_type ==behave)%>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_behave = n())
  uc_ratio <- merge(uc_buy,uc_behave,all = TRUE) %>%  mutate(uc_buy = ifelse(is.na(uc_buy),0,uc_buy),uc_ratio = uc_buy/uc_behave)
}


UCWeightedRatio <- function(){
  uc_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_buy = n())
  uc_view <- data_user %>% filter(behavior_type ==1)%>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_view = n())
  uc_collect <- data_user %>% filter(behavior_type ==2)%>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_collect = n())
  uc_cart <- data_user %>% filter(behavior_type ==3)%>% group_by_(.dots=list(quote(user_id), quote(item_category))) %>% summarize(uc_cart = n())
  weighted_ratio = merge(uc_buy,uc_view,all = TRUE) %>% merge(uc_collect,all = TRUE) %>% merge(uc_cart,all = TRUE)
  weighted_ratio[is.na(weighted_ratio)] <- 0
  weighted_ratio <- weighted_ratio %>% mutate(buy_ratio = uc_buy/(uc_view+2*uc_collect+3*uc_cart))%>%mutate(buy_ratio = ifelse(buy_ratio == Inf,-1,buy_ratio))
  
}


#uc_act <- UCAct()
#uc_buy_to_view <- UCBehaveToView(1)
#uc_buy_to_collect <- UCBehaveToView(2)
#uc_buy_to_cart <- UCBehaveToView(3)
#uc_weighted_ratio <- UCWeightedRatio()
