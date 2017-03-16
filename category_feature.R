categoryActivation <- function(){
  cat_act <- data_user %>% group_by(item_category) %>% summarise(cat_act = n())
}


catetoryBuyRatio <- function(){
  cat_act <- user_activation()
  cat_buy <- data_user %>% filter(behavior_type ==4)%>% group_by(item_category) %>% summarize(cat_buy = n())
  buy_ratio <- join(cat_buy,cat_act,by = c("item_category" = "item_category")) %>% mutate(buy_ratio = cat_buy/cat_act)
}

catetoryBuyRatioWeighted <- function(){
  cat_buy <- data_user %>% filter(behavior_type ==4)%>% group_by(item_category) %>% summarize(cat_buy = n())
  cat_view <- data_user %>% filter(behavior_type ==1)%>% group_by(item_category) %>% summarize(cat_view = n())
  cat_collect <- data_user %>% filter(behavior_type ==2)%>% group_by(item_category) %>% summarize(cat_collect = n())
  cat_cart <- data_user %>% filter(behavior_type ==3)%>% group_by(item_category) %>% summarize(cat_cart = n())
  weighted_ratio = merge(cat_buy,cat_view,all = TRUE) %>% merge(cat_collect,all = TRUE) %>% merge(cat_cart,all = TRUE)
  weighted_ratio[is.na(weighted_ratio)] <- 0
  weighted_ratio <- weighted_ratio %>% mutate(c_w_buy_ratio = cat_buy/(cat_view+2*cat_collect+3*cat_cart))%>%mutate(c_w_buy_ratio = ifelse(c_w_buy_ratio == Inf,-1,c_w_buy_ratio))
  
}
