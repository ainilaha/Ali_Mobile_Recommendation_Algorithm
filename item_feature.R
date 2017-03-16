itemAcativation <- function(){
  item_act <- data_user %>% group_by(item_id) %>% summarize(item_act = n())
}

itemBuyRatio <- function(){
  item_act <- itemAcativation()
  item_buy <- data_user %>% filter(behavior_type ==4)%>% group_by(item_id) %>% summarize(item_buy = n())
  buy_ratio <- join(item_buy,item_act,by = c("item_id" = "item_id")) %>% mutate(i_ratio = item_buy/item_act)
  
}
itemBuyRatioWeighted <- function(){
  item_buy <- data_user %>% filter(behavior_type ==4)%>% group_by(item_id) %>% summarize(item_buy = n())
  item_view <- data_user %>% filter(behavior_type ==1)%>% group_by(item_id) %>% summarize(item_view = n())
  item_collect <- data_user %>% filter(behavior_type ==2)%>% group_by(item_id) %>% summarize(item_collect = n())
  item_cart <- data_user %>% filter(behavior_type ==3)%>% group_by(item_id) %>% summarize(item_cart = n())
  weighted_ratio = merge(item_buy,item_view,all = TRUE) %>% merge(item_collect,all = TRUE) %>% merge(item_cart,all = TRUE)
  weighted_ratio[is.na(weighted_ratio)] <- 0
  weighted_ratio <- weighted_ratio %>% mutate(i_w_ratio = item_buy/(item_view+2*item_collect+3*item_cart))%>%mutate(i_w_ratio = ifelse(i_w_ratio == Inf,-1,i_w_ratio))
}
