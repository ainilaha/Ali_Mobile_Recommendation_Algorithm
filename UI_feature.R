UIAct <-function(){
  ui_act <- data_user %>% group_by_(.dots=list(quote(user_id), quote(item_id)))  %>% summarize(ui_act = n())
}

UIBuyRatio <- function(){
  ui_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_buy = n())
  ui_act <- UIAct()
  ui_ratio <- merge(ui_buy,ui_act,all = TRUE) %>%  mutate(ui_buy = ifelse(is.na(ui_buy),0,ui_buy),ui_ratio = ui_buy/ui_act)
}

UIBehaveToView <- function (behave){
  ui_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_buy = n())
  ui_behave <- data_user %>% filter(behavior_type ==behave)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_behave = n())
  ui_ratio <- merge(ui_buy,ui_behave,all = TRUE) %>%  mutate(ui_buy = ifelse(is.na(ui_buy),0,ui_buy),ui_ratio = ui_buy/ui_behave)
}
UIBack <- function(){
  #1: buy certain item more than once, whereas 0 is not
  back_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_buy = n()) %>% mutate(is_back = ifelse(item_id >1,0,1))
}

UIWeightedRatio <- function(){
  ui_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_buy = n())
  ui_view <- data_user %>% filter(behavior_type ==1)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_view = n())
  ui_collect <- data_user %>% filter(behavior_type ==2)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_collect = n())
  ui_cart <- data_user %>% filter(behavior_type ==3)%>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_cart = n())
  weighted_ratio = merge(ui_buy,ui_view,all = TRUE) %>% merge(ui_collect,all = TRUE) %>% merge(ui_cart,all = TRUE)
  weighted_ratio[is.na(weighted_ratio)] <- 0
  weighted_ratio <- weighted_ratio %>% mutate(ui_w_ratio = ui_buy/(ui_view+2*ui_collect+3*ui_cart))%>%mutate(ui_w_ratio = ifelse(ui_w_ratio == Inf,-1,ui_w_ratio))
  
}

#ui_act <- UIAct()
#ui_buy_to_view <- UIBehaveToView(1)
#ui_buy_to_collect <- UIBehaveToView(2)
#ui_buy_to_cart <- UIBehaveToView(3)
#back_buy <- UIBack()
#ui_weighted_ratio <- UIWeightedRatio()

