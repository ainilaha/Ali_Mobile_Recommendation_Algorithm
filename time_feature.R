UILatestAct <- function(labal_date="2016-10-02 00"){
  #labal date format should:"yyyy-mm-dd hh"
  #ui_latest_act<- data_user %>% group_by_(.dots=list(quote(user_id), quote(item_id)))%>% filter(ymd_h(time) == max(ymd_h(time)))%>% select(-behavior_type) %>% unique() %>% summarize(ui_latest_act =interval(ymd_h(time),ymd_h(labal_date)))%>% mutate(ui_latest_act = hour(as.period(ui_latest_act)))
  ui_latest_act<- data_user_unique %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_latest_act =interval(ymd_h(max_time),ymd_h(labal_date)))%>% mutate(ui_latest_act = hour(as.period(ui_latest_act)))
  #ui_latest_act<- data_user %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_latest_act =interval(max(ymd_h(time)),ymd_h(labal_date)))%>% mutate(ui_latest_act = hour(as.period(ui_latest_act)))
  #ui_latest_act<- data_user %>% group_by_(.dots=list(quote(user_id), quote(item_id)))%>% arrange(time) %>% filter(row_number()==n()) %>% summarize(ui_latest_act =interval(ymd_h(time),ymd_h(labal_date)))%>% mutate(ui_latest_act = hour(as.period(ui_latest_act)))
  
}
UIFirstToLast <- function(){
  ui_firt_to_last <- data_user_unique %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize(ui_firt_to_last =interval(ymd_h(max_time),ymd_h(min_time)))%>% mutate(ui_firt_to_last = hour(as.period(ui_latest_act)))
}


UIActToBuy <- function(){
  ui_buy <- data_user %>% filter(behavior_type ==4)%>% group_by_(.dots=list(quote(user_id), quote(item_id)))
  ui_buy_act <- data_user %>% subset(user_id %in% ui_buy$user_id) %>% subset(item_id %in% ui_buy$item_id) 
  ui_act_to_buy <- ui_buy_act %>%  group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize( ui_act_to_buy = interval(min(ymd_h(time)),max(ymd_h(time)))) %>% mutate(ui_act_to_buy = hour(as.period(ui_act_to_buy)))
}