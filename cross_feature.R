

UIActTimesRatio <- function(){
  ui_act_times_ration <- merge(ui_act,ui_weighted_ratio,all = TRUE)
  ui_act_times_ration[is.na(ui_act_times_ration)] <- 0
  ui_act_times_ration <- ui_act_times_ration %>% mutate(ui_act_t_ratio = ui_act*ui_w_ratio )
  ui_act_times_ration <- ui_act_times_ration %>% select(user_id,item_id,ui_act_t_ratio)
}

UIActByUCAct <- function(){
 ui_act_by_uc_act <- join(data_user_unique,ui_act,by=c("user_id","item_id"))%>% join(uc_act,by = c("user_id","item_category")) %>% mutate(ui_act_by_uc_act = ui_act/uc_act) %>% mutate(ui_act_by_uc_act = ifelse(ui_act_by_uc_act == Inf,-1,ui_act_by_uc_act))
 ui_act_by_uc_act <- ui_act_by_uc_act %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize( ui_act_by_uc_act =mean(ui_act_by_uc_act))

 }

UIActTimesURatio <- function(){
  ui_act_t_u_ratio <- join(data_user_unique,ui_act, by =c("user_id","item_id")) %>% join(u_weighted_ratio,by = "user_id") %>% mutate(ui_act_ut_ratio = ui_act*u_w_ratio )
  ui_act_t_u_ratio <- ui_act_t_u_ratio %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize( ui_act_ut_ratio =mean(ui_act_ut_ratio))
}

UIActByUAct <- function(){
  ui_act_by_u_act <- join(data_user,ui_act, by =c("user_id","item_id")) %>% join(u_act,by = "user_id") %>% mutate(ui_act_by_u_act = ui_act/user_act)
  ui_act_by_u_act <- ui_act_by_u_act %>% group_by_(.dots=list(quote(user_id), quote(item_id))) %>% summarize( ui_act_by_u_act =mean(ui_act_by_u_act))
}