### EXTRACT PROBLEMATIC RAT (Excel provided by the Olivier lab)


### OXYCODONE ### 


setwd("~/Dropbox (Palmer Lab)/Olivier_George_U01/Rat Information/Oxycodone")


ratinfo_list_deaths <- lapply(list.files(pattern = ".xlsx"), function(x){ 
  x <- u01.importxlsx(x)[[2]] %>% 
    clean_names() %>%
    mutate(cohort = paste0("C", str_pad(cohort, 2, "left", "0")),
           rfid = as.character(rfid),
           tailmark = str_match(tail_mark, "(M|F)[[0-9]]+")[, 1],
           naive = ifelse(grepl("Naive", tail_mark, ignore.case = T), "yes", "no"))
  if(!lubridate::is.POSIXct(x$day_excluded)){
    x <- x %>% 
      mutate(datedropped = openxlsx::convertToDateTime(day_excluded)) %>% 
      select(-day_excluded)
  }
  else{ 
    x <- x %>% 
      mutate(datedropped = day_excluded)
  }
  x <- x %>%
    mutate_all(as.character) %>% 
    mutate(datedropped = replace(datedropped, datedropped == "9/12/207", "2017-09-12")) %>%
    rename("reasoning" = "reasoning") %>%
    select(cohort, rfid, tailmark, naive, datedropped, reasoning)
  return(x)
}) %>% rbindlist() 


ratinfo_list_replacements <- lapply(list.files(pattern = ".xlsx"), function(x){ 
  if(length(u01.importxlsx(x)) > 2){ 
    x <- u01.importxlsx(x)[[3]] %>% 
      clean_names() %>%
      mutate(cohort = paste0("C", str_pad(cohort, 2, "left", "0")),
             originalrat = str_match(original_rat, "(M|F)[[0-9]]+")[, 1],
             replacement = str_match(replaced_with, "(M|F)[[0-9]]+")[, 1],
             rfidreplacement = as.character(rfid_of_replaced)) 
    
    names(x) <- gsub("x5", "comment", names(x))
    
    return(x)
  }
}) %>% rbindlist(fill = T) %>%
  select(cohort, originalrat, replacement, rfidreplacement, comment)





# check to see that all rats in replacements list are accounted for in deaths table
if(ratinfo_list_replacements %>% subset(!originalrat %in% ratinfo_list_deaths$tailmark) %>% nrow() != 0){
  ratinfo_list_replacements %>% subset(!originalrat %in% ratinfo_list_deaths$tailmark)
}
# if 0, proceed to left the replacements to dead animals table 

compromised_rats <- left_join(ratinfo_list_deaths, ratinfo_list_replacements %>% 
                                select(-cohort), by = c("tailmark" = "originalrat")) %>% 
  rename("death_comment" = "reasoning",
         "rfid_compromised" = "rfid") %>% 
  mutate(comment = str_to_title(comment)) %>% 
  mutate(labanimalid = ifelse(grepl("^Not Renumbered", comment), replacement, tailmark),
         rfid = ifelse(grepl("Renumbered", comment), rfidreplacement, rfid_compromised)) %>% 
  # mutate(labanimalid = coalesce(rfidreplacement, tailmark)) %>%  # labanimalid is the one ultimately used 
  mutate(death_comment = gsub("^ | $", "", death_comment))
 
rm(list = ls(pattern = "ratinfo_list_deaths|ratinfo_list_replacements"))












# ratinfo_list_deaths <- lapply(list.files(pattern = ".xlsx"), function(x){ 
#   x <- u01.importxlsx(x)[[2]] %>% 
#     clean_names() %>%
#     mutate(cohort = paste0("C", str_pad(cohort, 2, "left", "0")),
#            rfid = as.character(rfid),
#            tailmark = str_match(tail_mark, "(M|F)[[0-9]]+")[, 1],
#            naive = ifelse(grepl("Naive", tail_mark, ignore.case = T), "yes", "no"))
#   if(!lubridate::is.POSIXct(x$day_excluded)){
#     x <- x %>% 
#       mutate(datedropped = openxlsx::convertToDateTime(day_excluded)) %>% 
#       select(-day_excluded)
#   }
#   else{
#     x <- x %>% 
#       mutate(datedropped = day_excluded)
#   }
#   x <- x %>%
#     mutate_all(as.character) %>% 
#     mutate(datedropped = replace(datedropped, datedropped == "9/12/207", "2017-09-12")) %>%
#     rename("reasoning" = "reasoning") %>%
#     select(cohort, rfid, tailmark, naive, datedropped, reasoning)
#   return(x)
# }) %>% rbindlist() 
# 
# 
# ratinfo_list_deaths <- u01.importxlsx("Rat Information - All.xlsx")[[3]] # `Oxycodone - Deaths and Fails`
# ratinfo_list_replacements <- u01.importxlsx("Rat Information - All.xlsx")[[4]] # `Oxy - Rat Replacements`
# 
# ratinfo_list_deaths_processed <- ratinfo_list_deaths %>% 
#   clean_names() %>% 
#   mutate(cohort = paste0("C", str_pad(cohort, 2, "left", "0")),
#          rfid = as.character(rfid), 
#          tailmark = str_match(`tail_mark`, "(M|F)[[0-9]]+")[, 1],
#          naive = ifelse(grepl("Naive", `tail_mark`, ignore.case = T), "yes", "no"),
#          datedropped = openxlsx::convertToDateTime(`day_excluded`)) %>% 
#   # datedropped = replace(datedropped, `Day Excluded` == "9/12/207", "2017-09-12"))  %>% 
#   separate_rows(c(day_excluded, reasoning),  sep = ";", convert = FALSE) %>% 
#   mutate(datedropped = coalesce(lubridate::ymd(datedropped), lubridate::mdy(day_excluded))) %>% 
#   select(cohort, rfid, tailmark, naive, datedropped, reasoning ) %>% as.data.frame()
# 
# 
# ratinfo_list_replacements_processed <- ratinfo_list_replacements %>% 
#   mutate(cohort = paste0("C", str_pad(Cohort, 2, "left", "0")),
#          originalrat = str_match(`Original Rat`, "(M|F)[[0-9]]+")[, 1],
#          replacement = str_match(`Replaced with`, "(M|F)[[0-9]]+")[, 1],
#          rfidreplacement = as.character(`RFID of Replaced`)) %>% 
#   rename("comment" = `...5`) %>% 
#   select(cohort, originalrat, replacement, rfidreplacement, comment)
# 
# 
# 
# compromised_rats <- left_join(ratinfo_list_deaths_processed, ratinfo_list_replacements_processed %>% 
#                                 select(-cohort), by = c("tailmark" = "originalrat")) %>% 
#   rename("death_comment" = "reasoning",
#          "rfid_compromised" = "rfid") %>% 
#   mutate(comment = str_to_title(comment)) %>% 
#   mutate(labanimalid = ifelse(grepl("^Not Renumbered", comment), replacement, tailmark),
#          rfid = ifelse(grepl("Renumbered", comment), rfidreplacement, rfid_compromised)) %>% 
#   # mutate(labanimalid = coalesce(rfidreplacement, tailmark)) %>%  # labanimalid is the one ultimately used 
#   mutate(death_comment = gsub("^ | $", "", death_comment))



# ratinfo_excel <- list.files(pattern = ".*Cocaine.*")
# ratinfo_list <- lapply(ratinfo_excel, u01.importxlsx)
# names(ratinfo_list) <- ratinfo_excel

# lapply(ratinfo_list, function(x){
#   x <- rbindlist()
# })
# 
# cocaine_deaths <- lapply(list, `[[`, 1) %>% bind_rows()
# cocaine_replacement <- lapply(list, `[[`, 1) %>% bind_rows()