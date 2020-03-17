### EXTRACT PROBLEMATIC RAT (Excel provided by the Olivier lab)


### OXYCODONE ### 


setwd("~/Dropbox (Palmer Lab)/Olivier_George_U01/Rat Information")



ratinfo_list_deaths <- u01.importxlsx("Rat Information - All.xlsx")[[3]] # `Oxycodone - Deaths and Fails`
ratinfo_list_replacements <- u01.importxlsx("Rat Information - All.xlsx")[[4]] # `Oxy - Rat Replacements`

ratinfo_list_deaths_processed <- ratinfo_list_deaths %>% 
  clean_names() %>% 
  mutate(cohort = paste0("C", str_pad(cohort, 2, "left", "0")),
         rfid = as.character(rfid), 
         tailmark = str_match(`tail_mark`, "(M|F)[[0-9]]+")[, 1],
         naive = ifelse(grepl("Naive", `tail_mark`, ignore.case = T), "yes", "no"),
         datedropped = openxlsx::convertToDateTime(`day_excluded`)) %>% 
  # datedropped = replace(datedropped, `Day Excluded` == "9/12/207", "2017-09-12"))  %>% 
  separate_rows(c(day_excluded, reasoning),  sep = ";", convert = FALSE) %>% 
  mutate(datedropped = coalesce(lubridate::ymd(datedropped), lubridate::mdy(day_excluded))) %>% 
  select(cohort, rfid, tailmark, naive, datedropped, reasoning ) %>% as.data.frame()


ratinfo_list_replacements_processed <- ratinfo_list_replacements %>% 
  mutate(cohort = paste0("C", str_pad(Cohort, 2, "left", "0")),
         originalrat = str_match(`Original Rat`, "(M|F)[[0-9]]+")[, 1],
         replacement = str_match(`Replaced with`, "(M|F)[[0-9]]+")[, 1],
         rfidreplacement = as.character(`RFID of Replaced`)) %>% 
  rename("comment" = `...5`) %>% 
  select(cohort, originalrat, replacement, rfidreplacement, comment)




# ratinfo_excel <- list.files(pattern = ".*Cocaine.*")
# ratinfo_list <- lapply(ratinfo_excel, u01.importxlsx)
# names(ratinfo_list) <- ratinfo_excel

# lapply(ratinfo_list, function(x){
#   x <- rbindlist()
# })
# 
# cocaine_deaths <- lapply(list, `[[`, 1) %>% bind_rows()
# cocaine_replacement <- lapply(list, `[[`, 1) %>% bind_rows()