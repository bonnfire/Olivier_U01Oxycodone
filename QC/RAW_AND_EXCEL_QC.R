# create the raw and excel compare df's 

## create phenotypes long and wide



## for the oxy team to qc 
# use brent's file to correct 
## sha to be qc'ed by oxy team

oxy_xl_sha_df <- bind_rows(sha_rewards_new, sha_rewards_old) %>%
  select(cohort, labanimalid, exp, rewards, filename) %>% 
  rename("rewards_raw" = "rewards") %>% 
  mutate(exp = tolower(exp)) %>% 
  left_join(olivieroxy_excel %>% select(matches("cohort|labanimalid|rfid|^sha\\d")) %>% 
              distinct() %>% 
              gather("exp", "rewards_xl", -cohort, -rfid, -labanimalid),
            by = c("cohort", "labanimalid", "exp")) %>% 
  mutate_at(vars(matches("^rewards")), as.numeric) %>% 
  mutate(rewards_QC_diff = rewards_xl - rewards_raw,
         rewards_QC = ifelse(rewards_QC_diff == 0, "pass", "fail"))

oxy_xl_sha_df %>% 
  subset(rewards_QC == "fail") %>% 
  mutate(sex = str_extract(labanimalid, "[MF]")) %>% 
  spread(exp, rewards_xl) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num) %>% select(-labanimalid_num) %>% 
  openxlsx::write.xlsx(file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxy_qc_sha.xlsx")

oxy_xl_sha_df_qced <- oxy_xl_sha_df %>% 
  left_join(openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/decision_oxy_qc_sha.xlsx") %>% select(rfid, filename, decision), by = c("rfid", "filename") ) %>% 
  rename("rewards" = "decision") %>% 
  mutate(rewards = coalesce(rewards, rewards_raw))

oxy_xl_sha_df_qced %>% ggplot(aes(x = rewards_raw, y = rewards_xl)) + geom_point()
oxy_xl_sha_df_qced %>% ggplot(aes(x = rewards_raw, y = rewards)) + geom_point()


## lga to be qc'ed by oxy team 

oxy_xl_lga_df <- bind_rows(lga_rewards_new, lga_rewards_old) %>%
  select(cohort, labanimalid, exp, rewards, filename) %>% 
  rename("rewards_raw" = "rewards") %>% 
  mutate(exp = tolower(exp)) %>% 
  left_join(olivieroxy_excel %>% select(matches("cohort|labanimalid|rfid|^lga\\d")) %>% 
              distinct() %>% 
              gather("exp", "rewards_xl", -cohort, -rfid, -labanimalid),
            by = c("cohort", "labanimalid", "exp")) %>% 
  mutate_at(vars(matches("^rewards")), as.numeric) %>% 
  mutate(rewards_QC_diff = rewards_xl - rewards_raw,
         rewards_QC = ifelse(rewards_QC_diff == 0, "pass", "fail")) %>% 
  subset(!(is.na(labanimalid)|labanimalid == "NA"))

oxy_xl_lga_df %>% 
  subset(rewards_QC == "fail") %>% # XX come back to figure out the NA w comments and all (ex: F111 NA but accounted for in the Excel data comment) 
  mutate(sex = str_extract(labanimalid, "[MF]")) %>% 
  spread(exp, rewards_xl) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num) %>% select(-labanimalid_num) %>% 
  openxlsx::write.xlsx(file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxy_qc_lga.xlsx")


oxy_xl_lga_df_qced <- oxy_xl_lga_df %>% 
  left_join(openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/decision_oxy_qc_lga.xlsx") %>% select(rfid, filename, decision), by = c("rfid", "filename") ) %>% 
  rename("rewards" = "decision") %>% 
  mutate(rewards = coalesce(rewards, rewards_raw))

oxy_xl_lga_df_qced %>% ggplot(aes(x = rewards_raw, y = rewards_xl)) + geom_point()
oxy_xl_lga_df_qced %>% ggplot(aes(x = rewards_raw, y = rewards)) + geom_point()


# pr to be qc'ed by oxy team 

oxy_xl_pr_df <- bind_rows(pr_rewards_new, pr_rewards_old) %>%
  select(cohort, labanimalid, exp, rewards, filename) %>% 
  rename("rewards_raw" = "rewards") %>% 
  mutate(exp = tolower(exp)) %>% 
  left_join(olivieroxy_excel %>% select(matches("cohort|labanimalid|rfid|^pr\\d")) %>% 
              distinct() %>% 
              gather("exp", "rewards_xl", -cohort, -rfid, -labanimalid),
            by = c("cohort", "labanimalid", "exp")) %>% 
  mutate_at(vars(matches("^rewards")), as.numeric) %>% 
  mutate(rewards_QC_diff = rewards_xl - rewards_raw,
         rewards_QC = ifelse(rewards_QC_diff == 0, "pass", "fail"))
  

oxy_xl_pr_df %>% 
  subset(rewards_QC == "fail") %>% 
  mutate(sex = str_extract(labanimalid, "[MF]")) %>% 
  spread(exp, rewards_xl) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num) %>% select(-labanimalid_num) %>% 
  openxlsx::write.xlsx(file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxy_qc_pr.xlsx")




## RAW
# NEW and OLD and all exps combine 
rewards <- rbindlist(
  list(
    "new_sha" = sha_rewards_new_valid,
    "old_sha" = sha_rewards_old,
    "new_lga" = lga_rewards_new_valid,
    "old_lga" = lga_rewards_old,
    "new_pr" = pr_rewards_new,
    "old_pr" = pr_rewards_old
  ),
  idcol = "directory",
  fill = T
) ## 3166 observations from C01-7, no 2


######### JOIN TO WFU DATABASE 

# add notes about missingness (file or dead)

Olivier_Oxycodone_df <- WFU_OlivierOxycodone_test_df %>% select(cohort, rfid, comment) %>% 
  # rename("wfu_labanimalid" = "labanimalid") %>%
  mutate(cohort = paste0("C", cohort)) %>%
  dplyr::filter(grepl("^\\d", rfid)) %>% #525 (ignore the blanks and annotations in the excel)
  left_join(., olivieroxy_excel[, c("labanimalid", "rfid")], by = "rfid") %>% # add labanimalid number
  left_join(., ratinfo_list_deaths_processed %>% select(-c("naive", "datedropped")) %>% subset(grepl("surgery", reasoning)), by = c("rfid", "cohort")) %>% # 525 # deaths/compromises before any experiments 
  left_join(., ratinfo_list_replacements_processed %>% subset(grepl("^RENUMBERED", comment, ignore.case = T)) %>% select(cohort, originalrat, replacement), by = c("tailmark"="originalrat", "cohort")) %>% # replacements, when the animal dies labanimalid changes XX WAITING FOR THEM TO CONFIRM MISSING RFID
  left_join(., ratinfo_list_replacements_processed %>% subset(grepl("Not Renumbered", comment, ignore.case = T)) %>% select(cohort, rfidreplacement), by = c("rfid"="rfidreplacement", "cohort")) %>% 
  mutate(labanimalid = coalesce(labanimalid, replacement),
         tailmark = ifelse(!is.na(tailmark), paste(tailmark, "originally but replaced"), tailmark),
         comment = ifelse(!is.na(reasoning)&is.na(comment), reasoning,
                          ifelse(!is.na(reasoning)&!is.na(comment), paste0(comment, "; ", reasoning), comment))) %>%
  select(-c("replacement", "reasoning")) %>% # replacements, when the animal is the replacement labanimalid changes XX WAITING FOR THEM TO CONFIRM MISSING RFID
  left_join(., computernotes_oxy %>% subset(!grepl("cohort_notes", exp)) %>% select(cohort, exp, computernote), by = "cohort") %>% # 21525 (explains missing files for every session, every rat)
  rename("computernote_exp" = "computernote") %>% 
  left_join(., computernotes_oxy %>% subset(grepl("cohort_notes", exp)) %>% select(cohort, computernote), by = "cohort") %>% 
  # rowwise() %>% 
  # mutate(comment = ifelse(grepl(computernote, filename)&is.na(comment), reasoning,
  #                         ifelse(!is.na(reasoning)&!is.na(comment), paste0(comment, "; ", reasoning), comment))) %>% # 21525 (explains missing files for every session, every rat)
  left_join(., rewards, by = c("labanimalid", "cohort", "exp")) %>% # 21526 ## ADDING THE RAW REWARDS DATA # M155 WFU_OlivierOxycodone_test_df %>%rename("wfu_labanimalid" = "labanimalid") %>%mutate(cohort = paste0("C", cohort)) %>%dplyr::filter(grepl("^\\d", rfid)) %>% #811 (ignore the blanks and annotations in the excel)left_join(., olivieroxy_excel[, c("labanimalid", "rfid")], by = "rfid") %>% # add labanimalid numberleft_join(., computernotes_oxy, by = "cohort") %>% # 21525 (explains missing files for every session, every rat)subset(cohort == "C01") %>% add_count(labanimalid) %>% rename("n_cnotes_count" = "n") %>% left_join(., rewards, by = c("labanimalid", "cohort", "exp")) %>% add_count(labanimalid) %>% rename("n_crewards_count" = "n") %>% subset(n_cnotes_count != n_crewards_count) %>% View() 
  left_join(., ratinfo_list_deaths_processed %>% select(-c("tailmark", "naive")), by = c("rfid", "cohort")) %>% # 21608 # deaths/compromises # look back on 933000120138753 and 933000320047461 # bc we are trying to use data for as many days as possible, so hteh deatsh table may have repeats
  mutate_at(vars(contains("date")), lubridate::ymd) %>%
  group_by(labanimalid) %>%
  mutate(
    flag = case_when(
      grepl("Died", reasoning) & date >= datedropped ~ "DEAD_EXCLUDE", ## if the animal has died, remove all data on the data and after
      !grepl("Died", reasoning) & date == datedropped ~ "COMP_EXCLUDE" ## if the animal was compromised, only flag that day
    )
  ) %>%
  ungroup() %>% 
  select(cohort, rfid, labanimalid, exp, rewards, date, time, filename, tailmark, computernote_exp, computernote, everything())
## fix below code, bc grepl won't know to parse each character cell
%>% 
  rowwise() %>% 
  mutate(comment = ifelse(grepl(filename, computernote)&!is.na(comment), paste0("EXPECTED RAW MISSING; ", comment), 
                          ifelse(grepl(filename, computernote)&is.na(comment), "EXPECTED RAW MISSING", comment)))

## why only 200+ unique labanimals even though there are 500+ unique rfid's
Olivier_Oxycodone_df %>% distinct(rfid) %>% dim
WFU_OlivierOxycodone_test_df %>% distinct(rfid) %>% dim
Olivier_Oxycodone_df %>% distinct(labanimalid, rfid) %>% get_dupes(labanimalid)

WFU_OlivierOxycodone_test_df %>%
  rename("wfu_labanimalid" = "labanimalid") %>%
  mutate(cohort = paste0("C", cohort)) %>%
  dplyr::filter(grepl("^\\d", rfid)) %>% #811 (ignore the blanks and annotations in the excel)
  left_join(., olivieroxy_excel[, c("labanimalid", "rfid")], by = "rfid") %>% # add labanimalid number
  left_join(., computernotes_oxy, by = "cohort") %>% # 21525 (explains missing files for every session, every rat)
  ### PICK UP HERE AND EXTRACT THE COMPUTER NOTES EXCEL
  # left_join(., ratinfo_list_replacements_processed, by = c("rfid", "cohort")) # replacements XX WAITING FOR THEM TO CONFIRM MISSING RFID
  left_join(., rewards, by = c("labanimalid", "cohort", "exp")) %>% 
  subset(cohort %in% c("C01", "C04")) %>% add_count(labanimalid) %>% rename("n_rewards_count" = "n") %>% 
  left_join(., ratinfo_list_deaths_processed, by = c("rfid", "cohort")) %>% add_count(labanimalid) %>% rename("n_deaths_count" = "n") %>% subset(n_rewards_count!=n_deaths_count)


# left_join(.,
#   allcohorts2 %>% select(labanimalid, rfid, matches("^sha")) %>% distinct() %>%
#     gather(exp, rewards_excel, sha01:sha10) %>% mutate(exp = toupper(exp)),
#   by = c("labanimalid", "rfid", "exp")
# ) %>% ## 5/20 not sure why added this, perhaps to add excel rewards data
# rename("rewards_raw" = "rewards",
#        "exp_date" = "date",
#        "exp_time" = "time") %>% # 15527


WFU_OlivierOxycodone_test_df %>% select(cohort, rfid, exp, rewards, datedropped, flag) %>% subset(!is.na(flag))

## join to calculate the addiction index 
%>% left_join(tail_immersion_df, by = c("cohort", "labanimalid", "rfid")) %>% ## 01/29/2021 fix the rfid issue w naive animals first 
  left_join(von_frey_df[, c("rfid", "pain_force_per_rat_bsl", "pain_force_per_rat_wd")], by = "rfid") %>% 
  mutate(tolerance = diff(x12h_wd_w_oxy_s, oxy_on_board_s),
         pain = diff(pain_force_per_rat_bsl, pain_force_per_rat_wd)) %>% 
  group_by(cohort, sex) %>% 
  mutate(tolerance_mean = mean(tolerance, na.rm = T),
         tolerance_sd = sd(tolerance, na.rm = T), 
         pain_mean = mean(pain, na.rm = T), 
         pain_sd = sd(pain, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(tolerance_index = (tolerance - tolerance_mean)/tolerance_sd,
         pain_index = (pain - pain_mean)/pain_sd) %>% 
  mutate(addiction_index = rowMeans(select(., ends_with("index")), na.rm = TRUE)) %>% 









olivieroxy_excel %>% distinct()

## EXCEL LONG FORMAT

olivieroxy_excel %>% 
  select(cohort, labanimalid, rfid, matches("^(sha|pr|lga)")) %>% 
  mutate_at(vars(matches("^(sha|pr|lga)")), as.numeric) %>% 
  gather(exp, rewards_excel, sha01:sha06_special) %>% 
  mutate(exp = toupper(exp)) 


### FIRST ATTEMPT TO MERGE 
oxy_rawvsexcel <-
  rewards %>% left_join(
    .,
    olivieroxy_excel %>% 
      select(cohort, labanimalid, rfid, matches("^(sha|pr|lga)")) %>%
      mutate_all(as.character) %>%
      mutate_at(vars(matches("^(sha|pr|lga)")), as.numeric) %>%
      gather(exp, rewards_excel, sha01:sha06_special) %>%
      mutate(exp = toupper(exp)),
    by = c("labanimalid", "cohort", "exp")
  ) %>% 
  rename("rewards_raw" = "rewards") %>% 
  mutate_at(vars(matches("rewards")), as.numeric)


# oxy_rawvsexcel %>% subset(is.na(rewards_excel)) %>% mutate_at(vars(one_of("directory", "labanimalid", "cohort", "exp")), as.factor) %>% summary
# most of the na from the excel coems from the unlabelled id's in new lga 

# V REASSURING olivieroxy_excel %>% distinct(labanimalid) %>% nrow() ==  olivieroxy_excel %>% nrow()


##################
## SHA ########### 
##################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/QC")
olivier_sha_measures <- grep("rewards", names(oxy_rawvsexcel), value = T) 

# create plots 
pdf("olivier_sha.pdf", onefile = T)
for (i in 1:(length(olivier_sha_measures)/2)){
  g <-  oxy_rawvsexcel %>% subset(grepl("SHA", exp)) %>% 
    ggplot(aes_string(x = olivier_sha_measures[i], y = olivier_sha_measures[i+1])) + 
    geom_point(aes(color = directory)) + 
    labs(title = paste0("SHA_", olivier_sha_measures[i], "_Raw_VS_Excel_U01_Olivier", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(rewards_sha_tograph, aes_string(x = olivier_sha_measures[i], y = olivier_sha_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(olivier_sha_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()
# oxy_rawvsexcel %>% dim
# oxy_rawvsexcel %>% subset(grepl("SHA", exp)) %>% subset(rewards_raw != rewards_excel) %>% dim # 48/1330 (3.6% of the data)
# oxy_rawvsexcel %>% subset(rewards_raw == rewards_excel) %>% dim
# oxy_rawvsexcel %>% subset(rewards_raw != rewards_excel) %>% 
#   select(labanimalid, exp, filename, rewards_raw, rewards_excel) %>% 
#   openxlsx::write.xlsx("sha_compare.xlsx")
### QCing raw data





##################
## LGA ########### 
##################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/QC")
olivier_lga_measures <- grep("rewards", names(oxy_rawvsexcel), value = T) 

# create plots 
pdf("olivier_lga.pdf", onefile = T)
for (i in 1:(length(olivier_lga_measures)/2)){
  g <-  oxy_rawvsexcel %>% subset(grepl("LGA", exp)) %>% 
    ggplot(aes_string(x = olivier_lga_measures[i], y = olivier_lga_measures[i+1])) + 
    geom_point(aes(color = directory)) + 
    labs(title = paste0("LGA_", olivier_lga_measures[i], "_Raw_VS_Excel_U01_Olivier", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # g_cohort <-  ggplot(rewards_lga_tograph, aes_string(x = olivier_lga_measures[i], y = olivier_lga_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(olivier_lga_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()
# oxy_rawvsexcel %>% subset(grepl("LGA", exp)) %>% dim
# oxy_rawvsexcel %>% subset(grepl("LGA", exp)) %>% subset(rewards_raw != rewards_excel) %>% dim # 141/4629 (3.0% of the data)
# oxy_rawvsexcel %>% subset(rewards_raw == rewards_excel) %>% dim
# oxy_rawvsexcel %>% subset(rewards_raw != rewards_excel) %>% 
#   select(labanimalid, exp, filename, rewards_raw, rewards_excel) %>% 
#   openxlsx::write.xlsx("lga_compare.xlsx")
### QCing raw data





##################
## PR ############ 
##################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/QC")
olivier_pr_measures <- grep("rewards", names(oxy_rawvsexcel), value = T) 

# create plots 
pdf("olivier_pr.pdf", onefile = T)
for (i in 1:(length(olivier_pr_measures)/2)){
  g <-  oxy_rawvsexcel %>% subset(grepl("PR", exp)) %>% 
    ggplot(aes_string(x = olivier_pr_measures[i], y = olivier_pr_measures[i+1])) + 
    geom_point(aes(color = directory)) + 
    labs(title = paste0("PR_", olivier_pr_measures[i], "_Raw_VS_Excel_U01_Olivier", "\n")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    coord_cartesian(xlim = c(0, 300)) 
  
  # g_cohort <-  ggplot(rewards_pr_tograph, aes_string(x = olivier_pr_measures[i], y = olivier_pr_measures[i+3])) + 
  #   geom_point(aes(color = cohort_number)) + 
  #   facet_grid(~ cohort_number)
  #   labs(title = paste0(olivier_pr_measures[i], "_Raw_VS_Excel_U01_Kalivas", "\n")) + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  print(g)
  # print(g_cohort)
}

dev.off()
# oxy_rawvsexcel %>% subset(grepl("PR", exp)) %>% dim
# oxy_rawvsexcel %>% subset(grepl("PR", exp)) %>% subset(rewards_raw != rewards_excel) %>% dim # 249/1161 (21.4% of the data)
# oxy_rawvsexcel %>% subset(rewards_raw == rewards_excel) %>% dim
# oxy_rawvsexcel %>% subset(rewards_raw != rewards_excel) %>% 
#   select(labanimalid, exp, filename, rewards_raw, rewards_excel) %>% 
#   openxlsx::write.xlsx("pr_compare.xlsx")
### QCing raw data


