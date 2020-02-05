# create the raw and excel compare df's 

## RAW
# NEW and OLD and all exps combine 
rewards <- rbindlist(
  list(
    "new_sha" = sha_rewards_new,
    "old_sha" = sha_rewards_old,
    "new_lga" = lga_rewards_new,
    "old_lga" = lga_rewards_old,
    "new_pr" = pr_rewards_new,
    "old_pr" = pr_rewards_old
  ),
  idcol = "directory",
  fill = T
)

rewards <- rewards %>% mutate(exp = mgsub::mgsub(exp, c("PR([1-9]{1})$", paste0("TREATMENT", 1:4)), c("PR0\\1", paste0("PR0", 3:6, "_T0", 1:4))))


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
    labs(title = paste0(olivier_sha_measures[i], "_Raw_VS_Excel_U01_Olivier", "\n")) + 
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
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Cocaine/QC")

### QCing raw data





##################
## PR ############ 
##################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Cocaine/QC")

### QCing raw data



