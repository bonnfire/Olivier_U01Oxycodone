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

olivieroxy_excel %>% clean_names() %>% # use this fxn to return df, use make_clean_names on vector 
  rename("labanimalid" = "rat") %>%  
  select(cohort, labanimalid, rfid, matches("^(sha|pr|lga)")) %>% 
  mutate_at(vars(matches("^(sha|pr|lga)")), as.numeric) %>% 
  gather(exp, rewards_excel, sha01:sha06_special) %>% 
  mutate(exp = toupper(exp)) 


### FIRST ATTEMPT TO MERGE 
oxy_rawvsexcel <-
  rewards %>% left_join(
    .,
    olivieroxy_excel %>% clean_names() %>% # use this fxn to return df, use make_clean_names on vector
      rename("labanimalid" = "rat") %>%
      select(cohort, labanimalid, rfid, matches("^(sha|pr|lga)")) %>%
      mutate_all(as.character) %>%
      mutate_at(vars(matches("^(sha|pr|lga)")), as.numeric) %>%
      gather(exp, rewards_excel, sha01:sha06_special) %>%
      mutate(exp = toupper(exp)),
    by = c("labanimalid", "cohort", "exp")
  ) 

##################
## SHA ########### 
##################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Cocaine/QC")

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



