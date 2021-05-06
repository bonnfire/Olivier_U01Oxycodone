## create csv files

## (temp) for Olivier's NIDA GWAS graphs

######
## SHA  
######


oxy_xl_sha_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, rewards_raw, death_comment, rewards_QC) %>% 
  rename("rewards" = "rewards_raw") %>% 
  mutate(rewards = replace(rewards, rewards_QC != "pass", NA)) %>% 
  select(-rewards_QC) %>% 
  spread(exp, rewards) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/sha_gwas_rewards_oxy.csv", row.names = F)

oxy_xl_sha_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, active_raw, death_comment, active_QC) %>% 
  rename("active" = "active_raw") %>% 
  mutate(active = replace(active, active_QC != "pass", NA)) %>% 
  select(-active_QC) %>% 
  spread(exp, active) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/sha_gwas_active_oxy.csv", row.names = F)
  
oxy_xl_sha_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, inactive_raw, death_comment, inactive_QC) %>% 
  rename("inactive" = "inactive_raw") %>% 
  mutate(inactive = replace(inactive, inactive_QC != "pass", NA)) %>% 
  select(-inactive_QC) %>% 
  spread(exp, inactive) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/sha_gwas_inactive_oxy.csv", row.names = F)

oxy_xl_sha_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, rewards_raw, death_comment, rewards_QC) %>% 
  rename("rewards" = "rewards_raw") %>% 
  mutate(rewards = replace(rewards, rewards_QC != "pass", NA)) %>% 
  select(-rewards_QC) %>% 
  spread(exp, rewards) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  select(-matches("sha0[3-4]")) %>% 
  mutate(mean_sha_last2 = rowMeans(select(., starts_with("sha")), na.rm = T)) %>% 
  select(-matches("^sha")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/sha_gwas_mean_sha_last2_oxy.csv", row.names = F)

######
## LGA
######

oxy_xl_lga_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, rewards_raw, death_comment, rewards_QC) %>% 
  rename("rewards" = "rewards_raw") %>% 
  subset(rewards_QC == "pass") %>% 
  # mutate(rewards = replace(rewards, rewards_QC != "pass", NA)) %>% 
  select(-rewards_QC) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  distinct() %>% 
  spread(exp, rewards) %>% 
  rename_at(vars(matches("^lga")), ~paste0(., "_rewards")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/lga_gwas_rewards_oxy.csv", row.names = F)

oxy_xl_lga_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, active_raw, death_comment, active_QC) %>% 
  rename("active" = "active_raw") %>% 
  subset(active_QC == "pass") %>% 
  # mutate(active = replace(active, active_QC != "pass", NA)) %>% 
  select(-active_QC) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  distinct() %>% 
  spread(exp, active) %>% 
  rename_at(vars(matches("^lga")), ~paste0(., "_active")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/lga_gwas_active_oxy.csv", row.names = F)

oxy_xl_lga_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, inactive_raw, death_comment, inactive_QC) %>% 
  rename("inactive" = "inactive_raw") %>% 
  subset(inactive_QC == "pass") %>% 
  # mutate(inactive = replace(inactive, inactive_QC != "pass", NA)) %>% 
  select(-inactive_QC) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  distinct() %>% 
  spread(exp, inactive) %>% 
  rename_at(vars(matches("^lga")), ~paste0(., "_inactive")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/lga_gwas_inactive_oxy.csv", row.names = F)

oxy_xl_lga_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, rewards_raw, death_comment, rewards_QC) %>% 
  rename("rewards" = "rewards_raw") %>% 
  subset(rewards_QC == "pass") %>% 
  # mutate(rewards = replace(rewards, rewards_QC != "pass", NA)) %>% 
  select(-rewards_QC) %>% 
  subset(!(is.na(room))) %>% 
  subset(!is.na(rfid)) %>% 
  subset(!(rfid == "933000320047024"|rfid=="933000320046835")) %>% 
  distinct() %>% 
  spread(exp, rewards) %>% 
  mutate_at(vars(matches("lga1[1-4]$")), list(esc = ~.-lga01)) %>% 
  mutate(esc11_14_mean = rowMeans(select(., ends_with("_esc")), na.rm = TRUE)) %>% 
  select(-matches("^lga")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/lga_gwas_mean_lga_esc_11_14_oxy.csv", row.names = F)


######
## PR
######

oxy_xl_pr_df_qc %>% 
select(cohort, labanimalid, rfid, exp, room, box, rewards_raw, death_comment, rewards_QC) %>% 
  rename("rewards" = "rewards_raw") %>% 
  subset(rewards_QC == "pass") %>% 
  spread(exp, rewards) %>% 
  # mutate(rewards = replace(rewards, rewards_QC != "pass", NA)) %>% 
  select(-rewards_QC) %>% 
  rename_at(vars(matches("^pr")), ~paste0(., "_rewards")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/pr_gwas_rewards_oxy.csv", row.names = F)

oxy_xl_pr_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, active_raw, death_comment, active_QC) %>% 
  rename("active" = "active_raw") %>% 
  subset(active_QC == "pass") %>% 
  spread(exp, active) %>% 
  # mutate(active = replace(active, active_QC != "pass", NA)) %>% 
  select(-active_QC) %>% 
  rename_at(vars(matches("^pr")), ~paste0(., "_active")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/pr_gwas_active_oxy.csv", row.names = F)

oxy_xl_pr_df_qc %>% 
  select(cohort, labanimalid, rfid, exp, room, box, inactive_raw, death_comment, inactive_QC) %>% 
  rename("inactive" = "inactive_raw") %>% 
  subset(inactive_QC == "pass") %>% 
  spread(exp, inactive) %>% 
  # mutate(inactive = replace(inactive, inactive_QC != "pass", NA)) %>% 
  select(-inactive_QC) %>% 
  rename_at(vars(matches("^pr")), ~paste0(., "_inactive")) %>% 
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/pr_gwas_inactive_oxy.csv", row.names = F)


pr_gwas_c01_07 %>%  
  write.csv("~/Desktop/Database/csv files/u01_olivier_george_oxycodone/pr_gwas_tier2_oxy_c01_07.csv", row.names = F)



