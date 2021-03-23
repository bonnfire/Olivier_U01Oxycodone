## temporarily using this one: 


cohortfiles_xl_c01_07 <- list.files(path = "~/Dropbox (Palmer Lab)/Olivier_George_U01/GWAS Self Administration Data/Oxy Data", pattern = "*.xlsx") %>% grep("C0[1-7]", ., value = T)

# run this section after running all sections after cohort1
# 4 excels: 1,4 are both in the same format, 3 is similar but has comments in the names header, but 5 is in the format from the cocaine exp
olivieroxy_excel_rewards <- list("C01"=selfadmin_rewards_cohort1,
                         "C03"=selfadmin_rewards_cohort3,
                         "C04"=selfadmin_rewards_cohort4,
                         "C05"=selfadmin_rewards_cohort5,
                         "C06"=selfadmin_rewards_cohort6,
                         "C07"=selfadmin_rewards_cohort7) %>% rbindlist(idcol = "cohort", fill = T) %>% 
  clean_names() %>% # use this fxn to return df, use make_clean_names on vector 
  rename("labanimalid" = "rat") %>% 
  mutate_all(as.character)

olivieroxy_excel_dateslong <- olivieroxy_excel_rewards %>% select(matches("date|cohort")) %>% distinct() %>% # for record keeping, make sure to make this change on the actual excel! 
  gather(v, value, date_sha01:date_sha06_special) %>% separate(v, c("date", "exp"), sep = "_", extra = "merge") %>% 
  arrange(cohort) %>% 
  select(-date) %>% 
  mutate(exp = toupper(exp),
         value = as.character(lubridate::ymd(value))) %>% 
  rename("excel_date" = "value") %>% 
  subset(!is.na(excel_date)) %>% 
  mutate(excel_date = replace(excel_date, cohort == "C03" & exp == "LGA04", "2019-02-02"))## Based on Brent's comment on Slack 3/17/2020 "But for LGA04, the date seems to be incorrect on the data sheet. The date the experiment ended should be 2/2/2019, not 2/3/2019. " 

# create object for all values
olivieroxy_excel_all <- list(
  "C01"=selfadmin_xl_cohort1,
  "C03"=selfadmin_xl_cohort3,
  "C04"=selfadmin_xl_cohort4,
  "C05"=selfadmin_xl_cohort5,
  "C06"=selfadmin_xl_cohort6,
  "C07"=selfadmin_xl_cohort7
) %>% rbindlist(idcol = "cohort", fill = T) %>%
  select_if(~sum(!is.na(.)) > 0) %>% 
  clean_names() %>% 
  rename("labanimalid" = "rat") %>% 
  mutate(measurement = gsub("^ACTIVE.*", "active", measurement),
         measurement = gsub("^INACTIVE.*", "inactive", measurement),
         measurement = gsub("^PR.*", "pr_breakpoint", measurement),
         measurement = gsub("^REWARDS.*", "rewards", measurement)) 


# run this line after running all sections after cohort1

## to extract the mapping excels 
cohortinfofiles <- list.files(path = "~/Dropbox (Palmer Lab)/Olivier_George_U01/Rat Information/Oxycodone", pattern = "*.xlsx", full.names = T)



u01.importxlsx <- function(xlname){
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlnameF)
  names(df) <- excel_sheets(xlname)
  return(df)
}

########################
# COHORT 1 
########################
setwd("~/Dropbox (Palmer Lab)/Olivier_George_U01/GWAS Self Administration Data/Oxy Data")
filename <- cohortfiles_xl_c01_07[1]
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA
# set correct column names 
# create date columns
dates <- grep("^\\d+", names(selfadmin), value = T) # use these columns to make date columns # ignore the ...\\d columns
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates

setnames(selfadmin, toupper(as.character(selfadmin[1,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  sub("(PR\\d+)(TRTMEN)?(T)[0]?([1-9])", paste0("\\1_\\3", str_pad("\\4", 3, "left", "0")), names(selfadmin)) )
selfadmin <- selfadmin[-1,]
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
# selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table


nm <- names(selfadmin)[-c(1:2)] %>% sub("(PR\\d+)(TRTMEN)?(T)[0]?([1-9])", paste0("\\1_\\3", str_pad("\\4", 3, "left", "0")), .) # make date columns for this vector of exp names 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

comments_df <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))] #extact comments
comments_df <- comments_df %>% select(-matches("RFID|date")) %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column()
setnames(comments_df, append("EXP", comments_df[1, 2:4] %>% t() %>% unlist() %>% as.character) %>% tolower)
comments_df <- comments_df[-1,]
# selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^\\d", RFID))})
selfadmin_df <- selfadmin_split %>% rbindlist(idcol = "measurement") %>% dplyr::filter(measurement != "COMMENT")
selfadmin_rewards_cohort1 <- selfadmin_df %>% dplyr::filter(measurement == "ACTIVE")
selfadmin_xl_cohort1 <- selfadmin_df %>% ## Lani notes on 03/12 "Rewards data for this cohort is under Active; Inactive data is inactive"
  mutate(measurement = replace(measurement, measurement == "ACTIVE", "REWARDS"))

########################
# COHORT 3
########################  

# this file doesn't have rfid's so will need to extract from the replacement table
filename <- cohortfiles_xl_c01_07[2]
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA

# EXTRACT COMMENTS bc comments are stuck as header
comments_df <- selfadmin[1:2,] %>% t() %>% as.data.frame() 
setDT(comments_df, keep.rownames = TRUE)[]
setnames(comments_df, comments_df[1,] %>% t() %>% unlist() %>% as.character %>% tolower)
comments_df <- comments_df[-1, ]
comments_df$comments <- comments_df$comments %>% gsub("[.]{3}\\d", NA,. ) 

# set correct column names 
# create date columns
dates <- grep("^\\d+", selfadmin[3,] %>% unlist() %>% as.character(), value = T) # use these columns to make date columns # ignore anything else
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates
setnames(selfadmin, toupper(as.character(selfadmin[4,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  mgsub::mgsub(names(selfadmin), c("LGA15.*", "^PRT.+1$", "^PRT.+2$", "^PRT.+3$", "^PRT.+4$"), c("LGA15", "PR03_T01", "PR04_T02", "PR05_T03", "PR06_T04")))
names(selfadmin)[1] <- "RAT"
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
# selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table
# selfadmin <- selfadmin[-1,]

nm <- names(selfadmin)[-1] # make date columns for this vector of exp names  ## MISSING THE RFID COLUMN SO THAT IS WHY [-1] INSTEAD OF [-c(1:2)]
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR$", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^[MF]\\d+", RAT))})
selfadmin_df <- selfadmin_split %>% rbindlist(idcol = "measurement")
selfadmin_rewards_cohort3 <- selfadmin_df %>% dplyr::filter(measurement == "REWARDS")
selfadmin_xl_cohort3 <- selfadmin_df

# EXTRACT RFID
filename <- cohortinfofiles[2]
cohortinfo <- u01.importxlsx(filename)[[1]] %>% as.data.frame()
names(cohortinfo) <-  mgsub::mgsub(names(cohortinfo),
                                   c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date","Animal", "Shipping|Ship", "Dams"),
                                   c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames")) 
# join rfid onto the rewards
selfadmin_rewards_cohort3 <- selfadmin_rewards_cohort3 %>% 
  left_join(cohortinfo[, c("RAT", "RFID")], ., by = c("RAT"))

selfadmin_xl_cohort3 <- selfadmin_xl_cohort3 %>% 
  left_join(cohortinfo[, c("RAT", "RFID")], ., by = c("RAT")) 

  
########################
# COHORT 4
########################
# this file doesn't have rfid's so will need to extract from the replacement table

filename <- cohortfiles_xl_c01_07[3]
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA

# set correct column names 
# create date columns
dates <- grep("^\\d+", names(selfadmin), value = T) # use these columns to make date columns # ignore the ...\\d columns
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates

setnames(selfadmin, toupper(as.character(selfadmin[1,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  mgsub::mgsub(names(selfadmin), c("PR2", "^T.+1$", "^T.+2$", "^T.+3$", "^T.+4$", "LGA([1-9]{1})$"), c("PR02", "PR03_T01", "PR04_T02", "PR05_T03", "PR06_T04", "LGA0\\1")))
names(selfadmin)[1] <- "RAT"
selfadmin <- selfadmin[-1,]
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
# selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table

nm <- names(selfadmin)[-1] # make date columns for this vector of exp names  ## MISSING THE RFID COLUMN SO THAT IS WHY [-1] INSTEAD OF [-c(1:2)] 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

#extract comments
comments_df <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]
comments_df <- comments_df %>% select(-matches("RFID|date")) %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column()
setnames(comments_df, append("EXP", comments_df[1, 2:4] %>% t() %>% unlist() %>% as.character) %>% tolower)
comments_df <- comments_df[-1,]
# selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR$", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^[MF]\\d+", RAT))})
selfadmin_df <- selfadmin_split %>% rbindlist(idcol = "measurement") %>% dplyr::filter(measurement != "COMMENT")
selfadmin_rewards_cohort4 <- selfadmin_df %>% dplyr::filter(measurement == "REWARDS")
selfadmin_xl_cohort4 <- selfadmin_df



# EXTRACT RFID
filename <- cohortinfofiles[3]
cohortinfo <- u01.importxlsx(filename)[[1]] %>% as.data.frame()
names(cohortinfo) <-  mgsub::mgsub(names(cohortinfo),
                                   c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date","Animal", "Shipping|Ship", "Dams"),
                                   c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames")) 
# join rfid onto the rewards
selfadmin_rewards_cohort4 <- selfadmin_rewards_cohort4 %>% 
  left_join(cohortinfo[, c("RAT", "RFID")], ., by = c("RAT"))

selfadmin_xl_cohort4 <- selfadmin_xl_cohort4 %>% 
  left_join(cohortinfo[, c("RAT", "RFID")], ., by = c("RAT"))

########################
# COHORT 5
########################
# filename <- cohortfiles_xl_c01_07[4]

filename <- "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/Olivier_George_U01DA044451/excel_and_csv_files/OXY GWAS C05 Data.xlsx" # XX temp for Lani 
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  # u01.importxlsx(filename) %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA

# set correct column names 
# create date columns
dates <- grep("^\\d+", names(selfadmin), value = T) # use these columns to make date columns # ignore the ...\\d columns
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates

setnames(selfadmin, toupper(as.character(selfadmin[1,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  mgsub::mgsub(names(selfadmin), c("PR1", "PR2", "^T.+1$", "^T.+2$", "^T.+3$", "^T.+4$", "SHA.*?([0-9]{2})$", "LGA.*?([0-9]{2})$"), c("PR01", "PR02", "PR03_T01", "PR04_T02", "PR05_T03", "PR06_T04", "SHA\\1", "LGA\\1")))
names(selfadmin)[1:2] <- c("RAT", "RFID")
selfadmin <- selfadmin[-1,]
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
# selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table

nm <- names(selfadmin)[-c(1:2)] # make date columns for this vector of exp names  ## MISSING THE RFID COLUMN SO THAT IS WHY [-1] INSTEAD OF [-c(1:2)] 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

#extract comments
comments_df <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]
comments_df <- comments_df %>% select(-matches("RFID|date")) %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column()
setnames(comments_df, append("EXP", comments_df[1, 2:3] %>% t() %>% unlist() %>% as.character) %>% tolower)
comments_df <- comments_df[-1,]
# selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR$", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^[MF]\\d+", RAT))})
selfadmin_df <- selfadmin_split %>% rbindlist(idcol = "measurement") %>% dplyr::filter(measurement != "COMMENT")
selfadmin_rewards_cohort5 <- selfadmin_df %>% dplyr::filter(measurement == "REWARDS")
selfadmin_xl_cohort5 <- selfadmin_df


selfadmin_rewards_cohort5 <- selfadmin %>% 
  mutate(measurement = "REWARDS")

selfadmin_xl_cohort5 <- selfadmin %>% mutate(measurement = "PR_BREAKPOINT")## XX troubleshoot, doesn't have the other measurements


########################
# COHORT 6
########################

filename <- cohortfiles_xl_c01_07[5]
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA

# set correct column names 
# create date columns
dates <- grep("^\\d+", names(selfadmin), value = T) # use these columns to make date columns # ignore the ...\\d columns
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates

setnames(selfadmin, toupper(as.character(selfadmin[1,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  mgsub::mgsub(names(selfadmin), c("PR1", "PR2", "^T.+1$", "^T.+2$", "^T.+3$", "^T.+4$", "SHA.*?([0-9]{2})$", "LGA.*?([0-9]{2})$"), c("PR01", "PR02", "PR03_T01", "PR04_T02", "PR05_T03", "PR06_T04", "SHA\\1", "LGA\\1")))
names(selfadmin)[1:2] <- c("RAT", "RFID")
selfadmin <- selfadmin[-1,]
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
# selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table

nm <- names(selfadmin)[-c(1:2)] # make date columns for this vector of exp names  ## MISSING THE RFID COLUMN SO THAT IS WHY [-1] INSTEAD OF [-c(1:2)] 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

#extract comments
comments_df <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]
comments_df <- comments_df %>% select(-matches("RFID|date")) %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column()
setnames(comments_df, append("EXP", comments_df[1, 2:3] %>% t() %>% unlist() %>% as.character) %>% tolower)
comments_df <- comments_df[-1,]
# selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR$", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^[MF]\\d+", RAT))})
selfadmin_df <- selfadmin_split %>% rbindlist(idcol = "measurement") %>% dplyr::filter(measurement != "COMMENT")
selfadmin_rewards_cohort6 <- selfadmin_df %>% dplyr::filter(measurement == "REWARDS")
selfadmin_xl_cohort6 <- selfadmin_df


########################
# COHORT 7
########################

filename <- cohortfiles_xl_c01_07[6]
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA

# set correct column names 
# create date columns
dates <- grep("^\\d+", names(selfadmin), value = T) # use these columns to make date columns # ignore the ...\\d columns
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates

setnames(selfadmin, toupper(as.character(selfadmin[1,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  mgsub::mgsub(names(selfadmin), c("PR1", "PR2", "^T.+1$", "^T.+2$", "^T.+3$", "^T.+4$", "SHA.*?([0-9]{2})$", "LGA.*?([0-9]{2})$"), c("PR01", "PR02", "PR03_T01", "PR04_T02", "PR05_T03", "PR06_T04", "SHA\\1", "LGA\\1")))
names(selfadmin)[1:2] <- c("RAT", "RFID")
selfadmin <- selfadmin[-1,]
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
# selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table

nm <- names(selfadmin)[-c(1:2)] # make date columns for this vector of exp names  ## MISSING THE RFID COLUMN SO THAT IS WHY [-1] INSTEAD OF [-c(1:2)] 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

#extract comments
comments_df <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]
comments_df <- comments_df %>% select(-matches("RFID|date")) %>% t() %>% 
  as.data.frame() %>% 
  rownames_to_column()
setnames(comments_df, append("EXP", comments_df[1, 2:3] %>% t() %>% unlist() %>% as.character) %>% tolower)
comments_df <- comments_df[-1,]
# selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR$", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^[MF]\\d+", RAT))})
selfadmin_df <- selfadmin_split %>% rbindlist(idcol = "measurement") %>% dplyr::filter(measurement != "COMMENT")
selfadmin_rewards_cohort7 <- selfadmin_df %>% dplyr::filter(measurement == "REWARDS")
selfadmin_xl_cohort7 <- selfadmin_df








##################################################


### EXTRACT THE COMPUTER NOTES FROM THEIR DROPBOX
setwd("~/Dropbox (Palmer Lab)/GWAS (1)")
computernotes_oxy <- u01.importxlsx("computer notes.xlsx")[[2]] %>% 
  gather(exp, computernote, SHA01:cohort_notes) %>% 
  clean_names() %>% 
  dplyr::filter(grepl("^C", cohort)) %>% 
  naniar::replace_with_na(replace = list(computernote = "NA"))

### EXTRACT THE MAPPING FILES FROM THEIR DROPBOX
setwd("~/Dropbox (Palmer Lab)/Olivier_George_U01/Rat Information/Oxycodone")
rat_info_xl_filenames <- list.files(pattern = "*.xlsx")

rat_info_allcohort_xl_df <- lapply(rat_info_xl_filenames, function(x){
  path_sheetnames <- excel_sheets(x)
  df <- lapply(excel_sheets(path = x), read_excel, path = x) # including this, just in case the info file ever moves out of order
  names(df) <- path_sheetnames
  info_name <- grep("info|timeline", path_sheetnames, ignore.case = T, value = T) # allows for small changes, like info sheet vs info sheets
  df_info <- df[[info_name]]
  
  df_info <- df_info %>% mutate_all(as.character) # prevent any mismatched class attributes
  df_info <- df_info %>%
    mutate(naive = NA, 
           naive = replace(naive, is.na(RAT), "Empty"),
           naive = replace(naive, grepl("Naive", RAT, ignore.case=F), "Naive")) %>% 
    tidyr::fill(naive) %>% 
    mutate(naive = replace(naive, !grepl("Naive", naive), NA)) %>% 
    subset(grepl("^\\d", RFID)) # tackle the naive cases
  
  return(df_info)
})
names(rat_info_allcohort_xl_df) <- rat_info_xl_filenames
rat_info_allcohort_xl_df %<>% rbindlist(fill = T, idcol = "cohort") %<>% 
  mutate(cohort = str_extract(cohort, "C\\d{2}")) %<>%
  clean_names()


###### EXTRACT THE VON FREY AND TAIL IMMERSION DATA
setwd("~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone")

## Tail Immersion 
tail_immersion <- u01.importxlsx("GWAS Oxy Tail Immersion Data.xlsx")
tail_immersion_df <- lapply(tail_immersion, function(x){ # clean up the tables, so that the column names are uniform before rbindlist
  # give the right names
  x <- x %>% mutate(row = 1:n()) # add row number column 
  datastarts <- x$row[grep("rat id", as.character(unlist(x[,1])), ignore.case = T)] 
  names(x) <- x[datastarts, ]
  x <- x[-c(1:datastarts), ]
  x <- x %>% 
    clean_names() %>% 
    select_if(~sum(!is.na(.)) > 0) %>% 
    select(-matches("^x\\d+$")) %>% 
    mutate(row = 1:n()) 
  
  # label naives
  naivestarts_f <- x$row[grep("naive", as.character(unlist(x$rat_id)), ignore.case = T)]
  naivestarts_m <- x$row[grep("naive", as.character(unlist(x$rat_id_2)), ignore.case = T)]

  x <- x %>%
    mutate(naive = ifelse(row > naivestarts_f, "yes", "no"),
           naive_2 = ifelse(row > naivestarts_m, "yes", "no")) # one column for females, one column for males

  # separate the males and females, join
  males <- x %>% select(ends_with("_2"))
  names(males) = gsub("_2", "", names(males))

  x <- x %>% select(-ends_with("_2")) %>%
    bind_rows(males) %>% # join the males
    select(-row) %>% 
    mutate_at(vars(matches("bsl|oxy_on_board|x12h_wd_w_oxy")), as.numeric) %>%
    rename_if(is.numeric, list(~paste0(., "_s"))) %>%
    mutate_all(as.character) %>% 
    naniar::replace_with_na_all(condition = ~.x %in% c("NA", "N/A", "na"))
  
  
  return(x)
  
  
}) %>% rbindlist(fill = T, idcol = "cohort") %>% 
  mutate(cohort = paste0("C", str_pad(parse_number(cohort), 2, side = "left", pad = "0"))) %>% 
  mutate(comments = NA) %>% 
  mutate(comments = replace(comments, cohort == "C04", "animals did not receive oxy for tolerance timepoint before"),
         comments = replace(comments, cohort == "C07", "animals tested at the 12hr withdrawal timepoint had 7 days of LgA oxy SA after a 3 week break from oxy SA. Also, experimenter changed at the 12hr withdrawal timepoint")) %>% 
  mutate_at(vars(ends_with("_s")), as.numeric) %>% 
  rename("labanimalid" = "rat_id") 

tail_immersion_df <- tail_immersion_df %>% subset(grepl("[MF]\\d+", labanimalid)) 
# basic qc 
tail_immersion_df %>% get_dupes(labanimalid)
# join to the rfid
tail_immersion_df <- tail_immersion_df %>% left_join(rat_info_allcohort_xl_df %>% 
                                  select(rat, rfid), 
                                by = c("labanimalid" = "rat"))

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE")
# write.csv(tail_immersion_df, file = "oxycodone.xlsx", row.names = F)

## simple plot for the qc meeting w giordano 02/01/2021 
tail_immersion_df %>% select(cohort, labanimalid, oxy_on_board_s, x12h_wd_w_oxy_s) %>% mutate(sex = str_match(labanimalid, "[MF]")) %>% gather("session", "measure", -cohort, -labanimalid, -sex) %>% ggplot(aes(x = session, y = measure, fill = cohort)) + geom_boxplot() + facet_grid(~sex) 
  

## Von Frey
setwd("~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone")
von_frey <- u01.importxlsx("GWAS Oxy Von Frey Data.xlsx")[[1]]

von_frey_df <- von_frey %>% mutate_all(as.character)
names_von_frey_df <- von_frey[1:2,] %>% t() %>% as.data.frame() %>% 
  rename("qualifier2" = "V1", 
         "varname" = "V2") %>% 
  cbind(names(von_frey) %>% as.data.frame() %>% rename("qualifier1" = ".")) %>% 
  mutate(qualifier1 = gsub("[.][.][.]\\d", NA, qualifier1)) %>% 
  fill(qualifier1) %>%
  mutate(qualifier1 = replace(qualifier1, grepl("Pain", varname, ignore.case = T), NA)) %>% 
  mutate(qualifier = paste0(qualifier1, "_", qualifier2)) %>% 
  mutate(qualifier = gsub("NA", NA, qualifier)) %>% 
  fill(qualifier) %>% 
  mutate(qualifier = replace(qualifier, grepl("Pain", varname, ignore.case = T), NA)) %>% 
  mutate(varname = paste0(varname, "_", qualifier)) %>% 
  mutate(varname = gsub("_NA", "", varname))


names(von_frey_df) <- names_von_frey_df$varname
von_frey_df <- von_frey_df[-c(1:2), ]
von_frey_df <- von_frey_df %>% clean_names() %>% 
  rename("labanimalid" = "rat") %>% 
  mutate_at(vars(-matches("labanimalid|cohort|sex|group")), as.numeric) %>% 
  mutate(cohort = paste0("C", str_pad(cohort, 2, "left", "0")))


von_frey_df <- von_frey_df %>% subset(grepl("[MF]\\d+", labanimalid)) # found 0 cases
# basic qc 
von_frey_df %>% get_dupes(labanimalid) # found 0 cases
# join to the rfid
von_frey_df_rfid <- von_frey_df %>% left_join(rat_info_allcohort_xl_df %>% 
                                           select(rat, rfid), 
                                         by = c("labanimalid" = "rat")) %>% # 14 that are not in this mapping file
  left_join(compromised_rats[, c("labanimalid", "rfid")], by = c("labanimalid")) %>% # find the 
  mutate(rfid = coalesce(rfid.x, rfid.y)) %>% 
  select(-c("rfid.x", "rfid.y")) # 6 left without rfid's, missing from mapping files (no naive data from cohort 4)



von_frey_df %>% select(cohort, labanimalid, pain_force_per_rat_bsl, pain_force_per_rat_wd) %>% mutate(sex = str_match(labanimalid, "[MF]")) %>% gather("session", "measure", -cohort, -labanimalid, -sex) %>% ggplot(aes(x = session, y = measure, fill = cohort)) + geom_boxplot() + facet_grid(~sex) 











setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE")
C04_naive_wfu <- read_xlsx("c04 naive rat info.xlsx") %>%  # got the information from olivier lab (sent them von_frey_df %>% subset(is.na(rfid)) %>% select(labanimalid) %>% unlist() %>% paste0(collapse = ", ")), *MASTER TABLES FOR WFU COHORT 4 OXY NAIVE (COMES FROM OXY AND COCAINE SCRUBS)
  clean_names %>% 
  rename("labanimalid" = "georgeid") %>% 
  mutate(labanimalid = toupper(labanimalid)) 
  

von_frey_df_rfid <- von_frey_df_rfid %>% 
  left_join(C04_naive_wfu[, c("labanimalid", "rfid")], by = "labanimalid")  %>% 
  mutate(rfid = coalesce(rfid.x, rfid.y)) %>% 
  select(-c("rfid.x", "rfid.y"))



## XX pick up from here 08/17/2020
  x <- x %>% mutate(row = 1:n()) # add row number column 
  datastarts <- x$row[grep("rat id", as.character(unlist(x[,1])), ignore.case = T)] 
  names(x) <- x[datastarts, ]
  x <- x[-c(1:datastarts), ]
  x <- x %>% 
    clean_names() %>% 
    select_if(~sum(!is.na(.)) > 0) %>% 
    select(-matches("^x\\d+$")) %>% 
    mutate(row = 1:n()) 
  
  # label naives
  naivestarts_f <- x$row[grep("naive", as.character(unlist(x$rat_id)), ignore.case = T)]
  naivestarts_m <- x$row[grep("naive", as.character(unlist(x$rat_id_2)), ignore.case = T)]
  
  x <- x %>%
    mutate(naive = ifelse(row > naivestarts_f, "yes", "no"),
           naive_2 = ifelse(row > naivestarts_m, "yes", "no")) # one column for females, one column for males
  
  # separate the males and females, join
  males <- x %>% select(ends_with("_2"))
  names(males) = gsub("_2", "", names(males))
  
  x <- x %>% select(-ends_with("_2")) %>%
    bind_rows(males) %>% # join the males
    select(-row) %>% 
    mutate_at(vars(matches("bsl|oxy_on_board|x12h_wd_w_oxy")), as.numeric) %>%
    rename_if(is.numeric, list(~paste0(., "_s"))) %>%
    mutate_all(as.character) %>% 
    naniar::replace_with_na_all(condition = ~.x %in% c("NA", "N/A", "na"))
  
  
  return(x)
  
  
}) %>% rbindlist(fill = T, idcol = "cohort")






