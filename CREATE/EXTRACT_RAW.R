## extract raw 
setwd("~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone/Oxycodone GWAS")
# after cohort 3, there are only new files


## USEFUL FUNCTIONS

# FOR ~NEW~ DIRECTORIES
# #extract names to be assigned for various tables later
process_subjects_new <- function(x){
  
  read_subjects_new <- function(x){
    subjects <- fread(paste0("awk '/Subject/{print NR \"_\" $2}' ", " '", x, "'"),fill = T,header=F)
    subjects$filename <- x
    return(subjects)
  }

  read_meta_subjects_new <- function(x){
    date_time <- fread(paste0("awk '/Start/{print $3}' ", "'", x, "'", " | sed 'N;s/\\r\\n/_/g'"),fill = T,header=F)
    return(date_time)
  }
  date_time <- lapply(x, read_meta_subjects_new) %>% rbindlist() 
  
  names_sha_append <- lapply(x, read_subjects_new) %>% rbindlist() %>% rename("labanimalid"="V1") %>%
    cbind(., date_time) %>% 
    mutate(labanimalid = paste0( str_extract(labanimalid, "\\d+"), "_", 
                                str_extract(toupper(labanimalid), "[MF]\\d{1,3}"), "_", # labanimalid
                                str_extract(filename, "C\\d+"), "_", # cohort
                                sub('.*HSOXY', '', toupper(filename)), "_", # exp
                                sub(".*/.*/.*/", '', filename), "_",
                                V1)) %>% # subject id, cohort, experiment, file/location perhaps
  select(-V1)
  
  return(names_sha_append)
  
}


process_subjects_old <- function(x){
  
  read_subjects_old <- function(x){
    subject_old <- fread(paste0("grep -inEA1 --no-group-separator \"ratnumber|boxnumber\" ", "'", x, "'", "| grep -vE \"Rat|Box\""), header = F)
    subject_old$filename <- x 
    return(subject_old)
  }
  
  subjects_old <- lapply(x, read_subjects_old) %>% rbindlist() %>% 
    separate(V1, into = c("row", "value"), sep = "-", remove = T) %>% 
    mutate(box_id = ifelse((row_number() %% 2) == 0, "labanimalid", "box"))
  
  box <- subjects_old %>% dplyr::filter(box_id == "box") %>% select(value) %>% unlist() %>% as.character()
  labanimalid <- subjects_old %>% dplyr::filter(box_id == "labanimalid") %>% select(value) %>% unlist() %>% as.character()
  filename <- subjects_old %>% dplyr::filter(box_id == "box") %>% select(filename) %>% unlist() %>% as.character()
  row <- subjects_old %>% dplyr::filter(box_id == "box") %>% select(row) %>% unlist() %>% as.numeric()
  
  box_id_bind <- data.frame(box = box, 
                            labanimalid = labanimalid, 
                            filename = filename,
                            row = row) %>% 
    # mutate_all(as.character) %>% 
    mutate(labanimalid = replace(labanimalid, as.character(labanimalid)=="999", "F000")) %>% # create placeholder for the problematic cases
    # mutate(date = str_extract(filename, "\\d{8}") %>% lubridate::ymd(),
    #        cohort = str_extract(filename, "C\\d+"), 
    #        # cohort = gsub(".*C[0]?(\\d)+/.*", "cohort\\1", filename),
    #        exp = sub("-.*", "", sub(".*HSOXY([^.]+)[-].*", "\\1", toupper(filename)))) %>% 
    # merge(., cohorts_exp_date, by = c("cohort", "exp")) %>% 
    # mutate(valid = ifelse(date == excel_date, "valid", "invalid")) %>%
    mutate(labanimalid = paste0(str_match(toupper(labanimalid), "[FM]\\d{1,3}"), "_",
                                box, "_",
                                str_extract(filename, "C\\d+"), "_",
                                sub("-.*", "", sub(".*HSOXY([^.]+)[-].*", "\\1", toupper(filename))), "_",
                                sub("C.*", "", sub(".*/.*/.*/.*/", "", filename)), "_",
                                str_extract(filename, "\\d{8}"))) %>%  # subject id, box, cohort, experiment, computer, date
    select(one_of("labanimalid", "filename", "row"))
  # 
  # %>% dplyr::filter(!grepl("^NA", labanimalid))
  
  return(box_id_bind)
}


read_fread_old <- function(x, varname){
  
  fread_old_statements <- data.frame(varname = c("leftresponses", "rightresponses", "rewards"),
                                 statement = c("awk '/^BinsInActiveResponses/{flag=1;next}/endl/{flag=0}flag' ",
                                               "awk '/^ResponsesActBins/{flag=1;next}/endl/{flag=0}flag' ",
                                               "awk '/totalRewards/{flag=1;next}/TotalResponses/{flag=0}flag' ")) 
                                               # "awk '/BinRewards/{flag=1;next}/endl/{flag=0}flag' "))  #### 	 In=L Act=R  Rew=W InTS=U	ActTS=Y  RewTS=V  RewIRI=Z 	
  statement <- fread_old_statements[which(fread_old_statements$varname == varname),]$statement
  rawdata <- fread(paste0(statement, "'", x, "' | nl -s _ | sed \"s/[[:blank:]]//g\""), fill = T, header = F)
  rawdata$filename <- x
  
  return(rawdata)
}


############# (consider adding this code form cocaine to extract all three -- active, inactive, and rewards)
read_fread_new <- function(x, varname){
  
  fread_statements <- data.frame(varname = c("leftresponses", "rightresponses", "rewards", "lefttimestamps", "righttimestamps", "rewardstimestamps"),
                                 statement = c("awk '/L:/{flag=1;next}/R:/{flag=0}flag' ",
                                               "awk '/R:/{flag=1;next}/U:/{flag=0}flag' ",
                                               "awk '/W:/{flag=1;next}/Y:/{flag=0}flag' ", 
                                               "awk '/U:/{flag=1;next}/V:/{flag=0}flag' ",
                                               "awk '/Y:/{flag=1;next}/^$/{flag=0}flag' ",
                                               "awk '/V:/{flag=1;next}/W:/{flag=0}flag' "))
  statement <- fread_statements[which(fread_statements$varname == varname),]$statement
  rawdata <- fread(paste0(statement, "'", x, "'"), fill = T)
  data_indices <- grep("^0:$", rawdata$V1)
  split_data <- split(rawdata, cumsum(1:nrow(rawdata) %in% data_indices))
  
  keepzeroes <- c("leftresponses", "rightresponses", "rewards") # preserve bin sequences
  
  if(varname %in% keepzeroes){
    processeddata <- lapply(split_data, function(x){
      indexremoved <- x[,-1]
      processeddata_df <- data.frame(counts = as.vector(t(data.matrix(indexremoved)))) %>% # transpose to get by row
        mutate(bin = ifelse(row_number() == 1, "total", as.character(row_number() - 1)))
      return(processeddata_df)
    })
  }
  else{
    processeddata <- lapply(split_data, function(x){
      indexremoved <- x[,-1]
      nonzerorows <- indexremoved[rowSums(indexremoved) > 0, ] # remove excessively trailing 0's 
      processeddata_df <- data.frame(timestamps = as.vector(t(data.matrix(nonzerorows)))) # transpose to get by row
      if(any(processeddata_df$timestamps > 7500)){
        processeddata_df %<>% 
          mutate(bin = cut(timestamps, breaks=seq(from = 0, length.out = 73, by = 300), right = T, labels = seq(from = 1, to = 72, by =1))) %<>% 
          dplyr::filter(timestamps != 0)
      }
      else{
        processeddata_df %<>% 
          mutate(bin = cut(timestamps, breaks=seq(from = 0, length.out = 25, by = 300), right = T, labels = seq(from = 1, to = 24, by =1))) %<>% 
          dplyr::filter(timestamps != 0)
      }
      return(processeddata_df)
    }) 
  }
  
  
  return(processeddata)
}














## TO VALIDATE ENTRIES  (seems to only work for new directories)
## extract date and start time/end time to determine valid sessions
read_date_time_subject <- system("grep -a7r --no-group-separator \"Start Date: \" . | grep -E \"((Start|End) (Date|Time)|Subject|Box):\"", intern = T)
read_date_time_subject <- gsub("\\r", "", read_date_time_subject)

date_time_subject <- data.frame(labanimalid = gsub(".*Subject: ", "", grep("Subject", read_date_time_subject, value = T)) %>% toupper,
                                cohort = str_match(grep("Subject", read_date_time_subject, value = T), "C\\d{2}") %>% unlist() %>% as.character(),  
                                exp = toupper(sub('.*HSOXY', '', grep("Subject", read_date_time_subject, value = T) %>% gsub("-Subject.*", "", .))),
                                start_date = gsub(".*Start Date: ", "", grep("Start Date:", read_date_time_subject, value = T)),
                                end_date = gsub(".*End Date: ", "", grep("End Date:", read_date_time_subject, value = T)),
                                box = gsub(".*Box: ", "", grep("Box", read_date_time_subject, value = T)),
                                start_time = gsub(".*Start Time: ", "", grep("Start Time", read_date_time_subject, value = T)),
                                end_time = gsub(".*End Time: ", "", grep("End Time", read_date_time_subject, value = T)),
                                filename = sub(".*/.*/.*/", '', grep("Subject", read_date_time_subject, value = T)) %>% gsub("-Subject.*", "", .),
                                directory = str_match(grep("Subject", read_date_time_subject, value = T) %>% gsub("-Subject.*", "", .), "New_medassociates|Old") %>% unlist() %>% as.character()
) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(exp = mgsub::mgsub(exp, c("PR([1-9]{1})$", paste0("TREATMENT", 1:4)), c("PR0\\1", paste0("PR0", 3:6, "_T0", 1:4)))) %>% 
  mutate(start_datetime = lubridate::mdy_hms(paste(format(as.Date(start_date, "%m/%d/%y"), "%m/%d/20%y"), start_time)),
         end_datetime = lubridate::mdy_hms(paste(format(as.Date(end_date, "%m/%d/%y"), "%m/%d/20%y"), end_time)),
         experiment_duration = difftime(end_datetime, start_datetime, units = "mins") %>% as.numeric() %>% round(0)) %>% 
  select(-matches("_(date|time){1}$"))
         
## problems in being too lax in accepting all forms of subjects 
# gsub(".*Subject: ", "", grep("Subject", read_date_time_subject, value = T)) %>% toupper %>% table() # before processing

# fix the missexed animals and assignment
# mixed animals 
dupeids <- date_time_subject %>% subset(grepl("^[MF]", labanimalid)) %>% 
  distinct(labanimalid) %>% select(labanimalid) %>% mutate(numbers = gsub("[^\\d]+", "", labanimalid, perl = T)) %>% 
  get_dupes(numbers)
# correct assignment of the reference id's
date_time_subject_mut <- date_time_subject %>% subset(labanimalid%in% dupeids$labanimalid) %>% add_count(labanimalid) %>% # find trouble cases, use the frequency of occurence to determine which one is wrong, switch gender, and reattach to data
  mutate(labanimalid = ifelse(n < 5 & grepl("^F", labanimalid), gsub("F", "M", labanimalid),
                              ifelse(n < 5 & grepl("^M", labanimalid), 
                                     gsub("M", "F", labanimalid), labanimalid))) %>% 
  select(-n) %>% as.data.frame() %>% 
  left_join(date_time_subject, ., 
            by = c("cohort", "exp", "box", "start_datetime", "end_datetime", "filename", "directory", "experiment_duration")) %>% # gets the correct labanimals for reference to fill in the missing animals
  mutate(numbers = coalesce(labanimalid.y, labanimalid.x)) %>% # not yet labanimalid bc there are unassigned values 
  select(-matches("[.]")) 
# using reference id's to assign unlabelled sexes
date_time_subject_mut <- date_time_subject_mut %>% 
  left_join(., date_time_subject_mut %>% 
              select(numbers) %>% 
              dplyr::filter(grepl("^[MF]", numbers)) %>% 
              mutate(numbersonly = gsub("[^\\d]+", "", numbers, perl = T)) %>% distinct(), 
            by = c("numbers" = "numbersonly")) %>% 
  mutate(labanimalid = coalesce(numbers.y, numbers)) %>%
  select(-matches("[.]|numbers")) %>% 
  select(labanimalid, everything())
# check date_time_subject_mut$labanimalid %>% table()

# fix subject 0
# by using metadata, box and room to assign
date_time_subject_no0 <- date_time_subject_mut %>% mutate_all(as.character) %>% left_join(., date_time_subject_mut %>% split(., .$cohort) %>% lapply(., function(x){
  x <- x %>% 
    mutate(room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename), NA)) %>% 
    arrange(room, as.numeric(box)) %>% 
    dplyr::filter(!grepl("[MF]", labanimalid)|lead(!grepl("[MF]", labanimalid))|lag(!grepl("[MF]", labanimalid))) %>% 
    mutate(dbcomment = ifelse(!grepl("[MF]", labanimalid), "box info used to fill labanimalid", NA)) %>% 
    group_by(box) %>% mutate(labanimalid = labanimalid[grepl("[MF]", labanimalid)][1]) %>%  # spot checking for deaths
    arrange(labanimalid, start_datetime) 
  return(x) # remove labanimalid0 or blank subset from original df and then insert the corrected ones (keep the dbcomment variable)
}) %>% rbindlist(.) %>% mutate_all(as.character), by = c("cohort", "exp", "box", "filename", "directory", "start_datetime", "end_datetime", "experiment_duration")) %>% 
  mutate(labanimalid = coalesce(labanimalid.y, labanimalid.x)) %>%
  select(-matches("[.]|room")) %>% 
  select(labanimalid, everything()) 
  

# %>% 
#   mutate(room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename), NA),
#          start_datetime = as.character(lubridate::mdy_hms(start_datetime)),
#          end_datetime =  as.character(lubridate::mdy_hms(end_datetime)))


# date_time_subject_mut[str_detect(date_time_subject_mut$labanimalid, "^(M|F)\\d{4}", negate = F),]
date_time_subject_mut$labanimalid %>% table()

# replace rbindlist... with openxlsx::write.xlsx(., "labanimalid_assign_bybox.xlsx") to create the excel sheets that I sent to their lab 
# subject0 %>% openxlsx::write.xlsx(., "labanimalid_assign_bybox.xlsx")

date_time_subject_no0 %>% group_by(labanimalid, exp, cohort) %>% add_count() %>% subset(n!=1) %>% arrange(cohort, labanimalid, exp)
# example for why we need the excel sheet dates to confirm the correct labelling: date_time_subject_no0 %>% dplyr::filter(labanimalid == "F301", exp == "LGA19")

# before merging with excel dates
# include more dbcomments 
# fix strange filenames -2, -3

# include correct dates as another check (dates extracted from EXTRACT_EXCEL.R olivieroxy_excel_dateslong object)
date_time_subject_df_comp <- left_join(date_time_subject_no0, olivieroxy_excel_dateslong, by = c("cohort", "exp")) %>%
  mutate(valid = case_when(
    grepl("SHOCK", exp) & experiment_duration > 58 & excel_date == start_date ~ "yes",
    grepl("SHA", exp) & experiment_duration > 115 & excel_date == start_date~ "yes",
    grepl("LGA", exp) & experiment_duration > 355 & excel_date == start_date~ "yes",
    grepl("PR", exp) & experiment_duration > 60 & excel_date == start_date~ "yes",
    TRUE ~ "no")  ) ## XX check if we use start or end date; fix the mislabelled files XXXX PICK UP HERE 

















################################
########## SHA #################
################################





###### NEW FILES ##############
# label data with... 
sha_new_files <- grep(grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*txt", inv = T, value = T), pattern = ".*SHA", value = T) # 72 files

sha_subjects_new <- process_subjects_new(sha_new_files) %>% separate(labanimalid, c("row", "labanimalid"), sep = "_", extra = "merge") %>% 
  arrange(filename, as.numeric(row)) %>% select(-c(row, filename))
read_rewards_new <- function(x){
  rewards <- fread(paste0("awk '/W:/{flag=1;next}/5:/{flag=0}flag' ", "'", x, "' | awk '/0:/{print NR \"_\" $2}'"), header = F, fill = T)
  rewards$filename <- x
  return(rewards)
}
sha_rewards_new <-  lapply(sha_new_files, read_rewards_new) %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(sha_subjects_new) %>% 
  separate(labanimalid, into = c("labanimalid", "cohort", "exp", "filename", "date", "time"), sep = "_") %>% 
  mutate(date = lubridate::mdy(date), time = chron::chron(times = time)) 


# %>%  
#   left_join(., date_time_subject_df_comp %>% 
#               select(cohort, exp, filename, valid, start_date, start_time) %>% 
#               rename("date" = "start_date", "time" = "start_time"), 
#             by = c("cohort", "exp", "filename", "date", "time")) %>% 
#   dplyr::filter(valid == "yes") %>% 
#   mutate(time = as.character(time)) %>%
#   distinct() # fixes duplicates in filenames %in% c("MED1113C07HSSHA06", "MED1110C05HSSHA08", "MED1110C05HSSHA09") ### there are no dupes for dplyr::filter(!grepl("[MF]\\d+", labanimalid)) 


### dealing with missing subjects 
sha_rewards_new %>% count(labanimalid, exp, cohort) %>% subset(n!=1)


## notes 
## exclude files (from meeting)
# c("C01HSSHA06", "MED1113C07HSSHA05", "MED1114C07HSSHA08")
## exclude cases (from meeting )
# c("F720") for SHA03 bc both files with her data seem incorrect (MED1112C07HSSHA03 and MED1112C07HSSHA03-2)
# MED1113C07HSSHA07 is actually LGA data (code that validates the date is filtering out these cases, and in the file, sha07 data and pr data follows)

# deal with the missing subjects...
# join and update "df" by reference, i.e. without copy 
setDT(sha_rewards_new)             # convert to data.table without copy
sha_rewards_new[setDT(sha_rewards_new %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% # this captures all "NA" cases as checked with mutate_at(vars(labanimalid), na_if, "NA") %>% dplyr::filter(is.na(labanimalid))
                        left_join(., date_time_subject_df_comp %>% 
                                    select(labanimalid, cohort, exp, filename, start_date, start_time) %>% 
                                    rename("date" = "start_date", "time" = "start_time") %>% 
                                    mutate(time = as.character(time)), 
                                  by = c("cohort", "exp", "filename", "date", "time")) ), 
                on = c("rewards", "cohort", "exp", "filename", "date", "time", "valid"), labanimalid := labanimalid.y] # don't want to make another missing object
setDF(sha_rewards_new)
sha_rewards_new %<>% 
  mutate_at(vars(rewards), as.numeric)
## case: deal with mislabelled subject?
sha_rewards_new %>% count(labanimalid, cohort,exp) %>% subset(n != 1)
sha_rewards_new %<>% mutate(labanimalid = replace(labanimalid, exp=="SHA01"&time=="09:24:16", "M768"))



###### OLD FILES ##############
# label data with... 
sha_old_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*Old.*SHA", value = T) # 40 files
sha_subjects_old <- process_subjects_old(sha_old_files)

# extract data...
sha_rewards_old <- lapply(sha_old_files, read_fread_old, "rewards") %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(sha_subjects_old %>% arrange(filename, as.numeric(row)) %>% select(-c("row", "filename"))) %>% 
  separate(labanimalid, into = c("labanimalid", "box", "cohort", "exp", "computer", "date"), sep = "_") %>% 
  mutate(date = lubridate::ymd(date),
         rewards = rewards %>% as.numeric()) 

# %>% 
#   dplyr::filter(valid == "valid") # no need for distinct() bc it is not an issue here

# deal with the missing subjects...
sha_rewards_old %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% dim
# will remove these cases bc these files have 7 subjects and both misssing subjects have another "session" (matched box)
# sha_rewards_old %<>% dplyr::filter(grepl("[MF]", labanimalid)) 

## case: deal with mislabelled subject?
sha_subjects_old %>% dplyr::filter(grepl("NA",))
sha_rewards_old %>% add_count(labanimalid, cohort,exp) %>% subset(n != 1)
sha_rewards_old %<>% add_count(labanimalid, cohort,exp) %<>% dplyr::filter(n == 1|(n==2&rewards!=0)) %<>% select(-n)












################################
########## LGA #################
################################

###### NEW FILES ##############
# label data with... 
lga_new_files <- grep(grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*txt", inv = T, value = T), pattern = ".*LGA", value = T) # 274 files

lga_subjects_new <- process_subjects_new(lga_new_files) %>% separate(labanimalid, c("row", "labanimalid"), sep = "_", extra = "merge") %>% 
  arrange(filename, as.numeric(row)) %>% select(-c(row, filename))
# extract data with 'read_rewards_new' function from SHA
lga_rewards_new <-  lapply(lga_new_files, read_rewards_new) %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(lga_subjects_new) %>% 
  separate(labanimalid, into = c("labanimalid", "cohort", "exp", "filename", "date", "time"), sep = "_") %>% 
  mutate(date = lubridate::mdy(date), time = chron::chron(times = time)) 


# %>%  
#   left_join(., date_time_subject_df_comp %>% 
#               select(cohort, exp, filename, valid, start_date, start_time) %>% 
#               rename("date" = "start_date", "time" = "start_time"), 
#             by = c("cohort", "exp", "filename", "date", "time")) %>% 
#   dplyr::filter(valid == "yes") %>% 
#   mutate(time = as.character(time)) %>%
#   distinct() # fixes duplicates in filenames %in% c("MED1113C07HSlga06", "MED1110C05HSlga08", "MED1110C05HSlga09") ### there are no dupes for dplyr::filter(!grepl("[MF]\\d+", labanimalid)) 


### dealing with missing and mislabelled subjects 
lga_subjects_new %>% dplyr::filter(grepl("NA", labanimalid)) #281 
lga_rewards_new %>% count(labanimalid, exp, cohort) %>% subset(n!=1)

## notes 
## exclude files (from meeting)


# deal with the missing subjects...
# join and update "df" by reference, i.e. without copy 
# setDT(sha_rewards_new)             # convert to data.table without copy
# sha_rewards_new[setDT(sha_rewards_new %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% # this captures all "NA" cases as checked with mutate_at(vars(labanimalid), na_if, "NA") %>% dplyr::filter(is.na(labanimalid))
#                         left_join(., date_time_subject_df_comp %>% 
#                                     select(labanimalid, cohort, exp, filename, start_date, start_time) %>% 
#                                     rename("date" = "start_date", "time" = "start_time") %>% 
#                                     mutate(time = as.character(time)), 
#                                   by = c("cohort", "exp", "filename", "date", "time")) ), 
#                 on = c("rewards", "cohort", "exp", "filename", "date", "time", "valid"), labanimalid := labanimalid.y] # don't want to make another missing object
# setDF(sha_rewards_new)
# sha_rewards_new %<>% 
#   mutate_at(vars(rewards), as.numeric)
# ## case: deal with mislabelled subject?
# sha_rewards_new %>% count(labanimalid, cohort,exp) %>% subset(n != 1)
# sha_rewards_new %<>% mutate(labanimalid = replace(labanimalid, exp=="SHA01"&time=="09:24:16", "M768"))



###### OLD FILES ##############
# label data with... 
lga_old_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*Old.*LGA", value = T) # 82 files
lga_subjects_old <- process_subjects_old(lga_old_files)

# extract data...
lga_rewards_old <- lapply(lga_old_files, read_fread_old, "rewards") %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(lga_subjects_old %>% arrange(filename, as.numeric(row)) %>% select(-c("row", "filename"))) %>% 
  separate(labanimalid, into = c("labanimalid", "box", "cohort", "exp", "computer", "date"), sep = "_") %>% 
  mutate(date = lubridate::ymd(date),
         rewards = rewards %>% as.numeric()) 

# %>% 
#   dplyr::filter(valid == "valid") # no need for distinct() bc it is not an issue here

# deal with the missing subjects...
lga_rewards_old %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% dim
# will remove these cases bc these files have 7 subjects and both misssing subjects have another "session" (matched box)
# lga_rewards_old %<>% dplyr::filter(grepl("[MF]", labanimalid)) 

## case: deal with mislabelled subject?
lga_subjects_old %>% dplyr::filter(grepl("NA",))
lga_rewards_old %>% add_count(labanimalid, cohort,exp) %>% subset(n != 1) %>% arrange(labanimalid, as.numeric(rewards)) # thinking that the rewards = 41 belongs to F111
# lga_rewards_old %>% subset(labanimalid == "F111"&exp == "LGA01") returns nothing, so my guess is that the F128 labelled reward is mislabelled











################################
########## PR ##################
################################

###### NEW FILES ##############
pr_new_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*New.*PR/", value = T) # 83 files
# label data with...
pr_subjects_new <- process_subjects_new(pr_new_files) %>% separate(labanimalid, c("row", "labanimalid"), sep = "_", extra = "merge") %>%
  arrange(filename, as.numeric(row)) %>% select(-c(row, filename)) # 1037
# extract data with diff function from `read_rewards_new` for sha
readrewards_pr <- function(x){
  rewards <- fread(paste0("awk '/B:/{print NR \"_\" $2}' ", "'", x, "'"), header = F, fill = T)
  rewards$filename <- x
  return(rewards)
}
pr_rewards_new <- lapply(pr_new_files, readrewards_pr) %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(pr_subjects_new) %>%
  separate(
    labanimalid,
    into = c("labanimalid", "cohort", "exp", "filename", "date", "time"),
    sep = "_"
  ) %>% mutate(
    date = lubridate::mdy(date),
    time = chron::chron(times = time)
  ) 
# qc with...
pr_rewards_new %>% count(labanimalid, exp, cohort) %>% subset(n!=1)
pr_rewards_new %>% distinct() %>% add_count(labanimalid, exp, cohort) %>% subset(n!=1)

# deal with the missing subjects...
# join and update "df" by reference, i.e. without copy 
# setDT(pr_rewards_new)             # convert to data.table without copy
# pr_rewards_new[setDT(pr_rewards_new %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% 
#                        left_join(., date_time_subject_df_comp %>% 
#                                    dplyr::filter(grepl("PR", exp)) %>% 
#                                    mutate(time = as.character(start_time), 
#                                           date = as.character(start_date)), 
#                                  by = c("exp", "filename", "date", "time"), all.x = T)), 
#                 on = c("rewards", "exp", "filename", "date", "time"), labanimalid := labanimalid.y] # don't want to make another missing object
# setDF(pr_rewards_new)
# pr_rewards_new %<>% 
#   mutate_at(vars(rewards), as.numeric)
# # remove invalid point
# pr_rewards_new %<>% dplyr::filter(!(labanimalid == "F717" & exp == "PR01" & time == "07:45:31"))
# pr_rewards_new %>% distinct() %>% add_count(labanimalid, exp, cohort) %>% subset(n!=1) # dim of df is dim of distinct(df)
# pr_rewards_new <- pr_rewards_new %>% mutate(date = lubridate::ymd(date))


###### OLD FILES ##############

pr_old_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*Old.*PR/", value = T) # 24 files

# label data with... 
pr_subjects_old <- process_subjects_old(pr_old_files) ## quick qc pr_subjects_old %>% dplyr::filter(grepl("NA", labanimalid)) returns none

# extract data...
pr_rewards_old <- lapply(pr_old_files, read_fread_old, "rewards") %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(pr_subjects_old %>% arrange(filename, as.numeric(row)) %>% select(-c("row", "filename"))) %>% 
  separate(labanimalid, into = c("labanimalid", "box", "cohort", "exp", "computer", "date"), sep = "_") %>% 
  mutate(date = lubridate::ymd(date))
         # rewards = rewards %>% as.numeric()) 
# %>% 
#   dplyr::filter(valid == "valid") # no need for distinct() bc it is not an issue here

# deal with the missing subjects...

## case: deal with mislabelled subjects?
pr_rewards_old %>% add_count(labanimalid, cohort,exp) %>% subset(n != 1)
# pr_rewards_old <- pr_rewards_old %>% 
#   mutate(labanimalid = replace(labanimalid, box == "2"&filename=="./C01/Old/PR/K3C01HSPR02-20170905.txt", "M21"), 
#          labanimalid = replace(labanimalid, box == "3"&filename=="./C01/Old/PR/K2C01HSPR01-20170814.txt", "M3")) %>% 
#   dplyr::filter(!(rewards == 0 & labanimalid == "M3" & filename == "./C01/Old/PR/K2C01HSPR01-20170814.txt")) %>% 
#   mutate(date = lubridate::ymd(date))
# %>% 
#   add_count(labanimalid, cohort,exp) %>% 
#   subset(n != 1) 



