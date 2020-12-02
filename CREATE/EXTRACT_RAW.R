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
  
  read_meta_box_new <- function(x) {
    box_new <-
      fread(
        paste0("awk '/Box/{print $2}' ", "'", x, "'"),
        fill = T,
        header = F
      )
    return(box_new)
  }
  box_new <- lapply(x, read_meta_box_new) %>% rbindlist()
  
  
  names_sha_append <- lapply(x, read_subjects_new) %>% rbindlist() %>% rename("labanimalid"="V1") %>%
    cbind(., date_time) %>%
    rename("date_time" = "V1") %>% 
    cbind(., box_new) %>% 
    rename("box" = "V1") %>%
    mutate(labanimalid = paste0( str_extract(labanimalid, "\\d+"), "_", 
                                str_extract(toupper(labanimalid), "[MF]\\d{1,3}"), "_", # labanimalid
                                str_extract(filename, "C\\d+"), "_", # cohort
                                sub('.*HSOXY', '', toupper(filename)), "_", # exp
                                sub(".*/.*/.*/", '', filename),  "_",
                                date_time, "_",
                                box)) %>% # subject id, cohort, experiment, file/location perhaps
    select(-c("date_time", "box"))
  
  return(names_sha_append)
  # return(box_new)
  
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
# by using metadata, box and room to assign and merge back into the original df
date_time_subject_no0 <- date_time_subject_mut %>% mutate_all(as.character) %>% left_join(., date_time_subject_mut %>% split(., .$cohort) %>% lapply(., function(x){
  x <- x %>% 
    mutate(room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename), NA)) %>% 
    arrange(room, as.numeric(box)) %>% 
    dplyr::filter(!grepl("[MF]", labanimalid)|lead(!grepl("[MF]", labanimalid))|lag(!grepl("[MF]", labanimalid))) %>% 
    mutate(dbcomment = ifelse(!grepl("[MF]", labanimalid), "box info used to fill labanimalid", NA)) %>% 
    group_by(room, box) %>% mutate(labanimalid = labanimalid[grepl("[MF]", labanimalid)][1]) %>%  # spot checking for deaths
    ungroup()
  return(x) # remove labanimalid0 or blank subset from original df and then insert the corrected ones (keep the dbcomment variable)
}) %>% rbindlist(.) %>% mutate_all(as.character), by = c("cohort", "exp", "box", "filename", "directory", "start_datetime", "end_datetime", "experiment_duration")) %>% 
  mutate(labanimalid = coalesce(labanimalid.y, labanimalid.x)) %>%
  select(-matches("[.]|room")) %>% 
  select(labanimalid, everything()) 
  

# date_time_subject_no0[str_detect(date_time_subject_no0$labanimalid, "^(M|F)\\d{4}", negate = F),]
date_time_subject_no0$labanimalid %>% table()

# replace rbindlist... with openxlsx::write.xlsx(., "labanimalid_assign_bybox.xlsx") to create the excel sheets that I sent to their lab 
# subject0 %>% openxlsx::write.xlsx(., "labanimalid_assign_bybox.xlsx")

# date_time_subject_no0 %>% group_by(labanimalid, exp, cohort) %>% add_count() %>% subset(n!=1) %>% arrange(cohort, labanimalid, exp)
# example for why we need the excel sheet dates to confirm the correct labelling: date_time_subject_no0 %>% dplyr::filter(labanimalid == "xx", exp == "xx")


# include correct dates as another check (dates extracted from EXTRACT_EXCEL.R olivieroxy_excel_dateslong object)
# use this and their team to decide on what the cutoff points should be
# date_time_subject_no0 %>% 
#   mutate(exp = str_extract(exp, "LGA|SHA|PR"),
#          experiment_duration = as.numeric(experiment_duration)) %>% 
#   ggplot(aes(x = experiment_duration)) + geom_histogram(stat = "count") + facet_grid(rows = vars(exp))


## before making the date_time_subject_df_comp, fix the exps on some lines from date_time_subject_no0
date_time_subject_no0 <- date_time_subject_no0 %>% 
  mutate(start_date = as.Date(start_datetime) %>% as.character,
         start_time = format(lubridate::ymd_hms(start_datetime), "%H:%M:%S") %>% chron::chron(times = .) %>% as.character) %>% 
  mutate(exp = replace(exp, filename == "MED1110C03HSOXYLGA17"&start_date=="2019-02-28", "LGA18"),
       exp = replace(exp, grepl("BSB273[BCDE]C04HSOXYLGA15", filename)&start_date=="2019-07-26", "LGA16"),
       exp = replace(exp, grepl("BSB273[BC]C05HSOXYLGA11", filename)&start_date=="2019-10-15", "LGA12")) 

date_time_subject_df_comp <-
  left_join(date_time_subject_no0,
            olivieroxy_excel_dateslong,
            by = c("cohort", "exp")) %>%
  mutate(experiment_duration = as.numeric(experiment_duration)) %>%
  rowwise() %>%
  mutate(valid = ifelse(grepl("SHA", exp) & experiment_duration > 115 & stringr::str_detect(end_datetime, excel_date) & !grepl("C01", cohort), "yes", 
    ifelse(grepl("SHA", exp) & stringr::str_detect(end_datetime, excel_date) & experiment_duration > 175 & grepl("C01", cohort), "yes",
           # ifelse(grepl("LGA", exp) & stringr::str_detect(end_datetime, excel_date) & experiment_duration > 715 & !grepl("C0(1|3|4)", cohort), "yes", #later cohorts use end date
                  ifelse(grepl("LGA", exp) & stringr::str_detect(start_datetime, excel_date) & experiment_duration > 715 & grepl("C0(1|4|5)", cohort), "yes", # earlier cohorts use start date
                         ifelse(grepl("LGA", exp) & stringr::str_detect(end_datetime, excel_date) & experiment_duration > 715 & grepl("C03", cohort), "yes", # XX 5/21 found that C03 uses end date
                                ifelse(grepl("PR", exp) & stringr::str_detect(end_datetime, excel_date) & experiment_duration > 55 ,"yes", 
                                "no")))))) %>% ## Using start date ## Got these numbers from Lauren on 3/16/2020
  ungroup() 
  

# see the distribution of valid cases
date_time_subject_df_comp %>% select(exp, valid, cohort) %>% table()
date_time_subject_df_comp %>% subset(valid == "no"&cohort=="C03") %>% select(exp, filename) %>% table() %>% as.data.frame() %>% subset(Freq!=0) %>% get_dupes(exp)
date_time_subject_df_comp %>% subset(cohort == "C03") %>% mutate(end_date = as.character(as.Date(end_datetime))) %>% distinct(exp, start_date, end_date, excel_date) %>% mutate(matches = ifelse(start_date == excel_date, "start", ifelse(end_date == excel_date, "end", "neither"))) %>% select(matches) %>% table()


## date_time_subject_df_comp %>% subset(valid == "no") %>% dplyr::filter(map2_lgl(excel_date, start_datetime, str_detect)) checking row by row if the comparison is working (using tidyverse)
date_time_subject_df_comp %>% subset(valid == "no") %>% select(cohort, exp) %>% table()
date_time_subject_df_comp %>% add_count(labanimalid, exp) %>% subset(valid == "no"&n ==1) %>% select(labanimalid, exp) %>% table()


# %>% 
#   mutate(room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename), NA),
#          start_datetime = as.character(lubridate::mdy_hms(start_datetime)),
#          end_datetime =  as.character(lubridate::mdy_hms(end_datetime)))










  




################################
########## SHA #################
################################





###### NEW FILES ##############
# label data with... 
sha_new_files <- grep(grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*txt", inv = T, value = T), pattern = ".*SHA", value = T) # 104 files

sha_subjects_new <- process_subjects_new(sha_new_files) %>% separate(labanimalid, c("row", "labanimalid"), sep = "_", extra = "merge") %>% 
  arrange(filename, as.numeric(row)) %>% select(-c(row, filename))
read_rewards_new <- function(x){
  rewards <- fread(paste0("awk '/W:/{flag=1;next}/5:/{flag=0}flag' ", "'", x, "' | awk '/0:/{print NR \"_\" $2}'"), header = F, fill = T)
  rewards$filename <- x
  return(rewards)
}
sha_rewards_new <-  lapply(sha_new_files, read_rewards_new) %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(sha_subjects_new) %>% 
  separate(labanimalid, into = c("labanimalid", "cohort", "exp", "filename", "date", "time", "box"), sep = "_") %>% 
  mutate(date = lubridate::mdy(date), time = chron::chron(times = time)) %>% 
  mutate(room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename), NA)) %>% 
  mutate_at(vars("date", "time"), as.character)


# %>%  
#   left_join(., date_time_subject_df_comp %>% 
#               select(cohort, exp, filename, valid, start_date, start_time) %>% 
#               rename("date" = "start_date", "time" = "start_time"), 
#             by = c("cohort", "exp", "filename", "date", "time")) %>% 
#   dplyr::filter(valid == "yes") %>% 
#   mutate(time = as.character(time)) %>%
#   distinct() # fixes duplicates in filenames %in% c("MED1113C07HSSHA06", "MED1110C05HSSHA08", "MED1110C05HSSHA09") ### there are no dupes for dplyr::filter(!grepl("[MF]\\d+", labanimalid)) 


### dealing with missing subjects 
sha_rewards_new %>% get_dupes(labanimalid, exp, cohort)


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
                on = c("rewards", "cohort", "exp", "filename", "date", "time"), labanimalid := labanimalid.y] # don't want to make another missing object
setDF(sha_rewards_new)
sha_rewards_new %<>% 
  mutate_at(vars(rewards), as.numeric)

# add valid columns
sha_rewards_new_valid <- sha_rewards_new %>% 
  left_join(., date_time_subject_df_comp %>% 
              select(filename, valid, start_date, start_time) %>% 
              rename("date" = "start_date", "time" = "start_time"),
            by = c("filename", "date", "time")) %>% 
  distinct() %>% 
  subset(valid == "yes")

sha_rewards_new_valid %>% get_dupes(labanimalid, exp)


###### OLD FILES ##############
# label data with... 
sha_old_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*Old.*SHA", value = T) # 40 files
sha_subjects_old <- process_subjects_old(sha_old_files)

# extract data...
sha_rewards_old <- lapply(sha_old_files, read_fread_old, "rewards") %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(sha_subjects_old %>% arrange(filename, as.numeric(row)) %>% select(-c("row", "filename"))) %>% 
  separate(labanimalid, into = c("labanimalid", "box", "cohort", "exp", "computer", "date"), sep = "_") %>% 
  mutate(date = lubridate::ymd(date) %>% as.character,
         rewards = rewards %>% as.numeric()) 

sha_rewards_old %>% get_dupes(exp, labanimalid)

# %>% 
#   dplyr::filter(valid == "valid") # no need for distinct() bc it is not an issue here

# deal with the missing subjects...
sha_rewards_old %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% dim
# will remove these cases bc these files have 7 subjects and both missing subjects have another "session" (matched box)
# sha_rewards_old %<>% dplyr::filter(grepl("[MF]", labanimalid)) 

## case: deal with mislabelled subject?
sha_rewards_old %<>% subset(!((grepl("K3C01HSOXYSHA01-20180730.txt", filename)&labanimalid == "M153"&rewards==0)|(grepl("K3C01HSOXYSHA01-20180730.txt", filename)&labanimalid == "M155"&rewards==0))) #  5/28 "ignore the first occurence of box 5 and box 6 in K3C01HSOXYSHA01-20180730.txt" - Lauren
sha_rewards_old %<>% add_count(labanimalid, cohort,exp) %<>% dplyr::filter(n == 1|(n!=1&rewards!=0)) %<>% select(-n)
sha_rewards_old %>% get_dupes(exp, labanimalid)











################################
########## LGA #################
################################
setwd("~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone/Oxycodone GWAS")


## PROTOCOL 
## 5/22 from Lauren
# Experiment should only be 14 days, any additional days fill the time between the other experiments (PR and/or treatments) 
# So I think you only need to extract the first 14 LgA for each cohort for analysis


###### NEW FILES ##############
# label data with... 
lga_new_files <- grep(grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*txt", inv = T, value = T), pattern = ".*LGA", value = T) # 392 files

lga_subjects_new <- process_subjects_new(lga_new_files) %>% 
  subset(!grepl("C:\\\\MED-PC", labanimalid)) %>%  #  Add this line to correct this error message from file BSB273CC07HSOXYLGA16 -- from  Item 2 has 95 rows but longest item has 111; recycled with remainder.
  separate(labanimalid, c("row", "labanimalid"), sep = "_", extra = "merge") %>% 
  arrange(filename, as.numeric(row)) %>% select(-c(row, filename))

# extract data with 'read_rewards_new' function from SHA
lga_rewards_new <- lapply(lga_new_files, read_rewards_new) %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(lga_subjects_new) %>% 
  separate(labanimalid, into = c("labanimalid", "cohort", "exp", "filename", "date", "time", "box"), sep = "_") %>% 
  mutate(date = lubridate::mdy(date), time = chron::chron(times = time)) %>% 
  mutate(date = as.character(date), time = as.character(time))
  

## before binding to date_time_subject_df_comp to make sure that the exp matches
lga_rewards_new <- lga_rewards_new %>% 
  mutate(exp = replace(exp, filename == "MED1110C03HSOXYLGA17"&date=="2019-02-28", "LGA18"),
         exp = replace(exp, grepl("BSB273[BCDE]C04HSOXYLGA15", filename)&date=="2019-07-26", "LGA16"),
         exp = replace(exp, grepl("BSB273[BC]C05HSOXYLGA11", filename)&date=="2019-10-15", "LGA12")) 

## deal with missing subjects and give valid
setDT(lga_rewards_new)             # convert to data.table without copy
lga_rewards_new[setDT(lga_rewards_new %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% 
                        left_join(., date_time_subject_df_comp %>%
                                    select(labanimalid, cohort, exp, filename, valid, start_date, start_time, box) %>%
                                    rename("date" = "start_date", "time" = "start_time"),
                                  by = c("cohort", "exp", "filename", "date", "time", "box"), all.x = T)), 
                     on = c("rewards", "exp", "filename", "date", "time", "box"), labanimalid := labanimalid.y] # don't want to make another missing object
setDF(lga_rewards_new)


# add valid column (above code does not)
lga_rewards_new_valid <- lga_rewards_new %>%  
  left_join(., date_time_subject_df_comp %>% 
              select(filename, valid, start_date, start_time) %>% 
              rename("date" = "start_date", "time" = "start_time"),
            by = c("filename", "date", "time")) %>% 
  distinct() %>% 
  subset(valid == "yes") # Lauren says that we do not need to include the is.na valid cases are from c03 lga19 and c04 lga24 that don't exist in the excel sheets 
## 09/21/2020 -- none returned 


lga_rewards_new_valid <- lga_rewards_new_valid %>% 
  mutate(labanimalid = replace(labanimalid, filename == "BSB273CC04HSOXYLGA12"&box=="16", "M452"), # 5/22 "M452 should be in box 16 for that file" - Lani
         labanimalid = replace(labanimalid, filename == "BSB273CC04HSOXYLGA12"&box=="15", "M451") # 5/22 "Box 15 should be M451" - Lani
         ) %>% 
  mutate(rewards = as.numeric(rewards))
  
lga_rewards_new_valid %>% get_dupes(labanimalid, exp)


## note 
# C01 MED1110C01HSOXYLGA14 2018-08-31 LGA14 ## not verified by excel -- excel has 2018-09-01 for LGA14
# C03 MED1110C03HSOXYLGA19 2019-03-05 LGA19 ## not verified by excel -- lga19 doesn't exist
# C05 BSB273DC05HSOXYLGA09 2018-08-24 LGA09 ## not verified by excel -- excel has 2019-10-10 for LGA09
# C05 BSB273DC05HSOXYLGA17 2018-09-14 LGA17 ## not verified by excel -- excel has 10/28/2019 for LGA17
## could not carry on with cohorts 6 and 7 (excel sheets not yet uploaded) 5/20


###### OLD FILES ##############
# label data with... 
lga_old_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*Old.*LGA", value = T) # 82 files
lga_subjects_old <- process_subjects_old(lga_old_files)

# extract data...
lga_rewards_old <- lapply(lga_old_files, read_fread_old, "rewards") %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(lga_subjects_old %>% arrange(filename, as.numeric(row)) %>% select(-c("row", "filename"))) %>% 
  separate(labanimalid, into = c("labanimalid", "box", "cohort", "exp", "computer", "date"), sep = "_") %>% 
  mutate(date = lubridate::ymd(date) %>% as.character,
         rewards = rewards %>% as.numeric()) 

# %>% 
#   dplyr::filter(valid == "valid") # no need for distinct() bc it is not an issue here

# deal with the missing subjects...
lga_rewards_old %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% dim

## case: deal with mislabelled subject?
lga_rewards_old %>% add_count(labanimalid, cohort,exp) %>% subset(n != 1) %>% arrange(labanimalid, as.numeric(rewards)) # thinking that the rewards = 41 belongs to F111
# lga_rewards_old %>% subset(labanimalid == "F111"&exp == "LGA01") returns nothing, so my guess is that the F128 labelled reward is mislabelled
lga_rewards_old %>% get_dupes(labanimalid, exp) 

lga_rewards_old <- lga_rewards_old %>% 
  mutate(labanimalid = replace(labanimalid, filename=="./C01/Old/LGA/K1C01HSOXYLGA01-20180815.txt"&computer == "K1", "F122")) %>% ## noted in excel for C01 OXY "Rat F122 is F128 on the K1 Data file"
  add_count(labanimalid, exp) %>% 
  dplyr::filter(n==1|(n!=1 & grepl("-2.txt", filename))) %>% 
  select(-n) %>% 
  mutate_all(as.character)
  
lga_rewards_old %>% get_dupes(labanimalid, exp) 








################################
########## PR ##################
################################

###### NEW FILES ##############
pr_new_files <- grep(list.files(path = ".", recursive = T, full.names = T), pattern = ".*New.*PR/", value = T) # 99 files
# label data with...
pr_subjects_new <- process_subjects_new(pr_new_files) %>% separate(labanimalid, c("row", "labanimalid"), sep = "_", extra = "merge") %>%
  arrange(filename, as.numeric(row)) %>% select(-c(row, filename)) # 1232


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
    into = c("labanimalid", "cohort", "exp", "filename", "date", "time", "box"),
    sep = "_"
  ) %>% mutate(
    date = lubridate::mdy(date) %>% as.character,
    time = chron::chron(times = time) %>% as.character
  ) %>% 
  mutate(exp = mgsub::mgsub(exp, c("PR([1-9]{1})$", paste0("TREATMENT", 1:4)), c("PR0\\1", paste0("PR0", 3:6, "_T0", 1:4)))) # uniform exp names to join to subject comp

## for missing subjects 
pr_rewards_new <- left_join(pr_rewards_new %>% mutate(start_datetime = paste(date, time)), date_time_subject_df_comp, by = c("cohort", "exp", "filename", "start_datetime")) %>% 
  mutate(labanimalid = coalesce(labanimalid.y, labanimalid.x)) %>% 
  select(-c("labanimalid.x", "labanimalid.y")) %>% 
  mutate(rewards = replace(rewards, cohort == "C04"&exp == "PR02", NA)) %>% # "it was noted in the lab notebook that this data did not record properly, even though it was extracted from the computers; But Giordano fixed this issue for the cohorts after C04" - Lani (3/31)
  select(-c(setdiff(names(.), names(pr_rewards_new)))) %>% 
  distinct() %>% #1232 gets rid of many cases from cohort 4 and 5 of 0 rewards 
  mutate(rewards = as.numeric(rewards)) %>% 
  mutate(as.character)

# qc with...
pr_rewards_new %>% get_dupes(labanimalid, exp)
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
  mutate(date = lubridate::ymd(date) %>% as.character,
         rewards = as.numeric(rewards)) %>% 
  mutate(exp = mgsub::mgsub(exp, c("PR([1-9]{1})$", paste0("TREATMENT", 1:4)), c("PR0\\1", paste0("PR0", 3:6, "_T0", 1:4)))) # uniform exp names to join to subject comp



# deal with the missing subjects...
pr_rewards_old %>% subset(!grepl("M|F(\\d){3}", labanimalid)) %>% select(labanimalid) %>% table()

## case: deal with mislabelled subjects? (each id should be associated with one exp)
pr_rewards_old %>% get_dupes(labanimalid, exp)
pr_rewards_old %>% add_count(labanimalid, cohort,exp) %>% subset(n != 1)

pr_rewards_old <- pr_rewards_old %>% 
  dplyr::filter(!(labanimalid=="F120"&rewards=="0"&exp=="PR05_T03")) %>% 
  mutate_all(as.character)
















### Extract timeout presses

#####
## lga
#####

lga_c01_07_files <- list.files(path = "~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone/Oxycodone GWAS", recursive = T, full.names = T) %>% 
  grep("LGA", ., value = T)


lga_c01_07_timeout <- lapply(lga_c01_07_files, function(x){
  if(grepl("Old", x)){ # process if directory is old
    df <- fread(paste0("awk '/^RatNumber/{flag=1;next}/^ProgramName/{flag=0}flag' ", "'", x, "'"), fill = T, header = F) 
    df$filename = x
    df$box = fread(paste0("awk '/^BoxNumber/{flag=1;next}/^Sessionlength/{flag=0}flag' ", "'", x, "'"), fill = T, header = F)  
    df$to_active_presses = fread(paste0("awk '/^TotalTOResponses/{flag=1;next}/^TotalRspInAct/{flag=0}flag' ", "'", x, "'"), fill = T, header = F)
  }
  
  if(grepl("New", x)){ # process if directory is new
    df <- fread(paste0("awk '/Subject:/{print NR \"_\" $2}' ", "'", x, "'"), fill = T, header = F)
    df$filename = x 
    df$rewards = fread(paste0("awk '/B:/{print $2}' ", "'", x, "'"), fill = T, header = F)
    df$presses = fread(paste0("awk '/G:/{print $2}' ", "'", x, "'"), fill = T, header = F)
    df$box = fread(paste0("awk '/Box:/{print $2}' ", "'", x, "'"), fill = T, header = F) 
    df$to_active_presses =  df$presses -  df$rewards
    df <- df[, c("V1", "filename", "box", "to_active_presses")]
  }
  
  return(df)
  
})

# use to try for old  lga_c01_07_files[c(37:42, 113:118)]
# use to try for new  lga_c01_07_files[c(1:6, 469:474)]
# use to try for old and new  lga_c01_07_files[c(37:42, 113:118, 1:6, 469:474)]

lga_c01_07_timeout_df <- lga_c01_07_timeout %>% rbindlist() %>% 
  rename("labanimalid" = "V1") %>% 
  mutate(labanimalid = str_extract(toupper(labanimalid), "[MF]\\d+"),
         session = str_extract(toupper(filename), "LGA\\d+"),
         cohort = str_extract(toupper(filename), "/C\\d+/") %>% gsub("/", "", .),
         sex = str_extract(toupper(labanimalid), "[MF]"),
         room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename) %>% gsub(".*LGA/", "", .), NA)
  ) %>% 
  select(cohort, labanimalid, sex, session, box, room, to_active_presses, filename) 

lga_c01_07_timeout_trials1_14 <- lga_c01_07_timeout_df %>% 
  subset(parse_number(session) < 15) %>% 
  mutate(box = as.character(box))

# fix these 
lga_c01_07_timeout_trials1_14 %>% get_dupes(labanimalid, session)
# lga_c01_07_timeout_trials1_14_join %>% get_dupes(labanimalid, session) %>% naniar::vis_miss
lga_c01_07_timeout_trials1_14_distinct <- lga_c01_07_timeout_trials1_14 %>% 
  select(cohort, labanimalid, sex, session, box, room, to_active_presses) %>% 
  distinct() %>% 
  left_join(lga_c01_07_timeout_trials1_14 %>% # join to a dataframe that drops the filename and creates drop tag
              select(cohort, labanimalid, sex, session, box, room, to_active_presses) %>% 
              distinct() %>% 
              get_dupes(labanimalid, session) %>% 
              group_by(labanimalid, session, .drop = F) %>% 
              mutate(nzero = sum(to_active_presses)) %>% 
              subset(to_active_presses != nzero&to_active_presses == 0) %>% ungroup() %>% mutate(drop = "drop") %>% 
              select(labanimalid, cohort, session, to_active_presses, drop), by = c("labanimalid", "cohort", "session", "to_active_presses")) %>% 
  subset(is.na(drop))

lga_c01_07_timeout_trials1_14_brent <- lga_c01_07_timeout_trials1_14_distinct %>% 
  left_join(., lga_c01_07_timeout_trials1_14_distinct %>% get_dupes(labanimalid, session) %>% select(labanimalid, cohort, session, to_active_presses, dupe_count), by = c("labanimalid", "cohort", "session", "to_active_presses")) %>% 
  rename("need_check"="dupe_count") %>% 
  mutate(need_check = ifelse(!is.na(need_check), "check", NA)) %>% 
  select(cohort, labanimalid, sex, session, box, room, to_active_presses, need_check) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num, session) %>% 
  select(-labanimalid_num)


openxlsx::write.xlsx(lga_c01_07_timeout_trials1_14_brent, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxycodone_lga_to_presses.xlsx")

# use brent's file to correct 
lga_c01_07_timeout_brent_decision <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxycodone_lga_to_presses_BB.xlsx") %>% 
  clean_names() %>% 
  mutate_all(as.character) %>% 
  subset(is.na(decision)|decision=="KEEP") 

# write the data in wide form and calculate mean and deviations 
lga_c01_07_timeout_brent_decision_dev <- lga_c01_07_timeout_brent_decision %>% 
  left_join(lga_c01_07_timeout_brent_decision %>% 
              subset(cohort == "C07"&session == "LGA08"&room=="BSB273D") %>% 
              select(cohort, labanimalid, box, room), by = c("cohort", "box", "room")) %>% 
  rename("labanimalid" = "labanimalid.x") %>% 
  mutate(labanimalid = coalesce(labanimalid, labanimalid.y)) %>% 
  select(-labanimalid.y) %>% 
  select(-need_check, -decision, -notes, -box, -room) %>% 
  mutate(sex = str_extract(labanimalid, "[MF]")) %>% 
  spread(session, to_active_presses) %>% mutate_at(vars(matches("^LGA")), as.numeric) %>% mutate(mean = rowMeans(select(., matches("^LGA")), na.rm = T)) %>% mutate_at(vars(matches("^LGA")), list(dev = ~round((.-mean)^2))) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num) %>% 
  select(-labanimalid_num)

openxlsx::write.xlsx(lga_c01_07_timeout_brent_decision_dev, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxycodone_lga_to_presses_wide_dev.xlsx")






#####
## sha
#####
sha_c01_07_files <- list.files(path = "~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone/Oxycodone GWAS", recursive = T, full.names = T) %>% 
  grep("SHA", ., value = T)


sha_c01_07_timeout <- lapply(sha_c01_07_files, function(x){
  if(grepl("Old", x)){ # process if directory is old
    df <- fread(paste0("awk '/^RatNumber/{flag=1;next}/^ProgramName/{flag=0}flag' ", "'", x, "'"), fill = T, header = F) 
    df$filename = x
    df$box = fread(paste0("awk '/^BoxNumber/{flag=1;next}/^Sessionlength/{flag=0}flag' ", "'", x, "'"), fill = T, header = F)  
    df$to_active_presses = fread(paste0("awk '/^TotalTOResponses/{flag=1;next}/^TotalRspInAct/{flag=0}flag' ", "'", x, "'"), fill = T, header = F)
  }
  
  if(grepl("New", x)){ # process if directory is new
    df <- fread(paste0("awk '/Subject:/{print NR \"_\" $2}' ", "'", x, "'"), fill = T, header = F)
    df$filename = x 
    df$rewards = fread(paste0("awk '/W:/{flag=1;next}/5:/{flag=0}flag' ", "'", x, "' | awk '/0:/{print $2}'"), header = F, fill = T)
    df$presses = fread(paste0("awk '/R:/{flag=1;next}/5:/{flag=0}flag' ", "'", x, "' | awk '/0:/{print $2}'"), header = F, fill = T)
    df$box = fread(paste0("awk '/Box:/{print $2}' ", "'", x, "'"), fill = T, header = F) 
    df$to_active_presses =  df$presses -  df$rewards
    df <- df[, c("V1", "filename", "box", "to_active_presses")]
  }
  
  return(df)
  
})

sha_c01_07_timeout_df <- sha_c01_07_timeout %>% rbindlist() %>% 
  rename("labanimalid" = "V1") %>% 
  mutate(labanimalid = str_extract(toupper(labanimalid), "[MF]\\d+"),
         session = str_extract(toupper(filename), "SHA\\d+"),
         cohort = str_extract(toupper(filename), "/C\\d+/") %>% gsub("/", "", .),
         sex = str_extract(toupper(labanimalid), "[MF]"),
         room = ifelse(grepl("[[:alnum:]]+C\\d{2}HS", filename), gsub("C\\d{2}HS.*", "", filename) %>% gsub(".*SHA/", "", .), NA)
  ) %>% 
  mutate(box = as.character(box)) %>% 
  select(cohort, labanimalid, sex, session, room, box, to_active_presses, filename)

# check sex, session, and room
sha_c01_07_timeout_df %>% select(sex, session, room) %>% sapply(table, exclude = NULL)

# fix these cases
sha_c01_07_timeout_df %>% get_dupes(labanimalid, session)

sha_c01_07_timeout_distinct <- sha_c01_07_timeout_df %>% 
  select(cohort, labanimalid, sex, session, box, room, to_active_presses) %>% 
  distinct() %>% 
  left_join(sha_c01_07_timeout_df %>% 
              select(cohort, labanimalid, sex, session, box, room, to_active_presses) %>% 
              distinct() %>% 
              get_dupes(labanimalid, session) %>% 
              group_by(labanimalid, session, .drop = F) %>% 
              mutate(nzero = sum(to_active_presses)) %>% 
              subset(to_active_presses != nzero&to_active_presses == 0) %>% ungroup() %>% mutate(drop = "drop") %>% 
              select(labanimalid, cohort, session, to_active_presses, drop), by = c("labanimalid", "cohort", "session", "to_active_presses")) %>% 
  subset(is.na(drop))

sha_c01_07_timeout_brent <- sha_c01_07_timeout_distinct %>% 
  left_join(., sha_c01_07_timeout_distinct %>% get_dupes(labanimalid, session) %>% select(labanimalid, cohort, session, to_active_presses, dupe_count), by = c("labanimalid", "cohort", "session", "to_active_presses")) %>% 
  rename("need_check"="dupe_count") %>% 
  mutate(need_check = ifelse(!is.na(need_check), "check", NA)) %>% 
  select(cohort, labanimalid, sex, session, box, room, to_active_presses, need_check) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num, session) %>% 
  select(-labanimalid_num) %>% 
  mutate(need_check = replace(need_check, is.na(labanimalid), "check"))

openxlsx::write.xlsx(sha_c01_07_timeout_brent, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxycodone_sha_to_presses.xlsx")

# use brent's file to correct 
sha_c01_07_timeout_brent_decision <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxycodone_sha_to_presses_BB.xlsx") %>% 
  clean_names() %>% 
  mutate_all(as.character) %>% 
  subset(is.na(decision)|decision=="KEEP") 

# write the data in wide form and calculate mean and deviations 
sha_c01_07_timeout_brent_decision_dev <- sha_c01_07_timeout_brent_decision %>% 
  select(-need_check, -decision, -notes) %>% 
  spread(session, to_active_presses) %>% mutate_at(vars(matches("^SHA")), as.numeric) %>% mutate(mean = rowMeans(select(., matches("^SHA")), na.rm = T)) %>% mutate_at(vars(matches("^SHA")), list(dev = ~round((.-mean)^2))) %>% 
  mutate(labanimalid_num = parse_number(labanimalid)) %>% 
  arrange(cohort, sex, labanimalid_num) %>% 
  select(-labanimalid_num)

openxlsx::write.xlsx(sha_c01_07_timeout_brent_decision_dev, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE/oxycodone_sha_to_presses_wide_dev.xlsx")

