## extract raw 
setwd("~/Dropbox (Palmer Lab)/GWAS (1)/Oxycodone/Oxycodone GWAS")
# after cohort 3, there are only new files


## USEFUL FUNCTIONS

# FOR ~NEW~ DIRECTORIES
# #extract names to be assigned for various tables later
process_subjects_new <- function(x){
  
  read_subjects_new <- function(x){
    subjects <- fread(paste0("awk '/Subject/{print NR \"_\" $2}' ", "'", x, "'"),fill = T,header=F)
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
                                str_extract(toupper(labanimalid), "[MF]\\d{1,3}"), "_",
                                str_extract(filename, "C\\d+"), "_",
                                sub('.*HS', '', toupper(filename)), "_",
                                sub(".*/.*/.*/", '', filename), "_",
                                V1)) %>% # subject id, cohort, experiment, file/location perhaps
  select(-V1)
  
  return(names_sha_append)
  
}








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
  mutate(date = lubridate::mdy(date), time = chron::chron(times = time)) %>%  
  left_join(., date_time_subject_df_comp %>% 
              select(cohort, exp, filename, valid, start_date, start_time) %>% 
              rename("date" = "start_date", "time" = "start_time"), 
            by = c("cohort", "exp", "filename", "date", "time")) %>% 
  dplyr::filter(valid == "yes") %>% 
  mutate(time = as.character(time)) %>%
  dplyr::filter(!filename %in% c("C01HSSHA06", "MED1113C07HSSHA05", "MED1114C07HSSHA08")) %>% # update records several lines down from meeting to show other team's confirmation 
  distinct() # fixes duplicates in filenames %in% c("MED1113C07HSSHA06", "MED1110C05HSSHA08", "MED1110C05HSSHA09") ### there are no dupes for dplyr::filter(!grepl("[MF]\\d+", labanimalid)) 

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
sha_subjects_old <- process_subjects_old(sha_old_files)
# extract data...
sha_rewards_old <- lapply(sha_old_files, read_fread_old, "rewards") %>% rbindlist() %>% separate(V1, into = c("row", "rewards"), sep = "_") %>% arrange(filename, as.numeric(row)) %>% select(-row) %>% 
  bind_cols(sha_subjects_old %>% arrange(filename, as.numeric(row)) %>% select(-c("row", "filename"))) %>% 
  separate(labanimalid, into = c("labanimalid", "box", "cohort", "exp", "computer", "date", "valid"), sep = "_") %>% 
  mutate(date = lubridate::ymd(date),
         rewards = rewards %>% as.numeric()) %>% 
  dplyr::filter(valid == "valid") # no need for distinct() bc it is not an issue here

# deal with the missing subjects...
sha_rewards_old %>% dplyr::filter(!grepl("[MF]", labanimalid)) %>% dim
# will remove these cases bc these files have 7 subjects and both misssing subjects have another "session" (matched box)
sha_rewards_old %<>% dplyr::filter(grepl("[MF]", labanimalid)) 

## case: deal with mislabelled subject?
sha_rewards_old %>% add_count(labanimalid, cohort,exp) %>% subset(n != 1)
sha_rewards_old %<>% add_count(labanimalid, cohort,exp) %<>% dplyr::filter(n == 1|(n==2&rewards!=0)) %<>% select(-n)


