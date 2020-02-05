# setwd("~/Dropbox (Palmer Lab)/Olivier_George_U01")
## temporarily using this one: 


cohortfiles <- list.files(pattern = "*.xlsx")
# 4 excels: 1,3,4 are all in the same format but 5 is in the format from the cocaine exp

olivierfiles_oxy <- function(filename){
  
  
  setwd("~/Dropbox (Palmer Lab)/Olivier_George_U01/GWAS Self Administration Data/Oxy Data")
  options(scipen = 100) # prevent sci notation
  u01.importxlsx <- function(xlname){
    df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
    names(df) <- excel_sheets(xlname)
    return(df)
  }
  
### put back into fxn once one works for one file
  
  }

########################
# COHORT 1 
########################
filename <- cohortfiles[1]
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
selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table


nm <- names(selfadmin)[-c(1:2)] %>% sub("(PR\\d+)(TRTMEN)?(T)[0]?([1-9])", paste0("\\1_\\3", str_pad("\\4", 3, "left", "0")), .) # make date columns for this vector of exp names 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

comments <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))] #extact comments

selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^\\d", RFID))})

# from cohort 3 # Prtreatment03
# prtrtment04
# Prtrtment02
# Prtrtment01
# lga15pretrt



########################
# COHORT 3
########################
filename <- cohortfiles[2]
selfadmin <- u01.importxlsx(filename)[[1]] %>%
  as.data.table
selfadmin[ selfadmin == "n/a" ] <- NA # change all character n/a to actual NA
# set correct column names 
# create date columns
dates <- grep("^\\d+", selfadmin[3,] %>% unlist() %>% as.character(), value = T) # use these columns to make date columns # ignore anything else
dates <- as.character(as.POSIXct(as.numeric(dates) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")) # convert Excel character into dates

setnames(selfadmin, toupper(as.character(selfadmin[4,]) )) # now that dates are moved into separate vector, remove from the column names 
setnames(selfadmin,  sub("(PR\\d+)(TRTMEN)?(T)[0]?([1-9])", paste0("\\1_\\3", str_pad("\\4", 3, "left", "0")), names(selfadmin)) )
names(selfadmin)[1] <- "RAT"
selfadmin <- selfadmin[-1,]
selfadmin <- remove_empty(selfadmin, "cols") # janitor::remove_empty_cols() deprecated
selfadmin <- selfadmin[1:grep("average", selfadmin$RAT)[1],] # subset only the rewards table


nm <- names(selfadmin)[-c(1:2)] %>% sub("(PR\\d+)(TRTMEN)?(T)[0]?([1-9])", paste0("\\1_\\3", str_pad("\\4", 3, "left", "0")), .) # make date columns for this vector of exp names 
nm1 <- paste("date", nm, sep = "_") # make these date columns
selfadmin[ , ( nm1 ) := lapply( dates, function(x) rep(x, each = .N) ) ] # make the date columns 

comments <- selfadmin[which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))] #extact comments

selfadmin <- selfadmin[!which(selfadmin$RAT %in% c("COMMENT", "CONFLICT", "RESOLUTION"))]

selfadmin_exps <- grep("REWARDS|ACTIVE|INACTIVE|PR", selfadmin$RAT)
selfadmin_split <- split(selfadmin, cumsum(1:nrow(selfadmin) %in% selfadmin_exps))
names(selfadmin_split) <- lapply(selfadmin_split, function(x){ x$RAT %>% head(1)}) %>% unlist() %>% as.character()
selfadmin_split <- lapply(selfadmin_split, function(x){ x %>% dplyr::filter(grepl("^\\d", RFID))})


