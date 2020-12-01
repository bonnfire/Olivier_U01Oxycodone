## CREATE TRAIT DESCRIPTION TABLE

trait_description_table <- data.frame(
  variable = c("bodyweight"),
  variable_description = c("abdominal fat weight measured in grams"),
  cov_or_trait = "trait",
  general_category = c("Whole Body"),
  trait_name = c("Body Weight"),
  subtrait_name = c("abdominal fat weight")
) %>% 
  rbind(data.frame(variable = paste0(selfadmin_rewards_cohort1 %>% names %>% 
                                       tolower %>% grep("^(sha|pr|lga)", ., value = T), "_rewards")) %>% 
          mutate_all(as.character) %>% 
          mutate(variable_description = "number of oxycodone infusions received during", 
                 cov_or_trait = "trait",
                 general_category = "Behavior", 
                 trait_name = "Consumption level",
                 subtrait_name = "oxycodone preference") %>% 
          rowwise() %>% 
          mutate(variable_description = case_when(
            grepl("lga.*_rewards", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of long access self administration (6 hours)"),
            grepl("sha.*_rewards", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of short access self administration (2 hours)"),
            grepl("^pr", variable) ~ paste(variable_description,  "day",
                                                      parse_number(str_extract_all(variable, "\\d+")[[1]][1]), "of progressive ratio at breakpoint with opoid addiction drug or vehicle number", parse_number(str_extract_all(variable, "\\d+")[[1]][2]))
          )
          ) %>% 
          ungroup()
  ) %>% 
  rbind(data.frame(variable = paste0(selfadmin_rewards_cohort1 %>% names %>% 
                                       tolower %>% grep("^(sha|pr|lga)", ., value = T), "_left")) %>% 
          mutate_all(as.character) %>% 
          mutate(variable_description = "number of active lever presses during", 
                 cov_or_trait = "trait",
                 general_category = "Behavior", 
                 trait_name = "Consumption level",
                 subtrait_name = "oxycodone preference") %>% 
          rowwise() %>% 
          mutate(variable_description = case_when(
            grepl("lga.*_left", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of long access self administration (6 hours)"),
            grepl("sha.*_left", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of short access self administration (2 hours)"),
            grepl("^pr", variable) ~ paste(variable_description,  "day",
                                           parse_number(str_extract_all(variable, "\\d+")[[1]][1]), "of progressive ratio at breakpoint with opoid addiction drug or vehicle number", parse_number(str_extract_all(variable, "\\d+")[[1]][2]))
          )
          ) %>% 
          ungroup()
  ) %>% 
  rbind(data.frame(variable = paste0(selfadmin_rewards_cohort1 %>% names %>% 
                                       tolower %>% grep("^(sha|pr|lga)", ., value = T), "_right")) %>% 
          mutate_all(as.character) %>% 
          mutate(variable_description = "number of inactive lever presses during", 
                 cov_or_trait = "trait",
                 general_category = "Behavior", 
                 trait_name = "Consumption level",
                 subtrait_name = "oxycodone preference") %>% 
          rowwise() %>% 
          mutate(variable_description = case_when(
            grepl("lga.*_right", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of long access self administration (6 hours)"),
            grepl("sha.*_right", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of short access self administration (2 hours)"),
            grepl("^pr", variable) ~ paste(variable_description,  "day",
                                           parse_number(str_extract_all(variable, "\\d+")[[1]][1]), "of progressive ratio at breakpoint with opoid addiction drug or vehicle number", parse_number(str_extract_all(variable, "\\d+")[[1]][2]))
          )
          ) %>% 
          ungroup()
  ) %>% 
  rbind(data.frame(variable = selfadmin_rewards_cohort1 %>% names %>% 
                                       tolower %>% grep("^(sha|pr|lga)", ., value = T, invert = T)) %>% 
          mutate_all(as.character) %>% 
          mutate(variable_description = "NA", 
                 cov_or_trait = "covariate",
                 general_category = "NA", 
                 trait_name = "NA",
                 subtrait_name = "NA") %>% 
          rowwise() %>% 
          mutate(variable_description = case_when(
            grepl("date_lga", variable) ~ paste("date of day",
                                                     parse_number(variable), "of long access self administration (6 hours)"),
            grepl("date_sha", variable) ~ paste("date of day",
                                                     parse_number(variable), "of short access self administration (2 hours)"),
            grepl("date_pr", variable) ~ paste("date of day",
                                           parse_number(str_extract_all(variable, "\\d+")[[1]][1]), "of progressive ratio at breakpoint with opoid addiction drug or vehicle number", parse_number(str_extract_all(variable, "\\d+")[[1]][2]))
          )
          ) %>% 
          ungroup()
  ) %>% 
  rbind(data.frame(variable = paste0(selfadmin_rewards_cohort1 %>% names %>% 
                                       tolower %>% grep("^(sha|pr|lga)", ., value = T), "_age")) %>% 
          mutate_all(as.character) %>% 
          mutate(variable_description = "age of rat during", 
                 cov_or_trait = "covariate",
                 general_category = "NA", 
                 trait_name = "NA",
                 subtrait_name = "NA") %>% 
          rowwise() %>% 
          mutate(variable_description = case_when(
            grepl("lga", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of long access self administration (6 hours)"),
            grepl("sha", variable) ~ paste(variable_description, "day",
                                                     parse_number(variable), "of short access self administration (2 hours)"),
            grepl("pr", variable) ~ paste(variable_description,  "day",
                                           parse_number(str_extract_all(variable, "\\d+")[[1]][1]), "of progressive ratio at breakpoint with opoid addiction drug or vehicle number", parse_number(str_extract_all(variable, "\\d+")[[1]][2]))
          )
          ) %>% 
          ungroup()
  ) %>% 
  rbind(data.frame(variable = von_frey_df %>% select_if(is.numeric) %>% names) %>% 
              mutate_all(as.character) %>% 
              mutate(variable_description = "NA", 
                     cov_or_trait = "trait", 
                     general_category = "NA", 
                     trait_name = "NA", 
                     subtrait_name = "NA")) %>% 
  rbind(data.frame(variable = tail_immersion_df %>% select_if(is.numeric) %>% names) %>% 
          mutate_all(as.character) %>% 
          mutate(variable_description = "NA", 
                 cov_or_trait = "trait", 
                 general_category = "NA", 
                 trait_name = "NA", 
                 subtrait_name = "NA")) %>% 
  mutate_all(as.character)

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Olivier_U01Oxycodone/CREATE")
trait_description_table %>% 
  openxlsx::write.xlsx(file = "data_dictionary_olivier_oxycodone.xlsx") # open to make manual changes before sending to Apurva for Colorado group
