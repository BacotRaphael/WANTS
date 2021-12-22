# WANTS WASH monitoring tool - Data Cleaning Script
# REACH Yemen  
# 27/07/2021 - Raphael Bacot - raphael.bacot@reach-initiative.org 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, openxlsx, data.table, lubridate)

## Create directories 
dir.create("cleaning", showWarnings = F)

## Specify filenames
data.cholera.hh.filename <- "data/WANTS_Cholera_HH_-_REACH_Yemen_-_all_versions_-_False_-_2021-12-20-16-11-40.xlsx"
kobo.cholera.hh.filename <- "data/tool_cholera_hh.xlsx"

data.common.hh.filename <- "data/WANTS_Common_HH_-_REACH_Yemen_-_all_versions_-_False_-_2021-12-20-16-13-13.xlsx"
kobo.common.hh.filename <- "data/tool_common_hh.xlsx"

data.cholera.ki.filename <- "data/WASH_Cholera_Key_Informant_Questionnaire_-_all_versions_-_False_-_2021-12-20-16-13-20.xlsx"
kobo.cholera.ki.filename <- "data/tool_cholera_ki.xlsx"

data.common.ki.filename <- "data/WASH_Common_Key_Informant_Questionnaire_-_all_versions_-_False_-_2021-12-20-16-13-14.xlsx"
kobo.common.ki.filename <- "data/tool_common_ki.xlsx"

filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

# helper functions 
source("R/utils.R")

################################################################################################
## 1. Load and consolidate datsets
################################################################################################
## 1.1. Load all raw datasets
tools <- c("cholera.hh", "cholera.ki", "common.hh", "common.ki")                # Make sure that the list of tools here match the filenames above
for (t in tools) {
  assign(paste0("data_", gsub("\\.", "_", t)), read_excel(paste0("data.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("tool_", gsub("\\.", "_", t)), read_excel(paste0("kobo.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("choices_", gsub("\\.", "_", t)), read_excel(paste0("kobo.", t, ".filename") %>% get, sheet = "choices") %>% mutate_all(as.character))
}

pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

tool_hh <- bind_rows(tool_cholera_hh, tool_common_hh) %>% filter(!duplicated(name))
choices_hh <- bind_rows(choices_cholera_hh, choices_common_hh) %>% filter(!duplicated(paste0(list_name,name)))

tool_ki <- bind_rows(tool_cholera_ki, tool_common_ki) %>% filter(!duplicated(name))
choices_ki <- bind_rows(choices_cholera_ki, choices_common_ki) %>% filter(!duplicated(paste0(list_name,name)))

## Rename _uuid _index and mutate id in case of duplicates
list_df <- lapply(paste0("data_", gsub("\\.", "_", tools)), get)
names(list_df) <- paste0("data_", gsub("\\.", "_", tools))
list_df <- lapply(list_df, function(x) x <- x %>% setnames(old = c("_uuid", "_index", "NO"), new = c("uuid", "index", "number"), skip_absent = T) %>% mutate(id=1:nrow(.)) %>% select(id, uuid, everything()))
mapply(function(x,y) assign(x,y, .GlobalEnv), names(list_df), list_df)
# for (i in seq_along(1:length(list_df))){assign(names(list_df)[i], list_df[[i]])}

## 1.2. Streamline columns names in dataset and imported kobo tool and consolidate common and cholera datset for HH and KI
### Note: This section will align column names and choices and create a consolidate dataset (the first function creates data_hh, the second will create data_ki)
which.tools.loaded()                                                            ## quick function that checks which tool is loaded

harmonise.consolidate.hh()                                                      ## 2. harmonise and consolidate common and cholera HH 
harmonise.consolidate.ki()                                                      ## 3. harmonise and consolidate common and cholera HH
# harmonise.and.consolidate.datasets()                                          ## 1. original function that cleans HH and KI at once

################################################################################################
## 2. Apply changes from updated cleaning logs
################################################################################################
# tool.type <- "HH"
tool.type <- "KI"
tool <- tolower(tool.type)
if (tool.type=="HH"){data <- data_hh} else {data <- data_ki}

## 2.1. Load cleaning logs from all partners
dir.create("cleaning/partners feedback", showWarnings = F)
dir.create("cleaning/partners feedback/hh", showWarnings = F)
dir.create("cleaning/partners feedback/ki", showWarnings = F)

## Load internal cleaning log
file.internal.cleaning.log <- "cleaning/WASH_WANTS_ki_cleaning log_internal_2021-12-22.xlsx"
cleanig.log.internal <- read.xlsx(file.internal.cleaning.log) %>% mutate_all(as.character)

## Load Partners' external cleaning logs
updated.cl.files <- list.files(paste0("cleaning/partners feedback/", tool))
cleaning.log <- lapply(updated.cl.files, function(x) read.xlsx(paste0("cleaning/partners feedback/",tool,"/", x)) %>% mutate_all(as.character)) %>% bind_rows

## If all external cleaning log are already consolidated:
cleaning.log <- read_excel("cleaning/WASH_WANTS_ki_cleaning log_2021-12-22_updated.xlsx")

## Consolidate intenal and external cleaning logs
cleaning.log <- cleaning.log %>% bind_rows(cleanig.log.internal)
  
## 2.2. Apply changes from cleaning log
data_cleaned <- data
for (r in seq_along(1:nrow(cleaning.log))){
  variable <- cleaning.log[r, "variable"] %>% as.character
  if (variable %in% colnames(data_cleaned)){
    data_cleaned[data_cleaned$uuid %in% cleaning.log[r, "uuid"], variable] <- cleaning.log[r, "new_value"]
    }
  }

## 2.3. Write cleaned datasets
dir.create("output/data cleaned", showWarnings = F)
data_cleaned %>% write.xlsx(paste0("output/data cleaned/data_cleaned_", tool.type, "_", today, ".xlsx"))
data_cleaned %>% filter(tool == "common") %>% write.xlsx(paste0("output/data cleaned/data_cleaned_common_", tool.type, "_", today, ".xlsx"))
data_cleaned %>% filter(tool == "cholera") %>% write.xlsx(paste0("output/data cleaned/data_cleaned_cholera_", tool.type, "_", today, ".xlsx"))



