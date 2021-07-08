# WANTS WASH monitoring tool - Data Merge Script
# REACH Yemen - 
# District level
# 06/07/2021 - Raphael Bacot - raphael.bacot@reach-initiative.org 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, openxlsx, data.table)
pacman::p_load_gh("mabafaba/hypegrammaR", "mabafaba/reachR")

# devtools::install_github("mabafaba/hypegrammaR", force = T, build_vignettes = T)
library("hypegrammaR")
## Load sourced function
source("R/utils.R")

## Specifcy filenames
# data.cholera.hh.filename <- "data/WASH_WANTS HH Cholera_cleandata_2021-06-29_corrected.xlsx"
# data.cholera.ki.filename <- "data/WASH_WANTS KI Cholera_cleandata_2021-06-17_FS.xlsx"
# data.common.hh.filename <- "data/WASH_WANTS HH Common_cleandata_2021-06-29_FS_Corrected.xlsx"
# data.common.ki.filename <- "data/WASH_WANTS KI Common_cleandata_2021-06-29_FS_corrected.xlsx"

data.cholera.hh.filename <- "data/WANTS_Cholera_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-46-21.xlsx"
data.cholera.ki.filename <- "data/WASH_Cholera_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-39-21.xlsx"
data.common.hh.filename <- "data/WANTS_Common_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-53-27.xlsx"
data.common.ki.filename <- "data/WASH_Common_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-38-11.xlsx"

kobo.cholera.hh.filename <- "data/Cholera_HH_tool.xlsx"
kobo.cholera.ki.filename <- "data/Cholera_KI_tool.xlsx"
kobo.common.hh.filename <- "data/Common_HH_tool.xlsx"
kobo.common.ki.filename <- "data/Common_KI_tool.xlsx"

analysis.cholera.hh.filename <- "data/WANTS_hh_cholera_dap.csv"
analysis.cholera.ki.filename <- "data/cholera_KI_analysis_plan_v1_district.csv"
analysis.common.hh.filename <- "data/WANTS_hh_common_dap_V1.csv"
analysis.common.ki.filename <- "data/common_KI_analysis_plan_v1_district.csv"

filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

## load all datasets
tools <- c("cholera.hh", "cholera.ki", "common.hh", "common.ki")                # Make sure that the list of tools here match the filenames above
for (t in tools) {
  assign(paste0("data_", gsub("\\.", "_", t)), read.xlsx(paste0("data.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("tool_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("choices_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get, sheet = "choices") %>% mutate_all(as.character))
  assign(paste0("analysis_", gsub("\\.", "_", t)), read.csv(paste0("analysis.", t, ".filename") %>% get) %>% mutate_all(as.character))
}
pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

## Streamline columns names in dataset and imported kobo tool
# Clean headers in data
data_common_hh <- data_common_hh %>% setNames(paste0(gsub("Comm_HH_", "", colnames(.)))) ## Delete Comm_HH_ prefix from column names
data_cholera_hh <- data_cholera_hh %>% setNames(paste0(gsub("Chol_HH_", "", colnames(.)))) ## Delete Chol_HH_ prefix from column names
data_common_ki <- data_common_ki %>% setNames(paste0(gsub("\\.", "/", colnames(.)))) ## Replace the choice separator "." with "/" to streamline with others
# Clean variable names in Kobo tools
tool_common_hh <- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name))
tool_cholera_hh <- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name))

# W5.1 How does your household adapt to the lack of water? Recoding choices
data_common_hh <- data_common_hh %>% 
  rename(`adapt_lack/less_preferred_drinking`=`adapt_lack/surface_water_other`, # surface_water_other should be less_preferred_drinking
         `adapt_lack/surface_drinking`=`adapt_lack/surface_water`,              # adapt_lack/surface_water should be adapt_lack/surface_drinking
         `adapt_lack/less_preferred_other`=`adapt_lack/untreated`,              # untreated should be less_preferred_other
         `adapt_lack/surface_other`=`adapt_lack/other_surface`) %>%             # other_surface should be surface_other
  mutate(adapt_lack = gsub("surfasurface_waterce_water_other","less_preferred_drinking",
                           gsub("surface_water", "surface_drinking",
                                gsub("untreated", "less_preferred_other",
                                     gsub("other_surface", "surface_other", adapt_lack)))))
# Check Comment if all good
# data_common_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique

data_cholera_hh <- data_cholera_hh %>%
  rename(`adapt_lack/further_source`=`adapt_lack/further`,                      # further should be further_source
         `adapt_lack/dangerous_source`=`adapt_lack/dangerous`) %>%              # dangerous should be dangerous_source
  mutate(adapt_lack = gsub("further", "further_source",
                           gsub("dangerous", "dangerous_source", adapt_lack)))
# Check Comment if all good
# data_cholera_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique

## Renaming for the KI datasets !! ORDER DEPENDENT, Don't switch order of line 79 and 80 !!
data_cholera_ki <- data_cholera_ki %>% 
  rename(w_waterneeds=w_watersmell,                                             # Renaming wrongly named columns in data cholera KI tool (shifted)
         w_watersmell=w_waterquality_what)                                      # Renaming wrongly named columns in data cholera KI tool (shifted)

## Compare columns from different tools
col_cholera_hh = data.frame(id=colnames(data_cholera_hh), col_cholera_hh=colnames(data_cholera_hh))
col_cholera_ki = data.frame(id=colnames(data_cholera_ki), col_cholera_ki=colnames(data_cholera_ki))
col_common_hh = data.frame(id=colnames(data_common_hh), col_common_hh=colnames(data_common_hh))
col_common_ki = data.frame(id=colnames(data_common_ki), col_common_ki=colnames(data_common_ki))

col.all <- col_cholera_hh %>% full_join(col_cholera_ki, by = "id") %>% full_join(col_common_hh, by="id") %>% full_join(col_common_ki, by="id") %>% rename(question_header=id)

## For all dataset, create as many binary column as choices for all select_one questions
# Get the list of all select one columns for all datasets
tool.list <- gsub("\\.", "_", tools)
for (t in tool.list){
  assign(paste0("col.select.one.", t), 
         get.select.db(get(paste0("choices_", t)), get(paste0("tool_", t))) %>%
           filter(q.type=="select_one", !(name %in% c("g_enum_agency", "g_governorate", "g_district", "g_sub_district", "x_form"))) %>% pull(name))
}

# For each select one colum in each dataset, create as many binary columns as choices present in the corresponding kobo tool
# Can take up to 30 seconds, be patient...

for (t in tool.list){
  values <- choice.long(get(paste0("choices_", t)), get(paste0("tool_", t))) %>% filter(!name %in% c("g_governorate", "g_district", "g_sub_district"))
  for (col in get(paste0("col.select.one.",t))){
    unique.val <- values %>% filter(name==col) %>% pull(value)
    if (length(unique.val)>0 & (col %in% colnames(get(paste0("data_", t))))){
      for (var in unique.val){
        varname <- paste0(col, "/", var)
        df <- get(paste0("data_", t)) %>%
          mutate(!!sym(varname) := ifelse((is.na(!!sym(col)) | (!!sym(col) %in% c(""))), NA, ifelse(grepl(var, !!sym(col)), "1", "0")), .after=col)
        assign(paste0("data_", t), df)
      }
    }
  }
}

## Consolidate Common and Cholera datasets together for KI and HHs respectively 
data_ki <- data_cholera_ki %>% bind_rows(data_common_ki)
data_hh <- data_cholera_hh %>% bind_rows(data_common_hh)

## Mutate binary as numerical variable
cols.num.ki <- c(colnames(data_ki)[grepl("/", colnames(data_ki))])
data_ki <- data_ki %>% mutate_at(vars(all_of(cols.num.ki)), as.numeric)

cols.num.hh <- c(colnames(data_hh)[grepl("/|infant|child|adult|elderly|hh_member_", colnames(data_hh))], "hh_number")
data_hh <- data_hh %>% mutate_at(vars(all_of(cols.num.hh)), as.numeric)

## Match with pcodes 
metacol <- c("g_governorate","admin1Name_en","admin1Name_ar","g_district","admin2Name_en","admin2Name_ar","g_sub_district","admin3Name_en","admin3Name_ar")
data_ki <- data_ki %>% select(-any_of(c(""))) %>% 
  left_join(pcodes %>% select(admin3Pcode, any_of(metacol)), by = c("g_sub_district"="admin3Pcode")) %>%
  select(all_of(metacol), everything())

data_hh <- data_hh %>% select(-any_of(c(""))) %>%
  left_join(pcodes %>% select(admin3Pcode, any_of(metacol)), by = c("g_sub_district"="admin3Pcode")) %>% 
  select(all_of(metacol), everything())

### 2. Analysis

## 2.0 Load needed functions
source("R/functions.R")
source("R/from_hyperanalysis_to_datamerge.R")

## 2.1. Analysis for Household level data
data_hh <- data_hh %>%                                                          # renaming for hypergrammar
  setnames(old=c("_index","data_uuid"), new=c("index","uuid"), skip_absent = T)

# Load questionnaire, data analysis plan
questionnaire_hh <- load_questionnaire(data_hh, tool_cholera_hh, choices_cholera_hh, choices.label.column.to.use = "name")
dap_hh <- load_analysisplan(analysis.cholera.hh.filename)

# Launch Analysis Script
analysis_hh <- from_analysisplan_map_to_output(data = data_hh, analysisplan = dap_hh, weighting = NULL, questionnaire = questionnaire_hh, labeled = TRUE)

# SUMMARY STATS LIST ##
summary.stats.list.hh <- analysis_hh$results

# SUMMARY STATS LIST FORMATTED 
summarystats_hh <- summary.stats.list.hh %>% resultlist_summary_statistics_as_one_table
write.csv(summarystats_hh, paste0("HH_common_summarystats_final_",today,".csv"), row.names = F)

## 2.2. Analysis for KII level data
data_ki <- data_ki %>% setnames(old=c("_index","data_uuid"), new=c("index","uuid"), skip_absent = T)

# Load questionnaire and Data Analysis Plan
questionnaire_ki <- load_questionnaire(data_ki, tool_cholera_ki, choices_cholera_ki, choices.label.column.to.use = "name")
dap_ki <- load_analysisplan(analysis.cholera.ki.filename)

# Launch Analysis Script
analysis_ki <- from_analysisplan_map_to_output(data = data_ki, analysisplan = dap_ki, weighting = NULL, questionnaire = questionnaire_ki, labeled = TRUE)

# SUMMARY STATS LIST ##
summary.stats.list.ki <- analysis_ki$results

# SUMMARY STATS LIST FORMATTED 
summarystats_ki <- summary.stats.list.ki %>% resultlist_summary_statistics_as_one_table
write.csv(summarystats_ki, paste0("KI_common_summarystats_final_",today,".csv"), row.names = F)

# Archived

# 1. non looped way of getting all select one columns
# col.select.one.common_ki <- get.select.db(choices_common_ki, tool_common_ki) %>%
#   filter(q.type=="select_one", !(name %in% c("g_enum_agency", "g_governorate", "g_district", "g_sub_district", "x_form"))) %>% pull(name)
# col.select.one.cholera_ki <- get.select.db(choices_cholera_ki, tool_cholera_ki) %>%
#   filter(q.type=="select_one", !(name %in% c("g_enum_agency", "g_governorate", "g_district", "g_sub_district", "x_form"))) %>% pull(name)
# col.select.one.common_hh <- get.select.db(choices_common_hh, tool_common_hh) %>%
#   filter(q.type=="select_one", !(name %in% c("g_enum_agency", "g_governorate", "g_district", "g_sub_district", "x_form"))) %>% pull(name)
# col.select.one.cholera_hh <- get.select.db(choices_cholera_hh, tool_cholera_hh) %>%
#   filter(q.type=="select_one", !(name %in% c("g_enum_agency", "g_governorate", "g_district", "g_sub_district", "x_form"))) %>% pull(name)

# 2. non looped way (for one dataset) of mutating the binary columns
# values <- choice.long(choices_common_ki, tool_common_ki) %>% filter(!name %in% c("g_governorate", "g_district", "g_sub_district"))
# for (col in col.select.one){
#   unique.val <- values %>% filter(name==col) %>% pull(value)
#   if (length(unique.val)>0){
#     for (var in unique.val){
#       varname <- paste0(col, "/", var)
#       data_common_ki <- data_common_ki %>%
#         mutate(!!sym(varname) := ifelse((is.na(!!sym(col)) | (!!sym(col) %in% c(""))), NA, ifelse(grepl(var, !!sym(col)), 1, 0)), .after=col)
#     }
#   }
# }
