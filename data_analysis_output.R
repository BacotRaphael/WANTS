# WANTS WASH monitoring tool - Data Merge Script
# REACH Yemen  
# 06/07/2021 - Raphael Bacot - raphael.bacot@reach-initiative.org 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, openxlsx, data.table, lubridate)
pacman::p_load_gh("mabafaba/reachR2")
pacman::p_load_gh("mabafaba/hypegrammaR")

# devtools::install_github("mabafaba/hypegrammaR", force = T, build_vignettes = T, auth_token = "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")
# remotes::install_github("mabafaba/hypegrammaR")

## Load sourced function
source("R/utils.R")

## Create directories 
dir.create("analysis", showWarnings = F)
dir.create("output", showWarnings = F)

## Specifcy filenames
# data.cholera.hh.filename <- "data/WANTS_Cholera_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-46-21.xlsx"
# data.cholera.ki.filename <- "data/WASH_Cholera_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-39-21.xlsx"
# data.common.hh.filename <- "data/WANTS_Common_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-53-27.xlsx"
# data.common.ki.filename <- "data/WASH_Common_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-38-11.xlsx"

data.hh.cleaned.filename <- "output/data cleaned/data_cleaned_HH_2021-07-29.xlsx"
data.ki.cleaned.filename <- "output/data cleaned/data_cleaned_KI_2021-07-29.xlsx"

kobo.cholera.hh.filename <- "data/Cholera_HH_tool.xlsx"
kobo.cholera.ki.filename <- "data/Cholera_KI_tool.xlsx"
kobo.common.hh.filename <- "data/Common_HH_tool.xlsx"
kobo.common.ki.filename <- "data/Common_KI_tool.xlsx"

analysis.cholera.hh.filename <- "data/WANTS_hh_cholera_dap.csv"
analysis.cholera.ki.filename <- "data/cholera_KI_analysis_plan_v1_district.csv"
analysis.common.hh.filename <- "data/WANTS_hh_common_dap_V1.csv"
analysis.common.ki.filename <- "data/common_KI_analysis_plan_v1_district.csv"

filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

list.filenames <- grep("filename", ls(), value = T)
list.filenames <- list.filenames[!grepl("cleaned", list.filenames)]
list.tools.specified <- grep("hh|ki", unique(gsub("analysis\\.|data\\.|kobo\\.|\\.filename", "", list.filenames)), value = T)

## load all datasets
data_hh <- read.xlsx(data.hh.cleaned.filename)
data_ki <- read.xlsx(data.ki.cleaned.filename)

# tools <- c("cholera.hh", "cholera.ki", "common.hh", "common.ki")                # Make sure that the list of tools here match the filenames above
tools <- list.tools.specified
for (t in tools) {
  tool <- gsub("\\.", "_", t)
  assign(paste0("tool_", tool), read.xlsx(paste0("kobo.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("choices_", tool), read.xlsx(paste0("kobo.", t, ".filename") %>% get, sheet = "choices") %>% mutate_all(as.character))
  assign(paste0("analysis_", tool), read.csv(paste0("analysis.", t, ".filename") %>% get) %>% mutate_all(as.character))
}

pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

# Consolidates choices and tools from common and cholera
## Harmonise the kobo tools following the changes done in column headers in data_cleaning.R
harmonise.tools()

tool_hh <- bind_rows(tool_cholera_hh, tool_common_hh) %>% filter(!duplicated(name))
choices_hh <- bind_rows(choices_cholera_hh, choices_common_hh) %>% filter(!duplicated(paste0(list_name,name)))
tool_ki <- bind_rows(tool_cholera_ki, tool_common_ki) %>% filter(!duplicated(name))
choices_ki <- bind_rows(choices_cholera_ki, choices_common_ki) %>% filter(!duplicated(paste0(list_name,name)))

## For all dataset, create as many binary column as choices for all select_one questions 
# Get the list of all select one columns for all datasets
t.list <- gsub("common\\.|cholera\\.", "", list.tools.specified) %>% unique
for (t in t.list){
  assign(paste0("col.select.one.", t), get(paste0("tool_", t)) %>% filter(grepl("select_one", type)) %>% pull(name))
  assign(paste0("col.select.multiple.", t), get(paste0("tool_", t)) %>% filter(grepl("select_multiple", type)) %>% pull(name))
}

# For each select one/multiple colum in each dataset, create as many binary columns as choices present in the corresponding kobo tool
# Can take up to 30 seconds, be patient...
for (t in t.list){
  values <- choice.long(get(paste0("choices_", t)), get(paste0("tool_", t))) %>% filter(!name %in% c("g_governorate", "g_district", "g_sub_district"))
  for (col in c(get(paste0("col.select.one.",t)), get(paste0("col.select.multiple.",t)))){
    unique.val <- values %>% filter(name==col) %>% pull(value)
    if (length(unique.val)>0 & (col %in% colnames(get(paste0("data_", t))))){
      for (var in unique.val){
        varname <- paste0(col, ".", var)
        if (!varname %in% colnames(get(paste0("data_", t)))){
          df <- get(paste0("data_", t)) %>%
            mutate(!!sym(varname) := ifelse((is.na(!!sym(col)) | (!!sym(col) %in% c(""))), NA, ifelse(grepl(var, !!sym(col)), "1", "0")), .after=col)
          assign(paste0("data_", t), df)
          }
      }
    }
  }
}

## Mutate binary as numerical variable
cols.num.ki <- c(colnames(data_ki)[grepl("\\.", colnames(data_ki))])
data_ki <- data_ki %>% mutate_at(vars(all_of(cols.num.ki)), as.numeric)
cols.num.hh <- c(colnames(data_hh)[grepl("\\.|infant|child|adult|elderly|hh_member_", colnames(data_hh))], "hh_number")
data_hh <- data_hh %>% mutate_at(vars(all_of(cols.num.hh)), as.numeric)

## Match with pcodes 
metacol <- c("g_governorate","admin1Name_en","admin1Name_ar","g_district","admin2Name_en","admin2Name_ar","g_sub_district","admin3Name_en","admin3Name_ar")

data_ki <- data_ki %>% select(-any_of(c(""))) %>% 
  left_join(pcodes %>% select(admin3Pcode, any_of(metacol)), by = c("g_sub_district"="admin3Pcode")) %>%
  select(all_of(metacol), everything()) %>% mutate(date.month = lubridate::floor_date(as.Date(date_survey, format="%Y-%m-%d"), "month"))

data_hh <- data_hh %>% select(-any_of(c(""))) %>%
  left_join(pcodes %>% select(admin3Pcode, any_of(metacol)), by = c("g_sub_district"="admin3Pcode")) %>% 
  select(all_of(metacol), everything()) %>% mutate(date.month = lubridate::floor_date(as.Date(date_survey, format="%Y-%m-%d"), "month"))

### De-identify and rework column names for dashboard
# Anonymisation 
col.exclude <- c("g_enum_name", "g_enum_last_name", "g_enum_agency")            # to be updated
data_hh_anonymised <- data_hh %>% select(-any_of(col.exclude)) 
data_ki_anonymised <- data_ki %>% select(-any_of(col.exclude))

## Rework column title names  for external dataset
col.select.one <- c(col.select.one.hh, col.select.one.ki) %>% unique
col.select.one.binary <- paste0(col.select.one,".")

## Exclude all the select_one binary columns created for the Indesign data merge to limit column names
# separate between the binaries from select one and select multiples
data_hh_ext <- data_hh_anonymised %>% select(-any_of(matches(paste(col.select.one.binary, collapse = "|"))))
data_ki_ext <- data_ki_anonymised %>% select(-any_of(matches(paste(col.select.one.binary, collapse = "|"))))

# Rename using kobo tool labels for readability
## Household data
rep_hh <- bind_rows(tool_common_hh, tool_cholera_hh) %>% mutate(name = paste0("^", name)) %>% 
  bind_rows(choices_common_hh, choices_cholera_hh) %>%
  filter(!duplicated(name),!grepl("\\(|\\)", name), !is.na(`label::english`), !is.na(`label::arabic`)) %>%
  select(name, `label::english`, `label::arabic`) %>% mutate_all(~gsub("\\\\", "-", .)) %>%
  mutate(name = ifelse(!grepl("\\^", name), paste0(name, "$"), name))

data_hh_ext_labels <- data_hh_ext %>% setNames(colnames(.) %>% str_replace_all(., setNames(rep_hh$`label::english`, rep_hh$name)))
colnames(data_hh_ext_labels)[is.na(colnames(data_hh_ext_labels))] <- colnames(data_hh_ext)[is.na(colnames(data_hh_ext_labels))]

data_hh_ext_labels_ar <- data_hh_ext %>%  # In case column names needed in arabic 
  setNames(colnames(.) %>% str_replace_all(., setNames(rep_hh$`label::arabic`, rep_hh$name)))
colnames(data_hh_ext_labels_ar)[is.na(colnames(data_hh_ext_labels_ar))] <- colnames(data_hh_ext)[is.na(colnames(data_hh_ext_labels_ar))]

## Key informant data
rep_ki <- bind_rows(tool_common_ki, tool_cholera_ki) %>% mutate(name = paste0("^", name)) %>%
  bind_rows(choices_common_ki, choices_cholera_ki) %>%
  filter(!duplicated(name),!grepl("\\(|\\)", name), !is.na(`label::english`), !is.na(`label::arabic`)) %>%
  select(name, `label::english`, `label::arabic`) %>% mutate_all(~gsub("\\\\", "-", .)) %>%
  mutate(name = ifelse(!grepl("\\^", name), paste0(name, "$"), name))

data_ki_ext_labels <- data_ki_ext %>% setNames(colnames(.) %>% str_replace_all(., setNames(rep_ki$`label::english`, rep_ki$name)))
colnames(data_ki_ext_labels)[is.na(colnames(data_ki_ext_labels))] <- colnames(data_ki_ext)[is.na(colnames(data_ki_ext_labels))]

data_ki_ext_labels_ar <- data_ki_ext %>% # In case column names needed in arabic 
  setNames(colnames(.) %>% str_replace_all(., setNames(rep_ki$`label::arabic`, rep_ki$name))) 
colnames(data_ki_ext_labels_ar)[is.na(colnames(data_ki_ext_labels_ar))] <- colnames(data_ki_ext)[is.na(colnames(data_ki_ext_labels_ar))]

## Export all cleaned dataset for InDesign
data_hh_anonymised %>% write.xlsx(paste0("output/WANTS_data_hh_inDesign", today, ".xlsx"))
data_ki_anonymised %>% write.xlsx(paste0("output/WANTS_data_ki_inDesign", today, ".xlsx"))
  
## Export all cleaned dataset in xml and labels - Not for inDesign
data_hh_ext %>% write.xlsx(paste0("output/WANTS_data_hh_xml_", today,".xlsx"))
data_hh_ext_labels %>% write.xlsx(paste0("output/WANTS_data_hh_labels_", today,".xlsx"))
data_hh_ext_labels_ar %>% write.xlsx(paste0("output/WANTS_data_hh_labels_ar_", today,".xlsx"))

data_ki_ext %>% write.xlsx(paste0("output/WANTS_data_ki_xml_", today,".xlsx"))
data_ki_ext_labels %>% write.xlsx(paste0("output/WANTS_data_ki_labels_", today,".xlsx"))
data_ki_ext_labels_ar %>% write.xlsx(paste0("output/WANTS_data_ki_labels_ar_", today,".xlsx"))

## Split Common and CHolera for HH and KI and export it with the relevant name for PBI

data_common_hh_dashboard <- data_hh_ext %>% filter(tool=="common")
data_common_hh_dashboard %>% write.xlsx(paste0("output/WANTS_data_common_hh_dashboard_", today, ".xlsx"))
data_cholera_hh_dashboard <- data_hh_ext %>% filter(tool=="common")
data_cholera_hh_dashboard %>% write.xlsx(paste0("output/WANTS_data_cholera_hh_dashboard_", today, ".xlsx"))

data_common_ki_dashboard <- data_ki_ext %>% filter(tool=="common")
data_common_ki_dashboard %>% write.xlsx(paste0("output/WANTS_data_common_ki_dashboard_", today, ".xlsx"))
data_cholera_ki_dashboard <- data_ki_ext %>% filter(tool=="common")
data_cholera_ki_dashboard %>% write.xlsx(paste0("output/WANTS_data_cholera_ki_dashboard_", today, ".xlsx"))

### 2. Analysis
## 2.0 Load needed functions
source("R/functions.R")
source("R/from_hyperanalysis_to_datamerge.R")

## 2.1. Analysis for Household level data
data_hh <- data_hh %>%                                                          # renaming for hypergrammar
  setnames(old=c("_index","data_uuid"), new=c("index","uuid"), skip_absent = T)

# Load questionnaire, data analysis plan
questionnaire_hh <- hypegrammaR::load_questionnaire(data_hh, tool_cholera_hh, choices_cholera_hh, choices.label.column.to.use = "name")
dap_hh <- load_analysisplan(analysis.cholera.hh.filename)

# Launch Analysis Script
analysis_hh <- from_analysisplan_map_to_output(data = data_hh, analysisplan = dap_hh, weighting = NULL, questionnaire = questionnaire_hh, labeled = TRUE)
summary.stats.list.hh <- analysis_hh$results                                    # SUMMARY STATS LIST ##
summarystats_hh <- summary.stats.list.hh %>% resultlist_summary_statistics_as_one_table # SUMMARY STATS LIST FORMATTED

# final_melted_analysis_hh <- from_hyperanalysis_to_datamerge(summarystats_hh)  # Old function from sourced script, prefer to have it visible in main script if possible
melted_analysis_hh <- summarystats_hh %>%                                       # Pivot to wide all analysis results
  dplyr::rename(district_name=independent.var.value) %>%
  dplyr::select(-se, -min, -max, -repeat.var, -repeat.var.value, -independent.var) %>%
  pivot_wider(names_from = c("dependent.var", "dependent.var.value"), 
              values_from = "numbers") %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%                                       # at district level, assume that each question gets at least one answer => NA should be 0
  mutate_if(is.numeric, ~round(.*100, 4))                                       # multiply by 100 to get percentages

## join sample size, enumerator agency, governorate, month, year
ncol_agency <- data_hh %>% group_by(g_district) %>% summarise(n = length(unique(g_enum_agency))) %>% filter(n==max(n)) %>% pull(n) %>% unique
agency.cols <- paste0("g_enum_agency_", 1:ncol_agency)
data_hh_append <- data_hh %>% group_by(g_district) %>% dplyr::select(g_governorate, g_enum_agency, date_survey) %>%
  mutate(date_survey=lubridate::floor_date(as.Date(date_survey, format="%Y-%m-%d"), "month")) %>%
  dplyr::summarise_all(~paste(unique(.), collapse = " ")) %>% 
  mutate(date_survey=str_split(date_survey, " ")[[1]],
         month = lubridate::month(as.POSIXlt(date_survey, format="%Y-%m-%d"), label=T),
         year = lubridate::year(as.POSIXlt(date_survey, format="%Y-%m-%d")))
for (i in seq_along(1:length(agency.cols))){                                    # mutate as many agency columns as agency per district
  data_hh_append <- data_hh_append %>% mutate(!!sym(agency.cols[i]) := lapply(g_enum_agency, function(x) get.element(x, i)))}
data_hh_append <- data_hh_append %>% select(-g_enum_agency) %>% 
  left_join(data_hh %>% group_by(g_district) %>% summarise(sample_size=n()), by="g_district")

## Get arabic gov, dis + left_join by district => do the same for KI data
final_melted_analysis_hh <- melted_analysis_hh %>%
  left_join(pcodes %>% select(admin2Pcode, any_of(metacol)), by = c("district_name"="admin2Pcode")) %>%
  left_join(data_hh_append, by=c("district_name"="g_district")) %>%
  select(any_of(metacol), any_of(colnames(data_hh_append)), everything())

final_melted_analysis_hh_ar <- final_melted_analysis_hh %>% mutate_if(is.numeric, number.to.arabic)

# write.xlsx(summarystats_hh, paste0("analysis/HH_common_summarystats_final_",today,".xlsx"))
write.xlsx(final_melted_analysis_hh, paste0("analysis/HH_common_analysis_final_",today,".xlsx"))
write.xlsx(final_melted_analysis_hh_ar, paste0("analysis/HH_common_analysis_final_ar",today,".xlsx"))

## 2.2. Analysis for KII level data
data_ki <- data_ki %>% setnames(old=c("_index","data_uuid"), new=c("index","uuid"), skip_absent = T)

# Load questionnaire and Data Analysis Plan
questionnaire_ki <- hypegrammaR::load_questionnaire(data_ki, tool_cholera_ki, choices_cholera_ki, choices.label.column.to.use = "name")
dap_ki <- load_analysisplan(analysis.cholera.ki.filename)

# Launch Analysis Script
analysis_ki <- from_analysisplan_map_to_output(data = data_ki, analysisplan = dap_ki, weighting = NULL, questionnaire = questionnaire_ki, labeled = TRUE)
summary.stats.list.ki <- analysis_ki$results                                    # SUMMARY STATS LIST ##
summarystats_ki <- summary.stats.list.ki %>% resultlist_summary_statistics_as_one_table # SUMMARY STATS LIST FORMATTED 

melted_analysis_ki <- summarystats_ki %>%
  dplyr::rename(district_name=independent.var.value) %>%
  dplyr::select(-se, -min, -max, -repeat.var, -repeat.var.value, -independent.var) %>%
  pivot_wider(names_from = c("dependent.var", "dependent.var.value"), 
              values_from = "numbers") %>%
  mutate_all(~ifelse(is.na(.), 0, .)) %>%                                       # at district level, assume that each question gets at least one answer => NA should be 0
  mutate_if(~is.numeric(.), ~round(.*100, 4))                                       # multiply by 100 to get percentages

## join sample size, enumerator agency, governorate, month, year
ncol_agency <- data_ki %>% group_by(g_district) %>% summarise(n = length(unique(g_enum_agency))) %>% filter(n==max(n)) %>% pull(n) %>% unique
agency.cols <- paste0("g_enum_agency_", 1:ncol_agency)
data_ki_append <- data_ki %>% group_by(g_district) %>% dplyr::select(g_governorate, g_enum_agency, date_survey) %>%
  mutate(date_survey=lubridate::floor_date(as.Date(date_survey, format="%Y-%m-%d"), "month")) %>%
  dplyr::summarise_all(~paste(unique(.), collapse = " ")) %>% 
  mutate(date_survey=str_split(date_survey, " ")[[1]],
         month = lubridate::month(as.POSIXlt(date_survey, format="%Y-%m-%d"), label=T),
         year = lubridate::year(as.POSIXlt(date_survey, format="%Y-%m-%d")))
for (i in seq_along(1:length(agency.cols))){                                    # mutate as many agency columns as agency per district
  data_ki_append <- data_ki_append %>% mutate(!!sym(agency.cols[i]) := lapply(g_enum_agency, function(x) get.element(x, i)))}
data_ki_append <- data_ki_append %>% select(-g_enum_agency) %>% 
  left_join(data_ki %>% group_by(g_district) %>% summarise(sample_size=n()), by="g_district")

## Get arabic gov, dis + left_join by district => do the same for KI data
final_melted_analysis_ki <- melted_analysis_ki %>%
  left_join(pcodes %>% select(admin2Pcode, any_of(metacol)), by = c("district_name"="admin2Pcode")) %>%
  left_join(data_ki_append, by=c("district_name"="g_district")) %>%
  select(any_of(metacol), any_of(colnames(data_ki_append)), everything())

final_melted_analysis_ki_ar <- final_melted_analysis_ki %>% mutate_if(is.numeric, number.to.arabic)

write.xlsx(summarystats_ki, paste0("analysis/KI_common_summarystats_final_",today,".xlsx"))
write.xlsx(final_melted_analysis_ki, paste0("analysis/KI_common_analysis_final_",today,".xlsx"))
write.xlsx(final_melted_analysis_ki_ar, paste0("analysis/KI_common_analysis_final_ar",today,".xlsx"))

# Archived code

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
  