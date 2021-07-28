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
data.cholera.hh.filename <- "data/WANTS_Cholera_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-46-21.xlsx"
data.cholera.ki.filename <- "data/WASH_Cholera_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-39-21.xlsx"
data.common.hh.filename <- "data/WANTS_Common_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-53-27.xlsx"
data.common.ki.filename <- "data/WASH_Common_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-38-11.xlsx"

kobo.cholera.hh.filename <- "data/Cholera_HH_tool.xlsx"
kobo.cholera.ki.filename <- "data/Cholera_KI_tool.xlsx"
kobo.common.hh.filename <- "data/Common_HH_tool.xlsx"
kobo.common.ki.filename <- "data/Common_KI_tool.xlsx"

filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

# helper functions 
source("R/utils.R")

################################################################################################
## 1. Load and consolidate datsets
################################################################################################
## 1.1. Load all raw datasets
tools <- c("cholera.hh", "cholera.ki", "common.hh", "common.ki")                # Make sure that the list of tools here match the filenames above
for (t in tools) {
  assign(paste0("data_", gsub("\\.", "_", t)), read.xlsx(paste0("data.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("tool_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("choices_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get, sheet = "choices") %>% mutate_all(as.character))
}
pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

tool_hh <- bind_rows(tool_cholera_hh, tool_common_hh) %>% filter(!duplicated(name))
choices_hh <- bind_rows(choices_cholera_hh, choices_common_hh) %>% filter(!duplicated(paste0(list_name,name)))

tool_ki <- bind_rows(tool_cholera_ki, tool_common_ki) %>% filter(!duplicated(name))
choices_ki <- bind_rows(choices_cholera_ki, choices_common_ki) %>% filter(!duplicated(paste0(list_name,name)))

## 1.2. Streamline columns names in dataset and imported kobo tool and consolidate common and cholera datset for HH and KI

# harmonise.and.consolidate.datasets()                                          ## 1. harmonise and consolidate both HH and KI at the same time

harmonise.consolidate.hh()                                                      ## 2. harmonise and consolidate common and cholera HH 

harmonise.consolidate.ki()                                                      ## 3. harmonise and consolidate common and cholera HH

## Load cleaning logs




