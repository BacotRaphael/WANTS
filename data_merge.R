# WANTS WASH monitoring tool - Data Merge Script
# REACH Yemen - 
# District level datamerge EN/AR
# 07/07/2021 - Raphael Bacot - raphael.bacot@reach-initiative.org 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, openxlsx)


## Specifcy filenames
data.filename <- "data/data.xlsx"
kobo.filename <- "data/filename.xlsx"
filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

## Load datasets
data <- read.xlsx(data.filename)
tool <- read.xlsx(kobo.filename)
pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

## Match with pcodes 
data <- data %>% select(-any_of(c("chose"))) %>%
  left_join(pcodes %>% select(admin3Pcode, admin1Name_en, admin1Name_ar, admin2Name_en, admin2Name_ar, admin3Name_en, admin3Name_ar),
            by = c("col.name.sbd.code.data"="admin3Pcode"))

## Separate columns requiring no renaming / columns with select one only / columns with select multiples

## Speficy all columns to exclude from the factsheet output dataset
col.exclude <- c("")

## Specify all columns that need to be kept but for which no matching with labels is needed (date, district, codes, numbers)
col.no.rename <- c("admin1Name_en", "admin2Name_en", "admin3Name_en")
col.no.rename.ar <- c("admin1Name_ar", "admin2Name_ar", "admin3Name_ar") # to keep gov/district etc directly in arabi

## Specify all columns that need to be renamed from variable name to label (in english or arabic)
col.rename <- c("")   ## For the columns where each single entry will have a unique perfect match in the choice list from the tool
col.select.multiple <- c("") ## For the columns where each value has multiple choices separated by space

## Get rid of non wanted columns and split data between no_rename, rename and rename_select_multiple
data <- data %>% select(-any_of(col.exclude))

data_norename <- data %>% select(all_of(col.no.rename))
data_norename <- data %>% select(all_of(col.no.rename))

data_rename <- data %>% select(any_of(col.rename)) %>% select(-any_of(col.exclude))

## Match choice variable name with choice label from the kobo tool for all columns except multiple choices text column
## For english version
data_rename_en <- data_rename
data_rename_en[] <- choices$`label::english`[match(unlist(data_rename_en), choices$name)]

## For Arabic version
data_rename_ar <- data_rename 
data_rename_ar[] <- choices$`label::arabic`[match(unlist(data_rename_ar), choices$name)]

## Match choice variable name with choice label from the kobo tool for the multiple choices text column
## Reformat and add text questions Primary cooking space - safe cooking practices answers 
firstup <- function(x){substr(x, 1, 1) <- toupper(substring(x, 1, 1))
return(x)}

# English reformat
data_rename_select_multiple <- response %>%   
  select(col.select.multiple) %>%
  mutate_all(~gsub(" ,","," ,gsub("^, ", "", str_replace_all(., setNames(paste0(", ",choices$`label::english`), choices$name)))) %>% tolower %>% firstup(.))

# Arabic reformat 
data_rename_select_multiple_ar <- response %>%
  select(col.select.multiple) %>%
  mutate_all(~gsub(" ،", "،", gsub("^،", "", str_replace_all(., setNames(paste0("، ",choices$`label::arabic`), choices$name))))) %>%

## Step 3: merge into one dataset
data_merge <- cbind(data_rename_en, data_rename_select_multiple, data_norename)       
data_merge_ar <- cbind(data_rename_ar, data_rename_select_multiple_ar, data_norename_ar)

