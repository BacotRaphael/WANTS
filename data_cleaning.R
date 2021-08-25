# WANTS WASH monitoring tool - Data Cleaning Script
# REACH Yemen  
# 27/07/2021 - Raphael Bacot - raphael.bacot@reach-initiative.org 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, openxlsx, data.table, lubridate, randomcoloR)

## Create directories
dir.create("cleaning", showWarnings = F)

## Specify filenames and comment the file version you don't need

# data.cholera.hh.filename <- "data/WANTS_Cholera_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-46-21.xlsx"
# kobo.cholera.hh.filename <- "data/Cholera_HH_tool.xlsx"

# data.common.hh.filename <- "data/WANTS_Common_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-53-27.xlsx"
# kobo.common.hh.filename <- "data/Common_HH_tool.xlsx"

data.cholera.ki.filename <- "data/WASH_Cholera_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-39-21.xlsx"
kobo.cholera.ki.filename <- "data/Cholera_KI_tool.xlsx"
#
# data.common.ki.filename <- "data/WASH_Common_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-38-11.xlsx"
# kobo.common.ki.filename <- "data/Common_KI_tool.xlsx"

list.filenames <- grep("filename", ls(), value = T)
list.tools.specified <- grep("hh|ki", unique(gsub("data\\.|kobo\\.|\\.filename", "", list.filenames)), value = T)

filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

# helper functions
source("R/utils.R")

################################################################################################
## 1. Load and consolidate datasets
################################################################################################
## 1.1. Load all raw datasets
# tools <- c("cholera.hh", "cholera.ki", "common.hh", "common.ki")                # Make sure that the list of tools here match the filenames above
tools <- list.tools.specified
for (t in tools) {
  assign(paste0("data_", gsub("\\.", "_", t)), read.xlsx(paste0("data.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("tool_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("choices_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get, sheet = "choices") %>% mutate_all(as.character))
}

# create parameters to tell R which tool are loaded (common.ki, cholera.ki, common.hh, cholera.hh, hh and ki object that are either TRUE or FALSE for harmonisation later on)
which.tools.loaded()

pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

## 1.2. Streamline columns names in dataset and imported kobo tool and consolidate common and cholera datset for HH and KI
### Note: This section will align column names and choices and create a consolidate dataset (the first function creates data_hh, the second will create data_ki)

harmonise.consolidate.hh()                                                      ## 1. harmonise and consolidate common and cholera HH
harmonise.consolidate.ki()                                                      ## 2. harmonise and consolidate common and cholera HH
# harmonise.and.consolidate.datasets()                                          ##    original function that cleans HH and KI at once

## 1.3 Compare columns from different tools // uncomment the relevant one depending on the tool you have loaded [You need at least 2 hh tools from the same type]
combine.col.hh()
combine.col.ki()
combine.col.all()

## 1.4 Mutate binary as numerical variable
cols.num.ki <- c(colnames(data_ki)[grepl("\\.", colnames(data_ki))])
data_ki <- data_ki %>% mutate_at(vars(all_of(cols.num.ki)), as.numeric)

cols.num.hh <- c(colnames(data_hh)[grepl("\\.|infant|child|adult|elderly|hh_member_", colnames(data_hh))], "hh_number")
data_hh <- data_hh %>% mutate_at(vars(all_of(cols.num.hh)), as.numeric)

## 1.5 Consolidate common and HH updated tools [for data validation when exporting cleaning log]
## If there are changes to the tool, run 2 lines below [will allow flag potential questions that have different wording/choices but same column name]
# tool_hh <- bind_rows(tool_cholera_hh, tool_common_hh) %>% filter(!type %in% c("begin_group", "end_group", "image")) %>% group_by(name) %>% mutate(n=n()) %>% arrange(-n, name)
# tool_ki <- bind_rows(tool_cholera_ki, tool_common_ki)  %>% filter(!type %in% c("begin_group", "end_group", "image")) %>% group_by(name) %>% mutate(n=n()) %>% arrange(-n, name)

## If two different tools are loaded (hh common and hh cholera for example), the line below will combine them together
combine.tools()

################################################################################################
## 2. Cleaning
################################################################################################

## If all type of tools are loaded, precise which tool are you cleaning (uncomment the relevant one)
# tool.type <- "HH"
tool.type <- "KI"

## Rename data, tool and choices with corresponding version
if (tool.type=="HH"){
  data <- data_hh
  choices <- choices_hh
  tool <- tool_hh
  } else if (tool.type=="KI"){
    data <- data_ki 
    choices <- choices_ki
    tool <- tool_ki}

data <- data %>% 
  setnames(old = c("_uuid", "_index", "NO"), new = c("uuid", "index", "number"), skip_absent = T) %>%
  mutate(id=1:nrow(.)) %>% select(id, uuid, everything())

col.cl <- c("uuid", "agency", "enum_firstname", "enum_lastname", "area", "variable", "issue", "old_value", "new_value", "check_id", "fix", "checked_by")
col.cl.data <- c("uuid", "g_enum_agency", "g_enum_name", "g_enum_last_name", "g_sub_district", "variable", "issue", "old_value", "new_value", "check_id", "fix", "checked_by")
del.cl <- c("uuid", "agency", "enum_firstname", "enum_lastname", "area", "issue")
del.cl.data <- c("uuid", "g_enum_agency", "g_enum_name", "g_enum_last_name", "g_sub_district", "issue")

cleaning.log <- initialise.cleaning.log()
cleaning.log.internal <- initialise.cleaning.log()
deletion.log <- initialise.deletion.log()

################################################################################################
## Common checks
################################################################################################

## 2.0. Duplicate check
check_duplicates <- data %>% group_by(uuid) %>% mutate(n=n()) %>%
  mutate(flag = ifelse(n>1,T,F), issue = ifelse(flag==T,"Duplicate survey.","")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_duplicates %>% filter(flag)))==0){print("No duplicate surveys. The dataset seems clean.")} else {
  print(paste0(s," duplicate surveys detected. To be checked."))}

duplicate_surveys <- check_duplicates %>% filter(flag)
view(duplicate_surveys)                                                         ## Carefully inspect duplicates and make sure the latest version is the one to keep

kept_duplicate_surveys <- duplicate_surveys %>% group_by(uuid) %>%              ## Keep the surveys that were closed the latest
  filter(end == max(end)) %>% ungroup

view(kept_duplicate_surveys)                                                    ## Carefully inspect duplicates and make sure the latest version is the one to keep

## Write in the vector below the ids (not UUIDs!) that you want to delete.
id.to.delete <- c("")
# id.to.delete <- c("174", "172")
id.to.delete <- c(id.to.delete, duplicate_surveys$id[!duplicate_surveys$id %in% kept_duplicate_surveys$id])

if (!is.na(id.to.delete) & sum(!id.to.delete %in% duplicate_surveys$id)>0) stop("The id entered are not corresponding to the duplicates. Do it again!")

deletion.log <- deletion.log %>%                                                ## add to deletion log
  bind_rows(data %>% filter(id %in% id.to.delete) %>% 
              mutate(issue="Duplicate survey") %>%
              setnames(old=col.cl.data, new=col.cl, skip_absent = T) %>%
              select(any_of(del.cl))) 

data <- data %>% filter(!id %in% id.to.delete)                                  ## Keep only the relevant surveys

## If you still have duplicate surveys which you have doubt about, add it to the cleaning log by uncommenting and running the lines below:
# add.to.cleaning.log(checks = check_duplicates %>% filter(id %in% duplicate_surveys$id[!duplicate_surveys$id %in% id.to.delete]), 
#                     check_id = "0", question.names = c("uuid"))

## 2.1. Matching pcodes 
## A. Sub-district Matching
## 2.1.1. Perfect Matching of the district name with arabic and english name
check_pcode_sub <- data %>% select(col.cl.data[1:4], g_governorate,g_district, g_sub_district) %>%
  mutate(flag=ifelse(!g_sub_district %in% pcodes$admin3Pcode, T, F),
         issue=ifelse(flag,"The value entered is not a valid Pcode.","")) %>%
  left_join(pcodes %>% select(admin3Pcode, admin3RefName_ar) %>% filter(!duplicated(admin3Pcode)), by = c("g_sub_district"="admin3RefName_ar")) %>%
  mutate(admin3Pcode = ifelse(is.na(admin3Pcode) & g_sub_district %in% pcodes$admin3Pcode, g_sub_district, admin3Pcode)) %>%
  left_join(pcodes %>% select(admin3Pcode, admin3RefName_en) %>% filter(!duplicated(admin3Pcode)) %>%
              setNames(paste0(colnames(.), "_match_en")), by = c("g_sub_district"="admin3RefName_en_match_en")) %>%
  mutate(admin3Pcode = ifelse(is.na(admin3Pcode) & substr(admin3Pcode_match_en, 1, 4) == g_governorate, admin3Pcode_match_en, admin3Pcode)) %>% select(-admin3Pcode_match_en)

## 2.1.2. Partial matching of districtname in arabic
check_pcode_sub_2 <- check_pcode_sub %>% filter(is.na(admin3Pcode)) %>%
  mutate(admin3Pcode_match = str_replace_all(g_sub_district, setNames(pcodes$admin3Pcode,pcodes$admin3RefName_ar)),
         admin3Pcode_match = gsub("[\u0621-\u064A]+", "", admin3Pcode), 
         admin3Pcode_match = ifelse(g_governorate!= substr(str_trim(admin3Pcode_match), 1, 4), "", admin3Pcode_match),
         admin3Pcode = ifelse(is.na(admin3Pcode) & !is.na(admin3Pcode_match), admin3Pcode_match, admin3Pcode),
         admin3Pcode_final = "")

## 2.1.3. Export the remaining entries that have no match to be matched manually
check_pcode_sub_nomatch <- check_pcode_sub_2 %>% filter(is.na(admin3Pcode))
save.pcode.followup(check_pcode_sub_nomatch %>% select(-admin3Pcode) %>% relocate(g_district, .after ="admin3Pcode_final"),
                    pcode.table = pcodes,
                    paste0("cleaning/unmatched_pcodes_sub_", tool.type, "_", today, ".xlsx"))
browseURL(paste0("cleaning/unmatched_pcodes_sub_", tool.type, "_", today, ".xlsx"))

## Manually update the Pcode using the second sheet with the pcode list (usually, partial matching of arabic names with ctr + F works)
## Rename the updated file with _updated at the end (make sure that the filename belows matches your saved updated file)
# sub.pcode.followup.file.updated <- "cleaning/unmatched_pcodes_sub_HH_2021-08-01_updated.xlsx"
sub.pcode.followup.file.updated <- "cleaning/unmatched_pcodes_sub_KI_2021-08-01_updated.xlsx"

unmatched_pcodes_sub_updated <- read.xlsx(sub.pcode.followup.file.updated) %>%
  mutate(admin3Pcode = ifelse(!is.na(admin3Pcode_final), admin3Pcode_final, "")) 

check_pcode_sub_final <- bind_rows(check_pcode_sub %>% filter(!is.na(admin3Pcode)), 
                               check_pcode_sub_2 %>% filter(!is.na(admin3Pcode)), 
                               unmatched_pcodes_sub_updated) %>% select(-admin3Pcode_final, -admin3Pcode_match) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T) %>% mutate(g_sub_district=area)

## Save changes to cleaning log
## 1. Internal cleaning log for Pcodes for which we found a match without issue
internal_pcode_sub_log <- cleaning.log.new.entries(check_pcode_sub_final %>% filter(!admin3Pcode %in% c(NA,"")),
                                               check_id = "1", question.names = "g_sub_district", new.value = "admin3Pcode")
cleaning.log.internal <- cleaning.log.internal %>% bind_rows(internal_pcode_sub_log)

## 2. External cleaning log when no match has been found
add.to.cleaning.log(check_pcode_sub_final %>% filter(admin3Pcode %in% (c(NA,""))),
                    check_id = "1", question.names = "g_sub_district", new.value = "admin3Pcode")

## Apply changes for all matches found 
for (r in seq_along(1:nrow(internal_pcode_sub_log))){
  data[data$uuid==internal_pcode_sub_log[r, "uuid"], "g_sub_district"] <- internal_pcode_sub_log[r, "new_value"]
}

## District Matching
## 2.1.0. Matching district from sub-district name when possible
check_pcode_0 <- data %>% select(col.cl.data[1:4], g_governorate, g_district, g_sub_district) %>%
  mutate(flag=ifelse(!g_district %in% pcodes$admin2Pcode, T, F),
         issue=ifelse(flag,"The value entered is not a valid Pcode.",""),
         admin2Pcode=ifelse(str_detect(g_sub_district, g_governorate), substr(g_sub_district, 1, 6), NA),
         issue=ifelse(str_detect(g_sub_district, "YE") & is.na(admin2Pcode), "The sub-district Pcode doesn't match with the governorate entered.", issue))

## 2.1.1. Perfect Matching of the district name with arabic and english name
check_pcode <- check_pcode_0 %>%
  left_join(pcodes %>% select(admin2Pcode, admin2Name_ar) %>% filter(!duplicated(admin2Pcode)) %>%
              setNames(paste0(colnames(.), "_match_ar")), by = c("g_district"="admin2Name_ar_match_ar")) %>%
  mutate(admin2Pcode = ifelse(is.na(admin2Pcode) & str_detect(admin2Pcode_match_ar, g_governorate), 
                              admin2Pcode_match_ar, admin2Pcode)) %>% select(-admin2Pcode_match_ar) %>%
           left_join(pcodes %>% select(admin2Pcode, admin2Name_en) %>% filter(!duplicated(admin2Pcode)) %>%
              setNames(paste0(colnames(.), "_match_en")), by = c("g_district"="admin2Name_en_match_en")) %>%
  mutate(admin2Pcode = ifelse(is.na(admin2Pcode) & str_detect(admin2Pcode_match_en, g_governorate), admin2Pcode_match_en, admin2Pcode)) %>% 
  select(-admin2Pcode_match_en)

## 2.1.2. Partial matching of district name in arabic
check_pcode_2 <- check_pcode %>% filter(is.na(admin2Pcode)) %>%
  mutate(admin2Pcode_match = str_replace_all(g_district, setNames(pcodes$admin2Pcode,pcodes$admin2Name_ar)),
         admin2Pcode_match = str_trim(gsub("[\u0621-\u064A]+", "", admin2Pcode_match)), 
         admin2Pcode_match = ifelse(!str_detect(str_trim(admin2Pcode_match), g_governorate), "", admin2Pcode_match),
         admin2Pcode = ifelse(is.na(admin2Pcode) & !is.na(admin2Pcode_match), admin2Pcode_match, admin2Pcode),
         admin2Pcode_final = "")

## 2.1.3. Export the remaining entries that have no match to be matched manually
check_pcode_nomatch <- check_pcode_2 %>% filter(admin2Pcode %in% c("",NA, "NA")) %>% mutate_all(as.character)
save.pcode.followup(check_pcode_nomatch %>% select(-admin2Pcode),
                    pcode.table = pcodes %>% filter(!duplicated(admin2Pcode)) %>% select(-matches("admin3")),
                    paste0("cleaning/unmatched_pcodes_", tool.type, "_", today, ".xlsx"),
                    all.matches = check_pcode)
browseURL(paste0("cleaning/unmatched_pcodes_", tool.type, "_", today, ".xlsx"))

## Manually update the Pcode using the second sheet with the pcode list (usually, partial matching of arabic names with ctr + F works)
## Rename the updated file with _updated at the end (make sure that the filename belows matches your saved updated file)
# pcode.followup.file.updated <- "cleaning/unmatched_pcodes_HH_2021-08-01_updated.xlsx"
# pcode.followup.file.updated <- "cleaning/unmatched_pcodes_KI_2021-08-01_updated.xlsx"
pcode.followup.file.updated <- "cleaning/unmatched_pcodes_KI_2021-08-03.xlsx"

unmatched_pcodes_updated <- read.xlsx(pcode.followup.file.updated) %>%
  mutate(admin2Pcode = ifelse(!is.na(admin2Pcode_final), admin2Pcode_final, ""))

check_pcode_final <- bind_rows(check_pcode %>% filter(!admin2Pcode %in% c(NA,"","NA")), 
                               check_pcode_2 %>% filter(!admin2Pcode %in% c(NA,"","NA")), 
                               unmatched_pcodes_updated %>% mutate(admin2Pcode_match=as.character(admin2Pcode_match))) %>% select(-admin2Pcode_final, -admin2Pcode_match) %>%
  left_join(data %>% select(uuid, g_sub_district), by="uuid") %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)

## Save changes to cleaning log
## 1. Internal cleaning log for Pcodes for which we found a match without issue
internal_pcode_log <- cleaning.log.new.entries(check_pcode_final %>% filter(!is.na(admin2Pcode)),
                                                 check_id = "1", question.names = "g_district", new.value = "admin2Pcode")
cleaning.log.internal <- cleaning.log.internal %>% bind_rows(internal_pcode_log)

## 2. External cleaning log when no match has been found
add.to.cleaning.log(check_pcode_final %>% filter(is.na(admin2Pcode)),
                    check_id = "1", question.names = "g_district", new.value = "admin2Pcode")

## Apply changes for all matches found 
for (r in seq_along(1:nrow(internal_pcode_log))){
  data[data$uuid==internal_pcode_log[r, "uuid"], "g_district"] <- internal_pcode_log[r, "new_value"]
}

## 2.2. Checking agency name (TBD) For now manual:
## 2.1 Exporting file with names non matching
list.agency.tool <- choices %>% filter(list_name == "enum_agency") %>% pull(name)
check_agency <- data %>% select(col.cl.data[1:5], "g_enum_agency_other") %>% 
  mutate(flag=ifelse(!g_enum_agency %in% list.agency.tool[!list.agency.tool %in% "other"], T, F), issue = "The agency name entered is not part of the tool's choices.")
log_agency <-  check_agency %>% filter(flag) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T) %>% mutate(g_enum_agency=agency)
log_agency <- cleaning.log.new.entries(log_agency, check_id = "2", question.names = c("g_enum_agency", "g_enum_agency_other"))
save.follow.up.requests(log_agency, choices = choices, tool = tool, filename.out = paste0("cleaning/agency_log_", tool.type, "_", today,".xlsx"))
browseURL(paste0("cleaning/agency_log_", tool.type, "_", today,".xlsx"))

## Manually update the value for both agency and agency_other entry, and then rename of the file with "_updated" at the end
agency.log.file.updated <-  "cleaning/agency_log_HH_2021-08-01_updated.xlsx"
# agency.log.file.updated <-  "cleaning/agency_log_KI_2021-08-01_updated.xlsx"

agency_log_updated <- read.xlsx(agency.log.file.updated)

## Add to cleaning log
agency_log_internal <- agency_log_updated %>% filter(!new_value %in% c("",NA))
cleaning.log.internal <- cleaning.log.internal %>% bind_rows(agency_log_internal)
agency_log_external <- agency_log_updated %>% filter(new_value %in% c("",NA))
cleaning.log <- cleaning.log %>% bind_rows(agency_log_external)

## Apply changes for matches done manually
for (r in seq_along(1:nrow(agency_log_internal))){
  if (nrow(agency_log_internal)>0){
    var <- agency_log_internal[r, "variable"]
    data[data$uuid==agency_log_internal[r, "uuid"], var] <- agency_log_internal[r, "new_value"]
  }
}

## 2.2. Shortest path check 
C <- 2.5  # parameter to calibrate to determine tool specific NA threshold starting which surveys will be flagged [IQR rule] 
check_shortest_path <- data %>% mutate(CountNa=rowSums(is.na(.))) %>% group_by(tool) %>%
  mutate(NA_threshold=quantile(CountNa, 0.75)+C*(quantile(CountNa, 0.75)-quantile(CountNa, 0.25)),
         flag=ifelse(CountNa > NA_threshold, T, F)) %>% ungroup %>%
  mutate(issue=ifelse(flag, "The majority of entries are NAs", ""), check_id = "2", old_value = CountNa, 
         new_value="", fix="Checked with partner", checked_by="ON", variable = "Number of NA entries") %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_shortest_path %>% filter(flag)))==0){print("No enumerators seems to have taken the shortest path.")} else {
  print(paste0(s, " surveys have a very high number of NAs entries. To be checked."))}
cleaning.log <- cleaning.log %>% bind_rows(check_shortest_path %>% filter(flag) %>% select(all_of(col.cl)) %>% mutate_all(as.character))

## 2.3. Numerical outliers => ## To be done, separate low and high outliers in the function detect.outliers
col.num.all <- bind_rows(tool_cholera_hh, tool_cholera_ki, tool_common_hh, tool_common_ki) %>% filter(type=="integer") %>% pull(name)
method <- "iqr-log"

check_outliers <- data %>% select(uuid, any_of(col.num.all)) %>%
  detect.outliers(., method=method, n.sd=3) %>%                                 # n.sd will calibrate sensitivity of outlier detection. / see utils for other methods
  left_join(data %>% select(any_of(col.cl.data)), by="uuid") %>%
  mutate(check_id="3", new_value="", fix="Checked with partner", checked_by="ON") %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T) %>% dplyr::select(all_of(col.cl)) %>% mutate_all(as.character)
if((s <- nrow(check_outliers)) > 0){print(paste0("There are ", s, " numerical outliers detected using ", method, " method for the following variables"))
  print(check_outliers$variable %>% unique)} else {print("No numerical outliers detected.")}
cleaning.log <- cleaning.log %>% bind_rows(check_outliers)                      # Add to the cleaning log

################################################################################################
## HH survey checks
################################################################################################

## HH.1. Issues accessing to water reported but no accessing issues were highlighted
check_water <- data %>% select(any_of(col.cl.data),c("g_sub_district", "access_issues", "what_access_issues")) %>% 
  mutate(flag = ifelse(((access_issues == "no") & grepl("none|few", what_access_issues)),T,F),
         issue = ifelse(flag, "The household reported issues accessing to water but no accessing issues were highlighted.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent=T)
if ((s<-nrow(check_water %>% filter(flag))==0)){print("No issues related to access to water. The dataset seems clean.")} else {
  print(paste0(s,"Issues related to access to water detected. To be checked."))}
add.to.cleaning.log(checks = check_water, check_id = "HH.1", question.names = c("access_issues", "what_access_issues"))

## HH.2. Check if people who reported not treating water listed reason for not treating
check_treatment <- data %>% select(any_of(col.cl.data), c("treat_drinking_water", "treatment_reason")) %>% 
  mutate(flag = ifelse(((treat_drinking_water == "never") & (treatment_reason %in% c("", NA))),T,F),
         issue = ifelse(flag, "The household reported not treating drinking water, but did not provide a reason why.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent=T)
if ((s<-nrow(check_treatment %>% filter(flag)))==0){print("No issues realted to access to water treatment. The dataset seems clean.")} else {
  print(paste0(s,"Issues related to water treatment detected. To be checked."))}
add.to.cleaning.log(checks = check_treatment, check_id = "HH.2", question.names = c("treat_drinking_water", "treatment_reason"))

## HH.3. Check if people reported didn't have any issues accessing soap but soap was not available in the community in the past 30 days
check_soap <- data %>% select(any_of(col.cl.data), c("access_soap_issues", "what_access_soap_issues", "difficulty_access2")) %>% 
  mutate(flag = ifelse(((access_soap_issues == "no") & (what_access_soap_issues %in% c("too_expensive", "difficulty_reach") |
                                                          (grepl("not_accessible|not_affordable", difficulty_access2)))),T,F),
         issue = ifelse(flag, "The household reported not having issues accessing soap but soap was not accessible or affordable at market.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent=T)
if ((s<-nrow(check_soap %>% filter(flag)))==0){print("No issues realted to availability of soap. The dataset seems clean.")} else {
  print(paste0(s," issues realted to availability of soap detected. To be checked."))}
add.to.cleaning.log(checks = check_soap, check_id = "HH.3", question.names = c("access_soap_issues", "what_access_soap_issues","difficulty_access2"))
cleaning.log <- cleaning.log %>% filter(!(check_id=="HH.3" & is.na(old_value)))    # Clean the entries for survey from tool where difficulty access was not asked.

## HH.4. Check if people reported sharing a latrine but do not report how many people they share it with.
check_latrines <- data %>% select(any_of(col.cl.data), c("share_facility", "hh_share_facility")) %>% 
  mutate(flag = ifelse((share_facility == "Yes" & hh_share_facility %in% c(NA,"")),T,F),
         issue = ifelse(flag, "The household reported sharing sanitation facilities but did not indicate with how many households.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_latrines %>% filter(flag)))==0){print("No issues related with shared sanitation facilitites. The dataset seems clean.")} else {
  print(paste0(s,"Issues related with shared sanitation facilities detected. To be checked."))}
add.to.cleaning.log(checks = check_latrines, check_id = "HH.4", question.names = c("share_facility", "hh_share_facility"))

## HH.5. Check if people reported waste and trash frequently visible but garbage is supposed to be collected frequently [Common HH only]
### WARNING => Check left aside for now and garbage_collect not used for analysis as question formulation is ambiguous and not streamlined across tools
### Cholera HH: garbage_collect = S4. Has the household’s garbage been regularly collected in the last 30 days? (yes no dnk)
### Cholera HH: garbage_collect = S5.1 How frequently is garbage collected from your household? (every day, once a week, month, etc…)
# check_garbage <- data %>% select(any_of(col.cl.data), c("traces1", "garbage_collect")) %>% 
#   mutate(flag = ifelse((traces1 == "frequently" & (garbage_collect %in% c("once_week","every"))),T,F),
#          issue = ifelse(flag, "The household reported the trash to be visible in the street but waste collection should happen frequently", "")) %>%
#   setnames(old=col.cl.data, new=col.cl, skip_absent = T)
# if ((s<-nrow(check_garbage %>% filter(flag)))==0){print("No issues related to waste and trash management. The dataset seems clean.")} else {
#   print(paste0(s," issues related to waste and trash management detected. Check later."))}
# add.to.cleaning.log(checks = check_garbage, check_id = "HH.5", question.names = c("traces1", "garbage_collect"))

################################################################################################
## KI survey checks
################################################################################################

## KI.1. No issues related with water access were reported but but few or no members of the household have enough water for drinking, cooking, bathing and washing
check_water_needs <- data %>% select(any_of(col.cl.data), c("w_waterneeds", "w_wateraccess")) %>% 
  mutate(flag = ifelse(((w_wateraccess == "no") & (w_waterneeds %in% c("none","few"))),T,F),
         issue = ifelse(flag, "", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_water_needs %>% filter(flag)))==0){print("No issues related with access to water detected. The dataset seems clean.")} else {
  print(paste0(s," issues related with access to water have been detected. Check later"))}
add.to.cleaning.log(checks = check_water_needs, check_id = "KI.1", question.names = c("w_waterneeds", "w_wateraccess"))

## KI.1.2. No specific issues related with water access were reported but few or no members of the household have enough water for drinking, cooking, bathing and washing
## dropped as normally w_wateraccess = no means that w_accessproblem is NA.
# check_water_needs_2 <- data %>% select(any_of(col.cl.data), c("w_waterneeds", "w_accessproblem")) %>% 
#   mutate(flag = ifelse((is.na(w_accessproblem) & (w_waterneeds  %in% c("none", "few", "half"))),T,F),
#          issue = ifelse(flag, "The community KI reported a lack of access to water but no access constraint were highlighted", "")) %>%
#   setnames(old=col.cl.data, new=col.cl, skip_absent = T)
# if ((s<-nrow(check_water_needs_2 %>% filter(flag)))==0){print("No issues related to access to water. The dataset seems clean.")} else {
#   print(paste0(s," issues related with access to water have been detected. Check later."))}
# add.to.cleaning.log(checks = check_water_needs_2, check_id = "KI.1.2.", question.names = c("w_waterneeds", "w_accessproblem"))

## KI.2. KI reports not having no access constraint to soap while reporting that none or few members of the community had enough soap in the past 30 days 
check_soap <- data %>% select(any_of(col.cl.data), c("h_have_soap", "h_soapaccess")) %>% 
  mutate(flag = ifelse(((h_soapaccess == "no") & (h_have_soap %in% c("none", "few"))),T,F),
         issue = ifelse(flag, "The community KI reported households having issues with accessing soap but no access constraints were reported", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_soap %>% filter(flag)))==0){print("No issues related to soap access detected. The dataset seems clean.")} else {
  print(paste0(s," issues related to soap access have been detected. Check later"))}
add.to.cleaning.log(checks = check_soap, check_id = "KI.2", question.names = c("h_have_soap", "h_soapaccess"))

## KI.2.2. Issues with accessing soap reported but no type of problem specified in the follow-up question 
## dropped as normally h_soapaccess = no means that h_soap_problem is NA. 
# check_soap_needs_2 <- data %>% select(any_of(col.cl.data), c("h_soapaccess", "h_soap_problem")) %>% 
#   mutate(flag = ifelse((is.na(h_soap_problem) & (h_soapaccess == "yes")),T,F),
#          issue = ifelse(flag, "The community KI reported a lack of access to soap but no specific access constraint were highlighted.", "")) %>%
#   setnames(old=col.cl.data, new=col.cl, skip_absent = T)
# if ((s<-nrow(check_soap_needs_2 %>% filter(flag)))==0){print("No issues related with access to soap. The dataset seems clean.")} else {
#   print(s, " issues related with access to soap detected. Check later.")}
# add.to.cleaning.log(checks = check_soap_needs_2, check_id = "KI.2.2", question.names = c("h_soapaccess", "h_soap_problem"))

## KI.3. Check if people reported having enough soap in the community but soap wasn't accessible during the past 30-days [price question]
check_soap_price <- data %>% select(any_of(col.cl.data), c("h_soapaccess", "h_barsoap")) %>% 
  mutate(flag = ifelse(((h_soapaccess %in% c("half","everyone")) & (h_barsoap == "not_accessible")),T,F),
         issue = ifelse(flag, "The community KI reported the majority of the households having soap but soap wasn't available ni the past 30-days.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_soap_price %>% filter(flag)))==0){print("No issues related to soap price and accessibility detected. The dataset seems clean.")} else {
  print(paste0(s," issues related to soap price and accessibility have been detected. Check later"))}
add.to.cleaning.log(checks = check_soap_price, check_id = "KI.3", question.names = c("h_soapaccess", "h_barsoap"))

################################################################################
## Exporting cleaning logs
################################################################################

## 1. Save main cleaning log [hh or ki depending on tool.type]
cleaning.log <- cleaning.log %>% mutate(comment="", .after="new_value") %>%
  left_join(tool_hh %>% select(name, `label::english`, `label::arabic`) %>%
              setNames(paste0(colnames(.), "_hh")), by = c("variable"="name_hh")) %>%
  left_join(tool_ki %>% select(name, `label::english`, `label::arabic`) %>%
              setNames(paste0(colnames(.), "_ki")), by = c("variable"="name_ki"))
if (tool.type == "HH"){cleaning.log <- cleaning.log %>% select(-matches("_ki"))} else if (tool.type == "KI") {cleaning.log <- cleaning.log %>% select(-matches("_hh"))}

tool.lower <- tolower(tool.type)
save.follow.up.requests(cleaning.log, 
                        choices = choices, 
                        tool = tool, 
                        paste0("./cleaning/WASH_WANTS_", tool.lower, "_cleaning log_",today,".xlsx"))
# browseURL(paste0("./cleaning/WASH_WANTS_", tool.lower, "_cleaning log_",today,".xlsx"))

## Save internal cleaning log
save.follow.up.requests(cleaning.log.internal, 
                        choices = choices, 
                        tool = tool, 
                        paste0("./cleaning/WASH_WANTS_", tool.lower, "_cleaning log_internal_",today,".xlsx"))
# browseURL(paste0("./cleaning/WASH_WANTS_", tool.lower, "_cleaning log_internal_",today,".xlsx"))


## 2. Split cleaning logs by organisation
dir.create("cleaning/partners", showWarnings = F)
dir.create("cleaning/partners/hh", showWarnings = F)
dir.create("cleaning/partners/ki", showWarnings = F)
dir.create("cleaning/partners feedback", showWarnings = F)
dir.create("cleaning/partners feedback/hh", showWarnings = F)
dir.create("cleaning/partners feedback/ki", showWarnings = F)

organisations <- cleaning.log$agency %>% unique
for (org in organisations){
  cleaning.log %>% filter(agency==org) %>% 
    save.follow.up.requests(choices = choices, tool = tool,
                            paste0("cleaning/partners/", tool.lower, "/cleaning_log_", tool.lower,"_out_", org,"_", today, ".xlsx"))
}

################################################################################
## Logical check - code structure to add a new check
################################################################################
check_name <- data %>% select(any_of(col.cl.data), c("col1", "col2")) %>% 
  mutate(flag = ifelse((condition),T,F),
         issue = ifelse(flag, "", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_name %>% filter(flag)))==0){print("No issues related to xx detected. The dataset seems clean.")} else {
  print(s," issues related to xxx have been detected. Check later")}
add.to.cleaning.log(checks = check_name, check_id = "#", question.names = c("col1", "col2"))

################################################################################
## Harmonisation and consolidation code [archived as moved to utils.R]

# ## 1.2.1. Clean headers in data
# data_common_hh <- data_common_hh %>% 
#   setNames(paste0(gsub("Comm_HH_", "", colnames(.)))) %>%                       ## Delete Comm_HH_ prefix from column names
#   setNames(paste0(gsub("/", "\\.", colnames(.))))                               ## Replace the choice separator "/" with "." to streamline headers to PBI dashboard
# data_cholera_hh <- data_cholera_hh %>%
#   setNames(paste0(gsub("Chol_HH_", "", colnames(.)))) %>%                       ## Delete Chol_HH_ prefix from column names
#   setNames(paste0(gsub("/", "\\.", colnames(.)))) 
# data_cholera_ki <- data_cholera_ki %>%
#   setNames(paste0(gsub("/", "\\.", colnames(.))))
# 
# ## 1.2.2. Clean variable names in Kobo tools
# tool_common_hh <- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name))
# tool_cholera_hh <- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name))
# 
# ## 1.2.3. Question W5.1 How does your household adapt to the lack of water? Recoding choices
# data_common_hh <- data_common_hh %>% 
#   rename(`adapt_lack.less_preferred_drinking`=`adapt_lack.surface_water_other`, # surface_water_other should be less_preferred_drinking
#          `adapt_lack.surface_drinking`=`adapt_lack.surface_water`,              # adapt_lack.surface_water should be adapt_lack.surface_drinking
#          `adapt_lack.less_preferred_other`=`adapt_lack.untreated`,              # untreated should be less_preferred_other
#          `adapt_lack.surface_other`=`adapt_lack.other_surface`) %>%             # other_surface should be surface_other
#   mutate(adapt_lack = gsub("surface_water_other","less_preferred_drinking",
#                            gsub("surface_water", "surface_drinking",
#                                 gsub("untreated", "less_preferred_other",
#                                      gsub("other_surface", "surface_other", adapt_lack)))))
# # data_common_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique  # check line - comment if all good
# choices_common_hh <- choices_common_hh %>% 
#   mutate(name = gsub("surface_water_other","less_preferred_drinking",
#                      gsub("surface_water", "surface_drinking",
#                           gsub("untreated", "less_preferred_other",
#                                gsub("other_surface", "surface_other", name)))))
# 
# data_cholera_hh <- data_cholera_hh %>%
#   rename(`adapt_lack.further_source`=`adapt_lack.further`,                      # further should be further_source
#          `adapt_lack.dangerous_source`=`adapt_lack.dangerous`) %>%              # dangerous should be dangerous_source
#   mutate(adapt_lack = gsub("further", "further_source",
#                            gsub("dangerous", "dangerous_source", adapt_lack)))
# # data_cholera_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique # check line - comment if all good
# choices_common_hh <- choices_common_hh %>%
#   mutate(name = gsub("further", "further_source",
#                      gsub("dangerous", "dangerous_source", name)))
# 
# ## 1.2.4. Renaming for the KI datasets !! ORDER DEPENDENT, Don't switch order of line 79 and 80 !!
# data_cholera_ki <- data_cholera_ki %>% 
#   rename(w_waterneeds=w_watersmell,                                             # Renaming wrongly named columns in data cholera KI tool (shifted)
#          w_watersmell=w_waterquality_what)                                      # Renaming wrongly named columns in data cholera KI tool (shifted)
# 
# ## Clean the tool accordingly
# tool_cholera_ki <- tool_cholera_ki %>%
#   mutate(name=gsub("w_watersmell", "w_waterneeds", name), name=gsub("w_waterquality_what", "w_watersmell", name))
# 
# ## 1.3. Consolidate Common and Cholera datasets together for KI and HHs respectively 
# data_ki <- data_cholera_ki %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_ki %>% mutate(tool="common")) %>%
#   mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
# data_hh <- data_cholera_hh %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_hh %>% mutate(tool="common")) %>%
#   mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
