# WANTS WASH monitoring tool - Data Cleaning Script
# REACH Yemen  
# 27/07/2021 - Raphael Bacot - raphael.bacot@reach-initiative.org 

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
today <- Sys.Date()

## Install/Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, openxlsx, data.table, lubridate)

##
## Create directories 
dir.create("cleaning", showWarnings = F)

## Specifcy filename
data.cholera.hh.filename <- "data/WANTS_Cholera_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-46-21.xlsx"
data.cholera.ki.filename <- "data/WASH_Cholera_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-39-21.xlsx"
data.common.hh.filename <- "data/WANTS_Common_HH_-_REACH_Yemen_-_latest_version_-_False_-_2021-07-06-08-53-27.xlsx"
data.common.ki.filename <- "data/WASH_Common_Key_Informant_Questionnaire_-_latest_version_-_False_-_2021-07-06-08-38-11.xlsx"

kobo.cholera.hh.filename <- "data/Cholera_HH_tool.xlsx"
kobo.cholera.ki.filename <- "data/Cholera_KI_tool.xlsx"
kobo.common.hh.filename <- "data/Common_HH_tool.xlsx"
kobo.common.ki.filename <- "data/Common_KI_tool.xlsx"

filename.pcode <- "data/yem_admin_ochayemen_20191002.xlsx"

################################################################################################
## 1. Load and consolidate datsets
################################################################################################
## 1.1. Load all datasets
tools <- c("cholera.hh", "cholera.ki", "common.hh", "common.ki")                # Make sure that the list of tools here match the filenames above
for (t in tools) {
  assign(paste0("data_", gsub("\\.", "_", t)), read.xlsx(paste0("data.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("tool_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get) %>% mutate_all(as.character))
  assign(paste0("choices_", gsub("\\.", "_", t)), read.xlsx(paste0("kobo.", t, ".filename") %>% get, sheet = "choices") %>% mutate_all(as.character))
}
pcodes <- read.xlsx(filename.pcode, sheet = "admin3")

## 1.2. Streamline columns names in dataset and imported kobo tool
## 1.2.1. Clean headers in data
data_common_hh <- data_common_hh %>% 
  setNames(paste0(gsub("Comm_HH_", "", colnames(.)))) %>%                       ## Delete Comm_HH_ prefix from column names
  setNames(paste0(gsub("/", "\\.", colnames(.))))                               ## Replace the choice separator "/" with "." to streamline headers to PBI dashboard
data_cholera_hh <- data_cholera_hh %>%
  setNames(paste0(gsub("Chol_HH_", "", colnames(.)))) %>%                       ## Delete Chol_HH_ prefix from column names
  setNames(paste0(gsub("/", "\\.", colnames(.)))) 
data_cholera_ki <- data_cholera_ki %>%
  setNames(paste0(gsub("/", "\\.", colnames(.))))

## 1.2.2. Clean variable names in Kobo tools
tool_common_hh <- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name))
tool_cholera_hh <- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name))

## 1.2.3. Question W5.1 How does your household adapt to the lack of water? Recoding choices
data_common_hh <- data_common_hh %>% 
  rename(`adapt_lack.less_preferred_drinking`=`adapt_lack.surface_water_other`, # surface_water_other should be less_preferred_drinking
         `adapt_lack.surface_drinking`=`adapt_lack.surface_water`,              # adapt_lack.surface_water should be adapt_lack.surface_drinking
         `adapt_lack.less_preferred_other`=`adapt_lack.untreated`,              # untreated should be less_preferred_other
         `adapt_lack.surface_other`=`adapt_lack.other_surface`) %>%             # other_surface should be surface_other
  mutate(adapt_lack = gsub("surfasurface_waterce_water_other","less_preferred_drinking",
                           gsub("surface_water", "surface_drinking",
                                gsub("untreated", "less_preferred_other",
                                     gsub("other_surface", "surface_other", adapt_lack)))))
# data_common_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique  # check line - comment if all good

data_cholera_hh <- data_cholera_hh %>%
  rename(`adapt_lack.further_source`=`adapt_lack.further`,                      # further should be further_source
         `adapt_lack.dangerous_source`=`adapt_lack.dangerous`) %>%              # dangerous should be dangerous_source
  mutate(adapt_lack = gsub("further", "further_source",
                           gsub("dangerous", "dangerous_source", adapt_lack)))
# data_cholera_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique # check line - comment if all good

## 1.2.4. Renaming for the KI datasets !! ORDER DEPENDENT, Don't switch order of line 79 and 80 !!
data_cholera_ki <- data_cholera_ki %>% 
  rename(w_waterneeds=w_watersmell,                                             # Renaming wrongly named columns in data cholera KI tool (shifted)
         w_watersmell=w_waterquality_what)                                      # Renaming wrongly named columns in data cholera KI tool (shifted)

## Clean the tool accordingly
tool_cholera_ki <- tool_cholera_ki %>%
  mutate(name=gsub("w_watersmell", "w_waterneeds", name), name=gsub("w_waterquality_what", "w_watersmell", name))

## 1.2.5. Compare columns from different tools
col_cholera_hh = data.frame(id=colnames(data_cholera_hh), col_cholera_hh=colnames(data_cholera_hh))
col_cholera_ki = data.frame(id=colnames(data_cholera_ki), col_cholera_ki=colnames(data_cholera_ki))
col_common_hh = data.frame(id=colnames(data_common_hh), col_common_hh=colnames(data_common_hh))
col_common_ki = data.frame(id=colnames(data_common_ki), col_common_ki=colnames(data_common_ki))

col.all <- col_cholera_hh %>% full_join(col_cholera_ki, by = "id") %>% full_join(col_common_hh, by="id") %>% full_join(col_common_ki, by="id") %>% rename(question_header=id)

## 1.3. Consolidate Common and Cholera datasets together for KI and HHs respectively 
data_ki <- data_cholera_ki %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_ki %>% mutate(tool="common")) %>%
  mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
data_hh <- data_cholera_hh %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_hh %>% mutate(tool="common")) %>%
  mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
# rm(data_cholera_hh, data_cholera_ki, data_common_hh, data_common_ki)

## Mutate binary as numerical variable
cols.num.ki <- c(colnames(data_ki)[grepl("\\.", colnames(data_ki))])
data_ki <- data_ki %>% mutate_at(vars(all_of(cols.num.ki)), as.numeric)
cols.num.hh <- c(colnames(data_hh)[grepl("\\.|infant|child|adult|elderly|hh_member_", colnames(data_hh))], "hh_number")
data_hh <- data_hh %>% mutate_at(vars(all_of(cols.num.hh)), as.numeric)

################################################################################################
## 2. Cleaning
################################################################################################

source("R/utils.R")

## Which tool are you cleaning (uncomment the relevant one)
# tool.type <- "HH"
tool.type <- "KI"
if (tool.type=="HH"){data <- data_hh} else {data <- data_ki}
data <- data %>% setnames(old = c("_uuid", "_index", "NO"), new = c("uuid", "index", "number"), skip_absent = T)

cleaning.log <- initialise.cleaning.log()
col.cl <- c("uuid", "agency", "enum_firstname", "enum_lastname", "area", "variable", "issue", "check_id", "old_value", "new_value", "fix", "checked_by")
col.cl.data <- c("uuid", "g_enum_agency", "g_enum_name", "g_enum_last_name", "g_sub_district", "variable", "issue", "check_id", "old_value", "new_value", "fix", "checked_by")

## 2.0. Add pcodes
## Match with pcodes 
metacol <- c("g_governorate","admin1Name_en","admin1Name_ar","g_district","admin2Name_en","admin2Name_ar","g_sub_district","admin3Name_en","admin3Name_ar")
data <- data %>% select(-any_of(c(""))) %>% 
  left_join(pcodes %>% select(admin3Pcode, any_of(metacol)), by = c("g_sub_district"="admin3Pcode")) %>%
  select(all_of(metacol), everything())

## 2.0. Duplicate check
check_duplicates <- data %>% group_by(uuid) %>% mutate(n=n()) %>%
  mutate(flag = ifelse(n>1,T,F), issue = "Duplicate survey.") %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_duplicates %>% filter(flag)))==0){print("No duplicate surveys. The dataset seems clean.")} else {
  print(paste0(s," duplicate surveys detected. To be checked."))}
add.to.cleaning.log(checks = check_duplicates, check_id = "0", question.names = c("uuid"))

################################################################################################
## HH survey checks
################################################################################################

## HH.1. Issues accessing to water reported but no accessing issues were highlighted
check_water <- data %>% select(any_of(col.cl.data),c("g_sub_district", "access_issues", "what_access_issues")) %>% 
  mutate(flag = ifelse(((access_issues == "no") & grepl("none|few", what_access_issues)),T,F),
         issue = ifelse(flag, "The household reported issues accessing to water but no accessing issues were highlighted.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent=T)
if (nrow(check_water %>% filter(flag))==0){print("No issues related to access to water. The dataset seems clean.")} else {
  print("Issues related to access to water detected. To be checked.")}
add.to.cleaning.log(checks = check_water, check_id = "HH.1", question.names = c("access_issues", "what_access_issues"))

## HH.2. Check if people who reported not treating water listed reason for not treating
check_treatment <- data %>% select(any_of(col.cl.data), c("treat_drinking_water", "treatment_reason")) %>% 
  mutate(flag = ifelse(((treat_drinking_water == "never") & (treatment_reason %in% c("", NA))),T,F),
         issue = ifelse(flag, "The household reported not treating drinking water, but did not provide a reason why.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent=T)
if (nrow(check_treatment %>% filter(flag))==0){print("No issues realted to access to water treatment. The dataset seems clean.")} else {
  print("Issues related to water treatment detected. To be checked.")}
add.to.cleaning.log(checks = check_treatment, check_id = "HH.2", question.names = c("treat_drinking_water", "treatment_reason"))

## HH.3. Check if people reported didn't have any issues accessing soap but soap was not available in the community in the past 30 days
check_soap <- data %>% select(any_of(col.cl.data), c("access_soap_issues", "what_access_soap_issues", "difficulty_access2")) %>% 
  mutate(flag = ifelse(((access_soap_issues == "no") & (what_access_soap_issues %in% c("too_expensive", "difficulty_reach") |
                                                          (grepl("not_accessible|not_affordable", difficulty_access2)))),T,F),
         issue = ifelse(flag, "The household reported not having issues accessing soap but soap was not accessible or affordable at market.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent=T)
if (nrow(check_soap %>% filter(flag))==0){print("No issues realted to availability of soap. The dataset seems clean.")} else {
  print("Issues realted to availability of soap detected. To be checked.")}
add.to.cleaning.log(checks = check_soap, check_id = "HH.3", question.names = c("access_soap_issues", "what_access_soap_issues","difficulty_access2"))
cleaning.log <- cleaning.log %>% filter(!(check_id=="HH.3" & is.na(old_value)))    # Clean the entries for survey from tool where difficulty access was not asked.

## HH.4. Check if people reported sharing a latrine but do not report how many people they share it with.
check_latrines <- data %>% select(any_of(col.cl.data), c("share_facility", "hh_share_facility")) %>% 
  mutate(flag = ifelse((share_facility == "Yes" & hh_share_facility %in% c(NA,"")),T,F),
         issue = ifelse(flag, "The household reported sharing sanitation facilities but did not indicate with how many households.", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if (nrow(check_latrines %>% filter(flag))==0){print("No issues related with shared sanitation facilitites. The dataset seems clean.")} else {
  print("Issues related with shared sanitation facilities detected. To be checked.")}
add.to.cleaning.log(checks = check_latrines, check_id = "HH.4", question.names = c("share_facility", "hh_share_facility"))

## HH.5. Check if people reported waste and trash frequently visible but garbage is supposed to be collected frequently [Common HH only]
check_garbage <- data %>% select(any_of(col.cl.data), c("traces1", "garbage_collect")) %>% 
  mutate(flag = ifelse((traces1 == "frequently" & (garbage_collect %in% c("once_week","every"))),T,F),
         issue = ifelse(flag, "The household reported the trash to be visible in the street but waste collection should happen frequently", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_garbage %>% filter(flag)))==0){print("No issues related to waste and trash management. The dataset seems clean.")} else {
  print(paste0(s," issues related to waste and trash management detected. Check later."))}
add.to.cleaning.log(checks = check_garbage, check_id = "HH.5", question.names = c("traces1", "garbage_collect"))

################################################################################################
## KI survey checks
################################################################################################

## KI.1. No issues related with water access were reported but but few or no members of the household have enough water for drinking, cooking, bathing and washing
check_water_needs <- data %>% select(any_of(col.cl.data), c("w_waterneeds", "w_wateraccess")) %>% 
  mutate(flag = ifelse(((w_wateraccess == "no") & (w_waterneeds %in% c("none","few"))),T,F),
         issue = ifelse(flag, "", "")) %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if ((s<-nrow(check_water_needs %>% filter(flag)))==0){print("No issues related to xx detected. The dataset seems clean.")} else {
  print(s," issues related to xxx have been detected. Check later")}
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
if ((s<-nrow(check_name %>% filter(flag)))==0){print("No issues related to soap access detected. The dataset seems clean.")} else {
  print(s," issues related to soap access have been detected. Check later")}
add.to.cleaning.log(checks = check_name, check_id = "KI.2", question.names = c("col1", "col2"))

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
  print(s," issues related to soap price and accessibility have been detected. Check later")}
add.to.cleaning.log(checks = check_soap_price, check_id = "KI.3", question.names = c("h_soapaccess", "h_barsoap"))

################################################################################################
## Common checks
################################################################################################

## 2.5. Shortest path check 
C <- 2.5  # parameter to calibrate to determine tool specific NA threshold starting which surveys will be flagged [IQR rule] 
check_shortest_path <- data %>% mutate(CountNa=rowSums(is.na(.))) %>% group_by(tool) %>%
  mutate(NA_threshold=quantile(CountNa, 0.75)+C*(quantile(CountNa, 0.75)-quantile(CountNa, 0.25)),
         flag=ifelse(CountNa > NA_threshold, T, F)) %>% ungroup %>%
  mutate(issue=ifelse(flag, "The majority of entries are NAs", ""), check_id = "5", old_value = CountNa, 
         new_value="", fix="Checked with partner", checked_by="ON", variable = "Number of NA entries") %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T)
if (nrow(check_shortest_path %>% filter(flag))==0){print("No enumerators seems to have taken the shortest path.")} else {
  print("Some surveys have a very high number of NAs entries. To be checked.")}
cleaning.log <- cleaning.log %>% bind_rows(check_shortest_path %>% filter(flag) %>% select(all_of(col.cl)) %>% mutate_all(as.character))

## 2.6. Numerical outliers
col.num.all <- bind_rows(tool_cholera_hh, tool_cholera_ki, tool_common_hh, tool_common_ki) %>% filter(type=="integer") %>% pull(name)
method <- "iqr-log"

check_outliers <- data %>% select(uuid, any_of(col.num.all)) %>%
  detect.outliers(., method=method, n.sd=3) %>%                                 # n.sd will calibrate sensitivity of outlier detection. / see utils for other methods
  left_join(data %>% select(any_of(col.cl.data)), by="uuid") %>%
  mutate(issue="", check_id="6", new_value="", fix="Checked with partner", checked_by="ON") %>%
  setnames(old=col.cl.data, new=col.cl, skip_absent = T) %>%
  dplyr::select(all_of(col.cl)) %>% mutate_all(as.character)

if((s <- nrow(check_outliers)) > 0){print(paste0("There are ", s, " numerical outliers detected using ", method, " method for the following variables"))
  print(col.num.all)} else {print("No numerical outliers detected.")}

# Add to the cleaning log
cleaning.log <- cleaning.log %>% bind_rows(check_outliers)  

## To finish => write cleaning log etc... => make it tool dependent, split if needed by partner etc...
write.xlsx(cleaning.log, paste0("./output/WASH_WANTS Common_cleaning log_",current_date,".xlsx"))
browseURL(paste0("./output/WASH_WANTS Common_cleaning log_",current_date,".xlsx"))
write.xlsx(data, paste0("./output/WASH_WANTS Common_cleandata_",current_date,".xlsx"))




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


