## Data harmonisation & consolidation 

which.tools.loaded <- function(){
  common.ki <<- exists("data_common_ki")
  cholera.ki <<- exists("data_cholera_ki")
  common.hh <<- exists("data_common_hh")
  cholera.hh <<- exists("data_cholera_hh")
  hh <<- common.hh & cholera.hh
  ki <<- common.ki & cholera.ki
  one.hh <<- common.hh | cholera.hh
  one.ki <<- common.ki | cholera.ki
}

harmonise.consolidate.ki <- function() {
  ## This function does:
  ## 1. Clean headers in KI data for cholera depending on what has been set up in the parameters at the beginning of the script
  ## 2. Recode some headers and their corresponding choices in the dataset and tool (survey) for Cholera if relevant
  ## 3. consolidate common and cholera KI tools if both files are imported
  # 
  # common.ki <- exists("data_common_ki")
  # cholera.ki <- exists("data_cholera_ki")
  
  if (!one.ki) stop("There is no key informant level data loaded to consolidate.")
  if (exists("data_ki")) stop("Key Informant data has already been streamlined and consolidated.")
  
  if (common.ki){data_common_ki <<- data_common_ki %>% mutate(tool="common", tool_cholera = 0, .after = "date_survey")}
  if (cholera.ki){
    ## 1. Clean headers in data
    data_cholera_ki <<- data_cholera_ki %>%
      setNames(paste0(gsub("/", "\\.", colnames(.))))
    print("The column headers of data_cholera_ki dataset have been streamlined: / speparators have been replaced by .")
    
    ## 2.1 Renaming for the KI datasets !! ORDER DEPENDENT, Don't switch order of line 79 and 80 !!
    data_cholera_ki <<- data_cholera_ki %>% 
      rename(w_waterneeds=w_watersmell,                                             # Renaming wrongly named columns in data cholera KI tool (shifted)
             w_watersmell=w_waterquality_what)                                      # Renaming wrongly named columns in data cholera KI tool (shifted)
    print("Typo in the kobo tool fixed. The column watersmell has been renamed waterneeds and the column w_waterquality_what has been renamed w_watersmell.")
    
    ## 2.2 Rename the tool accordingly
    tool_cholera_ki <<- tool_cholera_ki %>%
      mutate(name=gsub("w_watersmell", "w_waterneeds", name), name=gsub("w_waterquality_what", "w_watersmell", name))
    print("The survey dataframe for cholera_ki (tool_cholera_ki) has been updated to incorporate these changes.")
    
    ## 3. Add tool column
    data_cholera_ki <<- data_cholera_ki %>% mutate(tool="cholera", tool_cholera = 1, .after = "date_survey")
  }
  
  ## Consolidate Common and Cholera datasets together for KI depending on what has been specified
  if (common.ki & cholera.ki){
    data_ki <<- data_cholera_ki %>% bind_rows(data_common_ki)} else if (cholera.ki & !common.ki) {
      data_ki <<- data_cholera_ki} else if (!cholera.ki & common.ki) {
        data_ki <<- data_common_ki}
  print(paste0("The Key informant data from ", paste(unique(data_ki$tool), collapse=" and ")," tool has been consolidated in the dataframe data_ki"))  
}

harmonise.consolidate.hh <- function(){
  ## This function does:
  ## 1. Clean headers in HH data for common and/or cholera depending on what has been set up in the parameters at the beginning of the script
  ## 2. Recode some headers and their corresponding choices in the dataset and tool (choices and survey)
  ## 3. consolidate common and cholera HH tools if both files are imported
  
  if (!one.hh) stop("There is no household level data loaded to consolidate.")
  if (exists("data_hh")) stop("Key Informant data has already been streamlined and consolidated.")
  
  if (common.hh){
    data_common_hh <<- data_common_hh %>% 
      setNames(paste0(gsub("Comm_HH_", "", colnames(.)))) %>%                       ## Delete Comm_HH_ prefix from column names
      setNames(paste0(gsub("/", "\\.", colnames(.))))                               ## Replace the choice separator "/" with "." to streamline headers to PBI dashboard
    tool_common_hh <<- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name)) ## Rename tool accordingly
    print("The column headers of data_common_hh dataset have been streamlined:")
    print("/ separators have been replaced by . and Comm_HH has been removed from column headers.")
    
    ## 2.1. Common tool - Recoding choices of Question W5.1 How does your household adapt to the lack of water?
    data_common_hh <<- data_common_hh %>% 
      rename(`adapt_lack.less_preferred_drinking`=`adapt_lack.surface_water_other`, # surface_water_other should be less_preferred_drinking
             `adapt_lack.surface_drinking`=`adapt_lack.surface_water`,              # adapt_lack.surface_water should be adapt_lack.surface_drinking
             `adapt_lack.less_preferred_other`=`adapt_lack.untreated`,              # untreated should be less_preferred_other
             `adapt_lack.surface_other`=`adapt_lack.other_surface`) %>%             # other_surface should be surface_other
      mutate(adapt_lack = gsub("surface_water_other","less_preferred_drinking",
                               gsub("surface_water", "surface_drinking",
                                    gsub("untreated", "less_preferred_other",
                                         gsub("other_surface", "surface_other", adapt_lack)))))
    # data_common_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique  # check line - comment if all good
    print("The choices from data_common_hh have been renamed for the Question W5.1 How does your household adapt to the lack of water?")
    print("The choice surface_water_other has been renamed less_preferred_drinking; surface_water renamed surface_drinking, untreated renamed less_preferred_other, other_surface renamed surface_other.")
    print("The binary columns headers have been renamed accordingly and the parent text question entries updated.")
    
    ## 2.2. Rename tool accordingly
    choices_common_hh <<- choices_common_hh %>% 
      mutate(name = gsub("surface_water_other","less_preferred_drinking",
                         gsub("surface_water", "surface_drinking",
                              gsub("untreated", "less_preferred_other",
                                   gsub("other_surface", "surface_other", name)))))
    
    data_common_hh <<- data_common_hh %>% mutate(tool="common", 
                                                 tool_cholera=ifelse(tool=="cholera", 1, 
                                                                     ifelse(tool=="common", 0, NA)), .after = "date_survey")
    print("The choices dataframe for common_hh (choices_common_hh) has been updated to incorporate these changes.")
  }
    
  if (cholera.hh){
    data_cholera_hh <<- data_cholera_hh %>%
      setNames(paste0(gsub("Chol_HH_", "", colnames(.)))) %>%                       ## Delete Chol_HH_ prefix from column names
      setNames(paste0(gsub("/", "\\.", colnames(.))))                               ## replace / separator with .
    tool_cholera_hh <<- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name)) ## Rename tool accordingly
    print("The column headers of data_cholera_hh dataset have been streamlined:")
    print("/ separators have been replaced by . and Chol_HH has been removed from column headers.")
    
    ## 2.3 Cholera tool - Recoding choices of Question W5.1 How does your household adapt to the lack of water?
    data_cholera_hh <<- data_cholera_hh %>%
      select(where(~!all(is.na(.)))) %>%                                            # unselect empty columns in case mess happened
      rename(`adapt_lack.further_source`=`adapt_lack.further`,                      # further should be further_source
             `adapt_lack.dangerous_source`=`adapt_lack.dangerous`) %>%              # dangerous should be dangerous_source
      mutate(adapt_lack = gsub("further", "further_source",
                               gsub("dangerous", "dangerous_source", adapt_lack)))
    # data_cholera_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique # check line - comment if all good
    print("The choices from data_cholera_hh have been renamed for the Question W5.1 How does your household adapt to the lack of water?")
    print("The choice further has been renamed further_source; dangerous renamed dangerous_source.")
    print("The binary columns headers have been renamed accordingly and the parent text question entries updated.")
    
    choices_cholera_hh <<- choices_cholera_hh %>%                                   ## rename tool accordingly
      mutate(name = gsub("further", "further_source",
                         gsub("dangerous", "dangerous_source", name)))
    print("The choices dataframe for common_hh (choices_cholera_hh) has been updated to incorporate these changes.")
    
    data_cholera_hh <<- data_cholera_hh %>% mutate(tool="cholera", 
                                                   tool_cholera=ifelse(tool=="cholera", 1, 
                                                                       ifelse(tool=="common", 0, NA)), .after = "date_survey")
  }
  
  ## Consolidate Common and Cholera datasets together for HH depending on what has been specified
  if (cholera.hh & common.hh){
    data_hh <<- data_cholera_hh %>% plyr::rbind.fill(data_common_hh)} else if (cholera.hh & !common.hh) {
      data_hh <<- data_cholera_hh} else if (!cholera.hh & common.hh) {
        data_hh <<- data_common_hh}
  print(paste0("The household data from ", paste(unique(data_hh$tool), collapse=" and ")," tool has been consolidated in the dataframe data_hh"))
}

combine.col.hh <- function(){
  if (hh) {
    col_cholera_hh = data.frame(id=colnames(data_cholera_hh), col_cholera_hh=colnames(data_cholera_hh))
    col_common_hh = data.frame(id=colnames(data_common_hh), col_common_hh=colnames(data_common_hh))
    col.all.hh <<- col_cholera_hh %>% full_join(col_common_hh, by="id") %>% rename(question_header=id)
    print("Column headers from the two household dataset have been consolidated into the dataframe col.all.hh to enable comparison.")
    }
  }

combine.col.ki <- function(){
  if (ki) {
    col_cholera_ki = data.frame(id=colnames(data_cholera_ki), col_cholera_ki=colnames(data_cholera_ki))
    col_common_ki = data.frame(id=colnames(data_common_ki), col_common_ki=colnames(data_common_ki))
    col.all.ki <<- col_cholera_ki %>% full_join(col_common_ki, by="id") %>% rename(question_header=id)
    print("Column headers from the two Key informant datasets have been consolidated into the dataframe col.all.ki to enable comparison.")
  }
}

combine.col.all <- function(){
  if (hh & ki){
    col_cholera_hh = data.frame(id=colnames(data_cholera_hh), col_cholera_hh=colnames(data_cholera_hh))
    col_cholera_ki = data.frame(id=colnames(data_cholera_ki), col_cholera_ki=colnames(data_cholera_ki))
    col_common_hh = data.frame(id=colnames(data_common_hh), col_common_hh=colnames(data_common_hh))
    col_common_ki = data.frame(id=colnames(data_common_ki), col_common_ki=colnames(data_common_ki))
    col.all <<- col_cholera_hh  %>% full_join(col_common_hh, by="id") %>% full_join(col_cholera_ki, by = "id") %>% full_join(col_common_ki, by="id") %>% rename(question_header=id)
    print("Column headers from all datasets have been consolidated into the dataframe col.all to enable comparison.")
  }
}

combine.tools <- function(){
  if (hh) {
    tool_hh <<- bind_rows(tool_cholera_hh, tool_common_hh) %>% filter(!duplicated(name))
    choices_hh <<- bind_rows(choices_cholera_hh, choices_common_hh) %>% filter(!duplicated(paste0(list_name,name)))}
  if (ki) {
    tool_ki <<- bind_rows(tool_cholera_ki, tool_common_ki) %>% filter(!duplicated(name))
    choices_ki <<- bind_rows(choices_cholera_ki, choices_common_ki) %>% filter(!duplicated(paste0(list_name,name)))}
  if (cholera.hh) {
    tool_hh <<- tool_cholera_hh
    choices_hh <<- choices_cholera_hh} else if (common.hh) {
      tool_hh <<- tool_common_hh
      choices_hh <<- choices_cholera_hh}
  if (cholera.ki) {
    tool_ki <<- tool_cholera_ki
    choices_ki <<- choices_cholera_ki} else if (common.ki) {
      tool_ki <<- tool_common_ki
      choices_ki <<- choices_common_ki}
  print("The tools and choices ")
}

harmonise.tools <- function(){
  
  tool_cholera_ki <<- tool_cholera_ki %>%
    mutate(name=gsub("w_watersmell", "w_waterneeds", name), name=gsub("w_waterquality_what", "w_watersmell", name))
  
  tool_common_hh <<- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name))
  tool_cholera_hh <<- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name))
  
  choices_common_hh <<- choices_common_hh %>% 
    mutate(name = gsub("surface_water_other","less_preferred_drinking",
                       gsub("surface_water", "surface_drinking",
                            gsub("untreated", "less_preferred_other",
                                 gsub("other_surface", "surface_other", name)))))
  
  choices_common_hh <<- choices_common_hh %>%
    mutate(name = gsub("further", "further_source",
                       gsub("dangerous", "dangerous_source", name)))
}

## Cleaning log functions
initialise.cleaning.log <- function() {
  return(data.frame(uuid = as.character(), 
                    agency = as.character(),
                    enum_firstname = as.character(), 
                    enum_lastname = as.character(),
                    area = as.character(), 
                    variable = as.character(), 
                    issue = as.character(),
                    old_value = as.character(),
                    new_value = as.character(), 
                    check_id=as.character(),
                    fix = as.character(), 
                    checked_by = as.character()))
}

initialise.deletion.log <- function(){
 return(data.frame(uuid = as.character(), 
                   agency = as.character(),
                   enum_firstname = as.character(), 
                   enum_lastname = as.character(),
                   area = as.character(), 
                   issue = as.character())) 
}

# checks=check_pcode_sub_final %>% filter(!admin3Pcode %in% c(NA,""))
# check_id=""
# question.names=c("g_sub_district")
# issue="issue"
# new.value="admin3Pcode"
# fix="Checked with partner"
# checked_by="ON"
# add.col=c("")

cleaning.log.new.entries <- function(checks, check_id, question.names=c(), issue="issue", new.value="" , fix="Checked with partner", checked_by="ON", add.col=c("")){
  df <- initialise.cleaning.log()
  if (nrow(checks)>0){
    for(q.n in question.names){
      new.entries <- checks %>%  filter(flag) %>%
        mutate(uuid=uuid %>% as.character,
               enum_firstname=enum_firstname %>% as.character,
               enum_lastname=enum_lastname %>% as.character,
               variable=q.n %>% as.character,
               issue=issue %>% as.character,
               check_id=check_id %>% as.character,
               old_value=!!sym(q.n) %>% as.character,
               new_value=if (new.value=="") {as.character(new.value)} else {as.character(!!sym(new.value))},
               fix=fix %>% as.character,
               checked_by= checked_by %>% as.character)
      new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
      df <- bind_rows(df, new.entries)
    }
    return(df %>% arrange(uuid, variable, agency))
  }
}

add.to.cleaning.log <- function(checks, check_id, question.names=c(), issue="issue", new.value="" , fix="Checked with partner", checked_by="ON", add.col=c("")){
  df <- initialise.cleaning.log()
  if (nrow(checks)>0){
    for(q.n in question.names){
      new.entries <- checks %>%  filter(flag) %>%
        mutate(uuid=uuid %>% as.character,
               enum_firstname=enum_firstname %>% as.character,
               enum_lastname=enum_lastname %>% as.character,
               variable=q.n %>% as.character,
               issue=issue %>% as.character,
               check_id=check_id %>% as.character,
               old_value=!!sym(q.n) %>% as.character,
               new_value=if (new.value=="") {as.character(new.value)} else {as.character(!!sym(new.value))},
               fix=fix %>% as.character,
               checked_by= checked_by %>% as.character)
      new.entries <- new.entries %>% select(all_of(col.cl), any_of(add.col))
      df <- bind_rows(df, new.entries)
    }
    cleaning.log <<- bind_rows(cleaning.log, df %>% arrange(uuid, variable, agency))
  }
}


get.col.range <- function(variable){
  column.number <- which(colnames(data.val)==variable)
  all <- expand.grid(LETTERS, LETTERS)
  all <- all[order(all$Var1,all$Var2),]
  alphabet <- c(LETTERS, do.call('paste0',all))
  col.excel <- alphabet[column.number]
  nrow <- nrow(data.val %>% filter(!is.na(!!sym(variable))))
  range.vect <- c("$", col.excel, "$2:$", col.excel, "$", (nrow + 1))             ## if nrow + 2 => will keep an additionnal field in drop down list to be updated if needed by partner
  range <- paste(range.vect, sep="", collapse="")
  value.sheet <- paste("'Choices validation'!")
  value <- paste(value.sheet, range, sep="", collapse="")
  return(value)
}

save.follow.up.requests <- function(cl, choices, tool,  filename.out="output/test.xlsx"){       # save follow-up requests
  wb <- createWorkbook()
  addWorksheet(wb, "Follow-up")
  addWorksheet(wb, "Choices validation")
  writeData(wb = wb, x = cl, sheet = "Follow-up", startRow = 1)
  
  data.val <<- data.validation.list(choices, tool)
  writeData(wb = wb, x = data.val, sheet = "Choices validation", startRow = 1)
  
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center", border="TopBottomLeftRight", borderColour="#000000")
  
  setColWidths(wb, "Follow-up", cols=1, widths=10)
  setColWidths(wb, "Follow-up", cols=2, widths=10)
  setColWidths(wb, "Follow-up", cols=3, widths=15)
  setColWidths(wb, "Follow-up", cols=4, widths=15)
  setColWidths(wb, "Follow-up", cols=5, widths=10)
  setColWidths(wb, "Follow-up", cols=6, widths=20)
  setColWidths(wb, "Follow-up", cols=7, widths=40)
  setColWidths(wb, "Follow-up", cols=8, widths=16)
  setColWidths(wb, "Follow-up", cols=9, widths=16)
  setColWidths(wb, "Follow-up", cols=10, widths=10)
  
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=6)
  addStyle(wb, "Follow-up", style = createStyle(wrapText=T), rows = 1:(ncol(cl)+1), cols=7)
  addStyle(wb, "Follow-up", style = col.style, rows = 1, cols=1:dim(cl)[2])
  
  col.id <- which(colnames(cl) %in% c("variable", "issue", "old_value"))
  random.color <- ""
  if (nrow(cl)>1) {for (r in 2:nrow(cl)){
    if(as.character(cl[r, "uuid"])==as.character(cl[r-1, "uuid"]) & 
       as.character(cl[r, "check_id"])==as.character(cl[r-1, "check_id"])){
      if (random.color == "") random.color <- randomColor(1, luminosity = "light")
      addStyle(wb, "Follow-up", style = createStyle(fgFill=random.color, wrapText=F), 
               rows = r:(r+1), cols=col.id, gridExpand = T)
    } else random.color=""
  }}
  
  for (r in 1:nrow(cl)){
    if (cl[r,"variable"] %in% colnames(data.val)){
      dataValidation(wb, "Follow-up", cols = which(colnames(cl)=="new_value"),
                     rows = r+1, type = "list",
                     value = get.col.range(cl[r,"variable"]))
    }
  }
  saveWorkbook(wb, filename.out, overwrite = TRUE)
} 

save.pcode.followup <- function(cl, pcode.table,  filename.out="output/test.xlsx", all.matches=NULL){
  wb <- createWorkbook()
  addWorksheet(wb, "Unmatched pcodes")
  addWorksheet(wb, "Pcodes")
  writeData(wb = wb, x = cl, sheet = "Unmatched pcodes", startRow = 1)
  writeData(wb = wb, x = pcode.table, sheet = "Pcodes", startRow = 1)
  
  if (!is.null(all.matches)){
    addWorksheet(wb, "All matches")
    writeData(wb = wb, x = all.matches, sheet = "All matches", startRow = 1)
  }
  
  style.col.color <- createStyle(fgFill="#E5FFCC", border="TopBottomLeftRight", borderColour="#000000")
  style.col.color.first <- createStyle(textDecoration="bold", fgFill="steelblue1", border="TopBottomLeftRight", borderColour="#000000", wrapText=F)
  col.style <- createStyle(textDecoration="bold", fgFill="#CECECE", halign="center", border="TopBottomLeftRight", borderColour="#000000")
  
  addStyle(wb, "Unmatched pcodes", style = col.style, rows = 1, cols=1:ncol(cl))
  addStyle(wb, "Unmatched pcodes", style = style.col.color.first, rows = 2:(nrow(cl)+1), cols=9)
  addStyle(wb, "Unmatched pcodes", style = style.col.color, rows = 2:(nrow(cl)+1), cols=10)
  # addStyle(wb, "Unmatched pcodes", style = style.col.color, rows = 2:(nrow(cl)+1), cols=8)
  
  setColWidths(wb, "Unmatched pcodes", cols=1, widths=25)
  setColWidths(wb, "Unmatched pcodes", cols=2, widths=10)
  setColWidths(wb, "Unmatched pcodes", cols=3, widths=14.5)
  setColWidths(wb, "Unmatched pcodes", cols=4, widths=14.5)
  setColWidths(wb, "Unmatched pcodes", cols=5, widths=14.5)
  setColWidths(wb, "Unmatched pcodes", cols=6, widths=12)
  setColWidths(wb, "Unmatched pcodes", cols=7, widths=5)
  setColWidths(wb, "Unmatched pcodes", cols=8, widths=35)
  setColWidths(wb, "Unmatched pcodes", cols=9, widths=18.5)
  setColWidths(wb, "Unmatched pcodes", cols=10, widths=18.5)
  setColWidths(wb, "Unmatched pcodes", cols=11, widths=18.5)
  # setColWidths(wb, "Unmatched pcodes", cols=12:ncol(cl), widths=10)
  
  saveWorkbook(wb, filename.out, overwrite = TRUE)
}

## Cleaning functions
## 1. Numerical outlier 
detect.outliers <- function(df, method="sd-linear", n.sd=3, n.iqr=3){
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!="uuid"]){
    df.temp <- data.frame(uuid=df$uuid, value=as.numeric(df[[col]])) %>% filter(!is.na(value) & value>0)
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier.high=ifelse(value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T), T, F),
               is.outlier.low =ifelse(value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T), T, F))
    } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(col=value,
               is.outlier.high=ifelse(col > quantile(col, 0.75) + n.iqr*IQR(col), T, F),
               is.outlier.low =ifelse(col < quantile(col, 0.25) - n.iqr*IQR(col), T, F))
    } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=ifelse(col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T), T, F), 
               is.outlier.low =ifelse(col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T), T, F))
    } else if (method=="iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier.high=ifelse(col.log > quantile(col.log, 0.75) + n.iqr*IQR(col.log), T, F),
               is.outlier.low =ifelse(col.log < quantile(col.log, 0.25) - n.iqr*IQR(col.log), T, F))
    } else stop("Method unknown")
    df.temp <- filter(df.temp, is.outlier.high | is.outlier.low) %>% 
      mutate(variable=col, old_value=value, 
             issue = ifelse(is.outlier.high, paste0("High numerical outlier, using ", method, " method"),
                            ifelse(is.outlier.low, paste0("Low numerical outlier, using ", method, " method"), ""))) %>%
      select(uuid, variable, old_value, issue)
    res <- rbind(res, df.temp)
  }
  return(res)
}

detect.outliers <- function(df, method="sd-linear", n.sd=3, n.iqr=3){
  res <- data.frame()
  for (col in colnames(df)[colnames(df)!="uuid"]){
    df.temp <- data.frame(uuid=df$uuid, value=as.numeric(df[[col]])) %>% filter(!is.na(value) & value>0)
    if (method=="sd-linear"){
      df.temp <- df.temp %>%
        mutate(is.outlier=ifelse(value > mean(value, na.rm=T) + n.sd*sd(value, na.rm=T) | 
                                   value < mean(value, na.rm=T) - n.sd*sd(value, na.rm=T), T, F))
    } else if (method=="iqr-linear") {
      df.temp <- df.temp %>%
        mutate(col=value,
               is.outlier=ifelse(col > quantile(col, 0.75) + n.iqr*IQR(col) |
                                   col < quantile(col, 0.25) - n.iqr*IQR(col), T, F))
    } else if (method=="sd-log"){
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier=ifelse(col.log > mean(col.log, na.rm=T) + n.sd*sd(col.log, na.rm=T) | 
                                   col.log < mean(col.log, na.rm=T) - n.sd*sd(col.log, na.rm=T), T, F))
    } else if (method=="iqr-log") {
      df.temp <- df.temp %>%
        mutate(col.log=log(value),
               is.outlier=ifelse(col.log > quantile(col.log, 0.75) + n.iqr*IQR(col.log) |
                                   col.log < quantile(col.log, 0.25) - n.iqr*IQR(col.log), T, F))
    } else stop("Method unknown")
    df.temp <- filter(df.temp, is.outlier) %>% 
      mutate(variable=col, old_value=value) %>%
      select(uuid, variable, old_value)
    res <- rbind(res, df.temp)
  }
  return(res)
}

## Kobo tool and label functions
get.ref.question <- function(x){
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}

get.choice.list.name <- function(x){
  x.1 <- str_split(x, " ")[[1]]
  if (length(x.1)==1) return(NA)
  else return(x.1[2])
}

get.element <- function(x,n){
  x <- str_split(x, " ")[[1]]
  if (length(x)==1 & n>1) return("") else return(x[n])
}

get.q.type <- function(x) return(str_split(x, " ")[[1]][1])

get.select.db <- function(choices, tool){
  # list of choices for each list_name (from TOOL_CHOICES)
  list.choices <- choices %>% filter(!is.na(list_name)) %>% group_by(list_name) %>% 
    mutate(choices=paste(name, collapse=";\n"),
           choices.label=paste(`label::english`, collapse=";\n")) %>% 
    summarise(choices=choices[1], choices.label=choices.label[1])
  select.questions <- tool %>% select(type, name) %>% 
    mutate(q.type=as.character(lapply(type, get.q.type)),
           list_name=as.character(lapply(type, get.choice.list.name))) %>% 
    filter(list_name!="NA" & list_name!="group" & list_name!="repeat") %>% 
    left_join(list.choices, by="list_name") %>% 
    filter(!is.na(choices))
  return(select.questions)
}

data.validation.list <- function(choices, tool){
  choicelist <- get.select.db(choices, tool) %>% dplyr::select(name, choices)                  # extract list of valid answer for all survey
  choice_validation <- choicelist %>% transpose %>% setNames(.[1,]) %>% slice(-1) %>% mutate_all(~str_split(.,";\n")) 
  nrow_validation <- lapply(choice_validation, function(x) length(x[[1]])) %>% unlist %>% max
  data.val <- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))
  for (c in colnames(choice_validation)){
    data.val <- data.val %>% mutate(!!sym(c) := c(unlist(choice_validation[[c]]), rep(NA, nrow_validation-length(choice_validation[[c]][[1]]))))
  }
  return(data.val)
}

choice.long <- function(choices, tool){
  choices <- data.validation.list(choices, tool)
  choices_long <- choices %>% pivot_longer(cols=colnames(.)) %>% arrange(name) %>% filter(!is.na(value))
  return(choices_long)
}

get.other.db <- function(){
  select.questions <- get.select.db()
  # for each "other" question, get ref.question and list of choices
  other.choice.name <- choices %>% 
    filter(grepl("Other|other",`label::english`)|grepl("أخرى",`label::arabic`)) %>% pull(name)
  other.db <- tool %>% filter(grepl(paste(other.choice.name,collapse="|"),relevant)) %>% 
    select("type", "name", "label::english", "relevant") %>% 
    mutate(ref.question=as.character(lapply(relevant, get.ref.question))) %>% 
    left_join(select(select.questions, "name", "q.type", "list_name", "choices", "choices.label"),
              by=c("ref.question"="name")) %>% 
    left_join(select(var.labels, "name", "label.full"), by="name") %>% 
    select(name, ref.question, label.full, q.type, list_name, choices, choices.label)
  
  return(other.db)
}

get.dependencies <- function(){
  # determine dependencies of "other" questions 
  # (i.e. if there is a change in any of the "other" question, we need to follow-up on other questions)
  relevant.cleaned <- filter(tool, !is.na(relevant) & !str_ends(relevant, "\\'other\\'\\)")) %>% 
    filter(str_starts(type, "select") | str_starts(type, "text") | str_starts(type, "integer"))
  dependencies <- other.db %>% 
    mutate(questions.other.affected=as.numeric(lapply(name, 
                                                      function(x){sum(str_detect(relevant.cleaned$relevant,
                                                                                 paste("\\{",x,"\\}", sep="")))}))) %>%
    mutate(questions.affected=as.numeric(lapply(ref.question, 
                                                function(x){sum(str_detect(relevant.cleaned$relevant, 
                                                                           paste("\\{",x,"\\}", sep="")))}))) %>%
    filter(questions.other.affected > 0 | questions.affected > 0) %>% 
    select(name, ref.question, questions.other.affected, questions.affected)
  return(dependencies)
}

get.label <- function(variable){
  return(var.labels[var.labels$name==variable, "label.full"])
}

choice.name2label <- function(list_name, name){
  return(as.character(choices_all[choices_all$list_name==list_name & 
                                    choices_all$name==name, "label::english"]))
}

choice.label2name <- function(list_name, label){
  return(as.character(choices_all[choices_all$list_name==list_name & 
                                    choices_all$`label::english`==label, "name"]))
}

## Other utils function 
number.to.arabic <- function(s){
  arabic <- c("\u0660","\u0661","\u0662","\u0663","\u0664","\u0665","\u0666","\u0667","\u0668","\u0669")
  english <- c("0","1","2","3","4","5","6","7","8","9")
  res <- s %>% str_replace_all(setNames(arabic, english))
  return(res)
}

arabic.tonumber <- function(s){
  arabic <- c("\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9")
  english <- c("01234567890123456789")
  suppressWarnings(res <- lapply(s, function(x) as.numeric(chartr(arabic, english, x))) %>% unlist)
  res[which(is.na(res))] <- s[which(is.na(res))]
  return(res)
}

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

##### main harmonisation and consolidation for HH and KI // Probably to be left aside in order to run HH or KI separately
harmonise.and.consolidate.datasets <- function(x) {
  ## 1.2. Streamline columns names in dataset and imported kobo tool
  ## 1.2.1. Clean headers in data
  data_common_hh <<- data_common_hh %>%
    setNames(paste0(gsub("Comm_HH_", "", colnames(.)))) %>%                       ## Delete Comm_HH_ prefix from column names
    setNames(paste0(gsub("/", "\\.", colnames(.))))                               ## Replace the choice separator "/" with "." to streamline headers to PBI dashboard
  data_cholera_hh <<- data_cholera_hh %>%
    setNames(paste0(gsub("Chol_HH_", "", colnames(.)))) %>%                       ## Delete Chol_HH_ prefix from column names
    setNames(paste0(gsub("/", "\\.", colnames(.))))
  data_cholera_ki <<- data_cholera_ki %>%
    setNames(paste0(gsub("/", "\\.", colnames(.))))

  ## 1.2.2. Clean variable names in Kobo tools
  tool_common_hh <<- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name))
  tool_cholera_hh <<- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name))

  ## 1.2.3. Question W5.1 How does your household adapt to the lack of water? Recoding choices
  data_common_hh <<- data_common_hh %>%
    rename(`adapt_lack.less_preferred_drinking`=`adapt_lack.surface_water_other`, # surface_water_other should be less_preferred_drinking
           `adapt_lack.surface_drinking`=`adapt_lack.surface_water`,              # adapt_lack.surface_water should be adapt_lack.surface_drinking
           `adapt_lack.less_preferred_other`=`adapt_lack.untreated`,              # untreated should be less_preferred_other
           `adapt_lack.surface_other`=`adapt_lack.other_surface`) %>%             # other_surface should be surface_other
    mutate(adapt_lack = gsub("surface_water_other","less_preferred_drinking",
                             gsub("surface_water", "surface_drinking",
                                  gsub("untreated", "less_preferred_other",
                                       gsub("other_surface", "surface_other", adapt_lack)))))
  # data_common_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique  # check line - comment if all good
  choices_common_hh <<- choices_common_hh %>%
    mutate(name = gsub("surface_water_other","less_preferred_drinking",
                       gsub("surface_water", "surface_drinking",
                            gsub("untreated", "less_preferred_other",
                                 gsub("other_surface", "surface_other", name)))))

  data_cholera_hh <<- data_cholera_hh %>%
    rename(`adapt_lack.further_source`=`adapt_lack.further`,                      # further should be further_source
           `adapt_lack.dangerous_source`=`adapt_lack.dangerous`) %>%              # dangerous should be dangerous_source
    mutate(adapt_lack = gsub("further", "further_source",
                             gsub("dangerous", "dangerous_source", adapt_lack)))
  # data_cholera_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique # check line - comment if all good
  choices_cholera_hh <<- choices_cholera_hh %>%
    mutate(name = gsub("further", "further_source",
                       gsub("dangerous", "dangerous_source", name)))

  ## 1.2.4. Renaming for the KI datasets !! ORDER DEPENDENT, Don't switch order of line 79 and 80 !!
  data_cholera_ki <<- data_cholera_ki %>%
    rename(w_waterneeds=w_watersmell,                                             # Renaming wrongly named columns in data cholera KI tool (shifted)
           w_watersmell=w_waterquality_what)                                      # Renaming wrongly named columns in data cholera KI tool (shifted)

  ## Clean the tool accordingly
  tool_cholera_ki <<- tool_cholera_ki %>%
    mutate(name=gsub("w_watersmell", "w_waterneeds", name), name=gsub("w_waterquality_what", "w_watersmell", name))

  ## 1.3. Consolidate Common and Cholera datasets together for KI and HHs respectively
  data_ki <<- data_cholera_ki %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_ki %>% mutate(tool="common")) %>%
    mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
  data_hh <<- data_cholera_hh %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_hh %>% mutate(tool="common")) %>%
    mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
}

harmonise.consolidate.ki.old <- function() {
  ## 1. Clean headers in data
  data_cholera_ki <<- data_cholera_ki %>%
    setNames(paste0(gsub("/", "\\.", colnames(.))))
  
  ## 2.1 Renaming for the KI datasets !! ORDER DEPENDENT, Don't switch order of line 79 and 80 !!
  data_cholera_ki <<- data_cholera_ki %>% 
    rename(w_waterneeds=w_watersmell,                                             # Renaming wrongly named columns in data cholera KI tool (shifted)
           w_watersmell=w_waterquality_what)                                      # Renaming wrongly named columns in data cholera KI tool (shifted)
  
  ## 2.2 Rename the tool accordingly
  tool_cholera_ki <<- tool_cholera_ki %>%
    mutate(name=gsub("w_watersmell", "w_waterneeds", name), name=gsub("w_waterquality_what", "w_watersmell", name))
  
  ## 3. Consolidate Common and Cholera datasets together for KI 
  data_ki <<- data_cholera_ki %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_ki %>% mutate(tool="common")) %>%
    mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
}


harmonise.consolidate.hh.old <- function(){
  
  ## 1. Clean headers in data
  data_common_hh <<- data_common_hh %>% 
    setNames(paste0(gsub("Comm_HH_", "", colnames(.)))) %>%                       ## Delete Comm_HH_ prefix from column names
    setNames(paste0(gsub("/", "\\.", colnames(.))))                               ## Replace the choice separator "/" with "." to streamline headers to PBI dashboard
  data_cholera_hh <<- data_cholera_hh %>%
    setNames(paste0(gsub("Chol_HH_", "", colnames(.)))) %>%                       ## Delete Chol_HH_ prefix from column names
    setNames(paste0(gsub("/", "\\.", colnames(.)))) 
  
  ## 1.2 Rename tool accordingly
  tool_common_hh <<- tool_common_hh %>% mutate(name = gsub("Comm_HH_", "", name))
  tool_cholera_hh <<- tool_cholera_hh %>% mutate(name = gsub("Chol_HH_", "", name))
  
  ## 2.1. Common tool - Recoding choices of Question W5.1 How does your household adapt to the lack of water?
  data_common_hh <<- data_common_hh %>% 
    rename(`adapt_lack.less_preferred_drinking`=`adapt_lack.surface_water_other`, # surface_water_other should be less_preferred_drinking
           `adapt_lack.surface_drinking`=`adapt_lack.surface_water`,              # adapt_lack.surface_water should be adapt_lack.surface_drinking
           `adapt_lack.less_preferred_other`=`adapt_lack.untreated`,              # untreated should be less_preferred_other
           `adapt_lack.surface_other`=`adapt_lack.other_surface`) %>%             # other_surface should be surface_other
    mutate(adapt_lack = gsub("surface_water_other","less_preferred_drinking",
                             gsub("surface_water", "surface_drinking",
                                  gsub("untreated", "less_preferred_other",
                                       gsub("other_surface", "surface_other", adapt_lack)))))
  # data_common_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique  # check line - comment if all good
  ## 2.2. Rename tool accordingly
  choices_common_hh <<- choices_common_hh %>% 
    mutate(name = gsub("surface_water_other","less_preferred_drinking",
                       gsub("surface_water", "surface_drinking",
                            gsub("untreated", "less_preferred_other",
                                 gsub("other_surface", "surface_other", name)))))
  ## 2.3 Cholera tool - Recoding choices of Question W5.1 How does your household adapt to the lack of water?
  data_cholera_hh <<- data_cholera_hh %>%
    rename(`adapt_lack.further_source`=`adapt_lack.further`,                      # further should be further_source
           `adapt_lack.dangerous_source`=`adapt_lack.dangerous`) %>%              # dangerous should be dangerous_source
    mutate(adapt_lack = gsub("further", "further_source",
                             gsub("dangerous", "dangerous_source", adapt_lack)))
  # data_cholera_hh %>% pull(adapt_lack) %>% str_split(" ") %>% unlist %>% unique # check line - comment if all good
  ## 2.4. Rename tool accordingly
  choices_cholera_hh <<- choices_cholera_hh %>%
    mutate(name = gsub("further", "further_source",
                       gsub("dangerous", "dangerous_source", name)))
  
  ## 3. Consolidate Common and Cholera datasets together for HH 
  data_hh <<- data_cholera_hh %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_hh %>% mutate(tool="common")) %>%
    mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
}

