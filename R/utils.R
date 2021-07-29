## Data harmonisation & consolidation 

harmonise.consolidate.ki <- function(x) {
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

harmonise.consolidate.hh <- function(x){
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
  choices_common_hh <<- choices_common_hh %>%
    mutate(name = gsub("further", "further_source",
                       gsub("dangerous", "dangerous_source", name)))
  
  ## 3. Consolidate Common and Cholera datasets together for HH 
  data_hh <<- data_cholera_hh %>% mutate(tool="cholera", .after = "date_survey") %>% bind_rows(data_common_hh %>% mutate(tool="common")) %>%
    mutate(tool_cholera=ifelse(tool=="cholera", 1,0), .after = "tool")
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
               new_value=new.value %>% as.character,
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

## Cleaning functions
## 1. Numerical outlier 
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
  choices_common_hh <<- choices_common_hh %>%
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