## Cleaning log functions

initialise.cleaning.log <- function() {
  return(data.frame(uuid = as.character(), 
                    agency = as.character(),
                    enum_firstname = as.character(), 
                    enum_lastname = as.character(),
                    area = as.character(), 
                    variable = as.character(), 
                    issue = as.character(),
                    check_id=as.character(),
                    old_value = as.character(),
                    new_value = as.character(), 
                    fix = as.character(), 
                    checked_by = as.character()))
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
  data.val <<- data.frame(matrix(NA, nrow = nrow_validation, ncol = 0))
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
  relevant.cleaned <- filter(tool.survey, !is.na(relevant) & !str_ends(relevant, "\\'other\\'\\)")) %>% 
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

## Other utils

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
