#' @import RosyUtils
#' @import RosyApp
#' @title fields_to_choices
#' @export
fields_to_choices <- function(fields){
  fields <- fields[which(fields$field_type%in%c("radio","dropdown","checkbox_choice","yesno")),]
  # fields$field_name[which(fields$field_type=="checkbox_choice")] <- fields$field_name[which(fields$field_type=="checkbox_choice")] %>% strsplit("___") %>% sapply(function(X){X[[1]]})
  fields <- fields[which(!is.na(fields$select_choices_or_calculations)),]
  choices <- NULL
  for(i in 1:nrow(fields)){
    field_name <- fields$field_name[i]
    form_name <- fields$form_name[i]
    field_label <- fields$field_label[i]
    field_type <- fields$field_type[i]
    selections <- fields$select_choices_or_calculations[i] %>% split_choices()
    choices <- choices %>% dplyr::bind_rows(
      data.frame(
        form_name = form_name,
        field_name = field_name,
        field_type = field_type,
        field_label =  ifelse(!is.na(field_label),field_label,field_name),
        code = selections$code,
        name =selections$name
      )
    )
  }
  choices$label <- paste(choices$form_name,"-",choices$field_label,"-",choices$name)
  rownames(choices) <- NULL
  return(choices)
}
#' @export
annotate_fields <- function(DB,skim= T){
  fields <- DB$metadata$fields#[,colnames(get_original_fields(DB))]
  fields <- fields[which(fields$field_type!="descriptive"),]
  fields <- fields[which(fields$field_type!="checkbox"),]
  fields$field_label[which(is.na(fields$field_label))] <- fields$field_name[which(is.na(fields$field_label))]
  fields  <- unique(fields$form_name) %>%
    lapply(function(IN){
      fields[which(fields$form_name==IN),]
    }) %>% dplyr::bind_rows()
  if(!"field_type_R"%in%colnames(fields))fields$field_type_R <- "character"
  fields$field_type_R[which(fields$field_type %in% c("radio","yesno","dropdown","checkbox_choice"))] <- "factor"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "integer")] <- "integer"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "date_mdy")] <- "date"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "date_ymd")] <- "date"
  fields$field_type_R[which(fields$text_validation_type_or_show_slider_number == "datetime_dmy")] <- "datetime"
  fields$in_original_redcap <- T
  fields$original_form_name <- fields$form_name
  if(DB$internals$is_transformed){
    fields$in_original_redcap <- fields$field_name %in% DB$transformation$original_fields$field_name
    fields$original_form_name <- DB$transformation$original_fields$form_name[match(fields$field_name,DB$transformation$original_fields$field_name)]
  }
  if(!"units" %in% colnames(fields))fields$units <- NA
  if(!"field_label_short" %in% colnames(fields)) fields$field_label_short <- fields$field_label
  # if(!"field_label_short" %in% colnames(fields))fields$ <- fields$field_label
  if(skim){
    skimmed <- NULL
    for (form in unique(fields$form_name)){
      COLS <- fields$field_name[which(fields$form_name==form)]
      CHECK_THIS <- DB$data[[form]]
      COLS <- COLS[which(COLS %in% colnames(CHECK_THIS))]
      skimmed <- skimmed %>% dplyr::bind_rows(CHECK_THIS[,COLS] %>% skimr::skim())
    }
    FOR_ORDERING <- fields$field_name
    fields <- fields %>% merge(skimmed,by.x = "field_name",by.y = "skim_variable",all = T)
    fields <- FOR_ORDERING %>%
      lapply(function(IN){
        fields[which(fields$field_name==IN),]
      }) %>% dplyr::bind_rows()
  }
  # bullet_in_console("Annotated `DB$metadata$fields`",bullet_type = "v")
  return(fields)
}
#' @export
annotate_forms <- function(DB){
  forms <- DB$metadata$forms
  for(status in c("Incomplete","Unverified","Complete")){
    forms[[tolower(status)]] <- forms$form_name %>% sapply(function(form_name){
      form_name %>% strsplit(" [:|:] ") %>% unlist() %>% sapply(function(form_name){
        (DB$data[[form_name]][[paste0(form_name,"_complete")]]==status) %>% which() %>% length()
      }) %>% paste0(collapse = " | ")
    })
  }
  return(forms)
}
#' @export
annotate_choices <- function(DB){
  forms <- DB$metadata$forms
  fields <- DB$metadata$fields
  choices <- DB$metadata$choices
  # choices$field_name_raw <- choices$field_name
  # choices$field_name_raw[which(choices$field_type=="checkbox_choice")] <- choices$field_name[which(choices$field_type=="checkbox_choice")] %>%
  #   strsplit("___") %>%
  #   sapply(function(X){X[[1]]})
  # choices$field_label_raw <- choices$field_label
  # choices$field_label_raw[which(choices$field_type=="checkbox_choice")] <- choices$field_name_raw[which(choices$field_type=="checkbox_choice")] %>%
  #   sapply(function(X){
  #     DB$metadata$fields$field_label[which(fields$field_name==X)] %>% unique()
  #   })
  choices$n <- 1:nrow(choices) %>% lapply(function(i){
    DF <- DB$data[[choices$form_name[i]]]
    if(is.null(DF))return(0)
    if(nrow(DF)==0)return(0)
    sum(DF[,choices$field_name[i]]==choices$name[i],na.rm = T)
    # print(i)
  }) %>% unlist()
  choices$n_total <- 1:nrow(choices) %>% lapply(function(i){
    DF <- DB$data[[choices$form_name[i]]]
    if(is.null(DF))return(0)
    if(nrow(DF)==0)return(0)
    sum(!is.na(DF[,choices$field_name[i]]),na.rm = T)
  }) %>% unlist()
  choices$perc <-  (choices$n/choices$n_total) %>% round(4)
  choices$perc_text <- choices$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
  # DB$summary$choices <- choices
  # bullet_in_console("Annotated `DB$summary$choices`",bullet_type = "v")
  return(choices)
}
#' @title fields_with_no_data
#' @export
fields_with_no_data <- function(DB){
  DB$metadata$fields$field_name[which(is.na(DB$metadata$fields$complete_rate)&!DB$metadata$fields$field_type%in%c("checkbox","descriptive"))]
}
# generate_cross_codebook <- function(){
#   codebook <- DB$metadata$choices
#   metadata <- get_original_field_names(DB)
#   data_choice<-get_default_data_choice(DB)
#
#   cross_codebook <- codebook[,c("form_name","field_name","name")]
#   cross_codebook$label <- paste0(cross_codebook$form_name, " - ",cross_codebook$field_name, " - ", cross_codebook$name)
#   # Generate all combinations
#   all_combinations <- expand.grid(cross_codebook$label, cross_codebook$label)
#
#   # Filter out self pairs
#   all_combinations <- all_combinations[all_combinations$Var1 != all_combinations$Var2, ]
#
#   # Extract variable names
#   variable1 <- cross_codebook$label[match(all_combinations$Var1, cross_codebook$label)]
#   variable2 <- cross_codebook$label[match(all_combinations$Var2, cross_codebook$label)]
#   # Remove duplicates where variable1 and variable2 are interchanged
#   all_combinations <- data.frame(
#     variable1 = pmin(variable1, variable2),
#     variable2 = pmax(variable1, variable2)
#   ) %>% unique()
#   form_field_pairs <- paste0(codebook$form_name," - ",codebook$field_name)
#   for(form_field_pair in form_field_pairs){ #form_field_pair<-form_field_pairs %>% sample(1)
#     keep_rows <- which(!((all_combinations$variable1 %>% startsWith(form_field_pair)) & (all_combinations$variable2 %>% startsWith(form_field_pair))))
#     all_combinations <- all_combinations[keep_rows,]
#   }
#   cross_codebook1 <- cross_codebook
#   colnames(cross_codebook1) <- colnames(cross_codebook1) %>% paste0(1)
#   cross_codebook2 <- cross_codebook
#   colnames(cross_codebook2) <- colnames(cross_codebook2) %>% paste0(2)
#   all_combinations <- all_combinations %>% merge(cross_codebook1,by.x="variable1",by.y = "label1")
#   all_combinations <- all_combinations %>% merge(cross_codebook2,by.x="variable2",by.y = "label2")
#   rownames(all_combinations) <- NULL
#   i <- 1:nrow(all_combinations) %>% sample1()
#   all_combinations$n <- 1:nrow(all_combinations) %>% lapply(function(i){
#     x<-DB$data[[all_combinations$form_name1[i]]]
#     x<-DB$data[[all_combinations$form_name1[i]]]
#     x[which(x[[all_combinations[i]]])]
#
#     ==codebook$name[i],na.rm = T)
#   }) %>% unlist()
#   codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
#     sum(!is.na(DB$data[[codebook$form_name[i]]][,codebook$field_name[i]]),na.rm = T)
#   }) %>% unlist()
#   codebook$perc <-  (codebook$n/codebook$n_total) %>% round(4)
#   codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
#   return(codebook)
# }
#' @title clean DB columns for plotting using the metadata
#' @description
#'  Turns choices into factors and integers to integer for table processing such as with table1 and plots
#' @param drop_blanks logical for dropping n=0 choices
#' @param drop_unknowns logical for dropping missing codes
#' @return DB object cleaned for table or plots
#' @export
clean_DB <- function(DB,drop_blanks=F,drop_unknowns=F){
  # DB <-  DB %>% annotate_fields(skim = F)
  for(FORM in names(DB$data)){
    DB$data[[FORM]] <- DB$data[[FORM]] %>% clean_DF(fields=DB$metadata$fields,drop_blanks= drop_blanks,drop_unknowns=drop_unknowns)
  }
  return(DB)
}
#' @title clean_DF
#' @export
clean_DF <- function(DF,fields,drop_blanks = T,drop_unknowns = T){
  for(COLUMN in colnames(DF)){
    if(COLUMN %in% fields$field_name){
      ROW <- which(fields$field_name==COLUMN)
      units <- NULL
      if(!is.na(fields$units[ROW])){
        units <- fields$units[ROW]
      }
      class <- fields$field_type_R[ROW][[1]]
      label <- ifelse(is.na(fields$field_label[ROW]),COLUMN,fields$field_label[ROW])[[1]]
      levels <- NULL
      if(!is.na(class)){
        if(class == "factor"){
          select_choices <- fields$select_choices_or_calculations[ROW]
          if(!is.na(select_choices)){
            levels <- split_choices(select_choices)[[2]]
          }else{
            levels <- unique(DF[[COLUMN]]) %>% drop_nas()
          }
          if(any(duplicated(levels))){
            DUPS <- levels %>% duplicated() %>% which()
            warning("You have a variable (",COLUMN,") with dupplicate names (",levels[DUPS] %>% paste0(collapse = ", "),"). This is not great but for this proccess they will be merged and treated as identical responses.")
            levels <- levels %>% unique()
          }
          if(drop_blanks){
            levels <- levels[which(levels%in%unique(DF[[COLUMN]]))]
          }
          if(!drop_unknowns){
            levels <- levels %>% append(unique(DF[[COLUMN]])) %>% unique() %>% drop_nas()
          }
        }
        if(class == "integer"){
          DF[[COLUMN]] <- as.integer(DF[[COLUMN]])
        }
        if(class == "numeric"){
          DF[[COLUMN]] <- as.numeric(DF[[COLUMN]])
        }
        DF
      }
      DF[[COLUMN]] <- DF[[COLUMN]] %>% clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  return(DF)
}
#' @title clean column for plotting; manual addition of clean_DB
#' @description
#'  Turns choices into factors and integers to integer for table processing such as with table1 and plots
#' @param col the column vector
#' @param class character for column type: integer, factor, numeric
#' @param label character for label
#' @param units character for units
#' @param levels character vector of levels for factor
#' @return cleaned column
#' @export
clean_column_for_table <- function(col,class,label,units,levels){
  if(!missing(class)){
    if(!is.null(class)){
      if(!is.na(class)){
        if(class=="integer"){
          col <-   col %>% as.integer()
        }
        if(class=="factor"){
          col <-   col %>% factor(levels = levels,ordered = T)
        }
        if(class=="numeric"){
          col <-   col %>% as.numeric()
        }
      }
    }
  }
  if(!missing(label)){
    attr(col, "label") <- label
  }
  if(!missing(units)){
    attr(col, "units") <- units
  }
  col
}
