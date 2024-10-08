#' @export
rmarkdown_DB <- function (DB,dir_other){
  if(missing(dir_other)){
    dir <- get_dir(DB) %>% file.path("output")
  }else{
    dir  <- dir_other
  }
  filename <- paste0(DB$short_name,"_full_summary_",gsub("-","_",Sys.Date()),".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown","pdf.Rmd",package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = F
  )
}
#' @export
stack_vars <- function(DB,vars,new_name,drop_na=T){
  DB <- validate_DB(DB)
  fields <- DB$metadata$fields
  if(!all(vars%in%metadata$field_name))stop("all vars must be in metadata.")
  the_stack <- NULL
  for(var in vars){# var <- vars %>% sample1()
    DF <- filter_DB(DB,field_names = var)[[1]]
    colnames(DF)[which(colnames(DF)==var)] <- new_name
    the_stack <-the_stack %>% dplyr::bind_rows(DF)
  }
  if(drop_na){
    the_stack <- the_stack[which(!is.na(the_stack[[new_name]])),]
  }
  return(the_stack)
}
get_original_field_names <- function(DB){
  if(DB$internals$is_transformed)return(DB$transformation$original_fields$field_name)
  return(DB$metadata$fields$field_name)
}
#' @export
get_all_field_names <- function(DB){
  return(DB$data %>% sapply(colnames) %>% unlist() %>% unique())
}
#' @title field_names_to_form_names
#' @export
field_names_to_form_names <- function(DB,field_names){
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  field_names_keys <- field_names[which(field_names%in%form_key_cols)]
  form_names_keys <- field_names_keys %>% sapply(function(field_name){
    DB$metadata$form_key_cols%>% names()%>% sapply(function(FORM){
      if(!field_name%in%DB$metadata$form_key_cols[[FORM]])return(NULL)
      return(FORM)
    })
  }) %>% unlist() %>% as.character() %>% unique()
  fields <- DB$metadata$fields
  field_names_not_keys <- field_names[which(!field_names%in%form_key_cols)]
  form_names_not_keys <- fields$form_name[match(field_names_not_keys, fields$field_name)] %>% drop_nas()
  form_names <- c(form_names_not_keys,form_names_keys) %>% unique()
  return(form_names)
}
#' @title form_names_to_field_names
#' @export
form_names_to_field_names <- function(form_names,DB){
  field_names <- NULL
  fields <- get_original_fields(DB) #DB$metadata$fields
  for(form_name in form_names){
    field_names <- field_names %>% append(fields$field_name[which(fields$form_name==form_name)])
  }
  return(unique(field_names))
}
#' @export
construct_header_list <- function(df_list,md_elements = c("form_name","field_type","field_label"),fields){
  if(anyDuplicated(fields$field_name)>0)stop("dup names not allowed in fields")
  df_col_list <- df_list %>% lapply(colnames)
  header_df_list <- df_col_list %>% lapply(function(field_names){
    x<- field_names%>% lapply(function(field_name){
      row <- which(fields$field_name==field_name)
      if(length(row)>0){
        return(as.character(fields[md_elements][row,]))
      }else{
        return(rep("",length(md_elements)))
      }
    }) %>% as.data.frame()
    colnames(x)<-field_names
    x<- x[which(apply(x, 1, function(row){any(row!="")})),]
    x
  })
  return(header_df_list)
}
#' @title form_names_to_field_names
#' @export
stripped_DB <- function (DB) {
  DB$redcap$log <- list()
  DB$data <- list()
  DB$data_update <- list()
  return(DB)
}
all_DB_to_char_cols <- function(DB){
  DB$data <-DB$data %>% all_character_cols_list()
  DB$data <-DB$data %>% all_character_cols_list()
  DB$data_update <-DB$data_update %>% all_character_cols_list()
  return(DB)
}
#' @title Select REDCap records from DB
#' @param field_names character vector of field_names to be included
#' @param form_names character vector of form_names to be included
#' @param filter_field character string of extra variable name to be filtered by if present in a data.frame
#' @param filter_choices character vector of extra variable values to be filtered by if present in a data.frame
#' @param warn_only logical for warn_only or stop
#' @return DB object that has been filtered to only include the specified records
#' @export
filter_DB <- function(DB, filter_field, filter_choices, form_names, field_names, warn_only = F){#, ignore_incomplete=F, ignore_unverified = F
  if(missing(field_names))field_names <- DB %>% get_all_field_names()
  if(is.null(field_names))field_names <- DB %>% get_all_field_names()
  if(missing(form_names))form_names <- names(DB$data)
  if(is.null(form_names))form_names <- names(DB$data)
  selected <- list()
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  is_key <- filter_field %in% form_key_cols
  if(!is_key){
    form_name <- field_names_to_form_names(DB,field_names = filter_field)
    is_repeating_filter <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==form_name)]
  }
  for(FORM in form_names){
    DF <- DB$data[[FORM]]
    is_repeating_form <- DB$metadata$forms$repeating[which(DB$metadata$forms$form_name==FORM)]
    if(is_something(DF)){
      filter_field_final <- filter_field
      filter_choices_final <- filter_choices
      if(!is_key){
        if(is_repeating_filter){
          if(!is_repeating_form){
            filter_field_final <- DB$metadata$form_key_cols[[FORM]]
            filter_choices_final <- DB$data[[form_name]][[filter_field_final]][which(DB$data[[form_name]][[filter_field]]%in%filter_choices)] %>% unique()
          }
        }
      }
      rows <-which(DB$data[[FORM]][[filter_field_final]]%in%filter_choices_final)
      cols <- colnames(DF)[which(colnames(DF)%in%c(DB$metadata$form_key_cols[[FORM]],field_names))]
      if(length(rows)>0&&length(cols)>0)selected[[FORM]] <- DF[rows,cols]
    }
  }
  return(selected)
}
field_names_metadata <- function(DB,field_names,col_names){
  fields <- get_original_fields(DB) #DB$metadata$fields
  # if(!deparse(substitute(FORM))%in%DB$metadata$forms$form_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  BAD <- field_names[which(!field_names%in%c(DB$metadata$fields$field_name,DB$redcap$raw_structure_cols,"arm_num","event_name"))]
  if(length(BAD)>0)stop("All column names in your form must match items in your metadata, `DB$metadata$fields$field_name`... ", paste0(BAD, collapse = ", "))
  # metadata <- DB$metadata$fields[which(DB$metadata$fields$form_name%in%instruments),]
  fields <- fields[which(fields$field_name%in%field_names),]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  if( ! missing(col_names)){
    if(is_something(col_names))fields <- fields[[col_names]]
  }
  return(fields)
}
filter_fields_from_form <- function(FORM,DB){
  forms <- DB %>% field_names_to_form_names(field_names = colnames(FORM))
  if(any(forms%in%get_original_forms(DB)$repeating))stop("All column names in your form must match only one form in your metadata, `DB$metadata$forms$form_name`, unless they are all non-repeating")
  fields <- DB %>% field_names_metadata(field_names = colnames(FORM))
  fields <- fields[which(fields$field_type!="descriptive"),]
  fields$has_choices <- !is.na(fields$select_choices_or_calculations)
  return(fields)
}
#' @title Clean to Raw REDCap forms
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
  fields <- filter_fields_from_form(FORM = FORM,DB = DB)
  for(i in 1:nrow(fields)){ # i <-  1:nrow(fields) %>% sample(1)
    COL_NAME <- fields$field_name[i]
    has_choices <- fields$has_choices[i]
    if(has_choices){
      z <- fields$select_choices_or_calculations[i] %>% split_choices()
      FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
        OUT <- NA
        if(!is.na(C)){
          coded_redcap <- which(z$name==C)
          if(length(coded_redcap)>0){
            OUT <- z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2 <- which(DB$metadata$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT <- DB$metadata$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }else{
      if(use_missing_codes){
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- C
          if(!is.na(C)){
            D <- which(DB$metadata$missing_codes$name==C)
            if(length(D)>0){
              OUT <- DB$metadata$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}
#' @title Raw to Labelled REDCap forms
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @return DB object
#' @export
raw_to_labelled_form <- function(FORM,DB){
  if(nrow(FORM)>0){
    use_missing_codes <- is.data.frame(DB$metadata$missing_codes)
    metadata <- filter_fields_from_form(FORM = FORM,DB = DB)
    for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
      COL_NAME <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if(has_choices){
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- NA
          if(!is.na(C)){
            coded_redcap <- which(z$code==C)
            if(length(coded_redcap)>0){
              OUT <- z$name[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2 <- which(DB$metadata$missing_codes$code==C)
                if(length(coded_redcap2)>0){
                  OUT <- DB$metadata$missing_codes$name[coded_redcap2]
                }else{
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
                }
              }else{
                warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }else{
        if(use_missing_codes){
          z <- DB$metadata$missing_codes
          FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
            OUT <- C
            if(!is.na(C)){
              D <- which(z$code==C)
              if(length(D)>0){
                OUT <- z$name[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  FORM
}
labelled_to_raw_DB <- function(DB){
  DB <- validate_DB(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data)){
    DB$data[[TABLE]] <- labelled_to_raw_form(FORM = DB$data[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
}
