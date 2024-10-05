#' @import RosyUtils
#' @import RosyApp
#' @title Summarize DB
#' @param records character vector of records to be summarized
#' @param drop_blanks optional logical for dropping blanks
#' @export
summarize_DB <- function(DB,records = NULL,drop_blanks = T, data_choice = DB$internals$reference_state){
  #project --------
  # DB$summary$users <- DB$redcap$users
  df_names <- c("metadata","forms","event_mapping","events","arms")
  # if(data_choice == "data"){
  #   df_names <- c(df_names,paste0(df_names,"_remap"))
  #   redcap_remap <- "remap"
  # }
  for(i in 1:length(df_names)){
    x <- DB$metadata[[df_names[i]]]
    if(!is.null(x)) DB$summary[[df_names[i]]] <- x
  }
  #records belong to arms 1 to 1 ----------
  DB$summary$data_choice <- data_choice
  DB$summary$all_records_n <- 0
  if(!is.null(DB$summary$all_records)){
    original_data <- DB$data
    if(!is.null(records)){
      DB$data <- DB %>% filter_DB(records = records,data_choice = data_choice)
      DB$summary$selected_records <- DB$summary$all_records[which( DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
      DB$summary$selected_records_n <- DB$summary$selected_records %>% nrow()
    }
    DB$summary$all_records_n <- DB$summary$all_records %>% nrow()
    DB$summary$record_log_sum <- summarize_records_from_log(DB, records = records)
  }
  DB$summary$user_log_sum <- summarize_users_from_log(DB, records = records)
  #arms----------------
  DB$summary$arms_n <- NA
  if(is.data.frame(DB$metadata$arms)){
    DB$summary$arms_n <- DB$metadata$arms %>% nrow()
    id_pairs <- DB$metadata$forms$form_name %>%  lapply(function(IN){DB$data[[IN]][,c(DB$redcap$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
    DB$metadata$arms$arm_records_n <- DB$metadata$arms$arm_num %>% sapply(function(arm){
      which(id_pairs$arm_num==arm)%>% length()
    })
  }
  #events belong to arms many to 1 ----------------
  # DB$summary$events_n <- DB$metadata$events %>% nrow()
  DB$summary$events_n <- NA
  if(is.data.frame(DB$metadata$events)){
    DB$summary$events_n <- DB$metadata$events %>% nrow()
    DB$summary$event_names_n <- DB$metadata$events$event_name %>% unique() %>% length()
    # 1:nrow(DB$metadata$event_mapping) %>% lapply(function(i){
    #   (DB$data[[DB$metadata$event_mapping$form[i]]][['redcap_event_name']]==DB$metadata$event_mapping$unique_event_name[i]) %>% which() %>% length()
    # })
    # for(event in ){
    #   DB$summary[[paste0(event,"_records_n")]] <- DB$data[[]][which(DB$metadata$arms$arm_num==arm)]
    # }
  }
  #forms/forms belong to events many to 1 (if no events/arms) ----------------
  DB$summary$forms_n <- 0
  if(is.data.frame(DB$summary$forms)){ # can add expected later
    DB$summary$forms_n <- DB$summary$forms %>% nrow()
    # DB$summary$forms <- DB  %>% annotate_forms(DB$summary$forms)
  }
  #fields belong to forms/forms 1 to 1 ----------------
  DB$summary$metadata_n <- 0
  DB$summary$metadata_n <- DB$metadata$fields[which(!DB$metadata$fields$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
  # DB$metadata$fields$field_type[which(!DB$metadata$fields$field_type%in%c("checkbox_choice","descriptive"))] %>% table()
  DB$summary$metadata <- DB %>%  annotate_redcap_metadata(metadata = DB$summary$metadata, data_choice = data_choice)
  #metadata/codebook =============
  codebook <- fields_to_choices(DB$summary$metadata) %>% annotate_choices(metadata =DB$summary$metadata,data_choice = data_choice,DB = DB)
  if(drop_blanks) codebook <- codebook[which(codebook$n>0),]
  DB$metadata$choices <- codebook
  #cross_codebook ------
  DB$data <- original_data
  return(DB)
}
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
save_summary <- function(DB,with_links=T,dir_other = file.path(DB$dir_path,"output"),file_name = paste0(DB$short_name,"_RosyREDCap"),separate = F,data_choice = "data"){
  DB <- DB %>% validate_RosyREDCap()
  to_save_list <- append(DB$data,DB[["summary"]])
  to_save_list <- to_save_list[which(to_save_list %>% sapply(is.data.frame))]
  to_save_list <- to_save_list[which((to_save_list %>% sapply(nrow) %>% unlist())>0)]
  link_col_list <- list()
  if(with_links){
    to_save_list <-to_save_list %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
    link_col_list <- list(
      "redcap_link"
    )
    names(link_col_list) <- DB$redcap$id_col
  }
  if(DB$internals$use_csv){
    to_save_list %>% list_to_csv(
      dir = dir_other,
      file_name = file_name,
      overwrite = TRUE
    )
  }else{
    to_save_list %>% list_to_excel(
      dir = dir_other,
      separate = separate,
      link_col_list = link_col_list,
      file_name = file_name,
      header_df_list = to_save_list %>% construct_header_list(),
      key_cols_list = construct_key_col_list(DB),
      overwrite = TRUE
    )
  }
}
#' @export
stack_vars <- function(DB,vars,new_name,drop_na=T){
  DB <- validate_RosyREDCap(DB)
  metadata <- DB$metadata$fields
  if(DB$internals$is_transformed){
    metadata <- DB$remap$metadata_remap
  }
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
get_default_fields <- function(DB){
  if(DB$internals$is_transformed)return(DB$metadata$fields$field_name)
  return(DB$metadata$fields$field_name)
}
#' @export
get_all_field_names <- function(DB){
  return(DB$data %>% sapply(colnames) %>% unlist() %>% unique())
}
#' @title field_names_to_form_names
#' @export
field_names_to_form_names <- function(DB,field_names,only_unique = T){
  form_key_cols <- DB$metadata$form_key_cols %>% unlist() %>% unique()
  field_names <- field_names[which(!field_names%in%form_key_cols)]
  fields <- get_original_fields(DB)
  form_names <- fields$form_name[match(field_names, DB$metadata$fields$field_name)] %>% drop_nas()
  if(only_unique)form_names <- unique(form_names)
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
#' @param records character vector of the IDs you want to filter the DB by
#' @param field_names character vector of field_names to be included
#' @param form_names character vector of form_names to be included
#' @param add_filter_var character string of extra variable name to be filtered by if present in a data.frame
#' @param add_filter_vals character vector of extra variable values to be filtered by if present in a data.frame
#' @param warn_only logical for warn_only or stop
#' @return DB object that has been filtered to only include the specified records
#' @export
filter_DB <- function(DB, records,field_names,form_names,add_filter_var,add_filter_vals,warn_only = F){#, ignore_incomplete=F, ignore_unverified = F
  if(missing(records)) records <- DB$summary$all_records[[DB$redcap$id_col]]
  if(is.null(records)) records <- DB$summary$all_records[[DB$redcap$id_col]]
  if(missing(field_names)){
    field_names <- DB %>% get_all_field_names()
  }
  if(missing(form_names))form_names <- names(DB$data)
  if (length(records)==0)stop("Must supply records")
  selected <- list()
  BAD  <- records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  GOOD  <- records[which(records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0){
    m <- paste0("Following records are not found in DB: ", BAD %>% paste0(collapse = ", "))
    warn_or_stop(m,warn_only = warn_only)
  }
  run_add_filter <- !missing(add_filter_var)&&!missing(add_filter_vals)
  for(FORM in form_names){
    OUT <- DB$data[[FORM]][which(DB$data[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    cols <- colnames(OUT)[which(colnames(OUT)%in%field_names)]
    if(length(cols)>0){
      if(run_add_filter){
        if(add_filter_var %in% colnames(OUT)){
          OUT <-  OUT[which(OUT[[add_filter_var]] %in% add_filter_vals),]
        }
      }
      # if(nrow(OUT)>0){
      selected[[FORM]] <- OUT[,colnames(OUT)[which(colnames(OUT)%in%c(DB$redcap$raw_structure_cols,field_names))]]
      # }
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
  DB <- validate_RosyREDCap(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data)){
    DB$data[[TABLE]] <- labelled_to_raw_form(FORM = DB$data[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
}
