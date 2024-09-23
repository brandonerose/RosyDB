#' @import RosyUtils
#' @import RosyApp
#' @title blank DB object
#' @return blank_DB list for reference
blank_DB <-  function(){ # can sort this better in version 3.0.0
  list(
    short_name=NULL,
    dir_path=NULL,
    # redcap = NULL,
    metadata = list(# model
      forms=NULL,
      fields=NULL,
      choices=NULL,
      forms_key_cols = NULL,
      arms=NULL,
      events=NULL,
      event_mapping = NULL,
      remap = list(
        forms=NULL,
        fields=NULL,
        choices=NULL,
        forms_key_cols = NULL,
        arms=NULL,
        events=NULL,
        event_mapping = NULL
      ),
      missing_codes=NULL
    ),
    data = NULL, #model
    data_update = NULL,
    quality_checks = NULL,
    tranformations = NULL,
    summary = list(
      subsets=NULL
    ),
    internals = list(
      last_metadata_update=NULL,
      last_metadata_dir_save=NULL,
      last_full_update=NULL,
      last_data_update=NULL,
      last_data_dir_save = NULL,
      last_data_transformation = NULL,
      last_summary = NULL,
      last_quality_check = NULL,
      last_clean = NULL,
      last_directory_save=NULL,
      data_extract_labelled = NULL,
      data_extract_merged = NULL,
      merge_form_name = "merged",
      reference_state = "data_extract",
      reference_metadata = "redcap",
      was_remapped = F,
      use_csv = F
    ),
    links = list(
      github = "https://github.com/brandonerose/RosyDB",
      thecodingdocs = "https://www.thecodingdocs.com/"
    )
  )
  # redcap = list(
  #   token_name=NULL,
  #   project_id=NULL,
  #   project_title= NULL,
  #   id_col=NULL,
  #   version=NULL,
  #   project_info=NULL,
  #   log=NULL,
  #   users=NULL,
  #   current_user=NULL,
  #   choices=NULL,
  #   raw_structure_cols = NULL,
  #   is_longitudinal = NULL,
  #   has_arms = NULL,
  #   has_multiple_arms = NULL,
  #   has_arms_that_matter = NULL,
  #   has_repeating_instruments_or_events = NULL,
  #   has_repeating_instruments = NULL,
  #   has_repeating_events = NULL
  # ),
}
validate_DB <- function(DB,silent = T,warn_only = F){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  outcome_valid <- T
  messages <- NULL
  if( ! all(names(blank_DB())%in%names(DB))){
    outcome_valid <- F
    messages <- messages %>% append("`DB` does not have the appropriate names. Did you use `load_DB()` or `setup_DB()` to generate it?")
  }
  if(is.null(DB$dir_path)){
    outcome_valid <- F
    messages <- messages %>% append("`DB$dir_path` is NULL!, Did you use `setup_DB()`?")
  }else{
    if( ! DB$dir_path %>% file.exists()) warning("`DB$dir_path`, '",DB$dir_path,"', does not exist!, Did you use `setup_DB()`?\nThis can also happen with shared directories.",immediate. = T)
  }
  if(is.null(DB$short_name)){
    outcome_valid <- F
    messages <- messages %>% append("`DB$short_name` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$short_name %>% validate_env_name()
  }
  if(!outcome_valid){
    for(m in messages){
      if(warn_only){
        warning(m,immediate. = T)
      }else{
        stop(m)
      }
    }
  }
  if(!silent){
    if((length(DB$data)==0)>0){
      warning("Valid list but no data yet!",immediate. = T)
    }
    if(outcome_valid){
      message("`DB` validated!")
    }
    message("DB Loaded!")
  }
  DB
}
#add year check
#' @title Setup for DB including token
#' @param short_name character name as a shortcut
#' @param dir_path character file path of the directory
#' @param token_name character string of what the token is called when using Sys.setenv and Sys.getenv
#' @param redcap_base character of the base REDCap link, ex. https://redcap.miami.edu
#' @param force logical for force blank load vs last save
#' @param validate logical for validation
#' @param merge_form_name name of merged non-repeating to be used in package
#' @return DB
#' @export
setup_DB <- function(short_name,dir_path,validate = T,use_csv = F){
  if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
  missing_dir_path <- missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, RosyDB will only run in R session. Package is best with a directory",immediate. = T)
    DB <- blank_DB()
  }
  if( ! missing_dir_path){
    DB <- load_DB(dir_path,validate = validate)
    DB$dir_path <- set_dir(dir_path)
  }
  DB$short_name <- validate_env_name(short_name)
  if(validate)DB <- validate_DB(DB)
  DB$internals$use_csv <- use_csv
  DB$data <- DB$data_extract %>% all_character_cols_list()
  return(DB)
}
#' @title Reads DB from the directory
#' @inheritParams setup_DB
#' @return DB
#' @export
load_DB <- function(short_name,validate = T){
  projects <- get_projects()
  if(nrow(projects)==0)return(blank_DB())
  #
  # DB_path <- file.path(dir_path,"R_objects","DB.rdata")
  # if(file.exists(DB_path)){
  #   DB  <- readRDS(file=DB_path)
  #   DB <- validate_DB(DB,silent = F,warn_only = !validate)
  # }else{
  #   DB <- blank_DB()
  # }
  DB
}
#' @title Saves DB in the directory
#' @param DB object generated using `load_DB()` or `setup_DB()`
#' @return Message
#' @export
save_DB <- function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  DB <- DB %>% validate_DB()
  DB$data_extract <- DB$data_extract %>% all_character_cols_list()
  DB %>% saveRDS(file=file.path(DB$dir_path,"R_objects","DB.rdata"))
  add_project(DB)
  # save_xls_wrapper(DB)
  message("Saved!")
}
#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param also_metadata logical for including metadata
#' @param data_choice whether to use 'data_extract' or 'data_transform'
#' @param only_dfs logical for including data.frames
#' @return DB tables
#' @export
show_DB <- function(DB,data_choice,also_metadata=T,only_dfs = T){
  DB <- validate_DB(DB)
  data_list <- list()
  if(missing(data_choice)){
    data_choice<- DB$internals$reference_state
  }
  DB[[data_choice]] %>% add_list_to_global(only_dfs = only_dfs)
  if(also_metadata){
    DB[["redcap"]] %>% add_list_to_global(only_dfs = only_dfs)
  }
  data_list %>% list2env(envir = .GlobalEnv)
}
#' @title Deletes DB object from directory (solves occasional problems)
#' @inheritParams save_DB
#' @inheritParams setup_DB
#' @param dir_path character file path of the directory
#' @return message
#' @export
delete_DB <- function(DB,dir_path){
  if(!missing(DB)){
    DB <- validate_DB(DB)
    DIR <- DB$dir_path
    if(!missing(dir_path))warning("You only need to provide a directory path using a DB object OR dir_path. DB will be used by default.",immediate. = T)
  } else {
    if(missing(dir_path))stop("You must provide a directory path using a DB object or dir_path")
    DIR <- dir_path
  }
  DIR  <- validate_dir(DIR,silent = F)
  delete_this <- file.path(DIR,"R_objects","DB.Rdata")
  if(file.exists(delete_this)){
    unlink(delete_this)
    message("Deleted saved DB")
  }else{
    warning("The DB object you wanted to is not there. Did you delete already? ",delete_this)
  }
}
