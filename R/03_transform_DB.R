#' @import RosyUtils
#' @import RosyApp
#' @title default_instrument_remap
#' @export
default_instrument_remap <- function(instruments_remap,merge_form_name = "merged"){
  instruments_remap$instrument_name_remap <- instruments_remap$instrument_name
  instruments_remap$instrument_label_remap <- instruments_remap$instrument_label
  instruments_remap <- instruments_remap[order(instruments_remap$repeating),]
  rep_rows <- which(instruments_remap$repeating)
  non_rep_rows <- which(!instruments_remap$repeating)
  reps <- instruments_remap$instrument_name[rep_rows]
  non_reps <- instruments_remap$instrument_name[non_rep_rows]
  lnr <- length(non_reps)
  lr <- length(reps)
  if(lnr>0){
    instruments_remap$instrument_name_remap[non_rep_rows] <- merge_form_name
    instruments_remap$instrument_label_remap[non_rep_rows] <- instruments_remap$instrument_label[which(instruments_remap$instrument_name==merge_form_name)]
  }
  return(instruments_remap)
}
#' @title default_transformation
#' @export
default_transformation <- function(DB){
  transformation_template <- DB$metadata$forms
  transformation_template <- transformation_template[order(transformation_template$repeating),]
  merge_form_name <- DB$internals$merge_form_name
  transformation_template$instrument_name_remap <- transformation_template$instrument_name
  transformation_template$instrument_label_remap <- transformation_template$instrument_label
  transformation_template$instrument_name_remap[which(!transformation_template$repeating)] <- merge_form_name
  merge_form_name_label <- merge_form_name
  if(merge_form_name %in% transformation_template$instrument_name){
    merge_form_name_label <- transformation_template$instrument_label[which(transformation_template$instrument_name==merge_form_name)]
  }
  transformation_template$instrument_label_remap[which(!transformation_template$repeating)] <- merge_form_name_label
  transformation_template$merge_to <- merge_form_name
  transformation_template$by.y <- transformation_template$by.x <- DB$metadata$form_key_cols[[merge_form_name]]
  transformation_template$x_first <- F
  transformation_template$x_first[which(transformation_template$repeating)] <- T
  return(transformation_template)
}
#' @title add_transformation_to_DB
#' @export
add_transformation_to_DB <- function(DB,tranformation_df,ask=T){
  tranformation_df_cols <-c(
    "instrument_name",
    "instrument_label",
    "repeating",
    "instrument_name_remap",
    "instrument_label_remap",
    "merge_to",
    "by.x",
    "by.y",
    "x_first"
  )
  if(any(!names(tranformation_df)%in%tranformation_df_cols)){
    bullet_in_console("Use `default_transformation(DB)` is an example!")
    stop("tranformation_df needs the following colnames... ", tranformation_df_cols %>% as_comma_string())
  }
  choice <- T
  if(!is.null(DB$transformation)){
    if(!identical(DB$transformation,tranformation_df)){
      if(ask){
        choice <- utils::askYesNo("Do you want to add transformation? (it doesn't match previous transform)")
      }
    }
  }
  #add more checks
  DB$transformation <- tranformation_df
  return(DB)
}
#' @title remap_named_df_list
#' @export
remap_named_df_list <- function(named_df_list,instruments_remap,merge_form_name = "merged",merge_list = NULL){
  forms <- unique(instruments_remap$instrument_name_remap)
  if(merge_form_name %in% forms){
    OUT[[merge_form_name]] <- merge_multiple(
      named_data_list = named_df_list,
      instrument_names = instruments_remap$instrument_name[which(instruments_remap$instrument_name_remap==merge_form_name)]
    )
    forms <- forms[which(forms!=merge_form_name)]
  }
  has_merge_list <- is_something(merge_list)
  for(TABLE in forms){
    OUT[[TABLE]] <- named_df_list[[TABLE]]
    if(has_merge_list){
      z<- merge_list[[TABLE]]
      if(is_something(z)){
        ref <- OUT[[TABLE]]
        mer <- named_df_list[[z$merge_to]]
        if(z$merge_to %in% names(OUT)){
          mer <- OUT[[z$merge_to]]
        }
        by.x <- z$by.x
        by.y <- z$by.y
        ref_names<-names(ref)
        mer_names<-names(mer)
        # new_name <- by.x %>% vec1_not_in_vec2(by.y)
        new_names <- ref_names %>% vec1_in_vec2(mer_names) %>% vec1_not_in_vec2(by.x)
        for(new_name in new_names){
          COL <- which(colnames(mer)==new_name)
          replace_name <- paste0(new_name,"_merged")
          a <- mer[,1:COL]
          a[[replace_name]] <- a[[COL]]
          b <- mer[,(COL+1):ncol(mer)]
          mer <- cbind(a,b)
        }
        bad_cols <- which(z$by.x!=z$by.y)
        z$by.x[bad_cols]
        z$by.y[bad_cols]
        if(length(bad_cols)>0){
          for(col in bad_cols){
            z$by.x[col] <- colnames(ref)[which(colnames(ref)== z$by.x[col])] <- paste0(z$by.y[col],"_merged")
          }
        }
        by.x <- z$by.x
        by.y <- z$by.y
        ref_names<-names(ref) %>% vec1_not_in_vec2(
          by.x %>% vec1_not_in_vec2(by.y)
        )
        mer_names<-names(mer)
        # new_name <- by.x %>% vec1_not_in_vec2(by.y)
        del_names <- mer_names %>% vec1_in_vec2(ref_names) %>% vec1_not_in_vec2(by.y)
        mer[,del_names] <- NULL
        all.x <- T
        if(is_something(z$all.x)){
          all.x <- z$all.x
        }
        a<- merge(
          x = ref,
          y = mer,
          by.x = by.x,
          by.y = by.y,
          all.x = all.x
        )
        all_names <- c(ref_names,names(mer)) %>% unique()
        if(is_something(z$x_first)){
          if(z$x_first){
            all_names <- c(by.y %>% vec1_in_vec2(by.x),names(mer) %>% vec1_not_in_vec2(by.y),ref_names) %>% unique()
          }
        }
        a<- a[,match(all_names,names(a))]
        OUT[[TABLE]] <- a
      }
    }
  }
  return(OUT)
}
#' @title purify_names_df_list
#' @export
purify_names_df_list <- function(named_df_list,instruments_remap,metadata,merge_list = NULL){
  if(!is_named_df_list(named_df_list)) stop("`named_df_list` is not a named df list")
  for(TABLE in names(named_df_list)){
    original_forms <- instruments_remap$instrument_name[which(instruments_remap$instrument_name_remap==TABLE)]
    DF <- named_df_list[[TABLE]]
    COLS <- c(merge_list[[TABLE]][["key"]],metadata$field_name[which(metadata$form_name%in%original_forms)]) %>% unique()
    COLS<- COLS %>% vec1_in_vec2(colnames(DF))
    DF <- DF[,COLS]
    named_df_list[[TABLE]] <- DF
  }
  return(named_df_list)
}
