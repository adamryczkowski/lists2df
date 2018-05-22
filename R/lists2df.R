#' Transforms list of records into a data.frame.
#'
#' @param l List of records. It is assumed, that all items with the same name share the same data type
#' @param list_columns Character vector with the names of the fields that should be treated as a nested lists.
#'                     They may contain heterogenous contents or simply be atomic types with length more than 1.
#' @return Returns a data.table
#' @export
lists_to_df<-function(l, list_columns=character(0), unnamed_default=NA_character_) {
  classes<-purrr::map(l, class)
  idx_lists<-which(purrr::map_lgl(classes, ~'list' %in% .))
  cns<-unique(unlist(purrr::map(l[idx_lists], names)))
  unnamed_count<-max(purrr::map_int(l, ~length(.)-length(names(.))))
  if(unnamed_count>0) {
    unnamed_cns<-paste0('.col_', seq_len(unnamed_count))
  } else {
    unnamed_cns<-character(0)
  }
  nrow<-length(l)

  nas<-list()

  dt<-data.table::data.table(..delete=rep(NA, nrow))
  for(cn in cns) {
    if(cn %in% list_columns) {
      val<-list(list())
      nas[[cn]]<-NA
    } else {
      val<-l[[1]][[cn]]
      if('character' %in% class(val)) {
        val[[1]]<-NA_character_
      } else {
        val[[1]]<-NA
      }
      nas[[cn]]<-val
    }
    dt[[cn]]<-rep(val, nrow)
  }
  for(cn in unnamed_cns) {
    nas[[cn]]<-unnamed_default
    dt[[cn]]<-rep(unnamed_default, nrow)
  }

  for(i in seq(1, nrow)) {
    for(cn in cns) {
      val<-l[[i]][[cn]]
      if(is.null(val)) {
        val<-nas[[cn]]
      }
      if(cn %in% list_columns) {
        val<-list(list(val))
      } else {
      }
      tryCatch(
        data.table::set(dt, i, cn, val),
        error=function(e) {dt[[cn]][[i]]<<-val}
      )
    }
    if(unnamed_count>0) {
      item<-l[[i]]
      ucnt<-length(item) - length(names(item))
      unames<-names(item)
      if(is.null(unames)) {
        unames<-rep('', length(item))
      }
      un<-1
      for(j in seq_along(item)) {
        if(unames[[j]]=='') {
          val<-item[[j]]
          data.table::set(dt, i, unnamed_cns[[un]], val)
          un<-un+1
        }
      }
    }
  }
  data.table::set(dt, NULL, '..delete', NULL)
  #dt[['..delete']]<-NULL
  return(dt)
}
