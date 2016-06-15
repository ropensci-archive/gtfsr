#' Get a Dataframes of GTFS data.
#'
#' @param paths Character. url links to zip files OR paths to local zip files. if to local path, then option `local` must be set to TRUE.
#' @param local Boolean. If the paths are searching locally or not. Default is FALSE (that is, urls).
#' @param quiet Boolean. Whether to see file download progress and files extract. FALSE by default.
#'
#' @return Dataframes of GTFS data.
#'
#' @export

import_gtfs <- function(paths, local = FALSE, quiet = FALSE) {

  feed_flow <- function(url) {

    path <- get_feed(url = url, quiet = quiet)

    if(is.null(path)) return(NULL) else {
      zip_dir <- unzip_gtfs_files(zipfile = path, quiet = quiet)
    }

    read_gtfs(zip_dir, quiet = quiet)
  }

  # check if single column of data was inputed. if so, convert to vector; error otherwise.
  if(!is.null(dim(paths))) {
    if(dim(paths)[2] == 1) {
      paths <- unlist(paths, use.names = FALSE)
    } else {
      stop('Please input a vector or single column of data.')
    }
  }

  if(local) {
    paths <- paths %>% sapply(. %>% normalizePath)
    data_list <- paths %>% lapply(. %>% unzip_gtfs_files(quiet=quiet) %>% read_gtfs(quiet=quiet))
  } else {
    data_list <- paths %>% lapply(. %>% feed_flow)
  }

  if(length(data_list) > 1) return(data_list) else data_list[[1]]

}