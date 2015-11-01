#' Ingest the Phoenix Dataset
#'
#' Given a directory with individual Phoenix dataset files, quickly read
#' them all in, name them correctly, and combine them into one dataframe.
#'
#' @param dir The path to the Phoenix folder.
#' @param start_date Start of date range as YYYYMMDD integer format.
#' @param end_date End of date range as YYYYMMDD integer format.
#'
#' @return A single dataframe with all the Phoenix events in the folder.
#' @author Andy Halterman, forked by Jesse Hammond
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' events <- ingest_phoenix("~/OEDA/phoxy_test/", 20140620, 20150101)
#'
#' @rdname ingest_phoenix
#' @export

ingest_phoenix <- function(dir, start_date, end_date){
  # Handle messy file paths
  lastletter <- stringr::str_sub(dir ,-1, -1)
  if (lastletter != "/"){
    dir <- paste0(dir, "/")
  }

  ## List files
  files <- list.files(dir)

  ## Pull files that fall in the date range provided
  filesdates <- as.integer(
    do.call('rbind', (stringr::str_split(files, '\\.')))[, 3])
  if(start_date < min(filesdates)){
    message('Note: specified range precedes the earliest Phoenix data.')
  }
  if(end_date > max(filesdates)){
    message('Note: specified range exceeds the latest Phoenix data. IT\'S NOT A CRYSTAL BALL PEOPLE')
  }
  files <- files[filesdates >= start_date & filesdates <= end_date]
  files <- paste0(dir, files)

  ## Set column dtypes
  coltypes <- c('character', rep('integer', 4), rep('character', 9)
                , rep('integer', 2),  'numeric', 'character', 'numeric'
                , 'numeric', rep('character', 6))
  ## Set column name
  phoenix_names <- c("event_id", "date", "Year", "Month", "Day", "SourceActorFull",
                     "sourceactorentity", "SourceActorRole", "SourceActorAttribute",
                     "TargetActorFull", "targetactorentity", "TargetActorRole",
                     "TargetActorAttribute", "eventcode", "rootcode", "pentaclass",
                     "GoldsteinScore", "Issues", "Lat", "Lon",
                     "LocationName", "StateName", "CountryCode", "SentenceID", "URLs",
                     "NewsSources")
  
  ## Quick and dirty: fread all files
  read_one <- function(file){
    t <- tryCatch(fread(file, stringsAsFactors = F, sep = '\t'
                        , colClasses = coltypes
                        , na.strings = ''
                        , col.names = phoenix_names)
                  , error = function(e) message(paste0('error reading ', file)))
    if(is.null(t) == F){
      return(t)
    } else {
      message('object is not a data.frame')
    }
  }

  message("Reading in files...")
  event_list  <- plyr::llply(files, read_one, .progress = plyr::progress_text(char = '='))

  ## Bind everything together
  events <- rbindlist(event_list)
  
  ## Convert dates to DATE object
#   events$date <- as.Date(lubridate::ymd(events$date))  # use lubridate, then de-POSIX the date.

  message("Process complete")
  print(head(event_list[[1]]))
  return(events)
}


