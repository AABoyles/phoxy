#' Ingest the Phoenix Dataset
#'
#' Given a directory with individual Phoenix dataset files, quickly read
#' them all in, name them correctly, and combine them into one dataframe.
#'
#' @param dir The path to the Phoenix folder.
#' @param phoenix_version [Not yet implemented]. Use the appropriate function for each Phoenix version.
#' @param read_func [Not yet implemented]. Use an alternative reading function like \code{fread} or \code{read_csv}.
#'
#' @return A single dataframe with all the Phoenix events in the folder.
#' @author Andy Halterman
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' events <- ingest_phoenix("~/OEDA/phoxy_test/")
#'
#' @rdname ingest_phoenix
#' @export
ingest_phoenix <- function(dir, phoenix_version = "auto", read_func = "read.csv", processing_function){
  # Handle messy file paths
  lastletter <- stringr::str_sub(dir ,-1, -1)
  if (lastletter != "/"){
    dir <- paste0(dir, "/")
  }
  
  ## List files
  files <- list.files(dir)
  files <- paste0(dir, files)

  ## Set column dtypes
  coltypes <- c('character', rep('integer', 4), rep('character', 8)
                , rep('integer', 3),  'numeric', 'character', 'numeric'
                , 'numeric', rep('character', 6))
  
  ## Quick and dirty: fread all files
  read_one <- function(file){
    t <- tryCatch(fread(file, stringsAsFactors = F, sep = '\t'
                        , colClasses = coltypes, na.strings = '')
                  , error = function(e) message(paste0('error reading ', file)))
    if(is.null(t) == F){
      return(t)
    } else {
      message('object is not a data.frame')
    }
  }

  message("Reading in files...")
  event_list  <- plyr::llply(files, read_one, .progress = plyr::progress_text(char = '='))

  # Bind everything together
  events <- rbindlist(event_list)
  setnames(events, c("event_id", "date", "Year", "Month", "Day", "SourceActorFull",
                     "sourceactorentity", "SourceActorRole", "SourceActorAttribute",
                     "TargetActorFull", "targetactorentity", "TargetActorRole",
                     "TargetActorAttribute", "eventcode", "rootcode", "pentaclass",
                     "GoldsteinScore", "Issues", "Lat", "Lon",
                     "LocationName", "StateName", "CountryCode", "SentenceID", "URLs",
                     "NewsSources"))
  events$Date <- as.Date(lubridate::ymd(events$Date))  # use lubridate, then de-POSIX the date.
  events$Year <- as.integer(events$Year)
  events$Month <- as.integer(events$Month)
  events$Day <- as.integer(events$Day)
  events$GoldsteinScore <- as.numeric(events$GoldsteinScore)
  events$Lat <- as.numeric(events$Lat)
  events$Lon <- as.numeric(events$Lon)

  message("Process complete")
  return(events)
}


