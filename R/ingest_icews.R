#' Ingest the ICEWS Event Dataset
#'
#' Given a directory with individual ICEWS dataset files, quickly read 
#' them all in, name them correctly, and combine them into one dataframe.
#'
#' @param dir The path to the ICEWS folder.
#' @param read_func [Not yet implemented]. Use an alternative reading function like \code{data.table::fread} or \code{readr::read_delim}.
#'
#' @return A single dataframe with all the ICEWS events in the folder.
#' @author Andy Halterman
#' @note This function is still in development and may contain errors and change quickly.
#' @examples
#'
#' events <- ingest_icews("~/ICEWS/study_28075/Data/")
#' 
#' @rdname ingest_icews
#' @export
ingest_icews <- function(dir){  
  # Handle messy file paths
  lastletter <- stringr::str_sub(dir ,-1, -1)
  if (lastletter != "/"){
    dir <- paste0(dir, "/")
  }
  
  files <- list.files(dir)
  files <- files[grep("\\.tab$", files)] # quick regex in case of zips still there
  files <- paste0(dir, files)
  
  # Quick and dirty: fread all files
  read_one <- function(file){
    t <- tryCatch(fread(file, stringsAsFactors = F, sep = '\t')
                        , error = function(e) message(paste0('error reading ', file)))
    if(class(t)[1] == 'data.frame' & is.null(t) == F){
      return(t)
    } else {
      message('object is not a data.frame')
    }
  }
    
  message("Reading in files...")
  event_list  <- plyr::llply(files, read_one, .progress = plyr::progress_text(char = '='))
  # bind everything together. Surpress this warning: "Unequal factor levels: coercing to character"
  events <- dplyr::bind_rows(event_list)
  names(events) <- c("Event.ID", "Event.Date", "Source.Name", "Source.Sectors", 
                     "Source.Country", "Event.Text", "CAMEO.Code", "Intensity", "Target.Name", 
                     "Target.Sectors", "Target.Country", "Story.ID", "Sentence.Number", 
                     "Publisher", "City", "District", "Province", "Country", "Latitude", 
                     "Longitude")
  events$Event.Date <- as.Date(lubridate::ymd(events$Event.Date))  # use lubridate, then de-POSIX the date.
  events$Latitude <- as.numeric(events$Latitude)
  events$Longitude <- as.numeric(events$Longitude)
  events$Intensity <- as.numeric(events$Intensity)
  events$Sentence.Number <- as.integer(events$Sentence.Number)
  #eventColClasses <- c(rep("integer", 5), rep("character", 11), "numeric", "character", "numeric", 
  #                     "numeric", rep("character", 6))
  message("Process complete")
  return(events)
}


  