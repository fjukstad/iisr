

#' Read IIS log files from a directory
#'
#' @export
read.iis <- function(dir,
                     extension = "log",
                     filenames = NULL,
                     max_log_files_to_read = 100,
                     cs_uri_stem = NULL,
                     skip = 4,
                     columns = NULL) {
  collected_log_files = 0
  log = NULL

  path = file.path(dir)
  folders = list.files(path)

  for (folder in folders) {
    path = file.path(dir, folder)

    log_filenames = list.files(path, pattern = paste0("*.", extension))

    if(!is.null(log_filenames)){
      log_filenames = log_filenames[stringr::str_detect(log_filenames, paste("\\b",filenames,"\\b", sep="", collapse = "|"))]
    }

    for (filename in log_filenames) {
      if (collected_log_files >= max_log_files_to_read) {
        break
      }

      fullpath = file.path(path, filename)
      message("Reading ", fullpath)

      temp_log = read.single_iis_logfile(fullpath,
                                         uri_stem = cs_uri_stem,
                                         skip = skip,
                                         columns = columns)

      # No entries in the log, e.g. if no entries match filter
      if(nrow(temp_log) == 0) {
        next;
      }

      temp_log = temp_log %>% dplyr::mutate(server = folder)
      temp_log = temp_log %>% dplyr::mutate(date_time = lubridate::ymd_hms(paste0(date,time)))

      log = dplyr::bind_rows(log, temp_log)

      collected_log_files = collected_log_files + 1

    }
  }
  rm(temp_log)
  return(log)

}



#' Reads a single IIS log file
#'
#' @export
read.single_iis_logfile <- function(filename,
                                    uri_stem = NULL,
                                    skip = 4,
                                    columns = NULL) {

  # If no columns specified, look for fields in IIS log file. Every IIS log file
  # looks like this:
  #
  #Software: Microsoft Internet Information Services 8.5
  #Version: 1.0
  #Date: 2020-02-20 00:00:01
  #Fields: date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) cs(Referer) sc-status sc-substatus sc-win32-status time-taken
  #
  # so we can skip to line 4 to get the column names
  if(is.null(columns)) {
    fields = scan(filename, '', skip = 3, nlines = 1, sep = '\n', quiet=T)
    fields = stringr::str_remove(fields, "#Fields: ")
    fields = stringr::str_replace_all(fields, "-", "_")
    columns = unlist(stringr::str_split(fields, " "))
  }

  log = readr::read_delim(filename,
                          skip = skip,
                          col_names = columns,
                          delim = " ") %>%
    dplyr::mutate(filename = filename)

   if (!is.null(uri_stem)) {
    log = log %>%
            dplyr::filter(stringr::str_detect(cs_uri_stem,
                                              pattern = paste(uri_stem, collapse = "|")))
  }

  return(log)
}
