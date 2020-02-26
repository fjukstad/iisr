

#' Read IIS log files from a directory
#'
#' @export
read.iis <- function(dir,
                     extension = "log",
                     filenames = NULL,
                     max_log_files_to_read = 100,
                     uri_stem = NULL,
                     skip = 4,
                     columns = NULL) {
  collected_log_files = 0
  log = NULL

  path = file.path(dir)
  folders = list.files(path)

  for (folder in folders) {
    path = file.path(dir, folder)

    log_filenames = list.files(path, pattern = paste0("*.", extension))

    # if(!is.null(log_filenames)){
    #   log_filenames = log_filenames[stringr::str_detect(log_filenames, paste("\\b",filenames,"\\b", sep="", collapse = "|"))]
    # }

    for (filename in log_filenames) {
      if (collected_log_files >= max_log_files_to_read) {
        break

      }

      fullpath = file.path(path, filename)

      temp_log = read.single_iis_logfile(fullpath, uri_stem, skip, columns = columns)

      temp_log = temp_log %>% dplyr::mutate(server = folder)

      log = dplyr::bind_rows(log, temp_log)

      collected_log_files = collected_log_files + 1

    }
  }

  return(log)

}



#' Reads a single IIS log file
#'
#' @export
read.single_iis_logfile <- function(filename,
                                    uri_stem = NULL,
                                    skip = 4,
                                    columns = NULL) {
  # default iis columns
  if(is.null(columns)) {
    columns = c(
      'date',
      'time',
      's_ip',
      'cs_method',
      'cs_uri_stem',
      'cs_uri_query',
      's_port',
      'cs_username',
      'c_ip',
      'cs_User_Agent_',
      'cs_Referer_',
      'sc_status',
      'sc_substatus',
      'sc_win32_status',
      'time_taken',
      'local_datetime',
      'time_taken_s',
      'datetimeStarted',
      'datetimeEnded'
    )
  }

  log = readr::read_delim(filename,
                          skip = skip,
                          col_names = columns,
                          delim = " ") %>%
    dplyr::mutate(filename = filename)
  #%>%
  #  dplyr::filter(!is.na(time_taken))

  if (!is.null(uri_stem)) {
    log = log %>%  dplyr::filter(stringr::str_detect(cs_uri_stem, pattern =
                                                       paste(uri_stem, collapse = "|")))
  }
  return(log)
}
