
#' Read IIS log files from a directory
#'
#' @export
read.iis <- function(dir,
                     extension="log",
                     filenames = NULL,
                     max_log_files_to_read=100) {

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

  collected_log_files = 0
  log = NULL

  path = file.path(dir)
  folders = list.files(path)

  for (folder in folders) {

    path = file.path(dir, folder)

    log_filenames = list.files(path, pattern = paste0("*.", extension))

    if(!is.null(filenames)){
      log_filenames = log_filenames[stringr::str_detect(log_filenames, paste("\\b",filenames,"\\b", sep="", collapse = "|"))]
    }

    for (filename in log_filenames) {

      if(collected_log_files >= max_log_files_to_read){
        break;
      }

      fullpath = file.path(path, filename)
      temp_log = readr::read_delim(fullpath,
                            skip = 4,
                            col_names = columns,
                            delim = " ") %>%
        dplyr::mutate(server = folder) %>%
        dplyr::mutate(filename = fullpath) %>%
        dplyr::filter(!is.na(time_taken))

      log = dplyr::bind_rows(log, temp_log)

      collected_log_files = collected_log_files + 1

    }
  }

  return(log);
}
