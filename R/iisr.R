# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

read.iis <- function() {

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

  path = file.path(params$input_directory)
  folders = list.files(path)

  for (folder in folders) {
    path = file.path(params$input_directory, folder)
    log_filenames = list.files(path, pattern = "*.log")
    for (filename in log_filenames) {

      if(collected_log_files >= params$max_log_files_to_read){
        break;
      }

      fullpath = file.path(path, filename)
      temp_log = read_delim(fullpath,
                            skip = 4,
                            col_names = columns,
                            delim = " ") %>%
        mutate(server = folder) %>%
        mutate(filename = fullpath) %>%
        filter(!is.na(time_taken))

      log = bind_rows(log, temp_log)

      collected_log_files = collected_log_files + 1

    }
  }

  return (log);
}
