library(stringr)

lib=Sys.getenv('DESTINIE_LIB_PATH')
if (str_length(lib)) {
  .libPaths(lib)
}

library(destinie)
library(jsonlite)

args = commandArgs(trailingOnly = TRUE)
config_path = args[[1]]

run_generator(config_path)
