library(stringr)
lib=Sys.getenv('DESTINIE_LIB_PATH')
if (str_length(lib)) {
  .libPaths(lib)
}
library(destinie)

args = commandArgs(trailingOnly = TRUE)
source = args[[1]]

duplicate_ages(source, seq(55, 68))
