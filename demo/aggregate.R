library(stringr)
lib=Sys.getenv('DESTINIE_LIB_PATH')
if (str_length(lib)) {
  .libPaths(lib)
}
library(destinie)

args = commandArgs(trailingOnly = TRUE)
shifted = args[[1]]
reforme = args[[2]]
result_path = args[[3]]
aggregate_results(shifted, reforme, result_path)
