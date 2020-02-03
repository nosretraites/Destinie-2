library(dplyr)
library(openxlsx)
library(stringr)
library(tidyr)

lib=Sys.getenv('DESTINIE_LIB_PATH')
if (str_length(lib)) {
  .libPaths(lib)
}
library(destinie)

args = commandArgs(trailingOnly = TRUE)
source = args[[1]]

shift_year(source, 1960)
