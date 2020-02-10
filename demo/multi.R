library(stringr)
lib=Sys.getenv('DESTINIE_LIB_PATH')
if (str_length(lib)) {
  .libPaths(lib)
}
library(destinie)
library(jsonlite)

args = commandArgs(trailingOnly = TRUE)
config_path = args[[1]]
result_path = args[[2]]

base_path = run_generator(config_path)
shifted_path = shift_year(base_path, 1960)
shifted = simulate(shifted_path, 'ACTUEL')
multiple_path = duplicate_ages(base_path, seq(55, 68))
reforme = simulate(multiple_path, 'COMM_PM', 0)
aggregate_results(shifted, reforme, result_path)
result_path
