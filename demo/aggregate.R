library(openxlsx)
library(dplyr)
library(stringr)

args = commandArgs(trailingOnly = TRUE)
shifted = args[[1]]
reforme = args[[2]]
result_path = args[[3]]
field='taux_remplacement_brut'

shifted_ech = read.xlsx(shifted, 'ech')
shifted_tr = read.xlsx(shifted, field) %>% mutate(scenario='actuel') %>% left_join(shifted_ech %>% select(Id, anaiss), by="Id")

reforme_ech = read.xlsx(reforme, 'ech')
reforme_tr = read.xlsx(reforme, field) %>% mutate(scenario='reforme') %>% left_join(shifted_ech %>% select(Id, anaiss), by="Id")

fullset = bind_rows(shifted_tr, reforme_tr)
wb <- createWorkbook("fullhouse")
env = environment()
for (field in c("fullset")){
  addWorksheet(wb, field)
  writeData(wb, field, env[[field]])
}
saveWorkbook(wb, result_path, overwrite = TRUE)
