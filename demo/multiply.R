library(dplyr)
library(openxlsx)
library(stringr)
library(tidyr)

args = commandArgs(trailingOnly = TRUE)
source = args[[1]]
age_exo=seq(62, 68)

ids = data.frame(Id=seq(1,length(age_exo)))
input = ids %>% mutate(age_exo=age_exo)

ech = crossing(input, read.xlsx(source, 'ech') %>% select(-Id, -age_exo))
emp = crossing(ids, read.xlsx(source, 'emp') %>% select(-Id))
fam = crossing(ids, read.xlsx(source, 'fam') %>% select(-Id))

wb <- createWorkbook("fullhouse")
env = environment()
for (field in c("ech", "emp", "fam")){
  addWorksheet(wb, field)
  writeData(wb, field, env[[field]])
}
prefix = str_sub(source,0,-6)
duplicate_path = paste0(prefix, '.multiple.xlsx')
saveWorkbook(wb, duplicate_path, overwrite = TRUE)
