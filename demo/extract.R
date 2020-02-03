library(openxlsx)
library(dplyr)
library(stringr)

args = commandArgs(trailingOnly = TRUE)
source = args[[1]]

liquidations = read.xlsx(source, 'liquidations')
retraites = read.xlsx(source, 'retraites')
extracted_liquidations = unique(liquidations %>% group_by(Id) %>% summarise(annee=max(annee)) %>% inner_join(liquidations, by=c('Id', 'annee')))
extracted_retraites = retraites %>% group_by(Id) %>% summarise(age=min(age)) %>% inner_join(retraites, by=c('Id', 'age'))

env = environment()
wb <- createWorkbook('fullhouse')
for (field in c('extracted_liquidations', 'extracted_retraites')){
  addWorksheet(wb, field)
  writeData(wb, field, env[[field]])
}
outputfile = str_c(str_sub(source,end=-6), 'extract.xlsx', sep=".")
saveWorkbook(wb, outputfile, overwrite = TRUE)
