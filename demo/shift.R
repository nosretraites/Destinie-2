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
eco = destinie::get_macro()
macro = eco$macro

ref_year=1960

# Calcule nouvelle année de naissance décalée et ID
ech_base = read.xlsx(source, 'ech')
shift = ref_year - ech_base$anaiss

id_shift = ech_base$Id + max(ech_base$Id)
ech_shift = ech_base %>% mutate(anaiss = ref_year, Id = id_shift)
ech = bind_rows(ech_base, ech_shift)

# Calcule les données 'emploi' décalées
emp_base = read.xlsx(source, 'emp')
emp_shift = emp_base %>% mutate(
    annee = age + ech$anaiss,
    annee_shiftee = annee + shift,
    Id = id_shift,
) %>% left_join(macro, by='annee') %>%
  left_join(macro, by=c('annee_shiftee' = 'annee'), suffix=c('', '_shift'))

emp_shift = emp_shift %>% mutate(salaire_pourcentage = salaire / SMPT,
                                 salaire = salaire_pourcentage * SMPT_shift)

emp = bind_rows(emp_base, emp_shift)
emp = select(emp, Id, age, statut, salaire)

# Calcule le nouvel Id pour 'famille'
fam_base = read.xlsx(source, 'fam')
fam_shift = fam_base %>% mutate(Id = id_shift)
fam = bind_rows(fam_base, fam_shift)


wb <- createWorkbook("fullhouse")
env = environment()
for (field in c("ech", "emp", "fam")){
  addWorksheet(wb, field)
  writeData(wb, field, env[[field]])
}
prefix = str_sub(source,0,-6)
duplicate_path = paste0(prefix, '.shifte.xlsx')
saveWorkbook(wb, duplicate_path, overwrite = TRUE)
