library(dplyr)
library(openxlsx)
library(stringr)
library(tidyr)

source = "/home/lesly/Work/Destinie-2/demo/2020-02-02--11-07-56.118833-config.results.xlsx"
macro = data.frame(annee=1900:2070, SMPT=1900:2070 * 1.0)

ref_year=1960
shift = ref_year - ech$anaiss

# Calcule nouvelle année de naissance décalée et ID
ech_base = read.xlsx(source, 'ech')
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
## Save workbook
saveWorkbook(wb, duplicate_path, overwrite = TRUE)


# Rscript demo/simulation.R --age-exo 0 --library ~/R-tests --regime ACUTEL --file
