
#' @description Importe les paramètres stockés dans les fichiers excel avec les options 
#' @export 
shift_year <- function(source, ref_year) {
  eco = destinie::get_macro()
  macro = eco$macro
    
  # Calcule nouvelle année de naissance décalée et ID
  ech_base = openxlsx::read.xlsx(source, 'ech')
  shift = ref_year - ech_base$anaiss

  id_shift = ech_base$Id + max(ech_base$Id)
  ech_shift = ech_base %>% mutate(anaiss = ref_year, Id = id_shift)
  ech = bind_rows(ech_base, ech_shift)

  # Calcule les données 'emploi' décalées
  emp_base = openxlsx::read.xlsx(source, 'emp')
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
  fam_base = openxlsx::read.xlsx(source, 'fam')
  fam_shift = fam_base %>% mutate(Id = id_shift)
  fam = bind_rows(fam_base, fam_shift)


  wb <- openxlsx::createWorkbook("fullhouse")
  env = environment()
  for (field in c("ech", "emp", "fam")){
    openxlsx::addWorksheet(wb, field)
    openxlsx::writeData(wb, field, env[[field]])
  }
  prefix = str_sub(source,0,-6)
  duplicate_path = paste0(prefix, '.shifte.xlsx')
  openxlsx::saveWorkbook(wb, duplicate_path, overwrite = TRUE)
  return(duplicate_path)
}

#' @description Importe les paramètres stockés dans les fichiers excel avec les options 
#' @export 
duplicate_ages <- function(source, age_exo) {

  ids = data.frame(Id=seq(1,length(age_exo)))
  input = ids %>% mutate(age_exo=age_exo)

  ech = crossing(input, openxlsx::read.xlsx(source, 'ech') %>% select(-Id, -age_exo))
  emp = crossing(ids, openxlsx::read.xlsx(source, 'emp') %>% select(-Id))
  fam = crossing(ids, openxlsx::read.xlsx(source, 'fam') %>% select(-Id))

  wb <- openxlsx::createWorkbook("fullhouse")
  env = environment()
  for (field in c("ech", "emp", "fam")){
    openxlsx::addWorksheet(wb, field)
    openxlsx::writeData(wb, field, env[[field]])
  }
  prefix = str_sub(source,0,-6)
  duplicate_path = paste0(prefix, '.multiple.xlsx')
  openxlsx::saveWorkbook(wb, duplicate_path, overwrite = TRUE)
  return(duplicate_path)
}

#' @description Importe les paramètres stockés dans les fichiers excel avec les options 
#' @export 
aggregate_results <- function(shifted, reforme, result_path) {
  field='taux_remplacement_brut'

  shifted_ech = openxlsx::read.xlsx(shifted, 'ech')
  shifted_tr = openxlsx::read.xlsx(shifted, field) %>% mutate(scenario='actuel') %>% left_join(shifted_ech %>% select(Id, anaiss), by="Id")

  reforme_ech = openxlsx::read.xlsx(reforme, 'ech')
  reforme_tr = openxlsx::read.xlsx(reforme, field) %>% mutate(scenario='reforme') %>% left_join(reforme_ech %>% select(Id, anaiss), by="Id")

  fullset = bind_rows(shifted_tr, reforme_tr)
  wb <- openxlsx::createWorkbook("fullhouse")
  env = environment()
  for (field in c("fullset")){
    openxlsx::addWorksheet(wb, field)
    openxlsx::writeData(wb, field, env[[field]])
  }
  openxlsx::saveWorkbook(wb, result_path, overwrite = TRUE)
  return(result_path)
}
