
#' @description Génère un fichier de carrière à partir d'un fichier de paramètres 
#' @export 
run_generator <- function(config_path) {
  input = fromJSON(config_path)
  carrieres_path=input$carrieres_path[1]
  meta = openxlsx::read.xlsx(carrieres_path, 'meta')
  series = openxlsx::read.xlsx(carrieres_path, 'serie')
  age_exo=0

  naissance = as.integer(input$naissance[1])
  age_mort=80
  debut=as.integer(input$debut[1])
  p_proportion = input$proportion
  profil_id=input$carriere
  base_id=id=meta$base[meta$id == profil_id]

  duree_carriere=age_mort-debut

  profil = series[[profil_id]][(debut):(debut+duree_carriere-1)]
  if (is.null(profil)) {
    profil = rep(1, duree_carriere)
  }

  eco = get_macro()
  base = eco$macro[[base_id]]

  statut = c(rep(63,debut),rep(1, age_mort-debut))
  salaire = c(
    rep(0,debut),
    p_proportion * profil * base[(naissance+debut-1900+1):((naissance+debut-1900+1)+duree_carriere-1)],
    rep(0, age_mort-debut-duree_carriere)
  )

  ech = data.frame(Id=as.integer(1),
                        sexe=as.integer(1),
                        findet=as.integer(0),
                        typeFP=as.integer(20),
                        anaiss=naissance,
                        neFrance=as.integer(1),
                        k=0,
                        taux_prim=0,
                        moisnaiss=as.integer(1),
                        emigrant=as.integer(0),
                        tresdip=as.integer(0),
                        peudip=as.integer(0),
                        dipl=as.integer(0),
                        age_exo=age_exo
                        )

  emp = data.frame(Id=as.integer(1),
                        age=as.integer(0:(age_mort-1)),
                        statut=as.integer(statut),
                        salaire=salaire)

  fam = data.frame(Id=as.integer(1),
                        pere=as.integer(0),
                        mere=as.integer(0),
                        enf1=as.integer(0),
                        enf2=as.integer(0),
                        enf3=as.integer(0),
                        enf4=as.integer(0),
                        enf5=as.integer(0),
                        enf6=as.integer(0),
                        matri=as.integer(1),
                        annee=as.integer(2019),
                        conjoint=as.integer(0))

  wb <- openxlsx::createWorkbook("fullhouse")
  env = environment()
  for (field in c("ech", "emp", "fam")){
    openxlsx::addWorksheet(wb, field)
    openxlsx::writeData(wb, field, env[[field]])
  }
  prefix = str_sub(config_path,0,-6)
  generated_path = paste0(prefix, '.xlsx')
  ## Save workbook
  openxlsx::saveWorkbook(wb, generated_path, overwrite = TRUE)
  return(generated_path)
}
