
#' @description Calcule
#' @export 
simulate <- function(source, provided_age_exo=-1) {
  
  # 0 = TauxPlein ; 3 = EXO
  comportement=0


  data("test")
  simul=test

  conv = list(ech=c('Id', 'sexe','findet',
                    'typeFP',
                    'anaiss',
                    'neFrance',
                    'moisnaiss',
                    'emigrant',
                    'tresdip',
                    'peudip',
                    'dipl'),
              emp=c('Id',
                    'age',
                    'statut'),
              fam=c('pere',
                    'mere',
                    'enf1',
                    'enf2',
                    'enf3',
                    'enf4',
                    'enf5',
                    'enf6',
                    'matri',
                    'Id',
                    'annee',
                    'conjoint'
              ))
  for (field in c('ech', 'emp', 'fam')){
    simul[[field]] = openxlsx::read.xlsx(sourcepath, field)
    for (d in conv[[field]]) {
      simul[[field]][[d]] = as.integer(simul[[field]][[d]])
    }
  }

  if(length(which(simul$ech$age_exo > 0))) {
    comportement=3
  }

  age_exo=0
  if (provided_age_exo >= 0) {
    age_exo=provided_age_exo
    comportement=3
    if (provided_age_exo>0) {
      simul$ech$age_exo=provided_age_exo
    }
  }

  champ<-"FE" # ou  FM
  fin_simul<-2070 #2110 au maximum ou 2070 plus classiquement
  simul$options_salaires <- list()    

  simul$options <- list(tp=F,anLeg=2019,pas1=3/12,pas2=1,
                    AN_MAX=as.integer(fin_simul),champ,
                    NoAccordAgircArrco=F, NoRegUniqAgircArrco=T, 
                    SecondLiq=F,mort_diff_dip=T,effet_hrzn=T,
                    comp=comportement,# 0 = TP ; 3 = EXO
                    ecrit_dr_test=T,
                    NoCoeffTemp=T,
                    codeRegime=as.integer(regime)
                    )

  ################
  #choix du scenario demographique 
  ################
  # ici la fecondite, l'esperance de vie et le solde migratoire suivent le scenario central des projections de l'Insee
  # pour la France entiere (attention a la coherence avec le champ precedemment choisi)
  # deux autres scenarios sont deja crees le premier ou tous les scenarios sont a bas et ce qui aboutit a une population agee
  # le second tous les scenarios sont a haut et ce qui aboutit a une population jeune    
  # les autres scenarios s'obtiennent en utilisant le programme \data_raw\obtention_hypdemo.R    
  data("fec_Cent_vie_Cent_mig_Cent")
  demo=fec_Cent_vie_Cent_mig_Cent
  #rm(fec_Cent_vie_Cent_mig_Cent)

  ############
  #chargement des equations regissant le marche du travail, de sante 
  ################

  data("eq_struct")

  ###################
  #chargement des parametres economiques puis projection des parametres dans le futur
  ##################
  library(dplyr)

  eco=as.list(get_macro())
  demo=as.list(demo)
  simullist=as.list(simul)
  eq_struct=as.list(eq_struct)
  simulation=as.environment(c(demo,eco,simullist,eq_struct))

  demoSimulation = as.environment(simulation)
  destinieSim(demoSimulation)

  if (length(demoSimulation$retraites)) {
    # détermination des derniers salaires
    age_depart = demoSimulation$retraites %>% group_by(Id) %>% filter(pension != 0) %>%
      summarise(age=min(age)) %>% select(Id,age)
    dernier_salaire_net_b = demoSimulation$salairenet %>% inner_join(age_depart, by=c('Id', 'age')) %>%
      select(Id, age, dernier_salaire_net=salaires_net)
    naiss = demoSimulation$ech %>% select(Id, anaiss)
    derniers_salaires_nets = dernier_salaire_net_b %>% left_join(by='Id', naiss) %>%
      mutate(annee=age+anaiss) %>% select(Id, annee, dernier_salaire_net)

    # desindexation infla
    base=2019
    adjust_infla = demoSimulation$macro %>% select(annee, Prix)
    adjust_infla$ref = adjust_infla$Prix / adjust_infla$Prix[which(adjust_infla$annee == base)]

    deflate <- function(df, ...) {
      name_it <- function(n) {
        paste0(quo_name(n), '_neut')
      }
      expr_it <- function(n) {
        quo((!! n) / ref)
      }
      exprs <- lapply(enquos(...), expr_it)
      names(exprs) <- lapply(enquos(...), name_it)
      return(
        df %>%
          left_join(adjust_infla %>% select(annee, ref), by="annee") %>%
          mutate(!!! exprs) %>% select(-ref)
      )
    }
    dernier_salaire_net_n = derniers_salaires_nets %>% deflate(dernier_salaire_net) %>%
      select(Id, dernier_salaire_net_neut)
    demoSimulation$retraites <- demoSimulation$retraites %>%
      deflate(pension, retraite_nette) %>%
      left_join(by='Id', dernier_salaire_net_n) %>%
      mutate(
        pension_m = pension / 12,
        pension_neut_m = pension_neut / 12,
        retraite_nette_m = retraite_nette / 12,
        retraite_nette_neut_m = retraite_nette_neut / 12,
        TR_net_neut = retraite_nette_neut / dernier_salaire_net_neut
      )
    #print(demoSimulation$retraites %>% select(annee, pension_neut_m))

    demoSimulation$taux_remplacement = demoSimulation$retraites %>% group_by(Id) %>% mutate(debut=min(annee)) %>% filter(annee == debut) %>% select(Id, TR_net_neut)

    demoSimulation$taux_remplacement_brut <- inner_join(
      demoSimulation$retraites %>% group_by(Id) %>% mutate(debut=min(annee)) %>% filter(annee == debut) %>% select(Id, age, pension),
      demoSimulation$emp %>% group_by(Id) %>% mutate(age = age+1),
      by = c("Id", "age")
    ) %>% select(Id, age, pension, salaire) %>% mutate(TR_brut = pension/salaire)
  }

  if(T) {
    ## Create a new workbook
    wb <- openxlsx::createWorkbook("fullhouse")
    for (field in c("ech", "emp", "fam", "liquidations", "retraites", "salairenet", "taux_remplacement", "taux_remplacement_brut", "cotisations", "macro")){
      openxlsx::addWorksheet(wb, field)
      openxlsx::writeData(wb, field, demoSimulation[[field]])
    }
    ## Save workbook
    outputfile = str_c(str_sub(sourcepath,end=-6), 'results.xlsx', sep=".")
    openxlsx::saveWorkbook(wb, outputfile, overwrite = TRUE)
  }

  return(outputfile)
}
