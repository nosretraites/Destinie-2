

#' @description Importe les paramètres stockés dans les fichiers excel avec les options 
#' @export 
get_macro <- function() {
  fin_simul<-2070
    
  data("eco_cho_7_prod13")
  eco=eco_cho_7_prod13

  # Supprime  l'augmentation de la CSG prévue en 2018
  eco$macro$TauxCSGRetFort[eco$macro$annee==2018] = NaN

  #rm(eco_cho_7_prod13)
  eco$macro <-eco$macro%>%
    mutate( 
      SMPTp = ifelse(is.na(SMPTp),0,SMPTp),
      SMICp = ifelse(is.na(SMICp),0,SMICp),
      PIBp = ifelse(is.na(PIBp),0,PIBp),
      PlafondSSp = ifelse(is.na(PlafondSSp),0,PlafondSSp),
      Prixp = ifelse(is.na(Prixp),0,Prixp),
      MinPRp = 1.02,
      RevaloRG = ifelse(is.na(RevaloRG),1+Prixp,RevaloRG),
      RevaloFP = ifelse(is.na(RevaloFP),1+Prixp,RevaloFP),
      RevaloSPC = ifelse(is.na(RevaloSPC),1+Prixp,RevaloSPC)
    ) %>%
    projection(
      SMPT ~ cumprod((1+SMPTp)*(1+Prixp)),
      PIB ~ cumprod((1+PIBp)*(1+Prixp)),
      PlafondSS ~ cumprod((1+PlafondSSp)*(1+Prixp)),
      SMIC ~ cumprod((1+SMICp)*(1+Prixp)),
      Prix ~ cumprod(1+Prixp),
      PointFP|PlafRevRG ~ SMPT,
      SalValid ~ SMIC,
      PlafARS1|PlafARS2|PlafARS3|PlafARS4|PlafARS5|PlafCF3|PlafCF4|PlafCF5|
        MajoPlafCF|sGMP|BMAF|SeuilPauvrete ~ SMPT,
      MaxRevRG ~ PlafondSS,
      MinPR ~ cumprod(MinPRp*(1+Prixp)),
      MinVieil1|MinVieil2|Mincont1|Mincont2 ~ lag(cumprod(1+Prixp)), # indexation standard. En evolution, indexation sur l'inflation de t-1.
      SalRefAGIRC_ARRCO|SalRefARRCO|SalRefAGIRC ~  cumprod(ifelse(annee%in%c(2016,2017,2018),(1+SMPTp+0.02)*(1+Prixp),(1+SMPTp)*(1+Prixp))),
      ValPtAGIRC|ValPtARRCO|ValPtAGIRC_ARRCO ~ cumprod(1+ifelse(annee%in%c(2016,2017,2018),pmax(Prixp-0.01,0),Prixp)),
      MinRevRG|SeuilExoCSG|SeuilExoCSG2|SeuilTxReduitCSG|SeuilTxReduitCSG2 ~cumprod(1+Prixp),
      .~1
    )


  # Indexation des valeurs de points de la réforme
  total = length(eco$macro$annee)
  duree = 17 # année
  debut = 2025 # croissance de 2025 appliquée en 2026 cf lag dans seriep

  idebut = debut - 1900 + 1
  transition = 0:duree/duree
  propInfla = c(rep(0, idebut - 1), 1 - transition, rep(0, total - duree - idebut))
  propCroissance = c(rep(0, idebut - 1), transition, rep(1, total - duree - idebut))

  serieInfla = eco$macro$Prixp
  serieCroissanceFlat = eco$macro$SMPTp
  serieCroissance = (1 + serieCroissanceFlat) * (1 + serieInfla) - 1
  seriep = cumprod(1 + lag(serieInfla * propInfla + serieCroissance * propCroissance, default=0))

  eco$macro <-eco$macro%>% mutate(
    IndexationInflaReforme = propInfla,
    IndexationCroissanceReforme = propCroissance,
    CotisationReforme = 0.2531,
    ValeurAchatReforme = 10 * seriep,
    ValeurVenteReforme = 0.55 * seriep,
    AgeEquilibre = 65
  )

  eco$macro=eco$macro%>%filter(annee<=fin_simul)
  eco$CiblesTrans <-  left_join(eco$macro %>% select(annee), eco$CiblesTrans)

  return(eco)
}
