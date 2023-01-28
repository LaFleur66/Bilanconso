prod_commune <- read.csv2("production-electrique-par-filiere-a-la-maille-commune.csv")
prod_region <- read.csv2("production-electrique-par-filiere-a-la-maille-region.csv")
prod_departement <- read.csv2("production-electrique-par-filiere-a-la-maille-departement.csv")

conso_commune <- read.csv2("consommation-electrique-par-secteur-dactivite-commune.csv")
conso_region <- read.csv2("consommation-electrique-par-secteur-dactivite-region.csv")
conso_departement <- read.csv2("consommation-electrique-par-secteur-dactivite-departement.csv")

conso_region <- conso_region %>% select("Année", "Code.Région", "Nom.Région", "CODE.GRAND.SECTEUR", "Conso.totale..MWh.", "Conso.moyenne..MWh.")
conso_departement <- conso_departement %>% select("Année", "Code.Département", "Nom.Département", "Code.Région", "Nom.Région", "CODE.GRAND.SECTEUR", "Conso.totale..MWh.", "Conso.moyenne..MWh.")
conso_commune <- conso_commune %>% select("Année", "Code.Commune", "Nom.Commune", "Code.Département", "Nom.Département", "Code.Région", "Nom.Région", "CODE.GRAND.SECTEUR", "Conso.totale..MWh.", "Conso.moyenne..MWh.")

prod_commune <- prod_commune %>% select("Année", "Nom.commune", "Code.commune", "Nom.département", "Code.département", "Nom.région", "Code.région", "Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.", "Energie.produite.annuelle.Eolien.Enedis..MWh.", "Energie.produite.annuelle.Hydraulique.Enedis..MWh.", "Energie.produite.annuelle.Bio.Energie.Enedis..MWh.", "Energie.produite.annuelle.Cogénération.Enedis..MWh.", "Energie.produite.annuelle.Autres.filières.Enedis..MWh." )
prod_departement <- prod_departement %>% select("Année", "Nom.département", "Code.département", "Nom.région", "Code.région", "Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.", "Energie.produite.annuelle.Eolien.Enedis..MWh.", "Energie.produite.annuelle.Hydraulique.Enedis..MWh.", "Energie.produite.annuelle.Bio.Energie.Enedis..MWh.", "Energie.produite.annuelle.Cogénération.Enedis..MWh.", "Energie.produite.annuelle.Autres.filières.Enedis..MWh." )
prod_region <- prod_region %>% select("Année", "Nom.région", "Code.région", "Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.", "Energie.produite.annuelle.Eolien.Enedis..MWh.", "Energie.produite.annuelle.Hydraulique.Enedis..MWh.", "Energie.produite.annuelle.Bio.Energie.Enedis..MWh.", "Energie.produite.annuelle.Cogénération.Enedis..MWh.", "Energie.produite.annuelle.Autres.filières.Enedis..MWh." )

prod_region[is.na(prod_region)] <- 0
prod_departement[is.na(prod_departement)] <- 0
prod_commune[is.na(prod_commune)] <- 0

conso_departement[is.na(conso_departement)] <- 0
conso_region[is.na(conso_region)] <- 0
conso_commune[is.na(conso_commune)] <- 0



cleanames <- function(x){
  p<-colnames(x)
  p<-str_to_lower(p)
  p<-str_trim(p)
  p<-str_replace_all(p, pattern = "[éèê]+", replacement = "e")
  p<-str_replace_all(p, pattern = "[.]+", replacement = "_")
  colnames(x)<-p
  return(x)
} 

colnames(prod_commune) <- c("annee", "nom_commune", "code_commune", "nom_departement", "code_departement", "nom_region", "code_region", "prod_phot", "prod_eol", "prod_hyd", "prod_bio", "prod_coge", "prod_autre")
colnames(prod_departement) <- c("annee", "nom_departement", "code_departement", "nom_region", "code_region", "prod_phot", "prod_eol", "prod_hyd", "prod_bio", "prod_coge", "prod_autre")
colnames(prod_region) <- c("annee", "nom_region", "code_region", "prod_phot", "prod_eol", "prod_hyd", "prod_bio", "prod_coge", "prod_autre")

conso_commune <- cleanames(conso_commune)
conso_region <- cleanames(conso_region)
conso_departement <- cleanames(conso_departement)


cleanames_column <- function(x){
  p <- x
  p<-str_to_lower(p)
  p<-str_trim(p)
  p<-str_replace_all(p, pattern = "[éèê]+", replacement = "e")
  p<-str_replace_all(p, pattern = "[.]+", replacement = "_")
  p<-str_replace_all(p, pattern = " ", replacement = "-")
  p<-str_replace_all(p, pattern = "î", replacement = "i")
  p<-str_replace_all(p, pattern = "ô", replacement = "o")
  return(p)
}


prod_commune$nom_commune <- cleanames_column(prod_commune$nom_commune)
prod_commune$nom_departement<- cleanames_column(prod_commune$nom_departement)
prod_commune$nom_region<- cleanames_column(prod_commune$nom_region)


prod_region$nom_region <- cleanames_column(prod_region$nom_region)

prod_departement$nom_departement<- cleanames_column(prod_departement$nom_departement)
prod_departement$nom_region<- cleanames_column(prod_departement$nom_region)


conso_commune$nom_commune <- cleanames_column(conso_commune$nom_commune)
conso_commune$nom_departement <- cleanames_column(conso_commune$nom_departement)
conso_commune$nom_region<- cleanames_column(conso_commune$nom_region)

conso_region$nom_region <- cleanames_column(conso_region$nom_region)

conso_departement$nom_departement <- cleanames_column(conso_departement$nom_departement)
conso_departement$nom_region <- cleanames_column(conso_departement$nom_region)

prod_region$prod_phot <- as.numeric(prod_region$prod_phot )
prod_region$prod_eol <- as.numeric(prod_region$prod_eol)
prod_region$prod_hyd <- as.numeric(prod_region$prod_hyd)
prod_region$prod_bio <- as.numeric(prod_region$prod_bio)
prod_region$prod_coge <- as.numeric(prod_region$prod_coge)
prod_region$prod_autre <- as.numeric(prod_region$prod_autre)

prod_departement$prod_phot <- as.numeric(prod_departement$prod_phot )
prod_departement$prod_eol <- as.numeric(prod_departement$prod_eol)
prod_departement$prod_hyd <- as.numeric(prod_departement$prod_hyd)
prod_departement$prod_bio <- as.numeric(prod_departement$prod_bio)
prod_departement$prod_coge <- as.numeric(prod_departement$prod_coge)
prod_departement$prod_autre <- as.numeric(prod_departement$prod_autre)

prod_commune$prod_phot <- as.numeric(prod_commune$prod_phot )
prod_commune$prod_eol <- as.numeric(prod_commune$prod_eol)
prod_commune$prod_hyd <- as.numeric(prod_commune$prod_hyd)
prod_commune$prod_bio <- as.numeric(prod_commune$prod_bio)
prod_commune$prod_coge <- as.numeric(prod_commune$prod_coge)
prod_commune$prod_autre <- as.numeric(prod_commune$prod_autre)



prod_region <- prod_region %>% group_by(annee, code_region) %>%
  summarise(nom_region = first(nom_region),
            prod_phot = sum(prod_phot),
            prod_eol = sum(prod_eol),
            prod_hyd = sum(prod_hyd),
            prod_bio = sum(prod_bio),
            prod_coge = sum(prod_coge),
            prod_autre = sum(prod_autre),
            .groups = 'drop')

prod_departement <- prod_departement %>% group_by(annee, code_departement) %>%
  summarise(nom_departement = first(nom_departement),
            code_region = first(code_region),
            nom_region = first(nom_region),
            prod_phot = sum(prod_phot),
            prod_eol = sum(prod_eol),
            prod_hyd = sum(prod_hyd),
            prod_bio = sum(prod_bio),
            prod_coge = sum(prod_coge),
            prod_autre = sum(prod_autre),
            .groups = 'drop')

prod_commune <- prod_commune %>% group_by(annee, code_commune) %>%
  summarise(nom_commune = first(nom_commune),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            code_region = first(code_region),
            nom_region = first(nom_region),
            prod_phot = sum(prod_phot),
            prod_eol = sum(prod_eol),
            prod_hyd = sum(prod_hyd),
            prod_bio = sum(prod_bio),
            prod_coge = sum(prod_coge),
            prod_autre = sum(prod_autre),
            .groups = 'drop')





prod_region <- prod_region %>%
  pivot_longer(
    cols = c("prod_phot", "prod_eol", "prod_hyd", "prod_bio", "prod_coge", "prod_autre"), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = FALSE)

prod_departement <- prod_departement %>%
  pivot_longer(
    cols = c("prod_phot", "prod_eol", "prod_hyd", "prod_bio", "prod_coge", "prod_autre"), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = FALSE)

prod_commune <- prod_commune %>%
  pivot_longer(
    cols = c("prod_phot", "prod_eol", "prod_hyd", "prod_bio", "prod_coge", "prod_autre"), names_to = "type_prod",
    names_prefix = "prod_", values_to = "prod_tot",
    values_drop_na = FALSE)


# Calcul et rajout des moyennes

prod_region$prod_tot <- as.numeric(prod_region$prod_tot)
prod_departement$prod_tot <- as.numeric(prod_departement$prod_tot)
prod_commune$prod_tot <- as.numeric(prod_commune$prod_tot)

conso_commune$conso_totale_mwh_ <- as.numeric(conso_commune$conso_totale_mwh_)
conso_departement$conso_totale_mwh_ <- as.numeric(conso_departement$conso_totale_mwh_)
conso_region$conso_totale_mwh_ <- as.numeric(conso_region$conso_totale_mwh_)




moy <- prod_region %>% group_by(annee, type_prod) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_region=0, nom_region="moyenne-france")

prod_region <- bind_rows(prod_region, moy)

moy <- prod_departement %>% group_by(annee, type_prod) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_departement=0, nom_departement="moyenne-france")

prod_departement <- bind_rows(prod_departement, moy)

moy <- prod_commune %>% group_by(annee, type_prod) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_commune=0, nom_commune="moyenne-france")

prod_commune <- bind_rows(prod_commune, moy)


moy <- conso_region %>% group_by(annee, code_grand_secteur) %>%
  summarise(conso_totale_mwh_ = mean(conso_totale_mwh_, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_region=0, nom_region="moyenne-france")

conso_region <- bind_rows(conso_region, moy)

moy <- conso_departement %>% group_by(annee, code_grand_secteur) %>%
  summarise(conso_totale_mwh_ = mean(conso_totale_mwh_, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_departement=0, nom_departement="moyenne-france")

conso_departement <- bind_rows(conso_departement, moy)

moy <- conso_commune %>% group_by(annee, code_grand_secteur) %>%
  summarise(conso_totale_mwh_ = mean(conso_totale_mwh_, na.rm = TRUE),
            .groups = 'drop') %>% mutate(code_commune=0, nom_commune="moyenne-france")

conso_commune <- bind_rows(conso_commune, moy)



moy_depart <- na.omit(prod_departement) %>% group_by(annee, type_prod, code_region) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            nom_region = first(nom_region),
            .groups = 'drop')
moy_depart["code_departement"] <- moy_depart$code_region * 100
moy_depart["nom_departement"] <- paste0("moyenne-",moy_depart$nom_region)

prod_departement <- bind_rows(prod_departement, moy_depart)


moy_commune1 <- na.omit(prod_commune) %>% group_by(annee, type_prod, code_region) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            nom_region = first(nom_region),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune1["code_commune"] <- moy_commune1$code_region * 10000
moy_commune1["nom_commune"] <- paste0("moyenne-",moy_commune1$nom_region)

prod_commune <- bind_rows(prod_commune, moy_commune1)


moy_commune2 <- na.omit(prod_commune) %>% group_by(annee, type_prod, code_departement) %>%
  summarise(prod_tot = mean(prod_tot, na.rm = TRUE),
            code_region = first(code_region),
            nom_region = first(nom_region),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune2["code_commune"] <- moy_commune2$code_region * 20000
moy_commune2["nom_commune"] <- paste0("moyenne-",moy_commune2$nom_departement)

prod_commune <- bind_rows(prod_commune, moy_commune2)




moy_depart <- na.omit(conso_departement) %>% group_by(annee, code_grand_secteur, code_region) %>%
  summarise(conso_totale_mwh_ = mean(conso_totale_mwh_, na.rm = TRUE),
            nom_region = first(nom_region),
            .groups = 'drop')
moy_depart["code_departement"] <- moy_depart$code_region * 100
moy_depart["nom_departement"] <- paste0("moyenne-",moy_depart$nom_region)

conso_departement <- bind_rows(conso_departement, moy_depart)


moy_commune1 <- na.omit(conso_commune) %>% group_by(annee, code_grand_secteur, code_region) %>%
  summarise(conso_totale_mwh_ = mean(conso_totale_mwh_, na.rm = TRUE),
            nom_region = first(nom_region),
            code_departement = first(code_departement),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune1["code_commune"] <- moy_commune1$code_region * 10000
moy_commune1["nom_commune"] <- paste0("moyenne-",moy_commune1$nom_region)

conso_commune <- bind_rows(conso_commune, moy_commune1)


moy_commune2 <- na.omit(conso_commune) %>% group_by(annee, code_grand_secteur, code_departement) %>%
  summarise(conso_totale_mwh_ = mean(conso_totale_mwh_, na.rm = TRUE),
            code_region = first(code_region),
            nom_region = first(nom_region),
            nom_departement = first(nom_departement),
            .groups = 'drop')

moy_commune2["code_commune"] <- moy_commune2$code_region * 20000
moy_commune2["nom_commune"] <- paste0("moyenne-",moy_commune2$nom_departement)

conso_commune <- bind_rows(conso_commune, moy_commune2)


saveRDS(conso_commune, "conso_commune_clean.rds")
saveRDS(conso_departement, "conso_departement_clean.rds")
saveRDS(conso_region, "conso_region_clean.rds")

saveRDS(prod_commune, "prod_commune_clean.rds")
saveRDS(prod_departement, "prod_departement_clean.rds")
saveRDS(prod_region, "prod_region_clean.RDS")

