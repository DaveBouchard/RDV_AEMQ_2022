#1 Installer et charger les librairies utiles
list.of.packages <- c("rio", "tidyverse", "lubridate", "leaflet", "shiny", "shinyjs", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)
install_formats()
options(scipen = 999)
options(max.print = 99999)

#2 Spécifier où se trouve votre fichier Excel (***Attention*** on doit utiliser des / et non des \)
setwd("C:/Users/h.levesque/Desktop/AEMQ")

#3 Importer le fichier Excel ventes.xlsx
ventes = import("ventes.xlsx")

# Décrire les données
# Combien de ventes par année?
table(year(ventes$DATE_VENTE))
# Retirer la vente '2200' du jeu de données
ventes = ventes %>% filter(year(ventes$DATE_VENTE)<=2022)
# Nos UV les plus populeuses
uv = ventes %>% group_by(UV) %>% summarize(nb=n())
# Les types de propriétés les plus fréquents
type = ventes %>% group_by(type=str_c(GENRE,"_",LIEN)) %>% summarize(nb=n())
# Statistiques descriptives des prix de vente
summary(ventes$PRIX_VENTE)
sd(ventes$PRIX_VENTE)/mean(ventes$PRIX_VENTE)
ventes%>%group_by(year(ventes$DATE_VENTE))%>%summarize(nb=n(),prix=mean(PRIX_VENTE),cv=sd(PRIX_VENTE)/mean(PRIX_VENTE))
# Histogramme des prix de vente
ggplot(ventes, aes_string(x = ventes$PRIX_VENTE)) + geom_histogram(bins = 200) + labs(x = "Prix", y = "fréquence")
ggplot(ventes, aes_string(x = ventes$PRIX_VENTE)) + geom_histogram(bins = 200) + labs(x = "Prix", y = "fréquence") + facet_grid(LIEN ~ .)
# Carte des prix
pal <- colorQuantile(c("#0000FF", "#0080FF", "#FFFFFF", "#FF8000", "#FF0000"), ventes$PRIX_VENTE, n = 5)
leaflet(data = ventes) %>% addTiles() %>% addCircleMarkers(lng = ~ ventes$LONGITUDE, lat =  ~ ventes$LATITUDE, color=pal(ventes$PRIX_VENTE), stroke = FALSE, fillOpacity = 0.75, radius = 10, popup = paste(ventes$PRIX_VENTE))
# Fréquence et proportion des prix supérieurs à 1000000$
sum(ventes$PRIX_VENTE>1000000)
mean(ventes$PRIX_VENTE>1000000)
# Autres variables
summary(ventes$AIRE_TERRAIN)
summary(ventes$ANNEE_CONSTRUCTION)
summary(ventes$AIRE_ETAGES)
table(ventes$GENRE)
table(ventes$LIEN)
# Diverses statistiques selon le genre et le lien physique
df=ventes %>% group_by(GENRE, LIEN) %>% summarize(nb=n(),prix_moyen=mean(PRIX_VENTE),prix_median=median(PRIX_VENTE),coef_var=sd(PRIX_VENTE)/mean(PRIX_VENTE), an_moyen=mean(ANNEE_CONSTRUCTION), aire_etages_moyen=mean(AIRE_ETAGES))
# Années de construction selon le genre
ggplot(ventes, aes_string(x = ventes$ANNEE_CONSTRUCTION)) + geom_histogram(bins = 200) + labs(x = "Année de construction", y = "fréquence") + facet_grid(GENRE ~ .)
