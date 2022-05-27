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

#4 PISA
ventes = ventes %>% filter(year(ventes$DATE_VENTE)<=2022) %>% mutate(ANNEE_VENTE=substr(DATE_VENTE,0,4), GENRE=as.character(GENRE), LIEN=as.character(LIEN))
k=length(unique(ventes$UV)) #le nombre de groupes par défaut correspond au nombre d'UV. Il peut être modifié.
l=10 #le nombre d'itérations par défaut est de 10 pour les fins de la présentation, mais il devrait être au moins de 100 en pratique.
for(m in seq_along(1:l)){
  print(m)
  ventes[[str_c("nom")]] <- str_c("cl_",kmeans(tibble(x = c(scale(ventes$LONGITUDE)), y = c(scale(ventes$LATITUDE))), k, iter.max = 200)$cluster)
  pisa <- lm(PRIX_VENTE ~ AIRE_ETAGES + ANNEE_CONSTRUCTION + AIRE_TERRAIN + ANNEE_VENTE + GENRE + LIEN + nom, data = ventes)
  coef_cste <- pisa$coefficients[[1]]
  coef_clusters <- tibble(nom = str_replace_all(names(pisa$coefficients), "nom", ""))
  coef_clusters[[str_c("iter_",m)]] <- pisa$coefficients + coef_cste
  coef_clusters <- coef_clusters %>% filter(grepl("cl_", nom))
  ventes <- left_join(ventes, coef_clusters, by = "nom")
  ventes[[str_c("iter_",m)]]<-scale(replace_na(ventes[[str_c("iter_",m)]],coef_cste))
  ventes <<- ventes %>% select(-nom)
}
ventes <- cbind(ventes, pisa_iphl = unname(rowMeans(ventes %>% select(contains("iter_")), na.rm = T)))
ventes$pisa_iphl=scale(ventes$pisa_iphl)[,1]
clean_column <- colnames(ventes %>% select(contains("iter_")))
ventes <- ventes %>% select(-one_of(clean_column))
model = lm(PRIX_VENTE ~ AIRE_ETAGES + ANNEE_CONSTRUCTION + AIRE_TERRAIN + ANNEE_VENTE + GENRE + LIEN + pisa_iphl, data = ventes)
print(summary(model))
pal <- colorQuantile(c("#0000FF", "#0080FF", "#FFFFFF", "#FF8000", "#FF0000"), ventes$pisa_iphl, n = 20)
print(leaflet(data = ventes) %>% addTiles() %>% addCircleMarkers(lng = ~ ventes$LONGITUDE, lat =  ~ ventes$LATITUDE, color=pal(ventes$pisa_iphl), stroke = FALSE, fillOpacity = 0.75, radius = 10, popup = paste(ventes$pisa_iphl)))


