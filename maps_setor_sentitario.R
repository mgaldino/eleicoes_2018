# fazer mapas com resulttado usando dados de urna
# maps por setor censitario
# bolsonaro, haddad, bancos, nulos e abstencoes

# limpando a area
rm(list = ls())

# Carregando pacotes
library(readr)
library(data.table)
library(stringr)
library(sf)
library(ggplot2)

# carregando dados de vitoria
al <- read_delim("data/bweb_1t_MG_101020181954.csv", delim = ";", locale = locale(encoding = "latin1"))

# separando dado de presidente em BH
al_pres <- al[al$NM_MUNICIPIO == "BELO HORIZONTE" & al$DS_CARGO_PERGUNTA == "Presidente",] %>% data.table()

# somando total de votos dos candidatos por zona
al_pres <- al_pres[, .(Total_cand = sum(QT_VOTOS)), by = list(NR_PARTIDO, SG_PARTIDO, NM_PARTIDO, NM_VOTAVEL,
                                                         DS_CARGO_PERGUNTA, NR_ZONA, NR_LOCAL_VOTACAO,
                                                         CD_MUNICIPIO, NM_MUNICIPIO)]
rm("al")
gc()

# carregando dados locais de votacoes georeferenciado pelo lucas gelap
locais <- read.csv2("data/2016_locais_capitais/belohorizonte2016.csv", encoding = "latin1", stringsAsFactors = F)
locais$NUM_SECAO <- NULL

locais <- locais[!duplicated(locais), ]

# merge

al_pres <- as.data.frame(al_pres)
al_pres1 <- merge(al_pres, locais, by.x = c("NR_ZONA", "NR_LOCAL_VOTACAO"),  by.y =  c("NUM_ZONA", "NUM_LV"), all.x = T)

na <- al_pres1[is.na(al_pres1$lat),]
na <- unique(na[, 1:2])

al_pres1$geom <- st_as_sf(al_pres1$lon, al_pres1$lat)
al_pres1 <- st_as_sf(al_pres1)

al_pres1 <- al_pres1[!is.na(al_pres1$lat), ]

my.sf.point <- st_as_sf(x = al_pres1, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84")

shp <- st_read("data/al_setores_censitarios/27SEE250GC_SIR.shp", options = "ENCODING=latin1")

shp <- shp[shp$NM_MUNICIP == "MACEIÓ", ]

plot()

plot(shp$geometry)
plot(my.sf.point["Total_cand"], add = T)
# merge das base com shap

shp %>%
  mutate(pt_count = lengths(st_crosses(shp, my.sf.point))) %>%
  select(pt_count) %>%
  plot()


# plotando os graficos
ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "FERNANDO HADDAD",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Votação HADDAD - BH")
ggsave("hadda_bh.png", width = 4.2, height = 6.67)

ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "JAIR BOLSONARO",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Votação BOLSONARO - BH")
ggsave("bolsonaro_bh.png", width = 4.2, height = 6.67)

ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "Nulo",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Nulo - BH")
ggsave("Nulo_bh.png", width = 4.2, height = 6.67)

ggplot() + geom_sf(data = shpp[shpp$NM_VOTAVEL == "Branco",], aes(fill = Total_cand)) + 
  scale_fill_continuous( high= "#132B43",  low = "#56B1F7") + theme_classic() + labs(title = "Branco - BH")
ggsave("Branco_bh.png", width = 4.2, height = 6.67)
