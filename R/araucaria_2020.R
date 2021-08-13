###################################################
## load library
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

#library(installr)
#updateR()
library(plotly)
library(RColorBrewer)
library(viridis)
library(raster)
library(rgdal)
library(ENMeval)
library(maptools)
library(XML)
library(dismo) 
library(sp) 
library(MASS)
library(maps) 
library(mapdata) 
library(plotrix) 
library(fields) 
library(spam) 
library(png) 
library(reshape)
library(spatstat)
library(mapdata) 
library(scales) 
library(ggplot2) 
library(mapproj)
library(maxnet)
library(here)

# Getting environmental variables

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/Bioclim/bio_clim_3")

#list.files(pattern=".tif")
bil <- list.files(pattern=".bil") #30 arc

# import rasterstack
bil.bios <- stack(bil)

# renomear os arquivos .tif importados
#names(bif.bios) <- paste0('bio', 1:19)

setwd("set_it/")

mata_atlantica <- readOGR("./Contorno_MA_Original/Contorno MA Original.shp")
crs(mata_atlantica) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
data.shape <-readOGR("./FOM_ecoregion/Ecorregiões Sul.shp")
mascara <- data.shape[data.shape$ECO_NAME == "Araucaria moist forests",]
mascara2 <- data.shape[data.shape$ECO_NAME == "Serra do Mar coastal forests",]
mascarateste2 <- union(mascara,mascara2)
plot(mascarateste2)

# Set same extent:

mascara_2 <- raster(mascarateste2)
res(mascara_2) <- 0.008333333 # resolution from altitude
ex_2 <- extent(mascara_2)

# Crop the climate layers to the extent of Mixed Ombrophilous Forest
altitude1 <- raster("./rastert_alt_00.asc") # Altitude
altitude1 <- raster("http://www.dpi.inpe.br/amb_data/Brasil/altitude_br.asc") # Ambdata website 30 arc-sec res
altitude <- resample(altitude1,mascara_2,"bilinear")
altitude <- crop (altitude, ex_2)
altitude <- mask(altitude,mascarateste2)
crs(altitude) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#### bioclim
bio_curr <- resample(bil.bios, mascara_2,"bilinear")
bio_curr <- crop(bio_curr, ex_2)
bio_curr <- mask(bio_curr, mascarateste2)

predictors_sdm <- bio_curr
crs(predictors_sdm) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

predictors_sdm <- addLayer(altitude,bio_curr)

## all variables inside the study mask
names(predictors_sdm) <- c("altitude","bio1","bio10","bio11", "bio12","bio13","bio14",
                              "bio15","bio16","bio17","bio18","bio19","bio2","bio3",
                              "bio4","bio5","bio6","bio7","bio8","bio9")

#create folder to save organized (inside the mask) variables
#dir.create('variaves_mascara_2020')
setwd('./variaves_mascara_2020') 
writeRaster(predictors_sdm,('all_variables_mask_plos'), format = 'GTiff', overwrite=T)
list.files(pattern=".tif")
setwd("set_it")

#======================= COLINEARITY ===========

# Avaliando se tenho problema nas minhas variáveis

altmat_val <- c(as.matrix(predictors_sdm$altitude)) # altitude
bio01_val <- c(as.matrix(predictors_sdm$bio1)) # bio1
bio02_val <- c(as.matrix(predictors_sdm$bio2)) # bio2
bio03_val <- c(as.matrix(predictors_sdm$bio3)) # bio3
bio04_val <- c(as.matrix(predictors_sdm$bio4)) # bio4
bio05_val <- c(as.matrix(predictors_sdm$bio5)) # bio5
bio06_val <- c(as.matrix(predictors_sdm$bio6)) # bio6
bio07_val <- c(as.matrix(predictors_sdm$bio7)) # bio7
bio08_val <- c(as.matrix(predictors_sdm$bio8)) # bio8
bio09_val <- c(as.matrix(predictors_sdm$bio9)) # bio9
bio10_val <- c(as.matrix(predictors_sdm$bio10)) # bio10
bio11_val <- c(as.matrix(predictors_sdm$bio11)) # bio11
bio12_val <- c(as.matrix(predictors_sdm$bio12)) # bio12
bio13_val <- c(as.matrix(predictors_sdm$bio13)) # bio13
bio14_val <- c(as.matrix(predictors_sdm$bio14)) # bio14
bio15_val <- c(as.matrix(predictors_sdm$bio15)) # bio15
bio16_val <- c(as.matrix(predictors_sdm$bio16)) # bio16
bio17_val <- c(as.matrix(predictors_sdm$bio17)) # bio17
bio18_val <- c(as.matrix(predictors_sdm$bio18)) # bio18
bio19_val <- c(as.matrix(predictors_sdm$bio19)) # bio19

## create a data-frame
tabela <- data.frame(cbind(altmat_val,bio01_val,bio02_val,bio03_val,bio04_val,bio05_val,bio06_val,
                           bio07_val,bio08_val,bio09_val,bio10_val,bio11_val,bio12_val,
                           bio13_val,bio14_val,bio15_val,bio16_val,bio17_val,
                           bio18_val,bio19_val))

# Matrix generation

valores_soma <- rowSums(tabela)
valores_soma_validos <- 1:nrow(tabela)
valores_soma_validos <- ifelse(is.na(valores_soma), NA, valores_soma_validos)
valores_soma_validos <- subset(valores_soma_validos, valores_soma_validos >0)

tabela_validos <- tabela[valores_soma_validos, ]
cor(tabela_validos)
round(cor(tabela_validos),2)
par(mfrow=c(1,1))

setwd("set_it")
setwd("./table_correlation_2020")

hist(cor(tabela_validos), main="Variable_correlation", col="gray90")

write.table(round(cor(tabela_validos), 2), 'cor_pres_2020.xls', row.names = T, sep = '\t')

write.table(ifelse(cor(tabela_validos) >= 0.7, 'Sim', 'Não'), 'cor_pres_afirmacao_2020.xls', row.names = T, 
            sep = '\t')
see <- (ifelse(cor(tabela_validos) >= 0.7, 'Yes', 'No'))

# plot corplot
library(corrplot)

tiff('camadas_ambientais_2020.tif', width = 25, height= 25, units = 'cm', res = 1000, compression = 'lzw')
corrplot(cor(tabela_validos), type = 'lower', diag = F, tl.srt = 45, mar = c(3, 1.5, 3, 1.5),
         title = 'Correlation Bioclimatic Variables')
dev.off()

# ================================================================

# export it
tiff('cor_2.tiff', width = 10, height = 12, units = 'in', res = 1000, compression = 'lzw')
corrplot(cor(tabela_validos), type = "lower", diag = F, 
         title = 'Correlation Bioclimatic Variables', 
         mar = c(3, 0.5, 2, 1), tl.srt = 45)
dev.off()

# expor table
tabela.cor <- cor(tabela_validos)
write.table(tabela.cor, 'tabela_cor_2020.txt', row.names = F, quote = F, sep = '\t')
write.table(round(tabela.cor, 2), 'tabela_cor_2020.xls', row.names = F, 
            quote = F, sep = '\t')

# PCA for colinearity among variables
setwd("set_it")
setwd('./pca_2020')

# pca do pacote 'stats'
pca <- prcomp(tabela_validos, scale = T)

# eigenvalues
summary(pca)

# barplot graph
screeplot(pca, main = 'Autovalues')
abline(h = 1, col = 'red', lty = 2) # 5 variables represent the whole bioclim variables

# import the figure
tiff('screeplot_2020_plos.tif', width = 20, height = 20, units = 'cm', 
     res = 1000, compression = 'lzw')
screeplot(pca, main = 'PCA Auto-value')
abline(h = 1, col = 'red', lty = 2)
dev.off()

# eigenvectors - escores)
pca$x

# relacao das variaveis com cada eixo (loadings - cargas)
pca$rotation[, 1:5]
abs(pca$rotation[, 1:5])

# export table contribution
write.table(round(abs(pca$rotation[, 1:5]), 2), 'contr_pca_2020.xls', 
            row.names = T, sep = '\t')
# plot
biplot(pca)
# import the figure
tiff('pca_2020_plos_2.tif', width = 20, height = 20, units = 'cm', 
     res = 1000, compression = 'lzw')
biplot(pca, main = 'PCA Auto-value')
#abline(h = 1, col = 'red', lty = 2)
dev.off()

# extracting asc. values and removing NAs
as.v <- values(predictors_sdm)
as.v.na <- na.omit(as.v)
head(as.v.na)
dim(as.v.na)

# pca como novas variaveis
# pca dos raster
#install.packages('RStoolbox')
library(RStoolbox)
pca.as <- rasterPCA(predictors_sdm, spca = T) # esse comando ira demorar

# components PCA contribution
summary(pca.as$model)

# plotting contribution
getwd()
screeplot(pca.as$model, main = 'Autovalues')
abline(h = 1, col = 'red', lty = 2)
dev.off()

tiff('screeplot_raster_plos_2020_demorado.tif', width = 20, height = 20, units = 'cm',
     res = 1000, compression = 'lzw')
screeplot(pca.as$model, main = 'Autovalues')
abline(h = 1, col = 'red', lty = 2)
dev.off()

plot(pca.as$map[[1:5]])

# export variables
setwd('')
for(i in 1:5){
  writeRaster(pca.as$map[[i]], paste0('pc', i, '_br.asc'), 
              format = 'ascii', overwrite=T)}

### Testing

library(vegan)
tabela_validos <- decostand(tabela_validos, method="standardize") 
summary(tabela_validos) # veja as Mean

#
tabela_validos_prcomp <- prcomp(tabela_validos)
plot(tabela_validos_prcomp)
axis(1)

### Each component variance
std_list <- tabela_validos_prcomp$sdev
std_list_pct <- std_list / sum (std_list) * 100
round(std_list_pct, 2)

# percentage of each component variance
tabela_validos_prcomp
round(data.frame(tabela_validos_prcomp[2]),2)
head(round(predict(tabela_validos_prcomp),2))

### factorial analysys

#install.packages('psych')
#install.packages('GPArotation')
library(psych)
library(GPArotation)
library(vegan)

setwd("set_it")

#dir.create('fatorial')
setwd("./fatorial")
as.v.na
as.v.na.p <- decostand(as.v.na, method = 'standardize')
summary(as.v.na.p)

# analises preliminares de possibilidade de uso da analise fatorial
# kmo e bartlett
KMO(cor(as.v.na.p)) # > 0.5
cortest.bartlett(cor(as.v.na.p), n = nrow(as.v.na)) # (p <0.05)
fa <- fa.parallel(as.v.na.p, fm = 'ml', fa = 'fa') # sugere 5 eixos

# exportar screeplot
tiff('screeplot_fatorial.tif', width = 20, height = 20, units = 'cm', res = 1000, compression = 'lzw')
fa.parallel(as.v.na.p, fm = 'ml', fa = 'fa')
dev.off()

# fatorial
fa.as <- fa(as.v.na.p, nfactors = 5, rotate = 'oblimin', fm = 'ml')
as.loadings <- loadings(fa.as)

# exportar screeplot
tiff('screeplot_fatorial_2020_ploa.tif', width = 20, height = 20, units = 'cm', res = 1000,
     compression = 'lzw')
fa.diagram(as.loadings)

dev.off()

# exportar tabela dos resultados
write.table(abs(round(as.loadings, 2)), 'as_loadings.xls', row.names = T, sep = '\t')

# Variables definition
# according to cor test and ecology: forest remnants;
# PCA
#==================================================================================###]
# variaveis escolhidas para o modelo: 
# usar Bio 01, 02, 04, 12, alem da mata atl
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

predictors_after_prean <- stack(predictors_sdm$bio01, #From PCA1
  predictors_sdm$bio2, #Fatorial 
  predictors_sdm$bio4, #From PCA2
  predictors_sdm$bio12, predictors_sdm$altitude) #Ecological aspects
plot(predictors_after_prean, axes=T)
### Caso queira recomeçarr a aanálise sem PCA e gráfico de correlaÃ§Ã£o
getwd()
#dir.create('rasters_2020')
setwd("./rasters_2020/")

#writeRaster(predictors_after_prean$remnant, filename="remnants_2020_art_1", format='GTiff',overwrite=T)
#remnants <- as(predictors_after_prean$remnant, 'SpatialPolygons')
#crs(remnants) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#shapefile(remnants, 'remnants_qgis.shp', overwrite=T)

# save rasters files
writeRaster(predictors_after_prean$bio01, filename="bio01_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$bio2, filename="bio02_2020.tif", format='GTiff', overwrite=T)
#writeRaster(predictors_after_prean$bio03, filename="bio03_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$bio4, filename="bio04_2020.tif", format='GTiff', overwrite=T)
#writeRaster(predictors_after_prean$bio11, filename="bio11_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$bio12, filename="bio12_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$altitude, filename="altitude.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean, filename="all_variables_2020.tif", format='GTiff',overwrite=T)

predictors_after_prean <- list.files(pattern=".tif") #30 arc

# Option 1
altitude <- raster("altitude.tif")
bio01 <- raster("bio01_2020.tif")
bio02 <- raster("bio02_2020.tif")
bio04 <- raster("bio04_2020.tif")
bio12 <- raster("bio12_2020.tif")
# Option 2

predictos_after_prean <- stack("all_variables_2020.tif")
names(predictos_after_prean) <- c("bio01","bio02","bio04","bio12","altitude")
plot(predictos_after_prean)

## Species data
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

dados <- read.csv("dados_atualizados.csv", header=T)
dados <- read.csv("araucaria.csv", header=T)
head(dados$datasetKey)
dados <- dados[,-1]
glimpse(str(dados$datasetKey))
go <- glimpse(str(dados$datasetKey))
###################################################################################

################ REMOVING CLOSING POINTS AND DUPLICATES ###########################
data("wrld_simpl")
araucaria  <- subset(dados, !is.na(lon) & !is.na(lat))
x11()
go <- plot(wrld_simpl, xlim=c(-70,-27.5),ylim=c(-33, 0), axes=TRUE, col="white")
box() # makes the grid around the map

# plot points again to add a border, for better visibility
points(araucaria$lon, araucaria$lat, col='red', cex=0.5, pch=18)

# Making data cleaning
lonzero = subset(araucaria, lon==0) # Nenhum dado está com zero para longitude
latzero = subset(araucaria, lat==0)  # # Nenhum dado está com zero para latitude

# If there are duplicated
dupsa <- duplicated(araucaria[, c('lon', 'lat')]) # possui pontos duplicados

# number of duplicates
sum(dupsa)  # 8 points

# cleaning duplicated
araucaria <- araucaria[!dupsa, ]
head(araucaria) # 951points

# adding presence column
araucaria$presence <- rep(1,nrow(araucaria)) 
colnames(araucaria) = c('lon', 'lat','species') 
head(araucaria)
box() # makes the grid around the map/faz os contornos do mapa 
dados <- araucaria[,-3]
head(dados)

# Only points inside study Mask

ac <- dados #testing points of the species
coordinates(ac) <- ~lon+lat
projection(ac) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
Imart.spdf2 <- ac[mascarafim,] # will give error (identicalCRS)
projection(ac)
projection(mascarafim)
projection(mascarafim)<-projection(ac)
Imart.spdf2 <- ac[mascarafim,]

############### remove ALTO Paraná
mascarateste2
mascara_fim <- raster(mascarateste2)
res(mascara_fim) <- 0.008333333 # resolution from altitude

ex_fim <- extent(mascara_fim)

ac <- dados #testing points of the species
coordinates(ac) <- ~lon+lat
projection(ac) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Imart.spdf2 <- ac[mascara_fim,] # will give error (identicalCRS)
projection(ac)
projection(mascarateste2)
projection(mascara_fim)<-projection(ac)
Imart.spdf3 <- ac[mascarateste2,]
projection(Imart.spdf3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

new_data3 <- as.data.frame(Imart.spdf3)
list1 <- 1:823
list2 <- rep("Araucaria_angustifolia", length(list1))
list3 <- cbind(list2,new_data3)
mask_data3 <- list3
mask_data3$presence <- rep(1,nrow(mask_data3)) 
names(mask_data3) <- c("Species", "lon", "lat", "Presence")

#dir.create('ocorrencia_na_mascara_2020')
# mudar diretorio
getwd()
setwd('./ocorrencia_na_mascara_2020')
# write .csv with correct dataset
write.csv(mask_data3, file = "pts_mask_2020.csv")

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

# Plot study area
estados <- readOGR("./estados/estados.shp") 
plot(estados[estados$Regiao=="SUL",], add=T)
plot(mascarateste2, add=T)
projection(estados) = CRS("+proj=longlat +datum=WGS84 +no_defs")

library(RgoogleMaps)
library(maps) #mapas simples, eixos, escala, cidades 
library(mapdata) #base de dados WorldHires e rios
library(rworldmap) #outra base de dados de mapas do mundo
library(maptools) #Ler ESRI shapefiles 
library(mapproj) #Projeções e grids
library(ggmap) #Gmaps, OSM + mapas baseados em ggplot2
library(rgdal)


setwd("./uc_brasil/")
ucs <- readOGR("./unidade_protecao_integralPolygon.shp")
#Define proper CRS
projection(ucs) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# imoport PAs in Brazil
ucs_suste <- readOGR("./unidade_uso_sustentavelPolygon.shp") 
#Define proper CRS
projection(ucs_suste) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#### dados APP em Clevelândia

plot(cleve_app)
plot(cleve_rl, colour="blue", add=T)

plot.new()
setwd("D:\\OneDrive\\Doutorado_2020\\Cap_2\\")

cleve_app <- readOGR("./Reserva_legal/2_Parana_APP/APP_Clevelandia.shp")
projection(cleve_app) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
cleve_rl <- readOGR("./Reserva_legal/3_Parana_RL/RESERVA_LEGAL_Clevelandia.shp")
projection(cleve_rl) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#ClevelÃ¢ndia
muni <- readOGR("./Reserva_legal/BR_Municipios_2020.shp")
cleve <- (muni[muni$NM_MUN=="ClevelÃ¢ndia",])

reserva_legal <- (cleve_rl[cleve_rl$NOM_TEMA=="Reserva Legal Averbada",])

#### Importar parques
mozart <- readOGR("./Cap_2/mozart.kml")
projection(mozart) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

tamarino <- readOGR("./Cap_2/parque_tamarino.kml")
projection(tamarino) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

sansao <- readOGR("./Cap_2/sansao.kml")
projection(sansao) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

viridis_pal(option = "D")(3)

plot.new()
dev.off()
png(paste0("./reservas_legais.png"),width=15,height=12)
#par(mar=c(5,3,5,3))
plot(cleve, axes=T, col="gray90",
     main="APPs e Reservas Legais de entorno das UCs de Clevelândia", xlab="Longitude", ylab="Latitude")
plot(cleve_app, axes=T, col="black", add=T)
plot(reserva_legal, axes=T, col="#3E4A89FF", add=T)
plot(mozart, col="#FDE725FF",add=T)
plot(tamarino, col="red",add=T)
plot(sansao, col="green",add=T)
plot(cleve_rl, col="pink",add=T)

#plot(mascara2, axes=T, col=viridis_pal(option = "D")(1), add=T)
legend(-52.3,-26.17, legend=c("Município de Clevelândia","Reservas Legais Averbadas",
                             "APPs","UC Mozart","UC Tamarino","UC Sansão"),
                          col=c("black","black","black","black","black","black"),
                  fill=c("gray90","#3E4A89FF","black","#FDE725FF","red","green"), box.lty=0)
#plot(estados[estados$Regiao=="SUL",], add=T,border="red")
dev.off()

# combine both spdf
ucs_full <- rbind(ucs,ucs_suste)
# crop it for study area
ucs_full1 <- crop(ucs_full,mascara)
ucs_full2 <- crop(ucs_full,mascara2)
#ucs_full3 <- crop(ucs_full,mascara3)
ucs_full <- rbind(ucs_full1,ucs_full2)

### Map pf Study Area
# Legend specifications
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

######################################
################################  ########################

##################################################
### Starting the modeling Session - here we go, grab a beer :)

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/ocorrencia_na_mascara_2020/")
testando <- read.csv("pts_mask_2020.csv", header=T)

library(biomod2)
DataSpecies <- testando
DataSpecies <- DataSpecies[,-6]
DataSpecies <- DataSpecies[,-1]
head(DataSpecies)

names(DataSpecies) <- c("Species", "lon", "lat", "Araucaria")

#loading environmental layers
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
#dir.create('rasters_2020')
setwd("./rasters_2020/")
#predictors_after_prean <- stack('all_variables_2020.tif')
#names(predictors_after_prean) <- c("remnant","altitude","bio01","bio04","bio11","bio12")
#plot(predictors_after_prean)

#myExpl <- predictos_after_prean

myExpl <- dropLayer(predictos_after_prean, c(5))
plot(myExpl)

# Name of my species
myRespName <- 'Araucaria'

#presence/absence data of my species
myResp <- as.numeric(DataSpecies[,myRespName])

# my species coordinates
myRespXY <- DataSpecies[,c("lon","lat")]

############################# LAST DATA FILTER #####################
### filtering even more data to one presence inside 1 km2 grid cell
##===================
## Remove duplicates
cell.pres <- cellFromXY(myExpl,myRespXY)
## Spatial points at center of each raster cell with presences
list.cell.pres <- sort(unique(cell.pres))
cell.pres.sp <- xyFromCell(myExpl,list.cell.pres,spatial=TRUE)

## Build data-set for presence
d.presence <- as.data.frame(extract(myExpl,cell.pres.sp)) 
Coords.presence <- coordinates(cell.pres.sp)
colnames(Coords.presence) <- c("x","y")
data.xy <- cbind(d.presence,Coords.presence)

## Data points with complete environmental information
wcomp <- which(complete.cases(data.xy))

## Transform as a SpatialPointsDataFrame and SpatialPoints (for presence only)
d <- SpatialPointsDataFrame(coords=Coords.presence[wcomp,], data=data.xy[wcomp,],
                            proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

p <- SpatialPoints(d) ## This is used for presence-only data


projection(p) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

myRespCoord <- d@coords
str(DataSpecies)

## Number of 1km pixels with at least one presence
npix <- nrow(d)

# Plot study area

plot.new()
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

pdf(paste0("./Araucaria/outputs/study_area.pdf"),width=15,height=12)
#par(mar=c(5,3,5,3))
plot(mata_atlantica, axes=T, col="gray90",
     main="Study Area", xlab="Longitude", ylab="Latitude")
#plot(mascara3, axes=T, col=viridis_pal(option = "E",direction = -1)(1), add=T)
#plot(mascara3, axes=T, col="#FDE725FF", add=T)
plot(mascara, axes=T, col="#35B779FF", add=T)
plot(mascara2, axes=T, col="#3E4A89FF", add=T)
#plot(mascara2, axes=T, col=viridis_pal(option = "D")(1), add=T)
legend(-45, -24, legend=c("Atlantic Rainforest Domain",
                          #"Alto Paraná Atlantic Forests",
                          "Mixed Ombrophilous Forest",
                          "Dense Ombrophilous Forests",
                          "Tropic of Capricorn - Dashed Line",
                          "Occurrence points (N=324)"),
       col=c("black","black","black","black","black"),
       fill=c("gray90","#35B779FF","#3E4A89FF","black","black"),box.lty=0)
abline(h=-23.5, lty=2, col="black")
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
plot(wrld_simpl, add=TRUE)
#points(p,col="black", pch=20) # p295 points final
scalebar(750, xy = c(-68,-33), type = 'bar', divs = 3, below = c('km'), 
         lonlat = T, lwd = 3)
dev.off()


# Plot
pdf(paste0("./Araucaria/outputs/study_area1_zoom3.pdf"),width=15,height=12)
par(mar=c(5,3,5,3))
plot(predictors_sdm$altitude,col=viridis_pal(option = "B")(255),axes=T)
plot(ucs_full, axes=T, col="#35B779FF", add=T)
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Study area altitudinal range (m)",
                          "Tropic of Capricorn - Dashed Line",
                          "Protected Areas",
                          "Occurrence points (N=324)"),
       col=c("black","black","black","black"),
       fill=c("gray90","black","#35B779FF","white"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 3)
points(p,col="white", pch=20) # p293 points inside study mask
dev.off()

## keep only all cells that are defined for all layers 
# see: https://r-forge.r-project.org/forum/message.php?msg_id=41240&group_id=302

# Starting first step
## Future distribution with worldclim Data 
gcms <- c("gs","he","no")
rcps <- c("45","85")
years <- c("70")
futu <- c("2080")
#bios <- c("1","2","4","12","13")
# For global climate models (GCMs): NorESM1-M, HadGEM2-ES, GISS-E2-R,  
# this loop take some minutes, go grab a coffee or a beer

for (gc in 1:length(gcms)) {
  for (rc in 1: length(rcps)) {
    for (ye in 1: length(years)) {
      for (fu in 1: length(futu))   {
        
        setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
        setwd("./Bioclim_futuro/future_bioclim/future_data_3")
        
        list.files(pattern=".tif")
        tif_fut <- list.files(pattern=".tif")
        
        #variable <- raster(paste0(tif_fut,gcms[gc],rcps[rc],years[ye],bios[bi],".tif"))
        bio01 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"1",".tif"))
        bio02 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"2",".tif"))
        #bio03 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"3",".tif"))
        bio04 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"4",".tif"))
        #bio11 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"11",".tif"))
        bio12 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"12",".tif"))
        
        variable <- stack(bio01,bio02,bio04,bio12)
        
        crs(variable)  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        #+proj=longlat +datum=WGS84 +no_defs (to use above)
        future_1 <- resample(variable,mascara_2,"bilinear")
        future_1 <- crop(future_1, ex_2)
        future_3 <- mask(future_1, mascarateste2)
        names(future_3) <- c("bio01","bio02","bio04","bio12")
        
        writeRaster(future_3, filename= paste0(gcms[gc],"_",rcps[rc],"_",futu[fu]),format = "GTiff", overwrite = T)
        
        
      }
    }
  }
}



for (i in 1: length(myRespName)) {

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

## keep only all cells that are defined for all layers 
# see: https://r-forge.r-project.org/forum/message.php?msg_id=41240&group_id=302
  # Avoiding Maxent warnings Status1
  intersect_mask <- function(x){
    values_x <- getValues(x)
    inter_x <- values_x %*% rep(1,nlayers(x))
    mask <- setValues(subset(x,1),values = (inter_x>0))
    return(mask)
  }
  ## keep only all cells that are defined for all layers 
  # see: https://r-forge.r-project.org/forum/message.php?msg_id=41240&group_id=302
myExpl <- stack(mask(myExpl, intersect_mask(myExpl)))
#set.seed(2589) ## Reproducible pseudo-absences
myBiomodData <- BIOMOD_FormatingData(resp.var = p,
                                     expl.var = myExpl,
                                     #resp.xy = myRespCoord,
                                     resp.name = myRespName,
                                     PA.nb.rep = 1,# see paper "how many pseudo-abs...
                                     PA.nb.absences = 10000,#see here why 1000, https://doi.org/10.1111/j.2041-210X.2011.00172.x
                                     PA.strategy = 'random',
                                     na.rm = TRUE)

saveRDS(myBiomodData,paste0("Araucaria/BiomodData.rds"))

myBiomodOption <- BIOMOD_ModelingOptions(GLM= list(type="quadratic",interaction.level=0,
                                                   myFormula=NULL,family=binomial(link="logit"),test="AIC"),
                                         GAM = list(algo="GAM_mgcv",type = 's_smoother',interaction.level = 0,
                                                    myFormula = NULL,family =binomial(link='logit')),
                                         RF = list(do.classif = TRUE, ntree = 500),
                                         ANN = NULL,
                                         MAXENT.Phillips=list(path_to_maxent.jar='D:/OneDrive/Cap_1_outros_papers/script_art_1/maxent',
                                                           visible=FALSE,maximumiterations=500,memory_allocated=512,
                                                         product=F, threshold=F, hinge=F))

set.seed(2589)
myBiomodModelOut <- BIOMOD_Modeling (myBiomodData,
                                     models=c('MAXENT.Phillips','GLM','GAM','RF','ANN'),
                                     models.options= myBiomodOption,
                                     NbRunEval= 10, 
                                     DataSplit=70, 
                                     VarImport=3, 
                                     models.eval.meth=c("TSS","ROC"),
                                     rescall.all.models=F,
                                     do.full.models=T,
                                     modeling.id= "4mod")# 4 stat models

## select a threshold to keep a single model
# Table generating most important variables and algos evaluation ;)
capture.output(get_evaluations(myBiomodModelOut),
               file=file.path(paste0("./Araucaria/outputs/formal_eval_evaluation.txt", sep="")))


capture.output(get_variables_importance(myBiomodModelOut),
               file=file.path(paste0("./Araucaria/outputs/formal_models_variables_importance.txt", sep="")))

myBiomodModelOut <- get(load(paste0("./Araucaria/Araucaria.4mod.models.out")))

# Ensemble modeling
myBiomodEM <- BIOMOD_EnsembleModeling (modeling.output= myBiomodModelOut,
                                       chosen.models=grep("_Full_",
                                                        get_built_models(myBiomodModelOut),
                                                    value=TRUE),
                                       #chosen.models='all',
                                       em.by="all", #Statistical models : 'algo'
                                       #em.by="PA_dataset+repet", #see why here https://rstudio-pubs-static.s3.amazonaws.com/38564_747d4bbf87704f0394734977bd4905c4.html
                                       eval.metric= c("ROC"),
                                       eval.metric.quality.threshold= c(0.6),#Setting threshold for cutoff between models, at least 2 models higher
                                       models.eval.meth = c('TSS','ROC'),
                                       prob.mean=FALSE,
                                       prob.ci=FALSE,
                                       prob.ci.alpha=0.05,
                                       committee.averaging=TRUE, #It gives both a prediction and a measure of uncertainty
                                       prob.mean.weight=F,
                                       prob.mean.weight.decay="proportional")
myBiomodEM <- get(load(paste0("./Araucaria/Araucaria.4modensemble.models.out")))

#= Make projections on current variables
myBiomodProj <- BIOMOD_Projection(modeling.output=myBiomodModelOut,
                                  new.env = myExpl,
                                  proj.name = "current",
                                  selected.models=grep("_Full_",
                                                     get_built_models(myBiomodModelOut),
                                                     value=TRUE), ## Full models only
                                  #selected.models=("all"),
                                  binary.meth= c('TSS','ROC'), #Important, this is the point where our probabilities will 
                                  compress= TRUE,
                                  filtered.meth=c('TSS','ROC'),
                                  build.clamping.mask = T,
                                  output.format=".grd",
                                  omi.na= T,
                                  on_0_1000=T)


BiomodProj <- get(load(paste0("./Araucaria/proj_current/Araucaria.current.projection.out")))

## BIOMOD_EnsembleForecasting 
BiomodEF <- BIOMOD_EnsembleForecasting(EM.output=myBiomodEM, ## Rules for assembling
                                       projection.output=myBiomodProj, ## Individual model projection
                                       binary.meth=c('TSS','ROC'),
                                       filtered.meth=c('TSS','ROC'),
                                       compress=TRUE,
                                       on_0_1000=T)

BiomodEF <- get(load(paste0("./Araucaria/proj_current/Araucaria.current.ensemble.projection.out")))


## Future distribution with Future Data

mod <- c("gs","he","no") # For global climate models (GCMs): NorESM1-M, HadGEM2-ES, GISS-E2-R,  
rcp <- c("45","85") #  for RCP 45 and 85
yr <- c("2080") # For 2050, 2080

n.mod <- length(mod)*length(rcp)*length(yr)

#if (run.models) {
  for (mc in 1:length(mod)) {
    for (j in 1:length(rcp)) {
      for (l in 1:length(yr)) {
          

        ## Message
        i.mod <- (mc-1)*length(rcp)*length(yr) + (j-1)*length(yr) + l
        cat(paste0("\n","Model ",i.mod,"/",n.mod,": ",mod[mc],"_",rcp[j],"_",yr[l],"\n"))
        setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
        ## Load climatic data
        setwd("./Bioclim_futuro/future_bioclim/future_data_3")
        future <- stack(paste0(mod[mc],"_",rcp[j],"_",yr[l],".tif"))
        names(future) <- c("bio01","bio02","bio04","bio12")
        #future <- stack(predictos_after_prean$altitude,future)
        myExplfut <- future
        # Avoiding Maxent warnings Status1
        intersect_mask <- function(x){
          values_x <- getValues(x)
          inter_x <- values_x %*% rep(1,nlayers(x))
          mask <- setValues(subset(x,1),values = (inter_x>0))
          return(mask)
        }
        ## keep only all cells that are defined for all layers 
        # see: https://r-forge.r-project.org/forum/message.php?msg_id=41240&group_id=302
        myExplfut <- stack(mask(myExplfut, intersect_mask(myExplfut)))
        ## Projections by model
        setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
      
        BiomodProjFuture <- BIOMOD_Projection(modeling.output=myBiomodModelOut,
                                              new.env=myExplfut,
                                              proj.name=paste0(mod[mc],"_",rcp[j],"_",yr[l]),
                                              selected.models=grep("_Full_",
                                                                   get_built_models(myBiomodModelOut),
                                                                   value=TRUE), ## Full models only
                                              #selected.models = 'all',
                                              binary.meth=c('TSS','ROC'),
                                              filtered.meth=c('TSS','ROC'),
                                              compress=TRUE,
                                              build.clamping.mask=FALSE, #
                                              omi.na=TRUE,
                                              on_0_1000=TRUE,
                                              output.format=".grd")
        
        ## BIOMOD_EnsembleForecasting == FUTURE ==
        BiomodEF_Future <- BIOMOD_EnsembleForecasting(EM.output=myBiomodEM, ## Rules for assembling
                                                      projection.output=BiomodProjFuture, ## Individual model projection
                                                      binary.meth=c('TSS','ROC'),
                                                      filtered.meth=c('TSS','ROC'),
                                                      compress=TRUE,
                                                      on_0_1000=TRUE)
        
      }
    }
  }
  
# End

##=====================
## Current distribution
## Presence points and altitude
# Legend specifications
a.arg <- list(at=seq(0,2000,length.out=5),labels=seq(0,2000,length.out=5),cex.axis=1.0)
l.arg <- list(text="Elevation (m)",side=2, line=0.5, cex=1.5)
# Plot
pdf(paste0("./Araucaria/outputs/presence_alt.pdf"),width=6.5,height=10)
#par(mar=c(0,0,0,1),cex=1.4)
plot(myExpl2$altitude,col=viridis_pal(option = "B")(255)[255:1],
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(0,2000))
plot(d,pch=19,add=TRUE,cex=1)
dev.off()


# Load predictions and update extent
pred <- stack(paste0("./Araucaria/proj_current/proj_current_Araucaria_ensemble.grd"))
ca <- pred[[1]]
unique(values(ca))

## Committee averaging
# Legend specifications
breakpoints <- c(-100,100,300,500,700,900,1100)
#colors <- c(grey(seq(0.9,0.7,-0.2)),gcolors(3))
a.arg <- list(at=seq(0,1000,length.out=6), labels=c("0","1","2","3","4","5"),cex.axis=1.5)
l.arg <- list(text="Vote",side=2, line=0.5, cex=1.5)
#colors <- c(grey(c(0.90,seq(0.7,0.3,-0.2))),"#568203","#013220")

###############################

# Plot
pdf(paste0("./Araucaria/outputs/ca_current1.pdf"),width=6.5,height=10)
par(mar=c(0,0,0,1),cex=1.4)
plot(ca,col=viridis_pal(option = "D")(7)[7:1],ext=mascara_2, breaks=breakpoints,
#plot(ca,col=colors,ext=mascara_2, breaks=breakpoints,
      legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE, box=FALSE, zlim=c(0,1000))
#plot(mascara, add=T, col="transparent",border="black")
#plot(mascarafim, add=T, col="transparent",border="black")
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(wrld_simpl, add=TRUE)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Export as tif
writeRaster(ca,paste0("./Araucaria/outputs/committee_averaging.tif"),datatype="INT2S",
            overwrite=TRUE,
            options=c("COMPRESS=LZW","PREDICTOR=2"))

writeRaster(ca,paste0("./Araucaria/outputs/ca_current.tif"), overwrite=T)

# Converting continuous distribution to binary predictions 
## you will use this raster file to compute connectivity

binary_map_current <- BinaryTransformation(ca, 599) #at least three models, 600 counts 3 at least
#plot(binary_map_current)

writeRaster(binary_map_current, filename = paste0("./Araucaria/outputs/binary_current"),
            format = "GTiff", overwrite = TRUE)

## Present species distribution area (km2)
## values(ca)>=500 gives vote >=2

SDA.pres <- sum(values(ca)>=600,na.rm=TRUE) # Just the sum because one pixel is 1km2.
compare_with_SDA.pres <- sum(values(binary_map_current)>0,na.rm=TRUE) ## it works ;)

## Present species distribution area (km2) - accordingly to continuous distribution
#raster <- ca
#
##oi <- as.data.frame(raster$Araucaria_EMcaByTSS_mergedAlgo_mergedRun_mergedData)
#colnames(oi)<-"media"
#projection(raster) = CRS("+proj=longlat +datum=WGS84 +no_defs")
#rst_prj <- projectRaster(raster, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#spy_prj <- rasterToPolygons(rst_prj)

## Present species distribution area (km2)
#unique(values(ca)
#library(rgeos)
#ta  <- gArea(spy_prj)/1e6 # total area = 217.849,2
#percentual_pres <- SDA.pres*100/ta # ta = total area of MOF  24,49
#km2_pres <- (ta*percentual_pres)/100

##=================
## Ecological niche

## 95% quantiles for alt, temp, prec, tseas, cwd
wC <- which(values(ca)>=600)
niche.df <- as.data.frame(myExpl)[wC,]
niche.df$alt <- myExpl2$altitude[wC]

Mean <- round(apply(niche.df,2,mean,na.rm=TRUE))
q <- round(apply(niche.df,2,quantile,c(0.025,0.975),na.rm=TRUE))
niche <- as.data.frame(rbind(Mean,q))
write.table(niche,paste0("./Araucaria/outputs/niche.txt"),sep="\t")

## Draw points in the SDA and extract environmental variables
# In SDA
nC <- length(wC)
Samp <- if (nC>1000) {sample(wC,1000,replace=FALSE)} else {wC}
mapmat.df <- as.data.frame(myExpl)[Samp,]
mapmat.df$species <- rep(c("Araucaria_angustifolia"))
write.csv2(mapmat.df,paste0("./Araucaria/outputs/niche_graph_species.csv"))

## Model performance of committee averaging

# Individual models

(scores_all <- get_evaluations(myBiomodModelOut))
scores_ROC <- as.numeric(scores_all["ROC","Testing.data",,,])
scores_ROC <- as.data.frame(scores_ROC)
scores_ROC <- scores_ROC[complete.cases(scores_ROC$scores_ROC), ] 
mean(scores_RO)
mean(scores_ROC$scores_ROC)
scores_TSS <- as.numeric(scores_all["TSS","Testing.data",,,])
mean(scores_ROC)
## select a threshold to keep a single model
score_thresh_ROC <- mean(tail(sort(scores_ROC)))
score_thresh_TSS <- mean(tail(sort(scores_TSS)))


Perf.mods <- as.data.frame(as.table(get_evaluations(myBiomodModelOut)))
names(Perf.mods) <- c("wIndex","Index","Model","Run","PA","Value")
write.table(Perf.mods,paste0("./Araucaria/outputs/current_model_evaluation.txt"),sep="\t")

# Observations and committee averaging predictions
ObsData <- myBiomodData@data.species
ObsData[is.na(ObsData)] <- 0
caData <- values(ca)[cellFromXY(ca,xy=myBiomodData@coord)]
PredData <- rep(0,length(caData))
PredData[caData>=600] <- 1

# Computing accuracy indices to evaluate model performance

# Accuracy_indices (see Liu 2011 and Pontius 2008)
accuracy_indices <- function(pred, obs, digits=2) {
  
  if (identical(dim(pred),as.integer(c(2,2)))) {
    df <- pred
    n00 <- df[1,1]
    n10 <- df[2,1]
    n01 <- df[1,2]
    n11 <- df[2,2]
  } else {
    
    # Create data-frame
    df <- data.frame(pred=pred, obs=obs)
    
    # Confusion matrix
    n00 <- sum((df["pred"] == 0) & (df["obs"] == 0))
    n10 <- sum((df["pred"] == 1) & (df["obs"] == 0))
    n01 <- sum((df["pred"] == 0) & (df["obs"] == 1))
    n11 <- sum((df["pred"] == 1) & (df["obs"] == 1))
  }
  
  # Accuracy indices
  N <- n11 + n10 + n00 + n01
  OA <- (n11 + n00) / N
  FOM <- n11 / (n11 + n10 + n01)
  Sensitivity <- n11 / (n11 + n01)
  Specificity <- n00 / (n00 + n10)
  TSS <- Sensitivity + Specificity - 1
  Prob_1and1 <- ((n11 + n10) / N) * ((n11 + n01) / N)
  Prob_0and0 <- ((n00 + n01) / N) * ((n00 + n10) / N)
  Expected_accuracy <- Prob_1and1 + Prob_0and0
  Kappa <- (OA - Expected_accuracy) / (1 - Expected_accuracy)
  
  # Results         
  r <- data.frame(OA=round(OA, digits), 
                  EA=round(Expected_accuracy, digits),
                  FOM=round(FOM, digits),
                  Sen=round(Sensitivity, digits),
                  Spe=round(Specificity, digits),
                  TSS=round(TSS, digits),
                  K=round(Kappa, digits))
  
  return(r)
}

# Round data frame with non-numeric elements
round_data_frame <- function(x, digits=0) {
  data.frame(lapply(x, function(y) if(is.numeric(y)) round(y, digits) else y))
}

Perf.ca <- accuracy_indices(pred=PredData,obs=ObsData,digits=3)
write.table(Perf.ca,paste0("./Araucaria/outputs/performance_ca.txt"),sep="\t")

## Variable importance
VarImp <- as.data.frame(get_variables_importance(myBiomodModelOut)[,,"Full","PA1"])
Rank <- as.data.frame(apply(-VarImp,2,rank))
VarImp$mean.rank <- apply(Rank,1,mean)
VarImp$rank <- rank(VarImp$mean.rank,ties.method="max")
write.table(VarImp,paste0("./Araucaria/outputs/varimp.txt"),sep="\t")

##====================
## Future distribution

##Considering RCP 45,85 and years 2050,2080
# ## Table for change in area
#SDA.fut <- data.frame(area.pres=SDA.pres,
 #                     rcp=rep(c("45","85"),each=4),yr=rep(rep(c("2050","2080"),each=2),2),
  #                    disp=rep(c("full","zero"),4),area.fut=NA)
SDA.fut <- data.frame(area.pres=SDA.pres,
                      rcp=rep(c("45","85"),each=2),yr=rep(c("2080"),each=2),
                      disp=rep(c("full","zero"),2),area.fut=NA)
# ## Change in altitude
#Alt.fut <- data.frame(mean.pres=niche$alt[1],q1.pres=niche$alt[2],q2.pres=niche$alt[3],
  #                     rcp=rep(c("45","85"),each=4),yr=rep(rep(c("2050","2080"),each=2),2),
   #                    disp=rep(c("full","zero"),4),mean.fut=NA,q1.fut=NA,q2.fut=NA)
Alt.fut <- data.frame(mean.pres=niche$alt[1],q1.pres=niche$alt[2],q2.pres=niche$alt[3],
                      rcp=rep(c("45","85"),each=2),yr=rep(c("2080"),each=2),
                     disp=rep(c("full","zero"),2),mean.fut=NA,q1.fut=NA,q2.fut=NA)


## Committee averaging for the three GCMs (sum)
 # For 2050, 2080 # attention with yrs and yr = 2 dif. loops
#dispersal <- c("full","zero")
  
for (j in 1:length(rcp)) {
  for (l in 1:length(yr)) {
    # Create stack of model predictions
     setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
      caS <- stack() 
      for (mc in 1:length(mod)) {
      name.f <- paste0("proj_",mod[mc],"_",rcp[j],"_",yr[l])
      pred.f <- stack(paste0("./Araucaria/",name.f,"/",name.f,"_Araucaria_ensemble.grd"))
      caS <- addLayer(caS,pred.f[[1]])
    }
  
    # Compute sum
    caFut <- sum(caS)
    # Legend
    breakpoints <- seq(-100,3100,by=200)
    a.arg <- list(at=seq(0,3000,length.out=16), labels=c(0:15), cex.axis=1.2)
    l.arg <- list(text="Vote",side=2, line=0.5, cex=1.5)

    # Plot (Committee Averaging Full Dispersal)
    pdf(paste0("./Araucaria/outputs/cafd_",rcp[j],"_",yr[l],"2.pdf"),width=6.5,height=10)
    #par(mar=c(0,0,0,1),cex=1.4)
    plot(caFut,col= viridis_pal(option = "B")(16),
         breaks=breakpoints,ext=mascara_2,
         legend.width=1.5,legend.shrink=0.75,legend.mar=7,
         axis.args=a.arg,legend.arg=l.arg,
         axes=FALSE,box=FALSE,zlim=c(0,3000))
    #plot(mascara, add=T, col="transparent",border="black")
    #plot(mascarafim, add=T, col="transparent",border="black")
    plot(estados[estados$Regiao=="SUL",], add=T,border="red")
    plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
    plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
    #plot(wrld_simpl, add=TRUE)
    dev.off()
    writeRaster(caFut, filename = paste0("./Araucaria/outputs/",rcp[j],"_",yr[l],"_caFD_plos"), format = "GTiff", overwrite = TRUE)
    
    ## Convert the map to binary in order to compute connectivity
    binary_map_fut_caFut <- BinaryTransformation(caFut, 1599) # at least 6 models, 1500 counts 7 at least, that's why 1499
    writeRaster(binary_map_fut_caFut, filename = paste0("./Araucaria/outputs/",
            rcp[j],"_binary_",yr[l],"_Full"), format = "GTiff", overwrite = TRUE)

    # Zero-dispersal
    caZD <- caFut
    projection(caZD) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    values(caZD)[values(ca)<600] <- 0 
    pdf(paste0("./Araucaria/outputs/cazd_",rcp[j],"_",yr[l],"_2.pdf"),width=6.5,height=10)
    #par(mar=c(0,0,0,1),cex=1.4)
    plot(caZD,col=viridis_pal(option = "B")(16),
         breaks=breakpoints,ext=mascara_2,
         legend.width=1.5,legend.shrink=0.75,legend.mar=7,
         axis.args=a.arg,legend.arg=l.arg,
         axes=FALSE,box=FALSE,zlim=c(0,3000))
    #plot(mascara, add=T, col="transparent",border="black")
    #plot(mascarafim, add=T, col="transparent",border="black")
    plot(estados[estados$Regiao=="SUL",], add=T,border="red")
    plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
    plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
    #plot(wrld_simpl, add=TRUE)    
    dev.off()
    
    # convert the caZD to raster file to calculate the amount of PA over each projected scenario
    writeRaster(caZD, filename = paste0("./Araucaria/outputs/",
                                         rcp[j],"_",yr[l],"_caZD"), format = "GTiff", overwrite = TRUE)
    
    ## Convert the map to binary in order to compute connectivity
    binary_map_caZD <- BinaryTransformation(caZD, 1599) 
    writeRaster(binary_map_caZD, filename = paste0("./Araucaria/outputs/",
                  rcp[j],"_binary_",yr[l],"_Zero"), format = "GTiff", overwrite = TRUE)
    
    # SDA (in km2)
    SDA.fut$area.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- sum(values(caFut)>=1600,na.rm=TRUE)
    SDA.fut$area.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"]  <- sum(values(caZD)>=1600,na.rm=TRUE)
    
    
    # Altitude
    # fd
    if (SDA.fut$area.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] != 0) {
      Alt.fut$mean.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- mean(values(predictos_after_prean$altitude)[values(caFut)>=1600],na.rm=TRUE)
      Alt.fut$q1.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- quantile(values(predictos_after_prean$altitude)[values(caFut)>=1600],0.025,na.rm=TRUE)
      Alt.fut$q2.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- quantile(values(predictos_after_prean$altitude)[values(caFut)>=1600],0.975,na.rm=TRUE)
        }
    # zd
    if (SDA.fut$area.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] != 0) {
      Alt.fut$mean.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] <- mean(values(predictos_after_prean$altitude)[values(caZD)>=1600],na.rm=TRUE)
      Alt.fut$q1.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] <- quantile(values(predictos_after_prean$altitude)[values(caZD)>=1600],0.025,na.rm=TRUE)
      Alt.fut$q2.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] <- quantile(values(predictos_after_prean$altitude)[values(caZD)>=1600],0.975,na.rm=TRUE)
          }
        }
      }
     
# SDA
SDA.fut$perc.change <- round(100*((SDA.fut$area.fut-SDA.fut$area.pres)/SDA.fut$area.pres))
write.table(SDA.fut,paste0("./Araucaria/outputs/sda_fut.txt"),sep="\t")

# Alt
Alt.fut$change <- Alt.fut$mean.fut-Alt.fut$mean.pres
write.table(Alt.fut,paste0("./Araucaria/outputs/Araucaria_alt_fut.txt"),sep="\t")
  
##========================================
## Save main objects
save(list=c("SDA.fut","Alt.fut","niche","Perf.mods","VarImp"),file=paste0("./Araucaria/outputs/main_obj.rda"))

}

# end of first step - Modeling Araucaria potential distribution

## connectivity of Araucaria
# evaluating protected areas for araucaria
# imoport PA in Brazil
# set wd to properly import PA shp

setwd("./uc_brasil/")
ucs <- readOGR("./unidade_protecao_integralPolygon.shp")
#Define proper CRS
projection(ucs) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# imoport PAs in Brazil
ucs_suste <- readOGR("./unidade_uso_sustentavelPolygon.shp") 
#Define proper CRS
projection(ucs_suste) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#+proj=longlat +datum=WGS84 +no_defs (to use above)

# combine both spdf
ucs_full_2 <- rbind(ucs,ucs_suste)
# crop it for study area
ucs_ucs_fim <- crop(ucs_full_2,mascarateste2)
projection(ucs_ucs_fim) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(ucs_ucs_fim, add=T, col="green")


#install.packages("ConR")
library(ConR)
disp <- c("caFD","caZD")
yrs <- c("2080")
dispersal <- c("Full","Zero")

  for (j in 1: length(rcp)) {
    for (m in 1: length(yrs)) {
      for (k in 1: length(disp)) {
        for (q in 1: length(dispersal)) {
        
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1")

    
object <- raster(paste0("./Araucaria/outputs/",rcp[j],"_",yrs[m],"_",disp[k],".tif"))
object_ca <- raster(paste0("./Araucaria/outputs/ca_current.tif"))

names(object) <- c(paste0("rcp_",rcp[j],"_",yrs[m],"_",disp[k]))

# which cells are not NA? These ones:
# notna <- which(!is.na(values(object)))

wC1 <- which(values(object)>=1600)
wC2 <- which(values(object_ca)>=600)

# grab 1000 cell index numbers at random
set.seed(1234)
samp <- sample(wC1, 1000, replace = T)
samp_ca <- sample(wC2,1000,replace=T)

# and their values
sampdata <- object[samp]
sampdata_ca <- object_ca[samp_ca]

# and their location coordinates
samplocs <- xyFromCell(object, samp)
samplocs_ca <- xyFromCell(object_ca, samp_ca)

# convert to a data frame
library(dplyr)
samp <- as.data.frame(cbind(samplocs, samp, sampdata))
names(samp) <- c('lon', 'lat', 'index', 'value')
samp <- samp %>% relocate(lat, lon, index,value)
samp_ca <- as.data.frame(cbind(samplocs_ca, samp_ca, sampdata_ca))
names(samp_ca) <- c('lon', 'lat', 'index', 'value')
samp_ca <- samp_ca %>% relocate(lat, lon, index,value)

# and optionally, a spatial object, in sf or sp
samp_sp <- samp
samp_sp_ca <- samp_ca
coordinates(samp_sp) <- ~lon + lat
coordinates(samp_sp_ca) <- ~lon + lat
crs(samp_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(samp_sp_ca) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#plot(samp_sp, add=T,col="red")
#plot(samp_sp_ca, add=T, col="green")

samp$tax <- rep(c("Araucaria angustifolia"))
samp_ca$tax <- rep(c("Araucaria angustifolia"))

samp$higher.tax.rank <- rep(c("Araucariaceae"))
samp_ca$higher.tax.rank <- rep(c("Araucariaceae"))
samp <- samp[,-3] # twice
samp <- samp[,-3] # twice
#samp <- samp[,c(2,1,3,4)]
samp <- samp %>% rename(ddlat=lat, ddlon=lon,tax=tax,family=higher.tax.rank)
samp_ca <- samp_ca[,-3] # twice
samp_ca <- samp_ca[,-3] # twice
samp_ca <- samp_ca %>% rename(ddlat=lat, ddlon=lon,tax=tax,family=higher.tax.rank)
#samp_ca <- samp_ca[,c(2,1,3,4)]

write.csv(samp,paste0("./Araucaria/outputs/",
                       rcp[j],"_",yrs[m],"_",disp[k],"samp_conR.csv"))
write.csv(samp_ca,paste0("./Araucaria/outputs/ca_samp_conR.csv"))

#back to proper WD

###### generate new maps using IUCN.eval for future
conRtable <- read.csv(paste0("./Araucaria/outputs/",rcp[j],
                         "_",yrs[m],"_",disp[k],"samp_conR.csv"),header=T)

conRtable <- conRtable[,-1]

MyData_samp <- conRtable
mapa_pa_samp <- IUCN.eval(MyData_samp, Cell_size_locations = 10, 
                         protec.areas = ucs_full, ID_shape_PA = "nome", 
                         method_protected_area = "no_more_than_one",
                         DrawMap = T,write_shp = F)

write.csv(mapa_pa_samp,paste0("./Araucaria/outputs/",
                                      rcp[j],"_",yrs[m],"_",disp[k],"_inPA.csv"))

### generata map for current occurrence inside PAs current

conRtable_current <- read.csv(paste0('./Araucaria/outputs/ca_samp_conR.csv'),
                              header=T)
conRtable_current <- conRtable_current[,-1]

MyData_samp_current <- conRtable_current
mapa_pa_samp_current <- IUCN.eval(MyData_samp_current, Cell_size_locations = 1, 
                                  protec.areas = ucs_full, ID_shape_PA = "nome", 
                                  method_protected_area = "no_more_than_one",
                                  DrawMap = T,write_shp = T)

write.csv(mapa_pa_samp_current,paste0("./Araucaria/outputs/currentinPA.csv"))


############################################################################

### Connectivity among predicted occurrence areas

connect <- raster(paste0("./Araucaria/outputs/",rcp[j],"_binary_",yrs[m],"_",dispersal[q],".tif"))
connect_cur <- raster("./Araucaria/outputs/binary_current.tif")


### Create a matrix from rasters
matrix_connect <- as.matrix(connect)  # future data (ensemble all)
matrix_connect_cur <- as.matrix(connect_cur)

SDA.future <- sum(values(connect)>0,na.rm=TRUE) # dá na mesma se colocar >=1
SDA.current <- sum(values(connect_cur)>=1,na.rm=TRUE)

bb_future <- bbox(connect) #fornece uma caixa delimitadora
bb_current <- bbox(connect_cur) #fornece uma caixa delimitadora

cs <- c(0.15, 0.15)  # cell size 
cc_future <- bb_future[, 1] + (cs/2)  # cell offset
cc_current <- bb_current[, 1] + (cs/2)  # cell offset

dd_future <- ceiling(diff(t(bb_future))/cs)  # number of cells per direction - transforma argumento em vetor
dd_current <- ceiling(diff(t(bb_current))/cs)  # number of cells per direction - transforma argumento em vetor

grd_future <- GridTopology(cellcentre.offset=cc_future, cellsize=cs, cells.dim=dd_future)
grd_current <- GridTopology(cellcentre.offset=cc_current, cellsize=cs, cells.dim=dd_current)

## Conversion from topology to poligon (shape)
sp_grd_future <- SpatialGridDataFrame(grd_future, data=data.frame(id=1:prod(dd_future)))
sp_grd_current <- SpatialGridDataFrame(grd_current, data=data.frame(id=1:prod(dd_current)))                        

sp_grd_future <- as(sp_grd_future, "SpatialPixels")
sp_grd_current <- as(sp_grd_current, "SpatialPixels")                                       

sp_grd_future <- as(sp_grd_future, "SpatialPolygons")
sp_grd_current <- as(sp_grd_current, "SpatialPolygons")

# set CRS

proj4string(sp_grd_future) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
proj4string(sp_grd_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
 
names(connect)<- "binary"
names(connect_cur)<- "binary"

shp_future <- rasterToPolygons(connect, fun=function(x){x>0})
shp_current<- rasterToPolygons(connect_cur, fun=function(x){x>0})

proj4string(shp_future) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(shp_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

over_future <- over(sp_grd_future, shp_future) # gathering ucs points 
over_current <- over(sp_grd_current, shp_current) # gathering ucs points 

sp_grd_grdDF_future <- SpatialGridDataFrame(grd_future, data=data.frame(id=1:prod(dd_future)))
sp_grd_grdDF_current <- SpatialGridDataFrame(grd_current, data=data.frame(id=1:prod(dd_current)))

sp_grd_coord_future <- as(sp_grd_grdDF_future, "SpatialPixels")
sp_grd_coord_current <- as(sp_grd_grdDF_current, "SpatialPixels")

sp_grd_coord_DF_future <- as.data.frame(sp_grd_coord_future)
sp_grd_coord_DF_current <- as.data.frame(sp_grd_coord_current)

df_coords_future <- data.frame(over_future, sp_grd_coord_DF_future)
df_coords_current <- data.frame(over_current, sp_grd_coord_DF_current)

names(df_coords_future)<-c("binary", "lonDD", "latDD")
names(df_coords_current)<-c("binary", "lonDD", "latDD")

df_coords_future <-subset(df_coords_future, binary == 1)
df_coords_current <-subset(df_coords_current, binary == 1)

names(df_coords_future)<-c("binary", "lonDD", "latDD")
names(df_coords_current)<-c("binary", "lonDD", "latDD")

# creating graphs using k-nearest neighbours

x_future <- as.matrix(df_coords_future[,2:3])
x_current <- as.matrix(df_coords_current[,2:3])

library(spatgraphs)
g_future <- spatgraph(x_future, "geometric", par=0.15) 
g_current <- spatgraph(x_current, "geometric", par=0.15) 


# Legend
breakpoints <- seq(-1,1,by=1)
#colors <- c(grey(c(0.90,seq(0.7,0.50,-0.05))),gcolors(3))
a.arg <- list(at=seq(-1,1,length.out=3), labels=c(-1,0,1), cex.axis=1.2)
l.arg <- list(text="Presence/Absence",side=2, line=0.5, cex=1.5)

# Plot (Future connectiviy
pdf(paste0("./Araucaria/outputs/connectivity_",rcp[j],"_",yrs[m],"_",
           dispersal[q],".pdf"),width=14,height=10)
#par(mar=c(0,0,0,1),cex=1.4)
plot(connect,col= viridis_pal(option = "B")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
points(df_coords_future$lonDD, df_coords_future$latDD, col="white", pch=20)
plot(g_future,x_future, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="white")
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c(paste0("Future distribution RCP ", 
                          rcp[j]," ",dispersal[q]," 2085"),
                          "Tropic of Capricorn - Dashed Line",
                          "Future connectivity"),
       col=c("black","black","black"),
       fill=c("#BB3754FF","black","white"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Plot (Future connectiviy)
pdf(paste0("./Araucaria/outputs/cur_connectivity1.pdf"),width=14,height=10)
plot(connect_cur,col= viridis_pal(option = "B")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
points(df_coords_current$lonDD, df_coords_current$latDD, col="white", pch=20)
plot(g_current,x_current, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="white")
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Current binary distribution",
                          "Tropic of Capricorn - Dashed Line",
                          "Current connectivity N=732"),
       col=c("black","black","black"),
       fill=c("#BB3754FF","black","white"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()


## Table to compare connectivity 

table_fut <- sum(sapply(g_future$edges, length)>0)
table_current <- sum(sapply(g_current$edges, length)>0)
write.table(table_fut,paste0("./Araucaria/outputs/", dispersal[q],"_",rcp[j],"_",yrs[m],"Araucaria_connectivity.txt"),sep="\t")
write.table(table_current,paste0("./Araucaria/outputs/Araucaria_current_connectivity.txt"),sep="\t")


        }
      }
    }
  }

# Turn all the script
}
#install.packages("rnaturalearthdata")
#library(rnaturalearthdata)

##### testing Protected Areas above


#### remove land-use change above presence/absence maps
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/Mapbiomas")

#list.files(pattern=".tif")
landuse <- raster("uso_do_solo_atual.tif")
landuse <- resample(landuse,mascara_2,"bilinear")
landuse <- crop (landuse, ex_2)
landuse_fim <- mask(landuse,mascarateste2)

# convert land-use areas where more than 30% were converted to agriculture and pasture

binary_map_landuse <- BinaryTransformation(landuse_fim, 0.3) 
plot(binary_map_landuse)

planted_forests <- raster("florestas_plantadas.tif")
planted_forests <- resample(planted_forests,mascara_2,"bilinear")
planted_forests <- crop (planted_forests, ex_2)
planted_forests <- mask(planted_forests,mascarateste2)
plot(planted_forests)

# convert planted-forests where at least 30% was converted to forests plantation
binary_map_planted_forests <- BinaryTransformation(planted_forests, 0.3) 
plot(binary_map_planted_forests)

##### converted area
antro_lands <- raster("areas_antropizadas.tif")
antro_lands <- resample(antro_lands,mascara_2,"bilinear")
antro_lands <- crop (antro_lands, ex_2)
antro_lands <- mask(antro_lands,mascarateste2)

##### converted area
antro_lands <- resample(antro_lands,mascara_2,"bilinear")
antro_lands <- crop (antro_lands, ex_2)
antro_lands <- mask(antro_lands,mascarateste2)

# convert antropic areas where at least 50% was converted to forests plantation
binary_map_antro_lands <- BinaryTransformation(antro_lands, 0.5) 
plot(binary_map_antro_lands)

writeRaster(binary_map_landuse,('binary_land_use'), format = 'GTiff', overwrite=T)
writeRaster(binary_map_planted_forests,('binary_planted_forest'), format = 'GTiff', overwrite=T)
writeRaster(binary_map_antro_lands,('binary_antro_lands'), format = 'GTiff', overwrite=T)
writeRaster(remnant_confut,('future_binary'), format = 'GTiff', overwrite=T)


#o representa áreas de não uso do solo para agricultura e pastagem
### setting land use above presence/absence maps
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1")

### RCP 4.5 2080 - ZERO
dados_finais_45_zero <- raster("./Mapbiomas/dados_finais_land_use_2080_Zero_45.tif")
SDA.fut_land_use_45_zero <- sum(values(dados_finais_45_zero)>0,na.rm=TRUE) ## it works too ;)
# Legend
breakpoints <- seq(-2,1,by=1)
a.arg <- list(at=seq(-2,1,length.out=4), labels=c("","","",""), 
              cex.axis=1.2)
l.arg <- list(text="Land-use / Absence / Presence",side=2, 
              line=0.5, cex=1.5)
# Plot (Future land=use 4.5 2080)
pdf(paste0("./Araucaria/outputs/presence_absence_land_use_45_zero.pdf"),width=14,height=10)
plot(dados_finais_45_zero,col= viridis_pal(option = "D")(4),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-2,1))
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future binary distribution with land-use",
                          "Tropic of Capricorn - Dashed Line",
                          "Land-use (agriculture,forest plantation,urban structure)"),
       col=c("black","black","black"),
       fill=c("#35B779FF","black","#440154FF"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()
################
#### Future 45 full
dados_finais_45_full <- raster("./Mapbiomas/dados_finais_land_use_2080_Full_45.tif")
SDA.fut_land_use <- sum(values(dados_finais)>0,na.rm=TRUE) ## it works too ;)
plot(dados_finais)

# Plot (Future land=use 4.5 2080)
pdf(paste0("./Araucaria/outputs/presence_absence_land_use_45_Full.pdf"),width=14,height=10)
plot(dados_finais_45_full,col= viridis_pal(option = "D")(4),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-2,1))
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future binary distribution with land-use",
                          "Tropic of Capricorn - Dashed Line",
                          "Land-use (agriculture,forest plantation,urban structure)"),
       col=c("black","black","black"),
       fill=c("#35B779FF","black","#440154FF"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

##### 85 full future
### RCP 4.5 2080 - ZERO
dados_finais_85_full <- raster("./Mapbiomas/dados_finais_land_use_2080_Full_85.tif")
SDA.fut_land_use_55_full <- sum(values(dados_finais_85_full)>0,na.rm=TRUE) ## it works too ;)

# Plot (Future land=use 4.5 2080)
pdf(paste0("./Araucaria/outputs/presence_absence_land_use_dados_finais_85_full.pdf"),width=14,height=10)
plot(dados_finais_85_full,col= viridis_pal(option = "D")(4),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-2,1))
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future binary distribution with land-use",
                          "Tropic of Capricorn - Dashed Line",
                          "Land-use (agriculture,forest plantation,urban structure)"),
       col=c("black","black","black"),
       fill=c("#35B779FF","black","#440154FF"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

##### 85 full future
### RCP 4.5 2080 - ZERO
dados_finais_85_zero <- raster("./Mapbiomas/dados_finais_land_use_2080_Zero_85.tif")
SDA.fut_land_use_85_zero <- sum(values(dados_finais_85_zero)>0,na.rm=TRUE) ## it works too ;)

# Plot (Future land=use 8.5 2080)
pdf(paste0("./Araucaria/outputs/presence_absence_land_use_dados_finais_85_zero.pdf"),width=14,height=10)
plot(dados_finais_85_zero,col= viridis_pal(option = "D")(4),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-2,1))
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future binary distribution with land-use",
                          "Tropic of Capricorn - Dashed Line",
                          "Land-use (agriculture,forest plantation,urban structure)"),
       col=c("black","black","black"),
       fill=c("#35B779FF","black","#440154FF"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

########### current distribution
remnant_concur <- raster("./Araucaria/outputs/binary_current.tif")
dados_finais_current <- raster("./Mapbiomas/dados_finais_land_use_current.tif")

# Plot (Current distribution land-use)
pdf(paste0("./Araucaria/outputs/presence_absence_land_use_current.pdf"),width=14,height=10)
plot(dados_finais_current,col= viridis_pal(option = "D")(4),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-2,1))
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Current binary distribution",
                          "Tropic of Capricorn - Dashed Line",
                          "Land-use (agriculture,forest plantation,urban structure)"),
       col=c("black","black","black"),
       fill=c("#35B779FF","black","#440154FF"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# importar os arquivos no formato rasterstack

#### create table and compare habitat loss due to land-use

## No land-use

binary_45_Full <- raster("./Araucaria/outputs/45_binary_2080_Full.tif")
binary_45_Zero <- raster("./Araucaria/outputs/45_binary_2080_Zero.tif")
binary_85_Full <- raster("./Araucaria/outputs/85_binary_2080_Full.tif")
binary_85_Zero <- raster("./Araucaria/outputs/85_binary_2080_Zero.tif")
binary_current <- raster("./Araucaria/outputs/binary_current.tif")

# Calculate suitable climatic areas
binary_45_Full_area <- sum(values(binary_45_Full)>0,na.rm=TRUE)
binary_45_Zero_area <- sum(values(binary_45_Zero)>0,na.rm=TRUE)
binary_85_Full_area <- sum(values(binary_85_Full)>0,na.rm=TRUE)
binary_85_Zero_area <- sum(values(binary_85_Zero)>0,na.rm=TRUE)
binary_current_area <- sum(values(binary_current)>0,na.rm=TRUE) ## it works too ;)

table_binary_area_land_use <- c(47683,47558,15767,15766,135468)
table_binary_area_scenario <- c("binary_45_Full_area","binary_45_Zero_area",
                                "binary_85_Full_area","binary_85_Zero_area",
                                "binary_current_area")
table_binary_area <- as.data.frame(table_binary_area,table_binary_area_scenario)

## Calculate suitable climatic areas with land-use
binary_45_Full_area_land_use <- sum(values(dados_finais_45_full)>0,na.rm=TRUE)
binary_45_Zero_area_land_use <- sum(values(dados_finais_45_zero)>0,na.rm=TRUE)
binary_85_Full_area_land_use <- sum(values(dados_finais_85_full)>0,na.rm=TRUE)
binary_85_Zero_area_land_use <- sum(values(dados_finais_85_zero)>0,na.rm=TRUE)
binary_current_area_land_use <- sum(values(dados_finais_current)>0,na.rm=TRUE) 

table_binary_area_land_use <- c(36365,36240,11993,11993,108174)
table_binary_area_scenario_land_use <- c("binary_45_Full_area_land_use",
                                "binary_45_Zero_area_land_use",
                                "binary_85_Full_area_land_use",
                                "binary_85_Zero_area_land_use",
                                "binary_current_area")

table_binary_area <- as.data.frame(table_binary_area,table_binary_area_scenario)

table_binary_area_land_use2 <- cbind(table_binary_area,
                                    table_binary_area_land_use)

table_binary_area_land_use2$perc.change <- round(100*((table_binary_area_land_use2$table_binary_area_land_use-table_binary_area_land_use2$table_binary_area)/binary_current_area))

write.csv2(table_binary_area_land_use2,paste0("./Araucaria/outputs/land_use_suitable_area_loss.csv"))

##########################

rcp <- c("45","85")
dispersal <- c("Full","Zero")

for (j in 1: length(rcp)) {
      for (q in 1: length(dispersal)) {
## PLOT CONNECTIVITY WITH LAND-USE

### Connectivity among predicted occurrence areas

land_use_future <- raster(paste0("./Mapbiomas/dados_finais_land_use_2080_",dispersal[q],"_",rcp[j],".tif"))

### Create a matrix from rasters
matrix_connect_land_use <- as.matrix(land_use_future)  # future data (ensemble all)

SDA.future_land_use <- sum(values(land_use_future)>0,na.rm=TRUE) # dá na mesma se colocar >=1

bb_future_land_use <- bbox(land_use_future) #fornece uma caixa delimitadora

cs <- c(0.15, 0.15)  # cell size 
cc_future_land_use <- bb_future_land_use[, 1] + (cs/2)  # cell offset

dd_future_land_use <- ceiling(diff(t(bb_future_land_use))/cs)  # number of cells per direction - transforma argumento em vetor

grd_future_land_use <- GridTopology(cellcentre.offset=cc_future_land_use, cellsize=cs, cells.dim=dd_future_land_use)

## Conversion from topology to poligon (shape)
sp_grd_future_land_use <- SpatialGridDataFrame(grd_future_land_use,
                                               data=data.frame(id=1:prod(dd_future_land_use)))

sp_grd_future_land_use <- as(sp_grd_future_land_use, "SpatialPixels")

sp_grd_future_land_use <- as(sp_grd_future_land_use, "SpatialPolygons")

# set CRS

proj4string(sp_grd_future_land_use) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

names(land_use_future)<- "binary"

shp_future_land_use <- rasterToPolygons(land_use_future, fun=function(x){x>0})

proj4string(shp_future_land_use) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

over_future_land_use <- over(sp_grd_future_land_use, shp_future_land_use) # gathering ucs points 

sp_grd_grdDF_future_land_use <- SpatialGridDataFrame(grd_future_land_use, data=data.frame(id=1:prod(dd_future_land_use)))

sp_grd_coord_future_land_use <- as(sp_grd_grdDF_future_land_use, "SpatialPixels")

sp_grd_coord_DF_future_land_use <- as.data.frame(sp_grd_coord_future_land_use)

df_coords_future_land_use <- data.frame(over_future_land_use, sp_grd_coord_DF_future_land_use)

names(df_coords_future_land_use)<-c("binary", "lonDD", "latDD")

df_coords_future_land_use <-subset(df_coords_future_land_use, binary == 1)

# creating graphs using k-nearest neighbours

x_future_land_use <- as.matrix(df_coords_future_land_use[,2:3])

library(spatgraphs)
g_future_land_use <- spatgraph(x_future_land_use, "geometric", par=0.15) 

## Table to compare connectivity 

table_fut_land_use <- sum(sapply(g_future_land_use$edges, length)>0)
write.table(table_fut_land_use,paste0("./Araucaria/outputs/", dispersal[q],"_",rcp[j],"_",yrs[m],"Araucaria_connectivity_table_fut_land_use.txt"),sep="\t")
     
# use brackets here to create only the table

# Legend
breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)

# Plot (Future connectiviy
pdf(paste0("./Araucaria/outputs/connectivity_land_use_",rcp[j],"_",yrs[m],"_",
           dispersal[q],".pdf"),width=14,height=10)

plot(land_use_future,col= viridis_pal(option = "B")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
points(df_coords_future_land_use$lonDD, df_coords_future_land_use$latDD, 
       col="white", pch=20)
plot(g_future_land_use,x_future_land_use, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="white")
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c(paste0("Future distribution RCP ", 
                                 rcp[j]," ",dispersal[q]," 2085"),
                          "Tropic of Capricorn - Dashed Line",
                          "Future connectivity with land-use"),
       col=c("black","black","black"),
       fill=c("#BB3754FF","black","white"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()
      }
  }


### Connectivity among predicted occurrence areas

land_use_current <- raster("./Mapbiomas/dados_finais_land_use_current.tif")

### Create a matrix from rasters
matrix_connect_cur_land_use <- as.matrix(land_use_current)

SDA.current_land_use <- sum(values(land_use_current)>=1,na.rm=TRUE)

bb_current_land_use <- bbox(land_use_current) #fornece uma caixa delimitadora

cs <- c(0.15, 0.15)  # cell size 
cc_current_land_use <- bb_current_land_use[, 1] + (cs/2)  # cell offset

dd_current_land_use <- ceiling(diff(t(bb_current_land_use))/cs)  # number of cells per direction - transforma argumento em vetor

grd_current_land_use <- GridTopology(cellcentre.offset=cc_current_land_use, cellsize=cs, cells.dim=dd_current_land_use)

## Conversion from topology to poligon (shape)
sp_grd_current_land_use <- SpatialGridDataFrame(grd_current_land_use, 
                                                data=data.frame(id=1:prod(dd_current_land_use)))                        

sp_grd_current_land_use <- as(sp_grd_current_land_use, "SpatialPixels")                                       

sp_grd_current_land_use <- as(sp_grd_current_land_use, "SpatialPolygons")

# set CRS

proj4string(sp_grd_current_land_use) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

names(land_use_current)<- "binary"

shp_current_land_use <- rasterToPolygons(land_use_current, fun=function(x){x>0})

proj4string(shp_current_land_use) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

over_current_land_use <- over(sp_grd_current_land_use, shp_current_land_use) # gathering ucs points 

sp_grd_grdDF_current_land_use <- SpatialGridDataFrame(grd_current_land_use, data=data.frame(id=1:prod(dd_current_land_use)))

sp_grd_coord_current_land_use <- as(sp_grd_grdDF_current_land_use, "SpatialPixels")

sp_grd_coord_DF_current_land_use <- as.data.frame(sp_grd_coord_current_land_use)

df_coords_current_land_use <- data.frame(over_current_land_use, sp_grd_coord_DF_current_land_use)

names(df_coords_current_land_use)<-c("binary", "lonDD", "latDD")

df_coords_current_land_use <-subset(df_coords_current_land_use, binary == 1)


# creating graphs using k-nearest neighbours

x_current_land_use <- as.matrix(df_coords_current_land_use[,2:3])

library(spatgraphs)
g_current_land_use <- spatgraph(x_current_land_use, "geometric", par=0.15) 

## Table to compare connectivity 
# written within connectivity loop ;)

# Legend
breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)

# Plot (Current connectiviy land_use)
pdf(paste0("./Araucaria/outputs/current_connectivity_land_use.pdf"),width=14,height=10)

plot(land_use_current,col= viridis_pal(option = "B")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
points(df_coords_current_land_use$lonDD, df_coords_current_land_use$latDD, 
       col="white", pch=20)
plot(g_current_land_use,x_current_land_use, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="white")
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c(paste0("Current distribution with land-use"),
                          "Tropic of Capricorn - Dashed Line",
                          "Future connectivity with land-use"),
       col=c("black","black","black"),
       fill=c("#BB3754FF","black","white"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

table_current_land_use <- sum(sapply(g_current_land_use$edges, length)>0)
write.table(table_current_land_use,paste0("./Araucaria/outputs/Araucaria_current_connectivity_table_current_land_use.txt"),sep="\t")

############## New strategy Protected Areas


binary_45_Full <- raster("./Araucaria/outputs/45_binary_2080_Full.tif")
binary_45_Zero <- raster("./Araucaria/outputs/45_binary_2080_Zero.tif")
binary_85_Full <- raster("./Araucaria/outputs/85_binary_2080_Full.tif")
binary_85_Zero <- raster("./Araucaria/outputs/85_binary_2080_Zero.tif")
binary_current <- raster("./Araucaria/outputs/binary_current.tif")


#### Create rasters within the protected areas only 
binary_45_Full_Ucs <- mask(dados_finais_45_full, ucs_ucs_fim)
plot(binary_45_Full_Ucs)

binary_45_Zero_Ucs <- mask(dados_finais_45_zero, ucs_ucs_fim)
plot(binary_45_Zero_Ucs)

binary_85_Full_Ucs <- mask(dados_finais_85_full, ucs_ucs_fim)
plot(binary_85_Full_Ucs)

binary_85_Zero_Ucs <- mask(dados_finais_85_zero, ucs_ucs_fim)
plot(binary_85_Zero_Ucs)

binary_current_Ucs <- mask(dados_finais_current, ucs_ucs_fim)
plot(binary_current_Ucs)

### save .tif rasters
setwd("./outputs")
writeRaster(binary_45_Full_Ucs, filename="./Araucaria/outputs/binary_45_Full_Ucs.tif", 
            format='GTiff', overwrite=T)
writeRaster(binary_45_Zero_Ucs, filename="./Araucaria/outputs/binary_45_Zero_Ucs.tif", 
            format='GTiff', overwrite=T)
writeRaster(binary_85_Full_Ucs, filename="./Araucaria/outputs/binary_85_Full_Ucs.tif", 
            format='GTiff', overwrite=T)
writeRaster(binary_85_Zero_Ucs, filename="./Araucaria/outputs/binary_85_Zero_Ucs.tif", 
            format='GTiff', overwrite=T)
writeRaster(binary_current_Ucs, filename="./Araucaria/outputs/binary_current_Ucs.tif", 
            format='GTiff', overwrite=T)

## Calculate suitable climatic areas with land-use
binary_45_Full_Ucs_area <- sum(values(binary_45_Full_Ucs)>0,na.rm=TRUE)
binary_45_Zero_Ucs_area <- sum(values(binary_45_Zero_Ucs)>0,na.rm=TRUE)
binary_85_Full_Ucs_area <- sum(values(binary_85_Full_Ucs)>0,na.rm=TRUE)
binary_85_Zero_Ucs_area <- sum(values(binary_85_Zero_Ucs)>0,na.rm=TRUE)
binary_current_Ucs_area <- sum(values(binary_current_Ucs)>0,na.rm=TRUE) 

table_binary_area_land_use_ucs <- c(2443,2361,1352,1352,7500)
table_binary_area_scenario_land_use_ucs <- c("binary_45_Full_area_land_use_ucs",
                                         "binary_45_Zero_area_land_use_ucs",
                                         "binary_85_Full_area_land_use_ucs",
                                         "binary_85_Zero_area_land_use_ucs",
                                         "binary_current_area_ucs")
table_binary_area_ucs <- as.data.frame(table_binary_area_land_use_ucs,
                                       table_binary_area_scenario_land_use_ucs)

table_binary_area_land_use_ucs_final <- cbind(table_binary_area_land_use2,
                                              table_binary_area_ucs)


table_binary_area_land_use_ucs_final$perc.change_ucs <- round(100*((table_binary_area_land_use_ucs_final$table_binary_area_land_use-table_binary_area_land_use_ucs_final$table_binary_area_land_use_ucs)/table_binary_area_land_use_ucs_final$table_binary_area_land_use)-100)

## Atention, Im just tired to think, but this table give you a negative perc.change_ucs
## in fact the result is the positive value. I fix it lates. Peace
write.csv2(table_binary_area_land_use_ucs_final,paste0("./Araucaria/outputs/how_much_within_pa.csv"))


### Plot the last figures of this article

# Plot (Current final_figure land_use + pa within)
pdf(paste0("./Araucaria/outputs/final_distribution_current_land_use_pa_within.pdf"),width=14,height=10)

# Legend
binary_current_teste <- BinaryTransformation(binary_current_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_current_teste) <- "+proj=longlat +datum=WGS84 +no_defs"
crs(dados_finais_current) <- "+proj=longlat +datum=WGS84 +no_defs"
crs(estados) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)
# Plot (Future land=use 4.5 2080)
pdf(paste0("./Araucaria/outputs/current_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_current,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_current_teste,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Current distribution with land-use",
                          "Tropic of Capricorn - Dashed Line",
                          "Current distribution within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Plot future scenarios

# Legend
binary_45_Full_Ucs_within <- BinaryTransformation(binary_45_Full_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_45_Full_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)
# Plot (Future land=use 4.5 2080 Full)
pdf(paste0("./Araucaria/outputs/future_full_45_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_45_full,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_45_Full_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
#plot(ucs_ucs_fim,col="transparent",contour="white",add=T)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 4.5 Full Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Future 4.5 Zero 2085
# Legend
binary_45_Zero_Ucs_within <- BinaryTransformation(binary_45_Zero_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_45_Zero_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Plot (Future land=use 4.5 2080 Zero)
pdf(paste0("./Araucaria/outputs/future_zero_45_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_45_zero,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_45_Zero_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 4.5 Zero Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Legend
binary_85_Full_Ucs_within <- BinaryTransformation(binary_85_Full_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_85_Full_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Plot (Future land=use 4.5 2080 Zero)
pdf(paste0("./Araucaria/outputs/future_full_85_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_85_full,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_85_Full_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 8.5 Full Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

# Legend
binary_85_Zero_Ucs_within <- BinaryTransformation(binary_85_Zero_Ucs, 0) #at least three models, 600 counts 3 at least
crs(binary_85_Zero_Ucs_within) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Plot (Future land=use 4.5 2080 Zero)
pdf(paste0("./Araucaria/outputs/future_zero_85_land_use_distri_within_PA.pdf"),width=14,height=10)
plot(dados_finais_85_zero,col= viridis_pal(option = "D")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(binary_85_Zero_Ucs_within,col=viridis_pal(option = "D",alpha = 0.8)(2),
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="red")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="blue")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="orange")
#plot(ucs_full,add=T, col="transparent",contour="white")
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Future ocurrence with land-use (RCP 8.5 Zero Dispersion 2085)",
                          "Tropic of Capricorn - Dashed Line",
                          "Future occurrence within Protected Areas",
                          "All Protected Areas contour"),
       col=c("black","black","black","black"),
       fill=c("#21908CFF","black","#FDE725CC","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

##########################

rcp <- c("45","85")
dispersal <- c("Full","Zero")

for (j in 1: length(rcp)) {
  for (q in 1: length(dispersal)) {
    ## PLOT CONNECTIVITY WITH LAND-USE
    
    ### Connectivity among predicted occurrence areas
    
    land_use_future_within_uc <- raster(paste0("./Araucaria/outputs/binary_",rcp[j],"_",dispersal[q],"_Ucs.tif"))
    land_use_future_within_uc <- BinaryTransformation(land_use_future_within_uc, 0) #at least three models, 600 counts 3 at least
    #land_use_current_within_uc <- raster(paste0("./Araucaria/outputs/binary_current_Ucs.tif"))
     
    ### Create a matrix from rasters
    matrix_connect_land_use_uc <- as.matrix(land_use_future_within_uc)  # future data (ensemble all)
    #matrix_connect_land_use_uc <- as.matrix(land_use_current_within_uc)  # future data (ensemble all)
    
    SDA.future_land_use_uc <- sum(values(land_use_future_within_uc)>0,na.rm=TRUE) # dá na mesma se colocar >=1
    
    bb_future_land_use_uc <- bbox(land_use_future_within_uc) #fornece uma caixa delimitadora
    
    cs <- c(0.15, 0.15)  # cell size 
    cc_future_land_use_uc <- bb_future_land_use_uc[, 1] + (cs/2)  # cell offset
    
    dd_future_land_use_uc <- ceiling(diff(t(bb_future_land_use_uc))/cs)  # number of cells per direction - transforma argumento em vetor
    
    grd_future_land_use_uc <- GridTopology(cellcentre.offset=cc_future_land_use_uc, cellsize=cs, cells.dim=dd_future_land_use_uc)
    
    ## Conversion from topology to poligon (shape)
    sp_grd_future_land_use_uc <- SpatialGridDataFrame(grd_future_land_use_uc,
                                                   data=data.frame(id=1:prod(dd_future_land_use_uc)))
    
    sp_grd_future_land_use_uc <- as(sp_grd_future_land_use_uc, "SpatialPixels")
    
    sp_grd_future_land_use_uc <- as(sp_grd_future_land_use_uc, "SpatialPolygons")
    
    # set CRS
    
    proj4string(sp_grd_future_land_use_uc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    
    names(land_use_future_within_uc)<- "binary"
    
    shp_future_land_use_uc <- rasterToPolygons(land_use_future_within_uc, fun=function(x){x>0})
    
    proj4string(shp_future_land_use_uc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    over_future_land_use_uc <- over(sp_grd_future_land_use_uc, shp_future_land_use_uc) # gathering ucs points 
    
    sp_grd_grdDF_future_land_use_uc <- SpatialGridDataFrame(grd_future_land_use_uc, data=data.frame(id=1:prod(dd_future_land_use_uc)))
    
    sp_grd_coord_future_land_use_uc <- as(sp_grd_grdDF_future_land_use_uc, "SpatialPixels")
    
    sp_grd_coord_DF_future_land_use_uc <- as.data.frame(sp_grd_coord_future_land_use_uc)
    
    df_coords_future_land_use_uc <- data.frame(over_future_land_use_uc, sp_grd_coord_DF_future_land_use_uc)
    
    names(df_coords_future_land_use_uc)<-c("binary", "lonDD", "latDD")
    
    df_coords_future_land_use_uc <-subset(df_coords_future_land_use_uc, binary == 1)
    
    # creating graphs using k-nearest neighbours
    
    x_future_land_use_uc <- as.matrix(df_coords_future_land_use_uc[,2:3])
    g_future_land_use_uc <- spatgraph(x_future_land_use_uc, "geometric", par=0.15) 
    
    ## Table to compare connectivity within PAs
    
    table_fut_land_use_uc <- sum(sapply(g_future_land_use_uc$edges, length)>0)
    write.table(table_fut_land_use_uc,paste0("./Araucaria/outputs/", dispersal[q],"_",rcp[j],"_",yrs[m],"_Araucaria_connectivity_table_fut_land_use_uc.txt"),sep="\t")
          
    
        }
      }
    # Legend
   
    breakpoints <- seq(-1,1,by=1)
    a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
    l.arg <- list(text="Absence/Presence",side=2, line=0.5, cex=1.5)
    
    
    crs(binary_current_teste) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    crs(dados_finais_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    crs(dados_finais_85_zero) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    colors <- c("gray40", "gray10", "#FF0800") 
    # Plot (Future connectiviy
    pdf(paste0("./Araucaria/outputs/connectivity_land_use_within_without_uc_revision",rcp[j],"_",yrs[m],"_",
               dispersal[q],".pdf"),width=14,height=10)
    
    pdf(paste0("./Araucaria/outputs/connectivity_land_use_within_without_uc_revision_finalmap.pdf"),width=14,height=10)
    plot(dados_finais_85_zero,col= colors,breaks=breakpoints,ext=mascara_2,
         legend.width=1.5,legend.shrink=0.6,legend.mar=7,
         axis.args=a.arg,legend.arg=l.arg,
         axes=FALSE,box=FALSE,zlim=c(-1,1))
    plot(land_use_future_within_uc,col=colors,#viridis_pal(option = "D",alpha = 0.8)(3),
         axes=F,box=F,add=T,legend=F)
    plot(estados[estados$Regiao=="SUL",], add=T,border="black")
    plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="black")
    plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="black")
    points(df_coords_future_land_use$lonDD, df_coords_future_land_use$latDD, 
           col="#00FFFF", pch=20) #00FFFF 
    plot(g_future_land_use,x_future_land_use, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
         add=T, col="#00FFFF")
    points(df_coords_future_land_use_uc$lonDD, df_coords_future_land_use_uc$latDD, 
           col="#FFFAFA", pch=20)
    plot(g_future_land_use_uc,x_future_land_use_uc, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
         add=T, col="#FFFAFA")
    abline(h=-23.5, lty=2, col="black")
    legend(-48, -27, legend=c(paste0("Future occurrence RCP ", 
                                     rcp[j]," ",dispersal[q]," 2085"),
                              "Future occurrence within PAs",
                              "Tropic of Capricorn - Dashed Line",
                              "Future connectivity outside PAs",
                              "Future connectivity within PAs",
                              "All Protected Areas contour"),
           col=c("black","black","black","black","black","black"),
           fill=c("gray50","#FDE725CC","black","gray70","green","transparent"),box.lty=0)
        scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
             lonlat = T, lwd = 6)
    dev.off()
   

land_use_current_within_uc <- raster(paste0("./Araucaria/outputs/binary_current_Ucs.tif"))
land_use_current_within_uc <- BinaryTransformation(land_use_current_within_uc, 0) #at least three models, 600 counts 3 at least

### Create a matrix from rasters
matrix_connect_land_use_uc <- as.matrix(land_use_current_within_uc)  # current data (ensemble all)

SDA.current_land_use_uc <- sum(values(land_use_current_within_uc)>0,na.rm=TRUE) # dá na mesma se colocar >=1

bb_current_land_use_uc <- bbox(land_use_current_within_uc) #fornece uma caixa delimitadora

cs <- c(0.15, 0.15)  # cell size 
cc_current_land_use_uc <- bb_current_land_use_uc[, 1] + (cs/2)  # cell offset

dd_current_land_use_uc <- ceiling(diff(t(bb_current_land_use_uc))/cs)  # number of cells per direction - transforma argumento em vetor

grd_current_land_use_uc <- GridTopology(cellcentre.offset=cc_current_land_use_uc, cellsize=cs, cells.dim=dd_current_land_use_uc)

## Conversion from topology to poligon (shape)
sp_grd_current_land_use_uc <- SpatialGridDataFrame(grd_current_land_use_uc,
                            data=data.frame(id=1:prod(dd_current_land_use_uc)))

sp_grd_current_land_use_uc <- as(sp_grd_current_land_use_uc, "SpatialPixels")

sp_grd_current_land_use_uc <- as(sp_grd_current_land_use_uc, "SpatialPolygons")

# set CRS

proj4string(sp_grd_current_land_use_uc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

names(land_use_current_within_uc)<- "binary"

shp_current_land_use_uc <- rasterToPolygons(land_use_current_within_uc, 
                                            fun=function(x){x>0})

proj4string(shp_current_land_use_uc) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

over_current_land_use_uc <- over(sp_grd_current_land_use_uc, shp_current_land_use_uc) # gathering ucs points 

sp_grd_grdDF_current_land_use_uc <- SpatialGridDataFrame(grd_current_land_use_uc, data=data.frame(id=1:prod(dd_current_land_use_uc)))

sp_grd_coord_current_land_use_uc <- as(sp_grd_grdDF_current_land_use_uc, "SpatialPixels")

sp_grd_coord_DF_current_land_use_uc <- as.data.frame(sp_grd_coord_current_land_use_uc)

df_coords_current_land_use_uc <- data.frame(over_current_land_use_uc, sp_grd_coord_DF_current_land_use_uc)

names(df_coords_current_land_use_uc)<-c("binary", "lonDD", "latDD")

df_coords_current_land_use_uc <-subset(df_coords_current_land_use_uc, binary == 1)

# creating graphs using k-nearest neighbours

x_current_land_use_uc <- as.matrix(df_coords_current_land_use_uc[,2:3])
g_current_land_use_uc <- spatgraph(x_current_land_use_uc, "geometric", par=0.15) 

## Table to compare connectivity within PAs

table_current_land_use_uc <- sum(sapply(g_current_land_use_uc$edges, length)>0)
write.table(table_current_land_use_uc,paste0("./Araucaria/outputs/Araucaria_current_connectivity_table_current_land_use_uc.txt"),sep="\t")

# Legend
breakpoints <- seq(-1,1,by=1)
a.arg <- list(at=seq(-1,1,length.out=3), labels=c("","",""), cex.axis=1.2)
l.arg <- list(text="Absence / Presence",side=2, line=0.5, cex=1.5)


crs(binary_current_teste) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(dados_finais_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

colors <- c("gray40", "gray10", "#FF0800") 
#colors2 <- c("gray50", "gray70", "#39FF14")
#viridis_pal(option = "D")(3)

# Plot (Current connectiviy)
pdf(paste0("./Araucaria/outputs/current_connectivity_land_use_within_without_uc_minor.pdf"),width=14,height=10)
plot(dados_finais_current,col= colors,breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
plot(land_use_current_within_uc,col= colors,
     axes=F,box=F,add=T,legend=F)
plot(estados[estados$Regiao=="SUL",], add=T,border="black")
plot(estados[estados$Nome=="SÃƒO PAULO",], add=T,border="black")
plot(estados[estados$Nome=="RIO DE JANEIRO",], add=T,border="black")
points(df_coords_current_land_use$lonDD, df_coords_current_land_use$latDD, 
       col="#00FFFF", pch=20)
plot(g_current_land_use,x_current_land_use, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="#00FFFF")
points(df_coords_current_land_use_uc$lonDD, df_coords_current_land_use_uc$latDD, 
       col="#FFFAFA", pch=20) #D01C8B
plot(g_current_land_use_uc,x_current_land_use_uc, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="#FFFAFA")#FF007F 
abline(h=-23.5, lty=2, col="black")
legend(-48, -27, legend=c("Predicted occurrence with land-use",
                          "Predicted occurrence within PAs",
                          "Tropic of Capricorn - Dashed Line",
                          "Current connectivity outside PAs",
                          "Current connectivity within PAs",
                          "All Protected Areas contour"),
       col=c("black","black","black","black","black","black"),
       fill=c("gray50","#FDE725CC","black","#00FFFF","#FFFAFA","transparent"),box.lty=0)
scalebar(500, xy = c(-47.5,-29.5), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, lwd = 6)
dev.off()

#getwd()
library(session)
memory.limit(size = 100000000000000000000)
##getwd()
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1")
#save.session("araucaria_2020_plos.Rda")
#restore.session("araucaria_2020.Rda")
#restore.session("araucaria_2020_plos.Rda")
