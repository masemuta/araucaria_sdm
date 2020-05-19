###################################################
## load library
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

#library(installr)
#updateR()
library(plotly)
library(RColorBrewer)
library(viridis)
#library(rJava)
library(raster)
library(rgdal)
library(ENMeval)
library(maptools)
library(XML)
library(dismo) 
#library(maxent) 
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
library(raster)
library(dismo)
library(maxnet)
library(here)
#help("here")

# Getting environmental variables

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/Bioclim/bio_clim_3")

#list.files(pattern=".tif")
bil <- list.files(pattern=".bil") #30 arc

# importar os arquivos no formato rasterstack
#
# importar os arquivos no formato rasterstack
bil.bios <- stack(bil)
plot(bil.bios)

# renomear os arquivos .tif importados
names(bif.bios) <- paste0('bio', 1:19)

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

# altitude
altmat_2 <- raster("./MDE_Malu/dados_mde_malu/rastert_alt_00.asc") # Altitude
# remnants atlantic rainfores
rem_mata <- raster("./remanescente_MA/ma_rem.tif")


#define proper CRS
crs(altmat_2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(rem_mata) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

mata_atlantica <- readOGR("./Contorno_MA_Original/Contorno MA Original.shp")
data.shape <-readOGR("./FOM_ecoregion/Ecorregiões Sul.shp")
mascara <- data.shape[data.shape$ECO_NAME == "Araucaria moist forests",]

# Set same extent:
mascara_2 <- raster(mascara)
res(mascara_2) <- 0.008333333 # resolution from altitude

r.new2 <- resample(rem_mata,mascara_2,"bilinear") # this one is ok
#r.new3 <- resample(altmat_2,altmat_2,"bilinear") 
r.new3 <- resample(altmat_2,mascara_2,"bilinear")

#### Worldclim bioclimatic variables # set same rosolution
r.new01 <- resample(bil.bios$bio_1,mascara_2,"bilinear")
r.new02 <- resample(bil.bios$bio_2,mascara_2,"bilinear")
r.new03 <- resample(bil.bios$bio_3,mascara_2,"bilinear")
r.new04 <- resample(bil.bios$bio_4,mascara_2,"bilinear")
r.new05 <- resample(bil.bios$bio_5,mascara_2,"bilinear")
r.new06 <- resample(bil.bios$bio_6,mascara_2,"bilinear")
r.new07 <- resample(bil.bios$bio_7,mascara_2,"bilinear")
r.new08 <- resample(bil.bios$bio_8,mascara_2,"bilinear")
r.new09 <- resample(bil.bios$bio_9,mascara_2,"bilinear")
r.new10 <- resample(bil.bios$bio_10,mascara_2,"bilinear")
r.new11 <- resample(bil.bios$bio_11,mascara_2,"bilinear")
r.new12 <- resample(bil.bios$bio_12,mascara_2,"bilinear")
r.new13 <- resample(bil.bios$bio_13,mascara_2,"bilinear")
r.new14 <- resample(bil.bios$bio_14,mascara_2,"bilinear")
r.new15 <- resample(bil.bios$bio_15,mascara_2,"bilinear")
r.new16 <- resample(bil.bios$bio_16,mascara_2,"bilinear")
r.new17 <- resample(bil.bios$bio_17,mascara_2,"bilinear")
r.new18 <- resample(bil.bios$bio_18,mascara_2,"bilinear")
r.new19 <- resample(bil.bios$bio_19,mascara_2,"bilinear")
## or 
#=======================================================
ex_2 <- extent(mascara_2)

#mataatl_1 <- crop(r.new2,ex_2)
#altmat_1 <- crop(r.new3,ex_2)
# ===========================
#bio01_1 <-  crop(mascara_2,bil.bios$bio_19)
#bio02_1 <- crop(r.new02,ex_2)
#bio03_1 <- crop(r.new03,ex_2)
#bio04_1 <- crop(r.new04,ex_2)
#bio05_1 <- crop(r.new05,ex_2)
#bio06_1 <- crop(r.new06,ex_2)
#bio07_1 <- crop(r.new07,ex_2)
#bio08_1 <- crop(r.new08,ex_2)
#bio09_1 <- crop(r.new09,ex_2)
#bio10_1 <- crop(r.new10,ex_2)
#bio11_1 <- crop(r.new11,ex_2)
#bio12_1 <- crop(r.new12,ex_2)
#bio13_1 <- crop(r.new13,ex_2)
#bio14_1 <- crop(r.new14,ex_2)
#bio15_1 <- crop(r.new15,ex_2)
##bio16_1 <- crop(r.new16,ex_2)
#bio17_1 <- crop(r.new17,ex_2)
#bio18_1 <- crop(r.new18,ex_2)
#bio19_1 <- crop(r.new19,ex_2)

# Removendo dados que ficam foram do raster extent da FOM
r.new_2 <- mask(r.new2,mascara)
r.new_3 <- mask(r.new3, mascara)
plot(r.new_3)

###################################
r.new_01 <- mask(r.new01, mascara)
r.new_02 <- mask(r.new02, mascara)
r.new_03 <- mask(r.new03, mascara)
r.new_04 <- mask(r.new04, mascara)
r.new_05 <- mask(r.new05, mascara)
r.new_06 <- mask(r.new06, mascara)
r.new_07 <- mask(r.new07, mascara)
r.new_08 <- mask(r.new08, mascara)
r.new_09 <- mask(r.new09, mascara)
r.new_10 <- mask(r.new10, mascara)
r.new_11 <- mask(r.new11, mascara)
r.new_12 <- mask(r.new12, mascara)
r.new_13 <- mask(r.new13, mascara)
r.new_14 <- mask(r.new14, mascara)
r.new_15 <- mask(r.new15, mascara)
r.new_16 <- mask(r.new16, mascara)
r.new_17 <- mask(r.new17, mascara)
r.new_18 <- mask(r.new18, mascara)
r.new_19 <- mask(r.new19, mascara)

# TESTANDO 
par(mfrow=c(1,1))
plot(r.new_01)
plot(mascara,add=T, axes=T)

predictors_good <- stack(r.new_2,r.new_3,r.new_01,r.new_02,
                         r.new_03,r.new_04,r.new_05,r.new_06,r.new_07,r.new_08,r.new_09,
                         r.new_10,r.new_11,r.new_12,r.new_13,r.new_14,r.new_15,r.new_16,
                         r.new_17,r.new_18,r.new_19)
### 13:11 - 13:59
#predictors_good_1 <- mask(predictors_good, mascara)
predictors_good_1 <- predictors_good 

## all variables inside the study mask
names(predictors_good_1) <- c("remnant", "altitude","bio01","bio02","bio03", "bio04","bio05","bio06",
                              "bio07","bio08","bio09","bio10","bio11","bio12","bio13",
                              "bio14","bio15","bio16","bio17","bio18","bio19")

#create folder to save organized (inside the mask) variables
#dir.create('variaves_mascara_2020')
setwd('./variaves_mascara_2020') 


writeRaster(predictors_good_1,('all_variables_mask'), format = 'GTiff', overwrite=T)


list.files(pattern=".tif")
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
#dir.create('table_correlation_2020')
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/table_correlation_2020/") 


#======================= AVALIANDO COLINEARIDADE ENTRE VARIAVEIS (PCA E TAL) ===========

# Avaliando se tenho problema nas minhas variáveis

remn <- c(as.matrix(predictors_good_1$remnant)) # mata atlanttica
altmat_val <- c(as.matrix(predictors_good_1$altitude)) # altitude
##############
bio01_val <- c(as.matrix(predictors_good_1$bio01)) # bio1
bio02_val <- c(as.matrix(predictors_good_1$bio02)) # bio2
bio03_val <- c(as.matrix(predictors_good_1$bio03)) # bio3
bio04_val <- c(as.matrix(predictors_good_1$bio04)) # bio4
bio05_val <- c(as.matrix(predictors_good_1$bio05)) # bio5
bio06_val <- c(as.matrix(predictors_good_1$bio06)) # bio6
bio07_val <- c(as.matrix(predictors_good_1$bio07)) # bio7
bio08_val <- c(as.matrix(predictors_good_1$bio08)) # bio8
bio09_val <- c(as.matrix(predictors_good_1$bio09)) # bio9
bio10_val <- c(as.matrix(predictors_good_1$bio10)) # bio10
bio11_val <- c(as.matrix(predictors_good_1$bio11)) # bio11
bio12_val <- c(as.matrix(predictors_good_1$bio12)) # bio12
bio13_val <- c(as.matrix(predictors_good_1$bio13)) # bio13
bio14_val <- c(as.matrix(predictors_good_1$bio14)) # bio14
bio15_val <- c(as.matrix(predictors_good_1$bio15)) # bio15
bio16_val <- c(as.matrix(predictors_good_1$bio16)) # bio16
bio17_val <- c(as.matrix(predictors_good_1$bio17)) # bio17
bio18_val <- c(as.matrix(predictors_good_1$bio18)) # bio18
bio19_val <- c(as.matrix(predictors_good_1$bio19)) # bio19

## create a data-frame
tabela <- data.frame(cbind(remn,
                           altmat_val,bio01_val,bio02_val,bio03_val,bio04_val,bio05_val,bio06_val,
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

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
# dir.create('table_correlation_2019')  # criar pasta no diretorio
setwd("./table_correlation_2020")

hist(cor(tabela_validos), main="Variable_correlation", col="gray90")

write.table(round(cor(tabela_validos), 2), 'cor_pres_2020.xls', row.names = T, sep = '\t')

write.table(ifelse(cor(tabela_validos) >= 0.7, 'Sim', 'Não'), 'cor_pres_afirmacao_2020.xls', row.names = T, 
            sep = '\t')
see <- (ifelse(cor(tabela_validos) >= 0.7, 'Yes', 'No'))

# plot da correlacao
#install.packages("corrplot")
library(corrplot)

tiff('camadas_ambientais_2020.tif', width = 25, height= 25, units = 'cm', res = 1000, compression = 'lzw')
corrplot(cor(tabela_validos), type = 'lower', diag = F, tl.srt = 45, mar = c(3, 1.5, 3, 1.5),
         title = 'Correlation Bioclimatic Variables')
dev.off()

# ================================================================

# exportar a figura
tiff('cor_2.tiff', width = 10, height = 12, units = 'in', res = 1000, compression = 'lzw')
corrplot(cor(tabela_validos), type = "lower", diag = F, 
         title = 'Correlation Bioclimatic Variables', 
         mar = c(3, 0.5, 2, 1), tl.srt = 45)
dev.off()


# exportar tabela
tabela.cor <- cor(tabela_validos)
write.table(tabela.cor, 'tabela_cor_2020.txt', row.names = F, quote = F, sep = '\t')
write.table(round(tabela.cor, 2), 'tabela_cor_2020.xls', row.names = F, 
            quote = F, sep = '\t')

# PCA for colinearity among variables
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
#dir.create('pca_2020') 
setwd('./pca_2020')

# pca do pacote 'stats'

pca <- prcomp(tabela_validos, scale = T)

# eigenvalues
summary(pca)

# grafico de barras com as contribuicoes
screeplot(pca, main = 'Autovalues')
abline(h = 1, col = 'red', lty = 2) # 5 variables represent the whole bioclim variables

# import the figure
tiff('screeplot_2220.tif', width = 20, height = 20, units = 'cm', 
     res = 1000, compression = 'lzw')
screeplot(pca, main = 'PCA Auto-value')
abline(h = 1, col = 'red', lty = 2)
dev.off()

# valores de cada eixo (eigenvectors - autovetores - escores)
pca$x

# relacao das variaveis com cada eixo (loadings - cargas)
pca$rotation[, 1:5]
abs(pca$rotation[, 1:5])

# exportar tabela com a contribuicao
write.table(round(abs(pca$rotation[, 1:5]), 2), 'contr_pca_2020.xls', 
            row.names = T, sep = '\t')

# plot
biplot(pca)
# import the figure
tiff('pca_2220.tif', width = 20, height = 20, units = 'cm', 
     res = 1000, compression = 'lzw')
biplot(pca, main = 'PCA Auto-value')
#abline(h = 1, col = 'red', lty = 2)
dev.off()

# extraindo valores dos arquivos .asc e omitindo os NAs
as.v <- values(predictors_good_1)
as.v.na <- na.omit(as.v)
head(as.v.na)
dim(as.v.na)

# pca como novas variaveis
# pca dos raster
#install.packages('RStoolbox')
library(RStoolbox)
pca.as <- rasterPCA(predictors_good_1, spca = T) # esse comando ira demorar

# contribuicao dos componentes
summary(pca.as$model)

# grafico de barras com as contribuicoes
getwd()
screeplot(pca.as$model, main = 'Autovalues')
abline(h = 1, col = 'red', lty = 2)
dev.off()


tiff('screeplot_raster.tif', width = 20, height = 20, units = 'cm',
     res = 1000, compression = 'lzw')
screeplot(pca.as$model, main = 'Autovalues')
abline(h = 1, col = 'red', lty = 2)
dev.off()

# plot das pcs como novas variaveis
plot(pca.as$map[[1:5]])

# exportar as novas variaveis
setwd('')
for(i in 1:5){
  writeRaster(pca.as$map[[i]], paste0('pc', i, '_br.asc'), 
              format = 'ascii', overwrite=T)}

###Esta parte padroniza todas as variaveis para terem media ZERO e variancia UM
###Com isto todas as variveis terao igual peso na hora de gerar os PCAs

library(vegan)
tabela_validos <- decostand(tabela_validos, method="standardize") 

# Pq standardize? Deixar todos com a mesma media
## e mesma variancia para que cada variavel responda de modo semelhante a PCA

summary(tabela_validos) # veja as Mean

#.........................
# Eliminar redundancias via Componente Principal - eliminar o maximo de redundancia possivel. Das vari?veis
## principais nos geraremos os componentes e avaliaremos quais sao as que mais variam

tabela_validos_prcomp <- prcomp(tabela_validos)

plot(tabela_validos_prcomp)
axis(1)

###Apresenta variancia para cada PC
std_list <- tabela_validos_prcomp$sdev
std_list_pct <- std_list / sum (std_list) * 100
round(std_list_pct, 2)

# aqui o resultado mostra os valores que indicam a
#porcentagem de variancia em cada eixo

### Correlacao entre cada variavel e as PCs
tabela_validos_prcomp

round(data.frame(tabela_validos_prcomp[2]),2)

head(round(predict(tabela_validos_prcomp),2))

###CRIAR image PC1 usando bio01 como referencia
###  ... e transformar tudo em NA = sem valor
imagem_PC2  <- bio04_val
imagem_PC2[] <- NA
x11()
par(mfrow=c(2,2))
hist(data.frame(predict(tabela_validos_prcomp))$PC1, main="")
hist(data.frame(predict(tabela_validos_prcomp))$PC3, main="")
hist(data.frame(predict(tabela_validos_prcomp))$PC6, main="")
hist(data.frame(predict(tabela_validos_prcomp))$PC8, main="")

imagem_PC2 <- bio01_val
imagem_PC2[]<-NA

x11()
par(mfrow=c(2,2))
hist(data.frame(predict(tabela_validos_prcomp))$PC1, main="")
hist(data.frame(predict(tabela_validos_prcomp))$PC3, main="")
hist(data.frame(predict(tabela_validos_prcomp))$PC6, main="")
hist(data.frame(predict(tabela_validos_prcomp))$PC8, main="")

### factorial analysys

#install.packages('psych')
#install.packages('GPArotation')
library(psych)
library(GPArotation)
library(vegan)

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

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
tiff('screeplot_fatorial_2.tif', width = 20, height = 20, units = 'cm', res = 1000,
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
# usar Bio 01, 04, 11, 12, alem da mata atl
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
predictors_good_1
predictors_after_prean <- stack(predictors_good_1$remnant,#Ecological aspects
                                #predictors_good_1$altitude,#Fatorial
                                predictors_good_1$bio01, #From PCA1
                                predictors_good_1$bio04, #Fatorial 
                                predictors_good_1$bio11, #From PCA2
                                predictors_good_1$bio12) #Ecological aspects
plot(predictors_after_prean, axes=T)
### Caso queira recomeçarr a aanálise sem PCA e gráfico de correlaÃ§Ã£o
getwd()
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
#dir.create('rasters_2020')
setwd("./rasters_2020/")

writeRaster(predictors_after_prean$remnant, filename="remnants_2020_art_1", format='GTiff',overwrite=T)
remnants <- as(predictors_after_prean$remnant, 'SpatialPolygons')
crs(remnants) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shapefile(remnants, 'remnants_qgis.shp', overwrite=T)

# save rasters files
writeRaster(predictors_after_prean$bio01, filename="bio01_2020.tif", format='GTiff', overwrite=T)
#writeRaster(predictors_after_prean$bio02, filename="bio02_2020.tif", format='GTiff', overwrite=T)
#writeRaster(predictors_after_prean$bio03, filename="bio03_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$bio04, filename="bio04_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$bio11, filename="bio11_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$bio12, filename="bio12_2020.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean$altitude, filename="altitude.tif", format='GTiff', overwrite=T)
writeRaster(predictors_after_prean, filename="all_variables_2020.tif", format='GTiff',overwrite=T)


## Species data
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")

dados <- read.csv("dados_atualizados.csv", header=T)
dados <- dados[,-1]

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
projection(ac) <- CRS('+proj=longlat +datum=WGS84')
Imart.spdf2 <- ac[mascara,] # will give error (identicalCRS)
projection(ac)
projection(mascara)
projection(mascara)<-projection(ac)
Imart.spdf2 <- ac[mascara,]
x11()
plot(mascara)
points(dados, col='black', pch=20) #original dataset
points(Imart.spdf2,col="green", pch=20) # p796 points inside study mask
length(Imart.spdf2) # 796
dev.off()


# Plot study area
estados <- readOGR("./estados.shp") 
plot(estados[estados$Regiao=="SUL",], add=T)
projection(estados) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

m <- getMap()
brasil <- m[which(m$NAME == "Brazil"), ]
paraguai <- m[which(m$NAME == "Paraguay"), ]
arg <- m[which(m$NAME == "Argentina"), ]
uru <- m[which(m$NAME == "Uruguay"), ]
bol <- m[which(m$NAME == "Bolivia"), ]
per <- m[which(m$NAME == "Peru"), ]
chile <- m[which(m$NAME == "Chile"), ]

setwd("./uc_brasil/")
ucs <- readOGR("./unidade_protecao_integralPolygon.shp")
#Define proper CRS
projection(ucs) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# imoport PAs in Brazil
ucs_suste <- readOGR("./unidade_uso_sustentavelPolygon.shp") 
#Define proper CRS
projection(ucs_suste) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# combine both spdf
ucs_full <- rbind(ucs,ucs_suste)
# crop it for study area
ucs_full <- crop(ucs_full,mascara)
ucs_suste1 <- crop(ucs_suste,mascara)
### Map pf Study Area
# Legend specifications

# Plot
pdf(paste0("./Araucaria/outputs/study_area.pdf"),width=15,height=12)
#par(mar=c(2,3,2,2))
plot(mata_atlantica, axes=T, col="gray30",
     main="Study Area", xlab="Longitude", ylab="Latitude")
plot(mascara, axes=T, col="gray7", add=T)
legend(-45, -24, legend=c("Atlantic Rainforest Domain",
                          "Mixed Ombrophilous Forest - Study Area",
                          "Tropic of Capricorn - Dashed Line"),
       col=c("black","black","black"),
       fill=c("gray40","green","black"),box.lty=0)
abline(h=-23.5, lty=2, col="black")
plot(brasil, add=T)
plot(paraguai, add=T)
plot(arg, add=T)
plot(uru, add=T)
plot(bol, add=T)
plot(per, add=T)
plot(chile, add=T)
plot(estados[estados$Regiao=="SUL",], add=T,col="white")
#plot(wrld_simpl, add=TRUE)
# NOT RUN {
scalebar(750, xy = c(-45,-30), type = 'bar', divs = 2, below = c('km'), 
         lonlat = T, label=c(0,750,1500), adj=c(2, -2), lwd = 2)
dev.off()


# Plot
pdf(paste0("./Araucaria/outputs/study_area_zoom.pdf"),width=15,height=12)
#par(mar=c(2,3,2,2))
plot(mascara, axes=T, col="green",
     main="Study Area", xlab="Longitude", ylab="Latitude")
plot(ucs_full, add=T, col="yellow")
plot(ucs_suste1$classifica,add=T, col="red")
legend(-45, -24, legend=c("Atlantic Rainforest Domain",
                          "Mixed Ombrophilous Forest - Study Area",
                          "Tropic of Capricorn - Dashed Line"),
       col=c("black","black","black"),
       fill=c("gray40","green","black"),box.lty=0)
points(p,col="black", pch=20) # p293 points inside study mask

dev.off()


#########################

pdf(paste0("./Araucaria/outputs/study_area.pdf"),width=10,height=12)
par(mar=c(2,3,2,2))
plot(mata_atlantica,col= "gray60", main= "Study Area", axes=T)
plot(mascara, axes=T, col="green", add=T)
legend(-45, -24, legend=c("Atlantic Rainforest Domain",
                          "Mixed Ombrophilous Forest"),
       col=c("black","black"),
       fill=c("gray40","green"),box.lty=0)
points(p,col="black", pch=20) # p295 points final
plot(wrld_simpl, add=TRUE)
# Add scalebar
dev.off()

#######################################
new_data <- as.data.frame(Imart.spdf2)
list1 <- 1:796
list2 <- rep("Araucaria_angustifolia", length(list1))
list3 <- cbind(list2,new_data)
mask_data <- list3
mask_data$presence <- rep(1,nrow(mask_data)) 
names(mask_data) <- c("Species", "lon", "lat", "Presence")
head(mask_data)


################################  ########################

#dir.create('ocorrencia_na_mascara_2020')
# mudar diretorio
getwd()
setwd('./ocorrencia_na_mascara_2020')
# write .csv with correct dataset
write.csv(mask_data, file = "pts_mask_2020.csv")

##################################################
### Starting the modeling Session - here we go, grab a beer :)

setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/ocorrencia_na_mascara_2020/")
testando <- read.csv("pts_mask_2020.csv", header=T)
testando <- testando[,-1]

library(biomod2)
DataSpecies <- testando
names(DataSpecies) <- c("Species", "lon", "lat", "Araucaria")
head(DataSpecies)

#loading environmental layers
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1/")
#dir.create('rasters_2020')
setwd("./rasters_2020/")
predictors_after_prean <- stack('all_variables_2020.tif')
names(predictors_after_prean) <- c("remnant","altitude","bio01","bio04","bio11","bio12")
plot(predictors_after_prean)

myExpl <- dropLayer(predictors_after_prean, c(2))


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
                            proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

p <- SpatialPoints(d) ## This is used for presence-only data


projection(p) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

myRespCoord <- d@coords
str(DataSpecies)

## Number of 1km pixels with at least one presence
npix <- nrow(d)
plot(myExpl$remnant)
plot(d,pch=19,add=TRUE,cex=1)


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
        #bio02 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"2",".tif"))
        #bio03 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"3",".tif"))
        bio04 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"4",".tif"))
        bio11 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"11",".tif"))
        bio12 <- raster(paste0(gcms[gc],rcps[rc],"bi",years[ye],"12",".tif"))
        
        variable <- stack(bio01,bio04,bio11,bio12)
        
        crs(variable)  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        future_1 <- resample(variable,mascara_2,"bilinear")
        future_3 <- mask(future_1,mascara)
        names(future_3) <- c("bio01","bio04","bio11","bio12")
        
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
set.seed(2589) ## Reproducible pseudo-absences
myBiomodData <- BIOMOD_FormatingData(resp.var = p,
                                     expl.var = myExpl,
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

set.seed(2589) ## Reproducible results
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
                                       #em.by="all", #Statistical models : 'algo'
                                       em.by="PA_dataset+repet", #see why here https://rstudio-pubs-static.s3.amazonaws.com/38564_747d4bbf87704f0394734977bd4905c4.html
                                       eval.metric= c("ROC"),
                                       eval.metric.quality.threshold= c(0.7),#Setting threshold for cutoff between models, at least 2 models higher
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
yr <- c("2080") # For 2080

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
        names(future) <- c("bio01","bio04","bio11","bio12")
        #future <- stack(predictors_good_1$altitude,future)
        future <- stack(predictors_after_prean$remnant,future)
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
        
        ## BIOMOD_EnsembleForecasting == FUTURE == ## Ensemble forecasting
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
a.arg <- list(at=seq(0,2000,length.out=5),labels=seq(0,2000,length.out=5),cex.axis=1.5)
l.arg <- list(text="Elevation (m)",side=2, line=0.5, cex=2.5)
# Plot
pdf(paste0("./Araucaria/outputs/presence_alt.pdf"),width=6.5,height=10)
par(mar=c(0,0,0,1),cex=1.4)
plot(predictors_after_prean$altitude,col=terrain.colors(255)[255:1],
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
colors <- c(grey(c(0.90,seq(0.7,0.3,-0.2))),"#568203","#013220")

###############################

# Plot
pdf(paste0("./Araucaria/outputs/ca_current.pdf"),width=6.5,height=10)
par(mar=c(0,0,0,1),cex=1.4)
plot(ca,col=viridis_pal(option = "B")(7),ext=mascara_2, breaks=breakpoints,
#plot(ca,col=colors,ext=mascara_2, breaks=breakpoints,
      legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE, box=FALSE, zlim=c(0,1000))
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
raster <- ca

oi <- as.data.frame(raster$Araucaria_EMcaByROC_mergedAlgo_Full_PA1)
colnames(oi)<-"media"
projection(raster) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rst_prj <- projectRaster(raster, crs = "+init=epsg:32722")
spy_prj <- rasterToPolygons(rst_prj)

## Present species distribution area (km2)
unique(values(ca))
library(rgeos)
ta  <- gArea(spy_prj)/1e6 # total area = 217.849,2
percentual_pres <- SDA.pres*100/ta # ta = total area of MOF  24,49
km2_pres <- (ta*percentual_pres)/100

##=================
## Ecological niche

## 95% quantiles for alt, temp, prec, tseas, cwd
wC <- which(values(ca)>=600)
niche.df <- as.data.frame(myExpl)[wC,]
niche.df$alt <- predictors_good_1$altitude[wC]
#niche.df$alt <- predictors_good_1$altitude[wC]

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

# Committee averaging performance
#Index <- c("ROC","ACCURACY","TSS","KAPPA")
#Perf.ca <- data.frame(ROC=NA,OA=NA,TSS=NA,K=NA,Sen=NA,Spe=NA)
#for (ind in 1:length(Index)) {
 # v <- Find.Optim.Stat(Stat=Index[ind],Fit=caData,Obs=ObsData,Fixed.thresh=800) 
  # https://rdrr.io/cran/biomod2/man/Find.Optim.Stat.html! 
  ## here thresh=600: three models at least
  #Perf.ca[,ind] <- v[1]
#}
#Perf.ca$Sen <- v[3]
#Perf.ca$Spe <- v[4]
#write.table(Perf.ca,paste0("./Araucaria/outputs/performance_ca.txt"),sep="\t")

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
    pdf(paste0("./Araucaria/outputs/cafd_",rcp[j],"_",yr[l],".pdf"),width=6.5,height=10)
    par(mar=c(0,0,0,1),cex=1.4)
    plot(caFut,col= viridis_pal(option = "B")(16),
         breaks=breakpoints,ext=mascara_2,
         legend.width=1.5,legend.shrink=0.75,legend.mar=7,
         axis.args=a.arg,legend.arg=l.arg,
         axes=FALSE,box=FALSE,zlim=c(0,3000))
    plot(estados[estados$Regiao=="SUL",], add=T,col="transparent")
    dev.off()
    writeRaster(caFut, filename = paste0("./Araucaria/outputs/",rcp[j],"_",yr[l],"_caFD"), format = "GTiff", overwrite = TRUE)
    
    ## Convert the map to binary in order to compute connectivity
    binary_map_fut_caFut <- BinaryTransformation(caFut, 1599) # at least 6 models, 1500 counts 7 at least, that's why 1499
    writeRaster(binary_map_fut_caFut, filename = paste0("./Araucaria/outputs/",
            rcp[j],"_binary_",yr[l],"_Full"), format = "GTiff", overwrite = TRUE)

    # Zero-dispersal
    caZD <- caFut
    values(caZD)[values(ca)<600] <- 0 
    pdf(paste0("./Araucaria/outputs/cazd_",rcp[j],"_",yr[l],".pdf"),width=6.5,height=10)
    par(mar=c(0,0,0,1),cex=1.4)
    plot(caZD,col=viridis_pal(option = "B")(16),
         breaks=breakpoints,ext=mascara_2,
         legend.width=1.5,legend.shrink=0.75,legend.mar=7,
         axis.args=a.arg,legend.arg=l.arg,
         axes=FALSE,box=FALSE,zlim=c(0,3000))
    plot(estados[estados$Regiao=="SUL",], add=T,col="transparent")
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
      Alt.fut$mean.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- mean(values(predictors_good_1$altitude)[values(caFut)>=1600],na.rm=TRUE)
      Alt.fut$q1.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- quantile(values(predictors_good_1$altitude)[values(caFut)>=1600],0.025,na.rm=TRUE)
      Alt.fut$q2.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="full"] <- quantile(values(predictors_good_1$altitude)[values(caFut)>=1600],0.975,na.rm=TRUE)
        }
    # zd
    if (SDA.fut$area.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] != 0) {
      Alt.fut$mean.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] <- mean(values(predictors_good_1$altitude)[values(caZD)>=1600],na.rm=TRUE)
      Alt.fut$q1.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] <- quantile(values(predictors_good_1$altitude)[values(caZD)>=1600],0.025,na.rm=TRUE)
      Alt.fut$q2.fut[SDA.fut$rcp==rcp[j] & SDA.fut$yr==yr[l] & SDA.fut$disp=="zero"] <- quantile(values(predictors_good_1$altitude)[values(caZD)>=1600],0.975,na.rm=TRUE)
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

# combine both spdf
ucs_full <- rbind(ucs,ucs_suste)
# crop it for study area
ucs_full <- crop(ucs_full,mascara)
ucs_suste1 <- crop(ucs_suste,mascara)

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
samp <- as.data.frame(cbind(samplocs, samp, sampdata))
names(samp) <- c('lon', 'lat', 'index', 'value')
samp_ca <- as.data.frame(cbind(samplocs_ca, samp_ca, sampdata_ca))
names(samp_ca) <- c('lon', 'lat', 'index', 'value')

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
samp <- samp[,c(2,1,3,4)]

samp_ca <- samp_ca[,-3] # twice
samp_ca <- samp_ca[,-3] # twice
samp_ca <- samp_ca[,c(2,1,3,4)]

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
                         method_protected_area = "no_more_than_one", DrawMap = T,write_shp = T)

write.csv(mapa_pa_samp,paste0("./Araucaria/outputs/",
                                      rcp[j],"_",yrs[m],"_",disp[k],"_inPA.csv"))

### generata map for current occurrence inside PAs current

conRtable_current <- read.csv(paste0('./Araucaria/outputs/ca_samp_conR.csv'),
                              header=T)
conRtable_current <- conRtable_current[,-1]

MyData_samp_current <- conRtable_current
mapa_pa_samp_current <- IUCN.eval(MyData_samp_current, Cell_size_locations = 1, 
                                  protec.areas = ucs_full, ID_shape_PA = "nome", 
                                  method_protected_area = "no_more_than_one", DrawMap = T,write_shp = T)

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

cs <- c(0.1, 0.1)  # cell size 
cc_future <- bb_future[, 1] + (cs/2)  # cell offset
cc_current <- bb_current[, 1] + (cs/2)  # cell offset

dd_future <- ceiling(diff(t(bb_future))/cs)  # number of cells per direction - transforma argumento em vetor
dd_current <- ceiling(diff(t(bb_current))/cs)  # number of cells per direction - transforma argumento em vetor

grd_future<- GridTopology(cellcentre.offset=cc_future, cellsize=cs, cells.dim=dd_future)
grd_current <- GridTopology(cellcentre.offset=cc_current, cellsize=cs, cells.dim=dd_current)

## Conversion from topology to poligon (shape)
sp_grd_future <- SpatialGridDataFrame(grd_future, data=data.frame(id=1:prod(dd_future)))
sp_grd_current <- SpatialGridDataFrame(grd_current, data=data.frame(id=1:prod(dd_current)))                        

sp_grd_future <- as(sp_grd_future, "SpatialPixels")
sp_grd_current <- as(sp_grd_current, "SpatialPixels")                                       

sp_grd_future <- as(sp_grd_future, "SpatialPolygons")
sp_grd_current <- as(sp_grd_current, "SpatialPolygons")

# set CRS

proj4string(sp_grd_future) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " 
proj4string(sp_grd_current) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

names(connect)<- "binary"
names(connect_cur)<- "binary"

shp_future <- rasterToPolygons(connect, fun=function(x){x>0})
shp_current<- rasterToPolygons(connect_cur, fun=function(x){x>0})

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
g_future <- spatgraph(x_future, "geometric", par=0.2) 
g_current <- spatgraph(x_current, "geometric", par=0.2) 

#g_futurw <- spatgraph(x_future, "geometric", par=0.15)

# Legend
breakpoints <- seq(-1,1,by=1)
#colors <- c(grey(c(0.90,seq(0.7,0.50,-0.05))),gcolors(3))
a.arg <- list(at=seq(-1,1,length.out=3), labels=c(-1,0,1), cex.axis=1.2)
l.arg <- list(text="Presence/Absence",side=2, line=0.5, cex=1.5)

# Plot (Future connectiviy)
pdf(paste0("./Araucaria/outputs/connectivity_",rcp[j],"_",yrs[m],"_",
           dispersal[q],".pdf"),width=6.5,height=10)
par(mar=c(0,0,0,1),cex=1.4)
plot(connect,col= viridis_pal(option = "B")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
points(df_coords_future$lonDD, df_coords_future$latDD, col="white", pch=20)
plot(g_future,x_future, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="white")
#plot(ucs_full, add=T, col= "transparent", border="blue")#not too bad
dev.off()

# Plot (Future connectiviy)
pdf(paste0("./Araucaria/outputs/cur_connectivity1.pdf"),width=6.5,height=10)
par(mar=c(0,0,0,1),cex=1.4)
plot(connect_cur,col= viridis_pal(option = "B")(3),breaks=breakpoints,ext=mascara_2,
     legend.width=1.5,legend.shrink=0.6,legend.mar=7,
     axis.args=a.arg,legend.arg=l.arg,
     axes=FALSE,box=FALSE,zlim=c(-1,1))
points(df_coords_current$lonDD, df_coords_current$latDD, col="white", pch=20)
plot(g_current,x_current, xlim=c(-54.45324,-48.16156), ylim=c(-29.73621,-23.56954), 
     add=T, col="white")

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



#getwd()
library(session)
memory.limit(size = 10000000000000)
##getwd()
setwd("D:/OneDrive/Cap_1_outros_papers/script_art_1")
#save.session("araucaria_2020.Rda")
#restore.session("araucaria_2020.Rda")
