# Where does our food come from - Germplasm-banks data macro regions - Circos
# H. Achicanoy & C. Khoury
# CIAT, 2015

options(warn=-1)
library(readxl)
library(dplyr)

country_regions <- read_excel('C:/Users/haachicanoy/Desktop/Countries_gb_data_regions_2015_10_15.xlsx', sheet=1); country_regions <- as.data.frame(country_regions)
data_country <- read_excel("C:/Users/haachicanoy/Desktop/JInnes_OriginCountry_ToCountry_circos_2015_10_15_final.xlsx", sheet=1); data_country <- as.data.frame(data_country)
country_regions <- country_regions[,c('Country','Region_nice')]

data_country$Region_origin <- NA
data_country$Region_recipient <- NA

# Asignar a cada país su respectiva región
for(i in 1:nrow(data_country))
{
  cat('Processing origin country\n')
  # Origin country
  mtch <- unique(match(data_country$Origin[i], country_regions$Country))
  data_country$Region_origin[i] <- country_regions$Region_nice[mtch]
  
  cat('Processing recipient country\n')
  # Recipient country
  mtch2 <- unique(match(data_country$Recipient[i], country_regions$Country))
  data_country$Region_recipient[i] <- country_regions$Region_nice[mtch2]
}; rm(i, mtch, mtch2)

# Archivo con Paises, Regiones y movimientos entre bancos de germoplasma
# Guardar
write.csv(data_country, "C:/Users/haachicanoy/Desktop/Prod_items_country_regions.csv", row.names=FALSE)
rm(country_regions, data_country)

g=gc(); rm(list=ls())

# Leerlo
data_exp <- read.csv("C:/Users/haachicanoy/Desktop/Prod_items_country_regions.csv")
transferences <- data_exp[,c("Region_origin", "Region_recipient")]
transferences <- unique(transferences)
transferences$Region_origin <- as.character(transferences$Region_origin)
transferences$Region_recipient <- as.character(transferences$Region_recipient)
rownames(transferences) <- 1:nrow(transferences)

# Calcular transferencia entre regiones a partir de la suma entre los paises
data_country <- list()
for(i in 1:nrow(transferences))
{
  subdata_exp <- subset(data_exp, subset=data_exp$Region_origin==transferences$Region_origin[i]&data_exp$Region_recipient==transferences$Region_recipient[i])
  data_country[[i]] <- data.frame(Region_origin=transferences$Region_origin[i], Region_recipient=transferences$Region_recipient[i],Average=sum(subdata_exp[,3]))
}; rm(i, subdata_exp, data_exp)

data_country <- Reduce(function(...){rbind(..., deparse.level = 1)}, data_country)
write.csv(data_country, "C:/Users/haachicanoy/Desktop/regions_sourceofprod.csv",row.names=FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Circos plot
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

g <- gc(); rm(list=ls())

options(warn=-1)
library(circlize)
library(readxl)
library(dplyr)
library(plyr)

cOrgData_region <- read.csv("C:/Users/haachicanoy/Desktop/regions_sourceofprod.csv")
cOrgData_region <- cOrgData_region[,c('Region_recipient','Region_origin','Average')]
names(cOrgData_region) <- c('R_recipients','R_origin','Average')
rownames(cOrgData_region) <- 1:nrow(cOrgData_region)
cOrgData_region$R_recipients <- as.character(cOrgData_region$R_recipients)
cOrgData_region$R_origin <- as.character(cOrgData_region$R_origin)

#regions <- as.character(sort(unique(cOrgData_region$R_origin)))
regions <- c('Andes','Australia New Zealand','Caribbean','Central Africa','Central America',
             'Central Asia','East Africa','East Asia','IOI','NE Europe','North America',
             'NW Europe','SE Europe','SE Mediterranean','South Asia','Southeast Asia',
             'Southern Africa','SW Europe','Temp. S. America','Trop. Pacific Region',
             'Trop. S. America','West Africa','West Asia')

# Determinar todas las posibles transferencias
all.combinations <- as.data.frame(expand.grid(regions, regions))
names(all.combinations) <- c("R_recipients", "R_origin")
all.combinations$R_recipients <- as.character(all.combinations$R_recipients)
all.combinations$R_origin <- as.character(all.combinations$R_origin)

all.combinations <- setdiff(all.combinations[,1:2], cOrgData_region[,1:2])
all.combinations$Average <- 0

cOrgData_region <- rbind(cOrgData_region, all.combinations); rm(all.combinations)

cOrgData_region$R_recipients <- gsub(pattern=regions[2], replacement='ANZ', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[3], replacement='Car', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[4], replacement='C\nAfr', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[5], replacement='C\nAmer', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[6], replacement='C\nAsia', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[7], replacement='E\nAfr', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[8], replacement='E\nAsia', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[10], replacement='NE\nEur', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[11], replacement='N\nAmer', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[12], replacement='NW\nEur', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[13], replacement='SE\nEur', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[14], replacement='SE\nMed', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[15], replacement='S\nAsia', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[16], replacement='SE\nAsia', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[17], replacement='S\nAfr', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[18], replacement='SW\nEur', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[19], replacement='Tp. S.\nAmer', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[20], replacement='Pac', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[21], replacement='Tr. S.\nAmer', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[22], replacement='W\nAfr', cOrgData_region$R_recipients, fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern=regions[23], replacement='W\nAsia', cOrgData_region$R_recipients, fixed=TRUE)

cOrgData_region$R_origin <- gsub(pattern=regions[2], replacement='ANZ', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[3], replacement='Car', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[4], replacement='C\nAfr', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[5], replacement='C\nAmer', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[6], replacement='C\nAsia', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[7], replacement='E\nAfr', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[8], replacement='E\nAsia', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[10], replacement='NE\nEur', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[11], replacement='N\nAmer', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[12], replacement='NW\nEur', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[13], replacement='SE\nEur', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[14], replacement='SE\nMed', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[15], replacement='S\nAsia', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[16], replacement='SE\nAsia', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[17], replacement='S\nAfr', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[18], replacement='SW\nEur', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[19], replacement='Tp. S.\nAmer', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[20], replacement='Pac', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[21], replacement='Tr. S.\nAmer', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[22], replacement='W\nAfr', cOrgData_region$R_origin, fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern=regions[23], replacement='W\nAsia', cOrgData_region$R_origin, fixed=TRUE)
rm(regions)

measData <- cOrgData_region; rm(cOrgData_region)
origin <- sort(as.character(unique(measData$R_origin)))
destiny <- sort(as.character(unique(measData$R_recipients)))

library(reshape2)
origin_regions <- xtabs(Average~R_origin+R_recipients, measData)
# origin_regions <- lapply(1:length(origin),function(j)
# {
#   origin_data <- measData[which(measData$R_origin==origin[j]),c("R_recipients","Average")]
#   origin_data <- origin_data[order(origin_data$R_recipients),]
#   
#   origin_region <- data.frame(t(origin_data$Average))
#   names(origin_region) <- origin_data$R_recipients
#   rownames(origin_region) <- paste(origin[j])
#   return(origin_region)
# })
# origin_regions <- Reduce(function(...) rbind(..., deparse.level=1), origin_regions)
order.regions <- c(4,23,8,2,18,3,9,20,11,1,14,12,22,10,19,21,15,16,13,6,5,7,17)
aux <- data.frame(origin = rownames(origin_regions), order = order.regions)
aux <- aux[order(aux$order),]
order.regions <- as.character(aux$origin)

origin_regions <- origin_regions[order.regions, order.regions]; rm(aux)

write.csv(origin_regions,paste("C:/Users/haachicanoy/Desktop/interchange_JIC_10_years.csv",sep=""),row.names=TRUE)

options(warn=-1)
library(dplyr)
library(readxl)
library(reshape2)

origin_regions <- read.csv("C:/Users/haachicanoy/Desktop/interchange_JIC_10_years.csv", row.names=1)
levels.regions <- rownames(origin_regions)

origin_regions <- as.matrix(origin_regions)
colnames(origin_regions) <- rownames(origin_regions)
dimnames(origin_regions) <- list(orig=levels.regions,dest=levels.regions)

##
##define ranges of circos sectors and their colors (both of the sectors and the links)
##
df1 <- data.frame(order=1:nrow(origin_regions), region=rownames(origin_regions))
df1 <- df1[order(df1$order),]
rownames(df1) <- 1:nrow(df1)
df1$xmin <- 0
df1$xmax <- rowSums(origin_regions)+colSums(origin_regions)
df1$region <- factor(df1$region, levels=df1$region)
n <- nrow(df1)
# Include RGB color
colors <- read.csv("C:/Users/haachicanoy/Desktop/id_colors_regions.csv")
colors <- colors[,c("Region","r","g","b")]
names(colors)[1] <- "region"
colors$region <- as.character(colors$region)

colors$region <- gsub(pattern='N\\nAmer', replacement='N\nAmer', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='C\\nAmer', replacement='C\nAmer', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='Trop. S.\\nAmer', replacement='Tr. S.\nAmer', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='Temp. S.\\nAmer', replacement='Tp. S.\nAmer', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='W\\nAfr', replacement='W\nAfr', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='C\\nAfr', replacement='C\nAfr', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='E\\nAfr', replacement='E\nAfr', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='S\\nAfr', replacement='S\nAfr', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='W\\nAsia', replacement='W\nAsia', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='C\\nAsia', replacement='C\nAsia', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='S\\nAsia', replacement='S\nAsia', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='E\\nAsia', replacement='E\nAsia', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='SE\\nAsia', replacement='SE\nAsia', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='NW\\nEur', replacement='NW\nEur', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='SW\\nEur', replacement='SW\nEur', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='NE\\nEur', replacement='NE\nEur', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='SE\\nEur', replacement='SE\nEur', x=colors$region, fixed=TRUE)
colors$region <- gsub(pattern='SE\\nMed', replacement='SE\nMed', x=colors$region, fixed=TRUE)

df1 <- merge(df1,colors,by="region")
df1 <- df1[order(df1$order),]
rownames(df1) <- 1:nrow(df1)
df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)

df1 <- df1[df1$xmax!=0,]
#df1$order <- 1:nrow(df1)
rownames(df1) <- 1:nrow(df1)
df1$region <- as.character(df1$region)
df1$region <- factor(df1$region, levels=df1$region)
n <- nrow(df1)

regions_to_delete <- c('Car','W\nAfr','C\nAfr','IOI','Pac')
origin_regions <- origin_regions[-match(regions_to_delete,rownames(origin_regions)),-match(regions_to_delete,colnames(origin_regions))]

##
##plot sectors
##
library("circlize")
par(mar=rep(0.5,4),xpd=TRUE)
circos.clear()

#basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.1), start.degree = 90, gap.degree =4)

#sector details
circos.initialize(factors=df1$region, xlim=cbind(df1$xmin, df1$xmax))

#plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors=df1$region, track.height=0.1, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         z = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #                            #text direction (dd) and adjusmtents (aa)
                         #                            theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         #                            dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                         #                            aa = c(1, 0.5)
                         #                            if(theta < 90 || theta > 270)  aa =c(0, 0.5)
                         
                         #plot country labels
                         #v.dist <- c(1.5, 2.0, 1.5, 2.0, 1.5, 2.0, 1.5, 2.0, 1.5, 2.0)
                         circos.text(x=mean(xlim), y=1.5, labels=name, facing='inside', cex=0.8, col=df1$rcol[z], font=2, niceFacing=TRUE) # facing = dd, adj = aa, bending.inside
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = df1$rcol[z], border=df1$rcol[z])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(origin_regions)[z], ytop=ylim[1]+0.3, 
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         #                            if(umeas[[i]]=="Area harvested")
                         #                            {
                         #                              circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=200), minor.ticks=1,
                         #                                          labels.away.percentage = 0.15)
                         #                            } else {
                         #                              if(umeas[[i]]=="Gross Production Value (current million US$)")
                         #                              {
                         #                                circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=500), minor.ticks=1,
                         #                                            labels.away.percentage = 0.15)
                         #                              } else {
                         #                                circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=1000), minor.ticks=1,
                         #                                            labels.away.percentage = 0.15)
                         #                                }
                         #                              }
                       })

circos.par(track.margin=c(0,0))

##
##plot links
##
#add sum values to df1, marking the x-position of the first links out (sum1) and in (sum2). Updated for further links in loop below.
df1$sum1 <- colSums(origin_regions)
df1$sum2 <- numeric(n)

#create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
df2<-cbind(as.data.frame(origin_regions),orig=rownames(origin_regions),stringsAsFactors=FALSE)
df2<-reshape(df2,idvar="orig",varying=list(1:n),direction="long",timevar="dest",time=rownames(origin_regions),v.names="origin_regions")
df2<-arrange(df2,desc(origin_regions))

#keep only the largest flows to avoid clutter
#df2<-subset(df2, origin_regions>quantile(origin_regions,0.95))
df2$orig <- gsub(pattern='_',replacement=' ',df2$orig)
df2$dest <- gsub(pattern='_',replacement=' ',df2$dest)

for(k in 1:nrow(df2)){
  #m,n reference of flow matrix
  m<-match(df2$orig[k],df1$region)
  n<-match(df2$dest[k],df1$region)
  
  #plot link
  circos.link(sector.index1=df1$region[m], point1=c(df1$sum1[m], df1$sum1[m] + abs(origin_regions[m, n])),
              sector.index2=df1$region[n], point2=c(df1$sum2[n], df1$sum2[n] + abs(origin_regions[m, n])),
              col = df1$lcol[m])
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[m] = df1$sum1[m] + abs(origin_regions[m, n])
  df1$sum2[n] = df1$sum2[n] + abs(origin_regions[m, n])
}

dev.copy2pdf(file=paste("C:/Users/haachicanoy/Desktop/interchange_JIC_all.pdf",sep=""), height=10, width=10)
