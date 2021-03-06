library(data.table)
library(dismo)
library(raster)
library(gstat)

#args[6]="./VigieChiro/ModPred/Tadten_DM_06_GI_SysGrid__30_34_2000_Lat41.45_51.61_Long-5.9_9.73"
#args[7]="C:/Users/Yves Bas/Documents/VigieChiro/GIS/FranceD__30_34.shp"
#args[8]=2000 #PixelSize
#ModRF_file=paste0("./VigieChiro/ModPred/ModRFActLog_",args[1],"_Seuil",args[5],".learner")
#load(ModRF_file)
SpeciesList=fread("SpeciesList.csv")
Rasteriz=T

#Limite
Limite=shapefile(paste0("./VigieChiro/GIS/",args[7]))
Sys.time()

LimiteL=as(Limite,'SpatialLines')

Title=substr(args[6],22,27)

SubT=""
if(sum(grepl("rsq",names(ModRF)))>0)
{
  SubT=paste0(SubT,"PseudoR2 = ",round(ModRF$rsq[length(ModRF$rsq)],2))
}
Num=sum(ModRF$y>0)
SubT=paste0(SubT," / N = ",Num)

PredLoc=fread(paste0(args[6],".csv"))

coordinates(PredLoc) <- c("Group.1", "Group.2")
proj4string(PredLoc) <- CRS("+init=epsg:4326") # WGS 84

CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
PredL93=spTransform(PredLoc,CRSL93)
v <- voronoi(PredL93)
#plot(v)

VL=crop(v,Limite)


MaxScale=quantile(subset(PredL93$pred,PredL93$pred>0.1),0.95)
if(is.na(MaxScale)){MaxScale=0.1}
ScaleAt=c(-0.1,c(1:49)/49*MaxScale,Inf)


if(nrow(PredL93)<20000)
{
  Taxon=substr(args[6],22,27)
  test=match(Taxon,SpeciesList$Esp)
  if(is.na(test))
  {
    Title=Taxon
  }else{
    Title=SpeciesList$NomFR[test]
  }
  # print(spplot(VL, 'pred',main=Title,col="transparent"
  #           ,par.settings =
  #            list(axis.line = list(col =  'transparent'))
  #         ,col.regions=get_col_regions(),at=ScaleAt))
  
  png(paste0(args[6],".png"))
  
  
  
  p=spplot(VL, 'pred',main=Title,col="transparent"
           ,par.settings =
             list(axis.line = list(col =  'transparent'))
           ,col.regions=get_col_regions(),at=ScaleAt,sp.layout = LimiteL
           ,xlab=SubT)
  
  print(p)
  
  dev.off()
  
}

if(Rasteriz)
{
r <- raster(Limite, res=as.numeric(args[8]))
vpred <- rasterize(VL, r, 'pred')

#gs <- gstat(formula=PredL93$pred~1, locations=PredL93, nmax=10, set=list(idp = 0))
#nn <- interpolate(r, gs)
## [inverse distance weighted interpolation]
#nnmsk <- mask(nn, vpred)
#plot(nnmsk,main=substr(args[6],22,27))

#spplot(VL, 'err', col.regions=get_col_regions())


spplot(vpred,main=substr(args[6],22,27),at=ScaleAt)
writeRaster(vpred,paste0(args[6],"_pred.asc"),overwrite=T)

png(paste0(args[6],"_R.png"))
print(spplot(vpred,main=substr(args[6],22,27),at=ScaleAt,sp.layout = LimiteL
             ,xlab=SubT))
dev.off()





verr <- rasterize(VL, r, 'err')
#plot(verr)
writeRaster(verr,paste0(args[6],"_err.asc"),overwrite=T)


}