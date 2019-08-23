
mag1<-3
m0<-mag1

##########################################################
indice.sismo.mas.antiguo=which(sismos$tiempodesdeAhora==max(sismos$tiempodesdeAhora))
indice.sismo.mas.reciente=which(sismos$tiempodesdeAhora==min(sismos$tiempodesdeAhora))
fechaMasantiguo=paste(sismos[indice.sismo.mas.antiguo,]$date, " ",sismos[indice.sismo.mas.antiguo,]$time)
sismosnuevos <- read.csv("C:/Users/GOETHE/sismos4.csv")
sismosManabiEsmeraldasNuevo=sismosnuevos[sismosnuevos$ProvinciaMasCercana %in% c("Manabi","Esmeraldas"),]
sismos6=sismosManabiEsmeraldasNuevo[,c("Mes","dia","hora","minuto","LongitudValor","LatitudValor","Magnitud")]

sismos6$time<-paste(sprintf("%02d", sismos6$hora),":",sprintf("%02d", sismos6$minuto),":",
                    sprintf("%02d",sample(1:59, 1, replace=T)),".00",sep="")
sismos6$date<-paste(sprintf("%04d", 2016),"-",sprintf("%02d", sismos6$Mes),"-",
                    sprintf("%02d",sismos6$dia),sep="")
sismos6$tiempodesdeAhora<-difftime(Sys.time(),
                                   as.POSIXct(paste(sismos6$date," ",sismos6$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
                                   ,units="mins")
sismos6<-sismos6[with(sismos6, order(tiempodesdeAhora)), ]


sismos7<-sismos6[,c("date","time","LongitudValor","LatitudValor","Magnitud")]
colnames(sismos7) =c("date","time","long","lat","mag")
sismos7$date <- as.factor(sismos7$date)
sismos7$time <- as.factor(sismos7$time)

sismos7$tiempo.dias=difftime(as.POSIXct(paste(sismos7$date," ",sismos7$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
                             ,as.POSIXct(fechaMasantiguo,"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
                             ,units="days")
sismos8<-sismos[,c("date","time","LongitudValor","LatitudValor","Magnitud","tiempo.dias")]
colnames(sismos8) =c("date","time","long","lat","mag","tiempo.dias")
sismos9=rbind(sismos7,sismos8)
sismos9$tiempo.dias=sismos9$tiempo.dias
sismos9<-sismos9[with(sismos9, order(-tiempo.dias)), ]
ts=as.numeric(sismos9$tiempo.dias)
tmax=max(ts)
combos<-combn(ts,2)

diftiempodias<-combn(ts,2,FUN=function(x) abs(x[1]-x[2]))
dfdiftiemposdias=data.frame(tiempo1=combos[1,],tiempo2=combos[2,],diftiempodias,stringsAsFactors=FALSE)
#View(dfdiftiemposdias)

latitudes=as.numeric(sismos9$lat)
combos<-combn(latitudes,2)
diflatitud<-combn(latitudes,2,FUN=function(x) abs(x[1]-x[2]))
#diflatitud<-combn(latitudes,2,FUN=function(x) (x[1]-x[2]))
dfdiflatitud=data.frame(lat1=combos[1,],lat2=combos[2,],diflatitud,stringsAsFactors=FALSE)
#View(dfdiflatitud)


longitudes=as.numeric(sismos9$long)
combos<-combn(longitudes,2)
diflongitud<-combn(longitudes,2,FUN=function(x) abs(x[1]-x[2]))
#diflongitud<-combn(longitudes,2,FUN=function(x) (x[1]-x[2]))
dfdiflongitud=data.frame(long1=combos[1,],long2=combos[2,],diflongitud,stringsAsFactors=FALSE)
#View(dfdiflongitud)

profundidades=as.numeric(sismos9$Profundidad2)
combos<-combn(as.numeric(sismos$Profundidad2),2)
difprofundidad<-combn(profundidades,2,FUN=function(x) abs(x[1]-x[2]))
dfdifprofundidad=data.frame(profund1=combos[1,],profund2=combos[2,],difprofundidad,stringsAsFactors=FALSE)


factor_cuadraticoIso=numeric(length(dfdiflatitud$diflatitud))
factor_cuadraticoAni=numeric(length(dfdiflatitud$diflatitud))

m=matrices.cov.normalizadas[[1]]
m1=m[1,1]
m2=m[2,1]
m3=m[2,2]
mc=matrices.cov[[1]]
factor_cuadraticoIso=dfdiflatitud$diflatitud^2+dfdiflongitud$diflongitud^2
factor_cuadraticoAni=m1*dfdiflongitud$diflongitud^2+2*m2*dfdiflatitud$diflatitud*dfdiflongitud$diflongitud+m3*dfdiflatitud$diflatitud^2

sismos9<-sismos9[with(sismos9, order(tiempo.dias)), ]
sismos<-sismos[with(sismos, order(tiempo.dias)), ]
############
##############################################################
# sismosManabiEsmeraldas=sismoscompleto[sismoscompleto$ProvinciaMasCercana %in% c("Manabi","Esmeraldas"),]
# sismos=sismosManabiEsmeraldas
# 
# sismos$time<-paste(sprintf("%02d", sismos$hora),":",sprintf("%02d", sismos$minuto),":","00.00",sep="")
# sismos$date<-paste(sprintf("%04d", 2016),"-",sprintf("%02d", sismos$Mes),"-",
#                    sprintf("%02d",sismos$dia),sep="")
# sismos$tiempodesdeAhora<-difftime(Sys.time(),
#                                   as.POSIXct(paste(sismos$date," ",sismos$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
#                                   ,units="mins")
# sismos<-sismos[with(sismos, order(-tiempodesdeAhora)), ]
# indice.sismo.mas.antiguo=which(sismos$tiempodesdeAhora==max(sismos$tiempodesdeAhora))
# 
# sismos$tiempo.minutos=difftime(as.POSIXct(paste(sismos$date," ",sismos$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
#                                ,as.POSIXct(paste(sismos[indice.sismo.mas.antiguo,]$date," ",sismos[indice.sismo.mas.antiguo,]$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
#                                ,units="mins")
# sismos$tiempo.dias=difftime(as.POSIXct(paste(sismos$date," ",sismos$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
#                             ,as.POSIXct(paste(sismos[indice.sismo.mas.antiguo,]$date," ",sismos[indice.sismo.mas.antiguo,]$time),"%Y-%m-%d %H:%M:%S",tz=Sys.timezone(location = TRUE))
#                             ,units="days")





cn <- 1:length(sismos9[, 1])+1

ti<-as.numeric(sismos9$tiempo.dias)
xrange <- c(min(ti), max(ti))
acumuladaTiempoMag<-function(mu,k0,c,a,p,m0, tiempoactual, tiempos,magnitudes)
{
  t0=min(tiempos)
  tiempos.precedentes=tiempos[which(tiempos<tiempoactual)]
  magnitudes.precedentes=magnitudes[which(tiempos<tiempoactual)]
  result=mu*(tiempoactual-t0)+sum((k0*a)*exp(a*(magnitudes.precedentes-m0))*(1-(c^(p-1))*((tiempoactual-tiempos.precedentes+c)^(1-p))))
  return(result)
}
acumuladaAniSismicFondoProfundidad<-function(mu,k0,c,a,p,d,q,m0,gamma,eta, tiempoactual, tiempos,magnitudes,factor_cuadraticoAni,N,j)
{
  t0=min(tiempos)
  k=N-j+1
  # print(k)
  if (k<N){
    
    inicio <-N*(k-1)-(k*(k-1))/2 + 1;
    fin <-k*N-(k*(k+1))/2;
  }
  else{
    inicio=N
    fin=N
    
  }
  #print(inicio)
  # print(fin)
  tiempos.precedentes=tiempos[which(tiempos<tiempoactual)]
  magnitudes.precedentes=magnitudes[which(tiempos<tiempoactual)]
  
  #temp2<-max(factor_cuadraticoAni[inicio:fin])
  #print(mu*sum(tasa_sismiscidad_fondo)*pi*radio^2*(tiempoactual-t0))
  # result1=sum((k0*a*(q-1)*(d^(q-1))*(1/pi))*exp(((a-gamma))*(magnitudes.precedentes-m0))*(1-(c^(p-1))*((tiempoactual-tiempos.precedentes+c)^(1-p)))
  #             *(factor_cuadraticoAni[inicio:fin]/(exp(gamma*(magnitudes.precedentes-m0)))+d)^(-q)
  #             )
  temp<-exp(a*( magnitudes.precedentes-m0));
  temp1<-exp(gamma*(magnitudes.precedentes-magnitud0));
  temp2<-max(factor_cuadraticoAni[inicio:fin]);
  result1=sum(k0*a*temp*(1-c^(p-1)/((tiempoactual-tiempos.precedentes+c)^(p-1)))*(1-d^(q-1)/((temp2/(temp1)+d)^(q-1))));
  # print("FFFF")
  #print(result1)
  result=mu*sum(tasa_sismiscidad_fondo)*pi*radio^2*(tiempoactual-t0)+result1;
  return(result)
}
acumuladaAniSismicFondo<-function(mu,k0,c,a,p,d,q,m0,gamma, tiempoactual, tiempos,magnitudes,factor_cuadraticoAni,tasa_sismiscidad_fondo,N,j)
{
  t0=min(tiempos)
  k=N-j+1
  # print(k)
  if (k<N){
    
    inicio <-N*(k-1)-(k*(k-1))/2 + 1;
    fin <-k*N-(k*(k+1))/2;
  }
  else{
    inicio=N
    fin=N
    
  }
  #print(inicio)
  # print(fin)
  tiempos.precedentes=tiempos[which(tiempos<tiempoactual)]
  magnitudes.precedentes=magnitudes[which(tiempos<tiempoactual)]
  
  #temp2<-max(factor_cuadraticoAni[inicio:fin])
  #print(mu*sum(tasa_sismiscidad_fondo)*pi*radio^2*(tiempoactual-t0))
  # result1=sum((k0*a*(q-1)*(d^(q-1))*(1/pi))*exp(((a-gamma))*(magnitudes.precedentes-m0))*(1-(c^(p-1))*((tiempoactual-tiempos.precedentes+c)^(1-p)))
  #             *(factor_cuadraticoAni[inicio:fin]/(exp(gamma*(magnitudes.precedentes-m0)))+d)^(-q)
  #             )
  temp<-exp(a*( magnitudes.precedentes-m0));
  temp1<-exp(gamma*(magnitudes.precedentes-magnitud0));
  temp2<-max(factor_cuadraticoAni[inicio:fin]);
  result1=sum(k0*a*temp*(1-c^(p-1)/((tiempoactual-tiempos.precedentes+c)^(p-1)))*(1-d^(q-1)/((temp2/(temp1)+d)^(q-1))));
  # print("FFFF")
  #print(result1)
  result=mu*sum(tasa_sismiscidad_fondo)*pi*radio^2*(tiempoactual-t0)+result1;
  return(result)
}
acumuladaAniSismicFondo2<-function(mu,k0,c,a,p,d,q,m0,gamma, tiempoactual, tiempos,magnitudes,factor_cuadraticoAni,tasa_sismiscidad_fondo,N,j)
{
  t0=min(tiempos)
  k=N-j+1
  # print(k)
  if (k<N){
    
    inicio <-N*(k-1)-(k*(k-1))/2 + 1;
    fin <-k*N-(k*(k+1))/2;
  }
  else{
    inicio=N
    fin=N
    
  }
  #print(inicio)
  #print(fin)
  tiempos.precedentes=tiempos[which(tiempos<tiempoactual)]
  magnitudes.precedentes=magnitudes[which(tiempos<tiempoactual)]
  
  #temp2<-max(factor_cuadraticoAni[inicio:fin])
  #print(mu*sum(tasa_sismiscidad_fondo)*pi*radio^2*(tiempoactual-t0))
  # result1=sum((k0*a*(q-1)*(d^(q-1))*(1/pi))*exp(((a-gamma))*(magnitudes.precedentes-m0))*(1-(c^(p-1))*((tiempoactual-tiempos.precedentes+c)^(1-p)))
  #         #    *(factor_cuadraticoAni[inicio:fin]/(exp(gamma*(magnitudes.precedentes-m0)))+d)^(-q)
  #             )
  #print("FFFF")
  # print(result1)
  result=mu*sum(tasa_sismiscidad_fondo)*pi*radio^2*(tiempoactual-t0)
  return(result)
}
cn1<-numeric(length(xrange))
tiempos=as.numeric(sismos9$tiempo.dias)
magnitudes<-as.numeric(sismos9$mag)
tiempos.array=seq(floor(xrange[1]),floor(xrange[2]))
N.sim=1000
sismos.sim=rstan::extract(object=fit,inc_warmup = FALSE)
tiempo=ts
tiempo_max=tmax
fit <- readRDS("fitAniSismicidadFondo.rds")

sismos.sim=read.csv("resultEtasEspacioTempAniProfSismicFondoCte.csv")
factor_cuadraticoAni=factor_cuadraticoAni
factor_cuadraticoIso=factor_cuadraticoIso
dif_tiempos=dfdiftiemposdias$diftiempodias
magnitud0=3
latitudes=sismos9$lat
longitudes=sismos9$long
lat_min=min(sismos9$lat)
lat_max=max(sismos9$lat)
long_min=min(sismos9$long)
long_max=max(sismos9$long)
radio=r
tasa_sismiscidad_fondo=sismos$bk
N=nrow(sismos9)
cn3=array (NA, c(N.sim, length(ti)))
cn4=array (NA, c(N.sim, length(ti)))
for (i in 1:N.sim ){
  mu<-sample(sismos.sim$mu,1,replace=FALSE)
  k0<-sample(sismos.sim$k,1,replace=FALSE)
  p<-sample(sismos.sim$p,1,replace=FALSE)
  c<-sample(sismos.sim$c,1,replace=FALSE)
  d<- sample(sismos.sim$d,1,replace=FALSE)
  q<-sample(sismos.sim$q,1,replace=FALSE)
  a<-sample(sismos.sim$alpha,1,replace=FALSE)
  gamma<- sample(sismos.sim$gamma,1,replace=FALSE)
  
  
  for(j in 1:length(ti) ){
    cn3[i,j]=acumuladaAniSismicFondo(mu=mu,k0=k0,c=c,a=a,p=p,d=d,q=q,m0=m0, gamma=gamma,as.numeric(ti[j]), tiempos,magnitudes,
                                     factor_cuadraticoAni,tasa_sismiscidad_fondo=tasa_sismiscidad_fondo,N,j)
    
    cn4[i,j]=acumuladaAniSismicFondo2(mu=mu,k0=k0,c=c,a=a,p=p,d=d,q=q,m0=m0, gamma=gamma,as.numeric(ti[j]), tiempos,magnitudes,
                                      factor_cuadraticoAni,tasa_sismiscidad_fondo=tasa_sismiscidad_fondo,N,j)
    #print(cn3[i,j])
  }
  print(i)
  
}
library(matrixStats)



medianas.acumuladas<-apply(cn4,2,median)
quant<-colQuantiles(cn4,probs=c(0.025,0.975))
medianas.acumuladas1<-apply(cn3,2,median)
quant1<-colQuantiles(cn3,probs=c(0.025,0.975))

for(i in 1:length(tiempos.array) ){
  cn1[i]=acumuladaTiempoMag(mu=mu,k0=k0,c=c,a=a,p=p,m0=m0, tiempos.array[i], tiempos,magnitudes)
  
}
cn2<-numeric(length(ti))
for(i in 1:length(ti) ){
  cn2[i]=acumuladaTiempoMag(mu=mu,k0=k0,c=c,a=a,p=p,m0=m0, as.numeric(ti[i]), tiempos,magnitudes)
  
}
mgrange <- max(cn)/4
bottom <- min(cn) - mgrange
yrange <- c(bottom, max(max(cn),max(ti)))
par(pty="s",xaxs='r')
plot(xrange, yrange, type = "n", main = "ETAS(anisótropo con sismicidad \n fondo variable) y predic.", xlab
     = "Tiempo lineal(dias)", ylab = "Número acumulado de eventos",
     lwd = 1)

#points(ti, 1:length(ti)+hypo[1,1]+1, type='s')
points(ti, 1:length(ti)+1, type='s')
#points(tiempos.array, cn1, type='s', col="red")
#points(ti, cn2, type='s', col="red")
#points(ti, cn4[1,], type='s', col="red")
points(ti, medianas.acumuladas1, type='s', col="red")
points(ti, medianas.acumuladas, type='s', col="red")
points(ti, quant1[,1], type='s', col="red",lty=2)
points(ti, quant1[,2], type='s', col="red",lty=2)
points(ti, quant[,1], type='s', col="red",lty=2)
points(ti, quant[,2], type='s', col="red",lty=2)

mag<-sismos9$mag
mgmax <- max(mag - mag1 + 1)
mag <- mag - mag1 + 0.5
segments(ti, bottom, ti, mag/mgmax * mgrange + bottom)	
abline(h=bottom)
abline(h=0)
abline(v=0,lty=2)
abline(v=max(sismos$tiempo.dias),lty=2,col="green")
abline(v=max(sismos$tiempo.dias)+30,lty=2,col="green")
#########################################
#abline(0,1,lty=1,col='red')



#########################################
par(pty="s",xaxs='r')
plot(c(0,max(1:length(ti)+1)), c(0,max(1:length(ti)+1)), type = "n", main = "Residuos ETAS(anisótropo \n con sismicidad fondo variable)", xlab
     = "Tiempo transformado", ylab = "Número acumulado de eventos",
     lwd = 1)
#points(cn2, 1:length(ti)+1, type='s', col="black")
points(medianas.acumuladas1, 1:length(ti)+1, type='s', col="black")
points(quant1[,1], 1:length(ti)+1, type='s', col="black",lty=2)
points(quant1[,2], 1:length(ti)+1, type='s', col="black",lty=2)


abline(0,1,lty=1,col='red')
#abline(50,1,col='red',lty=2)
#abline(-50,1,col='red',lty=2)

