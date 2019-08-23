functions{
  real loglikelihood(int N,
                     real mu,
                     real k,
                     real p,
                     real c, 
                     real q, 
                     real d, 
                     real alpha, 
                     real gamma, 
                     vector t, 
                     vector magnitudes, 
                     vector dif_tiempos,
                     vector latitudes, 
                     vector longitudes, 
                     vector factor_cuadraticoAni,
                     vector factor_cuadraticoIso,
                     real tmax,
                     real magnitud0,
                     real lat_min,
                     real lat_max,
                     real long_min,
                     real long_max,
                     real radio
                     ){
    
    real tasa_sismicidad[N];
    real integral_tasa[N];
    
    real integral_mu;
    real log_verosimilitud;
    tasa_sismicidad[N]<-log(mu);
    integral_tasa[N]<-0;
    for(j in 1:(N-1)){
      vector[N-j] y;
      vector[N-j] z;
      vector[N-1] x;
      real temp;
      real temp1;
      real temp2;
      int inicio;
      int fin;
      inicio <-N*(j-1)-(j*(j-1))/2 + 1;
      fin <-j*N-(j*(j+1))/2;
      
      
      y<-dif_tiempos[inicio:fin];
      z<-exp(-q*log(factor_cuadraticoIso[inicio:fin]./(exp(gamma*(magnitudes[(j+1):]-magnitud0)))+d));
      y<-(k*alpha*(p-1)*c^(p-1)*(q-1)*d^(q-1)*(1/pi()))*exp((alpha-gamma)*(magnitudes[(j+1):]-magnitud0)).*exp(-p*log(y+c));
      y<-y .* z;
      tasa_sismicidad[j]<-log(mu+sum(y));
  
      temp<-exp(alpha*(magnitudes[j+1]-magnitud0));
      temp1<-exp(gamma*(magnitudes[j+1]-magnitud0));
      temp2<-max(factor_cuadraticoIso[inicio:fin]);
      
      integral_tasa[j]<-k*alpha*temp*(1-c^(p-1)/((dif_tiempos[j]+c)^(p-1)))*(1-d^(q-1)/((temp2/(temp1)+d)^(q-1)));
      
      
    }
   
    
    
    integral_mu<-mu*tmax*pi()*radio^2;
    log_verosimilitud<-sum(tasa_sismicidad)-integral_mu-sum(integral_tasa);
    return(log_verosimilitud);
  }
} 
data{
  int<lower=0> N;
  vector[N] tiempo;
  real<lower=0> tiempo_max;
  vector[N] magnitudes;
  vector[N*(N-1)/2] factor_cuadraticoAni;
  vector[N*(N-1)/2] factor_cuadraticoIso;
  vector[N*(N-1)/2] dif_tiempos;
  real<lower=0> magnitud_corte;
  vector[N] latitudes;
  vector[N] longitudes;
  real lat_min;
  real lat_max;
  real long_min;
  real long_max;
  real<lower=0> radio;
}
parameters{
  real<lower=0> mu;
  real<lower=0> k;
  real<lower=1.000005> p;
  real<lower=0> c;
  real<lower=0> d;
  real<lower=1.00005> q;
  real<lower=0> alpha;
  real<lower=0> gamma;

  
}
model{
  //priors
  mu~exponential(2.8);
  k~exponential(2.8);
  p~exponential(2.8);
  c~exponential(2.8);
 d~exponential(2.8);
  q~exponential(2.8);

  gamma~exponential(2.8);
  alpha-gamma~exponential(5);
  
  increment_log_prob(loglikelihood(N,mu,k,p,c,q,d,alpha,gamma,tiempo,magnitudes,dif_tiempos,latitudes,longitudes,factor_cuadraticoAni,factor_cuadraticoIso,tiempo_max,magnitud_corte,lat_min,lat_max,long_min,long_max,radio));
}