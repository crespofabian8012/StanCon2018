functions{
  real loglikelihood(int N,
                     real mu,
                     real k,
                     real p,
                     real c, 
                     real alpha, 
                     vector t, 
                     vector magnitudes, 
                     vector dif_tiempos,
                     real tiempo_max,
                     real magnitud0){
    
    real tasa_sismicidad[N];
    real integral_tasa[N];
    
    real integral_mu;
    real log_verosimilitud;
    tasa_sismicidad[N]<-log(mu);
    integral_tasa[N]<-0;
   
    
    for(j in 1:(N-1)){
      vector[N-j] y;
   
      int inicio;
      int fin;
      inicio <-N*(j-1)-(j*(j-1))/2 + 1;
      fin <-j*N-(j*(j+1))/2;
      
      
      y<-dif_tiempos[inicio:fin];

      y<-(k*alpha*(p-1)*c^(p-1))*exp(alpha*(magnitudes[(j+1):]-magnitud0)).*exp(-p*log(y+c));

      tasa_sismicidad[j]<-log(mu+sum(y));
  
     
     integral_tasa[j]<-(k*alpha)*exp(alpha*(magnitudes[j+1]-magnitud0))*(1-c^(p-1)/((dif_tiempos[j]+c)^(p-1)));
      
    }
  
    
    
    integral_mu<-mu*tiempo_max;
    log_verosimilitud<-sum(tasa_sismicidad)-integral_mu-sum(integral_tasa);
    return(log_verosimilitud);
  }
} 
data{
  int<lower=0> N;
  vector[N] tiempo;
  real<lower=0> tiempo_max;
  vector[N] magnitudes;
  vector[N*(N-1)/2] dif_tiempos;
  real<lower=0> magnitud_corte;
}
parameters{
  real<lower=0> mu;
  real<lower=0> k;
  real<lower=1.000005> p;
  real<lower=0.00005> c;

  real<lower=0> alpha;
  //real<lower=0> A;


  
}
model{

 mu~exponential(2.8);
k~exponential(2.8);
p~exponential(0.3);
c~exponential(2.8);

  alpha~exponential(2.8);
  //A~exponential(2.8);
  
  increment_log_prob(loglikelihood(N,mu,k,p,c,alpha,tiempo,magnitudes,dif_tiempos,tiempo_max,magnitud_corte));
}