setwd("C:/Users/Bru/Desktop/1i ergasia dedrami")
autocorr_1<- function(y,l) {
  
  T=length(y)
  mm=mean(y)
  yy=y-matrix(rep(mm,T),T)
  
  
  v=matrix( yy[c((l+1):T)] , T-l)
  v_l=matrix(yy[1:(T-l)],T-l)
  
  g_l= crossprod(v,v_l)/(T-1) 
  g_0=var(y)
  
  rho_l=g_l/g_0
  
  w=list(rho_l,g_l)
  
  return(w)
  
}
