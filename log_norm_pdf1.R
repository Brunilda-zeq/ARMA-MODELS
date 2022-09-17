
setwd("C:/Users/Bru/Desktop/1i ergasia dedrami")
log_norm_pdf1=function(x,m,ss){
  #normal pdf
  f1=(-1/2)*log(ss);
  f2=-((x-m)^2)/(2*ss);
  
  f=f1+f2;
  
  return(f)
}

