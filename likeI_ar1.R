setwd("C:/Users/Bru/Desktop/1i ergasia dedrami")
likel_ar1=function(theta, y){
  
  T=length(y);
  lik=0;
  for  (l in  2:T)  {
    lik=lik+log(norm_pdf1( y[l],theta[1]+theta[2]*y[l-1], theta[3] ))
  }
  
  lik1=-lik;
  return(lik1)
}
