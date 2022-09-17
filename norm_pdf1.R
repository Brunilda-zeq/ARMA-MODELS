norm_pdf1=function(x, m,ss){
  f1= ((2*pi*ss)^(-1/2))
  f2= exp(-((x-m)^2)/(2*ss))
  f=f1*f2;
  return(f)
}  