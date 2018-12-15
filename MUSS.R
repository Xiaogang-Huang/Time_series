muss_kernel=function(x,y){
  n=length(x)
  z=diag(n)
  for(i in 1:n){
    for(j in i:n){
      z[i,j]=i*(n-j+1)
    }
  }
  z=z+t(z)
  diag(z)=diag(z)/2
  x%*%z%*%y
}
dist_muss=function(x,y){
  muss_kernel(x,x)+muss_kernel(y,y)-2*muss_kernel(x,y)
}
