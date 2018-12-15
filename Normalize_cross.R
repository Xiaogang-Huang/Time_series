cross_my=function(x,a){
  n=length(x)
  A=rep(0,n)
  for(i in 2:n){
    if(x[i-1]<a&x[i]>a){
      A[i]=1
    }else if(x[i-1]>a&x[i]<a){
      A[i]=-1
    }else{
      A[i]=0
    }
  }
  return(A)
}
bzh=function(x,h){
  n=length(x)
  m=length(h)
  b=rep(0,n)
  for(i in 1:m){
    b=b+cross_my(x,h[i])
  }
  c=which.min(abs(h-x[1]))
  b[1]=c-1
  return(cumsum(b))
}