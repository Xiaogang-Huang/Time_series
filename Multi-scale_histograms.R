#multi-scale histogram
chd=function(x,y,delta,tao,m1,d){
  l=length(x)
  sub_len=l/2^(delta-1)
  h=matrix(0,nrow = 2,ncol=tao)
  chd=0
  for(k in 1:2^(delta-1)){
    for(i in (k-1)*sub_len:k*sub_len){
      for(j in 1:l){
        bin_x=ceiling((x[l]-m1)/d)
        bin_y=ceiling((y[l]-m1)/d)
        h[,bin]=c(bin_x,bin_y)
      }
    }
    h=t(apply(h,1,cumsum))
    chd=chd+sqrt(sum((h[1,]-h[2,])^2))
  }
  chd/2^(delta-1)
}
shape_matching=function(data,q,delta,eps,tao){
  n=nrow(data)
  l=ncol(data)
  m1=min(data)
  m2=max(data)
  d=(m2-m1)/tao
  h=matrix(0,nrow = n,ncol=tao)
  for(i in 1:n){
    for(j in 1:l){
      bin=ceiling((data[i,j]-m1)/d)
      h[i,bin]=h[i,bin]+1
    }
  }
  h_cum=t(apply(h,1,cumsum))
  h_average=apply(h_cum,1,function(x) sum(x)/tao)
  achd=sqrt(tao*(h_average-h_average[q])^2)
  resultlist=which(achd<eps)
  j=1
  while(j<delta&length(resultlist)>0){
    resultlist_new=NULL
    for(i in resultlist){
      if(chd(data[i,],data[q,],delta,tao,m1,d)<eps){
        resultlist_new=c(resultlist_new,i)
      }
    }
    resultlist=resultlist_new
    j=j+1
  }
  return(resultlist)
}
result_1=shape_matching(x_matrix_nor,21,3,0.1,20)
x1%>%filter(n%in%n_x[result_1])%>%ggplot(aes(time,y,group=n))+geom_line()
