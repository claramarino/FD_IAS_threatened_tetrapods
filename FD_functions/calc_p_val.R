calc_pval_trait <- function(x,stat,k=250) { 
  require(evd) 
  require(DescTools) 
  n<-length(x) 
  if (n<k) stop("Error: Too few random values", "\n")
  
  b = sum(stat<x)+sum(stat==x)
  p = (b+1)/(n+1)
  
  if (b > (n-5)) { 
    m<-max(c(x,stat)) 
    x<-m-x 
    stat<-m-stat
    x<-sort(x,decreasing=T) 
    k<-250 
    repeat 
    { 
      tresh<-(x[k]+x[k+1])/2 
      M<-fpot(x, tresh, std.err=F) 
      AD.test<-AndersonDarlingTest(x[1:k], null ="pgpd",
                                   loc=tresh,scale=M$estimate[1],shape=M$estimate[2]) 
      if (AD.test$p.value>0.05) break 
      k<-k-10 
      if (k==0) break 
    } 
    if (k==0) stop("Error: Generalized Pareto distribution cannot be fitted", "\n") 
    p<-pgpd(x[k], tresh, M$estimate[1],M$estimate[2], lower.tail = F)
  } 
  
  if (b < 6) { 
    x<-sort(x,decreasing=T) 
    k<-250 
    repeat 
    { 
      tresh<-(x[k]+x[k+1])/2 
      M<-fpot(x, tresh, std.err=F) 
      AD.test<-AndersonDarlingTest(x[1:k], null ="pgpd",
                                   loc=tresh,scale=M$estimate[1],shape=M$estimate[2]) 
      if (AD.test$p.value>0.05) break 
      k<-k-10 
      if (k==0) break 
    } 
    if (k==0) stop("Error: Generalized Pareto distribution cannot be 
fitted", "\n") 
    p<-pgpd(x[k], tresh, M$estimate[1],M$estimate[2]) 
  } 
  return(p) 
}
