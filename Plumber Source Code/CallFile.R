library(RCurl)

getSum<-function(a,b){
  options(warn = -1)
  
  url<- paste("http://127.0.0.1:8000/sum?b=",b,"&a=",a,sep="")
    
  res<-postForm(url,.opts =list(httpheader = 
        c('Content-Type' = 'application/json', Accept = 'application/json')))

  temp<-gsub('^.|.$', '', res)
  options(warn = 0)
  return(as.integer(temp[1]))
}

message<-function(x){
  options(warn = -1)

  url<- paste("http://127.0.0.1:8000/echo?msg=",x,sep="")
  
  res<-getForm(url,.opts =list(httpheader = 
                                 c('Content-Type' = 'application/json', Accept = 'application/json')))

  
  options(warn = 0)
  return(res[[1]])
  
}






getSum(24,10)


message("hello_world!")

