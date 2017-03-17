initial.pop<-1000
iptg.gen<-50
non.iptg.gen<-0
#r.num<-10000
#g.num<-10000
r.prod<-1000
partition<-0.013
bimod.fun<-function(){ #bimodal distribution
  v<-c(10,1,0.5) #mean1,sd1,balance
  d<-runif(initial.pop,0,1)
  return(ifelse(d<v[3],as.integer(runif(initial.pop,7,50)),10^6))
}

#initial population setup
pop.table<-as.data.frame(matrix(,initial.pop,7))
colnames(pop.table)<-c("red","green","ratio","division time","divisions","bacteria number","time since division")
pop.table[,1]<-as.integer(exp(rnorm(initial.pop,7.85,0.37)))#rpois(initial.pop,r.num)
pop.table[,2]<-0#as.integer(exp(rnorm(initial.pop,7.85,0.37)))#rpois(initial.pop,g.num)
pop.table[,3]<-0#pop.table[,2]/pop.table[,1]
pop.table[,4]<- #setup of initial populations and persistors
  #sample(1:50,initial.pop,TRUE)  #change for different division time functions
  bimod.fun()
pop.table[,5]<-0
pop.table[,6]<-c(1:initial.pop)
pop.table[,7]<-as.integer(pop.table[,4]*runif(initial.pop,0,1))

#generation history setup
history<- data.frame(matrix(,dim(pop.table)[1],8))
colnames(history)<-c("green","red","ratio","persistor?","divisions","bacteria number","time since division","timestep number")
history[1:dim(pop.table)[1],1:7]<-pop.table
history[,8]<-1 #recording inital generation in history data frame

#loop representing a discrete timestep
for (j in 1:(iptg.gen+non.iptg.gen)) { 
  if (j<=iptg.gen){c=1
  }else{c=0}
  pop.table[,1]<-pop.table[,1] #lol
  pop.table[,2]<-pop.table[,2]+c*rpois(dim(pop.table)[1],370)
  pop.table[,7]<-pop.table[,7]+1
  pop.table<-pop.table[pop.table[,4]>75|0.02<runif(dim(pop.table)[1],0,1),]
  dividers<-pop.table[pop.table[,7]>=pop.table[,4],]  #data frame for bacteria which will divide
  
  #operations for dividing bacteria
  if (dim(dividers)[1]>0) {
    next.gen<-data.frame(matrix(,dim(dividers)[1]+dim(pop.table)[1],7))
    dividers[,8]<-rnorm(dim(dividers)[1],0.5,partition)
    dividers[,9]<-rbinom(dim(dividers)[1],dividers[,1],dividers[,8])
    dividers[,10]<-rbinom(dim(dividers)[1],dividers[,2],dividers[,8])
    #assigning red and green values for the next generation
    next.gen[1:(dim(dividers)[1]*2),1:2]<-matrix(c(dividers[,9],(dividers[,1]-dividers[,9]),
      dividers[,10],(dividers[,2]-dividers[,10])),(dim(dividers)[1]*2),2,byrow=FALSE) 
    next.gen[1:(dim(dividers)[1]*2),3]<-next.gen[1:(dim(dividers)[1]*2),2]/next.gen[1:(dim(dividers)[1]*2),1]
    next.gen[1:(dim(dividers)[1]*2),4]<-rep(dividers[1:(dim(dividers)[1]),4],2)
    next.gen[1:(dim(dividers)[1]*2),6]<-rep(dividers[1:(dim(dividers)[1]),6],2)
    next.gen[1:(dim(dividers)[1]*2),5]<-rep((dividers[1:(dim(dividers)[1]),5]+1),2)
    next.gen[1:(dim(dividers)[1]*2),7]<-0
    next.gen[(dim(dividers)[1]*2+1):(dim(dividers)[1]+dim(pop.table)[1]),c(1:7)]<-pop.table[pop.table[,7]<pop.table[,4],c(1:7)]
    pop.table<-next.gen
  }
  
  #updating of history table
  colnames(pop.table)<-c("green","red","ratio","persistor?","divisions","bacteria number","time since division")
  pop.table[,3]<-pop.table[,2]/pop.table[,1]
  h<-history
  history<-data.frame(matrix(,(dim(history)[1]+dim(pop.table)[1]),8))
  history[1:length(h[,1]),1:8]<-h
  history[(length(h[,1])+1):length(history[,1]),1:7]<-pop.table
  history[(length(h[,1])+1):length(history[,1]),8]<-j+1 #updating history with the new generation
}