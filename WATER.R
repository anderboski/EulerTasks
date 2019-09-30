# 
#    _
# 9 | |      
# 8 | |  _   
# 7 | |_| |    ___
# 6 |     |   |   |
# 5 |     |   |   |
# 4 |     |___|   | 
# 3 |             |
# 2 |             |  
# 1 |0 1 2 3 4 5 6|
#
# Input : [9 6 7 3 3 6 6 ]
# Output : 7
[9 8 7 8 9 5 6]
x = c(9,8,7,8,9,5,6)
y = c(9,0,0,0,0,0,9)
x=sample(0:9,7,replace = TRUE)
water = function(x){
  count = 0
  M = matrix(1,max(x),length(x))
  z = sapply(1:length(x),function(i) M[seq(1,by=1,length.out=(nrow(M)-x[i])),i]<<-0)
  Mprint=M
  M = M[!rowSums(M)%in%c(0,ncol(M)),]
  for (i in 1:nrow(M)){
    a = M[i,]
    ones = which(a==1)
    count = count + length(which(!(min(ones):max(ones))%in%ones))
  }
  print(Mprint)
  print(paste0("The amount of water is: ",count,"."))
  return(count)
}
water(x)


size = seq(10000,100000,by=5000)
times=rep(0,length(size))
aux=1
for (i in size){
  tic()
  samp= sample(1:9,i,replace = TRUE)
  z=water(samp)
  l=toc()
  times[aux] = l$toc-l$tic
  aux=aux+1
}

plot(size,times)

