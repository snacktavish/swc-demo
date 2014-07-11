library(vegan)

args<-commandArgs(TRUE)


run_proc<- function(fi1, fi2){
  file <- readLines(fi1)
  file <- gsub("   +", ", ", file)
  tc <- textConnection(file)
  processed <- read.table(tc, sep=",",skip=1)
  close(tc) 
  
  file <- readLines(fi2)
  file <- gsub("   +", ", ", file)
  tc <- textConnection(file)
  processed1 <- read.table(tc, sep=",",skip=1)
  close(tc) 
  
  ##Prune to include same indivduals
  in1=subset(processed, V2 %in% processed1$V2)
  in2=subset(processed1, V2 %in% processed$V2)
  in1m=as.matrix(in1[,3:4])
  in2m=as.matrix(in2[,3:4])
#  in2m=in2m*100
#  if (in1m[1,1] > in1m[70,1]){
#    in1m[,1]=in1m[,1]*-1}
#  if (in2m[1,1] > in2m[70,1]){
#    in2m[,1]=in2m[,1]*-1} 
#  if (in1m[1,2] < in1m[30,2]){
#    in1m[,2]=in1m[,2]*-1}
#  if (in2m[1,2] < in2m[30,2]){
#    in2m[,2]=in2m[,2]*-1} 
  vare.proc <- procrustes(in1m[c(1,30,70),], in2m[c(1,30,70),])
  summary(vare.proc)
  plot(vare.proc)
  plot(vare.proc, kind=2)
  residuals(vare.proc)
  summ=summary(vare.proc, digits = getOption("digits"))
#  stat=protest(in1m, in2m, scores = "sites", permutations = 99999) 
#  result <- list(stat=stat,summ=summ)
  return(vare.proc$ss)
}

run_proc(args[1],args[2])
