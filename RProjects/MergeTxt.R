multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){readLines(x,encoding="UTF-8")})
  Reduce(function(x,y) {merge(x,y)}, datalist)}
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){readLines(x,encoding="UTF-8")})
  }
mymergedata = multmerge("~/Downloads/zmPDSwR-master/CodeExamples/c06_Memorization_methods")
mymergedata = multmerge("~/Downloads/Test")
mymergedata
writeLines(mymergedata, con = "~/Downloads/Test/Outfile1.txt",sep = "\n", useBytes = FALSE)
cat(mymergedata,file="Outfile.txt",sep="\n")
write.table(mymergedata$x,"~/Downloads/Test/Outfile1.txt")
