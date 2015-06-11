readBMP<-function(file)
{
input<-file(file,"rb")
if(readBin(input,integer(),size=2)!=19778)return(NULL)
readBin(input,integer(),size=4)
readBin(input,integer(),size=2)
readBin(input,integer(),size=2)
start=readBin(input,integer(),size=4)

readBin(input,integer(),size=4)
X=readBin(input,integer(),size=4)
Y=readBin(input,integer(),size=4)
readBin(input,integer(),size=2)
bpp=readBin(input,integer(),size=2)
start=start-32
for (i in 1:(start/2))
readBin(input,integer(),size=2)

result<-readBin(input,integer(),n=X*Y,size=bpp/8)
result<-array(result,c(X,Y))
close(input)


return(result)
}


