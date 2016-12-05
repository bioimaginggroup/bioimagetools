.status<-function(status=NULL,random=FALSE)
{
if(random)status=sample(1:8,1)
if (is.null(status))
{
status=8
cat("0")
return(8)
}
else
{
switch(status,
"8" = cat("\b0"),
"1" = cat("\bo"),
"2" = cat("\b."),
"3" = cat("\bx"),
"4" = cat("\bX"),
"5" = cat("\bx"),
"6" = cat("\b."),
"7" = cat("\bo"),
"-1" = cat("\b")
)
status=status+1
if (status==9)status=1
return(status)
}
}
