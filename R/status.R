.status<-function(status)
{
if (is.null(status))
{
status=0
cat("0")
return(0)
}
else
{
switch(status,
"0" = cat("\b0"),
"1" = cat("\bo"),
"2" = cat("\b."),
"3" = cat("\bx"),
"4" = cat("\bX"),
"5" = cat("\bx"),
"6" = cat("\b."),
"7" = cat("\bo"),
"-1" = cat("\b"),
)
status=status+1
if (status==8)status=0
return(status)
}
}
