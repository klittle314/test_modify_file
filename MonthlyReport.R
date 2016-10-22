#Function to create a simple report on which teams have submitted data for a given month.
#edit Report_Submission.Rmd at line 65 to change the month.
#The package ezknitr allows me to direct output to an arbitrary location, evidently not possible directly with knitr
library(ezknitr)
ezknit(file="Report_Submission.Rmd",
       out_dir="C:/Users/Kevin/Dropbox/NNOHA NCA Resources/NDC Phase II/Team Data and Reports",
       out_suffix=as.character(Sys.Date()),
       keep_md=FALSE)