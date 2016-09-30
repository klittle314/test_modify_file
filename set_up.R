#set up the google sheet for testing
library(googlesheets)
library(zoo)


source("helper.R")


#First time read in googlesheet
#set up googlesheet outside of the shiny app
#sheet_name <- c("Draft NNOHA Dental Dashboard Data")

#gskey2 <- c("1dN9rj--OEghw7DdOO0f0y1dcObm2GQwvpbEPWcvAZUU")
# for Google sheet Draft NNOHA Dental Dashboard Tracking Tool_20160622.xlsx
#gskey2 <- c("1m6oVBbHRKb3UuDyImH1Tvl2nUXkQTwLi3Le2kwsciC4")
gskey2 <- c("1ia4R53Q1P8EGy5Yg4CKq0iCFk9CNOSV1Z73jhZ677IA")
gsobj <- gs_key(x=gskey2)
df_master <- gs_read(ss=gsobj,ws=4,range="A4:BC112")
df_master1 <- df_master
#df_master1 <- clean_up_df1(df_master)
#write google sheet with reformatted data
#first delete existing sheet
list_sheets <- gs_ws_ls(ss=gsobj)
if("Summary_Data" %in% list_sheets) {
  gs_ws_delete(ss=gsobj,ws="Summary_Data")
}
#reinitialize gs object
gsobj <- gs_key(x=gskey2)
gs_ws_new(ss=gsobj,ws="Summary_Data",col_extent=58,input=df_master1,anchor="A1")

