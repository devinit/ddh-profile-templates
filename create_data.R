list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "C:"
}

wd = paste0(prefix,"/git/ddh-profile-templates")
setwd(wd)


#### Insert data generation code here ####

write.csv(dat,"data/countries.csv",row.names=F,na="")