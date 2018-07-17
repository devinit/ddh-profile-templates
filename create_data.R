list.of.packages <- c("data.table","openxlsx")
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

# crs = read.csv("data/DDH/CRS.csv",na.strings="",as.is=T)
# save(crs,file="data/crs.RData")
load("data/crs.RData")

totals = data.table(crs)[,
  .(usd_disb_total=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name)]

p3_sector = data.table(crs)[,
  .(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,itep_sector)]
p3_sector = merge(p3_sector,totals,by="recipient_name")
p3_sector = p3_sector[complete.cases(p3_sector),]
p3_sector$percent = 100*(p3_sector$usd_disb/p3_sector$usd_disb_total)
p3_sector = p3_sector[order(p3_sector$recipient_name,-p3_sector$percent)]

p3_bundle = data.table(crs)[,
  .(usd_disb=sum(usd_disbursement,na.rm=T))
    ,by=.(recipient_name,oda_bundle_name)]
p3_bundle = merge(p3_bundle,totals,by="recipient_name")
p3_bundle = p3_bundle[complete.cases(p3_bundle),]
p3_bundle$percent = 100*(p3_bundle$usd_disb/p3_bundle$usd_disb_total)
p3_bundle = p3_bundle[order(p3_bundle$recipient_name,-p3_bundle$percent)]

p3_donor = data.table(crs)[,
  .(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,donor_name)]
p3_donor = merge(p3_donor,totals,by="recipient_name")
p3_donor = p3_donor[complete.cases(p3_donor),]
p3_donor$percent = 100*(p3_donor$usd_disb/p3_donor$usd_disb_total)
p3_donor = p3_donor[order(p3_donor$recipient_name,-p3_donor$percent)]

p4_sector_total = data.table(crs)[,
  .(usd_disb_sector_total=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,itep_sector)]
p4_bundle_total = data.table(crs)[,
  .(usd_disb_bundle_total=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,oda_bundle_name)]
p4a = data.table(crs)[,
  .(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,itep_sector,oda_bundle_name)]
p4a = merge(p4a,p4_bundle_total,by=c("recipient_name","oda_bundle_name"))
p4a = merge(p4a,p4_sector_total,by=c("recipient_name","itep_sector"))
p4a = p4a[complete.cases(p4a),]
p4a$bundle_percent_of_sector = 100*(p4a$usd_disb/p4a$usd_disb_sector_total)
p4a$sector_percent_of_bundle = 100*(p4a$usd_disb/p4a$usd_disb_bundle_total)
p4a = p4a[order(p4a$recipient_name,p4a$itep_sector,-p4a$sector_percent)]

profile_tables = list(
  "P3 bundle"=p3_bundle,
  "P3 donor"=p3_donor,
  "P3 sector"=p3_sector,
  "P4a"=p4a
  )
table_names = names(profile_tables)
wb = createWorkbook()
for(table_name in table_names){
  addWorksheet(wb,table_name)
  table_dat = profile_tables[[table_name]]
  writeData(wb,sheet=table_name,table_dat)
}
saveWorkbook(wb,file="data/profile_data.xlsx",overwrite=T)
