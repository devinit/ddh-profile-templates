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

#donor p4 starts here####

p4b_donor = data.table(crs)[
  ,.(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(donor_name,itep_sector,oda_bundle_name)
]
p4b_donor = p4b_donor[order(p4b_donor$donor_name,p4b_donor$itep_sector,-p4b_donor$usd_disb)]

# counting top sectors by bundle ####
p4b_donor$donor_sector = paste(p4b_donor$donor_name,p4b_donor$itep_sector)
top10_sector_donor = data.table(crs)[
  ,.(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(donor_name,itep_sector)
  ]
top10_sector_donor = top10_sector_donor[order(top10_sector_donor$donor_name,-top10_sector_donor$usd_disb)]

top10_sector_donor = top10_sector_donor[,head(.SD,10),by=.(donor_name)]
valid_combinations = paste(top10_sector_donor$donor_name,top10_sector_donor$itep_sector)


p4b_donor_top1_sector = subset(p4b_donor,donor_sector %in% valid_combinations)
p4b_donor_top1_sector$dup = duplicated(data.frame(p4b_donor_top1_sector)[,c("donor_name","itep_sector")])
p4b_donor_top1_sector = subset(p4b_donor_top1_sector,!dup)
p4b_donor_top1_sector = merge(p4b_donor_top1_sector,top10_sector_donor,all=FALSE)
p4b_donor_top1_sector$count = 1
p4b_donor_top1_sector_count = p4b_donor_top1_sector[,.(top1count=sum(count)),by=.(donor_name,oda_bundle_name)]
p4b_donor_top3_sector = subset(p4b_donor,donor_sector %in% valid_combinations)
p4b_donor_top3_sector = p4b_donor_top3_sector[,head(.SD,3),by=.(donor_name,itep_sector)]
p4b_donor_top3_sector$count = 1
p4b_donor_top3_sector_count = p4b_donor_top3_sector[,.(top3count=sum(count)),by=.(donor_name,oda_bundle_name)]
p4b_donor_top_sector = merge(p4b_donor_top1_sector_count,p4b_donor_top3_sector_count,all=TRUE)

# counting top bundles by sector ####
p4b_donor$donor_bundle = paste(p4b_donor$donor_name,p4b_donor$oda_bundle_name)
top10_bundle_donor = data.table(crs)[
  ,.(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(donor_name,oda_bundle_name)
  ]
top10_bundle_donor = top10_bundle_donor[order(top10_bundle_donor$donor_name,-top10_bundle_donor$usd_disb)]

top10_bundle_donor = top10_bundle_donor[,head(.SD,10),by=.(donor_name)]
valid_combinations = paste(top10_bundle_donor$donor_name,top10_bundle_donor$oda_bundle_name)


p4b_donor_top1_bundle = subset(p4b_donor,donor_bundle %in% valid_combinations)
p4b_donor_top1_bundle$dup = duplicated(data.frame(p4b_donor_top1_bundle)[,c("donor_name","oda_bundle_name")])
p4b_donor_top1_bundle = subset(p4b_donor_top1_bundle,!dup)
p4b_donor_top1_bundle = merge(p4b_donor_top1_bundle,top10_bundle_donor,all=FALSE)
p4b_donor_top1_bundle$count = 1
p4b_donor_top1_bundle_count = p4b_donor_top1_bundle[,.(top1count=sum(count)),by=.(donor_name,itep_sector)]
p4b_donor_top3_bundle = subset(p4b_donor,donor_bundle %in% valid_combinations)
p4b_donor_top3_bundle = p4b_donor_top3_bundle[,head(.SD,3),by=.(donor_name,oda_bundle_name)]
p4b_donor_top3_bundle$count = 1
p4b_donor_top3_bundle_count = p4b_donor_top3_bundle[,.(top3count=sum(count)),by=.(donor_name,itep_sector)]
p4b_donor_top_bundle = merge(p4b_donor_top1_bundle_count,p4b_donor_top3_bundle_count,all=TRUE)

#recipient p4 starts here ####
p4b_recipient = data.table(crs)[
  ,.(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,donor_name,oda_bundle_name)
  ]
p4b_recipient = p4b_recipient[order(p4b_recipient$recipient_name,p4b_recipient$donor_name,-p4b_recipient$usd_disb)]
p4b_recipient$recipient_sector = paste(p4b_recipient$recipient_name,p4b_recipient$donor_name)
top10_donor_sector = data.table(crs)[
  ,.(usd_disb=sum(usd_disbursement,na.rm=T))
  ,by=.(recipient_name,donor_name)
  ]
top10_donor_sector = top10_donor_sector[order(top10_donor_sector$recipient_name,-top10_donor_sector$usd_disb)]

top10_donor_sector = top10_donor_sector[,head(.SD,10),by=.(recipient_name)]
valid_combinations = paste(top10_donor_sector$recipient_name,top10_donor_sector$donor_name)

p4b_recipient_top1_bundle= subset(p4b_recipient,recipient_sector %in% valid_combinations)
p4b_recipient_top1_bundle = p4b_recipient_top1_bundle[,head(.SD,1),by=.(recipient_name,donor_name)]
p4b_recipient_top1_bundle$count = 1
p4b_recipient_top1_bundle_count = p4b_recipient_top1_bundle[,.(top1count=sum(count)),by=.(recipient_name,oda_bundle_name)]

p4b_recipient_top3_bundle = subset(p4b_recipient,recipient_sector %in% valid_combinations)
p4b_recipient_top3_bundle = p4b_recipient_top3_bundle[,head(.SD,3),by=.(recipient_name,donor_name)]
p4b_recipient_top3_bundle$count = 1
p4b_recipient_top3_bundle_count = p4b_recipient_top3_bundle[,.(top3count=sum(count)),by=.(recipient_name,oda_bundle_name)]
p4b_recipient_top_bundle = merge(p4b_recipient_top1_bundle_count,p4b_recipient_top3_bundle_count,all=TRUE)

conditional_attribute_count=function(crs,attrib1,attrib2,attrib3){
  p4b_recipient = data.table(crs)[
    ,.(usd_disb=sum(usd_disbursement,na.rm=T))
    ,by=c(attrib1,attrib2,attrib3)
    ]
  p4b_recipient = p4b_recipient[order(p4b_recipient[,attrib1, with=F],p4b_recipient[,attrib2, with=F],-p4b_recipient$usd_disb)]
  p4b_recipient$recipient_sector = paste(p4b_recipient[,attrib1, with=F][[1]],p4b_recipient[,attrib2, with=F][[1]])
  top10_donor_sector = data.table(crs)[
    ,.(usd_disb=sum(usd_disbursement,na.rm=T))
    ,by=c(attrib1,attrib2)
    ]
  top10_donor_sector = top10_donor_sector[order(top10_donor_sector[,attrib1, with=F],-top10_donor_sector$usd_disb)]

  top10_donor_sector = top10_donor_sector[,head(.SD,10),by=c(attrib1)]
  valid_combinations = paste(top10_donor_sector[,attrib1, with=F][[1]],top10_donor_sector[,attrib2, with=F][[1]])

  p4b_recipient_top1_bundle= subset(p4b_recipient,recipient_sector %in% valid_combinations)
  p4b_recipient_top1_bundle = p4b_recipient_top1_bundle[,head(.SD,1),by=c(attrib1,attrib2)]
  p4b_recipient_top1_bundle$count = 1
  p4b_recipient_top1_bundle_count = p4b_recipient_top1_bundle[,.(top1count=sum(count)),by=c(attrib1,attrib3)]

  p4b_recipient_top3_bundle = subset(p4b_recipient,recipient_sector %in% valid_combinations)
  p4b_recipient_top3_bundle = p4b_recipient_top3_bundle[,head(.SD,3),by=c(attrib1,attrib2)]
  p4b_recipient_top3_bundle$count = 1
  p4b_recipient_top3_bundle_count = p4b_recipient_top3_bundle[,.(top3count=sum(count)),by=c(attrib1,attrib3)]
  p4b_recipient_top_bundle = merge(p4b_recipient_top1_bundle_count,p4b_recipient_top3_bundle_count,all=TRUE)
  return(p4b_recipient_top_bundle)
}

profile_tables = list(
  "P3 bundle"=p3_bundle,
  "P3 donor"=p3_donor,
  "P3 sector"=p3_sector,
  "P4a"=p4a,
  "p4b donor top sector"=p4b_donor_top_sector,
  "p4b recip top bundle"=p4b_recipient_top_bundle
  )
table_names = names(profile_tables)
wb = createWorkbook()
for(table_name in table_names){
  addWorksheet(wb,table_name)
  table_dat = profile_tables[[table_name]]
  writeData(wb,sheet=table_name,table_dat)
}
saveWorkbook(wb,file="data/profile_data.xlsx",overwrite=T)
