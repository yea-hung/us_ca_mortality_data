# collapse each of the 3 race variables
collapse_race<-function(vv){
  cc<-readxl::read_excel('E:/mappings/CCDF_Dynamic_Data Dictionary.xlsx',
                         sheet=6,skip=1)
  cc<-as.data.frame(cc)
  vv<-factor(vv,cc$Race_Code,cc$Race_Description)
  levels(vv)[grepl('^WHI',levels(vv))]<-'White'
  levels(vv)[grepl('^BLA',levels(vv))]<-'Black'
  levels(vv)[grepl('^ASI|^IND|^FIL',levels(vv))]<-'Asian'
  levels(vv)[grepl('^AME|^ALEU|^ESKI',levels(vv))]<-'Native American'
  levels(vv)[grepl('^PAC|^GUAM|^SAMO|^HAWA',levels(vv))]<-'Pacific Islander'
  levels(vv)[grepl('OTHER',levels(vv))]<-'Other'
  is.na(vv)<-vv=='UNKNOWN'
  vv<-as.character(vv)
  vv
}
ii$race.1<-collapse_race(ii$race.1.code.final)
ii$race.2<-collapse_race(ii$race.2.code.final)
ii$race.3<-collapse_race(ii$race.3.code.final)

# define race/ethnicity
ii$mixed<-apply(ii[,paste('race',1:3,sep='.')],1,
                function(rr){ length(unique(rr[!is.na(rr)])) })
ii$re<-ifelse(ii$mixed>1,'Mixed',ii$race.1)
ii$re<-ifelse(ii$hispanic.origin=='Y','Latino',ii$re)