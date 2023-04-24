# define sex
ii$sex.2<-factor(ii$sex,c('F','M'),c('Female','Male'))
table(ii$sex.2,useNA='always')