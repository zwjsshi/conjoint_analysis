library(conjoint)
library(fpc)
#attributes (123) data
tprof_c = read.csv("C:\\Users\\zewen\\Documents\\A_My File\\McGill\\Pricing Analytics\\conjoint\\part2\\Response_15profiles_attr.csv")
#levels data
tlevn_c = read.csv("C:\\Users\\zewen\\Documents\\A_My File\\McGill\\Pricing Analytics\\conjoint\\part2\\Response_levels.csv")
#raing data
tprefm_c = read.csv("C:\\Users\\zewen\\Documents\\A_My File\\McGill\\Pricing Analytics\\conjoint\\part2\\Response_15profiles.csv")
#remove outliers 
tprefm_c = tprefm_c[-c(6,11,12,14),]
#resert index of new tprefm_c
rownames(tprefm_c) = NULL
#create tpref table
tpref_c = c()
for (i in (1:nrow(tprefm_c))){
  tpref_c = c(tpref_c, as.numeric(tprefm_c[c(i),]))
}
tpref_c = data.frame(tpref_c)

#respondent 12
caModel(y=tprefm_c[12,],x=tprof_c)

#all respondents 
partworthall= caPartUtilities(y=tpref_c,x=tprof_c,z=as.vector(tlevn_c[,c(2)]))
partworthall

Conjoint(y=tpref_c,x=tprof_c,z=as.vector(tlevn_c[,c(2)]))

#preferred choice for all
utility = as.data.frame(caTotalUtilities(y=tpref_c,x=tprof_c))
utility
avgpreferred = apply(utility, 2, mean)
apply(as.data.frame(avgpreferred), 2, which.max)

preferred = apply(utility, 1, which.max)
table(preferred) 

#segmentation
set.seed(12)
segments = caSegmentation(tprefm_c,tprof_c,2)
library(fpc)
plotcluster(segments$util,segments$sclu)

partseg = as.data.frame(cbind(partworthall,segments$sclu))
colnames(partseg)[14] = c("segment")
library(psych)
segtable = psych::describeBy(partseg, partseg$segment, mat=TRUE)
print(segtable[1:26,5:6])

