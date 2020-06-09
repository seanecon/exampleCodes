#LOS costing model


#read in charge file

charge_md=xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_md_20032010')
charge_ia=xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ia_20032010')
charge_ny=xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ny_20032010')



#need to get hospid using dshospid_hospid_xw
dshospid.hospid_ia=xpt2r.yz("Z:/j_scrdata/lapLearn",'dshopsid_hospid_ia_xw')
dshospid.hospid_ny=xpt2r.yz("Z:/j_scrdata/lapLearn",'dshopsid_hospid_ny_xw')
dshospid.hospid_md=xpt2r.yz("Z:/j_scrdata/lapLearn",'dshopsid_hospid_md_xw')

names(dshospid.hospid_ia)
dshopsid.hospid.xw=do.call(rbind,list(dshospid.hospid_md, dshospid.hospid_ny, dshospid.hospid_ia))

charge_md_withHospid=join(charge_md, dshopsid.hospid.xw, by=c('hospst','dshospid'))
charge_ny_withHospid=join(charge_ny, dshopsid.hospid.xw, by=c('hospst','dshospid'))
charge_ia_withHospid=join(charge_ia, dshopsid.hospid.xw, by=c('hospst','dshospid'))

#read in demo
demo=xpt2r.yz("Z:/j_scrdata/lapLearn",'_demo_iamdny')

#get comorb
comorb = xpt2r.yz('Z:/j_scrdata/lapLearn','comorb_iamymd')
comorb[,'key'] = as.character(comorb[,'key'])

comorb[,'comorbSum'] = rowSums(comorb[,c('chf','valve','pulmcirc','perivasc','para','neuro','chrnlung','dm','dmcx','hypothy','renlfail','liver','ulcer','lymph','mets','tumor','arth','coag','obese','wghtloss','lytes','bldloss','anemdef','alcohol','drug','psych','depress','htn.c')])


comorbcuts=c(0,1,2,3,Inf)
comorb=grpnv.supplycuts.yz(comorb, 'comorbSum', comorbcuts, 'comorbcat')


demoWithComorb=join(demo,comorb,by="key")


age4catcuts=quantile(demoWithComorb[,'age'],prob=c(0,0.25,0.5,0.75,1))
demoWithComorb=grpnv.supplycuts.yz(demoWithComorb, 'age', age4catcuts, 'age4cat')


#reading in ccr file
ccr=rename.vars(xpt2r.yz("F:/sid/Cost_to_charge_ratios",'ccr0310'),'year','whatyear')

genCost=function(indf,ccr){
  
  out=join(indf, ccr[,c('whatyear','hospid','gapicc')],by=c('whatyear','hospid'))
  out[,'cost']=out[,'gapicc']*out[,'totchg']
  return(out)
  
}
chk=do.call(rbind.fill,list(genCost(charge_md_withHospid, ccr),genCost(charge_ny_withHospid, ccr),genCost(charge_ia_withHospid, ccr)))


loscostdf=join(chk,demoWithComorb,by='key')[,c('race','age4cat','comorbcat','pay1','year','cost','los')]

loscostdf.nomiss=loscostdf[complete.cases(loscostdf),]

loscostdf.nomiss[,'race5cat']=replaceValJoin.yz(loscostdf.nomiss[,'race'] #this is typically a data column
                                         ,list(1,2,3,4,c(5,6)) 
                                         ,c(1,2,3,4,5)
                                         ,origValVn='race'
                                         ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
                                         ,outputJoinedDf=TRUE #if F means only output the new vector
                                         #if T, then output df             
)[,'race5cat']

summary(glm(log(cost)~factor(los)+age4cat+race5cat+comorbcat+pay1+year,data=subset(loscostdf.nomiss, los<=7)))






