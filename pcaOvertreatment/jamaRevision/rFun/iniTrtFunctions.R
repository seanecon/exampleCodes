



trtType.searchBetweenDxFirstCode <- function(timeTrtDf,seenCodes, between.exclusionCodes){
  
  #find first IMRT
  seenCodeIndex <- which(timeTrtDf[,'procedure'] %in% seenCodes)
  firstSeenCode.loc <-  ifelse(length(seenCodeIndex)==0, NA, min(seenCodeIndex))
  
  if (is.na(firstSeenCode.loc)){
    output <- 'noSeenCodes' # no imrt
    firstSeenCodeDate=NA
  }else {
    firstSeenCodeDate = timeTrtDf[firstSeenCode.loc,'time']
    if (any(vec[1:firstSeenCode.loc] %in% between.exclusionCodes )){output <- 'hasCodesWithPriorExclusionCodes'} else{output <-'hasCodesWithoutPriorExclusionCodes' }
  }
  outlist <- list(firstSeenCodeDate=firstSeenCodeDate,seenCodeIndicator=output)
  return(outlist)
}

trtType.searchEntireProfile <- function(timeTrtDf,seenCodes, exclusionCodes){
  
  #find first IMRT
  seenCodeIndex <- which(timeTrtDf[,'procedure'] %in% seenCodes)
  firstSeenCode.loc <-  ifelse(length(seenCodeIndex)==0, NA, min(seenCodeIndex))
  
  if (is.na(firstSeenCode.loc)){firstSeenCodeDate=NA; output='seenCodesUnfound'} else{firstSeenCodeDate <- timeTrtDf[firstSeenCode.loc,'time'];
                                                                                      if(any(timeTrtDf[,'procedure'] %in% exclusionCodes )){output='hasCodesWithExclusionCodes'}else{output='hasCodesWithoutExclusionCodes'}
  }
  outlist <- list(firstSeenCodeDate=firstSeenCodeDate,seenCodeIndicator=output)
  return(outlist)
}

onlyHas <- function(timeTrtDf,codes){
  seenCodeIndex <- which(timeTrtDf[,'procedure'] %in% codes)
  firstSeenCode.loc <-  ifelse(length(seenCodeIndex)==0, NA, min(seenCodeIndex))
  if (is.na(firstSeenCode.loc)){firstSeenCodeDate=NA} else{firstSeenCodeDate <- timeTrtDf[firstSeenCode.loc,'time']}
  if( all(timeTrtDf[,'procedure'] %in% codes)){output='onlyhasDesiredCodes'} else {output='hasOtherCodes'}
  outlist <- list(firstSeenCodeDate=firstSeenCodeDate,seenCodeIndicator=output)
  return(outlist)
}

notHasAny <- function(timeTrtDf,codes){
  
  !any(timeTrtDf[,'procedure'] %in% codes)
  
}



trtAssign <- function(timeTrtDf){  
  if (all(timeTrtDf[,'procedure']=='blank')){
    type='obs'; return(type)
  }else{
    
    timeTrtDf.noblank <- subset( timeTrtDf,!procedure=='blank')
    outList.imrt <- trtType.searchBetweenDxFirstCode( timeTrtDf.noblank,c('imrt'),c('robot_radpromy','perineal_promy','abdomen_radpromy'))
    if (outList.imrt$seenCodeIndicator == "hasCodesWithoutPriorExclusionCodes") {
      type='imrt'; return(type) }else{
        outList.rp <- trtType.searchBetweenDxFirstCode( timeTrtDf.noblank,c('robot_radpromy','perineal_promy','abdomen_radpromy'),c('imrt','ebrt','brach','cryo','sbrt','proton_beam','androgenorc'))
        
        if (outList.rp$seenCodeIndicator == "hasCodesWithoutPriorExclusionCodes") {
          
          type=assignTypeByPriority.yz( list(timeTrtDf.noblank[,'procedure'] )
                                        , c('robot_radpromy','perineal_promy','abdomen_radpromy') 
                                        ,unAssignedType='unAssigned')
          
          return(type) } else {
            #print('i am')
            outList.ebrt <- trtType.searchEntireProfile( timeTrtDf.noblank,c('ebrt'), 'imrt') 
            if (outList.ebrt$seenCodeIndicator=="hasCodesWithoutExclusionCodes") {type='ebrt'; return(type)} else{ 
              outList.brach <- onlyHas( timeTrtDf.noblank,c('brach'))
              if(outList.brach$seenCodeIndicator == "onlyhasDesiredCodes"){
                type <- 'brach'; return(type) } else{        
                  type <- assignTypeByPriority.yz( list(timeTrtDf.noblank[,'procedure']) 
                                                   , c('proton_beam','sbrt','cryo','androgeninj','androgenimp','androgenorc')
                                                   ,unAssignedType='unAssigned')
                  return(type)
                }      
            }
          }
      }
    
  }
}

assignIniTreatment <- function(profile.i){
  intoBlank <- c("pellymmy"
                 ,'radia_plan'
                 ,"imrt_plan"
                 ,"brach_plan"
                 ,"officevisit" 
                 ,"consultation" 
                 ,"hospitalcare" 
                 ,"telephonecare","nursing_home"
                 ,"DRE" 
                 ,"testosterone","biopsy","transurethral"
                 ,"bladder/urethra","male sling/AUS","penile prosthesis"
                 ,"percutaneous renal","colonoscopy","anesthesia","artline","PSA","testosterone/DHT","urography" 
                 ,"CTchest"
                 ,"CTabdomen/3D" 
                 ,"CTpelvis" 
                 ,"MRIchest" 
                 ,"MRIabdomen" 
                 ,"MRIpelvis" 
                 ,"PET" 
                 ,"ultrasound-transrectal" 
                 ,"ultrasound-abdominal/renal" 
                 ,"prostascint" 
                 ,"bonedensity" 
                 ,"bonescan" 
                 ,"chemotherapy" 
                 ,"chemo_assessmt","zoledronicacid","strontium" 
                 ,"urintest" 
                 ,"PVR","nonPSA_blood" 
                 , 'chest/rib/spine/hip/femur/osseousfi'
                 ,"urodynflow" 
                 ,"EKG_echo" 
                 ,"path_consul" 
                 ,"path_specimen" 
                 , NA
  )
  
  profile.i[which(profile.i[,"assoc.hs.type"] %in% intoBlank),'assoc.hs.type'] <- 'blank'
  vec =profile.i[,"assoc.hs.type"]
  timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
  iniTrt <- trtAssign(timeTrtDf)
  return(iniTrt)
}
















# 
# 
# seenCodes <- c('openRp','robotRp')
# between.exclusionCodes <- c('hormone')
# trtType.searchBetweenDxFirstCode <- function(timeTrtDf,seenCodes, between.exclusionCodes){
#   
#   #find first IMRT
#   seenCodeIndex <- which(timeTrtDf[,'procedure'] %in% seenCodes)
#   firstSeenCode.loc <-  ifelse(length(seenCodeIndex)==0, NA, min(seenCodeIndex))
#   
#   if (is.na(firstSeenCode.loc)){
#     output <- 'noSeenCodes' # no imrt
#     firstSeenCodeDate=NA
#   }else {
#     firstSeenCodeDate = timeTrtDf[firstSeenCode.loc,'time']
#     if (any(vec[1:firstSeenCode.loc] %in% between.exclusionCodes )){output <- 'hasCodesWithPriorExclusionCodes'} else{output <-'hasCodesWithoutPriorExclusionCodes' }
#   }
#   outlist <- list(firstSeenCodeDate=firstSeenCodeDate,seenCodeIndicator=output)
#   return(outlist)
# }
# 
# trtType.searchBetweenDxFirstCode(timeTrtDf,'imrt',c('openRp','robotRp'))
# trtType.searchBetweenDxFirstCode(timeTrtDf,c('openRp','robotRp'),c('hormone'))
# 
# seenCodes <- 'ebrt'
# exclusionCodes <- 'imrt'
# trtType.searchEntireProfile <- function(timeTrtDf,seenCodes, exclusionCodes){
#   
#   #find first IMRT
#   seenCodeIndex <- which(timeTrtDf[,'procedure'] %in% seenCodes)
#   firstSeenCode.loc <-  ifelse(length(seenCodeIndex)==0, NA, min(seenCodeIndex))
#   
#   if (is.na(firstSeenCode.loc)){firstSeenCodeDate=NA; output='seenCodesUnfound'} else{firstSeenCodeDate <- timeTrtDf[firstSeenCode.loc,'time'];
#                                                                                       if(any(timeTrtDf[,'procedure'] %in% exclusionCodes )){output='hasCodesWithExclusionCodes'}else{output='hasCodesWithoutExclusionCodes'}
#   }
#   outlist <- list(firstSeenCodeDate=firstSeenCodeDate,seenCodeIndicator=output)
#   return(outlist)
# }
# 
# vec <- c('ebrt','imrt')
# timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
# timeTrtDf
# 
# vec <- c('ebrt','brach')
# timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
# timeTrtDf
# trtType.searchEntireProfile(timeTrtDf,c('ebrt'), 'imrt')
# 
# onlyHas <- function(timeTrtDf,codes){
#   seenCodeIndex <- which(timeTrtDf[,'procedure'] %in% codes)
#   firstSeenCode.loc <-  ifelse(length(seenCodeIndex)==0, NA, min(seenCodeIndex))
#   if (is.na(firstSeenCode.loc)){firstSeenCodeDate=NA} else{firstSeenCodeDate <- timeTrtDf[firstSeenCode.loc,'time']}
#   if( all(timeTrtDf[,'procedure'] %in% codes)){output='onlyhasDesiredCodes'} else {output='hasOtherCodes'}
#   outlist <- list(firstSeenCodeDate=firstSeenCodeDate,seenCodeIndicator=output)
#   return(outlist)
# }
# vec <- c('ebrt','brach','')
# vec <- c('brach','brach')
# timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
# timeTrtDf
# onlyHas(timeTrtDf,c('brach','ebrt'))
# 
# 
# notHasAny <- function(timeTrtDf,codes){
#   
#   !any(timeTrtDf[,'procedure'] %in% codes)
#   
# }
# 
# vec <- c('ebrt','brach','')
# timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
# timeTrtDf
# notHasAny(timeTrtDf,c('robotRp','imrt'))
# 
# tdf <- data.frame(a=c(1,2,3,3,4),b=c('a','b','b','d','b'))
# subset(tdf,!b=='b')
# 
# 
# 
# 
# 
# 
# vec <- c('brach','imrt','brach','ebrt','robotRp','openRp','openRp','brach','cryo','cryo')
# vec <- c('')
# trtType.searchEntireProfile(timeTrtDf,c('ebrt'), 'imrt') 
# vec <- c('cryo','proton')
# 
# vec <- c('imrt','imrt','brach','ebrt','robotRp','openRp','imrt','openRp','brach','cryo','cryo')
# vec <- c('openRp','imrt','brach','ebrt','robotRp','openRp','imrt','openRp','brach','cryo','cryo')
# vec <- c('hormone','openRp','imrt','brach','ebrt','robotRp','openRp','imrt','openRp','brach','cryo','cryo')
# vec <- c('sbrt','openRp','imrt','brach','ebrt','robotRp','openRp','imrt','openRp','brach','cryo','cryo')
# 
# timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
# timeTrtDf
# 
# #fir unassigned we then use the following function to further assign
# assignTypeByPriority.yz(  valueList #each element is a treatemnt procedure profile
#                           , c('proton','sbrt','cryo','hormone','orchi') #most important is the 1st priority, last element is least proitty                                    # this is usually initital treatment types
#                           ,unAssignedType=NA #you can change this into say 'unknownType'
# )
# 
# profileDf <- xpt2r.yz('Z:/j_scrdata/pcaOverTreatmentRevision/treatmentProfileBatch','trtProfileBat100')
# 
# profile.i <- subset(profileDf,regcase=="8896072299")
# assignIniTreatment(profile.i)
# uniqReg <- unique(profileDf[,2])
# 
# 
# out <- assignIniTreatment(subset(profileDf,regcase==uniqReg[5]))
# 
# outList <- list()
# for(i in 1:10){
#   outList <- lappend.yz(outList,assignIniTreatment(subset(profileDf,regcase==uniqReg[i])))
#   print(i)
#   print(uniqReg[i])
#   
# }
# outList
# ddply(profileDf,'regcase',function(x){assignIniTreatment(x)})
# profile.i <-subset(profileDf,regcase==uniqReg[8])
# 
# 
# 
# 
# 
# trtAssign <- function(timeTrtDf){  
#   if (all(timeTrtDf[,'procedure']=='blank')){
#     type='obs'; return(type)
#   }else{
#     
#     timeTrtDf.noblank <- subset( timeTrtDf,!procedure=='blank')
#     outList.imrt <- trtType.searchBetweenDxFirstCode( timeTrtDf.noblank,c('imrt'),c('robot_radpromy','perineal_promy','abdomen_radpromy'))
#     if (outList.imrt$seenCodeIndicator == "hasCodesWithoutPriorExclusionCodes") {
#       type='imrt'; return(type) }else{
#         outList.rp <- trtType.searchBetweenDxFirstCode( timeTrtDf.noblank,c('robot_radpromy','perineal_promy','abdomen_radpromy'),c('imrt','ebrt','brach','cryo','sbrt','proton_beam','androgenorc'))
#         
#         if (outList.rp$seenCodeIndicator == "hasCodesWithoutPriorExclusionCodes") {
#           
#           type=assignTypeByPriority.yz( list(timeTrtDf.noblank[,'procedure'] )
#                                         , c('robot_radpromy','perineal_promy','abdomen_radpromy') 
#                                         ,unAssignedType='unAssigned')
#           
#           return(type) } else {
#             print('i am')
#             outList.ebrt <- trtType.searchEntireProfile( timeTrtDf.noblank,c('ebrt'), 'imrt') 
#             if (outList.ebrt$seenCodeIndicator=="hasCodesWithoutExclusionCodes") {type='ebrt'; return(type)} else{ 
#               outList.brach <- onlyHas( timeTrtDf.noblank,c('brach'))
#               if(outList.brach$seenCodeIndicator == "onlyhasDesiredCodes"){
#                 type <- 'brach'; return(type) } else{        
#                   type <- assignTypeByPriority.yz( list(timeTrtDf.noblank[,'procedure']) 
#                                                    , c('proton_beam','sbrt','cryo','androgeninj','androgenimp','androgenorc')
#                                                    ,unAssignedType='unAssigned')
#                   return(type)
#                 }      
#             }
#           }
#       }
#     
#   }
# }
# 
# assignIniTreatment <- function(profile.i){
#   intoBlank <- c("pellymmy"
#                  ,'radia_plan'
#                  ,"imrt_plan"
#                  ,"brach_plan"
#                  ,"officevisit" 
#                  ,"consultation" 
#                  ,"hospitalcare" 
#                  ,"telephonecare","nursing_home"
#                  ,"DRE" 
#                  ,"testosterone","biopsy","transurethral"
#                  ,"bladder/urethra","male sling/AUS","penile prosthesis"
#                  ,"percutaneous renal","colonoscopy","anesthesia","artline","PSA","testosterone/DHT","urography" 
#                  ,"CTchest"
#                  ,"CTabdomen/3D" 
#                  ,"CTpelvis" 
#                  ,"MRIchest" 
#                  ,"MRIabdomen" 
#                  ,"MRIpelvis" 
#                  ,"PET" 
#                  ,"ultrasound-transrectal" 
#                  ,"ultrasound-abdominal/renal" 
#                  ,"prostascint" 
#                  ,"bonedensity" 
#                  ,"bonescan" 
#                  ,"chemotherapy" 
#                  ,"chemo_assessmt","zoledronicacid","strontium" 
#                  ,"urintest" 
#                  ,"PVR","nonPSA_blood" 
#                  , 'chest/rib/spine/hip/femur/osseousfi'
#                  ,"urodynflow" 
#                  ,"EKG_echo" 
#                  ,"path_consul" 
#                  ,"path_specimen" 
#                  , NA
#   )
#   
#   profile.i[which(profile.i[,"assoc.hs.type"] %in% intoBlank),'assoc.hs.type'] <- 'blank'
#   vec =profile.i[,"assoc.hs.type"]
#   timeTrtDf <- data.frame(time=seq(length(vec)),procedure=vec)
#   iniTrt <- trtAssign(timeTrtDf)
#   return(iniTrt)
# }
# 
# 
# 
# 
# 
# 
# 
# 
