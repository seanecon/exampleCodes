/*this to get the low dimco risk and short survival*/
/*need to use dimco to get their high low risk*/

/*libname pcaot 'Z:/j_scrdata/pcaOverTreatment';*/
data _dajccm; set svr09sr.pedsfprostate (keep=regcase %_paste(dajccm|%_seq(10))); run;
data dx_2004_2007;
set psfpr09.pca_dxdate_all;
if _first_dx_year<=2007 and _first_dx_year>=2004;
run;

/*all diagnosed between 2004 and 2007*/
%_joh(
  dx_2004_2007 
, 
, _dajccm
, regcase %_paste(dajccm|%_seq(10))
, regcase
, _dxDajccm
, 0.8
)

%dnobs(svr09sr.pedsfprostate  psfpr09.pca_dxdate_all)
%_genMets_dajccm(
   _dxDajccm
/*regcase _first_cancer_site _first_dx_year dajccm1-10 (2004+)*/
, pcaot.pcadx20042007Mets
, varsLists=%_paste(dajccm|%_seq(10))
/*%_paste(dajcct|%_seq(10))*/
, locationVn=_first_cancer_site
, maxcol=10 /*there are 10 sites*/
, metsVn=_metsInd
)

%let keptPedsfVars=
regcase 
/*birthm*/
birthyr birthm /*used by macro _firstcancer_dx_date*/
/*M_sex */
/*pedsf 1 is male*/
M_sex /*sumdenomnc 1 is male*/
/*birthd */
/*only in sumdenomnc*/
race
/*1 white 2 black 3 is other, 4 is asian, 5 is hispanic, 6 native american, 0 is unknown, we probably will be excluding these with unknown race*/
/*urbrur*/
urban
state 
county
zip	
/*geo*/
/* codpub*/
/*codpubf*/
 /*cause of death*/
/*in new data 2011, codpub is changed into codpubf*/
%_paste(reg|%_seq(1,10,1))
/*%_paste(yrdx|%_seq(1,10,1))*/
/*%_paste(modx|%_seq(1,10,1))*/
med_dodm med_dodd med_dody
/*for dimico*/
%_paste(cs1st|%_seq(10))  
/*probably psa score should be divided by 10*/
/*gleason score*/
%_paste(cs6st|%_seq(10)) 
%_paste(csex|%_seq(10))
;

data pcaot._pesfRaw;
set svr09sr.pedsfprostate(keep=&keptPedsfVars.);
run;

/*first find these meet entiroement and their diagnossi date (index day)*/
/*the cohort*/
%dvn(scrimr.prost_bf12after12_dx0107)

/*comorb for all pca*/
%dvn(psfpr09.pca_comorb)
data pca_comorb_dxDay;
set psfpr09.pca_comorb;
keep regcase _first_cancer_site _first_dx_day _first_dx_year _first_dx_month _firstdxdate_num chrlson;
run;


%_joz(pca_comorb_dxDay,pcaot._pesfRaw,_withSite,regcase)

%_genDAmicoPcaRiskCat(
  _withSite
/*regcase _first_cancer_site cs1st1-cs1st10 cs6st1-cs6st10 dajcct1-dajcct10*/
, _TMPDamico
, varsLists=%_paste(cs1st|%_seq(10)) | %_paste(cs6st|%_seq(10)) | %_paste(csex|%_seq(10))
, locationVn=_first_cancer_site
, maxcol=10
)

%dvn(_tmpdamico)

data chk; set _tmpdamico; if _first_dx_year>=2004;run; 



proc freq data=chk; table _damicoRisk;run;

%kdv(_TMPDamico
, %_paste(cs1st|%_seq(10))  %_paste(cs6st|%_seq(10))  %_paste(csex|%_seq(10)) /*use %str(|) to separate */
, _TMPDamico_1 /*if outdsn_list is empty then operation is done on indsn_list*/
, 0 /*can supply just one element, if so, that element will be repeated if keep then 1, if drop then 0*/
)
data outDf_regcase; set scrimr.prost_bf12after12_dx0107; keep regcase; run;
/*N=*/
%dnobs(scrimr.prost_bf12after12_dx0107)
/*N=125299 entitlement bf12 after 12 dx 2001-2007 die within 6 months removed*/
%_joz(
 outDf_regcase
,_TMPDamico_1
,output
, regcase /*this will be recyled to the length of rightDslist*/
)

/*Next line is important because we now restrict to dx year=2004 or later*/
data output1;
set output;
if _first_dx_year>=2004;
run;
/*N=69429 dx after 2004*/

/*now let us pick up registry location*/
%_pickAryVal(
  output1
, _grabRegistry
, 10 /*MAX NUMBER OF COLUMNS IN arrays*/
, _first_cancer_site /*variable which indicates the columns we like to pick*/
, %_paste(reg|%_seq(1,10,1)) /*separated by |, one group is a one set of varaible, order matters*/
, C /*indicator type of array for each set, if there are two sets and the first one is numerican and the second one is character, then:  N C*/
, registry
)


%_vt(_grabRegistry)
/*already meet entitlement and already remove diagnosis not in 2004 2005, 2006 and 2007*/
/*has comorbdity and registory information*/



/* metsVn=_metsInd*/


/*pcaot.robot_notRadiaBfTrt*/
/*pcaot.imrt_notSrgBfTrt*/


%dvn(pcaot.robot_notRadiaBfTrt)

%_vins(
  _grabRegistry /*ds can be one or many data*/
, regcase
, pcaot.robot_notRadiaBfTrt /*only vn is used, the data should be unique, if not then dedup is going on*/
, _grabRegistry_robotInd
, hasRobotNoRadia /*can be left empty, if so, it will be called _MAC_vins_ind_&vn., this variable takes value 1 or .*/
/*you can change . into 0, but this may take further action*/
, dedup_set_ds=0 /*first dedup set_ds*/
, missing_2_0=1 /*want to change missing (i.e., .) into 0*/
/*doing so will takes more time, so typically do not recommend so*/
)


%_vins(
  _grabRegistry_robotInd /*ds can be one or many data*/
, regcase
, pcaot.imrt_notSrgBfTrt /*only vn is used, the data should be unique, if not then dedup is going on*/
, _grabRegistry_robotimrtInd
, hasimrtNoSrg /*can be left empty, if so, it will be called _MAC_vins_ind_&vn., this variable takes value 1 or .*/
/*you can change . into 0, but this may take further action*/
, dedup_set_ds=0 /*first dedup set_ds*/
, missing_2_0=1 /*want to change missing (i.e., .) into 0*/
/*doing so will takes more time, so typically do not recommend so*/
)
/*first run genMoreRelaxedCohort_06252012*/
/*now we need an indicator for first_only_PCA because we may change our denominator to firstOnlyCancer*/


%_vins(
   _grabRegistry_robotimrtInd /*ds can be one or many data*/
, regcase
, pcaot.observationdx0407 /*only vn is used, the data should be unique, if not then dedup is going on*/
, _robotimrtObsInd
, observation /*can be left empty, if so, it will be called _MAC_vins_ind_&vn., this variable takes value 1 or .*/
/*you can change . into 0, but this may take further action*/
, dedup_set_ds=0 /*first dedup set_ds*/
, missing_2_0=1 /*want to change missing (i.e., .) into 0*/
/*doing so will takes more time, so typically do not recommend so*/
)


data pcaot._firstOnlyDx20042007;
set psfpr09.firstonlypca_dx_year2004 psfpr09.firstonlypca_dx_year2005 psfpr09.firstonlypca_dx_year2006
psfpr09.firstonlypca_dx_year2007;
keep regcase;
run;

/*add an indicator of first_only_Cancer*/
%_vins(
   _robotimrtObsInd /*ds can be one or many data*/
, regcase
, pcaot._firstOnlyDx20042007 /*only vn is used, the data should be unique, if not then dedup is going on*/
, _robotimrtObsIndOnlyPca
, firstOnlyPCa /*can be left empty, if so, it will be called _MAC_vins_ind_&vn., this variable takes value 1 or .*/
/*you can change . into 0, but this may take further action*/
, dedup_set_ds=0 /*first dedup set_ds*/
, missing_2_0=1 /*want to change missing (i.e., .) into 0*/
/*doing so will takes more time, so typically do not recommend so*/
)

/*grab an mets indicator, which is geneated in getDemoTable.sas*/ 
data _pcadx20042007Mets;
set pcaot.pcadx20042007Mets (keep=regcase _metsInd);run;
%_joz(_robotimrtObsIndOnlyPca, _pcadx20042007Mets,_addMets,regcase)

%dvn(pcaot.pcadx20042007Mets)

%kdv( 
     _addMets
 , _first_dx_day _first_cancer_site REG1
REG2 REG3 REG4 REG5 REG6 REG7 REG8 REG9 REG10 _psaPedsfRawRecord _gleason _csexStg _psaLevel MED_DODM MED_DODD MED_DODY
 , pcaot.cancerPatientDs_07052012
 ,0
)


%genxpt(pcaot.cancerPatientDs_07052012)


proc freq data=pcaot.cancerPatientDs_07052012;table _first_dx_year;run;

proc freq data=pcaot.cancerPatientDs_07052012;table  _damicoRisk;run;


%_vt(pcaot.cancerPatientDs_07052012)

/*add an indicator of mets make sure added here, I need to see How I grab stage and grade age at dx, ses..last time...may be not here tro following what I did.*/
....make sure this is completed before go to R!!
%dvn(pcaot.cancerPatientDs_07052012)



proc freq data=pcaot.cancerPatientDs_07052012;
table _damicoRisk _damicoT2NOSIndicator;run;

%_vt(pcaot.cancerPatientDs_07052012)
%genxpt(pcaot.cancerPatientDs_july52012)
%pathname(pcaot)
/* Z:\j_scrdata\pcaOverTreatment*/
%_vt(scrimr.hasImrtEnt1212Dx0107)
