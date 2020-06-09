%_libyz(overtrtR,Z:\j_scrdata\pcaOverTreatmentRevision)
%_libyz(sr12,Z:\j_rawdata\seer12\pedsf\prostate)

/********************get the variable that will be used for demo table*********/
%let demoPedsfVars=
patient_id 
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
/*%_paste(cs1st|%_seq(10))  */
/*probably psa score should be divided by 10*/
/*gleason score*/
/*%_paste(cs6st|%_seq(10)) */
/*%_paste(csex|%_seq(10))*/
;

data overtrtR._pesfRaw;
set sr12.pedsfprostate(keep=&demoPedsfVars.);
run;
%rnvn(overtrtR._pesfRaw,patient_id,regcase)




/*****************get first only concer*************/
data siteinfo;
set sr12.pedsfprostate(keep=patient_id %_seqsuf(site,10));
run;
/*fistOnly is  a stupid term, it just means only*/
data overtrtR.pca_firstOnlyCancer;
set siteinfo;
if site1='619' and site2='';
keep patient_id;
run;
/*first cancer dx date*/
%_firstcancer_dx_date(
 sr12.pedsfprostate
,619
/*e.g., 679 678 677 676 675 674 673 672 671 670*/
/*e.g., 619*/
,patient_id
,overtrtR.pcaDx
, /*if empty then it is _first_dx_month*/
,  /*if empty then it is _first_dx_day*/
,  /*if empty then it is _first_dx_year*/
,  /*if empty then it is  _dxdate_num*/
)

%_joz(overtrtR.pca_firstOnlyCancer,overtrtR.pcaDx,overtrtR.firstOnlypcaDxInfo,patient_id)
proc freq data=overtrtR.firstOnlypcaDxInfo;table _first_dx_year;run;

/**********GET cOHORT BASED ON ENTITLEMENT*********************/

data pedsfin;
set pedx9209.pedsfprostate(
keep=patient_id med_dody med_dodm birthm birthyr %_seqsuf(site,10) %_seqsuf(hist,10) %_seqsuf(yrdx,10) %_seqsuf(modx,10) %_seqsuf(src,10) %_seqsuf(gho,252)  %_seqsuf(mon,252)
)
;
%_clnlog
%getcohort_march132013(
		  input_pedsf=pedsfin
		, output_cohort_dsn=overtrtR.prost_bf12after12_dx9209
		, cancer_codes=619 
		, hist_codes=8140 8550
		, chkNmonth_diesincedx=6   
		, chkNmonth_medABbfdx=12                          
		, chkNmonth_medABafdx=12  /*you need to specify if you want 12 after*/                     
		, chkNmonth_noHMObfdx=12                          
		, chkNmonth_noHMOafdx=12 /*you need to specify, if you want 12 after*/                          
		, age_lowerbd=780 /*65*12=780, inclusive*/     
		, age_upperbd=1199 /*100*12-1=1199, inclusive*/               
		, diag_start_yr_of_interest=1992                                  
		, diag_end_yr_of_interest=2009 
        , entStartYear=1991 /*previous it was 1986, but in 2012 data it is 1991*/
		, true_data_upbd_suffix=252
		, search_till_true_end=0
		, medAB_vn=mon
		, medAB_vn_type=C                                
		, hasmedAB_values=3
		, hmo_vn=gho
		, hmo_vn_type=C                                 
		, nohmo_values=0
		, idvn=patient_id
		, sample_size_track=1
		, sample_size_track_ds=overtrtR.cohort_restriction_details
		)

/*get firstonly cancer dx 2004 and on*/

data _firstOnly_dx20042009; set overtrtR.firstOnlypcaDxInfo;
if _first_dx_year >=2004 and _first_dx_year <=2009;
keep patient_id;
run;
%_bingvs(overtrtR.prost_bf12after12_dx9209 _firstOnly_dx20042009
, _in_what /*if empty then the default output name is _bingvs_ds*/
, patient_id /*based on these vars for unique combinations*/
, 1  /*default is 1, ie. want to know hwo value beong to which data*/
/*if you want to understanding how each vars value belong to datasets then put 1, if you just want union of varsiable values then put 0*/
)

data _bf12af12_firstOnly_dx0409;
set _in_what;
if _in_ds1=1 and _in_ds2=1;
keep patient_id;
run;

proc freq data=overtrtR.bf12af12_firstOnly_dx0409;table _first_dx_year;run;

%_joz(_bf12af12_firstOnly_dx0409,overtrtR.firstOnlypcaDxInfo,overtrtR.bf12af12_firstOnly_dx0409,patient_id)

data _patientId;
set overtrtR.bf12af12_firstOnly_dx0409;
keep patient_id;
run;

%rnvn(_patientId,patient_id,regcase)
%_clnlog
%_genSeerDemo_NoComorb(
  sr12.pedsfprostate
, seerSES.ses_score_fromBrent
/*as of April 20, 2011 seerSES is Z:\j_rawdata\seer_SES*/
/*data for with ses score*/
, geo_id2
/*zip5digit_vn */
/*right now it is geo_id2 in zipSesDs*/
, ses1
/*ses_vn */
/*right now it is ses1*/
, prostate 
/*what_cancer*/
/*e.g., prostate bladder*/
,patient_id
, %_paste(e10ex|%_seq(10))  /*%_paste(e10ex|%_seq(10)) */
,  %_paste(csex|%_seq(10)) /*%_paste(DAJCCT|%_seq(10)) now  %_paste(csex|%_seq(10)) */
,overtrtR.demo_noComorb_03162013
)


data tmpDxDate;
set overtrtR.firstOnlypcaDxInfo;
keep patient_id _firstdxdate_num;
run;
%rnvn(tmpDxDate,patient_id,regcase)
%_joz(_patientId,tmpDxDate,_regcaseDxDateNum,regcase)

/**/
/*%_clnlog*/
/*%_getseerComorbSrchClaims_joh(*/
/* _patientId*/
/*,regcase */
/*,%_paste(svr10sr.medpar|%substrlist(%_seq(2003,2009),3,2)) */
/*,%_paste(svr10sr.nch|%substrlist(%_seq(2003,2009),3,2)) */
/*,%_paste(svr10sr.outp|%substrlist(%_seq(2003,2009),3,2))*/
/*,overtrtR.comorbSrch*/
/*)*/


%_seer_comorb_seanzhang_final(
  _regcaseDxDateNum /*regcase and event date*/
, overtrtR.comorbSrch /*SY file is fine, no SY is also good*/
, noSY
/*noSY*/
/*can also be SY, if so _osym will be called, noSY then use _joh*/
,_firstdxdate_num /*numeric sas date*/
,overtrtR.comorbVar
,_claim_date_comorb /*usually this is _claim_date_comorbv if you use _getSeerComorbSrchClaims_joh*/
/*,START*/
/*,FINISH*/
,365 /*usually 365, 365 days before event date, event_date-365*/
,%_paste(_dx_comorb|%_seq(10))
/*you can NOT use _dx_comorb1-_dx_comorb10*/
/*because internall there is a word count*/
/*use %_paste(_dx_comorb|%_seq(10))*/
,%_paste(srgcde|%_seq(10))  
/* use %_paste(srgcde|%_seq(10))*/
/*do NOT use srgcde1-srgcde10 due to internal count*/
,HCPCS
,los /*los vn in medpar*/
,FILETYPE /*FileType is variable name. this var has three values: M=Medpar, O=Outpatient,N=NCH*/
,30 /*if empty then all would be Prior claimsS, this is usually set as 30*/
)

/***********************need to remove mets***************************/

data _dajccm; set sr12.pedsfprostate (keep=patient_id %_paste(dajccm|%_seq(10))); run;
%rnvn(_dajccm,patient_id,regcase)
data dx_2004_2009; set overtrtR.firstOnlypcaDxInfo;
if _first_dx_year >=2004 and _first_dx_year <=2009;
regcase=patient_id;
keep regcase _first_cancer_site _first_dx_year;
run;

/*all diagnosed between 2004 and 2007*/
%_joh(
  dx_2004_2009 
, 
, _dajccm
, regcase %_paste(dajccm|%_seq(10))
, regcase
, _dxDajccm
, 0.8
)
%_genMets_dajccm(
   _dxDajccm
/*regcase _first_cancer_site _first_dx_year dajccm1-10 (2004+)*/
, overtrtR.pcadx20042009Mets
, varsLists=%_paste(dajccm|%_seq(10))
/*%_paste(dajcct|%_seq(10))*/
, locationVn=_first_cancer_site
, maxcol=10 /*there are 10 sites*/
, metsVn=_metsInd
, mxAsMissing=0 /*Mx means mets has not beeen formally tested based on imaging test, usually this indicates noMets, so default is 0*/
)

/*remove these with missing mets and mets 1*/
data overTrtR.noMets_FirstOnly_dx0409;
set overtrtR.pcadx20042009Mets;
if _metsInd=0;
keep regcase;
run;


/**************************get Damico********************/
%_clnlog
%liuv(sr12.pedsfprostate
,
_forDamico
,  
patient_id
/*PSA score*/
%_paste(cs1st|%_seq(10))  
/*probably psa score should be divided by 10*/
/*gleason score*/
%_paste(cs6st|%_seq(10)) 
%_paste(cs8st|%_seq(10)) 
%_paste(csex|%_seq(10))
)

%_varexist( overtrtR.firstOnlypcaDxInfo,regcase patient_id)
%_jotsev(
  _forDamico
,  
, overtrtR.firstOnlypcaDxInfo
,
/*if empty, then it is all the variables other than keys*/
, _pedsfForDamico
, patient_id 
/*if different, fid pid | family_id patient_id */
/*if the same, fid pid*/
, inner
)

data chk;
set  _pedsfForDamico;
if _first_dx_year>=2004;run;


%_genDAmicoPcaRiskCat(
   _pedsfForDamico
/*regcase _first_cancer_site cs1st1-cs1st10 cs6st1-cs6st10 dajcct1-dajcct10*/
, _TMPDamico
, varsLists=%_paste(cs1st|%_seq(10)) | %_paste(cs6st|%_seq(10)) | %_paste(csex|%_seq(10))
, locationVn=_first_cancer_site
, maxcol=10
)

/*note damico only apply to 2004 and on*/
data overtrtR.damico; set  _tmpDamico(keep=patient_id _damicoRisk _first_dx_year _gleason
/*_dajcctStg*/
_clinicalStg
_psaLevel _psaPedsfRawRecord _damicoT2NOSIndicator); 
if _first_dx_year>=2004;
run;

%_vt(overtrtR.damico)
/*need to get treatment profile*/


proc freq data=overtrtR.damico;table _damicorisk;run;


/*now join append peds*/
/*first only cancer*/
%_joz(overtrtr.bf12af12_firstOnly_dx0409,overtrtR._pesfRaw)

data ok_id;
set overtrtr.bf12af12_firstOnly_dx0409; 
regcase =patient_id;
keep regcase;
run;

%dvn(overtrtR.damico)
data _damico;
set overtrtR.damico;
regcase=patient_id;
keep regcase _damicoRisk _damicoT2NOSIndicator;
run; 

data  _mets;
set overtrtR.pcadx20042009Mets;
keep regcase _metsInd;
run;
data _chrlson;
set 
overtrtR.comorbVar;
keep regcase chrlson;
run;

data _noncomorb;
set overtrtR.demo_noComorb_03162013;
regcase=patient_id;
keep regcase _tStage_raw _tStage_grp_detailed _tStage_grp_simple _dxage _grade _ses BIRTHM BIRTHYR
M_SEX RACE STATE COUNTY ZIP URBRUR URBAN CODPUB _first_dx_day  _first_dx_month _first_dx_year
_firstdxdate_num
;
run;

data registry;
set sr12.pedsfprostate(keep=patient_id reg1);
run;
%rnvn(registry,patient_id reg1, regcase registry)


%_jozol(
  ok_id
, _damico _mets  _chrlson _noncomorb registry
, regcase  /*this will be recyled to the length of rightDslist*/
/*separated by |*/
/*can have multiple keys*/
, updateLeft=1 
/*default would update left*/
/*type 1: input leftDs is the same*/
/*updateLeft=0*/
/*type 2: input leftDs is updated*/
/*updateLeft=1*/
, keysSep=%str(|)
, outdsKeyWord=_demoSamp
)

proc freq data=_demoSamp_4;table reg1;run;



%compare2dataVn(
  _demoSamp_5
, pcaot.cancerPatientDs_07052012
, inboth=_tmp_inboth_MACCompare2dataVN
, in1not2=_tmp_in1not2_MACCompare2dataVN
, in2not1=_tmp_in2not1_MACCompare2dataVN
)

%_vt(_tmp_in2not1_MACCompare2dataVN)
data pcaot.cancerPatientDs_03222013;
set _demoSamp_5;
_gradeCat='restType';
if _grade in ('1','2') then _gradeCat='wellmodDiff';
if _grade in ('3','4') then _gradeCat='poorUndiff';
;
run;
%genxpt(pcaot.cancerPatientDs_03222013)









