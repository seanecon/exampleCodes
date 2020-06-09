/*the macro would generate multiple time point comorbdity for a person*/
/*first get a comorb search file*/
/*then apply the comorb macro*/
/*the trick is to allow for multiple time points*/

/*claims up to 2009*/
libname seer09nc 'Z:\j_rawdata\seer09\nonCancer';

%_kbds(%_paste(svr09sr.|medparnc|%substrList(%_seq(1991,2009),3,2))
, %_paste(seer09nc.|medparnc|%substrList(%_seq(1991,2009),3,2))
, Auxfolderpath=C:/temp, replace=T)

%_kbds(%_paste(svr09sr.|nchnc|%substrList(%_seq(1991,2009),3,2))
, %_paste(seer09nc.|nchnc|%substrList(%_seq(1991,2009),3,2))
, Auxfolderpath=C:/temp, replace=T)

%_kbds(%_paste(svr09sr.|outpnc|%substrList(%_seq(1991,2009),3,2))
, %_paste(seer09nc.|outpnc|%substrList(%_seq(1991,2009),3,2))
, Auxfolderpath=C:/temp, replace=T)


/*next we will syd the data*/


%_syd(
%_paste(seer09nc.|nchnc|%substrList(%_seq(1991,2009),3,2)) /*a list of dataset separated by space*/
, hicbic /*a list of rules separated by index_rule_sep (e.g., #)*/
/*go to _syd demo to see details*/
, index_rule_sep=%str(#)
)

%_syd(
%_paste(seer09nc.|outpnc|%substrList(%_seq(1991,2009),3,2)) /*a list of dataset separated by space*/
, hicbic /*a list of rules separated by index_rule_sep (e.g., #)*/
/*go to _syd demo to see details*/
, index_rule_sep=%str(#)
)

%_syd(
%_paste(seer09nc.|medparnc|%substrList(%_seq(1991,2009),3,2)) /*a list of dataset separated by space*/
, hicbic /*a list of rules separated by index_rule_sep (e.g., #)*/
/*go to _syd demo to see details*/
, index_rule_sep=%str(#)
)

%_kbds(svr09sr.sumdenomnc
, seer09nc.sumdenomnc
, Auxfolderpath=C:/temp, replace=T)
%rnvn(seer09nc.sumdenomnc,shufhic,hicbic)
data seer09nc.sumdenomnc_hicbic;
set  seer09nc.sumdenomnc(keep=hicbic);
run;


libname pcaot 'Z:/j_scrdata/pcaOverTreatment';
PROC IMPORT OUT=tmpIndexTime 
            DATAFILE= "Z:/j_scrdata/pcaOverTreatment/randomIndexTime.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data tmpindextime_1;
set tmpIndexTime;
length indexday $ 2;
indexday='01';
indexyear_1=put(indexyear,4.);
drop indexyear;
rename indexyear_1=indexyear;
indexmonth_1=put(indexmonth,z2.);
drop indexmonth;
rename indexmonth_1=indexmonth;
run;
%chmdy2nummdy(tmpindextime_1
, indexmonth indexday indexyear> indexdate_num /*e.g., from_dtm from_dtd from_dty>from_date_num|thru_dtm thru_dtd thru_dty>thru_date_num*/
/*or, from_date_ch>from_date_num if from_date_ch*/
, pcaot.ncindextime
)

%_ranhan(
  pcaot.ncindextime
, pcaot.hicbic_test10k
, 10000 /*sample size*/
,hicbic indexmonth indexday indexyear indexdate_num
,88
)



/*get these people sumdenom file for demographic data*/

%let sumnc_demogVars=
hicbic birthm birthyr birthd /*used by macro _firstcancer_dx_date*/
/*M_sex */
/*pedsf 1 is male*/
sex /*sumdenomnc 1 is male*/
/*birthd */
/*only in sumdenomnc*/
race
/*1 white 2 black 3 is other, 4 is asian, 5 is hispanic, 6 native american, 0 is unknown, we probably will be excluding these with unknown race*/
/*urbrur*/
/*urban*/
/*state */
/*county*/
/*zip	*/
/*geo*/
/* codpub*/
/*codpubf*/
 /*cause of death*/
/*in new data 2011, codpub is changed into codpubf*/
med_dodm med_dodd med_dody /*in sum denom and pedsf are the same*/


/*geographic location variable in sumdeno is a bit intersting..*/
%_paste(urban|%_seq(1986,2009))
%_paste(st|%_seq(1986,2009))
%_paste(cnty|%_seq(1986,2009))
%_paste(zip|%_seq(1986,2009))
%_paste(registry|%_seq(1986,2009))
/*first registory found and is retained until the patient moved into a new registry*/
%_paste(reg2cd|%_seq(1986,2009))
/*registroy variable based on state and country, the last residence the patient lived at for that year*/
;

%_joz(pcaot.hicbic_test10k, seer09nc.sumdenomnc ,  _sumnc_demoRaw_tmp, hicbic)
%liuv(_sumnc_demoRaw_tmp,pcaot.sumnc_demoRaw,&sumnc_demogVars.)

data pcaot._sumnc_demoRaw_tmp;
set _sumnc_demoRaw_tmp;
run;
data pcaot.sumnc_demoRaw;
set pcaot._sumnc_demoRaw_tmp(keep=&sumnc_demogVars. indexmonth indexyear);
run;

%genxpt(pcaot.sumnc_demoRaw)

%dvn(pcaot.hicbic_test10k)

%incsas(P:\SEER\genComorb)
%_getseerComorbSrchClaims_osym(
  pcaot.hicbic_test10k
,hicbic /*this dataset contains paitent id*/
, %_paste(seer09nc.|medparnc|%substrList(%_seq(1991,2009),3,2)) /*sy medpfiles*/
, %_paste(seer09nc.|nchnc|%substrList(%_seq(1991,2009),3,2)) /*sy nch files*/
, %_paste(seer09nc.|outpnc|%substrList(%_seq(1991,2009),3,2)) /*sy op files*/
,seer09nc.comorbSrchFile_test10k
)


%_syd(
  seer09nc.comorbSrchFile_test10k
, hicbic /*a list of rules separated by index_rule_sep (e.g., #)*/
/*go to _syd demo to see details*/
, index_rule_sep=%str(#)
)

%_osym(
  pcaot.ncindextime
, 
, seer09nc.comorbSrchFile_test10k
/*if empty then all vars in correspoinding symasterDsKeptvarsList will be kept*/
/*a list of symasterDsKeptvars separated by delim, default delim is %str(|)*/
, 
, hicbic /*index name, if its length is less than symasterdsn then it will be recycled*, the list separated by space*/
, pcaot.elig_hicbic_comorbsrch
/*output dsns are separated by space*/
, delim=%str(|)
)

%_joz(pcaot.hicbic_test10k,pcaot.ncindextime,eventdate_ds_tmp, hicbic)

data eventdate_ds;
set eventdate_ds_tmp;
keep hicbic indexdate_num;
run;

%_seerComorbSeanZhang(
 eventdate_ds /*id and numeric event_date*/
,seer09nc.comorbSrchFile_test10k
,hicbic /*regcase or hicbic*/ 
/*ideally this is sydata, if so you need to change _joh into _osym inside this macro*/
/*Suo ying file*/
,SY
/*either noSY or SY*/
/*case sensitive*/
/*if noSY, internall it runs _joh*/
/*if SY, internally it run _osym*/
,indexdate_num /*numeric sas date*/
, pcaot.comorb_test10k
,_claim_date_comorb /*usually this is _claim_date_comorb*/
/*,START*/
/*,FINISH*/
,365 /*usually 365, 365 days before event date, event_date-365*/
,%_paste(_dx_comorb|%_seq(10))
/*Variable names:  the diagnosis codes, ie 'DX01-DX10'*/
/*you can NOT use _dx_comorb1-_dx_comorb10*/
/*because internall there is a word count*/
/*use %_paste(_dx_comorb|%_seq(10))*/
, %_paste(srgcde|%_seq(10)) /*SURG01-SURG10*/
/* use %_paste(srgcde|%_seq(10))*/
/*do NOT use srgcde1-srgcde10 due to internal count*/
,HCPCS
,los /*los vn in medpar*/
,FILETYPE /*M=Medpar, O=Outpatient,N=NCH*/
,30 /*if empty then all would be Prior claimsS, this is usually set as 30*/
)

%_vt(pcaot.comorb_test10k)

proc freq data=pcaot.comorb_test10k;
table chrlson;run;


%genxpt(pcaot.comorb_test10k)
