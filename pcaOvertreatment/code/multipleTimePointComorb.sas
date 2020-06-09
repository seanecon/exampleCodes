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

%_ranhan(
 seer09nc.sumdenomnc_hicbic
, hicbic_sample
, 100 /*sample size*/
,
)

%_osym(
  hicbic_sample
/*  elements in subsetdsnList are separated by space */
  /*one data, if there are more than one master data and subsetDsnlist length is shorter than master data List*/
/*then this subsetdsnlist will be recycled to the length of symasterdsnlist*/
, 
/*if empty then all vars in the corresponding subsetdsn will be kept*/
/*a list of subsetDsKeptVars separated by delim, default delim is %str(|)*/
/*subsetDsKeptVars should include variables used to create sy_name*/
/*the variables used for creating the index (i.e., sy_name) should be included*/
/*for example, if a and b are used for creating sy_name then a and b should be included here*/
,  %_paste(seer09nc.|nchnc|%substrList(%_seq(1991,2009),3,2)) /*symasterdsn is indexed many data*/
/*symasterdsns are separated by space*/
, 
/*if empty then all vars in correspoinding symasterDsKeptvarsList will be kept*/
/*a list of symasterDsKeptvars separated by delim, default delim is %str(|)*/
, hicbic /*index name, if its length is less than symasterdsn then it will be recycled*, the list separated by space*/
, %_paste(nchnc|%substrList(%_seq(1991,2009),3,2))
/*output dsns are separated by space*/
, delim=%str(|))

%incsas(P:\SEER\genComorb)

%_getseerComorbSrchClaims_osym(
  hicbic_sample
,hicbic /*this dataset contains paitent id*/
, %_paste(seer09nc.|medparnc|%substrList(%_seq(1991,2009),3,2)) /*sy medpfiles*/
, %_paste(seer09nc.|nchnc|%substrList(%_seq(1991,2009),3,2)) /*sy nch files*/
, %_paste(seer09nc.|outpnc|%substrList(%_seq(1991,2009),3,2)) /*sy op files*/
,out_comorb_srch_file
)


%_syd(
  out_comorb_srch_file
, hicbic /*a list of rules separated by index_rule_sep (e.g., #)*/
/*go to _syd demo to see details*/
, index_rule_sep=%str(#)
)

data seer09nc.out_comorb_srch_file;
set out_comorb_srch_file;
run;

%_vt(out_comorb_srch_file)

data eventdate_ds;
set hicbic_sample;
eventDate_vn=14860;
run;

%_seerComorbSeanZhang(
 eventdate_ds /*id and numeric event_date*/
,out_comorb_srch_file
,hicbic /*regcase or hicbic*/ 
/*ideally this is sydata, if so you need to change _joh into _osym inside this macro*/
/*Suo ying file*/
,SY
/*either noSY or SY*/
/*case sensitive*/
/*if noSY, internall it runs _joh*/
/*if SY, internally it run _osym*/
,eventdate_vn /*numeric sas date*/
,comorb_outdsn
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
,losvn /*los vn in medpar*/
,FILETYPE /*M=Medpar, O=Outpatient,N=NCH*/
,30 /*if empty then all would be Prior claimsS, this is usually set as 30*/
)

%_vt(WORK.COMORB_OUTDSN)
