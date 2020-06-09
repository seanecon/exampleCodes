/*use ALL the regcases in original pedsf (first and only PCA regcases are a subset of this original pedsf regcases) and genreate one single comorb search file*/

data _allpedsfregcase;
set svr09sr.pedsfprostate(keep=regcase);
run;

%incsas(P:\SEER\genComorb)

%_clnlog
/*the following chunk of code would take long hours to run*/
/*%_getSeerComorbSrchClaims_joh(*/
/*_allpedsfregcase */
/*,regcase */
/*,%_paste(svr09sr.medpar|%substrlist(%_seq(1992,2009),3,2)) */
/*,%_paste(svr09sr.nch|%substrlist(%_seq(1992,2009),3,2)) */
/*,%_paste(svr09sr.outp|%substrlist(%_seq(1992,2009),3,2))*/
/*,psfpr09.pca_comorbSrchFile*/
/*)*/

%_seer_comorb_seanzhang_final(
  psfpr09.allpca_dx_date /*regcase and event date*/
, psfpr09.pca_comorbSrchFile /*SY file is fine, no SY is also good*/
, noSY
/*noSY*/
/*can also be SY, if so _osym will be called, noSY then use _joh*/
,_firstdxdate_num /*event date: numeric sas date*/
, psfpr09.firstpca_comorb /*note, this is the comob calcuated at the  pca diagnosis date for all regcase*/
/*it is NOT firstOnlyPCA, firstOnlyPCA has less people. because it requires PCA is the only cancer*/
,_claim_date_comorb /*usually this is _claim_date_comorb in the search file*/
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



/*now create a dataset which has regcase diagnosis date*/

%_firstcancer_dx_date(
 svr09sr.pedsfprostate
,619 /*e.g., 679 678 677 676 675 674 673 672 671 670*/
,psfpr09.pca_dxDate_all/*first and only cancer is a subset of this data which contains every PCA patient*/
, /*if empty then it is _first_dx_month*/
, /*if empty then it is _first_dx_day*/
,  /*if empty then it is _first_dx_year*/
,   /*if empty then it is  _dxdate_num*/
)


