/*libname pcaot 'Z:/j_scrdata/pcaOverTreatment';*/
/*find first imrt and first prostectomy within 1 year dx*/
/*hcpcs codes for different treatments and allTrtCodes are just combination of all types of codes*/
%let surgeryCodes=55810  55812  55815  55840  55842  55845;
%let robotCodes=55866;

%let radiaNotImrtCodes=
/*External beam radiation (includes 2D and 3D-CRT): */
77401  77402  77403  77404  77406  77407  77408  77409  77411  77412  77413  77414  77416  
/*SBRT: */
77373  77421  77435
/*Neutron beam therapy: */
77422  77423
/*Brachytherapy:  */
55859  55860  55862  55865  55875  76873  76965  77750  77761  77762  77763  77776  77777  77778  77781  77782  77783  
77784  77785  77786  77787  77789  77790  77799  C1164  C1174  C1325  C1350  C1700  C1701  C1702  C1703  C1704  C1705  
C1706  C1707  C1708  C1709  C1710  C1711  C1712  C1715  C1716  C1717  C1718  C1719  C1720  C1728  C1790  C1791  C1792  
C1793  C1794  C1795  C1796  C1797  C1798  C1799  C1800  C1801  C1802  C1803  C1804  C1805 C1806  C2632  C2633  C2634  
C2635  C2636  C2638  C2639  C2640  C2641  C2642  C2643  G0256  G0261  Q3001       
/*Proton beam therapy: */
77520  77522  77523  77525
;
%let imrtCodes=G0174  77418  0073T;
%let hormonesCodes=
/*Androgen deprivation therapy and Orchiectomy:*/
54520  54522  54530  54535
/*Androgen deprivation injectable:*/
11980  96400 C9430  C9216  J0128  J0970  J1000  J1056  J1380  J1390  J1410  J1675  J1950  J3315  J9165  J9202  J9217  
J9218  J9219  J9225 J9226 Q2020  S0165  S0133  S9560 90782 G0356  J9155
/*Androgen deprivation implant:*/
11980  11981  11982  11983
;
%let cryoCodes=55873 G0160;
%let allTrtCodes=&surgeryCodes. &radiaNotImrtCodes. &imrtCodes. &hormonesCodes. &cryocodes.;
 

%incsas(P:\SEER\genCohort)
data _onlyPCa_dxDate19922007;
set %_paste(psfpr09.firstOnlyPca_dx_year|%_seq(1992,2007));
keep regcase _first_dx_month _first_dx_day _first_dx_year;
run;
%_vt(_onlyPCa_dxDate19922007)
%_clnlog
%_anyCode_timeWinSinceDx(
  %_paste(prorawnh.nh|%_seq(2004,2008))  /*eg. nch or op, or medp*/
, hcpcs /*for nch and op it is hcpcs, for medp it is icdcode..*/
, &imrtCodes.
, from_dtm from_dtd from_dty
, _onlyPCa_dxDate19922007
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*>lb, eg lb=0 */
, 365 /*<=ub, eg, lb=365*/
, _has_imrt
, %_paste(pcaot.imrtNh|%_seq(2004,2008)) /*for no-record it is a one data*/
, 
, regcase
)

%_anyCode_timeWinSinceDx(
  %_paste(prorawop.op|%_seq(2004,2008))  /*eg. nch or op, or medp*/
, hcpcs /*for nch and op it is hcpcs, for medp it is icdcode..*/
, &imrtCodes.
, from_dtm from_dtd from_dty
, _onlyPCa_dxDate19922007
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*>lb, eg lb=0 */
, 365 /*<=ub, eg, lb=365*/
, _has_imrt
, %_paste(pcaot.imrtOp|%_seq(2004,2008)) /*for no-record it is a one data*/
, 
, regcase
)

/*has any robot*/
%_anyCode_timeWinSinceDx(
  %_paste(prorawnh.nh|%_seq(2004,2008))  /*eg. nch or op, or medp*/
, hcpcs /*for nch and op it is hcpcs, for medp it is icdcode..*/
, &robotCodes.
, from_dtm from_dtd from_dty
, _onlyPCa_dxDate19922007
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*>lb, eg lb=0 */
, 365 /*<=ub, eg, lb=365*/
, _has_robot
, %_paste(pcaot.robotNh|%_seq(2004,2008)) /*for no-record it is a one data*/
, 
, regcase
)

%_anyCode_timeWinSinceDx(
  %_paste(prorawop.op|%_seq(2004,2008))  /*eg. nch or op, or medp*/
, hcpcs /*for nch and op it is hcpcs, for medp it is icdcode..*/
, &robotCodes.
, from_dtm from_dtd from_dty
, _onlyPCa_dxDate19922007
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*>lb, eg lb=0 */
, 365 /*<=ub, eg, lb=365*/
, _has_robot
, %_paste(pcaot.robotOp|%_seq(2004,2008)) /*for no-record it is a one data*/
, 
, regcase
)

/*has any treatment */
%_anyCode_timeWinSinceDx(
  %_paste(prorawnh.nh|%_seq(2004,2008))  /*eg. nch or op, or medp*/
, hcpcs /*for nch and op it is hcpcs, for medp it is icdcode..*/
, &allTrtCodes.
, from_dtm from_dtd from_dty
, _onlyPCa_dxDate19922007
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*>lb, eg lb=0 */
, 365 /*<=ub, eg, lb=365*/
, _has_anyTrt
, %_paste(pcaot.anyTrtNh|%_seq(2004,2008)) /*for no-record it is a one data*/
, 
, regcase
)

%_anyCode_timeWinSinceDx(
  %_paste(prorawop.op|%_seq(2004,2008))  /*eg. nch or op, or medp*/
, hcpcs /*for nch and op it is hcpcs, for medp it is icdcode..*/
, &allTrtCodes.
, from_dtm from_dtd from_dty
, _onlyPCa_dxDate19922007
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*>lb, eg lb=0 */
, 365 /*<=ub, eg, lb=365*/
, _has_anyTrt
, %_paste(pcaot.anyTrtOp|%_seq(2004,2008)) /*for no-record it is a one data*/
, 
, regcase
)



/*stop here*/

/*next first day of imrt or robot*/

data _hasImrt;
set  %_paste(pcaot.imrtOp|%_seq(2004,2008))  %_paste(pcaot.imrtNh|%_seq(2004,2008)) ;
keep regcase from_date_num hcpcs;
run;


data _hasrobot;
set  %_paste(pcaot.robotOp|%_seq(2004,2008))  %_paste(pcaot.robotNh|%_seq(2004,2008)) ;
keep regcase from_date_num hcpcs;
run;
/*then based on first imrt day to check any prostatectomy between dx and first imrt*/
/**/




/*run the following for imrt*/

%_clnlog

%let exclusionCodes_imrt=&surgeryCodes.;
data _hasimrt_regcase; set _hasImrt; keep regcase; run;
%_joh(
   _hasImrt_regcase
,  
, %_paste(prorawop.op|%_seq(2004,2008))  %_paste(prorawnh.nh|%_seq(2004,2008))
, hcpcs from_dtm from_dtd from_dty
, regcase
, %_paste(imrt_op|%_seq(2004,2008))  %_paste(imrt_nh|%_seq(2004,2008))
,
)
data _allnchOP_hasimrt; set  %_paste(imrt_op|%_seq(2004,2008))  %_paste(imrt_nh|%_seq(2004,2008)); run;
%chmdy2nummdy(_allnchOP_hasimrt
, from_dtm from_dtd from_dty > from_date_num /*e.g., from_dtm from_dtd from_dty>from_date_num|thru_dtm thru_dtd thru_dty>thru_date_num*/
/*or, from_date_ch>from_date_num if from_date_ch*/
, _allnchOP_hasimrt_1)

data firstpcaDxDate; set psfpr09.firstpca_dx_date; keep regcase _firstdxdate_num; run;
%_firstLastVnWithinId_L2W(
 indsn=_hasimrt
, id=regcase
, rankbasedonVn=from_date_num /*first last is based on this Variable*/
, vars= from_date_num /*can include rankBasedOnVn..., all vars should numeric*/
, outdsn=tmpout_imrt
, firstLastSuffixes=F L
, nonsort=0 /*nonsort=0 is faster*/
)
data tmpout_imrt_F; set tmpout_imrt; trt_date_first=from_date_num_F; keep regcase trt_date_first; run;
%_joz(tmpout_imrt_F, firstpcaDxDate, dx_firstTrt_date_ds , regcase)
data dx_firstTrt_date_ds_1; set dx_firstTrt_date_ds; keep regcase trt_date_first _firstdxdate_num; run;

%_hasProcedureBetweenDates(
   _allnchOP_hasimrt_1
/*has three variables patient_id claim_code (e.g., hcpcs) claim_date*/
, dx_firstTrt_date_ds_1 
/*has three varaibles: patient_id start_date end_date*/
/*this is one data, each individaul */
, regcase /*regcase*/
, from_date_num /*in claims_manydata*/
, _firstdxdate_num /*in startDate_endDate_onedata*/
, trt_date_first /*in startDate_endDate_onedata*/
, hcpcs /*the variable name for code, e.g., hcpcs in nch and op file*/
, &exclusionCodes_imrt. /*a list of codes that we are looking for*/
, _hasExclusionCode /*if empty then it is _hasBetweenDates*/
, _regcase_Exclude_imrt
)

%_duzu(_hasimrt_regcase,pcaot._uniqueHasImrt,)
%_duzu(_regcase_Exclude_imrt,pcaot._unique_regcase_Exclude_imrt,)
/*in robot not in need to exclude*/
%_jom(
  pcaot._uniqueHasImrt pcaot._unique_regcase_Exclude_imrt scrimr.prost_bf12after12_dx0107  
, regcase
, pcaot.imrt_notSrgBfTrt
, 1 0 1 
)



%dnobs(pcaot._uniqueHasImrt pcaot._unique_regcase_Exclude_imrt scrimr.prost_bf12after12_dx0107 pcaot.imrt_notSrgBfTrt)


/*-----------------------------------------*/
/**/
/*pcaot._uniqueHasImrt has 21913 observations.*/
/*pcaot._unique_regcase_Exclude_imrt has 577 observations.*/
/*scrimr.prost_bf12after12_dx0107 has 125299 observations.*/
/*pcaot.imrt_notSrgBfTrt has 17444 observations.*/
/**/
/*----------------------------------------*/

/*run the following for robot*/

%_clnlog

%let exclusionCodes_robot=&radiaNotImrtCodes. &imrtcodes.;
data _hasrobot_regcase; set _hasrobot; keep regcase; run;

%_joh(
   _hasrobot_regcase
,  
, %_paste(prorawop.op|%_seq(2004,2008))  %_paste(prorawnh.nh|%_seq(2004,2008))
, hcpcs from_dtm from_dtd from_dty
, regcase
, %_paste(robot_op|%_seq(2004,2008))  %_paste(robot_nh|%_seq(2004,2008))
,
)
data _allnchOP_hasrobot; set  %_paste(robot_op|%_seq(2004,2008))  %_paste(robot_nh|%_seq(2004,2008)); run;
%chmdy2nummdy(_allnchOP_hasrobot
, from_dtm from_dtd from_dty > from_date_num /*e.g., from_dtm from_dtd from_dty>from_date_num|thru_dtm thru_dtd thru_dty>thru_date_num*/
/*or, from_date_ch>from_date_num if from_date_ch*/
, _allnchOP_hasrobot_1)

data firstpcaDxDate; set psfpr09.firstpca_dx_date; keep regcase _firstdxdate_num; run;
%_firstLastVnWithinId_L2W(
 indsn=_hasrobot
, id=regcase
, rankbasedonVn=from_date_num /*first last is based on this Variable*/
, vars= from_date_num /*can include rankBasedOnVn..., all vars should numeric*/
, outdsn=tmpout_robot
, firstLastSuffixes=F L
, nonsort=0 /*nonsort=0 is faster*/
)
data tmpout_robot_F; set tmpout_robot; trt_date_first=from_date_num_F; keep regcase trt_date_first; run;
%_joz(tmpout_robot_F, firstpcaDxDate, dx_firstTrt_date_ds , regcase)
data dx_firstTrt_date_ds_1; set dx_firstTrt_date_ds; keep regcase trt_date_first _firstdxdate_num; run;

%_hasProcedureBetweenDates(
   _allnchOP_hasrobot_1
/*has three variables patient_id claim_code (e.g., hcpcs) claim_date*/
, dx_firstTrt_date_ds_1 
/*has three varaibles: patient_id start_date end_date*/
/*this is one data, each individaul */
, regcase /*regcase*/
, from_date_num /*in claims_manydata*/
, _firstdxdate_num /*in startDate_endDate_onedata*/
, trt_date_first /*in startDate_endDate_onedata*/
, hcpcs /*the variable name for code, e.g., hcpcs in nch and op file*/
, &exclusionCodes_robot. /*a list of codes that we are looking for*/
, _hasExclusionCode /*if empty then it is _hasBetweenDates*/
, _regcase_Exclude_robot
)

%_duzu(_hasrobot_regcase,pcaot._uniqueHasRobot,)
%_duzu(_regcase_Exclude_robot,pcaot._unique_regcase_Exclude_robot,)



/*in robot not in need to exclude*/
%_jom(
  pcaot._uniqueHasRobot pcaot._unique_regcase_Exclude_robot scrimr.prost_bf12after12_dx0107
, regcase
, pcaot.robot_notRadiaBfTrt
, 1 0 1 
)

%dnobs(pcaot._uniqueHasRobot pcaot._unique_regcase_Exclude_robot scrimr.prost_bf12after12_dx0107 pcaot.robot_notRadiaBfTrt)
/**/
/*-----------------------------------------*/
/**/
/*pcaot._uniqueHasRobot has 5144 observations.*/
/*pcaot._unique_regcase_Exclude_robot has 80 observations.*/
/*scrimr.prost_bf12after12_dx0107 has 125299 observations.*/
/*pcaot.robot_notRadiaBfTrt has 3494 observations.*/
/**/
/*----------------------------------------*/

/*now I need to extract observatrional patients*/
%kdv(%_paste(pcaot.anyTrtNh|%_seq(2004,2008)) %_paste(pcaot.anyTrtOp|%_seq(2004,2008))
, regcase /*use %str(|) to separate */
, %_paste(tmpregcase|%_seq(10)) /*if outdsn_list is empty then operation is done on indsn_list*/
, 1 /*can supply just one element, if so, that element will be repeated if keep then 1, if drop then 0*/
)
data allreg;
set %_paste(tmpregcase|%_seq(10));
run;
%_duzu(allreg,_allUniqueTrtReg,regcase)
/*data _bf12after12_dx0107;*/
/*set scrimr.prost_bf12after12_dx0107;*/
/*keep regcase;*/
/*run;*/
/*firstPcadx20042007, entitlement prost_bf12after12_dx0107*/
%_jom(
   _allUniqueTrtReg scrimr.prost_bf12after12_dx0107
, regcase 
, pcaot.observationdx0407
,     0    1    /*in all three input datasets  | is used to separating between two selection rules*/   
)

%_bingvs(pcaot.observationdx0407 pcaot.robot_notRadiaBfTrt pcaot.imrt_notSrgBfTrt
, bingvs_ds /*if empty then the default output name is _bingvs_ds*/
, regcase
, 1  /*default is 1, ie. want to know hwo value beong to which data*/
/*if you want to understanding how each vars value belong to datasets then put 1, if you just want union of varsiable values then put 0*/
)


data troubleCases;
set bingvs_ds;
if _in_ds1+_in_ds2+_in_ds3>1;
run;
/*there is no observation in trouble cases., so the three cohorts are mututlaly exclusive*/

%dnobs(pcaot.observationdx0407 pcaot.robot_notRadiaBfTrt pcaot.imrt_notSrgBfTrt)
/*%genxpt(pcaot.observationdx0407 pcaot.robot_notRadiaBfTrt pcaot.imrt_notSrgBfTrt)*/

