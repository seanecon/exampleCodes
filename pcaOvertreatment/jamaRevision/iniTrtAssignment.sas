
/*this has all the dx years*/
data input_dx_date_file;
set psfpr09.pca_dxdate_all(keep=_first_dx_month _first_dx_day _first_dx_year regcase);
run;
proc freq data=input_dx_date_file;table _first_dx_year;run;
/*                  _first_                             Cumulative    Cumulative*/
/*                  dx_year    Frequency     Percent     Frequency      Percent*/
/*                  ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ*/
/*                  1978              1        0.00             1         0.00*/
/*                  1983              1        0.00             2         0.00*/
/*                  1985              1        0.00             3         0.00*/
/*                  1986              2        0.00             5         0.00*/
/*                  1987              2        0.00             7         0.00*/
/*                  1988              2        0.00             9         0.00*/
/*                  1989              1        0.00            10         0.00*/
/*                  1990              5        0.00            15         0.00*/
/*                  1991              1        0.00            16         0.00*/
/*                  1992          28117        5.43         28133         5.43*/
/*                  1993          25354        4.89         53487        10.32*/
/*                  1994          22211        4.29         75698        14.61*/
/*                  1995          21055        4.06         96753        18.67*/
/*                  1996          21171        4.09        117924        22.76*/
/*                  1997          22197        4.28        140121        27.04*/
/*                  1998          22028        4.25        162149        31.29*/
/*                  1999          23650        4.56        185799        35.86*/
/*                  2000          44936        8.67        230735        44.53*/
/*                  2001          45829        8.84        276564        53.37*/
/*                  2002          46140        8.90        322704        62.27*/
/*                  2003          41331        7.98        364035        70.25*/
/*                  2004          40328        7.78        404363        78.03*/
/*                  2005          36770        7.10        441133        85.13*/
/*                  2006          38533        7.44        479666        92.57*/
/*                  2007          38527        7.43        518193       100.00*/

data input_dx_date_file;
set overtrtr.bf12af12_firstOnly_dx0409(keep=patient_id _first_dx_month _first_dx_day _first_dx_year);
run;
%rnvn(input_dx_date_file, patient_id, regcase)
%dnobs(overtrtr.bf12af12_firstOnly_dx0409)
%genxpt(overtrtr.bf12af12_firstOnly_dx0409)
%_clnlog
%_claimsTimeWinSinceDx(
  %_paste(svr10sr.outp|%substrlist(%_seq(2004,2010),3,2))   %_paste(svr10sr.nch|%substrlist(%_seq(2004,2010),3,2)) /*eg. nch or op, or medp*/
, %_paste(overTrtr.outpCpt|%substrlist(%_seq(2004,2010),3,2))   %_paste(overTrtr.nchCpt|%substrlist(%_seq(2004,2010),3,2))
, from_dtm from_dtd from_dty  /*from_dtm from_dtd from_dty and the like*/
, hcpcs /*vars to be kept other than claim dates and patient_id, usually you need hcpcs*/
, input_dx_date_file
, _first_dx_month _first_dx_day _first_dx_year /*order mattern  month day year, pedsf_file does not have date, you need to add it yourself*/
, 0 /*timelb since dx   >lb, eg lb=0 */
, 365 /*timeub since dx   <=ub, eg, lb=365*/
/*if this empty only &patient_id. &claims_proc_date_vns. &code_vn. are kept*/
,regcase /*regcase usually*/
)

%_clnlog
data overtrtr.opNch_trtProfile;
set  %_paste(overTrtr.outpCpt|%substrlist(%_seq(2004,2010),3,2))  %_paste(overTrtr.nchCpt|%substrlist(%_seq(2004,2010),3,2));
run;

%_duzu(overtrtr.opNch_trtProfile,overtrtr.opNch_trtProfile,)

%_sortyz(
  inds=overtrtr.opNch_trtProfile
, outds=overtrtr.opNch_trtProfile /*if empty then there is no out=outds*/
, keys=regcase _proc_time_since_dx 
, descendKeys= /*the key that should be descending, make sure descendingKeys are included in keys!*/
, inds_parentheses=  /*inds(outds_parentheses)*/
, outds_parentheses=%str(keep=regcase hcpcs _proc_time_since_dx) /*outds(outds_parentheses)*/
, nodupType= /*three types: keys or record or leave it blank*/
/*if keys only makes sure no dup keys, record would make sure no dup record (every variable will be compared, more intensive)*/
/*if blank, there there is no dedup option*/
, whereSubsetStmt= /*you do not need to add where. For example where age>50; you only need supply: age>50 */
)


data overtrtr.opNch_trtProfile;
length assoc_hs_type $ 35;
set overtrtr.opNch_trtProfile;
%cpt2trt_pca(hcpcs)
run;
%_libyz(trtbat,Z:\j_scrdata\pcaOverTreatmentRevision\treatmentProfileBatch)
%_clnlog
%DSplitByVar(overtrtr.opNch_trtProfile
, regcase /*one variable, can be numeric or character*/
, 100
, %_paste(trtbat.trtProfile|bat|%_seq(100)) /*can be empty*/
, AllPotentialCuts_outds=_allcuts_MAC_DSplitbyvar
)
%_clnlog
%genxpt(%_paste(trtbat.trtProfile|bat|%_seq(100)))


%dnobs(overtrtr.opNch_trtProfile)


/*the search file is 2001 to 2008, so we can only search for int treatment for dx 2001-2007*/
%_denp(pcaIni._iniHcpcs_manyData_dx0107); set %_paste(_hasproc_manydata|%_seq(16)); run;
data _tmp; set %_paste(_hasproc_manydata|%_seq(16)); run;

data pcaIni._1yrIniTrtCand_dx0107;
set _tmp;
%assoc_hs_type_hcpcs(hcpcs)
run;

%_vt(pcaIni._1yrIniTrtCand_dx0107)
data radiaImrtdata;
set pcaIni._1yrIniTrtCand_dx0107;
if assoc_hs_type='imrt' or assoc_hs_type='radia';
run;

data radiaImrtdata_2001;
set radiaImrtdata;
if _first_dx_year='2001';
run;

data radiaImrtdata_2006;
set radiaImrtdata;
if _first_dx_year='2006';
run;

%_pin(
 radiaImrtdata_2006 /*can have where statement*/
,assoc_hs_type
,1)
%_pin(
 radiaImrtdata_2001 /*can have where statement*/
,assoc_hs_type
,1)

%_vt(pcaIni._1yrIniTrtCand_dx0107)

%_bsmei(
 imrtdata
,imrtdata_chk
,_first_dx_year
,n /*min max range  avg std var nmiss n cv (CV is coefficient of variance) USS CSS*/
,byvars /*by varaibles*/
,ordervars /*if empty then it is grpbyvars*/
)
%head(pcaIni._1yrIniTrtCand_dx0107)
/*for surgery*/
data tmp_surg_1;
set pcaIni._1yrIniTrtCand_dx0107;
if assoc_hs_type in ('robot_radpromy','open_radpromy');
keep regcase assoc_hs_type from_date_num;
run;
%which_dup_long
(
 tmp_surg_1
,tmp_surg_2
,regcase
,from_date_num /*secondary id*/
,1 /*do not specify first, if you want the first specify 1*/
)
%_vt(tmp_surg_3)
/*then find these patients who meet entitlement criterion*/
%_jot(tmp_surg_2,scrimr.prost_bf12after12_dx0107,tmp_surg_3,regcase)
data tmp_surg4; set tmp_surg_3(keep=regcase assoc_hs_type from_date_num); run;
%_numdate2chardate(tmp_Surg4, from_date_num,,treatDate)
%liuv(tmp_Surg4,tmp_Surg4,regcase assoc_hs_type treatDate)
%_joz(tmp_Surg4, psfpr09.demo_nocomorb_july012011, tmp_surg5, regcase)
%liuv(tmp_Surg5,tmp_Surg5,regcase assoc_hs_type treatDate _dxage _tstage_raw _grade)
%_vt(tmp_Surg5)
%_joz(tmp_Surg5, psfpr09.pca_comorb, tmp_surg6, regcase)
%liuv(tmp_Surg6,tmp_Surg6,regcase assoc_hs_type treatDate _dxage _tstage_raw _grade CHRLSON)
%_joz(tmp_Surg6, psfpr09.pca_dxdate_all, tmp_surg7, regcase)
%liuv(tmp_Surg7,tmp_Surg7,regcase assoc_hs_type treatDate _dxage _tstage_raw _grade CHRLSON _first_dx_month _first_dx_year)
data forSam.firstYrSurgEntDx0107; set tmp_Surg7; run;
data exportSample;
set tmp_Surg7;
if _n_<=100;
regcase ="encrypted";
run;
%_2csv_core(
  exportSample /*input dataset name*/
, forSam.exportSample /*can be empty then will be same as indsn_list*/ 
)
proc print data=exportSample;run;

%_vt(forSam.firstYrSurgEntDx0107)

proc freq data=pcaIni._1yrIniTrtCand_dx0107; table assoc_hs_type; run;

/*radiation*/
data tmp_radia_1;
set pcaIni._1yrIniTrtCand_dx0107;
/*if assoc_hs_type in ('radia','proton_beam','imrt','cry');*/
if assoc_hs_type in ('radia','imrt');
keep regcase assoc_hs_type from_date_num;
run;
%which_dup_long
(
 tmp_radia_1
,tmp_radia_2
,regcase
,from_date_num /*secondary id*/
,1 /*do not specify first, if you want the first specify 1*/
)

%_vt(tmp_radia_2)

data tmp_radia_2_1;
set tmp_radia_2;
if assoc_hs_type in ('radia','imrt');
run;

%_pin(tmp_radia_2_1
,assoc_hs_type
,1 /*0 or 1, can be empty, if empty then no print*/
, /*can be empty, if empty, outdsn is _freqout_output*/
,1 /*0 or 1, if empty then it is 0*/
, /*can be empty, if empty then do not store the outptu into csv file, if not emtpty, it has libname.dsn format, e.g., templib.freqout*/
)
%_pin(tmp_radia_2_1, )

/*then find these patients who meet entitlement criterion*/
%_jot(tmp_radia_2,scrimr.prost_bf12after12_dx0107,tmp_radia_3,regcase)
data tmp_radia4; set tmp_radia_3(keep=regcase assoc_hs_type from_date_num); run;
%_numdate2chardate(tmp_radia4, from_date_num,,treatDate)
%_vt(tmp_radia4)
data forSam.firstYrRadiaEntDx0107; set tmp_radia4(keep=regcase assoc_hs_type treatDate); run;

%_vt(forSam.firstYrRadiaEntDx0107)

%_vt(scrimr.prost_bf12after12_dx0107)
data psfpr09.pca_dx_date_all;
run;

data chk; set tmp_radia4(keep=regcase assoc_hs_type treatDate); run;

/*no need for the rest*/
%liuv(tmp_radia4,tmp_radia4,regcase assoc_hs_type treatDate)
%_joz(tmp_radia4, psfpr09.demo_nocomorb_july012011, tmp_radia5, regcase)
%liuv(tmp_radia5,tmp_radia5,regcase assoc_hs_type treatDate _dxage _tstage_raw _grade)
%_joz(tmp_radia5, psfpr09.pca_comorb, tmp_radia6, regcase)
%liuv(tmp_radia6,tmp_radia6,regcase assoc_hs_type treatDate _dxage _tstage_raw _grade CHRLSON)
%_joz(tmp_radia6, psfpr09.pca_dxdate_all, tmp_radia7, regcase)
%liuv(tmp_radia7,tmp_radia7,regcase assoc_hs_type treatDate _dxage _tstage_raw _grade CHRLSON _first_dx_month _first_dx_year)


%_vt(forSam.firstYrRadiaEntDx0107)

/*load the entitlement file for SAM and Florian*/
%_vt(scrimr.prost_bf12after12_dx0107)
data forSam.entitleRegcaseDx0107; set scrimr.prost_bf12after12_dx0107 (keep=regcase);run;









