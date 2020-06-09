%macro _ary2ds(
 inAry
,primaryIdVn
,vns
,outAryValueVn
);
%local inAry aryVns outAryValueVn;
%mend;









%macro _aryjoin(
 inAry
,primaryIdVn
,secondaryIdVec
);
%mend;

%macro(
  indsn
, outdsn
, keptvars
);
%local indsn outdsn;

%mend;

/*sumdenom noncancer sample demographic file*/

libname pcaot 'Z:/j_scrdata/pcaOverTreatment';
data pcaot.testdf_sumdenom_Demo;
set seer09nc.sumdenomnc;
if _n_<1000;
run;
%genxpt(pcaot.testdf_sumdenom_demo)

regcase birthm birthyr /*used by macro _firstcancer_dx_date*/
M_sex /*pedsf 1 is male*/
sex /*sumdenomnc 1 is male*/
birthd /*only in sumdenomnc*/
race
/*1 white 2 black 3 is other, 4 is asian, 5 is hispanic, 6 native american, 0 is unknown, we probably will be excluding these with unknown race*/
urbrur
urban
state 
county
zip	/*geo*/
/* codpub*/
codpubf
 /*cause of death*/
/*in new data 2011, codpub is changed into codpubf*/
med_dodm med_dodd med_dody /*in sum denom and pedsf are the same*/


/*geographic location variable in sumdeno is a bit intersting..*/
urban1986-urban2009
st1986-st2009
cnty1986-cnty2009
zip1986-zip2009
registry1986-registry2009 /*first registory found and is retained until the patient moved into a new registry*/
reg2cd1986-reg2cd2009 /*registroy variable based on state and country, the last residence the patient lived at for that year*/


/*age is time invariant*/
demo_timeInv
demo_timeV

data _birth_demog_addday; set _inpedsf_pc(keep=regcase birthm birthyr); length birthd $2.; birthd='15'; run;
%_joz(_birth_demog_addday, _first_dx_date_addday_ds, _tmp, regcase)
%chmdy2nummdy(_tmp, birthm birthd birthyr > _birthdate_num, _tmp1)


data _pesdf_regcase_zip5dig; set _inpedsf_pc(keep=regcase zip);length _zip_5dig $ 5.; _zip_5dig=substr(zip,1,5);keep regcase _zip_5dig;run;
data _zip_seer_score; set &zipSesDs.(keep=&zip5digit_vn. &ses_vn.); run;
%rnvn(_zip_seer_score, &zip5digit_vn. &ses_vn., _zip_5dig _ses)
%_genSeerSES(
  _pesdf_regcase_zip5dig /*dataset contains patient id and 5 digit zip*/
, _zip_seer_score /*dataset contains zipcode and ses_core, this data set is obtained from Brent*/
, _zip_5dig /*shared by patientidZipDs and zipSesDs*/
, _ses /*in zipSesDs*/
, _sesScore_out /*output ds*/
)

data ;
set ;
run;


%_r2sasYz(datapath
/*no / at the end*/
,dataname /*no .dbf*/
,sasdsn 
)
