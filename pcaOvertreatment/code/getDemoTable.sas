/*grab mets*/
/*we need all prostate cancer*/


data _allpcaDx_regcase; set psfpr09.pca_dxdate_all (keep=regcase); run;
/*psfpr09.pca_dxdate_all has a bit smaller number of obs than svr09sr.pedsfprostate probably due ot dx year before 1992 are removed*/

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
%_vt(pcaot.pcadx20042007Mets)
%genxpt(pcaot.pcadx20042007Mets)


%dvn(psfpr09.demo_noComorb_july012011)



%_genSeerPCaClinicalTstgCat(
  regcase_stgvn_ds
, first_dx_date_ds /*there is a variable switch in 2003, so you need this dx year info*/
, dx_num_time_vn  /*_firstdxdate_num*/
, outdsn
, stg_interestCancerVn
, stgGrpVn_simple
, stgGrpVn_detailed
, sitelocVn /*from first_dx_date_ds, usually called _first_cancer_site*/
, stg10vns_upto2003=%_paste(e10ex|%_seq(10))
, stg10vns_after2003=%_paste(csex|%_seq(10))
/*%_paste(dajcct|%_seq(10)), before 2012 June 08 we use this, suboptimal*/
)

/* pcaot.cancer_dxstageGrade*/

data _grade;
length _gradecat $20.;
set psfpr09.demo_noComorb_july012011(keep=regcase _grade);

/*„1? = Well differentiated; differentiated, NOS*/
/*„2? = Moderately differentiated; intermediate*/
/*Differentiation*/
/*„3? = Poorly differentiated; differentiated*/
/*„4? = undifferentiated; anaplastic*/
/*„5? = T-cell; T-precursor*/
/*„6? = B-cell; Pre-B;B-precursor*/
/*„7? = Null cell; Non T-non B;*/
/*„8? = N K cell (natural killer cell)*/
/*„9? = Cell type not determined, not stated or not*/
/*Applicable*/
_gradeCat='restType';
if _grade in ('1','2') then _gradeCat='wellmodDiff';
if _grade in ('3','4') then _gradeCat='poorUndiff';
run;

data _stage;
set psfpr09.stageafterjune082012;
run;

%_joz(_grade, _stage, pcaot.cancer_dxstageGrade, regcase)

proc freq data=pcaot.cancer_dxstageGrade;table _gradeCat;run;
%genxpt(pcaot.cancer_dxstageGrade)
/* _gradecat      Frequency     Percent     Frequency      Percent*/
/*                ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ*/
/*                poorUndiff       154329       29.78        154329        29.78*/
/*                restType          35016        6.76        189345        36.54*/
/*                wellmodDiff      328848       63.46        518193       100.00*/

%_vt(pcaot.cancer_dxstageGrade)

 



