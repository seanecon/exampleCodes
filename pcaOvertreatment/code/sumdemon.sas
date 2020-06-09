1. find out these age 66 month, calendar(age_66)>1992_jan_1 (we have 1991 data)
2  find out age_end, so calendar(age_end) + 10 <= obswind (dec 31 2007)
3  find out patient who meets the above two standards




4 check HMO=0 and ent=3 in all between (carlendar(age_66), calendar(age_end))

5 create 10 year death outcomes

%_vt(sumdenom.batch_1)
libname sumdenom "V:\seanyz\temp\sumdenomncfiles";
%splitds(svr09sr.sumdenomnc, 1000, outdsnkw=sumdenom.batch_)
%dnobs(svr09sr.sumdenomnc)

%_vt(svr09sr.sumdenomnc)
%macro _genFollowUp(indsn,outdsn, lastObservedDate=31/12/2009);
data _indsn_1;
set &indsn. (where=(sex='1'));
keep shufhic hmo1-hmo288 ent1-ent288 sex race birthyr birthd birthm  MED_DODM MED_DODD MED_DODY;
run;

%_chBirthDeath2FollowUpSinceBirth(
/*input*/
  _indsn_1
, MED_DODM MED_DODD MED_DODY
 /*med_dodm med_dodd med_dody, order matters*/
,BIRTHM BIRTHD BIRTHYR /*birthm birthd birthyr,  order matters, and birthd is set to '15' for seer medicare data*/
,&lastObservedDate. /*eg. 31/12/2006 */
/*output*/
,&outdsn.
,followupsinceBirthVn=followUp /*since birth*/
,deathIndicatorVn=deathIndicator
/*optional parameters*/
,timeunit=year /*year, month, day, week*/
)
%rnvn(&outdsn., shufhic, hicbic)
%genxpt(&outdsn.)
%mend;

%_genFollowUp(sumdenom.batch_999,sumdenom.batch_short_999)
%macro _batchout(start,end);
%local i;
%do i=&start. %to &end.;
%_genFollowUp(sumdenom.batch_&i.,sumdenom.batch_short_&i.)
%end;
%mend;
/*%_batchout(101,1000)*/

%dvn(sumdenom.batch_short_999)







keptVars <-c('shufhic','med.dodm','med.dodd','med.dody','sex','race','birthyr','birthd','birthm'
             , hmoVars, entVars
             )
%genxpt(sumdenom.batch_999)
%xud(MArraySameValueTill2DSR)
%dvn(sumdenom.batch_1)

data check;
set sumdenom.batch_999;
indiv_from_suffix=1;
run;

%_vt(sumdenom.batch_999)

%ArraySameValueTill(
  check
, outdsn
, hmo
, C /*C or N, N means numeric and C mean character*/
, 1 /* suffix means suffix of array variables, startsuffix can be smaller than endsuffix*/
, 288
, indiv_from_suffix
, 0 
 /*e.g., for character 'G' 'K' 'C', supply  G K C for , do not supply 'G' 'K' 'C' */
 /* for numberic  1 2 3, supply 1 2 3*/
, length_of_charvar=32
, dsn_CenterSuffix_OutsideBound=_obs_CenterSuffix_OutsideBound /*observations in this data have centering index outside boundary defined by startsuffix and endsuffix*/
)

%xud(ArraySameValueTill2DSR)

%MArraySameValueTill2DSR(
  check
, shufhic
, outdsn
, indiv_from_suffix
, hmo ent              /* separated by space */
, C C             /* separated by space */
, 0 | 3   /* values of interest are separated by | */
, 71
, 288
, lowerbd_array_suffix_vn_list
, upperbd_array_suffix_vn_list
, bwd_howmany_tilldiff_vn_list /*backward*/
, fwd_howmany_tilldiff_vn_list /*foward*/
, keep_indiv_arraykw_data=0 /*delete those intermediate datasets corresponding to each array_keyword*/
, interest_val_sep=%str(|)  /*separator for intereseted values*/
)


data final;
set outdsn;
keep _first_diff_value _how_many_till_diff;run;
proc print data=final;run;

%_vt(outdsn)


