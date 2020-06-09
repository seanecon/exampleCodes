



data troubleReg;
length patient_id  $ 10;
input patient_id;
cards;
 0200306075 0200307045 0200338075 0200341005 0200360075 0200400011 0200403010 0200405000 0200405020 0200407080
0200499021  
0202475021  
0205427040 
0207482030   
0219447011   
0226445040   
0228477000  
0231444050   
0235450030   
0235459060 
0236454060 
0238412020   
0241478060  
0242467000
0254497040  
0260457030 
0272418090  
0285475010 
0291499000  
0297401080 
0297426070 
2010188083  
2078142017  
2083174001 
2089102027 
2098153036  
;
run;

%_joh(
  troubleReg  /*do not have two or more records with the same key values?...no.. I think you can have*/
      /*if small data is a many data, _joh would pick up the last records of the many data into a unique data then do the merge*/
,  /*these variables in small dataset you like to have, make sure key variable is NOT in this list*/
/*this is typically alwasy needed, if you do not need carry and variable from small, it is a subset problem then use _srok macro*/
/*separated by %str(|), if not fully specified, the specified would be recycled*/
, sr12.pedsfprostate /*the list of large data sets you do NOT want to sort*/
,  %_paste(e10ex|%_seq(10)) %_paste(csex|%_seq(10))  %_paste(cs1st|%_seq(10))  %_paste(cs6st|%_seq(10))   /*specifiy this will trigger keep statement, so if you specificy this make sure key variable is included*/
/*froim Aug 02, 2011, on I do not need to add keys, it will automatically added*/
/*if you want to keep everthing in large table, then leave this blank*/
/*separated by %str(|), if not fully specified, the specified would be recycled*/
, patient_id /*the keyvar, only one key, key should be positive integer in either numeric or character form*/
/*key should be positive integer in either numeric or character format*/
/*each element of keyList is one single key variable*/
, _checktrouble
)


data overtrtr.checktrouble;
set overtrtr.checktrouble(drop=_first_dx_day _first_cancer_site _first_dx_month _first_dx_year _firstdxdate_num
);
run;



%_genSeerPCaClinicalTstgCat(
  overtrtr.checktrouble
, overtrtR.firstOnlypcaDxInfo /*there is a variable switch in 2003, so you need this dx year info*/
, _firstdxdate_num
, outdsn
, _stg  /*output VN For raw stage group, usually _stage*/
, stgGrpVn_simple  /*output VN For simple stage group */
, stgGrpVn_detailed  /*output VN For detailed stage group*/
, _first_cancer_site  /*from first_dx_date_ds, usually called _first_cancer_site*/
, patient_id
, stg10vns_upto2003=%_paste(e10ex|%_seq(10))
, stg10vns_after2003=%_paste(csex|%_seq(10))
/*%_paste(dajcct|%_seq(10)), before 2012 June 08 we use this, suboptimal*/
)


%_joz(overtrtr.checktrouble, overtrtR.firstOnlypcaDxInfo, _outforDamico,patient_id )

%_genDAmicoPcaRiskCat(
  _outforDamico
/*regcase _first_cancer_site cs1st1-cs1st10 cs6st1-cs6st10 dajcct1-dajcct10*/
, _damicoOut
, varsLists=%_paste(cs1st|%_seq(10)) | %_paste(cs6st|%_seq(10)) | %_paste(csex|%_seq(10))
/*later you prbably want to use cs8st1-10 isntead of cs6st1-10*/
/*%_paste(dajcct|%_seq(10))*/
/*dajcct is based on best pathology, but csex is clinical pathology so csex is better*/

/*dajcct is so the best stage, for surgerical paitent, it is pathologic(best), for radiation, the best is then just clinical since there is no way to get patholotgical*/

, locationVn=_first_cancer_site
, maxcol=10 /*there are 10 sites*/
, damicoRiskVn=_damicoRisk
)


%_vt(_damicoOut)

%_vt(outdsn)

