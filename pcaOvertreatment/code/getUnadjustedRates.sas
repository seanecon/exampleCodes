/*get first only cnacer*/
data firstOnlyPca0407;
set %_paste(psfpr09.firstOnlyPca_dx_year|%_seq(2004,2007));
keep regcase ;
run;

%dnobs(%_paste(psfpr09.firstOnlyPca_dx_year|%_seq(2004,2007)))

%_joz(firstOnlyPca0407, psfpr09.demo_nocomorb_june102012, _test, regcase)

data test66above;
set _test;
if _dxage>=66;
keep regcase _first_dx_year;
run;

%_jot(test66above, scrimr.prost_bf12after12_dx0107, _final,regcase)

data nomets; set pcaot.pcadx20042007Mets;
if _metsInd=0;
keep regcase;
run;

%_jot(_final,nomets,_final_nomets,regcase)


%_pin(
 indsn= _final_nomets /*can have where statement*/
,vars=_first_dx_year
,print=1 /*0 or 1, can be empty, if empty then no print*/
,outdsn= /*can be empty, if empty, outdsn is _freqout_output*/
,freqorder= /*0 or 1, if empty then it is 0*/
,csvdsn= /*can be empty, if empty then do not store the outptu into csv file, if not emtpty, it has libname.dsn format, e.g., templib.freqout*/
,is_vars_comb=
/*I add this option, so if you want to get frequence of combination of vars*/
/*empty or 0 or 1, emtpy is defaulted to 0, 1 mean yes I like get combination of vars*/
/*you need a least 2 variable to get combination*/
/*if is_vars_comb is empty then is_vars_comb=0*/
/*if there is only one variable and you sepcifiy is_vars_comb=1 then is_vars_comb will be forced into 0 because you need >=2 variables to generate combination*/
)
