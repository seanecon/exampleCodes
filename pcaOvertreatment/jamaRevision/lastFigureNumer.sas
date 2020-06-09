
We have located the numerators by year for our paper (IMRT, robot, and observation). 
The numerators meet all the exclusion criteria (e.g., first and only cancer, age >=66, entitlement, no metastatic disease, no missing stage, gleason score, psa, etc).

1) limiting to 66+ and 2) enforcing the same entitlement criteria

If the numbers for the denominator still look too high, 
we will think about enforcing the same exclusion criteria (e.g., metastatic disease, missing Gleason score, stage, psa, etc)


%_vt(overtrtR.pcadx20042009Mets)

%_vt(pcaot.cancerPatientDs_03222013)


data _66plus;
set pcaot.cancerPatientDs_03222013;
if _dxAge>=66;run;



data troubleReg;
length patient_id  $ 10;
input patient_id;
cards;
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




%_joz(troubleReg, overtrtR.demo_noComorb_03162013, _out, patient_id)
