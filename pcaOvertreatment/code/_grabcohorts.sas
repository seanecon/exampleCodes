/*hcpcs codes for different treatments and allTrtCodes are just combination of all types of codes*/

%let surgeryCodes=55810  55812  55815  55840  55842  55845  55866;

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
%let allTrtCodes=&surgeryCodes. &radiationNotImrtCodes. &imrtCodes. &hormonesCodes. &cryocodes.;
 


