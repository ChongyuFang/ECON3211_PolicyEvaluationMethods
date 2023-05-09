********** ECO3211 Quantitative Methods for Policy Evaluation

********** Fall 2020, CUHKSZ

********** Problem Set 4 Regression Discontinuity Design

********** Prepared by Chongyu Fang



********** Basic Setup **********

* clears memory of all data

clear

* close the log file if any already open

capture log close

* turn the -more- off if you want STATA to report results continuously

set more off

* set our working directory

cd "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_Data"

* open a log file

log using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_Chongyu_Fang.log", text replace

* load in data

use "2010adult_English.dta", clear

* birth year (note: there are some useless negative values and we nullify these negative values

gen yob = qa1y_best

replace yob = . if qa1y_best<0

* summarize the percentile of birth year

sum yob, d

* drop the top/bottom 10% birth year data

drop if yob < 1943

drop if yob > 1987

* birth month (note: there are some useless negative values and we nullify these negative values

gen mob = qa1m

replace mob = . if qa1m < 0

* generate cohorts

gen cohort = yob

replace cohort = yob+1 if mob >= 9 & mob != .

drop if cohort == .

* generate cutoff

gen cutoff = .

replace cutoff = 1969 if provcd==31|provcd==33|provcd==36|provcd==51

replace cutoff = 1970 if provcd==11|provcd==12|provcd==13|provcd==14|provcd==21|provcd==22|provcd==23|provcd==32|provcd==37|provcd==41|provcd==42|provcd==44|provcd==50|provcd==53

replace cutoff = 1971 if provcd==34|provcd==52|provcd==61

replace cutoff = 1972 if provcd==35

replace cutoff = 1974 if provcd==62

replace cutoff = 1975 if provcd==43|provcd==45

* generate monthly running variable

gen dist_m = .

replace dist_m = (yob*12+mob)-(1969*12+9) if provcd==31|provcd==33|provcd==36|provcd==51

replace dist_m = (yob*12+mob)-(1970*12+9) if provcd==11|provcd==12|provcd==13|provcd==14|provcd==21|provcd==22|provcd==23|provcd==32|provcd==37|provcd==41|provcd==42|provcd==44|provcd==50|provcd==53

replace dist_m = (yob*12+mob)-(1971*12+9) if provcd==34|provcd==52|provcd==61

replace dist_m = (yob*12+mob)-(1972*12+9) if provcd==35

replace dist_m = (yob*12+mob)-(1974*12+9) if provcd==62

replace dist_m = (yob*12+mob)-(1975*12+9) if provcd==43|provcd==45

* generate normalized running variable

gen x=dist_m

* location dummy

gen d=(dist_m >= 0)

* interaction terms

gen dx = d*x

gen x2 = x*x

gen dx2 = d*x2

gen x3 = x*x*x

gen dx3 = d*x3

gen x4 = x*x*x*x

gen dx4 = d*x4

* education variable

gen educ = cfps2010eduy_best

* treatment: compulsory schooling

gen school = 0

replace school = 1 if qc605 == 1

replace school = . if qc605 < 0

*********************************************************************************
************************ Validity test of RDD ***********************************
*********************************************************************************

preserve

* select optimal bandwidth for schooling

rdbwselect school x, bwselect(CCT)

* save bandwidth results

gen bw_CCT = round(e(h_CCT))

* first stage

reg school d x dx if abs(x) <= bw_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*) replace

predict hat_school if e(sample) //local linear fit

predict sd_school if e(sample), stdp //std errors of the local linear fit

gen ub_school = hat_school+1.96*sd_school

gen lb_school = hat_school-1.96*sd_school // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_school=mean(school)

* graph

keep if abs(x) <= bw_CCT + 2

twoway (scatter mean_school x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_school lb_school x if x<0, sort lpattern(dash)) ///
(rline ub_school lb_school x if x>=0, sort lpattern(dash)) ///
(line hat_school x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_school x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(Distance to the Cutoff) xline(0) ///
title("Schooling", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_firststage.tif", as(tif) replace

restore

* then we use the histogram to conduct density test

histogram x, discrete width(1) xline(0) legend(col(2)) xtitle("Birth Cohort")

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_density.tif", as(tif) replace

* check smoothness of gender

preserve

* male

gen male = gender

replace male = . if gender < 0

* Select optimal bandwidth (h) for gender

rdbwselect male x, bwselect(CCT)

* Save bandwidth results

gen bw_CCT = round(e(h_CCT))

reg male d x dx if abs(x) <= bw_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*)

predict hat_male if e(sample) // local linear fit

predict sd_male if e(sample), stdp // std errors of the local linear fit

gen ub_male=hat_male+1.96*sd_male

gen lb_male=hat_male-1.96*sd_male // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_male = mean(male)

* gragh

keep if abs(x) <= bw_CCT+2

twoway (scatter mean_male x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_male lb_male x if x<0, sort lpattern(dash)) ///
(rline ub_male lb_male x if x>=0, sort lpattern(dash)) ///
(line hat_male x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_male x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Gender", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_gender.tif", as(tif) replace

restore

* check smoothness of urban hukou at age 3

preserve

gen hukou3 = 1 if qa302 == 3

replace hukou3 = 0 if qa302 == 1

replace hukou3 = . if qa302 < 0

* select optimal bandwidth (h) for hukou status at 3

rdbwselect hukou3 x, bwselect(CCT)

* save bandwidth results

gen bw_CCT = round(e(h_CCT))

reg hukou3 d x dx if abs(x) <= bw_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*)

predict hat_hukou3 if e(sample) // local linear fit

predict sd_hukou3 if e(sample), stdp // std errors of the local linear fit

gen ub_hukou3=hat_hukou3+1.96*sd_hukou3

gen lb_hukou3=hat_hukou3-1.96*sd_hukou3 // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_hukou3 = mean(hukou3)

* gragh

keep if abs(x) <= bw_CCT+2

twoway (scatter mean_hukou3 x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_hukou3 lb_hukou3 x if x<0, sort lpattern(dash)) ///
(rline ub_hukou3 lb_hukou3 x if x>=0, sort lpattern(dash)) ///
(line hat_hukou3 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_hukou3 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Hukou Status at Age 3", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_hukou3.tif", as(tif) replace

restore

* check smoothness of urban hukou at age 12

preserve

gen hukou12 = 1 if qa402 == 3

replace hukou12 = 0 if qa402 == 1

replace hukou12 = . if qa402 < 0

* select optimal bandwidth (h) for hukou status at 12

rdbwselect hukou12 x, bwselect(CCT)

* save bandwidth results

gen bw_CCT = round(e(h_CCT))

reg hukou12 d x dx if abs(x)<=bw_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*)

predict hat_hukou12 if e(sample) // local linear fit

predict sd_hukou12 if e(sample), stdp // std errors of the local linear fit

gen ub_hukou12=hat_hukou12+1.96*sd_hukou12

gen lb_hukou12=hat_hukou12-1.96*sd_hukou12 // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_hukou12=mean(hukou12)

* gragh

keep if abs(x) <= bw_CCT+2

twoway (scatter mean_hukou12 x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_hukou12 lb_hukou12 x if x<0, sort lpattern(dash)) ///
(rline ub_hukou12 lb_hukou12 x if x>=0, sort lpattern(dash)) ///
(line hat_hukou12 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_hukou12 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Hukou status at age 12", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_hukou12.tif", as(tif) replace

restore

* check smoothness of Han ethnicity group

preserve

gen han = 1 if qa5code == 1

replace han = 0 if han == .

replace han = . if qa5code < 0

* select optimal bandwidth (h) for Han ethnicity group

rdbwselect han x, bwselect(CCT)

* save bandwidth results

gen bw_CCT = round(e(h_CCT))

reg han d x dx if abs(x)<=bw_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*)

predict hat_han if e(sample) // local linear fit

predict sd_han if e(sample), stdp // std errors of the local linear fit

gen ub_han=hat_han+1.96*sd_han

gen lb_han=hat_han-1.96*sd_han // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_han=mean(han)

* gragh

keep if abs(x)<=bw_CCT+2

twoway (scatter mean_han x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_han lb_han x if x<0, sort lpattern(dash)) ///
(rline ub_han lb_han x if x>=0, sort lpattern(dash)) ///
(line hat_han x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_han x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Han group", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_han.tif", as(tif) replace

restore

* check smoothness of mother's total years of education

preserve

replace meduc=3 if tb4_a_m==1
replace meduc=6 if tb4_a_m==2
replace meduc=9 if tb4_a_m==3
replace meduc=12 if tb4_a_m==4
replace meduc=15 if tb4_a_m==5
replace meduc=16 if tb4_a_m==6
replace meduc=19 if tb4_a_m==7
replace meduc=22 if tb4_a_m==8
replace meduc=. if tb4_a_m<0

* select optimal bandwidth (h) for mother education level

rdbwselect meduc x, bwselect(CCT)

* save bandwidth results

gen bw_CCT = round(e(h_CCT))

reg meduc d x dx if abs(x)<=bw_CCT,cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*)

predict hat_meduc if e(sample) // local linear fit

predict sd_meduc if e(sample), stdp // std errors of the local linear fit

gen ub_meduc=hat_meduc+1.96*sd_meduc

gen lb_meduc=hat_meduc-1.96*sd_meduc // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_meduc=mean(meduc)

* gragh

keep if abs(x)<=bw_CCT+2

twoway (scatter mean_meduc x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_meduc lb_meduc x if x<0, sort lpattern(dash)) ///
(rline ub_meduc lb_meduc x if x>=0, sort lpattern(dash)) ///
(line hat_meduc x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_meduc x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Mother's education level", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_meduc.tif", as(tif) replace

restore

* check smoothness of father's total years of education

preserve

replace feduc=3 if tb4_a_f==1
replace feduc=6 if tb4_a_f==2
replace feduc=9 if tb4_a_f==3
replace feduc=12 if tb4_a_f==4
replace feduc=15 if tb4_a_f==5
replace feduc=16 if tb4_a_f==6
replace feduc=19 if tb4_a_f==7
replace feduc=22 if tb4_a_f==8
replace feduc=. if tb4_a_f<0

* select optimal bandwidth (h) for father education level

rdbwselect feduc x, bwselect(CCT)

* save bandwidth results

gen bw_CCT = round(e(h_CCT))

reg feduc d x dx if abs(x)<=bw_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table1.xls", dec(3) drop(_I*)

predict hat_feduc if e(sample) // local linear fit

predict sd_feduc if e(sample), stdp // std errors of the local linear fit

gen ub_feduc=hat_feduc+1.96*sd_feduc

gen lb_feduc=hat_feduc-1.96*sd_feduc // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_feduc=mean(feduc)

* gragh

keep if abs(x)<=bw_CCT+2

twoway (scatter mean_feduc x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_feduc lb_feduc x if x<0, sort lpattern(dash)) ///
(rline ub_feduc lb_feduc x if x>=0, sort lpattern(dash)) ///
(line hat_feduc x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_feduc x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Father's education level", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_feduc.tif", as(tif) replace

restore

*********************************************************************************
*********** impact of the compulsory education policy on schooling **************
*********************************************************************************

* reduced form

preserve

* select optimal bandwidth (h) for reduced form

rdbwselect educ x, all

* save bandwidth results

gen bw_re_CCT=round(e(h_CCT)) 

gen bw_re_IK=round(e(h_IK))

gen bw_re_CV=round(e(h_CV))

* select optimal bandwidth (h) for first stage

rdbwselect school x, all

* save bandwidth results

gen bw_first_CCT=round(e(h_CCT))

gen bw_first_IK=round(e(h_IK))

gen bw_first_CV=round(e(h_CV))

* use the min{first_h,reduce_h} to select the optimal bandwidth for local linear regression

gen bw_educ_CCT=min(bw_first_CCT, bw_re_CCT)

gen bw_educ_IK=min(bw_first_IK, bw_re_IK)

gen bw_educ_CV=min(bw_first_CV, bw_re_CV)

* triangle kernal function

gen triw_educ_CCT=1-abs(x/bw_educ_CCT)

replace triw_educ_CCT=1/bw_educ_CCT if triw_educ_CCT == 0

gen triw_educ_IK=1-abs(x/bw_educ_IK)

replace triw_educ_IK=1/bw_educ_IK if triw_educ_IK == 0

gen triw_educ_CV=1-abs(x/bw_educ_CV)

replace triw_educ_CV=1/bw_educ_CV if triw_educ_CV == 0

* using CCT as bandwidth

reg educ d x dx if abs(x)<=bw_educ_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*) replace

predict hat_educ if e(sample) // local linear fit
predict sd_educ if e(sample), stdp // std errors of the local linear fit
gen ub_educ=hat_educ+1.96*sd_educ
gen lb_educ=hat_educ-1.96*sd_educ // 95% CI

* mean probability of treatment in each bin

by x, sort: egen mean_educ=mean(educ)

* gragh
keep if abs(x)<=bw_educ_CCT+2

twoway (scatter mean_educ x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_educ lb_educ x if x<0, sort lpattern(dash)) ///
(rline ub_educ lb_educ x if x>=0, sort lpattern(dash)) ///
(line hat_educ x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_educ x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Education", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/nonparametric_educ.tif", as(tif) replace

* use d as IV

* rectangular kernel

ivreg2 educ (school=d) x dx if abs(x)<=bw_educ_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*)

* triangle kernel

ivreg2 educ (school=d) x dx [pw=triw_educ_CCT] if abs(x)<=bw_educ_CCT, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*)

*********************************************************************************
************************** Robustness Checks ************************************
*********************************************************************************

* using IK as bandwidth

* rectangular kernel

ivreg2 educ (school=d) x dx if abs(x)<=bw_educ_IK,cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*)

* triangular kernel

ivreg2 educ (school=d) x dx [pw=triw_educ_IK] if abs(x)<=bw_educ_IK, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*)

* using CV as bandwidth

* rectangular kernel

ivreg2 educ (school=d) x dx if abs(x)<=bw_educ_CV,cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*)

* triangular kernel

ivreg2 educ (school=d) x dx [pw=triw_educ_CV] if abs(x)<=bw_educ_CV, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table2.xls", dec(3) drop(_I*)

restore

* using parametric approach

* first order

ivreg2 educ (school=d) x dx, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table3.xls", dec(3) drop(_I*) replace

* second order with slope change

preserve

reg educ d x dx x2 dx2, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table3.xls", dec(3) drop(_I*)

predict hat_educ if e(sample) // local linear fit

predict sd_educ if e(sample), stdp // std errors of the local linear fit

gen ub_educ=hat_educ+1.96*sd_educ

gen lb_educ=hat_educ-1.96*sd_educ // 95% C.I.

* mean probability of treatment in each bin

by x, sort: egen mean_educ=mean(educ)

* graph

twoway (scatter mean_educ x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_educ lb_educ x if x<0, sort lpattern(dash)) ///
(rline ub_educ lb_educ x if x>=0, sort lpattern(dash)) ///
(line hat_educ x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_educ x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Education", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/parametric_educ2.tif", as(tif) replace

ivreg2 educ (school=d) x dx x2 dx2, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table3.xls", dec(3) drop(_I*)

restore

* third order without slope change 

preserve

reg educ d x x2 x3, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table3.xls", dec(3) drop(_I*)

predict hat_educ if e(sample) // local linear fit

predict sd_educ if e(sample), stdp // std errors of the local linear fit

gen ub_educ=hat_educ+1.96*sd_educ

gen lb_educ=hat_educ-1.96*sd_educ // 95% C.I.

by x, sort: egen mean_educ=mean(educ)

* graph

twoway (scatter mean_educ x, mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_educ lb_educ x if x<0, sort lpattern(dash)) ///
(rline ub_educ lb_educ x if x>=0, sort lpattern(dash)) ///
(line hat_educ x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_educ x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) ///
title("Education", size(small))

graph export "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/PS4_figs/parametric_educ3.tif", as(tif) replace

ivreg2 educ (school=d) x x2 x3, cluster(x)

outreg2 using "/Users/fangchongyu/Desktop/ECO3211 Quantitative Methods for Policy Evaluation/Problem sets/Problem set 4/table3.xls", dec(3) drop(_I*)

restore

* Finished! Close the log file

cap log close










