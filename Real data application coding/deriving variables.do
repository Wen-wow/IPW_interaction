set more off
clear all

global rawdata "*"
cd "*"


**# Bookmark #1 ---------------To have eligible participants------*

*** To have participants aged 25-60 at wave 10

* use variable age_dv, which is supposed to be the corrected age at time of interview.

*Those aged 25-60 at wave 10 
use "$rawdata\j_indall"
keep if j_age_dv>=25 & j_age_dv<61 
keep pidp j_ivfio j_age_dv j_memorig j_sampst j_ivfho j_hidp
save indall2560, replace

*wave 9
use "$rawdata\i_indall"
keep pidp i_ivfio i_age_dv i_memorig i_sampst i_ivfho i_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & i_age_dv>=24 & i_age_dv<61)
drop _merge
save indall2560, replace
*wave 8
use "$rawdata\h_indall"
keep pidp h_ivfio h_age_dv h_memorig h_sampst h_ivfho h_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & h_age_dv>=23 & h_age_dv<61)
drop _merge
save indall2560, replace
*wave 7
use "$rawdata\g_indall"
keep pidp g_ivfio g_age_dv g_memorig g_sampst g_ivfho g_hidp
merge 1:1 pidp using indall2560 
keep if _merge>1 | (_merge==1 & g_age_dv>=22 & g_age_dv<61)
drop _merge
save indall2560, replace
*wave 6
use "$rawdata\f_indall"
keep pidp f_ivfio f_age_dv f_memorig f_sampst f_ivfho f_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & f_age_dv>=21 & f_age_dv<61)
drop _merge
save indall2560, replace
*wave 5
use "$rawdata\e_indall"
keep pidp e_ivfio e_age_dv e_memorig e_sampst e_ivfho e_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & e_age_dv>=20 & e_age_dv<61)
drop _merge
save indall2560, replace
*wave 4
use "$rawdata\d_indall"
keep pidp d_ivfio d_age_dv d_memorig d_sampst d_ivfho d_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & d_age_dv>=19 & d_age_dv<61)
drop _merge
save indall2560, replace
*wave 3
use "$rawdata\c_indall"
keep pidp c_ivfio c_age_dv c_memorig c_sampst c_ivfho c_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & c_age_dv>=18 & c_age_dv<61)
drop _merge
save indall2560, replace
*wave 2
use "$rawdata\b_indall"
keep pidp b_ivfio b_age_dv b_memorig b_sampst b_ivfho b_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & b_age_dv>=17 & b_age_dv<61)
drop _merge
save indall2560, replace
*wave 1
use "$rawdata\a_indall"
keep pidp a_ivfio a_age_dv a_memorig a_sampst a_ivfho a_hidp
merge 1:1 pidp using indall2560
keep if _merge>1 | (_merge==1 & a_age_dv>=16 & a_age_dv<61)
drop _merge
save indall2560, replace

*Generate overall origin variable
gen origin=a_memorig 
replace origin=b_memorig if origin>=.
replace origin=c_memorig if origin>=.
replace origin=d_memorig if origin>=.
replace origin=e_memorig if origin>=.
replace origin=f_memorig if origin>=.
replace origin=g_memorig if origin>=.
replace origin=h_memorig if origin>=.
replace origin=i_memorig if origin>=.
replace origin=j_memorig if origin>=.
drop *_memorig
label define origlab 1 "GPS GB 2009/10" 2 "GPS NI 2009/10" 3 "BHPS GP 1991" 4 "BHPS SCOT 1999" 5 "BHPS WAL 1999" 6 "BHPS NI 2001" 7 "EMB 2009/10" 8 "IEMB 2014/15"
label values origin origlab
*Drop those from BHPS
drop if origin>2 & origin<7

*Generate wave at (household) recruitment and wave at individual inclusion
gen waverecruit=1 if origin<3 | origin==7
replace waverecruit=6 if origin==8
gen firstwave=waverecruit
replace firstwave=2 if firstwave==1 & a_ivfio>=.
replace firstwave=3 if firstwave<3 & a_ivfio>=. & b_ivfio>=.
replace firstwave=4 if firstwave<4 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=.
replace firstwave=5 if firstwave<5 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=. & d_ivfio>=.
replace firstwave=6 if firstwave<6 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=. & d_ivfio>=. & e_ivfio>=.
replace firstwave=7 if firstwave<7 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=. & d_ivfio>=. & e_ivfio>=. & f_ivfio>=.
replace firstwave=8 if firstwave<8 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=. & d_ivfio>=. & e_ivfio>=. & f_ivfio>=. & g_ivfio>=.
replace firstwave=9 if firstwave<9 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=. & d_ivfio>=. & e_ivfio>=. & f_ivfio>=. & g_ivfio>=. & h_ivfio>=.
replace firstwave=10 if firstwave<10 & a_ivfio>=. & b_ivfio>=. & c_ivfio>=. & d_ivfio>=. & e_ivfio>=. & f_ivfio>=. & g_ivfio>=. & h_ivfio>=. & i_ivfio>=.

*Generate sampling status at first wave
gen sampst1=a_sampst if firstwave==1
replace sampst1=b_sampst if firstwave==2 
replace sampst1=c_sampst if firstwave==3 
replace sampst1=d_sampst if firstwave==4 
replace sampst1=e_sampst if firstwave==5 
replace sampst1=f_sampst if firstwave==6 
replace sampst1=g_sampst if firstwave==7 
replace sampst1=h_sampst if firstwave==8 
replace sampst1=i_sampst if firstwave==9
replace sampst1=j_sampst if firstwave==10

save indall2560, replace

*Fill in missing ages 
mvdecode *_age_dv, mv(-9, -8,-7,-2, -1)
replace a_age_dv=b_age_dv-1 if a_age_dv>=. & b_age_dv<.
replace a_age_dv=c_age_dv-2 if a_age_dv>=. & c_age_dv<.
replace a_age_dv=d_age_dv-3 if a_age_dv>=. & d_age_dv<.
replace a_age_dv=e_age_dv-4 if a_age_dv>=. & e_age_dv<.
replace a_age_dv=f_age_dv-5 if a_age_dv>=. & f_age_dv<.
replace a_age_dv=g_age_dv-6 if a_age_dv>=. & g_age_dv<.
replace a_age_dv=h_age_dv-7 if a_age_dv>=. & h_age_dv<.
replace a_age_dv=i_age_dv-8 if a_age_dv>=. & i_age_dv<.
replace a_age_dv=j_age_dv-9 if a_age_dv>=. & j_age_dv<.

replace f_age_dv=j_age_dv-4 if f_age_dv>=. & j_age_dv<.
replace f_age_dv=i_age_dv-3 if f_age_dv>=. & i_age_dv<.
replace f_age_dv=h_age_dv-2 if f_age_dv>=. & h_age_dv<.
replace f_age_dv=g_age_dv-1 if f_age_dv>=. & g_age_dv<.
replace f_age_dv=e_age_dv+1 if f_age_dv>=. & e_age_dv<.
replace f_age_dv=d_age_dv+2 if f_age_dv>=. & d_age_dv<.
replace f_age_dv=c_age_dv+3 if f_age_dv>=. & c_age_dv<.
replace f_age_dv=b_age_dv+4 if f_age_dv>=. & b_age_dv<.
replace f_age_dv=a_age_dv+5 if f_age_dv>=. & a_age_dv<.

replace i_age_dv=j_age_dv-1 if i_age_dv>=. & j_age_dv<.
replace i_age_dv=h_age_dv+1 if i_age_dv>=. & h_age_dv<.
replace i_age_dv=g_age_dv+2 if i_age_dv>=. & g_age_dv<.
replace i_age_dv=f_age_dv+3 if i_age_dv>=. & f_age_dv<.
replace i_age_dv=e_age_dv+4 if i_age_dv>=. & e_age_dv<.
replace i_age_dv=d_age_dv+5 if i_age_dv>=. & d_age_dv<.
replace i_age_dv=c_age_dv+6 if i_age_dv>=. & c_age_dv<.
replace i_age_dv=b_age_dv+7 if i_age_dv>=. & b_age_dv<.
replace i_age_dv=a_age_dv+8 if i_age_dv>=. & a_age_dv<.

*Change firstwave to 2/3 if still youth at wave 1/2
replace firstwave=2 if firstwave==1 & a_ivfio>20 & a_ivfio<=25
replace firstwave=3 if firstwave==2 & b_ivfio>20 & b_ivfio<=25

*Generate participation at first wave and age at first wave
*Needed if separately modelling initial individual participation (to create an individual initial response weight)
gen part1=a_ivfio if a_ivfio<2 
replace part1=0 if firstwave==1 & a_ivfio>=2
gen age1=a_age_dv if part1<.
replace part1=b_ivfio if firstwave==2 & b_ivfio<2 
replace part1=0 if firstwave==2 & b_ivfio>=2
replace age1=b_age_dv if part1<. & firstwave==2
replace part1=c_ivfio if firstwave==3 & c_ivfio<2 
replace part1=0 if firstwave==3 & c_ivfio>=2
replace age1=c_age_dv if part1<. & firstwave==3
replace part1=c_ivfio if firstwave==3 & c_ivfio<2 
replace part1=0 if firstwave==3 & c_ivfio>=2
replace age1=c_age_dv if part1<. & firstwave==3
replace part1=d_ivfio if firstwave==4 & d_ivfio<2 
replace part1=0 if firstwave==4 & d_ivfio>=2
replace age1=d_age_dv if part1<. & firstwave==4
replace part1=e_ivfio if firstwave==5 & e_ivfio<2 
replace part1=0 if firstwave==5 & e_ivfio>=2
replace age1=e_age_dv if part1<. & firstwave==5
replace part1=f_ivfio if firstwave==6 & f_ivfio<2 
replace part1=0 if firstwave==6 & f_ivfio>=2
replace age1=f_age_dv if part1<. & firstwave==6
replace part1=g_ivfio if firstwave==7 & g_ivfio<2 
replace part1=0 if firstwave==7 & g_ivfio>=2
replace age1=g_age_dv if part1<. & firstwave==7
replace part1=h_ivfio if firstwave==8 & h_ivfio<2 
replace part1=0 if firstwave==8 & h_ivfio>=2
replace age1=h_age_dv if part1<. & firstwave==8
replace part1=i_ivfio if firstwave==9 & i_ivfio<2 
replace part1=0 if firstwave==9 & i_ivfio>=2
replace age1=i_age_dv if part1<. & firstwave==9
replace part1=j_ivfio if firstwave==10 & j_ivfio<2 
replace part1=0 if firstwave==10 & j_ivfio>=2
replace age1=j_age_dv if part1<. & firstwave==10

*Drop those coming in at wave 10 since only very small number & won't have exposure at wave 9
drop if firstwave==10

*Label key variables
*Note that if someone was not osm then wave of inclusion will be first wave in which they appeared as tsm/psm
label var waverecruit "Wave of household recruitment"
label var firstwave "Wave of inclusion as adult"

label define samplab 1 "osm" 2 "psm" 3 "tsm"
label values sampst1 samplab

save indall2560, replace


********************************************************
**************************************************************
**Run do file to get initial wave household ids for those who don't enter the study at wave 1 or 6
do hhcheck_v0.2.do


*Get ethnicity, sex, etc from cross-wave data
use "$rawdata\xwavedat"
keep pidp sex_dv ethn_dv psu strata
save basicdata, replace
use indall2560
merge 1:1 pidp using basicdata
keep if _merge==3
drop _merge
rename (sex_dv ethn_dv) (sex ethnic)
compress
save indall2560, replace


**# Bookmark #2 ------- weight (a) ------------

**Sampling weights / non-response at wave 1 (or 6 for IEMB)
use "$rawdata\a_hhsamp"
keep a_hidp a_hhdenus_xd a_hhorig a_ivfho
save weightcheck1, replace
use "$rawdata\a_hhresp"
keep a_hidp a_hhdenus*
save temp1, replace
use weightcheck1
merge 1:1 a_hidp using temp1
*Those where households did not respond do not have hh response weight
keep if _merge==3
gen basewgt=a_hhdenus_xw
drop _merge
save weightswave1, replace  

use "$rawdata\f_hhsamp"
keep f_hidp f_hhorig f_hhdenui_xd
keep if f_hhorig==8
drop f_hhorig
save weightcheck6, replace
use "$rawdata\f_hhresp"
keep f_hidp f_hhdenui*
save temp6, replace
use weightcheck6
merge 1:1 f_hidp using temp6
keep if _merge==3
summ f_hhdenui_xw
gen newhhwt=f_hhdenui_xw/r(mean)
gen basewgt=newhhwt
drop _merge
save weightswave6, replace 

*Get base weights
use indall2560
merge m:1 a_hidp using weightswave1
drop if _merge==2
drop _merge a_hhorig 
merge m:1 f_hidp using weightswave6, update
drop if _merge==2
drop _merge 
replace basewgt=basewgt*(30169/(30169+2468)) if origin<8
replace basewgt=basewgt*(2468/(2468+30169)) if origin==8
save indall2560, replace


**# Bookmark #3  -------------- weight (b)------------

** to have the variables needed first, then use logit (lasso).
** these variables are from wave 1 or 6, including
**individual: age group, sex, ethnic group, 
**household: net income quintiles, at least one in paid employment, highest qualification level, 
**the number of bedrooms, housing tenure, household size, at least one UK born or not,
**additional: government region, sample origin, firstwave as an adult.

**GET HOUSEHOLD INFORMATION FROM FIRST WAVE IN WHICH PERSON'S HOUSEHOLD WOULD HAVE BEEN INCLUDED 
clear
use indall2560
gen hhwave=1 if origin<8
replace hhwave=6 if origin==8
save temp, replace
keep if hhwave==1
keep pidp a_hidp
save hhwave1, replace
use temp
keep if hhwave==6
keep pidp f_hidp
save hhwave6, replace


*Variables from HH sampling files
use hhwave1
drop pidp 
duplicates drop 
merge 1:1 a_hidp using "$rawdata\a_hhsamp"
keep if _merge==3
drop _merge
keep a_hidp a_gor_dv
rename a_gor_dv region
save hhwave1vars, replace

use hhwave6
drop pidp
duplicates drop
merge 1:1 f_hidp using "$rawdata\f_hhsamp"
keep if _merge==3
drop _merge
keep f_hidp f_gor_dv
rename f_gor_dv region
save hhwave6vars, replace

use indall2560
merge m:1 a_hidp using hhwave1vars
drop _merge
merge m:1 f_hidp using hhwave6vars, update
drop _merge
compress
save indall2560, replace

*VARIABLES FROM HH RESPONSE FILES
use hhwave1
drop pidp 
duplicates drop 
merge 1:1 a_hidp using "$rawdata\a_hhresp"
keep if _merge==3
drop _merge
keep a_hidp a_hsbeds a_tenure_dv a_hhsize a_fihhmnnet1_dv  

rename a_hsbeds hsbeds
rename a_tenure_dv tenure
rename a_hhsize hhsize
rename a_fihhmnnet1 hhnet
xtile incomeq=hhnet, nq(5)
save hhwave1vars, replace

use hhwave6
drop pidp
duplicates drop
merge 1:1 f_hidp using "$rawdata\f_hhresp"
keep if _merge==3
drop _merge
keep f_hidp  f_hsbeds f_tenure_dv f_hhsize f_fihhmnnet1_dv 
rename f_hsbeds hsbeds
rename f_tenure_dv tenure
rename f_hhsize hhsize
rename f_fihhmnnet1 hhnet
xtile incomeq=hhnet, nq(5)
save hhwave6vars, replace


use indall2560
merge m:1 a_hidp using hhwave1vars
drop _merge
merge m:1 f_hidp using hhwave6vars, update
drop _merge
compress
drop hhnet
label var incomeq "Net income quintile"
save indall2560, replace

*GET SOME HOUSEHOLD VARIABLES FROM INDIVIDUAL FILES
use hhwave1
drop pidp 
duplicates drop 
merge 1:m a_hidp using "$rawdata\a_indall"
keep if _merge==3
mvdecode a_employ, mv(-9, -8, -2, -1)
bysort a_hidp: egen employ=min(a_employ)
keep a_hidp employ
duplicates drop
save hhwave1vars, replace

use hhwave6
drop pidp 
duplicates drop 
merge 1:m f_hidp using "$rawdata\f_indall"
keep if _merge==3
mvdecode f_employ, mv(-9, -8, -2, -1)
bysort f_hidp: egen employ=min(f_employ)
keep f_hidp employ
duplicates drop
save hhwave6vars, replace

use indall2560
merge m:1 a_hidp using hhwave1vars
drop _merge
merge m:1 f_hidp using hhwave6vars, update
drop _merge
compress
label var employ "At least one person in hhold in paid employment"
save indall2560, replace

use hhwave1
drop pidp 
duplicates drop 
merge 1:m a_hidp using "$rawdata\a_indresp"
keep if _merge==3
mvdecode a_qfhigh_dv, mv(-9, -8,-7,-2, -1)
bysort a_hidp: egen qualhigh=min(a_qfhigh_dv)
mvdecode a_ukborn, mv(-9, -8,-7,-2, -1)
bysort a_hidp: egen anynotuk=max(a_ukborn)
bysort a_hidp: egen allnotuk=min(a_ukborn)
recode anynotuk allnotuk (1/4=0) (5=1)
keep a_hidp qualhigh anynotuk allnotuk
duplicates drop
save hhwave1vars, replace
use hhwave6
drop pidp 
duplicates drop 
merge 1:m f_hidp using "$rawdata\f_indresp"
keep if _merge==3
mvdecode f_qfhigh_dv, mv(-9, -8,-7,-2, -1)
bysort f_hidp: egen qualhigh=min(f_qfhigh_dv)
mvdecode f_ukborn, mv(-9, -8,-7,-2, -1)
bysort f_hidp: egen anynotuk=max(f_ukborn)
bysort f_hidp: egen allnotuk=min(f_ukborn)
recode anynotuk allnotuk (1/4=0) (5=1)
keep f_hidp qualhigh anynotuk allnotuk
duplicates drop
save hhwave6vars, replace

use indall2560
merge m:1 a_hidp using hhwave1vars
drop _merge
merge m:1 f_hidp using hhwave6vars, update
drop _merge
compress
label var qualhigh "Highest household qualification"
recode qualhigh (1/5=1) (6/11=2) (12/15=3) (96=4)
label define quallab 1 "Degree/diploma/higher" 2 "A level/bacc/highers" 3 "O level/GCSE" 4 "None"
label values qualhigh quallab
save indall2560, replace

*Regroup some variables
recode ethnic (4=2) (5/8=5) (9/13=3) (14/16=4) (17=5) (3=5) (97=5) (-9=.)
label define ethlab 1 "White British" 2 "Other White" 3 "Asian" 4 "Black" 5 "Mixed/other"
label values ethnic ethlab
mvdecode sex, mv(-9, -8,-7,-2, -1)
gen hhsizegp=hhsize
recode hhsizegp (6/16=6)
mvdecode hsbeds, mv(-9, -8,-7,-2, -1)
recode hsbeds (0=1) (4/20=4)


mvdecode tenure , mv(-9, -8,-7,-2, -1)
gen housing=tenure
recode housing (1/2=0) (6/7=1) (3/5=2) (8=2)
label define houslab 0 "Owned/mortgaged" 1 "Private rented" 2 "Local authority, housing assoc ,other"
label values housing houslab
gen agegp=age1
recode agegp (15/19=0) (20/24=1) (25/29=2) (30/39=3) (40/49=4) (50/60=5)
label define agelab 0 "<20" 1 "20-24" 2 "25-29" 3 "30-39" 4 "40-49" 5 "50-60"
label values agegp agelab
save indall2560, replace

*Model p(part1/household responded at wave 1 or 6) using lasso
clear
use indall2560
keep pidp part1 age1 sex allnotuk hhsizegp ethnic qualhigh incomeq housing hsbeds employ origin region firstwave
gen age1gp=age1
recode age1gp (16/19=16) (20/24=20) (25/29=25) (30/39=30) (40/49=40) (50/60=50)
recode sex (2=0)
drop age1
vl set, categorical(12) uncertain(19) dummy
vl create include=(age1gp ethnic hhsizegp origin)
vl create outcome=(part1)
vl create dummy=vldummy-outcome
vl create factors=vlcategorical+dummy
vl modify factors=factors-include
vl substitute ifactors=i.factors
vl substitute iinclude=i.include
vl substitute interact1=i.factors##i.factors
vl substitute interact2=i.include##i.factors
vl substitute interact3=i.include##i.include
lasso logit part1 $interact1 $interact2 $interact3 if hhsizegp~=1 

**************************************************************
**************************************************************
*  Hosmer-Lemeshow test for missingness model of deriving individual participation weight
estimates store ind_sel_logit

etable, estimates(ind_sel_logit) column(estimates) cstat(_r_b,  nformat(%7.2f)) cstat(_r_ci,  nformat(%7.2f)  cidelimiter(,)) mstat(aic) mstat(N) showstars showstarsnote title("Table. Missingness model for individual participation") export(v1.0_ind_sel_res_lasso.xlsx, replace)

* Hosmer-Lemeshow and Pearson tests
logit part1 i.age1gp i.sex i.allnotuk i.hhsizegp i.ethnic i.qualhigh i.incomeq i.housing i.hsbeds i.employ i.origin i.region i.firstwave ///
i.firstwave#i.region i.firstwave#i.hsbeds i.firstwave#i.incomeq i.firstwave#i.qualhigh i.firstwave#i.housing i.firstwave#i.sex i.firstwave#i.allnotuk ///     
i.region#i.hsbeds i.region#i.incomeq i.region#i.employ i.region#i.qualhigh i.region#i.housing i.region#i.sex i.region#i.allnotuk ///
i.hsbeds#i.incomeq  i.hsbeds#i.qualhigh i.hsbeds#i.housing i.hsbeds#i.sex ///
i.incomeq#i.employ i.incomeq#i.qualhigh i.incomeq#i.housing i.incomeq#i.sex i.incomeq#i.allnotuk ///
i.employ#i.qualhigh i.employ#i.housing i.employ#i.sex i.employ#i.allnotuk ///
i.qualhigh#i.housing  i.qualhigh#i.sex i.qualhigh#i.allnotuk ///
i.housing#i.sex i.housing#i.allnotuk ///
i.sex#i.allnotuk ///
i.age1gp#i.firstwave i.age1gp#i.region i.age1gp#i.hsbeds i.age1gp#i.incomeq i.age1gp#i.employ i.age1gp#i.qualhigh i.age1gp#i.housing i.age1gp#i.sex i.age1gp#i.allnotuk i.age1gp#i.ethnic i.age1gp#i.hhsizegp i.age1gp#i.origin ///
i.ethnic#i.region  i.ethnic#i.hsbeds  i.ethnic#i.incomeq  i.ethnic#i.employ  i.ethnic#i.qualhigh  i.ethnic#i.housing  i.ethnic#i.sex  i.ethnic#i.allnotuk  i.ethnic#i.hhsizegp  i.ethnic#i.origin ///
i.hhsizegp#i.region i.hhsizegp#i.hsbeds i.hhsizegp#i.incomeq i.hhsizegp#i.employ i.hhsizegp#i.qualhigh i.hhsizegp#i.housing i.hhsizegp#i.sex i.hhsizegp#i.origin ///
i.origin#i.region i.origin#i.hsbeds i.origin#i.incomeq i.origin#i.qualhigh i.origin#i.housing i.origin#i.sex 
estimates store ind_only_logit
etable, estimates(ind_sel_logit ind_only_logit) column(estimates) cstat(_r_b,  nformat(%7.2f)) mstat(aic) mstat(N) showstars showstarsnote title("Table. Missingness model for individual participation") export(v1.0_ind_sel_compare.xlsx, replace)

estat gof, group(10) table
save "ind_weight.dta", replace



predict prob
* this prob is the probability of individual response given his/her household is recruited/responded.
* If there is only one person in a responded household, then the prob is 1.
*hhsize is the number of persons in a household,
*So prob=1 if hhsizegp==1.
gen weight1=1 if hhsizegp==1
replace weight1=1/prob if hhsizegp~=1 & hhsizegp<.
replace prob=1 if hhsizegp==1


*************************************************************
*************************************************************
* weight assessment
twoway histogram weight1 if weight1!=. ,by(part1, title("Individual participation weights using IPW with all interactions")  ) xtitle("")

graph export "ind_weight_two_fig.tif", width(3160) height(1800) replace

dtable if weight1!=. & part1==1,continuous(weight1, statistics(min p1 p5 p25 p50 p75 p95 p99 max)) export("ind_weight_summarise_selected.xlsx", replace)
dtable if weight1!=. & part1==0,continuous(weight1 , statistics(min p1 p5 p25 p50 p75 p95 p99 max)) export("ind_weight_summarise_nonselected.xlsx", replace)

*weight overlap
generate wo=1/prob if part1==1
replace wo=1/(1-prob) if part1==0
twoway (histogram weight1 if part1==0 & weight1!=., start(1) width(0.3) color(red%30)) ///        
       (histogram weight1 if part1==1 & weight1!=., start(1) width(0.3) color(green%30)), ///   
       legend(order(1 "Non-selected individuals" 2 "Selected individuals" ) position(6) rows(1))  title("Individual participation weights using IPW with all interactions")  xtitle("")
	
graph export "ind_weight_overlap_one_fig.tif", width(3160) height(1800) replace



compress
keep pidp prob weight1
save initial_part_weights, replace


**# Bookmark #4 ------ to have the main variables-----------
* main variables are: unemployment status (only wave 9), and sleep duration(only wave 10), and
* mediator: wellbeing 
* confounders: age, sex, educational level, marital status, ethnic, and a binary variable indicating whether anyone in the household was UK-born.

clear
use "$rawdata\a_indresp"
keep pidp a_hidp a_marstat_dv a_qfhigh_dv a_scghq1_dv a_jbstat
save wave1_main, replace
use "$rawdata\b_indresp"
keep pidp b_hidp b_marstat_dv b_qfhigh_dv b_scghq1_dv b_jbstat
save wave2_main, replace 
use "$rawdata\c_indresp"
keep pidp c_hidp c_marstat_dv c_qfhigh_dv c_scghq1_dv c_jbstat
save wave3_main, replace
use "$rawdata\d_indresp"
keep pidp d_hidp d_marstat_dv d_qfhigh_dv d_scghq1_dv d_jbstat
save wave4_main, replace
use "$rawdata\e_indresp"
keep pidp e_hidp e_marstat_dv e_qfhigh_dv e_scghq1_dv e_jbstat
save wave5_main, replace
use "$rawdata\f_indresp"
keep pidp f_hidp f_marstat_dv f_qfhigh_dv f_scghq1_dv f_jbstat
save wave6_main, replace
use "$rawdata\g_indresp"
keep pidp g_hidp g_marstat_dv g_qfhigh_dv g_scghq1_dv g_jbstat
save wave7_main, replace
use "$rawdata\h_indresp"
keep pidp h_hidp h_marstat_dv h_qfhigh_dv h_scghq1_dv h_jbstat
save wave8_main, replace
use "$rawdata\i_indresp"
keep pidp i_hidp i_jbstat i_marstat_dv i_qfhigh_dv i_scghq1_dv
save wave9_main, replace
use "$rawdata\j_indresp"
keep pidp j_hidp j_hrs_slph j_hrs_slpm
save wave10_main, replace

clear
use "$rawdata\xwavedat"
keep pidp psu strata
save samplingvars, replace

**# Bookmark # for data ready

clear
use indall2560
*Drop those not in 25-60 at wave 10, i.e., no in 24-59 at wave 9
keep if i_age_dv>=24 & i_age_dv<60
keep pidp origin firstwave sex ethnic i_age_dv part1 origin hhsize incomeq employ qualhigh allnotuk hhsizegp basewgt housing 
compress
drop if basewgt>=.

* Analysis will exclude those who did not respond at first wave when they are an adult
merge m:1 pidp using initial_part_weights
drop if _merge==2
drop _merge
keep if part1==1 
keep if weight1!=.


merge 1:1 pidp using samplingvars
keep if _merge==3
drop _merge

merge 1:1 pidp using wave1_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave2_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave3_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave4_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave5_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave6_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave7_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave8_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave9_main
drop if _merge==2
drop _merge
merge 1:1 pidp using wave10_main  

drop if _merge==2
drop _merge


* The outcome: sleep duration
mvdecode j_hrs_slph j_hrs_slpm, mv(-9, -8, -7, -2, -1)
replace j_hrs_slpm=0 if j_hrs_slph!=. & j_hrs_slpm==.
generate sleep_duration=j_hrs_slph*60 + j_hrs_slpm
drop j_hrs_slph j_hrs_slpm

* The exposure: unemployment status 
mvdecode i_jbstat, mv(-9, -8, -7, -2, -1)
generate unemp=1 if i_jbstat==3 | i_jbstat==8
replace unemp=0 if i_jbstat!=3 & i_jbstat!=8 & i_jbstat!=.

* The mediator and confounders. Mediator is called wellbeing here.
mvdecode *_scghq1_dv *_qfhigh_dv *_marstat_dv, mv(-9, -8, -7, -2, -1)
*for mastat_dv, 0 means under 16 years old, so to be missing
mvdecode *_marstat_dv, mv(0)

*These 3 derived variables are for the weighting model
foreach var in "scghq1_dv" "qfhigh_dv" "marstat_dv"{
	generate `var'=i_`var'
	replace `var'=h_`var' if `var'==. & h_`var'>0
	replace `var'=g_`var' if `var'==. & g_`var'>0
	replace `var'=f_`var' if `var'==. & f_`var'>0
	replace `var'=e_`var' if `var'==. & e_`var'>0
	replace `var'=d_`var' if `var'==. & d_`var'>0
	replace `var'=c_`var' if `var'==. & c_`var'>0
	replace `var'=b_`var' if `var'==. & b_`var'>0
	replace `var'=a_`var' if `var'==. & a_`var'>0
}

rename (scghq1_dv qfhigh_dv marstat_dv) (wellbeing pqfhigh pmarstat)

recode pqfhigh (1/6=1) (7/12=2) (13/16=3) (96=4)
recode pmarstat (1/2=1) (3/6=0)

***** qualification and marital status from wave 9 or 8 that are used in the analysis model.
generate qfhigh=i_qfhigh_dv
replace qfhigh=h_qfhigh_dv if qfhigh==. & h_qfhigh_dv>0
recode qfhigh (1/6=1) (7/12=2) (13/16=3) (96=4) 
replace qfhigh=pqfhigh if qfhigh==. & pqfhigh==1

generate marstat=i_marstat_dv
replace marstat=h_marstat_dv if marstat==. & h_marstat_dv>0
recode marstat (1/2=1) (3/6=0)

generate agegp9=i_age_dv
recode agegp9 (24/29=0) (30/39=1) (40/49=2) (50/59=3)

*there are two people with sex=0, labeled as inconsistant, to be missing
replace sex=. if sex==0
recode sex (2=0) (1=1)

egen miss=rowmiss(sleep_duration unemp agegp9 sex ethnic allnotuk qfhigh marstat)
generate complete=1 if miss==0
* replace complete=0 if complete!=1 | complete==.
replace complete=0 if complete~=1

duplicates tag pidp, gen(dup)
list if dup>=1
drop dup

*drop if sleep duration is zero.
drop if sleep_duration==0 // 1 observation deleted.

count if complete==1

mvdecode *_jbstat, mv(-9, -8, -7, -2, -1)
gen pjbstat=i_jbstat
replace pjbstat=h_jbstat if pjbstat>=. & h_jbstat<.
replace pjbstat=g_jbstat if pjbstat>=. & g_jbstat<.
replace pjbstat=f_jbstat if pjbstat>=. & f_jbstat<.
replace pjbstat=e_jbstat if pjbstat>=. & e_jbstat<.
replace pjbstat=d_jbstat if pjbstat>=. & d_jbstat<.
replace pjbstat=c_jbstat if pjbstat>=. & c_jbstat<.
replace pjbstat=b_jbstat if pjbstat>=. & b_jbstat<.
replace pjbstat=a_jbstat if pjbstat>=. & a_jbstat<.
generate punemp=1 if pjbstat==3 | pjbstat==8
replace punemp=0 if pjbstat!=3 & pjbstat!=8 & pjbstat!=.

keep pidp sleep_duration unemp agegp9 sex ethnic allnotuk qfhigh marstat wellbeing complete basewgt weight1 punemp pqfhigh pmarstat psu strata
label define ukborn_lab 0 "At least one UK born" 1 "All not UK born"
label define agegp9_lab 0 "24-29" 1 "30-39" 2 "40-49" 3 "50-59"
label define qfhigh_lab 1 "Degree/diploma" 2 "A level/equivalent" 3 "GCSE/equivalent" 4 "None"
label define marstat_lab 0 "Other" 1 "Married/civil partnership/living as couple"
label define sex_lab 0 "Female" 1 "Male"
label define unemp_lab 0 "Employed" 1 "Umemployed"

label values allnotuk ukborn_lab
label values agegp9 agegp9_lab
label values qfhigh qfhigh_lab
label values marstat marstat_lab
label values sex sex_lab
label values unemp unemp_lab

label var sleep_duration "Sleep duration"
label var allnotuk "Whether anyone in household born in UK"
label var agegp9 "Age group"
label var qfhigh "Educational level"
label var marstat "Marital status"
label var sex "Sex"
label var unemp "Unemployment status"
label var ethnic "Ethnic group"


label var punemp "Proxy employment status"
label define punemp_lab 0 "Employed" 1 "Unemployed"
label values punemp punemp_lab

****** check whether there are missing values not coded well
codebook

mdesc sleep_duration unemp agegp9 sex ethnic allnotuk qfhigh marstat wellbeing complete basewgt weight1 punemp pqfhigh pmarstat psu strata 
save data_ready, replace

