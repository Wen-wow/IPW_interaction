set more off
clear all

global rawdata "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu3\p2\011\working\data\6614stata_71AD291548B37BC63B02ADDCE17A3EF4A21E2B36918B337DCCAB5EB2B86D9363_V1\UKDA-6614-stata\stata\stata13_se\ukhls"
cd "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu3\p2\011\working\data\processed_try"

*********Note
*drop logit i2, so logit i1, i3, i4 is i1, i2, i3 in the paper

* --------- analysis------
*  the probability of being a complete case
*4 selection models for IPW

clear
use data_ready
/*
label var punemp "Proxy employment status"
label define punemp_lab 0 "Employed" 1 "Unemployed"
label values punemp punemp_lab
save data_ready,replace
*/
**# Bookmark #1 ------data cleaning----------


***** get a table of variable characteristics used in the analysis model
dtable ,continuous(sleep_duration  basewgt weight1 , statistics(mean sd))  factor(unemp agegp9 marstat qfhigh allnotuk ethnic sex, statistics(fvfrequency fvpercent )) title(Descriptive statistics for all variables used) export(v1.0_descriptive_stats.xlsx, replace)

dtable, continuous(sleep_duration basewgt weight1 unemp agegp9 marstat qfhigh allnotuk ethnic sex, statistics(count)) export(v1.0_descriptive_stats_count.xlsx, replace)

mdesc  sleep_duration  basewgt weight1 unemp agegp9 marstat qfhigh allnotuk ethnic sex

dtable if complete==1,continuous(sleep_duration  basewgt weight1 , statistics(mean sd))  factor(unemp agegp9 marstat qfhigh allnotuk ethnic sex, statistics(fvfrequency fvpercent )) title(Descriptive statistics for all variables used) export(v1.0_cc_descriptive_stats.xlsx, replace)

* weights for all methods
generate weight_ab=basewgt*weight1



**# Bookmark #2   ---- CCA ----------
svyset, clear
svyset psu [pweight=weight_ab], strata(strata) singleunit(scaled)
svy: regress sleep_duration i.unemp i.agegp9 i.sex i.ethnic i.allnotuk i.qfhigh i.marstat  if complete==1
estimates store cca

**# Bookmark #3 ----logit noi-------

lasso logit complete i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk
estimates store sel_logit_noi

predict p_logitnoi 
generate weight_logitnoi=1/p_logitnoi

generate weight_logitnoi_tot=weight_ab*weight_logitnoi

save "weight_logitnoi.dta", replace

svyset, clear
svyset psu [pweight=weight_logitnoi_tot], strata(strata) singleunit(scaled)
svy: regress sleep_duration i.unemp i.agegp9 i.sex i.ethnic i.allnotuk i.qfhigh i.marstat  if complete==1

estimates store logit_noi

**# Bookmark #4 -------logit i-----
*a#b causes Stata to include the interaction term between a and b in the model, but it does not include each of a and b separately (so you have to write out a and b separately to have a valid model). 
*a##b causes Stata to include a, and b, and the interaction term.

*(1) interactions with only proxy exposure and mediator
lasso logit complete i.punemp##c.wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk

estimates store sel_logit_i1

predict p_logiti1 
generate weight_logiti1=1/p_logiti1

generate weight_logiti_tot1=weight_ab*weight_logiti1

svyset, clear
svyset psu [pweight=weight_logiti_tot1], strata(strata) singleunit(scaled)
svy: regress sleep_duration i.unemp i.agegp9 i.sex i.ethnic i.allnotuk i.qfhigh i.marstat  if complete==1

estimates store logit_i1


*(2) interactions with proxy exposure and mediator + confounders, as well as
*mediator and confounders
lasso logit complete i.punemp#c.wellbeing i.punemp#i.pqfhigh i.punemp#i.pmarstat i.punemp#i.agegp9 i.punemp#i.ethnic i.punemp#i.sex i.punemp#i.allnotuk  c.wellbeing#i.pqfhigh c.wellbeing#i.pmarstat c.wellbeing#i.agegp9 c.wellbeing#i.ethnic c.wellbeing#i.sex c.wellbeing#i.allnotuk     i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk

estimates store sel_logit_i2

predict p_logiti2 
generate weight_logiti2=1/p_logiti2
generate weight_logiti_tot2=weight_ab*weight_logiti2
svyset, clear
svyset psu [pweight=weight_logiti_tot2], strata(strata) singleunit(scaled)
svy: regress sleep_duration i.unemp i.agegp9 i.sex i.ethnic i.allnotuk i.qfhigh i.marstat  if complete==1

estimates store logit_i2


* (3) all possible interactions
lasso logit complete i.punemp#c.wellbeing i.punemp#i.pqfhigh i.punemp#i.pmarstat i.punemp#i.agegp9 i.punemp#i.ethnic i.punemp#i.sex i.punemp#i.allnotuk  c.wellbeing#i.pqfhigh c.wellbeing#i.pmarstat c.wellbeing#i.agegp9 c.wellbeing#i.ethnic c.wellbeing#i.sex c.wellbeing#i.allnotuk i.pqfhigh#i.pmarstat i.pqfhigh#i.agegp9 i.pqfhigh#i.ethnic i.pqfhigh#i.sex i.pqfhigh#i.allnotuk i.pmarstat#i.agegp9 i.pmarstat#i.ethnic i.pmarstat#i.sex i.pmarstat#i.allnotuk i.agegp9#i.ethnic i.agegp9#i.sex i.agegp9#i.allnotuk i.ethnic#i.sex i.ethnic#i.allnotuk i.sex#i.allnotuk    i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk


estimates store sel_logit_i3

predict p_logiti3 
generate weight_logiti3=1/p_logiti3
generate weight_logiti_tot3=weight_ab*weight_logiti3
svyset, clear
svyset psu [pweight=weight_logiti_tot3], strata(strata) singleunit(scaled)
svy: regress sleep_duration i.unemp i.agegp9 i.sex i.ethnic i.allnotuk i.qfhigh i.marstat  if complete==1

estimates store logit_i3

save "all_weights.dta", replace

*clear
*use "all_weights.dta"

****************************************************
****************************************************
*weight assessment
label define select_lab 0 "Non-selected individuals" 1 "Selected individuals"
label values complete select_lab
* no interaction
twoway (histogram weight_logitnoi if complete==0 & weight_logitnoi!=., start(1) width(1) color(red%30)) ///        
       (histogram weight_logitnoi if complete==1 & weight_logitnoi!=., start(1) width(1) color(green%30)), ///   
       legend(order(1 "Non-selected individuals" 2 "Selected individuals" ) position(6) rows(1))  title("Complete-case weights using IPW (ii)")  xtitle("")
	
graph export "histogram_interaction_no_one_fig.tif", width(3160) height(1800) replace

twoway histogram weight_logitnoi if weight_logitnoi!=. ,by(complete, title("Complete-case weights using IPW with no interactions")  note("")) xtitle("")

graph export "histogram_interaction_no_two_fig.tif", width(3160) height(1800) replace

* one interaction
twoway (histogram weight_logiti1 if complete==0 & weight_logiti1!=., start(1) width(1) color(red%30)) ///        
       (histogram weight_logiti1 if complete==1 & weight_logiti1!=., start(1) width(1) color(green%30)), ///   
       legend(order(1 "Non-selected individuals" 2 "Selected individuals" ) position(6) rows(1))  title("Complete-case weights using IPW (ii)")  xtitle("")
	
graph export "histogram_interaction_ii_one_fig.tif", width(3160) height(1800) replace

twoway histogram weight_logiti1 if weight_logiti1!=. ,by(complete, title("Complete-case weights using IPW (ii)")  note("")) xtitle("")

graph export "histogram_interaction_ii_two_fig.tif", width(3160) height(1800) replace

* more interactions
twoway (histogram weight_logiti2 if complete==0 & weight_logiti2!=., start(1) width(1) color(red%30)) ///        
       (histogram weight_logiti2 if complete==1 & weight_logiti2!=., start(1) width(1) color(green%30)), ///   
       legend(order(1 "Non-selected individuals" 2 "Selected individuals" ) position(6) rows(1))  title("Complete-case weights using IPW (iii)")  xtitle("")
	
graph export "histogram_interaction_iii_one_fig.tif", width(3160) height(1800) replace

twoway histogram weight_logiti2 if weight_logiti2!=. ,by(complete, title("Complete-case weights using IPW (iii)")  note("")) xtitle("")

graph export "histogram_interaction_iii_two_fig.tif", width(3160) height(1800) replace

*all interactions
twoway (histogram weight_logiti3 if complete==0 & weight_logiti3!=., start(1) width(1) color(red%30)) ///        
       (histogram weight_logiti3 if complete==1 & weight_logiti3!=., start(1) width(1) color(green%30)), ///   
       legend(order(1 "Non-selected individuals" 2 "Selected individuals" ) position(6) rows(1))  title("Complete-case weights using IPW with all interactions")  xtitle("")
	
graph export "histogram_interaction_all_one_fig.tif", width(3160) height(1800) replace

twoway histogram weight_logiti3 if weight_logiti3!=. ,by(complete, title("Complete-case weights using IPW with all interactions")  note("")) xtitle("")

graph export "histogram_interaction_all_two_fig.tif", width(3160) height(1800) replace


dtable if weight_logitnoi!=. & complete==1,continuous(weight_logitnoi weight_logiti1 weight_logiti2 weight_logiti3 , statistics(min p1 p5 p25 p50 p75 p95 p99 max)) export("weight_summarise_selected.xlsx", replace)
dtable if weight_logitnoi!=. & complete==0,continuous(weight_logitnoi weight_logiti1 weight_logiti2 weight_logiti3 , statistics(min p1 p5 p25 p50 p75 p95 p99 max)) export("weight_summarise_nonselected.xlsx", replace)

**********************************************
**********************************************
* model diagnostics using Hosmer-Lemeshow test for missingness models
etable, estimates(sel_logit_noi sel_logit_i1 sel_logit_i2 sel_logit_i3) column(estimates) cstat(_r_b,  nformat(%7.2f)) cstat(_r_ci,  nformat(%7.2f)  cidelimiter(,)) mstat(aic) mstat(N) showstars showstarsnote title("Table 1. Estimates from missingness models (lasso).") export(v2.0_sel_res_lasso.xlsx, replace)

* Covariates selected for logit noi and logit i1 are the same, i.e. only i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk
* HL test for logit i1 (8 covariates)
logit complete i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk
estat gof, group(10) table

* HL test for logit i2 (8 covariates plus: i.punemp#i.pqfhigh i.punemp#i.pmarstat i.punemp#i.agegp9 i.punemp#i.ethnic i.punemp#i.sex i.punemp#i.allnotuk c.wellbeing#i.pqfhigh c.wellbeing#i.agegp9 c.wellbeing#i.sex)
logit complete i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk i.punemp#i.pqfhigh i.punemp#i.pmarstat i.punemp#i.agegp9 i.punemp#i.ethnic i.punemp#i.sex i.punemp#i.allnotuk c.wellbeing#i.pqfhigh c.wellbeing#i.agegp9 c.wellbeing#i.sex
estat gof, group(10) table

* HL test for logit i3 * HL test for logit i2 (8 covariates plus: i.punemp#i.pqfhigh i.punemp#i.pmarstat i.punemp#i.agegp9 i.punemp#i.ethnic i.punemp#i.sex i.punemp#i.allnotuk c.wellbeing#i.pqfhigh c.wellbeing#i.agegp9 i.pqfhigh#i.pmarstat i.pqfhigh#i.agegp9 i.pqfhigh#i.ethnic i.pqfhigh#i.sex i.pqfhigh#i.allnotuk i.pmarstat#i.agegp9 i.pmarstat#i.ethnic i.pmarstat#i.sex i.pmarstat#i.allnotuk i.agegp9#i.ethnic i.agegp9#i.sex i.agegp9#i.allnotuk i.ethnic#i.sex i.ethnic#i.allnotuk i.sex#i.allnotuk)
logit complete i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk i.punemp#i.pqfhigh i.punemp#i.pmarstat i.punemp#i.agegp9 i.punemp#i.ethnic i.punemp#i.sex i.punemp#i.allnotuk c.wellbeing#i.pqfhigh c.wellbeing#i.agegp9 i.pqfhigh#i.pmarstat i.pqfhigh#i.agegp9 i.pqfhigh#i.ethnic i.pqfhigh#i.sex i.pqfhigh#i.allnotuk i.pmarstat#i.agegp9 i.pmarstat#i.ethnic i.pmarstat#i.sex i.pmarstat#i.allnotuk i.agegp9#i.ethnic i.agegp9#i.sex i.agegp9#i.allnotuk i.ethnic#i.sex i.ethnic#i.allnotuk i.sex#i.allnotuk
estat gof, group(10) table


*----------output illustrative example results

etable, estimates(cca logit_noi logit_i1 logit_i2 logit_i3) column(estimates) cstat(_r_b,  nformat(%7.2f)) cstat(_r_ci,  nformat(%7.2f)  cidelimiter(,)) mstat(N) showstars showstarsnote title("Table 2. Illustrative example result (lasso).") export(v2.0_res_lasso.xlsx, replace)
etable, estimates(cca logit_noi logit_i1 logit_i2 logit_i3) column(estimates) cstat(_r_b,  nformat(%7.0f)) cstat(_r_ci,  nformat(%7.0f)  cidelimiter(,)) mstat(N) showstars showstarsnote title("Table 2. Illustrative example result (lasso).") export(v2.0_res_lasso_no_decimal.xlsx, replace)
