set more off
clear all

global rawdata "*"
cd "*"


clear
use data_ready

**# Bookmark #1 get a table of variable characteristics used in the analysis model, the whole and complete observations
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

**# Bookmark #3 ----logit with no interactions -------

lasso logit complete i.punemp wellbeing i.pqfhigh i.pmarstat i.agegp9 i.ethnic i.sex i.allnotuk

predict p_logitnoi 
generate weight_logitnoi=1/p_logitnoi
generate weight_logitnoi_tot=weight_ab*weight_logitnoi
svyset, clear
svyset psu [pweight=weight_logitnoi_tot], strata(strata) singleunit(scaled)
svy: regress sleep_duration i.unemp i.agegp9 i.sex i.ethnic i.allnotuk i.qfhigh i.marstat  if complete==1

estimates store logit_noi

**# Bookmark #4 -------logit with three kinds of interactions-----

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
*proxy mediator and confounders
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


etable, estimates(sel_logit_i1 sel_logit_i2 sel_logit_i3) column(estimates) cstat(_r_b,  nformat(%7.2f)) cstat(_r_ci,  nformat(%7.2f)  cidelimiter(,)) mstat(aic) mstat(N) showstars showstarsnote title("Table 1. Estimates from selection models (first time) (lasso).") export(v2.0_sel_res_lasso.xlsx, replace)
etable, estimates(sel_logit_i1 sel_logit_i2 sel_logit_i3) column(estimates) mstat(aic) mstat(N) showstars showstarsnote title("Table 1. Estimates from selection models (first time) (lasso).") export(v2.0_sel_res_lasso_another.xlsx, replace)

