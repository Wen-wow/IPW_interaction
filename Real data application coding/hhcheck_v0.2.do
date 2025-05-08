set more off
clear all


global rawdata "*"
cd "*"


use indall2560
keep if waverecruit==1 & firstwave==9
keep pidp i_hidp
save check19, replace
use indall2560
keep if waverecruit==1 & firstwave==8
keep pidp h_hidp
save check18, replace
use indall2560
keep if waverecruit==1 & firstwave==7
keep pidp g_hidp
save check17, replace
use indall2560
keep if waverecruit==1 & firstwave==6
keep pidp f_hidp
save check16, replace
use indall2560
keep if waverecruit==1 & firstwave==5
keep pidp e_hidp
save check15, replace
use indall2560
keep if waverecruit==1 & firstwave==4
keep pidp d_hidp
save check14, replace
use indall2560
keep if waverecruit==1 & firstwave==3
keep pidp c_hidp
save check13, replace
use indall2560
keep if waverecruit==1 & firstwave==2
keep pidp b_hidp
save check12, replace

use indall2560
keep if waverecruit==6 & firstwave==9
keep pidp i_hidp
save check69, replace
use indall2560
keep if waverecruit==6 & firstwave==8
keep pidp h_hidp
save check68, replace
use indall2560
keep if waverecruit==6 & firstwave==7
keep pidp g_hidp
save check67, replace

use "$rawdata\a_indall"
keep a_hidp pidp
rename pidp personida
save wave1, replace
use "$rawdata\b_indall"
keep pidp b_hidp
rename pidp personidb
save wave2, replace
use "$rawdata\c_indall"
keep pidp c_hidp
rename pidp personidc
save wave3, replace
use "$rawdata\d_indall"
keep pidp d_hidp
rename pidp personidd
save wave4, replace
use "$rawdata\e_indall"
keep pidp e_hidp
rename pidp personide
save wave5, replace
use "$rawdata\f_indall"
keep pidp f_hidp 
rename pidp personidf
save wave6, replace
use "$rawdata\g_indall"
keep g_hidp pidp
rename pidp personidg
save wave7, replace
use "$rawdata\h_indall"
keep pidp h_hidp
rename pidp personidh
save wave8, replace
use "$rawdata\i_indall"
keep pidp i_hidp
rename pidp personidi
save wave9, replace
use "$rawdata\j_indall"
keep pidp j_hidp
rename pidp personidj
save wave10, replace
use "$rawdata\k_indall"
keep pidp k_hidp
rename pidp personidk
save wave11, replace
use "$rawdata\l_indall"
keep pidp l_hidp 
rename pidp personidl
save wave12, replace

use check19
merge m:m i_hidp using wave9
keep if _merge==3
drop _merge
keep personidi i_hidp
rename personidi pidp
save check19, replace
use wave1
rename personida pidp
merge 1:1 pidp using check19
keep if _merge==3
drop _merge
keep i_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave19, replace

use check18
merge m:m h_hidp using wave8
keep if _merge==3
drop _merge
keep personidh h_hidp
rename personidh pidp
save check18, replace
use wave1
rename personida pidp
merge 1:1 pidp using check18
keep if _merge==3
drop _merge
keep h_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave18, replace

use check17
merge m:m g_hidp using wave7
keep if _merge==3
drop _merge
keep personidg g_hidp
rename personidg pidp
save check17, replace
use wave1
rename personida pidp
merge 1:1 pidp using check17
keep if _merge==3
drop _merge
keep g_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave17, replace

use check16
merge m:m f_hidp using wave6
keep if _merge==3
drop _merge
keep personidf f_hidp
rename personidf pidp
save check16, replace
use wave1
rename personida pidp
merge 1:1 pidp using check16
keep if _merge==3
drop _merge
keep f_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave16, replace

use check15
merge m:m e_hidp using wave5
keep if _merge==3
drop _merge
keep personide e_hidp
rename personide pidp
save check15, replace
use wave1
rename personida pidp
merge 1:1 pidp using check15
keep if _merge==3
drop _merge
keep e_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave15, replace

use check14
merge m:m d_hidp using wave4
keep if _merge==3
drop _merge
keep personidd d_hidp
rename personidd pidp
save check14, replace
use wave1
rename personida pidp
merge 1:1 pidp using check14
keep if _merge==3
drop _merge
keep d_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave14, replace

use check13
merge m:m c_hidp using wave3
keep if _merge==3
drop _merge
keep personidc c_hidp
rename personidc pidp
save check13, replace
use wave1
rename personida pidp
merge 1:1 pidp using check13
keep if _merge==3
drop _merge
keep c_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave13, replace

use check12
merge m:m b_hidp using wave2
keep if _merge==3
drop _merge
keep personidb b_hidp
rename personidb pidp
save check12, replace
use wave1
rename personida pidp
merge 1:1 pidp using check12
keep if _merge==3
drop _merge
keep b_hidp a_hidp 
rename a_hidp hidpwave1
duplicates drop 
save check_wave12, replace

use check69
merge m:m i_hidp using wave9
keep if _merge==3
drop _merge
keep personidi i_hidp
rename personidi pidp
save check69, replace
use wave6
rename personidf pidp
merge 1:1 pidp using check69
keep if _merge==3
drop _merge
keep i_hidp f_hidp 
rename f_hidp hidpwave6
duplicates drop 
save check_wave69, replace

use check68
merge m:m h_hidp using wave8
keep if _merge==3
drop _merge
keep personidh h_hidp
rename personidh pidp
save check68, replace
use wave6
rename personidf pidp
merge 1:1 pidp using check68
keep if _merge==3
drop _merge
keep h_hidp f_hidp 
rename f_hidp hidpwave6
duplicates drop 
save check_wave68, replace

use check67
merge m:m g_hidp using wave7
keep if _merge==3
drop _merge
keep personidg g_hidp
rename personidg pidp
save check67, replace
use wave6
rename personidf pidp
merge 1:1 pidp using check67
keep if _merge==3
drop _merge
keep g_hidp f_hidp 
rename f_hidp hidpwave6
duplicates drop 
save check_wave67, replace

use indall2560
merge m:1 i_hidp using check_wave19
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 h_hidp using check_wave18
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 g_hidp using check_wave17
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 f_hidp using check_wave16
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 e_hidp using check_wave15
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 d_hidp using check_wave14
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 c_hidp using check_wave13
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 b_hidp using check_wave12
replace a_hidp=hidpwave1 if _merge==3 & a_hidp>=.
drop _merge hidpwave1
merge m:1 i_hidp using check_wave69
replace f_hidp=hidpwave6 if _merge==3 & f_hidp>=.
drop _merge hidpwave6
merge m:1 h_hidp using check_wave68
replace f_hidp=hidpwave6 if _merge==3 & f_hidp>=.
drop _merge hidpwave6
merge m:1 g_hidp using check_wave67
replace f_hidp=hidpwave6 if _merge==3 & f_hidp>=.
drop _merge hidpwave6

save indall2560, replace