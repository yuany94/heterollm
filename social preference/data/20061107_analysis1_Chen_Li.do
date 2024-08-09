clear
capture log close
set memo 100m
set matsize 800
set more 1

*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*$$$ Chen and Li: "Group Identity and Social Preferences "                  $$
*$$$ Note: Table 5 top panel on expected earnings is in a seperate program  $$
*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


***********************************
** Table 1: Summary for Sessions **
***********************************
use ID-behavior-data.dta, clear
keep if stage == 3 & round == 1
tab treatment


**********************************************************
** Table 2 Distribution Preferences parameters: Control **
**********************************************************
use ID-behavior-data.dta, clear
	keep if stage == 3 & (treatment == "original" | treatment == "control")
	keep if myrole == 2            /*player B only*/
	keep if treatment == "control" /*Control sessions only */

** Generate the guilt/envy index for B's act1 (r1 and s1) and act2 (r2 and s2)
	gen r1 = .
	gen s1 = .
	gen r2 = .
	gen s2 = .
	gen c = .
	gen q = . 

	replace r1 = 1 if payoffB_Bact1 > payoffA_Bact1
	replace s1 = 1 if payoffB_Bact1 < payoffA_Bact1
	replace r2 = 1 if payoffB_Bact2 > payoffA_Bact2
	replace s2 = 1 if payoffB_Bact2 < payoffA_Bact2

	replace r1 = 0 if r1 == .
	replace s1 = 0 if s1 == .
	replace r2 = 0 if r2 == .
	replace s2 = 0 if s2 == .

** dummy variable for choice of action 1 (Left)
	gen left = (actforsame == 1)

** Generate the X variables
	gen x0 = payoffB_Bact1 - payoffB_Bact2
	gen x1 = r1*payoffA_Bact1 - r2*payoffA_Bact2 + r2*payoffB_Bact2 - r1*payoffB_Bact1  
	gen x2 = s1*payoffA_Bact1 - s2*payoffA_Bact2 + s2*payoffB_Bact2 - s1*payoffB_Bact1
	gen x1plusx2 = x1 + x2


** Maximum likelihood estimation
	capture program drop _all 
	program define recip
		version 7.0
		args todo b lnf
		tempvar theta1
		mleval `theta1' = `b'
		mlsum `lnf' = log(exp(`theta1')/(exp(`theta1')+1))*$ML_y1 + log(1/(exp(`theta1')+1))*(1-$ML_y1)
	end

** gamma: logit model parameter (paper page 16)  
** rho: charity parameter
** sigma: envy parameter

ml model d0 recip (left = x1 x2 x0 , nocons)
ml maximize

	matrix V = e(V)

	scalar gamma    = round(_b[x0], 0.0001)
	scalar se_gamma = round(_se[x0], 0.00001)
	scalar t_gamma  = round(_b[x0]/_se[x0], 0.001)

	scalar rho    = round((_b[x1])/(_b[x0]), 0.0001)
	matrix list V
	scalar cov_x0_x1 = V[3,1]
	scalar se_rho = round(sqrt([(_b[x1]^2)*(_se[x0]^2)/(_b[x0]^4)] + [(_se[x1]^2)/(_b[x0]^2)] - [(2*_b[x1])*cov_x0_x1/(_b[x0]^3)]), 0.00001) 
	scalar t_rho  = round(rho/se_rho, 0.001)

	scalar sigma  = round((_b[x2])/(_b[x0]), 0.0001)
	matrix list V
	scalar cov_x0_x2 = V[3,2]
	scalar se_sigma = round(sqrt([(_b[x2]^2)*(_se[x0]^2)/(_b[x0]^4)] + [(_se[x2]^2)/(_b[x0]^2)] - [(2*_b[x2])*cov_x0_x2/(_b[x0]^3)]), 0.00001) 
	scalar t_sigma  = round(sigma/se_sigma, 0.001)

	** Table2 Top Panel
	* Our estimation of Gamma is very similar to Charness&Rabin
	display "Gamma (std err, t stat) is " gamma  "(" se_gamma  ", " t_gamma ")"
	display "rho   (std err, t stat) is " rho    "(" se_rho    ", " t_rho ")"
	display "sigma (std err, t stat) is " sigma  "(" se_sigma  ", " t_sigma ")"


*******************************************************************
** Table 2 Disibution Preferences parameters: Original Treatment **
*******************************************************************
use ID-behavior-data.dta, clear
	keep if stage == 3 & treatment == "original" /*Original eatment sessions only */
	keep if myrole == 2                        /*player B only*/

** Generate the guilt/envy index for B's act1 (r1 and s1) and act2 (r2 and s2)
	gen r1 = .
	gen s1 = .
	gen r2 = .
	gen s2 = .

	replace r1 = 1 if payoffB_Bact1 > payoffA_Bact1
	replace s1 = 1 if payoffB_Bact1 < payoffA_Bact1
	replace r2 = 1 if payoffB_Bact2 > payoffA_Bact2
	replace s2 = 1 if payoffB_Bact2 < payoffA_Bact2

	replace r1 = 0 if r1 == .
	replace s1 = 0 if s1 == .
	replace r2 = 0 if r2 == .
	replace s2 = 0 if s2 == .

** Generate the X variables
	gen x0 = payoffB_Bact1 - payoffB_Bact2
	gen x1 = r1*payoffA_Bact1 - r2*payoffA_Bact2 + r2*payoffB_Bact2 - r1*payoffB_Bact1  
	gen x2 = s1*payoffA_Bact1 - s2*payoffA_Bact2 + s2*payoffB_Bact2 - s1*payoffB_Bact1
	gen x1plusx2 = x1 + x2

save temp-dist.dta, replace

** reshape the data to the long format
use temp-dist.dta, clear
	keep treatment date round subject actforsame r1 s1 r2 s2 x0 x1 x2 x1plusx2
	rename actforsame act
	gen ingr = 1
	save temp-dist-ingr.dta, replace

use temp-dist.dta, clear
	keep treatment date round subject actforother r1 s1 r2 s2 x0 x1 x2 x1plusx2
	rename actforother act
	gen ingr = 0
	save temp-dist-outgr.dta, replace

use temp-dist-ingr.dta, replace
	append using temp-dist-outgr.dta

	** dummy variable for choice of action 1 (Left)
		gen left = (act == 1)


* Interact the X variables with the In-group indicator
gen ingr_x1 = ingr*x1
gen ingr_x2 = ingr*x2
gen ingr_x1x2 = ingr*x1plusx2

summ act x1 x2 ingr_x1 ingr_x2 x0

** Maximum likelihood estimation 
** This program was used to replicate Charness&Rabin results
capture program drop _all 
program define recip
	version 7.0
	args todo b lnf
	tempvar theta1
	mleval `theta1' = `b'
	mlsum `lnf' = log(exp(`theta1')/(exp(`theta1')+1))*$ML_y1 + log(1/(exp(`theta1')+1))*(1-$ML_y1)
end


ml model d0 recip (left = x1 x2 ingr_x1 ingr_x2 x0, nocons)
ml maximize

	matrix V = e(V)

	scalar gamma    = round(_b[x0], 0.0001)
	scalar se_gamma = round(_se[x0], 0.00001)
	scalar t_gamma  = round(_b[x0]/_se[x0], 0.001)

	scalar rho    = round((_b[x1])/(_b[x0]), 0.0001)
	matrix list V
	scalar cov_x0_x1 = V[5,1]
	scalar se_rho = round(sqrt([(_b[x1]^2)*(_se[x0]^2)/(_b[x0]^4)] + [(_se[x1]^2)/(_b[x0]^2)] - [(2*_b[x1])*cov_x0_x1/(_b[x0]^3)]), 0.00001) 
	scalar t_rho  = round(rho/se_rho, 0.001)

	scalar sigma  = round((_b[x2])/(_b[x0]), 0.0001)
	matrix list V
	scalar cov_x0_x2 = V[5,2]
	scalar se_sigma = round(sqrt([(_b[x2]^2)*(_se[x0]^2)/(_b[x0]^4)] + [(_se[x2]^2)/(_b[x0]^2)] - [(2*_b[x2])*cov_x0_x2/(_b[x0]^3)]), 0.00001) 
	scalar t_sigma  = round(sigma/se_sigma, 0.001)

	scalar a1      = round((_b[ingr_x1])/(_b[x1]), 0.0001)
	matrix list V
	scalar cov_ingrx1_x1 = V[3,1]
	scalar se_a = round(sqrt([(_b[ingr_x1]^2)*(_se[x1]^2)/(_b[x1]^4)] + [(_se[ingr_x1]^2)/(_b[x1]^2)] - [(2*_b[ingr_x1])*cov_ingrx1_x1/(_b[x1]^3)]), 0.00001) 
	scalar t_a  = round(a1/se_a, 0.001)

	scalar b      = round((_b[ingr_x2])/(_b[x2]), 0.0001)
	matrix list V
	scalar cov_ingrx2_x2 = V[4,2]
	scalar se_b = round(sqrt([(_b[ingr_x2]^2)*(_se[x2]^2)/(_b[x2]^4)] + [(_se[ingr_x2]^2)/(_b[x2]^2)] - [(2*_b[ingr_x2])*cov_ingrx2_x2/(_b[x2]^3)]), 0.00001) 
	scalar t_b  = round(b/se_b, 0.001)

display "gamma (std err, t stat) is " gamma  "(" se_gamma  ", " t_gamma ")"
display "rho   (std err, t stat) is " rho    "(" se_rho    ", " t_rho   ")"
display "sigma (std err, t stat) is " sigma  "(" se_sigma  ", " t_sigma ")"
display "a     (std err, t stat) is " a1      "(" se_a      ", " t_a     ")"
display "b     (std err, t stat) is " b      "(" se_b      ", " t_b     ")"


erase temp-dist.dta
erase temp-dist-ingr.dta
erase temp-dist-outgr.dta


**********************************************************************
** Table 3 Panel A: Determinants of player B's Positive Reciprocity **
**********************************************************************
use ID-behavior-data.dta, clear
	keep if stage == 3 & (treatment == "original" | treatment == "control")
	keep if game == "Resp 5a" | game == "Resp 1a" | game == "Resp 2a" | game == "Resp 3" | game == "Resp 4" | game == "Resp 8" | game == "Resp 9" 
	keep if myrole == 2 /*keep player B*/

** Generate unique identifier for individual participant
catenate sess_subj = date subj

** Dependent var: reward or not? 
	gen reward_ingr = .
	replace reward_ingr = 1 if actforsame == 2 /*"Right" is the action of rewarding*/
	replace reward_ingr = 0 if actforsame == 1

	gen reward_outgr = .
	replace reward_outgr = 1 if actforother == 2 /*"Right" is the action of rewarding*/
	replace reward_outgr = 0 if actforother == 1


** indep var 1: B's Cost to reward (in 100 tokens)
	gen cost = (payoffB_Bact1 - payoffB_Bact2)/100 

** indep var 2: Benefit to A if B rewards (in 100 tokens)
	gen benefitA = (payoffA_Bact2 - payoffA_Bact1)/100 

** indep var 3: B's payoff behind A if B rewards (in 100 tokens)
	gen Bbehind = (payoffA_Bact2 - payoffB_Bact2)/100
 
save temp-pos.dta, replace

*&&&&&&&&&&&&&&&&&&&&&&&&&&&& Control &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

use temp-pos.dta, clear
	keep if treatment == "control"
	rename reward_ingr reward
	
	* Table 3 Panel A column 1
	dlogit2 reward cost benefitA Bbehind, cluster(sess_subj)



*&&&&&&&&&&&&&&&&&&&&&&&&&&&& Treatment &&&&&&&&&&&&&&&&&&&&&&&&&&&&

** First, reshape the data to the long format
use temp-pos.dta, clear
	keep if treatment == "original"
	keep treatment date stage round subject sess_subj reward_ingr cost benefitA Bbehind 	
	rename reward_ingr reward
	gen ingr = 1
	save temp-ingr.dta, replace

use temp-pos.dta, clear
	keep if treatment == "original"
	keep treatment date stage round subject sess_subj reward_outgr cost benefitA Bbehind 	
	rename reward_outgr reward
	gen ingr = 0
	save temp-outgr.dta, replace

use temp-ingr.dta, replace
append using temp-outgr.dta

** Table 3 Panel A column 2: without interaction
dlogit2 reward ingr cost benefitA Bbehind, cluster(sess_subj)

** Table 3 Panel A column 3: with interactions
**     The program below computes the marginal effects 
**     (note dlogit2 doesn't apply here because of the interactions with a dummy variable)

		gen cost_ingr = cost*ingr
		gen benefitA_ingr = benefitA*ingr
		gen Bbehind_ingr = Bbehind*ingr

	logit reward ingr cost benefitA Bbehind cost_ingr benefitA_ingr Bbehind_ingr, cluster(sess_subj)

	quietly summarize ingr if e(sample)
	local mingr = r(mean) 

	quietly summarize cost if e(sample)
	local mcostB = r(mean) 

	quietly summarize benefitA if e(sample)
	local mbenefitA = r(mean) 

	quietly summarize Bbehind if e(sample)
	local mBbehind = r(mean) 

	quietly summarize cost_ingr if e(sample)
	local mcostingr = r(mean) 

	quietly summarize benefitA_ingr if e(sample)
	local mbenefitAingr = r(mean) 

	quietly summarize Bbehind_ingr if e(sample)
	local mBbehindingr = r(mean) 


	local xb  _b[_cons] + _b[ingr]*`mingr' + _b[cost]*`mcostB' + _b[benefitA]*`mbenefitA' + _b[Bbehind]*`mBbehind'  ///
                          + _b[cost_ingr]*`mcostB'*`mingr' + _b[benefitA_ingr]*`mbenefitA'*`mingr'                    ///
                          + _b[Bbehind_ingr]*`mBbehind'*`mingr'  

	local xb1 _b[_cons] + _b[ingr]                                        ///
                          + (_b[cost] + _b[cost_ingr])*`mcostB'             ///
                          + (_b[benefitA] + _b[benefitA_ingr])*`mbenefitA'  ///
                          + (_b[Bbehind] + _b[Bbehind_ingr])*`mBbehind'  
                           
	local xb0 _b[_cons] + _b[cost]*`mcostB' + _b[benefitA]*`mbenefitA' + _b[Bbehind]*`mBbehind'  

	local xb1_ingr  _b[_cons] + _b[ingr] + _b[cost]*`mcostB' + _b[benefitA]*`mbenefitA'    ///
                                + _b[Bbehind]*`mBbehind' + _b[cost_ingr]*`mcostB'*`mingr'    ///
                                + _b[benefitA_ingr]*`mbenefitA'*`mingr' + _b[Bbehind_ingr]*`mBbehind'*`mingr'  

	local xb0_ingr  _b[_cons] + _b[cost]*`mcostB' + _b[benefitA]*`mbenefitA' + _b[Bbehind]*`mBbehind'      ///
                                + _b[cost_ingr]*`mcostB'*`mingr' + _b[benefitA_ingr]*`mbenefitA'*`mingr'     ///
                                + _b[Bbehind_ingr]*`mBbehind'*`mingr'  

	* "ingr": marg effect, std error, p-value 
	predictnl double dydingr = (exp(`xb1_ingr')/(1+exp(`xb1_ingr'))) - (exp(`xb0_ingr')/(1+exp(`xb0_ingr'))) in 1, ///
					se(singr) p(pingr)
	list dydingr singr pingr in 1

	* "cost": marg effect, std error, p-value
	predictnl double dydcost = (_b[cost])*exp(`xb')/(1+exp(`xb'))/(1+exp(`xb')) in 1, se(scost) p(pcost)
	list dydcost scost pcost in 1

	* "benefitA"
	predictnl double dydbenefitA = (_b[benefitA])*exp(`xb')/(1+exp(`xb'))/(1+exp(`xb')) in 1,  ///
			            se(sbenefitA) p(pbenefitA)
	list dydbenefitA sbenefitA pbenefitA in 1

	* "Bbehind"
	predictnl double dydbehind = (_b[Bbehind])*exp(`xb')/(1+exp(`xb'))/(1+exp(`xb')) in 1, se(sbehind) p(pbehind)
	list dydbehind sbehind pbehind in 1

	* "cost_ingr"
	predictnl double dydcostingr = ((_b[cost] + _b[cost_ingr])*exp(`xb1')/(1+exp(`xb1'))/(1+exp(`xb1'))) ///
                                   - (_b[cost]*exp(`xb0')/(1+exp(`xb0'))/(1+exp(`xb0'))) in 1, se(scostingr) p(pcostingr)
	list dydcostingr scostingr pcostingr in 1

	* "benefitA_ingr"
	predictnl double dydbenefitAingr = ((_b[benefitA] + _b[benefitA_ingr])*exp(`xb1')/(1+exp(`xb1'))/(1+exp(`xb1'))) ///
                    - (_b[benefitA]*exp(`xb0')/(1+exp(`xb0'))/(1+exp(`xb0'))) in 1, se(sbenefitAingr) p(pbenefitAingr)
	list dydbenefitAingr sbenefitAingr pbenefitAingr in 1

	* "Bbehind_ingr"
	predictnl double dydbehindingr = ((_b[Bbehind] + _b[Bbehind_ingr])*exp(`xb1')/(1+exp(`xb1'))/(1+exp(`xb1'))) ///
                      - (_b[benefitA]*exp(`xb0')/(1+exp(`xb0'))/(1+exp(`xb0'))) in 1, se(sbehindingr) p(pbehindingr)
	list dydbehindingr sbehindingr pbehindingr in 1


	** compared to dlogit2 results
	dlogit2 reward ingr cost benefitA Bbehind cost_ingr benefitA_ingr Bbehind_ingr, cluster(sess_subj)

erase temp-pos.dta
erase temp-ingr.dta
erase temp-outgr.dta


**********************************************************************
** Table 3 Panel B: Determinants of player B's negative reciprocity **
**********************************************************************
use ID-behavior-data.dta, clear

	keep if stage == 3 & (treatment == "original"  | treatment == "control")
	keep if game == "Resp 2b" | game == "Resp 10" | game == "Resp 11" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d"  
	keep if myrole == 2 /*keep player B*/

** Generate unique identifier for individual participant
catenate sess_subj = date subj

** Dependent var: punish or not? 
	gen punish_ingr = .
	/*"Left" is the action of punishing for these games*/	
		replace punish_ingr = 1 if actforsame == 1 & (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
		replace punish_ingr = 0 if actforsame == 2 & (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
	/*"Right" is the action of punishing for these games*/	
		replace punish_ingr = 1 if actforsame == 2 & (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d") 
		replace punish_ingr = 0 if actforsame == 1 & (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d") 


	gen punish_outgr = .
	/*"Left" is the action of punishing for these games*/
		replace punish_outgr = 1 if actforother == 1 & (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
		replace punish_outgr = 0 if actforother == 2 & (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
	/*"Right" is the action of punishing for these games*/
		replace punish_outgr = 1 if actforother == 2 & (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d"  ) 
		replace punish_outgr = 0 if actforother == 1 & (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d"  ) 


** indep var 1: B's Cost to punish (in 100 tokens)
	gen cost = .
	* note: in game Resp 2b, player B gains 25 tokens when punishing
	/*"Left" is the action of punishing for these games*/	
		replace cost = (payoffB_Bact2 - payoffB_Bact1)/100 if (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
	/*"Right" is the action of punishing for these games*/	
		replace cost = (payoffB_Bact1 - payoffB_Bact2)/100 if (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d"  ) 

** indep var 2: Damage to A if B punishes (in 100 tokens)
	gen damageA = (payoffA_Bact2 - payoffA_Bact1)/100 
	replace damageA = (payoffA_Bact2 - payoffA_Bact1)/100 if (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
	replace damageA = (payoffA_Bact1 - payoffA_Bact2)/100 if (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d"  ) 

** indep var 3: B's payoff ahead of A if B punishes (in 100 tokens)
	gen Bahead = (payoffA_Bact2 - payoffB_Bact2)/100
	replace Bahead = (payoffB_Bact1 - payoffA_Bact1)/100 if (game == "Resp 2b" | game == "Resp 1b" | game == "Resp 6" | game == "Resp 7") 
	replace Bahead = (payoffB_Bact2 - payoffA_Bact2)/100 if (game == "Resp 10" | game == "Resp 11" | game == "Resp 12" | game == "Resp 13a" | game == "Resp 13b" | game == "Resp 13c" | game == "Resp 13d"  ) 

save temp-neg.dta, replace


*&&&&&&&&&&&&&&&&&&&&&&&& Control &&&&&&&&&&&&&&&&&&&&&&&&&&&&

use temp-neg.dta, clear
	keep if treatment == "control"
	rename punish_ingr punish

	* Table 3 Panel B: column 1
	dlogit2 punish cost damageA Bahead, cluster(sess_subj)


*&&&&&&&&&&&&&&&&&&&&&&&& Treatment &&&&&&&&&&&&&&&&&&&&&&&&&&&&

** First, reshape the data to the long format
use temp-neg.dta, clear
	keep if treatment == "original"
	keep treatment date stage round subject sess_subj punish_ingr cost damageA Bahead 	
	rename punish_ingr punish
	gen ingr = 1
	save temp-ingr.dta, replace

use temp-neg.dta, clear
	keep if treatment == "original"
	keep treatment date stage round subject sess_subj punish_outgr cost damageA Bahead	
	rename punish_outgr punish
	gen ingr = 0
	save temp-outgr.dta, replace

use temp-ingr.dta, replace
append using temp-outgr.dta

** Table 3 Panel B: column 2 (without interaction)
	dlogit2 punish ingr cost damageA Bahead, cluster(sess_subj)

** Table 3 Panel B: column 3 (with interactions)
**     The program below computes the marginal effects 
**     (note dlogit2 doesn't apply here because of the interactions with a dummy variable)

		gen cost_ingr    = cost*ingr
		gen damageA_ingr = damageA*ingr
		gen Bahead_ingr  = Bahead*ingr

	logit punish ingr cost damageA Bahead cost_ingr damageA_ingr Bahead_ingr, cluster(sess_subj)

	quietly summarize ingr if e(sample)
	local mingr = r(mean) 

	quietly summarize cost if e(sample)
	local mcostB = r(mean) 

	quietly summarize damageA if e(sample)
	local mdamageA = r(mean) 

	quietly summarize Bahead if e(sample)
	local mBahead = r(mean) 

	quietly summarize cost_ingr if e(sample)
	local mcostingr = r(mean) 

	quietly summarize damageA_ingr if e(sample)
	local mdamageAingr = r(mean) 

	quietly summarize Bahead_ingr if e(sample)
	local mBaheadingr = r(mean) 


	local xb  _b[_cons] + _b[ingr]*`mingr' + _b[cost]*`mcostB' + _b[damageA]*`mdamageA' + _b[Bahead]*`mBahead'  ///
                          + _b[cost_ingr]*`mcostB'*`mingr' + _b[damageA_ingr]*`mdamageA'*`mingr'                    ///
                          + _b[Bahead_ingr]*`mBahead'*`mingr'  

	local xb1 _b[_cons] + _b[ingr]                                        ///
                          + (_b[cost] + _b[cost_ingr])*`mcostB'             ///
                          + (_b[damageA] + _b[damageA_ingr])*`mdamageA'  ///
                          + (_b[Bahead] + _b[Bahead_ingr])*`mBahead'  
                           
	local xb0 _b[_cons] + _b[cost]*`mcostB' + _b[damageA]*`mdamageA' + _b[Bahead]*`mBahead'  

	local xb1_ingr  _b[_cons] + _b[ingr] + _b[cost]*`mcostB' + _b[damageA]*`mdamageA'    ///
                                + _b[Bahead]*`mBahead' + _b[cost_ingr]*`mcostB'*`mingr'    ///
                                + _b[damageA_ingr]*`mdamageA'*`mingr' + _b[Bahead_ingr]*`mBahead'*`mingr'  

	local xb0_ingr  _b[_cons] + _b[cost]*`mcostB' + _b[damageA]*`mdamageA' + _b[Bahead]*`mBahead'      ///
                                + _b[cost_ingr]*`mcostB'*`mingr' + _b[damageA_ingr]*`mdamageA'*`mingr'     ///
                                + _b[Bahead_ingr]*`mBahead'*`mingr'  

	* "ingr": marg effect, std error, p-value 
	predictnl double dydingr = (exp(`xb1_ingr')/(1+exp(`xb1_ingr'))) - (exp(`xb0_ingr')/(1+exp(`xb0_ingr'))) in 1, ///
					se(singr) p(pingr)
	list dydingr singr pingr in 1

	* "cost": marg effect, std error, p-value
	predictnl double dydcost = (_b[cost])*exp(`xb')/(1+exp(`xb'))/(1+exp(`xb')) in 1, se(scost) p(pcost)
	list dydcost scost pcost in 1

	* "damageA"
	predictnl double dyddamageA = (_b[damageA])*exp(`xb')/(1+exp(`xb'))/(1+exp(`xb')) in 1,  ///
			            se(sdamageA) p(pdamageA)
	list dyddamageA sdamageA pdamageA in 1

	* "Bahead"
	predictnl double dydahead = (_b[Bahead])*exp(`xb')/(1+exp(`xb'))/(1+exp(`xb')) in 1, se(sahead) p(pahead)
	list dydahead sahead pahead in 1

	* "cost_ingr"
	predictnl double dydcostingr = ((_b[cost] + _b[cost_ingr])*exp(`xb1')/(1+exp(`xb1'))/(1+exp(`xb1'))) ///
                                   - (_b[cost]*exp(`xb0')/(1+exp(`xb0'))/(1+exp(`xb0'))) in 1, se(scostingr) p(pcostingr)
	list dydcostingr scostingr pcostingr in 1

	* "damageA_ingr"
	predictnl double dyddamageAingr = ((_b[damageA] + _b[damageA_ingr])*exp(`xb1')/(1+exp(`xb1'))/(1+exp(`xb1'))) ///
                    - (_b[damageA]*exp(`xb0')/(1+exp(`xb0'))/(1+exp(`xb0'))) in 1, se(sdamageAingr) p(pdamageAingr)
	list dyddamageAingr sdamageAingr pdamageAingr in 1

	* "Bahead_ingr"
	predictnl double dydaheadingr = ((_b[Bahead] + _b[Bahead_ingr])*exp(`xb1')/(1+exp(`xb1'))/(1+exp(`xb1'))) ///
                      - (_b[damageA]*exp(`xb0')/(1+exp(`xb0'))/(1+exp(`xb0'))) in 1, se(saheadingr) p(paheadingr)
	list dydaheadingr saheadingr paheadingr in 1

	** compared to dlogit2 results
	dlogit2 punish ingr cost damageA Bahead cost_ingr damageA_ingr Bahead_ingr, cluster(sess_subj)

erase temp-neg.dta
erase temp-ingr.dta
erase temp-outgr.dta
 

******************
** Table4 (SWM) **
******************
	use ID-behavior-data.dta, clear
	keep if stage == 3 
	keep if treatment == "control" | treatment == "original"

	** Drop role A for Dict games
	drop if game == "Dict 1" & myrole == 1
	drop if game == "Dict 2" & myrole == 1
	drop if game == "Dict 3" & myrole == 1
	drop if game == "Dict 4" & myrole == 1
	drop if game == "Dict 5" & myrole == 1

	** Drop the games/roles which have all outcomes yield the same aggregate payoffs
	drop if game == "Dict 5"
	drop if game == "Resp 5a"
	drop if game == "Resp 5b"
	drop if game == "Resp 9" & myrole == 2


	** For Resp 1b and Resp 2b, we treat OUT as role A's SWM action
	gen swmax_act = . 
	replace swmax_act = 2 if myrole == 1 & game == "Resp 1a" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 1b" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 6" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 7" & swmax_act == .
	replace swmax_act = 2 if myrole == 1 & game == "Resp 2a" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 2b" & swmax_act == .
	replace swmax_act = 2 if myrole == 1 & game == "Resp 3" & swmax_act == .
	replace swmax_act = 2 if myrole == 1 & game == "Resp 4" & swmax_act == .
	replace swmax_act = 2 if myrole == 1 & game == "Resp 8"  & swmax_act == .
	replace swmax_act = 2 if myrole == 1 & game == "Resp 9"  & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 10" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 11" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 12" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 13a"  & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 13b" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 13c" & swmax_act == .
	replace swmax_act = 1 if myrole == 1 & game == "Resp 13d" & swmax_act == .

	replace swmax_act = 2 if myrole == 2 & game == "Dict 1" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Dict 2" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Dict 3" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Dict 4" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 1a"  & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 1b" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 6" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 7" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 2a" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 2b" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 3" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 4" & swmax_act == .
	replace swmax_act = 2 if myrole == 2 & game == "Resp 8"  & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 10" & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 11" & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 12" & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 13a"  & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 13b" & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 13c" & swmax_act == .
	replace swmax_act = 1 if myrole == 2 & game == "Resp 13d" & swmax_act == .

	** Indicator: ingr_swm = 1 if subjects choose the SW maximizing action for an ingroup match
	gen     ingr_swm = 1 if actforsame == swmax_act 
	replace ingr_swm = 0 if actforsame != swmax_act & actforsame != 0 

	** Indicator: ingr_swm = 1 if subjects choose the SW maximizing action for an ingroup match
	gen     outgr_swm = 1 if actforother == swmax_act 
	replace outgr_swm = 0 if actforother != swmax_act & actforother != 0 & treatment == "original"
	replace outgr_swm = . if treatment == "control"

** Generate unique identifier for each participant
	catenate sess_subj = date subject

	gen original = (treatment == "original")

	replace sametype = 0 if treatment == "control"

save temp-table4.dta, replace 

** Table 4: Proportion of SWM decisions for ingroup
	summ ingr_swm if treatment == "original" & myrole == 1
	summ ingr_swm if treatment == "original" & myrole == 2
	summ ingr_swm if treatment == "original"

** Table 4: Proportion of SWM decisions for outgroup
	summ outgr_swm if treatment == "original" & myrole == 1
	summ outgr_swm if treatment == "original" & myrole == 2
	summ outgr_swm if treatment == "original"

** Table 4: Proportion of SWM decisions for control
	summ ingr_swm if treatment == "control" & myrole == 1
	summ ingr_swm if treatment == "control" & myrole == 2
	summ ingr_swm if treatment == "control"

********* The following is a simple equality of proportion test (without clustering the standard error) *******
** Hypothesis: Ingr > Outgr
	* Player A
		use temp-table4.dta, clear
		drop if treatment == "control" 
		prtest ingr_swm = outgr_swm if myrole == 1
		mcc ingr_swm outgr_swm if myrole == 1

	* Player B
		prtest ingr_swm = outgr_swm if myrole == 2
		mcc ingr_swm outgr_swm if myrole == 2

	* Over all
		prtest ingr_swm = outgr_swm
		mcc ingr_swm outgr_swm

** Hypothesis: Ingr > Control
	* Player A
		use temp-table4.dta, clear
		prtest ingr_swm if myrole == 1, by(original)

	* Player B		
		prtest ingr_swm if myrole == 2, by(original)

	* Over all
		prtest ingr_swm, by(original)

** Hypothesis: Control > Outgr
	* Player A
		use temp-table4.dta, clear
		replace outgr_swm = ingr_swm if treatment == "control"
		prtest outgr_swm if myrole == 1, by(original)

	* Player B
		prtest outgr_swm if myrole == 2, by(original)

	* Over all
		prtest outgr_swm, by(original)


*** The following is a equality-of-proportion-test equivalent regression clustering standard error *****
***         on the individual participant's level                                                  *****

** Hypothesis: Ingr > Outgr
	* Player A
		use temp-table4.dta, clear
		drop if treatment == "control"
		save temp-reshape.dta, replace
		* reshape the data to the long format	
		use temp-reshape.dta, clear
		keep treatment date round subject myrole sess_subj ingr_swm
		rename ingr_swm swm
		gen ingr = 1
		sort treatment date round subject
		save temp-ingr.dta, replace
	
		use temp-reshape.dta, clear
		keep treatment date round subject myrole sess_subj outgr_swm
		rename outgr_swm swm
		gen ingr = 0
		sort treatment date round subject
		save temp-outgr.dta, replace
		
		use temp-ingr.dta, clear
		append using temp-outgr.dta
		reg swm ingr if myrole == 1, cluster(sess_subj)

		erase temp-reshape.dta
		erase temp-ingr.dta
		erase temp-outgr.dta

	* Player B		
		reg swm ingr if myrole == 2, cluster(sess_subj)

	* Over all
		reg swm ingr, cluster(sess_subj)

** Hypothesis: Ingr > Control
	* Player A
		use temp-table4.dta, clear
		reg ingr_swm original if myrole == 1, cluster(sess_subj)

	* Player B		
		reg ingr_swm original if myrole == 2, cluster(sess_subj)

	* Over all
		reg ingr_swm original, cluster(sess_subj)

** Hypothesis: Outgr < Control
	* Player A
		use temp-table4.dta, clear
		replace outgr_swm = ingr_swm if treatment == "control"
		reg outgr_swm original if myrole == 1, cluster(sess_subj)

	* Player B		
		reg outgr_swm original if myrole == 2, cluster(sess_subj)

	* Over all
		reg outgr_swm original, cluster(sess_subj)

erase temp-table4.dta


********************************************
** Table 5 Bottom Panel: Actual Earnings **
********************************************
use ID-behavior-data.dta, clear
	keep if stage == 3 
	keep if treatment == "control" | treatment == "original"

** summary on actual earnings for ingroup
	summ mypayoff if treatment == "original" & myrole == 1 & sametype == 1
	summ mypayoff if treatment == "original" & myrole == 2 & sametype == 1
	summ mypayoff if treatment == "original" & sametype == 1

** summary on actual earnings for outgroup
	summ mypayoff if treatment == "original" & myrole == 1 & sametype == 0
	summ mypayoff if treatment == "original" & myrole == 2 & sametype == 0
	summ mypayoff if treatment == "original" & sametype == 0

** summary on actual earnings for control
	summ mypayoff if treatment == "control" & myrole == 1
	summ mypayoff if treatment == "control" & myrole == 2
	summ mypayoff if treatment == "control" 

** Generate unique identifier for each participant
	catenate sess_subj = date subject

	gen original = (treatment == "original")

save temp-table5.dta, replace


** Hypothesis: Ingr > Outgr
	* Player A
		use temp-table5.dta, clear
		drop if treatment == "control" 
		gen ingr = (sametype == 1)
		reg mypayoff ingr if myrole == 1, cluster(sess_subj)
	
	* Player B
		reg mypayoff ingr if myrole == 2, cluster(sess_subj)

	* Overall
		reg mypayoff ingr, cluster(sess_subj)


** Hypothesis: Ingr > Control
	* Player A
		use temp-table5.dta, clear
		drop if treatment == "original" & sametype == 0
		reg mypayoff original if myrole == 1, cluster(sess_subj)

	* Player B
		reg mypayoff original if myrole == 2, cluster(sess_subj)

	* Over all
		reg mypayoff original, cluster(sess_subj)

** Hypothesis: Outgr < Control
	* Player A
		use temp-table5.dta, clear
		drop if treatment == "original" & sametype == 1
		reg mypayoff original if myrole == 1, cluster(sess_subj)

	* Player B
		reg mypayoff original if myrole == 2, cluster(sess_subj)

	* Over all
		reg mypayoff original, cluster(sess_subj)


erase temp-table5.dta



*********************************************
** Table 7: Self-reported group attachment ** 
*********************************************
	use survey.dta, clear
	drop if treatment == "control"

	gen paintings = 1 if treatment == "original" | treatment == "nochat" | treatment == "nohelp"
	replace paintings = 0 if treatment == "random within" | treatment == "random btw same" | treatment == "random btw other"

	gen     chat = 1 if treatment == "original" | treatment == "random within" | treatment == "random btw same" | treatment == "random btw other"
	replace chat = 0 if treatment == "nochat" | treatment == "nohelp"    

	gen     oo = 1 if treatment == "original" | treatment == "nochat" | treatment == "random btw same" | treatment == "random btw other" | treatment == "random within"
	replace oo = 0 if treatment == "nohelp"

	gen     within_subj = 1 if treatment == "original" | treatment == "random within" | treatment == "nochat" | treatment == "nohelp"    
	replace within_subj = 0 if treatment == "random btw same" | treatment == "random btw other"

	reg attach_to_gr paintings chat oo within_subj, cluster(date)
	ologit attach_to_gr paintings chat oo within_subj, cluster(date)


***************************************
** Figure 1: other-other allocations **
***************************************
	use ID-behavior-data.dta, clear
	keep if stage == 2 & treatment == "original"

	/*give ingroup A vs B in scenario 1*/
	summ  r1s1_giveA r2s1_giveA r3s1_giveA r4s1_giveA r5s1_giveA
	summ  r1s1_giveB r2s1_giveB r3s1_giveB r4s1_giveB r5s1_giveB

	/*give outgroup A vs B in scenario 2*/
	summ  r1s2_giveA r2s2_giveA r3s2_giveA r4s2_giveA r5s2_giveA
	summ  r1s2_giveB r2s2_giveB r3s2_giveB r4s2_giveB r5s2_giveB

	/*give ingroup A vs outgroup B in scenario 3*/
	summ  r1s3_giveA r2s3_giveA r3s3_giveA r4s3_giveA r5s3_giveA
	summ  r1s3_giveB r2s3_giveB r3s3_giveB r4s3_giveB r5s3_giveB

	/*ingroup and outgroup difference, normalized by endowmend*/
	collapse (mean) r1s3_giveA r2s3_giveA r3s3_giveA r4s3_giveA r5s3_giveA r1s3_giveB r2s3_giveB r3s3_giveB r4s3_giveB r5s3_giveB
	gen diff13 = (r1s3_giveA - r1s3_giveB)*100/200
	gen diff23 = (r2s3_giveA - r2s3_giveB)*100/250  
	gen diff33 = (r3s3_giveA - r3s3_giveB)*100/300 
	gen diff43 = (r4s3_giveA - r4s3_giveB)*100/350 
	gen diff53 = (r5s3_giveA - r5s3_giveB)*100/400
	
	summ diff13 
	summ diff23 
	summ diff33 
	summ diff43 
	summ diff53 



*********************************************************************************
** Replicate the results in Appendix A (summary stats)                         **
** Note: The summary stats are based on all treatments                         **
**       % A diff and % B diff computation excluded the random btw treatments ** 
*********************************************************************************

** For control sessions
use ID-behavior-data.dta, clear
keep if stage == 3 & treatment == "control"


/*A's act*/
gen out = 1 if actforsame == 1 & myrole == 1
replace out = 0 if actforsame == 2 & myrole == 1
summ out if myrole == 1 & game == "Resp 1a"
summ out if myrole == 1 & game == "Resp 1b"
summ out if myrole == 1 & game == "Resp 6"
summ out if myrole == 1 & game == "Resp 7"
summ out if myrole == 1 & game == "Resp 2a"
summ out if myrole == 1 & game == "Resp 2b"
summ out if myrole == 1 & game == "Resp 3"
summ out if myrole == 1 & game == "Resp 4"
summ out if myrole == 1 & game == "Resp 5a"
summ out if myrole == 1 & game == "Resp 5b"
summ out if myrole == 1 & game == "Resp 8"
summ out if myrole == 1 & game == "Resp 9"
summ out if myrole == 1 & game == "Resp 10"
summ out if myrole == 1 & game == "Resp 11"
summ out if myrole == 1 & game == "Resp 12"
summ out if myrole == 1 & game == "Resp 13a"
summ out if myrole == 1 & game == "Resp 13b"
summ out if myrole == 1 & game == "Resp 13c"
summ out if myrole == 1 & game == "Resp 13d"

/*B's act*/
gen left = 1 if actforsame == 1 & myrole == 2
replace left = 0 if actforsame == 2 & myrole == 2
summ left if myrole == 2 & game == "Dict 1"
summ left if myrole == 2 & game == "Dict 2"
summ left if myrole == 2 & game == "Dict 3"
summ left if myrole == 2 & game == "Dict 4"
summ left if myrole == 2 & game == "Dict 5"

summ left if myrole == 2 & game == "Resp 1a"
summ left if myrole == 2 & game == "Resp 1b"
summ left if myrole == 2 & game == "Resp 6"
summ left if myrole == 2 & game == "Resp 7"
summ left if myrole == 2 & game == "Resp 2a"
summ left if myrole == 2 & game == "Resp 2b"
summ left if myrole == 2 & game == "Resp 3"
summ left if myrole == 2 & game == "Resp 4"
summ left if myrole == 2 & game == "Resp 5a"
summ left if myrole == 2 & game == "Resp 5b"
summ left if myrole == 2 & game == "Resp 8"
summ left if myrole == 2 & game == "Resp 9"
summ left if myrole == 2 & game == "Resp 10"
summ left if myrole == 2 & game == "Resp 11"
summ left if myrole == 2 & game == "Resp 12"
summ left if myrole == 2 & game == "Resp 13a"
summ left if myrole == 2 & game == "Resp 13b"
summ left if myrole == 2 & game == "Resp 13c"
summ left if myrole == 2 & game == "Resp 13d"


** For treatment (original and all additional treatments)
use ID-behavior-data.dta, clear
keep if stage == 3 & treatment != "control"

/*A's ingr act*/
gen ingr_out = 1 if actforsame == 1 & myrole == 1
replace ingr_out = 0 if actforsame == 2 & myrole == 1
summ ingr_out if myrole == 1 & game == "Resp 1a"
summ ingr_out if myrole == 1 & game == "Resp 1b"
summ ingr_out if myrole == 1 & game == "Resp 6"
summ ingr_out if myrole == 1 & game == "Resp 7"
summ ingr_out if myrole == 1 & game == "Resp 2a"
summ ingr_out if myrole == 1 & game == "Resp 2b"
summ ingr_out if myrole == 1 & game == "Resp 3"
summ ingr_out if myrole == 1 & game == "Resp 4"
summ ingr_out if myrole == 1 & game == "Resp 5a"
summ ingr_out if myrole == 1 & game == "Resp 5b"
summ ingr_out if myrole == 1 & game == "Resp 8"
summ ingr_out if myrole == 1 & game == "Resp 9"
summ ingr_out if myrole == 1 & game == "Resp 10"
summ ingr_out if myrole == 1 & game == "Resp 11"
summ ingr_out if myrole == 1 & game == "Resp 12"
summ ingr_out if myrole == 1 & game == "Resp 13a"
summ ingr_out if myrole == 1 & game == "Resp 13b"
summ ingr_out if myrole == 1 & game == "Resp 13c"
summ ingr_out if myrole == 1 & game == "Resp 13d"

/*B's ingr act*/
gen ingr_left = 1 if actforsame == 1 & myrole == 2
replace ingr_left = 0 if actforsame == 2 & myrole == 2
summ ingr_left if myrole == 2 & game == "Dict 1"
summ ingr_left if myrole == 2 & game == "Dict 2"
summ ingr_left if myrole == 2 & game == "Dict 3"
summ ingr_left if myrole == 2 & game == "Dict 4"
summ ingr_left if myrole == 2 & game == "Dict 5"
summ ingr_left if myrole == 2 & game == "Resp 1a"
summ ingr_left if myrole == 2 & game == "Resp 1b"
summ ingr_left if myrole == 2 & game == "Resp 6"
summ ingr_left if myrole == 2 & game == "Resp 7"
summ ingr_left if myrole == 2 & game == "Resp 2a"
summ ingr_left if myrole == 2 & game == "Resp 2b"
summ ingr_left if myrole == 2 & game == "Resp 3"
summ ingr_left if myrole == 2 & game == "Resp 4"
summ ingr_left if myrole == 2 & game == "Resp 5a"
summ ingr_left if myrole == 2 & game == "Resp 5b"
summ ingr_left if myrole == 2 & game == "Resp 8"
summ ingr_left if myrole == 2 & game == "Resp 9"
summ ingr_left if myrole == 2 & game == "Resp 10"
summ ingr_left if myrole == 2 & game == "Resp 11"
summ ingr_left if myrole == 2 & game == "Resp 12"
summ ingr_left if myrole == 2 & game == "Resp 13a"
summ ingr_left if myrole == 2 & game == "Resp 13b"
summ ingr_left if myrole == 2 & game == "Resp 13c"
summ ingr_left if myrole == 2 & game == "Resp 13d"

/*A's outgr act*/
gen outgr_out = 1 if actforother == 1 & myrole == 1
replace outgr_out = 0 if actforother == 2 & myrole == 1
summ outgr_out if myrole == 1 & game == "Resp 1a"
summ outgr_out if myrole == 1 & game == "Resp 1b"
summ outgr_out if myrole == 1 & game == "Resp 6"
summ outgr_out if myrole == 1 & game == "Resp 7"
summ outgr_out if myrole == 1 & game == "Resp 2a"
summ outgr_out if myrole == 1 & game == "Resp 2b"
summ outgr_out if myrole == 1 & game == "Resp 3"
summ outgr_out if myrole == 1 & game == "Resp 4"
summ outgr_out if myrole == 1 & game == "Resp 5a"
summ outgr_out if myrole == 1 & game == "Resp 5b"
summ outgr_out if myrole == 1 & game == "Resp 8"
summ outgr_out if myrole == 1 & game == "Resp 9"
summ outgr_out if myrole == 1 & game == "Resp 10"
summ outgr_out if myrole == 1 & game == "Resp 11"
summ outgr_out if myrole == 1 & game == "Resp 12"
summ outgr_out if myrole == 1 & game == "Resp 13a"
summ outgr_out if myrole == 1 & game == "Resp 13b"
summ outgr_out if myrole == 1 & game == "Resp 13c"
summ outgr_out if myrole == 1 & game == "Resp 13d"

/*B's outgr act*/
gen outgr_left = 1 if actforother == 1 & myrole == 2
replace outgr_left = 0 if actforother == 2 & myrole == 2
summ outgr_left if myrole == 2 & game == "Dict 1"
summ outgr_left if myrole == 2 & game == "Dict 2"
summ outgr_left if myrole == 2 & game == "Dict 3"
summ outgr_left if myrole == 2 & game == "Dict 4"
summ outgr_left if myrole == 2 & game == "Dict 5"
summ outgr_left if myrole == 2 & game == "Resp 1a"
summ outgr_left if myrole == 2 & game == "Resp 1b"
summ outgr_left if myrole == 2 & game == "Resp 6"
summ outgr_left if myrole == 2 & game == "Resp 7"
summ outgr_left if myrole == 2 & game == "Resp 2a"
summ outgr_left if myrole == 2 & game == "Resp 2b"
summ outgr_left if myrole == 2 & game == "Resp 3"
summ outgr_left if myrole == 2 & game == "Resp 4"
summ outgr_left if myrole == 2 & game == "Resp 5a"
summ outgr_left if myrole == 2 & game == "Resp 5b"
summ outgr_left if myrole == 2 & game == "Resp 8"
summ outgr_left if myrole == 2 & game == "Resp 9"
summ outgr_left if myrole == 2 & game == "Resp 10"
summ outgr_left if myrole == 2 & game == "Resp 11"
summ outgr_left if myrole == 2 & game == "Resp 12"
summ outgr_left if myrole == 2 & game == "Resp 13a"
summ outgr_left if myrole == 2 & game == "Resp 13b"
summ outgr_left if myrole == 2 & game == "Resp 13c"
summ outgr_left if myrole == 2 & game == "Resp 13d"


/*A diff: % of A whose decisions are contingent on groups*/
use ID-behavior-data.dta, clear
keep if stage == 3 
drop if treatment == "control"
drop if (treatment == "random btw same" | treatment == "random btw other")
drop if myrole == 1 & (game == "Dict 1" | game == "Dict 2" | game == "Dict 3" | game == "Dict 4" | game == "Dict 5")
tab actforsame, missing
tab actforother, missing
gen diff = (actforsame != actforother)

summ diff if myrole == 1 & game == "Resp 1a"
summ diff if myrole == 1 & game == "Resp 1b"
summ diff if myrole == 1 & game == "Resp 6"
summ diff if myrole == 1 & game == "Resp 7"
summ diff if myrole == 1 & game == "Resp 2a"
summ diff if myrole == 1 & game == "Resp 2b"
summ diff if myrole == 1 & game == "Resp 3"
summ diff if myrole == 1 & game == "Resp 4"
summ diff if myrole == 1 & game == "Resp 5a"
summ diff if myrole == 1 & game == "Resp 5b"
summ diff if myrole == 1 & game == "Resp 8"
summ diff if myrole == 1 & game == "Resp 9"
summ diff if myrole == 1 & game == "Resp 10"
summ diff if myrole == 1 & game == "Resp 11"
summ diff if myrole == 1 & game == "Resp 12"
summ diff if myrole == 1 & game == "Resp 13a"
summ diff if myrole == 1 & game == "Resp 13b"
summ diff if myrole == 1 & game == "Resp 13c"
summ diff if myrole == 1 & game == "Resp 13d"


/*B diff: % of B whose decisions are contingent on groups*/
summ diff if myrole == 2 & game == "Dict 1"
summ diff if myrole == 2 & game == "Dict 2"
summ diff if myrole == 2 & game == "Dict 3"
summ diff if myrole == 2 & game == "Dict 4"
summ diff if myrole == 2 & game == "Dict 5"
summ diff if myrole == 2 & game == "Resp 1a"
summ diff if myrole == 2 & game == "Resp 1b"
summ diff if myrole == 2 & game == "Resp 6"
summ diff if myrole == 2 & game == "Resp 7"
summ diff if myrole == 2 & game == "Resp 2a"
summ diff if myrole == 2 & game == "Resp 2b"
summ diff if myrole == 2 & game == "Resp 3"
summ diff if myrole == 2 & game == "Resp 4"
summ diff if myrole == 2 & game == "Resp 5a"
summ diff if myrole == 2 & game == "Resp 5b"
summ diff if myrole == 2 & game == "Resp 8"
summ diff if myrole == 2 & game == "Resp 9"
summ diff if myrole == 2 & game == "Resp 10"
summ diff if myrole == 2 & game == "Resp 11"
summ diff if myrole == 2 & game == "Resp 12"
summ diff if myrole == 2 & game == "Resp 13a"
summ diff if myrole == 2 & game == "Resp 13b"
summ diff if myrole == 2 & game == "Resp 13c"
summ diff if myrole == 2 & game == "Resp 13d"

keep if stage == 3
sort treatment date myrole subject
collapse (sum) diff, by(treatment date myrole subject)
** subject who differentiates in at least one game
tab diff
gen     subj_diff = 1 if diff > 0
replace subj_diff = 0 if diff == 0
sort treatment myrole
save temp-table6.dta, replace

************************************************************************
** Table 6: % of participants who differentiate groups (Bottom panel) **
************************************************************************
	* original vs randomwithin
	use temp-table6.dta, clear
	keep if treatment == "original" | treatment == "randomwithin"
	gen treatment2 = (treatment == "original")
	sort treatment2 myrole
	prtest subj_diff if myrole == 1, by(treatment2)
	prtest subj_diff if myrole == 2, by(treatment2)

	* original vs nochat
	use temp-table6.dta, clear
	keep if treatment == "original" | treatment == "nochat"
	gen treatment2 = (treatment == "original")
	sort treatment2 myrole
	prtest subj_diff if myrole == 1, by(treatment2)
	prtest subj_diff if myrole == 2, by(treatment2)

	* nochat vs nohelp
	use temp-table6.dta, clear
	keep if treatment == "nochat" | treatment == "nohelp"
	gen treatment2 = (treatment == "nochat")
	sort treatment2 myrole
	prtest subj_diff if myrole == 1, by(treatment2)
	prtest subj_diff if myrole == 2, by(treatment2)

	* original vs nohelp
	use temp-table6.dta, clear
	keep if treatment == "original" | treatment == "nohelp"
	gen treatment2 = (treatment == "original")
	sort treatment2 myrole
	prtest subj_diff if myrole == 1, by(treatment2)
	prtest subj_diff if myrole == 2, by(treatment2)

** Table 6 (Top panel)
	use temp-table6.dta, clear
	sort treatment myrole subject
	collapse (mean) subj_diff, by(treatment myrole)

	erase temp-table6.dta


** The end ** 

