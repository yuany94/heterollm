clear
capture log close
set memo 100m
set matsize 800
set more 1

****************************************************************************************************************
** Table 5 Top Panel: use simulation to compute *expected* earnings for the original treatment and the control**
**   Note: this program may take hours to run 
****************************************************************************************************************
use ID-behavior-data.dta, clear
	keep if stage == 3 
	keep if treatment == "control" | treatment == "original"

	tab date if treatment == "control"
	tab date if treatment == "original"

** number the sessions (used for the loop)
	gen sess = .
	replace sess = 1  if date == "050316PQ"
	replace sess = 2  if date == "050318OT"
	replace sess = 3  if date == "050319OR"
	replace sess = 4  if date == "050325PO"
	replace sess = 5  if date == "050326P3"
	replace sess = 6  if date == "050401QD"
	replace sess = 7  if date == "050722O6"
	replace sess = 8  if date == "050801LR"
	replace sess = 9  if date == "050801NP"
  	replace sess = 10 if date == "050127R4"
  	replace sess = 11 if date == "050202PR"
   	replace sess = 12 if date == "050203QK"
  	replace sess = 13 if date == "050207OP"
  	replace sess = 14 if date == "050209Q5"
  	replace sess = 15 if date == "050210PP"
  	replace sess = 16 if date == "050211LN"
  	replace sess = 17 if date == "050216PO"
   	replace sess = 18 if date == "050217PL"
  	replace sess = 19 if date == "050219PM"
   	replace sess = 20 if date == "050720LQ"
  	replace sess = 21 if date == "050720NP"
  	replace sess = 22 if date == "050722M3"
  	replace sess = 23 if date == "050729LO"
  	replace sess = 24 if date == "050729NK"

** temp-variable: game_code (for the convenience of generating table Appendix A)
gen game_code = .
replace game_code =  1  if game == "Dict 1"
replace game_code =  2  if game == "Dict 2"
replace game_code =  3  if game == "Dict 3"
replace game_code =  4  if game == "Dict 4"
replace game_code =  5  if game == "Dict 5"
replace game_code =  18 if game == "Resp 10"
replace game_code =  19 if game == "Resp 11"
replace game_code =  20 if game == "Resp 12"
replace game_code =  21 if game == "Resp 13a"
replace game_code =  22 if game == "Resp 13b"
replace game_code =  23 if game == "Resp 13c"
replace game_code =  24 if game == "Resp 13d"
replace game_code =  6  if game == "Resp 1a"
replace game_code =  7  if game == "Resp 1b"
replace game_code =  10 if game == "Resp 2a"
replace game_code =  11 if game == "Resp 2b"
replace game_code =  12 if game == "Resp 3"
replace game_code =  13 if game == "Resp 4"
replace game_code =  14 if game == "Resp 5a"
replace game_code =  15 if game == "Resp 5b"
replace game_code =  8  if game == "Resp 6"
replace game_code =  9  if game == "Resp 7"
replace game_code =  16 if game == "Resp 8"
replace game_code =  17 if game == "Resp 9"

	* Generate unique id for individual participant, called "person" (There were 370 participants in Control and Original)
	sort sess subject round 
	catenate sess_subj = sess subj
	egen person = group(sess_subj)
	
	* Generate unique id for each observation, called "obs" (There were 2968 observations in Conrol and Original)
	sort sess round subject 
	gen obs = _n
	save temp-expected-earnings.dta, replace

	* 
	use temp-expected-earnings.dta, clear
	keep treatment date sess round subject obs
	sort obs
	gen fakeopp_id1 = 1
	gen fakeopp_id2 = 2
	gen fakeopp_id3 = 3
	gen fakeopp_id4 = 4
	gen fakeopp_id5 = 5
	gen fakeopp_id6 = 6
	gen fakeopp_id7 = 7
	gen fakeopp_id8 = 8
	gen fakeopp_id9 = 9
	gen fakeopp_id10 = 10
	gen fakeopp_id11 = 11
	gen fakeopp_id12 = 12
	gen fakeopp_id13 = 13
	gen fakeopp_id14 = 14
	gen fakeopp_id15 = 15
	gen fakeopp_id16 = 16
	save temp-random-pairing.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id1
	rename fakeopp_id1 fakeopp_id
	save temp-fakeopp_id1.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id2
	rename fakeopp_id2 fakeopp_id
	save temp-fakeopp_id2.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id3
	rename fakeopp_id3 fakeopp_id
	save temp-fakeopp_id3.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id4
	rename fakeopp_id4 fakeopp_id
	save temp-fakeopp_id4.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id5
	rename fakeopp_id5 fakeopp_id
	save temp-fakeopp_id5.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id6
	rename fakeopp_id6 fakeopp_id
	save temp-fakeopp_id6.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id7
	rename fakeopp_id7 fakeopp_id
	save temp-fakeopp_id7.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id8
	rename fakeopp_id8 fakeopp_id
	save temp-fakeopp_id8.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id9
	rename fakeopp_id9 fakeopp_id
	save temp-fakeopp_id9.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id10
	rename fakeopp_id10 fakeopp_id
	save temp-fakeopp_id10.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id11
	rename fakeopp_id11 fakeopp_id
	save temp-fakeopp_id11.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id12
	rename fakeopp_id12 fakeopp_id
	save temp-fakeopp_id12.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id13
	rename fakeopp_id13 fakeopp_id
	save temp-fakeopp_id13.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id14
	rename fakeopp_id14 fakeopp_id
	save temp-fakeopp_id14.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id15
	rename fakeopp_id15 fakeopp_id
	save temp-fakeopp_id15.dta, replace

	use temp-random-pairing.dta, clear
	keep obs fakeopp_id16
	rename fakeopp_id16 fakeopp_id
	save temp-fakeopp_id16.dta, replace

	use temp-fakeopp_id1.dta, clear
	append using temp-fakeopp_id2.dta
	append using temp-fakeopp_id3.dta
	append using temp-fakeopp_id4.dta
	append using temp-fakeopp_id5.dta
	append using temp-fakeopp_id6.dta
	append using temp-fakeopp_id7.dta
	append using temp-fakeopp_id8.dta
	append using temp-fakeopp_id9.dta
	append using temp-fakeopp_id10.dta
	append using temp-fakeopp_id11.dta
	append using temp-fakeopp_id12.dta
	append using temp-fakeopp_id13.dta
	append using temp-fakeopp_id14.dta
	append using temp-fakeopp_id15.dta
	append using temp-fakeopp_id16.dta
	sort obs fakeopp_id
	save temp-fakeopp_id.dta, replace

* Create the dataset that includes the random pairing id numbers
use temp-expected-earnings.dta, clear
sort obs
merge obs using temp-fakeopp_id.dta
	tab _merge
	drop _merge
	keep  treatment date round subject mytype myrole actforsame actforother game game_code sess sess_subj person obs fakeopp_id
	sort treat date sess round subject

	* Drop the following lines since one can't be paired with self
	drop if subject == fakeopp_id   
	
	* Drop the following lines because the following subject IDs are missing (#subjects < 16 in these sessions)
	sort date
	by date: summ subject
	drop if fakeopp_id == 16 & date == "050127R4"
	drop if (fakeopp_id == 15 | fakeopp_id == 16) & date == "050316PQ"
	drop if (fakeopp_id >= 13) & date == "050326P3"
	drop if (fakeopp_id >= 15) & date == "050401QD"
	drop if (fakeopp_id >= 15) & date == "050722O6"

	* Drop the following lines because the fakeopp_id is the second observation of people who participated twice
	drop if date == "050127R4" & fakeopp_id == 16  	
	drop if date == "050207OP" & (fakeopp_id == 1 | fakeopp_id == 10)				
	drop if date == "050316PQ" & fakeopp_id == 3    

* Error flag
	gen fakeopp_type = -1
	gen fakeopp_role = -1
	gen fakeopp_actforsame = -1
	gen fakeopp_actforoth = -1


	sort treatment date sess round subject
	save temp-exppayoff.dta, replace

local s = 1
while `s' <= 24 {
	use temp-exppayoff.dta, clear

	keep if sess == `s'

	local N = _N
	forvalues i = 1/`N' {
	local fakeoppid = fakeopp_id[`i']
	local sessid = sess[`i']
	local game_num =  game_code[`i']
	local round_num = round[`i']
	local subjectid = subject[`i']
	forvalues j = 1/`N' {

	quietly replace fakeopp_type        = mytype[`j']      if treatment == "original" & subject[`j'] == `fakeoppid' & sess[`j'] == `sessid' & game_code[`j'] == `game_num' in `i'
	quietly replace fakeopp_type        = .                if treatment == "control"  & subject[`j'] == `fakeoppid' & sess[`j'] == `sessid' & game_code[`j'] == `game_num' in `i'
	quietly replace fakeopp_role        = myrole[`j']      if subject[`j'] == `fakeoppid' & sess[`j'] == `sessid' & game_code[`j'] == `game_num' in `i'
	quietly replace fakeopp_actforsame  = actforsame[`j']  if subject[`j'] == `fakeoppid' & sess[`j'] == `sessid' & game_code[`j'] == `game_num' in `i'
	quietly replace fakeopp_actforoth   = actforother[`j'] if treatment == "original" & subject[`j'] == `fakeoppid' & sess[`j'] == `sessid' & game_code[`j'] == `game_num' in `i'
	quietly replace fakeopp_actforoth   = .                if treatment == "control"  & subject[`j'] == `fakeoppid' & sess[`j'] == `sessid' & game_code[`j'] == `game_num' in `i'

	}
	display " Session # is now `sessid', round is `round_num', subj is `subjectid'"  
	}

save exppay\temp-exppay-sess`s'.dta, replace
local s = `s' + 1
}

	use exppay\temp-exppay-sess1.dta, clear
	append using exppay\temp-exppay-sess2.dta
	append using exppay\temp-exppay-sess3.dta
	append using exppay\temp-exppay-sess4.dta
	append using exppay\temp-exppay-sess5.dta
	append using exppay\temp-exppay-sess6.dta
	append using exppay\temp-exppay-sess7.dta
	append using exppay\temp-exppay-sess8.dta
	append using exppay\temp-exppay-sess9.dta
	append using exppay\temp-exppay-sess10.dta
	append using exppay\temp-exppay-sess11.dta
	append using exppay\temp-exppay-sess12.dta
	append using exppay\temp-exppay-sess13.dta
	append using exppay\temp-exppay-sess14.dta
	append using exppay\temp-exppay-sess15.dta
	append using exppay\temp-exppay-sess16.dta
	append using exppay\temp-exppay-sess17.dta
	append using exppay\temp-exppay-sess18.dta
	append using exppay\temp-exppay-sess19.dta
	append using exppay\temp-exppay-sess20.dta
	append using exppay\temp-exppay-sess21.dta
	append using exppay\temp-exppay-sess22.dta
	append using exppay\temp-exppay-sess23.dta
	append using exppay\temp-exppay-sess24.dta

save exppay\temp-exppay-allsess.dta, replace

** 
use exppay\temp-exppay-allsess.dta, clear

	** Drop the following lines since one can't be matched with the same role
	drop if myrole == fakeopp_role
	
	** Search for some potential errors (okay)
	count if fakeopp_type == -1
	count if fakeopp_role == -1
	count if fakeopp_actforsame == -1
	count if fakeopp_actforoth == -1

	** Replace action for Role A in Dict games with 0 
	tab actforsame, missing
	tab treatment myrole if actforsame == .
	replace actforsame = 0 if actforsame == . & myrole == 1

	tab actforother, missing
	tab treatment myrole if actforother == .
	replace actforother = 0 if actforother == . & myrole == 1 & treatment == "original"

	tab fakeopp_actforsame, missing
	tab treatment myrole if fakeopp_actforsame == .
	tab game fakeopp_role if fakeopp_actforsame == .
	replace fakeopp_actforsame = 0 if fakeopp_actforsame == . & fakeopp_role == 1

	tab fakeopp_actforoth, missing
	tab treatment myrole if fakeopp_actforoth == . /*fakeopp_actforoth is not valid for control*/
	tab game fakeopp_role if fakeopp_actforoth == . & treatment == "original"
	replace fakeopp_actforoth = 0 if fakeopp_actforoth == . & fakeopp_role == 1 & treatment == "original"

	count if fakeopp_type == . 
	tab treatment fakeopp_type, missing /*okay, type is not defined in control*/
	count if fakeopp_role == .          /*okay*/
	
** Compute expected payoffs in the fake pairings
* Are we the same type? --> choice is actforsame or actforother? --> who is A/B? asact/bsact --> 
*     aspayoff/bspayoff --> mypayoff/fakeopp_payoff  

	gen     fake_sametype = 1 if mytype == fakeopp_type & treatment == "original"
	replace fake_sametype = 0 if mytype != fakeopp_type & treatment == "original"
	replace fake_sametype = . if treatment == "control"
	sort treatment
	by treatment: tab fake_sametype

	gen     fake_myact  = actforsame   if fake_sametype == 1 & treatment == "original"
	replace fake_myact  = actforother  if fake_sametype == 0 & treatment == "original"
	replace fake_myact  = actforsame   if treatment == "control"

	gen     fakeopp_act = fakeopp_actforsame if fake_sametype == 1 & treatment == "original" 
	replace fakeopp_act = fakeopp_actforoth  if fake_sametype == 0 & treatment == "original"
	replace fakeopp_act = fakeopp_actforsame if treatment == "control"
  
	tab  game fake_myact  if treatment == "original", missing
	tab  game fakeopp_act if treatment == "original", missing
	tab  fake_myact fakeopp_act if treatment == "control", missing

	gen     fake_asact = fake_myact if myrole == 1
	replace fake_asact = fakeopp_act if myrole == 2

	gen     fake_bsact = fake_myact if myrole == 2
	replace fake_bsact = fakeopp_act if myrole == 1

	gen case_num = 3*fake_asact + fake_bsact 

	** Generate expected payoffs
	
	** Generate payoff avariables that correspond to the game payoff matrix
	gen AspayoffData1 = .
	gen AspayoffData2 = .
	gen AspayoffData3 = .
	gen AspayoffData4 = .
	gen AspayoffData5 = .
	gen AspayoffData6 = .
	gen AspayoffData7 = .
	gen AspayoffData8 = . 

	gen BspayoffData1 = .
	gen BspayoffData2 = .
	gen BspayoffData3 = .
	gen BspayoffData4 = .
	gen BspayoffData5 = .
	gen BspayoffData6 = .
	gen BspayoffData7 = . 
	gen BspayoffData8 = . 

* "Dict 1"	
	replace  AspayoffData1 = 400 if game == "Dict 1"
	replace  AspayoffData2 = 750 if game == "Dict 1"
	replace  AspayoffData3 = -1 if game == "Dict 1"
	replace  AspayoffData4 = -1 if game == "Dict 1"
	replace  AspayoffData5 = -1 if game == "Dict 1"
	replace  AspayoffData6 = -1 if game == "Dict 1"
	replace  AspayoffData7 = -1 if game == "Dict 1"
	replace  AspayoffData8 = -1 if game == "Dict 1"


	replace  BspayoffData1 = 400 if game == "Dict 1"
	replace  BspayoffData2 = 400 if game == "Dict 1"
	replace  BspayoffData3 = -1 if game == "Dict 1"
	replace  BspayoffData4 = -1 if game == "Dict 1"
	replace  BspayoffData5 = -1 if game == "Dict 1"
	replace  BspayoffData6 = -1 if game == "Dict 1"
	replace  BspayoffData7 = -1 if game == "Dict 1"
	replace  BspayoffData8 = -1 if game == "Dict 1"
  

* "Dict 2"
	replace  AspayoffData1 = 400 if game == "Dict 2"
	replace  AspayoffData2 = 750 if game == "Dict 2"
	replace  AspayoffData3 = -1 if game == "Dict 2"
	replace  AspayoffData4 = -1 if game == "Dict 2"
	replace  AspayoffData5 = -1 if game == "Dict 2"
	replace  AspayoffData6 = -1 if game == "Dict 2"
	replace  AspayoffData7 = -1 if game == "Dict 2"
	replace  AspayoffData8 = -1 if game == "Dict 2"
  
  
	replace  BspayoffData1 = 400 if game == "Dict 2"
	replace  BspayoffData2 = 375 if game == "Dict 2"
	replace  BspayoffData3 = -1 if game == "Dict 2"
	replace  BspayoffData4 = -1 if game == "Dict 2"
	replace  BspayoffData5 = -1 if game == "Dict 2"
	replace  BspayoffData6 = -1 if game == "Dict 2"
	replace  BspayoffData7 = -1 if game == "Dict 2"
	replace  BspayoffData8 = -1 if game == "Dict 2"
  
  
* "Dict 3"
	replace  AspayoffData1 = 300 if game == "Dict 3"
	replace  AspayoffData2 = 700 if game == "Dict 3"
	replace  AspayoffData3 = -1 if game == "Dict 3"
	replace  AspayoffData4 = -1 if game == "Dict 3"
	replace  AspayoffData5 = -1 if game == "Dict 3"
	replace  AspayoffData6 = -1 if game == "Dict 3"
	replace  AspayoffData7 = -1 if game == "Dict 3"
	replace  AspayoffData8 = -1 if game == "Dict 3"

	replace  BspayoffData1 = 600 if game == "Dict 3"
	replace  BspayoffData2 = 500 if game == "Dict 3"
	replace  BspayoffData3 = -1 if game == "Dict 3"
	replace  BspayoffData4 = -1 if game == "Dict 3"
	replace  BspayoffData5 = -1 if game == "Dict 3"
	replace  BspayoffData6 = -1 if game == "Dict 3"
	replace  BspayoffData7 = -1 if game == "Dict 3"
	replace  BspayoffData8 = -1 if game == "Dict 3"


* "Dict 4"
	replace  AspayoffData1 = 200 if game == "Dict 4"
	replace  AspayoffData2 = 600 if game == "Dict 4"
	replace  AspayoffData3 = -1 if game == "Dict 4"
	replace  AspayoffData4 = -1 if game == "Dict 4"
	replace  AspayoffData5 = -1 if game == "Dict 4"
	replace  AspayoffData6 = -1 if game == "Dict 4"
	replace  AspayoffData7 = -1 if game == "Dict 4"
	replace  AspayoffData8 = -1 if game == "Dict 4"
  
	replace  BspayoffData1 = 700 if game == "Dict 4"
	replace  BspayoffData2 = 600 if game == "Dict 4"
	replace  BspayoffData3 = -1 if game == "Dict 4"
	replace  BspayoffData4 = -1 if game == "Dict 4"
	replace  BspayoffData5 = -1 if game == "Dict 4"
	replace  BspayoffData6 = -1 if game == "Dict 4"
	replace  BspayoffData7 = -1 if game == "Dict 4"
	replace  BspayoffData8 = -1 if game == "Dict 4"


* "Dict 5"  
	replace  AspayoffData1 = 0 if game == "Dict 5"
	replace  AspayoffData2 = 400 if game == "Dict 5"
	replace  AspayoffData3 = -1 if game == "Dict 5"
	replace  AspayoffData4 = -1 if game == "Dict 5"
	replace  AspayoffData5 = -1 if game == "Dict 5"
	replace  AspayoffData6 = -1 if game == "Dict 5"
	replace  AspayoffData7 = -1 if game == "Dict 5"
	replace  AspayoffData8 = -1 if game == "Dict 5"
  
	replace  BspayoffData1 = 800 if game == "Dict 5"
	replace  BspayoffData2 = 400 if game == "Dict 5"
	replace  BspayoffData3 = -1 if game == "Dict 5"
	replace  BspayoffData4 = -1 if game == "Dict 5"
	replace  BspayoffData5 = -1 if game == "Dict 5"
	replace  BspayoffData6 = -1 if game == "Dict 5"
	replace  BspayoffData7 = -1 if game == "Dict 5"
	replace  BspayoffData8 = -1 if game == "Dict 5"  


* "Resp 1a"  
	replace  AspayoffData1 = -1 if game == "Resp 1a"
	replace  AspayoffData2 = -1 if game == "Resp 1a"
	replace  AspayoffData3 = -1 if game == "Resp 1a"
	replace  AspayoffData4 = 750 if game == "Resp 1a"
	replace  AspayoffData5 = 750 if game == "Resp 1a"
	replace  AspayoffData6 = -1 if game == "Resp 1a"
	replace  AspayoffData7 = 400 if game == "Resp 1a"
	replace  AspayoffData8 = 750 if game == "Resp 1a"
 
	replace  BspayoffData1 = -1 if game == "Resp 1a"
	replace  BspayoffData2 = -1 if game == "Resp 1a"
	replace  BspayoffData3 = -1 if game == "Resp 1a"
	replace  BspayoffData4 = 0 if game == "Resp 1a"
	replace  BspayoffData5 = 0 if game == "Resp 1a"
	replace  BspayoffData6 = -1 if game == "Resp 1a"
	replace  BspayoffData7 = 400 if game == "Resp 1a"
	replace  BspayoffData8 = 400 if game == "Resp 1a"
  

* "Resp 1b"  
	replace   AspayoffData1 = -1 if game == "Resp 1b"
	replace   AspayoffData2 = -1 if game == "Resp 1b"
	replace   AspayoffData3 = -1 if game == "Resp 1b"
	replace   AspayoffData4 = 550 if game == "Resp 1b"
	replace   AspayoffData5 = 550 if game == "Resp 1b"
	replace   AspayoffData6 = -1 if game == "Resp 1b"
	replace   AspayoffData7 = 400 if game == "Resp 1b"
	replace   AspayoffData8 = 750 if game == "Resp 1b"
  
	replace   BspayoffData1 = -1 if game == "Resp 1b"
	replace   BspayoffData2 = -1 if game == "Resp 1b"
	replace   BspayoffData3 = -1 if game == "Resp 1b"
	replace   BspayoffData4 = 550 if game == "Resp 1b"
	replace   BspayoffData5 = 550 if game == "Resp 1b"
	replace   BspayoffData6 = -1 if game == "Resp 1b"
	replace   BspayoffData7 = 400 if game == "Resp 1b"
	replace   BspayoffData8 = 400 if game == "Resp 1b"
  

* "Resp 6"
	replace   AspayoffData1 = -1 if game == "Resp 6"
	replace   AspayoffData2 = -1 if game == "Resp 6"
	replace   AspayoffData3 = -1 if game == "Resp 6"
	replace   AspayoffData4 = 100 if game == "Resp 6"
	replace   AspayoffData5 = 100 if game == "Resp 6"
	replace   AspayoffData6 = -1 if game == "Resp 6"
	replace   AspayoffData7 = 75 if game == "Resp 6"
	replace   AspayoffData8 = 125 if game == "Resp 6"

	replace   BspayoffData1 = -1 if game == "Resp 6"
	replace   BspayoffData2 = -1 if game == "Resp 6"
	replace   BspayoffData3 = -1 if game == "Resp 6"
	replace   BspayoffData4 = 1000 if game == "Resp 6"
	replace   BspayoffData5 = 1000 if game == "Resp 6"
	replace   BspayoffData6 = -1 if game == "Resp 6"
	replace   BspayoffData7 = 125 if game == "Resp 6"
	replace   BspayoffData8 = 125 if game == "Resp 6"
  
  
* "Resp 7"  
	replace   AspayoffData1 = -1 if game == "Resp 7"
	replace   AspayoffData2 = -1 if game == "Resp 7"
	replace   AspayoffData3 = -1 if game == "Resp 7"
	replace   AspayoffData4 = 450 if game == "Resp 7"
	replace   AspayoffData5 = 450 if game == "Resp 7"
	replace   AspayoffData6 = -1 if game == "Resp 7"
	replace   AspayoffData7 = 200 if game == "Resp 7"
	replace   AspayoffData8 = 400 if game == "Resp 7"
    
	replace   BspayoffData1 = -1 if game == "Resp 7"
	replace   BspayoffData2 = -1 if game == "Resp 7"
	replace   BspayoffData3 = -1 if game == "Resp 7"
	replace   BspayoffData4 = 900 if game == "Resp 7"
	replace   BspayoffData5 = 900 if game == "Resp 7"
	replace   BspayoffData6 = -1 if game == "Resp 7"
	replace   BspayoffData7 = 400 if game == "Resp 7"
	replace   BspayoffData8 = 400 if game == "Resp 7"


* "Resp 2a"
	replace  AspayoffData1 = -1 if game == "Resp 2a"
	replace  AspayoffData2 = -1 if game == "Resp 2a"
	replace  AspayoffData3 = -1 if game == "Resp 2a"
	replace  AspayoffData4 = 750 if game == "Resp 2a"
	replace  AspayoffData5 = 750 if game == "Resp 2a"
	replace  AspayoffData6 = -1 if game == "Resp 2a"
	replace  AspayoffData7 = 400 if game == "Resp 2a"
	replace  AspayoffData8 = 750 if game == "Resp 2a"
  
	replace  BspayoffData1 = -1 if game == "Resp 2a"
	replace  BspayoffData2 = -1 if game == "Resp 2a"
	replace  BspayoffData3 = -1 if game == "Resp 2a"
	replace  BspayoffData4 = 0 if game == "Resp 2a"
	replace  BspayoffData5 = 0 if game == "Resp 2a"
	replace  BspayoffData6 = -1 if game == "Resp 2a"
	replace  BspayoffData7 = 400 if game == "Resp 2a"
	replace  BspayoffData8 = 375 if game == "Resp 2a"
  
  
* "Resp 2b"  
	replace  AspayoffData1 = -1 if game == "Resp 2b"
	replace  AspayoffData2 = -1 if game == "Resp 2b"
	replace  AspayoffData3 = -1 if game == "Resp 2b"
	replace  AspayoffData4 = 550 if game == "Resp 2b"
	replace  AspayoffData5 = 550 if game == "Resp 2b"
	replace  AspayoffData6 = -1 if game == "Resp 2b"
	replace  AspayoffData7 = 400 if game == "Resp 2b"
	replace  AspayoffData8 = 750 if game == "Resp 2b"

	replace  BspayoffData1 = -1 if game == "Resp 2b"
	replace  BspayoffData2 = -1 if game == "Resp 2b"
	replace  BspayoffData3 = -1 if game == "Resp 2b"
	replace  BspayoffData4 = 550 if game == "Resp 2b"
	replace  BspayoffData5 = 550 if game == "Resp 2b"
	replace  BspayoffData6 = -1 if game == "Resp 2b"
	replace  BspayoffData7 = 400 if game == "Resp 2b"
	replace  BspayoffData8 = 375 if game == "Resp 2b"

* "Resp 3"  
	replace  AspayoffData1 = -1 if game == "Resp 3"
	replace  AspayoffData2 = -1 if game == "Resp 3"
	replace  AspayoffData3 = -1 if game == "Resp 3"
	replace  AspayoffData4 = 750 if game == "Resp 3"
	replace  AspayoffData5 = 750 if game == "Resp 3"
	replace  AspayoffData6 = -1 if game == "Resp 3"
	replace  AspayoffData7 = 300 if game == "Resp 3"
	replace  AspayoffData8 = 700 if game == "Resp 3"
  
	replace  BspayoffData1 = -1 if game == "Resp 3"
	replace  BspayoffData2 = -1 if game == "Resp 3"
	replace  BspayoffData3 = -1 if game == "Resp 3"
	replace  BspayoffData4 = 100 if game == "Resp 3"
	replace  BspayoffData5 = 100 if game == "Resp 3"
	replace  BspayoffData6 = -1 if game == "Resp 3"
	replace  BspayoffData7 = 600 if game == "Resp 3"
	replace  BspayoffData8 = 500 if game == "Resp 3"


* "Resp 4"
	replace  AspayoffData1 = -1 if game == "Resp 4"
	replace  AspayoffData2 = -1 if game == "Resp 4"
	replace  AspayoffData3 = -1 if game == "Resp 4"
	replace  AspayoffData4 = 700 if game == "Resp 4"
	replace  AspayoffData5 = 700 if game == "Resp 4"
	replace  AspayoffData6 = -1 if game == "Resp 4"
	replace  AspayoffData7 = 200 if game == "Resp 4"
	replace  AspayoffData8 = 600 if game == "Resp 4"
  
	replace  BspayoffData1 = -1 if game == "Resp 4"
	replace  BspayoffData2 = -1 if game == "Resp 4"
	replace  BspayoffData3 = -1 if game == "Resp 4"
	replace  BspayoffData4 = 200 if game == "Resp 4"
	replace  BspayoffData5 = 200 if game == "Resp 4"
	replace  BspayoffData6 = -1 if game == "Resp 4"
	replace  BspayoffData7 = 700 if game == "Resp 4"
	replace  BspayoffData8 = 600 if game == "Resp 4"
  

* "Resp 5a"
	replace  AspayoffData1 = -1 if game == "Resp 5a"
	replace  AspayoffData2 = -1 if game == "Resp 5a"
	replace  AspayoffData3 = -1 if game == "Resp 5a"
	replace  AspayoffData4 = 800 if game == "Resp 5a"
	replace  AspayoffData5 = 800 if game == "Resp 5a"
	replace  AspayoffData6 = -1 if game == "Resp 5a"
	replace  AspayoffData7 = 0 if game == "Resp 5a"
	replace  AspayoffData8 = 400 if game == "Resp 5a"

	replace  BspayoffData1 = -1 if game == "Resp 5a"
	replace  BspayoffData2 = -1 if game == "Resp 5a"
	replace  BspayoffData3 = -1 if game == "Resp 5a"
	replace  BspayoffData4 = 0 if game == "Resp 5a"
	replace  BspayoffData5 = 0 if game == "Resp 5a"
	replace  BspayoffData6 = -1 if game == "Resp 5a"
	replace  BspayoffData7 = 800 if game == "Resp 5a"
	replace  BspayoffData8 = 400 if game == "Resp 5a"


* "Resp 5b"
	replace  AspayoffData1 = -1 if game == "Resp 5b"
	replace  AspayoffData2 = -1 if game == "Resp 5b"
	replace  AspayoffData3 = -1 if game == "Resp 5b"
	replace  AspayoffData4 = 0 if game == "Resp 5b"
	replace  AspayoffData5 = 0 if game == "Resp 5b"
	replace  AspayoffData6 = -1 if game == "Resp 5b"
	replace  AspayoffData7 = 0 if game == "Resp 5b"
	replace  AspayoffData8 = 400 if game == "Resp 5b"

	replace  BspayoffData1 = -1 if game == "Resp 5b"
	replace  BspayoffData2 = -1 if game == "Resp 5b"
	replace  BspayoffData3 = -1 if game == "Resp 5b"
	replace  BspayoffData4 = 800 if game == "Resp 5b"
	replace  BspayoffData5 = 800 if game == "Resp 5b"
	replace  BspayoffData6 = -1 if game == "Resp 5b"
	replace  BspayoffData7 = 800 if game == "Resp 5b"
	replace  BspayoffData8 = 400 if game == "Resp 5b"


* "Resp 8"
	replace   AspayoffData1 = -1 if game == "Resp 8"
	replace   AspayoffData2 = -1 if game == "Resp 8"
	replace   AspayoffData3 = -1 if game == "Resp 8"
	replace   AspayoffData4 = 725 if game == "Resp 8"
	replace   AspayoffData5 = 725 if game == "Resp 8"
	replace   AspayoffData6 = -1 if game == "Resp 8"
	replace   AspayoffData7 = 400 if game == "Resp 8"
	replace   AspayoffData8 = 750 if game == "Resp 8"

	replace   BspayoffData1 = -1 if game == "Resp 8"
	replace   BspayoffData2 = -1 if game == "Resp 8"
	replace   BspayoffData3 = -1 if game == "Resp 8"
	replace   BspayoffData4 = 0 if game == "Resp 8"
	replace   BspayoffData5 = 0 if game == "Resp 8"
	replace   BspayoffData6 = -1 if game == "Resp 8"
	replace   BspayoffData7 = 400 if game == "Resp 8"
	replace   BspayoffData8 = 375 if game == "Resp 8"
  

* "Resp 9"
	replace   AspayoffData1 = -1 if game == "Resp 9"
	replace   AspayoffData2 = -1 if game == "Resp 9"
	replace   AspayoffData3 = -1 if game == "Resp 9"
	replace   AspayoffData4 = 450 if game == "Resp 9"
	replace   AspayoffData5 = 450 if game == "Resp 9"
	replace   AspayoffData6 = -1 if game == "Resp 9"
	replace   AspayoffData7 = 350 if game == "Resp 9"
	replace   AspayoffData8 = 450 if game == "Resp 9"

	replace   BspayoffData1 = -1 if game == "Resp 9"
	replace   BspayoffData2 = -1 if game == "Resp 9"
	replace   BspayoffData3 = -1 if game == "Resp 9"
	replace   BspayoffData4 = 0 if game == "Resp 9"
	replace   BspayoffData5 = 0 if game == "Resp 9"
	replace   BspayoffData6 = -1 if game == "Resp 9"
	replace   BspayoffData7 = 450 if game == "Resp 9"
	replace   BspayoffData8 = 350 if game == "Resp 9"


* "Resp 10"  
	replace  AspayoffData1 = -1 if game == "Resp 10"
	replace  AspayoffData2 = -1 if game == "Resp 10"
	replace  AspayoffData3 = -1 if game == "Resp 10"
	replace  AspayoffData4 = 375 if game == "Resp 10"
	replace  AspayoffData5 = 375 if game == "Resp 10"
	replace  AspayoffData6 = -1 if game == "Resp 10"
	replace  AspayoffData7 = 400 if game == "Resp 10"
	replace  AspayoffData8 = 350 if game == "Resp 10"
  
	replace  BspayoffData1 = -1 if game == "Resp 10"
	replace  BspayoffData2 = -1 if game == "Resp 10"
	replace  BspayoffData3 = -1 if game == "Resp 10"
	replace  BspayoffData4 = 1000 if game == "Resp 10"
	replace  BspayoffData5 = 1000 if game == "Resp 10"
	replace  BspayoffData6 = -1 if game == "Resp 10"
	replace  BspayoffData7 = 400 if game == "Resp 10"
	replace  BspayoffData8 = 350 if game == "Resp 10"

  

* "Resp 11"  
	replace  AspayoffData1 = -1 if game == "Resp 11"
	replace  AspayoffData2 = -1 if game == "Resp 11"
	replace  AspayoffData3 = -1 if game == "Resp 11"
	replace  AspayoffData4 = 400 if game == "Resp 11"
	replace  AspayoffData5 = 400 if game == "Resp 11"
	replace  AspayoffData6 = -1 if game == "Resp 11"
	replace  AspayoffData7 = 400 if game == "Resp 11"
	replace  AspayoffData8 = 0 if game == "Resp 11"
  
	replace  BspayoffData1 = -1 if game == "Resp 11"
	replace  BspayoffData2 = -1 if game == "Resp 11"
	replace  BspayoffData3 = -1 if game == "Resp 11"
	replace  BspayoffData4 = 1200 if game == "Resp 11"
	replace  BspayoffData5 = 1200 if game == "Resp 11"
	replace  BspayoffData6 = -1 if game == "Resp 11"
	replace  BspayoffData7 = 200 if game == "Resp 11"
	replace  BspayoffData8 = 0 if game == "Resp 11"


* "Resp 12"  
	replace   AspayoffData1 = -1 if game == "Resp 12"
	replace   AspayoffData2 = -1 if game == "Resp 12"
	replace   AspayoffData3 = -1 if game == "Resp 12"
	replace   AspayoffData4 = 375 if game == "Resp 12"
	replace   AspayoffData5 = 375 if game == "Resp 12"
	replace   AspayoffData6 = -1 if game == "Resp 12"
	replace   AspayoffData7 = 400 if game == "Resp 12"
	replace   AspayoffData8 = 250 if game == "Resp 12"
  

	replace   BspayoffData1 = -1 if game == "Resp 12"
	replace   BspayoffData2 = -1 if game == "Resp 12"
	replace   BspayoffData3 = -1 if game == "Resp 12"
	replace   BspayoffData4 = 1000 if game == "Resp 12"
	replace   BspayoffData5 = 1000 if game == "Resp 12"
	replace   BspayoffData6 = -1 if game == "Resp 12"
	replace   BspayoffData7 = 400 if game == "Resp 12"
	replace   BspayoffData8 = 350 if game == "Resp 12"

*"Resp 13a"  
	replace   AspayoffData1 = -1 if game == "Resp 13a"
	replace   AspayoffData2 = -1 if game == "Resp 13a"
	replace   AspayoffData3 = -1 if game == "Resp 13a"
	replace   AspayoffData4 = 750 if game == "Resp 13a"
	replace   AspayoffData5 = 750 if game == "Resp 13a"
	replace   AspayoffData6 = -1 if game == "Resp 13a"
	replace   AspayoffData7 = 800 if game == "Resp 13a"
	replace   AspayoffData8 = 0 if game == "Resp 13a"

	replace   BspayoffData1 = -1 if game == "Resp 13a"
	replace   BspayoffData2 = -1 if game == "Resp 13a"
	replace   BspayoffData3 = -1 if game == "Resp 13a"
	replace   BspayoffData4 = 750 if game == "Resp 13a"
	replace   BspayoffData5 = 750 if game == "Resp 13a"
	replace   BspayoffData6 = -1 if game == "Resp 13a"
	replace   BspayoffData7 = 200 if game == "Resp 13a"
	replace   BspayoffData8 = 0 if game == "Resp 13a"

* "Resp 13b"  
	replace   AspayoffData1 = -1 if game == "Resp 13b"
	replace   AspayoffData2 = -1 if game == "Resp 13b"
	replace   AspayoffData3 = -1 if game == "Resp 13b"
	replace   AspayoffData4 = 750 if game == "Resp 13b"
	replace   AspayoffData5 = 750 if game == "Resp 13b"
	replace   AspayoffData6 = -1 if game == "Resp 13b"
	replace   AspayoffData7 = 800 if game == "Resp 13b"
	replace   AspayoffData8 = 0 if game == "Resp 13b"
  
	replace   BspayoffData1 = -1 if game == "Resp 13b"
	replace   BspayoffData2 = -1 if game == "Resp 13b"
	replace   BspayoffData3 = -1 if game == "Resp 13b"
	replace   BspayoffData4 = 750 if game == "Resp 13b"
	replace   BspayoffData5 = 750 if game == "Resp 13b"
	replace   BspayoffData6 = -1 if game == "Resp 13b"
	replace   BspayoffData7 = 200 if game == "Resp 13b"
	replace   BspayoffData8 = 50 if game == "Resp 13b"
  

* "Resp 13c"  
	replace   AspayoffData1 = -1 if game == "Resp 13c"
	replace   AspayoffData2 = -1 if game == "Resp 13c"
	replace   AspayoffData3 = -1 if game == "Resp 13c"
	replace   AspayoffData4 = 750 if game == "Resp 13c"
	replace   AspayoffData5 = 750 if game == "Resp 13c"
	replace   AspayoffData6 = -1 if game == "Resp 13c"
	replace   AspayoffData7 = 800 if game == "Resp 13c"
	replace   AspayoffData8 = 0 if game == "Resp 13c"

	replace   BspayoffData1 = -1 if game == "Resp 13c"
	replace   BspayoffData2 = -1 if game == "Resp 13c"
	replace   BspayoffData3 = -1 if game == "Resp 13c"
	replace   BspayoffData4 = 750 if game == "Resp 13c"
	replace   BspayoffData5 = 750 if game == "Resp 13c"
	replace   BspayoffData6 = -1 if game == "Resp 13c"
	replace   BspayoffData7 = 200 if game == "Resp 13c"
	replace   BspayoffData8 = 100 if game == "Resp 13c"
  
  
* "Resp 13d"  
	replace   AspayoffData1 = -1 if game == "Resp 13d"
	replace   AspayoffData2 = -1 if game == "Resp 13d"
	replace   AspayoffData3 = -1 if game == "Resp 13d"
	replace   AspayoffData4 = 750 if game == "Resp 13d"
	replace   AspayoffData5 = 750 if game == "Resp 13d"
	replace   AspayoffData6 = -1 if game == "Resp 13d"
	replace   AspayoffData7 = 800 if game == "Resp 13d"
	replace   AspayoffData8 = 0 if game == "Resp 13d"


	replace   BspayoffData1 = -1 if game == "Resp 13d"
	replace   BspayoffData2 = -1 if game == "Resp 13d"
	replace   BspayoffData3 = -1 if game == "Resp 13d"
	replace   BspayoffData4 = 750 if game == "Resp 13d"
	replace   BspayoffData5 = 750 if game == "Resp 13d"
	replace   BspayoffData6 = -1 if game == "Resp 13d"
	replace   BspayoffData7 = 200 if game == "Resp 13d"
	replace   BspayoffData8 = 150 if game == "Resp 13d"

	gen fake_aspayoff = .
	gen fake_bspayoff = .

	replace fake_aspayoff = AspayoffData1 if case_num == 1 & fake_aspayoff == . 
	replace fake_aspayoff = AspayoffData2 if case_num == 2 & fake_aspayoff == . 
	replace fake_aspayoff = AspayoffData3 if case_num == 3 & fake_aspayoff == .  
	replace fake_aspayoff = AspayoffData4 if case_num == 4 & fake_aspayoff == .
	replace fake_aspayoff = AspayoffData5 if case_num == 5 & fake_aspayoff == .  
	replace fake_aspayoff = AspayoffData6 if case_num == 6 & fake_aspayoff == . 
	replace fake_aspayoff = AspayoffData7 if case_num == 7 & fake_aspayoff == .  
	replace fake_aspayoff = AspayoffData8 if case_num == 8 & fake_aspayoff == . 

	replace fake_bspayoff = BspayoffData1 if case_num == 1 & fake_bspayoff == . 
	replace fake_bspayoff = BspayoffData2 if case_num == 2 & fake_bspayoff == . 
	replace fake_bspayoff = BspayoffData3 if case_num == 3 & fake_bspayoff == .  
	replace fake_bspayoff = BspayoffData4 if case_num == 4 & fake_bspayoff == .
	replace fake_bspayoff = BspayoffData5 if case_num == 5 & fake_bspayoff == .  
	replace fake_bspayoff = BspayoffData6 if case_num == 6 & fake_bspayoff == . 
	replace fake_bspayoff = BspayoffData7 if case_num == 7 & fake_bspayoff == .  
	replace fake_bspayoff = BspayoffData8 if case_num == 8 & fake_bspayoff == . 

	gen fake_mypayoff = .
	replace fake_mypayoff = fake_aspayoff if myrole == 1
	replace fake_mypayoff = fake_bspayoff if myrole == 2

	sort treatment date round subject fake_sametype fake_mypayoff
	
	collapse (mean) fake_mypayoff myrole, by(treatment date game subject fake_sametype)

	save table5-exp-earnings.dta, replace
	
	
* Table 5 Top Panel: summary stats
	use table5-exp-earnings.dta, clear
	* ingr
	summ fake_mypayoff if treatment == "original" & fake_sametype == 1 & myrole == 1
	summ fake_mypayoff if treatment == "original" & fake_sametype == 1 & myrole == 2
	summ fake_mypayoff if treatment == "original" & fake_sametype == 1

	* ingr
	summ fake_mypayoff if treatment == "original" & fake_sametype == 0 & myrole == 1
	summ fake_mypayoff if treatment == "original" & fake_sametype == 0 & myrole == 2
	summ fake_mypayoff if treatment == "original" & fake_sametype == 0

	* control
	summ fake_mypayoff if treatment == "control" & myrole == 1
	summ fake_mypayoff if treatment == "control" & myrole == 2
	summ fake_mypayoff if treatment == "control"

* Table 5 Top Panel: hypotheses testing
	
	* ingr > outgr
	use table5-exp-earnings.dta, clear
	catenate sess_subj = date subject
	reg fake_mypayoff fake_sametype if treatment == "original" & myrole == 1, cluster(sess_subj)
	reg fake_mypayoff fake_sametype if treatment == "original" & myrole == 2, cluster(sess_subj)
	reg fake_mypayoff fake_sametype if treatment == "original", cluster(sess_subj)

	* ingr > contr
	use table5-exp-earnings.dta, clear
	catenate sess_subj = date subject
	gen original = (treatment == "original")
	drop if treatment == "original" & fake_sametype == 0
	reg fake_mypayoff original if myrole == 1, cluster(sess_subj)
	reg fake_mypayoff original if myrole == 2, cluster(sess_subj)
	reg fake_mypayoff original, cluster(sess_subj)

	* contr > outgr
	use table5-exp-earnings.dta, clear
	catenate sess_subj = date subject
	gen original = (treatment == "original")
	drop if treatment == "original" & fake_sametype == 1
	reg fake_mypayoff original if myrole == 1, cluster(sess_subj)
	reg fake_mypayoff original if myrole == 2, cluster(sess_subj)
	reg fake_mypayoff original, cluster(sess_subj)


	erase temp-expected-earnings.dta
	erase temp-random-pairing.dta
	erase temp-fakeopp_id1.dta
	erase temp-fakeopp_id2.dta
	erase temp-fakeopp_id3.dta
	erase temp-fakeopp_id4.dta
	erase temp-fakeopp_id5.dta
	erase temp-fakeopp_id6.dta
	erase temp-fakeopp_id7.dta
	erase temp-fakeopp_id8.dta
	erase temp-fakeopp_id9.dta
	erase temp-fakeopp_id10.dta
	erase temp-fakeopp_id11.dta
	erase temp-fakeopp_id12.dta
	erase temp-fakeopp_id13.dta
	erase temp-fakeopp_id14.dta
	erase temp-fakeopp_id15.dta
	erase temp-fakeopp_id16.dta
	erase temp-fakeopp_id.dta
	erase temp-exppayoff.dta

** The end **