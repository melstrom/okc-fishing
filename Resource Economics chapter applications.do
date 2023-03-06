******************************************
***************SP analysis****************
******************************************
clear
use "C:\Users\rmelstrom\Desktop\Research\Paper ideas\Resource economics recreational fishing\Stata work\Resource economics stated preference data.dta"

gen tcost = 2*(distance*.2837+(distance/40)*(income/2000)/2)
replace playground = playground*children

clogit choice tcost catfish sunfish playground restrooms banktrees optout, group(choice_id) cluster(angler_id)


gen newamenity = (alternative!=2)
preserve
********************************************
************Adding playgrounds**************
********************************************
replace newamenity = newamenity*children
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[optout]*optout)
by choice_id, sort: egen sumv = sum(exp_v)
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[playground]*newamenity + _b[optout]*optout)
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore, preserve
********************************************
*************Adding restrooms***************
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[optout]*optout)
by choice_id, sort: egen sumv = sum(exp_v)
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish +_b[restroom]*newamenity + _b[optout]*optout)
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore, preserve
********************************************
*************Adding bank trees**************
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[optout]*optout)
by choice_id, sort: egen sumv = sum(exp_v)
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish +_b[banktrees]*newamenity + _b[optout]*optout)
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore

********************************************
***************Adding sunfish***************
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[optout]*optout)
by choice_id, sort: egen sumv = sum(exp_v)
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish*1.25 + _b[optout]*optout)
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp


******************************************
***************RP analysis****************
******************************************
clear
use "C:\Users\rmelstrom\Desktop\Research\Paper ideas\Resource economics recreational fishing\Stata work\Resource economics revealed preference data.dta"
replace income = 12500 if (income==1)
replace income = 37499.5 if (income==2)
replace income = 62499.5 if (income==3)
replace income = 87499.5 if (income==4)
replace income = 124999.5 if (income==5)
replace income = 150000 if (income==6)
**empty cells are replaced with the average of nonmissing values
replace income = 83228.47 if missing(income)

gen pond = (acres==1)
replace catfish = 0 if distance>5&pond==1&choice==0
replace sunfish = 0 if distance>5&pond==1&choice==0
replace bass = 0 if distance>5&pond==1&choice==0

gen logacres = ln(acres)
gen catchall = (waterbody=="Other")
replace logacres = 0 if catchall==1
replace distance = 50 if catchall==1
replace oklahomacity=0 if catchall==1
gen tcost = 2*(distance*.2837+(distance/40)*(income/2000)/2)
drop if distance>200

clogit choice tcost catfish sunfish bass logacres oklahomacity pond catchall, group(choice_id) cluster(angler_id)

preserve
********************************************
***********Closing Lake Hefner**************
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall)
by choice_id, sort: egen sumv = sum(exp_v)
gen s = exp_v/sumv
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall) if waterbody!="Hefner"
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen sstar = exp_vstar/sumvstar
table waterbody if oklahomacity==1, c(m s m sstar m choice m tcost)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore,preserve
********************************************
***********Closing Lake Stanley Draper******
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall)
by choice_id, sort: egen sumv = sum(exp_v)
gen s = exp_v/sumv
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall) if waterbody!="Stanley Draper"
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen sstar = exp_vstar/sumvstar
table waterbody if oklahomacity==1, c(m s m sstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore,preserve
********************************************
*Closing OKC Ponds (including Crystal Lake)*
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall)
by choice_id, sort: egen sumv = sum(exp_v)
gen s = exp_v/sumv
gen okcpond = (waterbody=="OKC Pond")
replace okcpond = (waterbody=="Crystal") if okcpond==0
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall) if okcpond==0
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen sstar = exp_vstar/sumvstar
table waterbody if oklahomacity==1, c(m s m sstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore, preserve

********************************************
***************Adding sunfish***************
********************************************
gen exp_v = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall)
by choice_id, sort: egen sumv = sum(exp_v)
gen exp_vstar = exp(_b[tcost]*tcost + _b[catfish]*catfish + _b[sunfish]*sunfish*1.25 + _b[bass]*bass + _b[logacres]*logacres + _b[oklahomacity]*oklahomacity + _b[pond]*pond + _b[catchall]*catchall)
by choice_id, sort: egen sumvstar = sum(exp_vstar)
gen wtp = (ln(sumvstar) - ln(sumv))/(-1*_b[tcost])
sum wtp
restore

