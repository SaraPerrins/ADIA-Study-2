options nofmterr;

libname pce 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\PCE';
libname dat 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data';
libname MH  'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\MH';

proc format;
value adultfmt
10 = 'Mother' 
11 = 'Stepmother' 
12 = 'Foster mother' 
13 = 'Grandmother' 
14 = "Father’s girlfriend" 
15 = 'Sister' 
16 = 'Other female relative' 
17 = 'Adult female friend' 
20 = 'Father' 
21 = 'Stepfather' 
22 = 'Foster father' 
23 = 'Grandfather' 
24 = "Mother’s boyfriend" 
25 = 'Brother' 
26 = 'Other male relative' 
27 = 'Adult male friend' 
29 = 'Other relative, gender unknown' 
30 = 'Babysitter' 
31 = 'Teacher' 
32 = 'Therapist' 
33 = 'School Counselor' 
34 = 'Pastor/ Clergy/ Rabbi' 
35 = 'Caseworker' 
36 = 'Victim Advocate' 
37 = 'Physician/ Nurse' 
40 = 'Other (specify)' ;

value timefmt 
1 = 'early childhood(0-6 yrs)'
2 = 'childhood(7-10 yrs)'
3 = 'adolescent(11-14 yrs)'
/*4 = 'other(>14 yrs)'*/;

run;


/******************************************************************/
/**************************supportive environment******************/
/******************************************************************/;


*16. Number of years the child has lived in the neighborhood (residential stability)
NOAA2: How long child has [child] lived in this 
neighborhood? (If s/he has moved in and out, how long has s/he lived in this neighborhood since the last time s/he moved in?) ;
*17. Caregiver perception of neighborhood safety during adolescence
NOAA25 = In this neighborhood, I always feel safe ;
*18. Caregiver perception of positive neighborhood experiences during adolescence—presence of supportive adults in the community (neighborhood’s collective efficacy) 
(calculated score 0-10, strongly disagree/disagree = 0, agree/strongly agree = 1) 
NOAA5 = Neighbors would intervene if skip school 
NOAA6 = Adults set good examples 
NOAA8 = People around here help their neighbors 
NOAA14 = This is a close knit neighborhood 
NOAA18= Adults act in responsible ways 
NOAA20= Neighbors can be trusted;

Data NOAA;
set pce.NOAA1201; /*age12, 14 & 16*/
if sum(of NOAA33 NOAA34 NOAA35 NOAA36)>=2 then parent_involv=1; /* change to at least 2 out of the 4, then yes!!!!*/
else parent_involv=0;
if sum(of NOAA33 NOAA34 NOAA35 NOAA36)=. then parent_involv=.;

resid_stab=NOAA2;
*binary coding 19 20 items to calculated score 0-10, strongly disagree/disagree = 0 agree/strongly agree = 1;
	array V NOAA3 NOAA22 NOAA25 NOAA28 NOAA5 NOAA6 NOAA8 NOAA14 NOAA18 NOAA20;
	array R NOAA3R NOAA22R NOAA25R NOAA28R NOAA5R NOAA6R NOAA8R NOAA14R NOAA18R NOAA20R;

	do _N_ = 1 to 10;
		if V{_N_}=1 then R{_N_]=0;
		if V{_N_}=2 then R{_N_]=0;
		if V{_N_}=3 then R{_N_]=0;
		if V{_N_}=4 then R{_N_]=1;
	end;

neighborhood_safety=NOAA25R;

if sum(of NOAA5R NOAA6R NOAA8R NOAA14R NOAA18R NOAA20R)>=3 then neighborhood_exp=1;/* at least 3 out of the 6 =yes!!!*/
else  neighborhood_exp=0;
if sum(of NOAA5R NOAA6R NOAA8R NOAA14R NOAA18R NOAA20R)= . then neighborhood_exp = .;

/*if sum(of NOAA5R NOAA6R NOAA8R NOAA14R NOAA18R NOAA20R)>=6 then chk=1 n=1158 43%*/;
if visit<=6 & visit>=0 then time=1;
if visit>6 & visit<=10 then time=2;
if visit> 10 & visit<=14 then time=3;
if visit>14 then time=4;

Label parent_involv="Strength of parental involvement in youth activities during adolescence as a Protective Factor"
resid_stab="Number of years the child has lived in the neighborhood (residential stability)"
neighborhood_safety="Caregiver perception of neighborhood safety during adolescence"
neighborhood_exp="Caregiver perception of positive neighborhood experiences during adolescence—presence of supportive adults in the community (neighborhood’s collective efficacy)";
keep center id time NOAA2 NOAA5 NOAA6 NOAA8 NOAA14 NOAA18 NOAA20 NOAA33 NOAA34 NOAA35 NOAA36 parent_involv: resid_stab neighborhood_safety: neighborhood_exp:;
Run;

proc freq data=NOAA;
table neighborhood_safety*time neighborhood_exp*time;
run;

*19. Caregiver perception of positive neighborhood experiences during childhood 
NEA1= People in this neighborhood help each other out. 
NEA4= We watch out for each other’s children in this neighborhood. 
NEA5= I’m proud to live in this neighborhood. 
NEA7 = There are people I can count on in this neighborhood. 
SCORING 
If 1, then YES, If 2-4, then NO 
1 = VERY MUCH LIKE your neighborhood/community 
2 = SOMEWHAT LIKE your neighborhood/ community 
3 = VERY LITTLE LIKE your neighborhood/ community 
4 = NOT AT ALL LIKE your neighborhood/community ;

Data NEIA;
SET PCE.NEIA0404(rename=(neia1=nea1 neia4=nea4 neia5=nea5 neia7=nea7)); /* age 0 - 4 */
keep center id visit nea1 nea4 nea5 nea7;
run;
Data NEA;
SET PCE.NEA0603; /* age 8 */
keep center id visit nea1 nea4 nea5 nea7;
run;
proc sort data=neia; by center id visit; run;
proc sort data=nea;  by center id visit; run;

Data NEA2;
Merge neia nea;
by center id visit;
Array V nea1 nea4 nea5 nea7;
Do _N_ = 1 to 4;
if V{_N_} in (2 3 4) then V{_N_}=0;
else if V{_N_} in (1) then V{_N_}=1;
End;

if sum(of nea1 nea4 nea5 nea7) >=2 then neighborhood_exp=1; /* at least 2 out of the 4 to be yes!!!*/
else neighborhood_exp=0;
IF nea1=. & nea4=. & nea5=. & nea7=. then neighborhood_exp=.;
/*if nea1=1 & nea4=1 & nea5=1 & nea7=1 then chk=1 n=377 13% */;
if visit<=6 & visit>=0 then time=1;
if visit>6 & visit<=10 then time=2;
if visit> 10 & visit<=14 then time=3;
if visit>14 then time=4;
run;
proc freq data=nea2;
table neighborhood_exp*time/missing;
run;

*20. Youth perception of community safety 
WVA12= How often do you feel safe when you are outside in your neighborhood 
SCORING 
If 4, then YES If 0-3, then NO 
0 = Never 1 = 1 time 2 = 2 times 3 = 3 times 4 = >3 times;
*22. Youth perception of school safety
WVA10= How often do you feel safe when you are at school 
SCORING 
If 4, then YES If 0-3, then NO 
0 = Never 1 = 1 time 2 = 2 times 3 = 3 times 4 = >3 times ;
Data WVA;
set PCE.WVA0603; /* age 8 */
if WVA12 = 4 then neighborhood_safety = 1;
else if WVA12 in (0 1 2 3) then neighborhood_safety = 0;
*03/20/2023, check with Jessie;
neighborhood_safety_s=WVA12;
if WVA10 = 4 then school_safety_y = 1;
else if wva10 in (0 1 2 3) then school_safety_y = 0;

if visit<=6 & visit>=0 then time=1;
if visit>6 & visit<=10 then time=2;
if visit> 10 & visit<=14 then time=3;
if visit>14 then time=4;
keep center id time wva10 wva12 neighborhood_safety: school_safety_y:;
label school_safety_y="Youth perception of community safety";
run;
proc freq data=wva;
table neighborhood_safety*time school_safety_y*time;
run;

*21. Teacher perception of school safety during early childhood
Ssa2= school safe place for teachers & students 
Ssa3= school is in safe neighborhood 
SCORING 
If 1, then YES, If 2-5 then NO 
1 = Very much like my school 
2 = Like my school  
3 = Somewhat like my school  
4 = Not very much like my school 
5 = Not at all like my school;
Proc freq Data = pce.ssa0708;
table visit ssa2 ssa3;
run;
Proc freq Data = PCE.safa0404;
table visit safa2 safa3;
run;

Data safa;
set PCE.safa0404(rename=(safa2=ssa2 safa3=ssa3)); /* age 4, 5 & 6*/
keep center id visit ssa2 ssa3;
run;
Data ssa;
set pce.ssa0708; /* age 8 & 10 */
keep center id visit ssa2 ssa3;
run;
proc sort data=safa; by center id visit; run;
proc sort data=ssa; by center id visit; run;

Data safa_ssa;
merge safa ssa;
by center id visit;
*binary coding If 1, then YES, If 2-5 then NO;
array V ssa2 ssa3;
array R ssa2R ssa3R;
do _N_ = 1 to 2;
if V{_N_} in (1 ) then R{_N_]=1;
if V{_N_} in (2 3 4 5)   then R{_N_]=0;
end;
if sum(of ssa2R ssa3R) >=1 then school_safety_t=1;
if sum(of ssa2R ssa3R) = 0 then school_safety_t=0;
if sum(of ssa2R ssa3R) = . then school_safety_t=.;
if visit<=6 & visit>=0 then time=1;
if visit>6 & visit<=10 then time=2;
if visit> 10 & visit<=14 then time=3;
if visit>14 then time=4;
label school_safety_t="Teacher perception of school safety during early childhood";
keep center id time ssa2 ssa3 school_safety_t:;
run;

proc sort data=safa_ssa; by center id visit; run;

proc freq data=safa_ssa;
table school_safety_t:; run;