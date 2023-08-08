options nofmterr;

libname RB 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\RB';
libname dat 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data';


proc format;
value timefmt 
1 = 'early childhood(0-6 yrs)'
2 = 'childhood(7-10 yrs)'
3 = 'adolescent(11-14 yrs)'
4 = 'other(>14 yrs)';

run;

*****************************************************************************************************************;
*Risk Behavior Outcomes;

*****************************************************************************************************************;

*-------------------------------------------------------------------------------------;
*Delinquent or Violent Behaviors;

*How many times in the last year have you: 
DELA3= Made obscene telephone calls such as calling someone and saying dirty things? 
DELA4=Been drunk in a public place? 
DELA5=Purposely damaged or destroyed property that did not belong to you, (for example, breaking, cutting or marking up something)? 
DELA6= Purposely set fire to a house, building, car, or other property or tried to do so? 
DELA7= Avoided paying for things such as movies, bus or subway rides, food, or computer services? 
DELA8= Gone into or tried to go into a building to steal something? 
DELA9= Stolen or tried to steal money or things worth $5 or less? 
DELA10= Stolen or tried to steal money or things worth between $5 and $50? 
DELA11= Stolen or tied to steal money or things worth more than $50 but less than $100? 
DELA12= Stolen or tried to steal money or things worth $100 or more? 
DELA13= Snatched someone’s purse or wallet or picked someone’s pocket? 
DELA14= Knowingly bought, sold or held stolen goods or tried to do any of these things. 
DELA15= Gone joyriding, that is, taken a motor vehicle such as a car or motorcycle for a ride or drive without the owner’s permission?
DELA16= Stolen or tried to steal a motor vehicle such as a car or motor cycle? 
DELA17= Used a slug or fake money to pay for something? 
DELA18= Used or tried to use checks, credit or bankcards without the owner’s permission? 
DELA19= Tried to cheat someone by selling them something that was worthless or not what you 
said it was? 
DELA20= Attacked someone with a weapon or with the idea of seriously hurting or killing them? 
DELA21= How many times in the last year have you hit someone with the idea of hurting them (other than the events you just mentioned)? 
DELA22= How many times in the last year have you used a weapon, force, or strong-arm methods to get money or things from people? 
DELA23= Thrown objects such as rocks or bottles at people (other than events you have already mentioned)? 
DELA24= Been involved in gang or posse fights? 
DELA25= Been paid for having sexual relations with someone? 
DELA26= Physically hurt or threatened to hurt someone to get them to have sex with you? 
DELA28= How many times in the past year have you been arrested?  

If 1 or 2 or 3, then YES 
0 = Never 
1 = One or two times 
2 = Between three and nine times 
3 = 10 or more times 
;

proc freq data=RB.dela1201;
table DELA3-DELA26 DELA28/missing; run;

%macro sum(v1,num,v2);
array &num {*} &v2;
if Sum(of &num{*})>=1 then &v1=1;
if Sum(of &num{*}) =0 then &v1=0;
if Sum(of &num{*}) =. then &v1=.;
%mend;

Data DELA1;
set RB.dela1201;
Array V DELA3-DELA28; 
Do _N_ = 1 to 26;
if V{_N_}in (0) then V{_N_}=0; /* 0=0, 1,2,3=1*/
if V{_N_}in (1 2 3) then V{_N_}=1;
if V{_N_}in (.) then V{_N_}=.;
end;
%sum(RB_deliq_lo, M1,dela1 dela2 dela3 dela7 dela9);
%sum(RB_deliq_Mod,M2,dela4 dela5 dela10 dela13 dela14 dela15 dela17 dela18 dela19 dela21 dela23);
%sum(RB_deliq_hi, M3,dela6 dela8 dela11 dela16 dela20 dela22 dela24 dela25);
%sum(RB_deliq,    M0,dela3-dela26 dela28);
%sum(RB_vio,      M4,dela1 dela20 dela21 dela22 dela23 dela24 dela26);
label RB_deliq_Lo ='Minor Delinquent or Violent Behaviors'
	  RB_deliq_mod='Moderate Delinquent or Violent Behaviors'
	  RB_deliq_Hi ='Serious Delinquent or Violent Behaviors'
	  RB_deliq    ='Delinquent or Violent Behaviors'
	  RB_vio      ='Violent Behaviors'
;
KEEP center id visit DELA1-DELA28 RB_:;
RUN;

proc freq data=DELA1;
table DELA3-DELA26 DELA28 RB_deliq:;
run;

*Some checking;
data chk;
set DELA1;
if sum(of DELA3, DELA4, DELA5, DELA6, DELA7, DELA8, DELA9, DELA10, DELA11, DELA12, DELA13, DELA14, DELA15, DELA16, DELA17, DELA18, DELA19, DELA20, DELA21,
DELA22, DELA23, DELA24, DELA25, DELA26, DELA28) IN (. 0);
*if sum(of dela1, dela2, dela3, dela7, dela9) in (. 0);
run;
proc freq data=chk; table rb:;
run;

*Sexual Health- Risky Sexual Behaviors ;
* DELA25= Been paid for having sexual relations with someone? 
If 1, 2 or 3, then YES 
0 = Never 
1 = One or two times 
2 = Between three and nine times 
3 = 10 or more times  

JVQa8= In the last year, did anyone make you do other sexual things when you didn’t want them to? 
0 = No 1 = Yes 

JVQA10= In the last year, did anyone try to force you to have sex, that is, sexual intercourse of any kind, even if it didn’t happen? 
0 = No 1 = Yes ;

Data DELA2;
set RB.dela1201;
keep center id visit DELA25;
run;

Data JVQA;
set RB.JVQA1201;
keep center id visit JVQA8 JVQA10 JVQA13;
run;
*racial discrimination;
Proc freq data=JVQA;
table visit JVQA13 visit*JVQA13;
run;

Proc sort data=dela2; by center id visit; run;
proc sort data=jvqa;  by center id visit; run;

Data RSB;
Merge dela2 jvqa;
by center id visit;
RB_sexb=0;
if dela25 in (1 2 3) | jvqa8 = 1 | jvqa10 = 1 then RB_sexb=1;
if dela25=. & jvqa8=. & jvqa10=. then RB_sexb=.;
label RB_sexb='Sexual Health- Risky Sexual Behaviors';
run;

Proc freq data=RSB;
table dela25 jvqa8 jvqa10 RB_sexb;
run;
*some checking;
proc print data=RSB (obs=10);
where rb_sexb=.;
var dela25 jvqa8 jvqa10 RB_sexb;
run;

*Sexual Health- Pregnancy ;
*YSUA12= In the last 12 months, did you get pregnancy testing at a clinic 
0 = No 
1 = Yes OR 
AHSA30= In the last 12 months, did you get medical services for pregnancy? 
1 = Yes 
0 = No OR  
ASEC5A= Have you ever been pregnant OR ASEC5B= Have you ever gotten someone pregnant 
0= No 
1= YES 
8= don’t know 
ASEC5B= Have you ever gotten someone pregnant ;

Data YSUA1;
set RB.YSUA1201;
keep center id visit YSUA12;
run;

Data AHSA1;
set RB.AHSA1201;
keep center id visit AHSA30;
run;

Data ASEC;
set RB.ASEC1201;
keep center id visit ASEC5A ASEC5b;
run;
proc freq data=asec; table asec:; run;

proc sort data=YSUA1; by center id visit; run;
proc sort data=AHSA1; by center id visit; run;
proc sort data=ASEC;  by center id visit; run;

Data Preg;
Merge YSUA1 AHSA1 ASEC;
by center id visit;
*redo RB_preg variable, take out YSUA12;
Array V AHSA30 ASEC5a ASEC5b;
Do _N_ = 1 to 3;
if V{_N_}=0 then V{_N_}=0; 
if V{_N_}=1 then V{_N_}=1;
if V{_N_}=. | V{_N_}=8 then V{_N_}=.;
end;
if sum(of AHSA30, ASEC5a, ASEC5b)>=1 then RB_preg=1;
if sum(of AHSA30, ASEC5a, ASEC5b)= 0 then RB_preg=0;
if sum(of AHSA30, ASEC5a, ASEC5b)= . then RB_preg=.;
label RB_preg='Sexual Health- Pregnancy';

keep center id visit AHSA30 ASEC5a ASEC5b RB_preg;
run;

Proc freq data=Preg;
table AHSA30 ASEC5a ASEC5b;
run;

*Sexual Health- STD/HIV ;
*YSUA14= In the last 12 months, did you get medical care for HIV/AIDS, other than getting tested? 
0 = No 
1 = Yes OR 
AHSB10= In the last 12 months, have you had hepatitis? 
AHSB11= Do you have HIV/AIDS? ;

Data YSUA2;
set RB.YSUA1201;
keep center id visit YSUA14 YSUA9;
run;

Data AHSB1;
set RB.AHSB1201;
keep center id visit AHSB10 AHSB11 AHSB12;
run;

proc sort data=ysua2; by center id visit; run;
proc sort data=ahsb1; by center id visit; run;

Data hiv;
merge ysua2 ahsb1;
by center id visit;
*take out ASHB10 HEPATITIS;
if sum(YSUA14, AHSB11) >=1 then RB_HIV=1;
if sum(YSUA14, AHSB11) =0  then RB_HIV=0;
if sum(YSUA14, AHSB11) =.  then RB_HIV=.;

label RB_HIV='Sexual Health- STD/HIV';
Keep center id visit YSUA14 AHSB11 RB_HIV YSUA9 AHSB12;
run;

Proc freq data=HIV;
table YSUA14 AHSB11 RB_HIV YSUA9 AHSB12;
run;

proc print data=HIV(obs=20);
where rb_hiv=0;
var ysua14 ahsb11;
run;
*Substance Dependence and Substance Abuse ;
*Ysua9= In the last 12 months, did you get drug/alcohol treatment or counseling? 
0 = No 1 = Yes 
TADA1= In the past year did you ever smoke cigarettes? 
TADA2= In the past year did you drink beer, wine, wine coolers, malt liquor, or hard liquor?
TADA3= In the past year did you ever use or try marijuana (weed, pot, or grass)? 
TADA4= In the past year, did you use or try any other illegal drugs or drugs that were not prescribed to you by a doctor? 
0 = No 1 = Yes 
;
Data TADA;
set RB.TADA1201;
keep center id visit TADA1-TADA4;
run;
Proc freq data=tada; table tada1-tada4;run;

Proc sort data=TADA; by center id visit; run;
Data SU;
merge YSUA2 TADA;
by center id visit;
if sum(of YSUA9, TADA1, TADA2, TADA3, TADA4) =0 then RB_su=0;
else if sum(of YSUA9, TADA1, TADA2, TADA3, TADA4)>=1 then RB_su=1;
else if sum(of YSUA9, TADA1, TADA2, TADA3, TADA4) =. then RB_su=.;
label RB_su='Substance Dependence and Substance Abuse';
Keep center id visit YSUA9 TADA1-TADA4 RB_su;
run;
Proc freq data=SU;
table YSUA9 TADA1-TADA4 RB_su;
run;
proc print data=su (obs=20);
where rb_su=.;
var YSUA9 TADA1 TADA2 TADA3 TADA4 rb_su;
run;

*Injury Outcomes 
*Youth Peer Victimization
JVQA2= In the last year, did anyone hit or attack you on purpose WITH an object or weapon? Somewhere like at home, 
at school or work, at a store, in a car, on the street or anywhere else? 
0= no 1= yes 
JVQA13= Have you ever been hit or attacked because of your skin color, religion, or where your family comes from? 
How about because of some physical problem you have, or because someone said you were gay or lesbian? ;

Data VIC;
set RB.JVQA1201;
if JVQA2=1 | jvqa13=1 then inj_victim=1;
else if JVQA2=0 | jvqa13=0 then inj_victim=0;
else if JVQA2=. & jvqa13=. then inj_victim=.;
label inj_victim='Injury-Youth Peer Victimization';
keep center id visit JVQA2 JVQA13 inj_victim;
run;

proc freq data=VIC;
table JVQA: inj_victim;
run;
proc print data=VIC(obs=50);
where inj_victim=.;
run;

* Physical health 
AHSB12= In the past year, have you had any long-term chronic health problems or conditions that lasted more than three months? 
0 = No 1 = Yes ;
*YBPA55= I am overweight 
0=Not true 1=Somewhat or sometimes true 2= Very true or often true;
Data ahsb2;
set RB.AHSB1201;
keep center id visit ahsb12;
run;

Data YBPA;
set RB.YBPA1201;
keep center id visit YBPA55;
run;
Proc sort data=YBPA; by center id visit; run;
proc sort data=ahsb2;by center id visit; run;

Data PH;
merge YBPA AHSB2;
by center id visit;
PH_Health=0;
if ahsb12=1 | ybpa55 = 2 then PH_health=1;
if ahsb12=. & ybpa55=. then PH_health=.;
label PH_health='Physical health-Chronic disease or Obese';
keep center id visit ahsb12 ybpa55 PH_health;
run;

Proc freq data=PH;
table ahsb12 ybpa55 PH_health;
run;

Proc print data=PH(obs=50);
where PH_health=.;
var ahsb12 ybpa55 PH_health;
run;

* Future Opportunities and Aspirations 
(Future Expectations for Self – PFEA
Future Events Questionnaire – FEQB )
*PFEA4c= What are your goals for yourself for the upcoming year?
How likely is it that you will… 
FEQB1= have children 
FEQB2= get married
FEQB3= go to college 
FEQB9= be able to get the kind of job you want 
;

/*Data PFEA;
set RB.PFEA1201;
keep center id visit PFEA4c;
run;*/
proc contents data=RB.FEQB1201;run;
proc freq data=RB.FEQB1201;
table visit FEQB1 FEQB2  FEQB9 FEQB3 FEQB4 FEQB5 FEQB10 ;
run;
Data FEQB;
set RB.FEQB1201;
Array V FEQB3 FEQB4 FEQB5 FEQB10;/*reverse coding*/
Array S FEQB3R FEQB4R FEQB5R FEQB10R;
Do _N_ = 1 to 4;
if V{_N_}=1 then S{_N_}=5; 
if V{_N_}=2 then S{_N_}=4;
if V{_N_}=3 then S{_N_}=3;
if V{_N_}=4 then S{_N_}=2;
if V{_N_}=5 then S{_N_}=1;
end;
*FEQB positive vs. negative aspirations;
future_op_pos=mean(of FEQB1,FEQB2,FEQB6,FEQB7,FEQB8,FEQB9);
future_op_neg=mean(of FEQB3R,FEQB4R,FEQB5R,FEQB10R);

label 	future_op_pos='Positive future Opportunities and Aspirations()'
		future_op_neg='Negative Future Opportunities and Aspirations()'
		FEQB3R='Likely: have child without being married' 
		FEQB4R='Likely: that you will get divorced' 
		FEQB5R='Likely: you will get on welfare sometime' 
		FEQB10R='Likely: you be employed sometime';
keep center id visit FEQB1 FEQB2  FEQB9 FEQB3 FEQB4 FEQB5 FEQB10 future_op:;
run;

Proc sort data=FEQB; by center id visit; run;

*only keep age18 for outcome, age 16 for future aspiration;
Data DELA1;
set DELA1;
if visit>=16;
*drop visit;
run;

Data RSB;
set RSB;
if visit>=16;
*drop visit;
run;

Data PREG;
set PREG;
if visit>=16;
*drop visit;
run;

Data HIV;
set HIV;
if visit>=16;
*drop visit;
run;

Data SU;
set SU;
if visit>=16;
*drop visit;
run;

Data VIC;
set VIC;
if visit>=16;
*drop visit;
run;

Data PH;
set PH;
if visit>=16;
*drop visit;
run;

Data FEQB;
set FEQB;
if visit>=16;
*drop visit;
run;

Data RB;
merge DELA1 RSB PREG HIV SU VIC PH FEQB;
by center id visit;
run;

*combine age 16-age18 outcomes;
Proc means data=RB;
class visit;
var RB_deliq: RB_vio RB_sexb RB_preg
RB_HIV RB_su inj_victim PH_health future_op:;
run;

proc summary data=RB;
class center id;
var RB_deliq: RB_vio RB_sexb RB_preg 
RB_su;
output out=RB_c1(drop=_type_ _freq_) max=;
run;

Data RB_c1;
set RB_c1;
if center ne ' ' & id ne ' ';
run;

Data RB_c2;
set RB;
if visit=18;
Keep center id 
RB_HIV 		/*age18 only*/
inj_victim 	/*age18 only*/
PH_Health 	/*age18 only*/
;
run;

Data RB_c3;
set RB;
if visit=16;
Keep center id 
future_op:	/*age16 only*/
;
run;

Data RBacrosstime;
merge RB_c1 RB_c2 RB_c3;
by center id;
run;


Data dat.RB1;
set RB;
run;

Data dat.RBacrosstime;
set RBacrosstime;
run;

Data RB1;
set dat.RB1;
run;

Data RBacrosstime;
set dat.RBacrosstime;
run;







ODS RTF FILE = 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output/Rb_frequency_12_14_2022.rtf' STYLE= minimal;
proc freq data=OUT;
table RB: inj_victim PH_health;
RUN;
title 'Risk behavior outcomes';
proc freq data=out;
table DELA3-DELA26 DELA28 RB_deliq:;
run;
Proc freq data=out;
table dela25 jvqa8 jvqa10 RB_sexb;
run;
Proc freq data=out;
table YSUA12 AHSA30 ASEC5a ASEC5b RB_preg;
run;
Proc freq data=out;
table YSUA14 AHSB10 AHSB11 RB_HIV YSUA9 AHSB12;
run;
Proc freq data=out;
table YSUA9 TADA1-TADA4 RB_su;
run;
title 'Injury outcomes';
proc freq data=out;
table JVQA: inj_victim;
run;
title 'Physical health outcomes';
Proc freq data=out;
table ahsb12 ybpa55 PH_health;
run;
title 'Future aspiration outcomes';
Proc freq data=out;
table FEQB1-FEQB3 FEQB9;
run;
Proc means data=out;
var future_op;
run;

ODS RTF CLOSE;
title;


ODS RTF FILE = 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output/Risk_beh_Reliability_1_5_2023.rtf' STYLE= minimal;
title 'item reliability for all deliquency items (r=0.86)';
proc freq data=dela1;
table dela3-dela26 dela28 RB_deliq RB_vio; 
run;
Proc corr data=dela1 ALPHA nomiss;
	var dela3-dela26 dela28;
Run;
title 'item reliability for violence items(r=0.66)';
Proc corr data=dela1 ALPHA nomiss;
	var dela1 dela20 dela21 dela22 dela23 dela24 dela26;
Run;
title 'item reliability for minor deliquency items(r=0.62)';
Proc corr data=dela1 ALPHA nomiss;
	var dela1 dela2 dela3 dela7 dela9;
Run;
title 'item reliability for moderate deliquency items(r=0.73)';
Proc corr data=dela1 ALPHA nomiss;
	var dela4 dela5 dela10 dela13 dela14 dela15 dela17 dela18 dela19 dela21 dela23;
Run;
title 'item reliability for severe deliquency items(r=0.67)';
Proc corr data=dela1 ALPHA nomiss;
	var dela6 dela8 dela11 dela16 dela20 dela22 dela24 dela25;
Run;

/*title 'PCA for all deliquency items';
Proc factor data=dela1
simple corr
method=prin
priors=one
mineigen=1.0
ev score;
var dela3-dela26 dela28;
run;*/

ODS RTF CLOSE;
title;
