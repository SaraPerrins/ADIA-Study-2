options nofmterr;

libname OCS 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\OCS';
libname dat 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data';


proc format;
value timefmt 
1 = 'early childhood(0-6 yrs)'
2 = 'childhood(7-10 yrs)'
3 = 'adolescent(11-14 yrs)'
4 = 'other(>14 yrs)';

run;

/******************************************************************/
/************OTHER CHILDHOOD STRESSORS AND EXPANDED ACES **********/
/******************************************************************/

/*Emotional Neglect  
RNS10D= Neglect 
RNA10F= Caretaker absence/incapacity 
RNA10G= Emotional maltreatment 
RNA10H= Moral/legal/educational neglect 
RNS10J= General neglect 
RNA10K= severe neglect  
If YES to any, then Yes 
1= YES 
2= No ;
proc contents data=ocs.RNAB1201; run;
proc freq data=ocs.RNAB1201;
table RNA10D RNA10F RNA10G RNA10H RNA10J RNA10K/missing; run;
Data rnab;
set ocs.rnab1201;
if RNA10D>=1 | RNA10F>=1 | RNA10G>=1 | RNA10H>=1 | RNA10J>=1 | RNA10K>=1 then ocs_neglect_beh=0;
if RNA10D=1 | RNA10F=1 | RNA10G=1 | RNA10H=1 | RNA10J=1 | RNA10K=1 then ocs_neglect_beh=1;
label ocs_neglect_beh="Neglectful behavior";
keep center id visit RNA10D RNA10F RNA10G RNA10H RNA10J RNA10K ocs_neglect_beh;
run;
Proc sort data=rnab;by center id visit; Run;
proc freq data=rnab;
table visit*ocs_neglect_beh/missing;
run;*/

*About my parents (Revised Neglectful Behavior Scale) - AMPA
When you were in elementary school
AMPA1A= how often did you parents do things with you just for fun? 
•	AMPA2A = how often were your parents interested in your activities or hobbies?
•	AMPA3A= how often did your parents(s) help you with your homework?
•	AMPA4A= how often did your parent(s) want to know what you were doing if you were not at home? 
AMPA5A= how often did your parent(s) comfort you if you were upset?
•	AMPA6A= how often did your parent(s) make sure that you bathed regularly?
•	AMPA7A= how often did your parent(s) help you to do your best?
•	AMPA8A = how often did your parent(s) make sure you always went to school?
•	AMPA9A= how often did your parent(s) care if you got into trouble in school?
•	AMPA10A= how often did your parent(s) make sure that you saw a doctor when you needed one?
•	AMPA11A= how often were your parent(s) interested in the kind of friends that you had?
•	AMPA12A= how often did your parent(s) give you enough to eat?
•	AMPA13A= how often did your parent(s) help you when you had trouble understanding something?
•	AMPA14A= how often did your parent(s) read books to you?
•	AMPA15A= how often did your parent(s) help you when you had problems?
•	AMPA16A= how often did your parent(s) praise you? 

•	AMPA17A= how often did your parent(s) care if you did bad things, like shoplifting?
•	AMPA18A= how often did your parent(s) tell you they loved you?
•	AMPA19A= how often did your parent(s) keep the house clean?
•	AMPA20A= how often did your parent(s) give you enough clothes to keep you warm?
•	AMPA21A= how often did your parent(s) take care of you when you were sick?
•	AMPA22A= how often did your parent(s) have something for you to eat when you were hungry?
AMPA23a= how often did your parent(s) make sure that you had somewhere safe to play?

•	AMPA24A= how often did your parent(s) leave you home alone after dark?
•	AMPA25A= how often did your parent(s) leave you home alone during the day? 
r
0 = Never
1= Almost Never
2= Sometimes
3= A lot ;
/*these are measureing 7-10 yrs, use B for adolescent*/

proc contents data=ocs.AMPA1002 ORDER=VARNUM; run;
proc freq data=ocs.AMPA1002;
table AMPA1A	AMPA2A	AMPA3A	AMPA4A	AMPA5A	AMPA6A	AMPA7A	AMPA8A	AMPA9A	AMPA10A	AMPA11A	AMPA12A	AMPA13A	AMPA14A	AMPA15A	AMPA16A	AMPA17A	AMPA18A	AMPA19A	
AMPA20A	AMPA21A	AMPA22A	AMPA23A	AMPA24A	AMPA25A/missing; run;
Data AMPA;
set ocs.AMPA1002;
Array V AMPA1A	AMPA2A	AMPA3A	AMPA4A	AMPA5A	AMPA6A	AMPA7A	AMPA8A	AMPA9A	AMPA10A	AMPA11A	AMPA12A	AMPA13A	AMPA14A	AMPA15A	AMPA16A	AMPA17A	AMPA18A	AMPA19A	
AMPA20A	AMPA21A	AMPA22A	AMPA23A	AMPA24A	AMPA25A; 
Do _N_ = 1 to 25;
if V{_N_}in (1 2 3) then V{_N_}=9;
if V{_N_}in (0) then V{_N_}=1; /* 0=1, 1,2,3=0*/
if V{_N_}in (9) then V{_N_}=0;
end;
KEEP center id visit AMPA1A	AMPA2A	AMPA3A	AMPA4A	AMPA5A	AMPA6A	AMPA7A	AMPA8A	AMPA9A	AMPA10A	AMPA11A	AMPA12A	AMPA13A	AMPA14A	AMPA15A	AMPA16A	AMPA17A	AMPA18A	AMPA19A	
AMPA20A	AMPA21A	AMPA22A	AMPA23A	AMPA24A	AMPA25A;
RUN;
proc freq data=AMPA;
table AMPA1A	AMPA2A	AMPA3A	AMPA4A	AMPA5A	AMPA6A	AMPA7A	AMPA8A	AMPA9A	AMPA10A	AMPA11A	AMPA12A	AMPA13A	AMPA14A	AMPA15A	AMPA16A	AMPA17A	AMPA18A	AMPA19A	
AMPA20A	AMPA21A	AMPA22A	AMPA23A	AMPA24A	AMPA25A /missing; run;
Data AMPA;
set AMPA;
if sum(of AMPA1A -- AMPA25A)>=1 then ocs_neglect_beh=1;
if sum(of AMPA1A -- AMPA25A)= 0  then ocs_neglect_beh=0;
if sum(of AMPA1A -- AMPA25A)= . then ocs_neglect_beh=.;

label ocs_neglect_beh="Neglectful behavior";
keep center id visit AMPA1A -- AMPA25A ocs_neglect_beh;
run;
Proc sort data=AMPA;by center id visit; Run;
proc freq data=AMPA;
table visit*ocs_neglect_beh/missing;
run;

/*Harsh Physical Violence
(IF YES to any of the timepoints, then YES)
RNA10B= Physical abuse;
Data rnab10B;
set ocs.rnab1201;
if RNA10B=1 then ocs_harsh_pv=1;
if RNA10B=2 then ocs_harsh_pv=0;
label ocs_harsh_pv="Harsh Physical Violence";
keep center id visit RNA10B ocs_harsh_pv;
run;
Proc sort data=rnab10B;by center id visit; Run;
proc freq data=rnab10B;
table RNA10B visit*ocs_harsh_pv/missing;
run;*/
*Over the last six months have you or anyone else had to do any of the following things to [child] because of his / her behavior?

DMA17= Push, grab or shove him/ her.

DMA18= Spank him / her

DMA19= Slap him/ her

SMA20- Kick, or bite, or hit, him or her with a fist

DMA21= Hit or try to hit him/ her with some THING. (like a
switch or a belt or a hairbrush)

DMA22= Beat him/her up

DMA23= burn him or her , or scald him/her with hot water

DMA24= Threaten him/ her with a knife or fun

DMA25= Use a knife or gun on him/ her
 

0 = Did not occur
1 = Respondent ONLY
2 = Other(s) than
respondent
3 = Respondent with
others;
proc contents data=ocs.DMA0603 ORDER=VARNUM; run;
proc freq data=ocs.DMA0603;
table DMA17-DMA25/missing; run;
Data DMA;
set ocs.DMA0603;
Array V DMA17-DMA25; 
Do _N_ = 1 to 8;
if V{_N_}in (0) then V{_N_}=0;
if V{_N_}in (1 2 3) then V{_N_}=1;
end;
if sum(of DMA17-DMA25)>=1 then ocs_harsh_pv=1;
if sum(of DMA17-DMA25)= 0  then ocs_harsh_pv=0;
if sum(of DMA17-DMA25)= . then ocs_harsh_pv=.;
label ocs_harsh_pv="Harsh Physical Violence";

KEEP center id visit DMA17-DMA25 ocs_harsh_pv;
RUN;
Proc sort data=DMA;by center id visit; Run;
proc freq data=DMA;
table visit  DMA17-DMA25 ocs_harsh_pv visit*ocs_harsh_pv/missing;
run;

*In the past year, how often have you: 
PCCT4= Hit him/her on bottom with object
PCCT7= Hit him/her with fist or kicked hard
PCCT8= Spanked him/ her on the bottom with hand
PCCT9= Grabbed and choked him/her around neck
PCCT11= Beat him/her up
PCCT13= Burned of scalded him/her on purpose
PCCT16= Slapped him/her on the hand, arm or leg
PCCT19= threatened him/her with a knife or gun
PCCT20= Threw or knocked him/her down
PCC22= Slapped him/her on face head or ears

SCORING
0 = This has never happened
1 = Once in the past year
2 = Twice in the past year
3 = 3 – 5 times in the past year
4 = 6 – 10 times in the past year
5 = 11 – 20 times in the past year
6 = > 20 times in the past year
7 = Not in the past year but it did happen before ;

proc contents data=ocs.PCCT1201 ORDER=VARNUM; run;
proc freq data=ocs.PCCT1201;
table PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22/missing; run;
Data PCCT;
set ocs.PCCT1201;
Array V PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22; 
Do _N_ = 1 to 10;
if V{_N_}in (0) then V{_N_}=0;
if V{_N_} >= 1  then V{_N_}=1;
end;
if sum(of PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22)>=1 then ocs_harsh_pv=1;
if sum(of PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22)= 0  then ocs_harsh_pv=0;
if sum(of PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22)= . then ocs_harsh_pv=.;
label ocs_harsh_pv="Harsh Physical Violence";

KEEP center id visit PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22 ocs_harsh_pv;
RUN;
Proc sort data=PCCT;by center id visit; Run;
proc freq data=PCCT;
table visit  PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22 ocs_harsh_pv visit*ocs_harsh_pv/missing;
run;

*Basic Need Instability – Food Insecurity 

(IF YES to any of the timepoints, then YES);

*During the past 30 days, 

POM9= did your household run out of money to buy food
POM10= did you rely on a limited number of foods to feed your children because you were running out of money to buy food for a meal?
POM11= did you or adult members of your household eat
less than you felt because there was not enough money for food?
POM12= did you or adult members of your household cut the size of your meals or skip meals because there was not enough money for food?;
proc contents data=ocs.POM1201 ORDER=VARNUM; run;
proc freq data=ocs.POM1201;
table POM9 POM10 POM11 POM12/missing; run;
Data POM;
set ocs.POM1201;
Array V POM9 POM10 POM11 POM12; 
Do _N_ = 1 to 4;
if V{_N_}=8 then V{_N_}=.;
end;
if sum(of POM9 POM10 POM11 POM12)>=1 then ocs_BN_food=1;
if sum(of POM9 POM10 POM11 POM12)= 0  then ocs_BN_food=0;
if sum(of POM9 POM10 POM11 POM12)= . then ocs_BN_food=.;
label ocs_BN_food="Basic Need Instability – Food Insecurity";

KEEP center id visit POM9 POM10 POM11 POM12 ocs_BN_food;
RUN;
Proc sort data=POM;by center id visit; Run;
proc freq data=POM;
table visit  POM9 POM10 POM11 POM12 ocs_BN_food visit*ocs_BN_food/missing;
run;
*ESIA5= How much are you worried, upset or bothered from day to day re: Not enough money for basic necessities, such as clothing, housing, food, and health care.

SCORING
If 4, then YES

1 = Not at all bothered
2 = A little bothered
3 = Somewhat bothered
4 = Bothered a great deal;
Data esia;
set ocs.esia0404;
if ESIA5 in (1 2 3) then ocs_BN_cstressor=0;
if ESIA5 = 4 then ocs_BN_cstressor=1;
label ocs_BN_cstressor="Caregiver stressors";
keep center id visit ESIA5 ocs_BN_cstressor;
run;
Proc sort data=esia;by center id visit; Run;
proc freq data=esia;
table visit ESIA5 ocs_BN_cstressor visit*ocs_BN_cstressor/missing;
run;


*SERA1E= Respondent used financial help (Welfare, food stamps, housing, etc)

SCORING
0 = No
1= Yes;
Data sera;
set ocs.sera0404;
if SERA1E = 0 then ocs_BN_poverty=0;
if SERA1E = 1 then ocs_BN_poverty=1;
label ocs_BN_poverty="Basic Need Instability- Poverty";
keep center id visit SERA1E ocs_BN_poverty;
run;
Proc sort data=sera;by center id visit; Run;
proc freq data=sera;
table visit SERA1E ocs_BN_poverty visit*ocs_BN_poverty/missing;
run;

*What kind of services did you or your child get?

WFA1C= Regular on-going financial assistance (like TANF)
WFA1D= help with food or money for food
WFA1F= help with rent, utilities, clothing

SCORING
0=No
1= YES;
proc contents data=ocs.WFA0708 ORDER=VARNUM; run;
proc freq data=ocs.WFA0708;
table WFA1C WFA1D WFA1F/missing; run;
Data WFA;
set ocs.WFA0708;
if sum(of WFA1C WFA1D WFA1F)>=1 then ocs_BN_poverty=1;
if sum(of WFA1C WFA1D WFA1F)= 0  then ocs_BN_poverty=0;
if sum(of WFA1C WFA1D WFA1F)= . then ocs_BN_poverty=.;
label ocs_BN_poverty="Basic Need Instability- Poverty";

KEEP center id visit WFA1C WFA1D WFA1F ocs_BN_poverty;
RUN;
Proc sort data=WFA;by center id visit; Run;
proc freq data=WFA;
table visit WFA1C WFA1D WFA1F ocs_BN_poverty visit*ocs_BN_poverty/missing;
run;

*In the last 12 months
POM17= have you been late in making rent or mortgage payments because you didn’t have enough money?
POM18= have you had enough money to provide necessary clothing (like shoes or warm enough clothes) for your family?;
Data POM1;
set ocs.POM1201;
if POM17 =1 | POM18=1 then ocs_BN_poverty=1;
if sum(of POM17 POM18)= 0  then ocs_BN_poverty=0;
label ocs_BN_poverty="Basic Need Instability- Poverty";
KEEP center id visit POM17 POM18 ocs_BN_poverty;
RUN;
Proc sort data=POM1;by center id visit; Run;
proc freq data=POM1;
table visit  POM17 POM18 ocs_BN_poverty visit*ocs_BN_poverty/missing;
run;
*Basic Need Instability- Homelessness;

*LECA10= Was child ever homeless? (Or did she live at a homeless shelter) 
LECA11= were you or child’s family ever evicted this past year?
LEB10= was child ever homeless (or did she/he live in a homeless shelter)
LEB11= were you (or child’s family evicted in the past year)
SRUB8A- In the last year, did you ever need a homeless shelter as a place to stay?

SCORING
0= NO
1= YES;

Data LECA1;
set ocs.LECA0404;
if leca10=1 | leca11=1 then ocs_BN_homeless=1;
if leca10=0 & leca11=0 then ocs_BN_homeless=0;
label ocs_BN_homeless="Basic Need Instability- Homeless";
KEEP center id visit leca10 leca11 ocs_BN_homeless;
RUN;
Proc sort data=LECA1;by center id visit; Run;
proc freq data=LECA1;
table visit  leca10 leca11 ocs_BN_homeless visit*ocs_BN_homeless/missing;
run;

Data LEB1;
set ocs.LEB0708;
if LEB10=1 | LEB11=1 then ocs_BN_homeless=1;
if LEB10=0 & LEB11=0 then ocs_BN_homeless=0;
label ocs_BN_homeless="Basic Need Instability- Homeless";
KEEP center id visit LEB10 LEB11 ocs_BN_homeless;
RUN;
Proc sort data=LEB1;by center id visit; Run;
proc freq data=LEB1;
table visit  LEB10 LEB11 ocs_BN_homeless visit*ocs_BN_homeless/missing;
run;

Data SRUB1;
set ocs.SRUB1002;
ocs_BN_homeless=SRUB8A;
label ocs_BN_homeless="Basic Need Instability- Homeless";
KEEP center id visit SRUB8A ocs_BN_homeless;
RUN;
Proc sort data=SRUB1;by center id visit; Run;
proc freq data=SRUB1;
table visit  SRUB8A ocs_BN_homeless visit*ocs_BN_homeless/missing;
run;

*Traumatic separation from a loved one
(If yes to any time point, then yes);
*LECA4= did anybody separate
LECA4A= did child’s parents/caregiver separate
LECA5= did anybody divorce
LECA5A= did child’s parents/caregiver divorce
LEC6= did anyone move out during the past year
LEC13= did anyone who was close to child die during the past year


LEB4= Did anybody separate 
LEB4A= Did child’s parents/ caregiver separate 
LEB5- Did anybody divorce
LEB5A= Did child’s parents/caregiver divorce
LEB6= did anyone move out during the past year

LEB13= did anyone who was close to child die during the past year

LECC4= Did anybody separate?
LECC5= Did anybody divorce?
LECC6= Did anybody (that’s/he normally lives with) move out of the house for some other reason (not divorce or separation) in the past year?

0= NO
1= YES;

Data LECA2;
set ocs.LECA0404;
if LECA4=1 | LECA4A=1 | LECA5=1 | LECA5A=1 | LECA6=1 | LECA13=1 then ocs_sep=1;
if sum(of LECA4 LECA4A LECA5 LECA5A LECA6 LECA13)=0 then ocs_sep=0;
label ocs_sep="Traumatic separation from a loved one";
KEEP center id visit LECA4 LECA4A LECA5 LECA5A LECA6 LECA13 ocs_sep;
RUN;
Proc sort data=LECA2;by center id visit; Run;
proc freq data=LECA2;
table visit  LECA4 LECA4A LECA5 LECA5A LECA6 LECA13 ocs_sep visit*ocs_sep/missing;
run;

Data LEB2;
set ocs.LEB0708;
if LEB4=1 | LEB4a=1 | LEB5=1 | LEB5a=1 | LEB6=1 | LEB13=1 then ocs_sep=1;
if sum(of LEB4 LEB4a LEB5 LEB5a LEB6 LEB13)=0 then ocs_sep=0;
label ocs_sep="Traumatic separation from a loved one";
KEEP center id visit LEB4 LEB4a LEB5 LEB5a LEB6 LEB13 ocs_sep;
RUN;
Proc sort data=LEB2;by center id visit; Run;
proc freq data=LEB2;
table visit LEB4 LEB4a LEB5 LEB5a LEB6 LEB13 ocs_sep visit*ocs_sep/missing;
run;

Data LECC2;
set ocs.LECC1201;
if LECC4=1 | LECC5=1 | LECC6=1 then ocs_sep=1;
if sum(of LECC4 LECC5 LECC6)=0 then ocs_sep=0;
label ocs_sep="Traumatic separation from a loved one";
KEEP center id visit LECC4 LECC5 LECC6 ocs_sep;
RUN;
Proc sort data=LECC2;by center id visit; Run;
proc freq data=LECC2;
table visit  LECC4 LECC5 LECC6 ocs_sep visit*ocs_sep/missing;
run;

*Contact with Potentially traumatic system

(If yes to any time point, then yes);

*LEB18A= was anyone in child’s family or household arrested

WFA1B= Did you or [child] get assistance or services from the welfare department or Department of Social Services, in the last year? (age 7-9)

LEB19A= Was anyone in child’s family or household jailed or imprisoned?
0 = No
1= YES;

Data LEB3;
set ocs.LEB0708;
if LEB18a=1 | LEB19a=1 then ocs_trauma_sys=1;
if sum(of LEB18a LEB19a)=0 then ocs_trauma_sys=0;
label ocs_trauma_sys="Contact with Potentially traumatic system";
KEEP center id visit LEB18a LEB19a ocs_trauma_sys;
RUN;
Proc sort data=LEB3;by center id visit; Run;
proc freq data=LEB3;
table visit LEB18a LEB19a ocs_trauma_sys visit*ocs_trauma_sys/missing;
run;

Data WFA3;
set ocs.WFA0708;
ocs_trauma_sys=WFA1b;
label ocs_trauma_sys="Contact with Potentially traumatic system";
KEEP center id visit WFA1b ocs_trauma_sys;
RUN;
Proc sort data=WFA3;by center id visit; Run;
proc freq data=WFA3;
table visit WFA1b ocs_trauma_sys visit*ocs_trauma_sys/missing;
run;

*HWVA1= Have you ever seen someone arrested? 

HWVA1A= How many times have you ever seen someone arrested? 

Who did you see being arrested?
HWVA1B1= your father or someone like a father to you
HWVA1B2= your mother or someone like a mother to you
HWVA1B3= Your sister or brother
HWVA1B4= another family member of youth
HWVA1b5= a friend or someone you knew

SCORING
0= NO
1= YES


HWVA1C= How often have you seen someone arrested in the last year?

0 = Never
1= 1 time
2= 2-3 times
3= >4 

Who did you see being arrested?
HWVA1D1= your father or someone like a father to you
HWVA1D2= your mother or someone like a mother to you
HWVA1D3= Your sister or brother
HWVA1D4= another family member of youth
HWVA1D5= a friend or someone you knew

SCORING
0= NO
1= YES;
proc contents data=ocs.HWVA1002 ORDER=VARNUM; run;
proc freq data=ocs.HWVA1002;
table HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5/missing; run;
Data HWVA;
set ocs.HWVA1002;
Array V HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5; 
Do _N_ = 1 to 12;
if V{_N_}>=1 then V{_N_}=1;
if V{_N_}= 0 then V{_N_}=0;
end;
if sum(of HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5)>=1 then ocs_trauma_sys=1;
if sum(of HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5)= 0  then ocs_trauma_sys=0;
if sum(of HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5)= . then ocs_trauma_sys=.;
label ocs_trauma_sys="Contact with Potentially traumatic system";

KEEP center id visit HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5 ocs_trauma_sys;
RUN;
Proc sort data=HWVA;by center id visit; Run;
proc freq data=HWVA;
table visit  HWVA1 HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5 ocs_trauma_sys visit*ocs_trauma_sys/missing;
run;

*Contact with potentially traumatic system (court involvement)

(If yes to any time point, then yes);

*LEB17A= Was child required to be in court for any reason in the past year?

0 = No
1= YES

LEB17B
2= Custody hearing
3= Witness
4= Victim 

LECC18= Was child required to be in court for any reason in the past year?

0 = No
1= YES

LECC18A= custody hearing
LECC18B= foster care or child protection hearing

LECA17A= Was child required to be in court for any reason this past year?

0 = No
1= YES

LECA17B
2= Custody hearing
3= Witness
4= Victim;

*;
proc freq data=ocs.LECA0404;
table LECA17A LECA17B; run;
Data LECA4;
set ocs.LECA0404;
if LECA17A in (0 1) | LECA17B in (1 2 3 4) then ocs_trauma_court=0;
if LECA17A=1 | LECA17B >=2 then ocs_trauma_court=1;
label ocs_trauma_court="Contact with potentially traumatic system (court involvement)";
KEEP center id visit LECA17A LECA17B ocs_trauma_court;
RUN;
Proc sort data=LECA4;by center id visit; Run;
proc freq data=LECA4;
table visit  LECA17A LECA17B ocs_trauma_court visit*ocs_trauma_court/missing;
run;


*;
proc freq data=ocs.LEB0708;
table LEB17A LEB17B; run;
Data LEB4;
set ocs.LEB0708;
if LEB17A in (0 1) | LEB17B in (1 2 3 4 5) then ocs_trauma_court=0;
if LEB17a=1 | LEB17b in (2 3 4) then ocs_trauma_court=1;
if sum(of LEB17a LEB17b)=. then ocs_trauma_court=.;
label ocs_trauma_court="Contact with potentially traumatic system (court involvement)";
KEEP center id visit LEB17a LEB17b ocs_trauma_court;
RUN;
Proc sort data=LEB4;by center id visit; Run;
proc freq data=LEB4;
table visit LEB17a LEB17b ocs_trauma_court visit*ocs_trauma_court/missing;
run;

*;
Proc freq data=ocs.LECC1201;
table lecc18 lecc18a lecc18b; run;
Data LECC4;
set ocs.LECC1201;
if LECC18 >=0 | LECC18A >=0 | LECC18B >=0 then ocs_trauma_court=0;
if LECC18 =1  | LECC18A =1  | LECC18B =1  then ocs_trauma_court=1;
label ocs_trauma_court="Contact with potentially traumatic system (court involvement)";
KEEP center id visit LECC18 LECC18A LECC18B ocs_trauma_court;
RUN;
Proc sort data=LECC4;by center id visit; Run;
proc freq data=LECC4;
table visit  LECC18 LECC18A LECC18B ocs_trauma_court visit*ocs_trauma_court/missing;
run;


*Risk behaviors of caregiver and household members

(If yes to any time point, then yes);

*WVA22=How many times have you seen drugs in your home?

0 = Never
1 = 1 time
2 = 2 times
3 = 3 times
4 = >3 times;
Data WVA;
set ocs.WVA0603;
if WVA22=0    then ocs_RB_house=0;
if WVA22 >= 1 then ocs_RB_house=1;
label ocs_RB_house="Risk behaviors of caregiver and household members";
KEEP center id visit WVA22 ocs_RB_house;
RUN;
Proc sort data=WVA;by center id visit; Run;
proc freq data=WVA;
table visit WVA22 ocs_RB_house visit*ocs_RB_house/missing;
run;

*SUA9A= have you (or child) used or received a service like: alcohol or drug counseling? 

0= No
1= YES
Does anyone that you live with… ;
Data SUA;
set ocs.SUA0708;
ocs_RB_house=SUA9A;
label ocs_RB_house="Risk behaviors of caregiver and household members";
KEEP center id visit SUA9A ocs_RB_house;
RUN;
Proc sort data=SUA;by center id visit; Run;
proc freq data=SUA;
table visit SUA9A ocs_RB_house visit*ocs_RB_house/missing;
run;

*RBFA5 = use cocaine or crack?
RBFA6= use meth, speed, crystal or uppers?
RBFA7= inject drugs 

SCORING
No=0
YES= 1

RBFA9A= About how often is someone in your house drunk or high?

SCORING

1 = < Once a month
2 = 1 – 3 times a month
3 = Once or twice a
week
4 = Almost every day ;
proc freq data=ocs.RBFA1201;
table rbfa5 rbfa6 rbfa7 RBFA9A;
run;
Data RBFA;
set ocs.RBFA1201;
if RBFA5>=0 | RBFA6>=0 | RBFA7>=0 | RBFA9A in (1 2 3 4) then  ocs_RB_house=0;
if RBFA5=1 | RBFA6=1 | RBFA7=1 | RBFA9A in (3 4) then  ocs_RB_house=1;
label ocs_RB_house="Risk behaviors of caregiver and household members";
KEEP center id visit RBFA5 RBFA6 RBFA7 RBFA9A ocs_RB_house;
RUN;
Proc sort data=RBFA;by center id visit; Run;
proc freq data=RBFA;
table visit RBFA5 RBFA6 RBFA7 RBFA9A ocs_RB_house visit*ocs_RB_house/missing;
run;

*Risk behaviors of friends 


(If yes to any time point, then yes);
*SXA9A= has anyone ever asked you to help sell drugs by finding buyers, holding drugs or money, or acting as a lookout?

SXA9B= Have you ever helped anyone sell drugs by finding buyers, holding drugs or money, or acting as a lookout?

SXA9C= Do any of your friends sell drugs or help sell drugs by finding buyers, holding drugs or money, or acting as a lookout?

SCORING
0=No
1= Yes

If YES to any, then YES;
proc contents data=ocs.SXA0603;run;
Data SXA;
set ocs.SXA0603;
if sum(SXA9A, SXA9B, SXA9C)>=1 then ocs_RB_peer=1;
if sum(SXA9A, SXA9B, SXA9C)= 0 then ocs_RB_peer=0;
if sum(SXA9A, SXA9B, SXA9C)=. then ocs_RB_peer=.;
label ocs_RB_peer="Risk behaviors of friends";
KEEP center id visit SXA9A SXA9B SXA9C ocs_RB_peer;
RUN;
Proc sort data=SXA;by center id visit; Run;
proc freq data=SXA;
table visit SXA9A SXA9B SXA9C ocs_RB_peer visit*ocs_RB_peer/missing;
run;

*How many of your close friends…

RBFA15= smoke cigarettes
RBFA16= drink alcohol
RBFA18= carry guns, knives or other weapons
RBFA19= smoke marijuana
RBFA20= use cocaine or crack
RBFA21= use heroin
RBFA22= use other drugs
RBFA23= sell or deliver drugs ;
Data RBFA5;
set ocs.RBFA1201;
if sum(rbfa15, rbfa16, rbfa18, rbfa19, rbfa20, rbfa21, rbfa22, rbfa23)>=0 then ocs_RB_peer=0;
if sum(rbfa15, rbfa16, rbfa18, rbfa19, rbfa20, rbfa21, rbfa22, rbfa23)>=1 then ocs_RB_peer=1;
label ocs_RB_peer="Risk behaviors of friends";
KEEP center id visit rbfa15 rbfa16 rbfa18 rbfa19 rbfa20 rbfa21 rbfa22 rbfa23 ocs_RB_peer;
RUN;
Proc sort data=RBFA5;by center id visit; Run;
proc freq data=RBFA5;
table visit rbfa15 rbfa16 rbfa18 rbfa19 rbfa20 rbfa21 rbfa22 rbfa23 ocs_RB_peer visit*ocs_RB_peer/missing;
run;
*Victim of a crime

(If yes to any time point, then yes);

*LEB16= has your family been the victim of any property crime this past year;

Data LEB6;
set ocs.LEB0708;
ocs_crime_victim=LEB16;
label ocs_crime_victim="Victim of a crime"; /* wrong labels, duplicated with potentially traumatic system*/
KEEP center id visit LEB16 ocs_crime_victim;
RUN;
Proc sort data=LEB6;by center id visit; Run;
proc freq data=LEB6;
table visit LEB16 ocs_crime_victim visit*ocs_crime_victim/missing;
run;

*Witnessed violence 

(IF YES to any of the timepoints, then YES);

*LEB20= In the last year has child heard any loud, long arguments?

LEB21= Has child seen anyone physically threatened with a weapon?

LEB21B= Did physical threat with a weapon involve family members?

LEB22= Did s/he see anyone get shot or stabbed? (other than on TV or movies)


LEB23= Has s/he seen someone killed or murdered?

LEB24= Did s/he witness anyone being sexually abused, assaulted or raped?

LEB25= Has s/he seen anyone getting hit, kicked or physically harmed in some other way?

SCORING
0= NO
1= YES;
Data LEB7;
set ocs.LEB0708;
if sum(of LEB20 LEB21 LEB21B LEB22 LEB23 LEB24 LEB25)>=0 then ocs_wit_vio=0;
if sum(of LEB20 LEB21 LEB21B LEB22 LEB23 LEB24 LEB25)>=1 then ocs_wit_vio=1;
if sum(of LEB20 LEB21 LEB21B LEB22 LEB23 LEB24 LEB25)=.  then ocs_wit_vio=.;

label ocs_wit_vio="Witnessed violence";
KEEP center id visit LEB20 LEB21 LEB21B LEB22 LEB23 LEB24 LEB25 ocs_wit_vio;
RUN;
Proc sort data=LEB7;by center id visit; Run;
proc freq data=LEB7;
table visit LEB20 LEB21 LEB21B LEB22 LEB23 LEB24 LEB25 ocs_wit_vio visit*ocs_wit_vio/missing;
run;

*HWVA2= Have you ever seen someone being slapped, kicked, hit with something or beaten up? 

HWVA3= Have you ever seen someone pull a gun on another person?

HWVA4= Have you ever seen someone pull a knife or razon on anyone?

HWVA5= Have you ever seen someone get stabbed or cut with some type of weapon?

HWVA6= Have you ever seen someone get shot?

HWVA7= Have you ever seen someone getting sexually assaulted, molested or raped?

SCORING
0=NO
1= YES

HWVA2B= How often have you seen this happen in the last year? 

SCORING
0 = Never
1= 1 time
2= 2-3 times
3= >4 ;

Data HWVA7;
set ocs.HWVA1002;
if sum(of HWVA2, HWVA3, HWVA4, HWVA5, HWVA6, HWVA7)>=0 | HWVA2B>=0  then ocs_wit_vio=0;
if sum(of HWVA2, HWVA3, HWVA4, HWVA5, HWVA6, HWVA7)>=1 | HWVA2B>=1 then ocs_wit_vio=1;
label ocs_wit_vio="Witnessed violence";

KEEP center id visit HWVA2 HWVA3 HWVA4 HWVA5 HWVA6 HWVA7 HWVA2B ocs_wit_vio;
RUN;
Proc sort data=HWVA7;by center id visit; Run;
proc freq data=HWVA7;
table visit  HWVA2 HWVA3 HWVA4 HWVA5 HWVA6 HWVA7 HWVA2B ocs_wit_vio visit*ocs_wit_vio/missing;
run;



Proc datasets lib=work memtype=data;
run;
quit;


Data OCS;
merge AMPA
DMA
ESIA
HWVA
HWVA7
LEB1
LEB2
LEB3
LEB4
LEB6
LEB7
LECA1
LECA2
LECA4
LECC2
LECC4
PCCT
POM
POM1
RBFA
RBFA5
/*RNAB
RNAB10B*/
SERA
SRUB1
SUA
SXA
WFA
WFA3
WVA;
by center id visit;
if visit<=6 & visit>=0 then time=1;
if visit>6 & visit<=10 then time=2;
if visit> 10 & visit<=14 then time=3;
if visit>14 then time=4;
if time <=3;
run;

proc freq data=ocs;
table time ocs_neglect_beh ocs_harsh_pv time*ocs_neglect_beh time*ocs_harsh_pv/norow nocol nopercent nocum;
run;

Data OCS.ocs;
set ocs;
run;

Data ocs1;
set ocs;
keep center id visit time ocs_:;
run;

proc summary data=ocs;
class center id time;
var ocs_:;
output out=ocs2(drop=_type_ _freq_) max=;
run;

Data ocsbytime;
set ocs2;
if center ne ' ' & id ne ' ' & time ne .;
run;

proc summary data=ocs;
class center id;
var ocs_:;
output out=ocs3(drop=_type_ _freq_) max=;
run;

Data ocsacrosstime;
set ocs3;
if center ne ' ' & id ne ' ';
ocs_cnt=sum(of ocs_:);
if ocs_cnt=0  then ocs=0;
if ocs_cnt>=1 then ocs=1;
label ocs_cnt="# of other childhood stressors and expanded ACEs domains"
      ocs    ="Presence of any other childhood stressor and expanded ACEs";
run;

proc freq data=ocsacrosstime;
table ocs_cnt ocs ocs_:;
run;

Data OCS.ocsbytime;
set ocsbytime;
run;

Data OCS.ocsacrosstime;
set ocsacrosstime;
run;

*merge traditional ACE with OCS;
Data LS_MH_PCE;
set dat.LS_MH_PCE;
if sum(of M, MP, MS, MN, MF, ML, ME, MM, MD)>=1 then anyACE=1;
if sum(of M, MP, MS, MN, MF, ML, ME, MM, MD)= 0 then anyACE=0;
if sum(of M, MP, MS, MN, MF, ML, ME, MM, MD)= . then anyACE=.;
run;

proc sort data=LS_MH_PCE; by id; run;
proc sort data=ocsacrosstime;   by id; run;

Data LS_MH_PCE_OCS;
merge LS_MH_PCE(in=w) ocsacrosstime;
by id;
if w;
label anyACE="Presence of any traditional ACEs";
run;
proc contents data=LS_MH_PCE_OCS order=varnum; run;
Data dat.LS_MH_PCE_OCS;
set LS_MH_PCE_OCS;
run;




/******************************************************************/
/********************frequency tables******************************/
/******************************************************************/

ODS RTF FILE = 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Output/OCS_frequency_11_16_2022.1.rtf' STYLE= minimal;
title 'OTHER CHILDHOOD STRESSORS AND EXPANDED ACES';
Proc freq data=LS_MH_PCE_OCS;
table ocs ocs_cnt anyACE;
run;
Proc freq data=LS_MH_PCE_OCS;
table ocs*anyACE ocs_cnt*anyACE/ norow nocol nopercent nocum;
run;

 
Proc freq data=LS_MH_PCE_OCS;
table ocs_neglect_beh ocs_harsh_pv;
run;

Proc freq data=LS_MH_PCE_OCS;
table ocs_BN_food
ocs_BN_cstressor
ocs_BN_poverty
ocs_BN_homeless;
run;

Proc freq data=LS_MH_PCE_OCS;
table ocs_sep
ocs_trauma_sys
ocs_trauma_court;
run;

Proc freq data=LS_MH_PCE_OCS;
table ocs_RB_house
ocs_RB_peer;
run;

Proc freq data=LS_MH_PCE_OCS;
table ocs_crime_victim
ocs_wit_vio;
run;


proc freq data=AMPA;
table visit AMPA1A	AMPA2A	AMPA3A	AMPA4A	AMPA5A	AMPA6A	AMPA7A	AMPA8A	AMPA9A	AMPA10A	AMPA11A	AMPA12A	AMPA13A	AMPA14A	AMPA15A	AMPA16A	AMPA17A	AMPA18A	AMPA19A	
AMPA20A	AMPA21A	AMPA22A	AMPA23A	AMPA24A	AMPA25A ocs_neglect_beh visit*ocs_neglect_beh/norow nocol nopercent nocum;
run;
proc freq data=DMA;
table visit DMA17-DMA25 ocs_harsh_pv visit*ocs_harsh_pv/norow nocol nopercent nocum;
run;
proc freq data=PCCT;
table visit  PCCT4 PCCT7 PCCT8 PCCT9 PCCT11 PCCT13 PCCT16 PCCT19 PCCT20 PCCT22 ocs_harsh_pv visit*ocs_harsh_pv/norow nocol nopercent nocum;
run;
proc freq data=POM;
table visit  POM9 POM10 POM11 POM12 ocs_BN_food visit*ocs_BN_food/norow nocol nopercent nocum;
run;
proc freq data=esia;
table visit ESIA5 ocs_BN_cstressor visit*ocs_BN_cstressor/norow nocol nopercent nocum;
run;
proc freq data=sera;
table visit SERA1E ocs_BN_poverty visit*ocs_BN_poverty/norow nocol nopercent nocum;
run;
proc freq data=WFA;
table visit WFA1C WFA1D WFA1F ocs_BN_poverty visit*ocs_BN_poverty/norow nocol nopercent nocum;
run;
proc freq data=POM1;
table visit  POM17 POM18 ocs_BN_poverty visit*ocs_BN_poverty/norow nocol nopercent nocum;
run;
proc freq data=LECA1;
table visit  leca10 leca11 ocs_BN_homeless visit*ocs_BN_homeless/norow nocol nopercent nocum;
run;
proc freq data=LEB1;
table visit  LEB10 LEB11 ocs_BN_homeless visit*ocs_BN_homeless/norow nocol nopercent nocum;
run;
proc freq data=SRUB1;
table visit  SRUB8A ocs_BN_homeless visit*ocs_BN_homeless/norow nocol nopercent nocum;
run;
proc freq data=LECA2;
table visit  LECA4 LECA4A LECA5 LECA5A LECA6 LECA13 ocs_sep visit*ocs_sep/norow nocol nopercent nocum;
run;
proc freq data=LEB2;
table visit LEB4 LEB4a LEB5 LEB5a LEB6 LEB13 ocs_sep visit*ocs_sep/norow nocol nopercent nocum;
run;
proc freq data=LECC2;
table visit  LECC4 LECC5 LECC6 ocs_sep visit*ocs_sep/norow nocol nopercent nocum;
run;
proc freq data=LEB3;
table visit LEB18a LEB19a ocs_trauma_sys visit*ocs_trauma_sys/norow nocol nopercent nocum;
run;
proc freq data=WFA3;
table visit WFA1b ocs_trauma_sys visit*ocs_trauma_sys/norow nocol nopercent nocum;
run;
proc freq data=HWVA;
table visit  HWVA1 HWVA1A HWVA1B1--HWVA1B5 HWVA1C HWVA1D1--HWVA1D5 ocs_trauma_sys visit*ocs_trauma_sys/norow nocol nopercent nocum;
run;
proc freq data=LECA4;
table visit  LECA17A LECA17B ocs_trauma_court visit*ocs_trauma_court/norow nocol nopercent nocum;
run;
proc freq data=LEB4;
table visit LEB17a LEB17b ocs_trauma_court visit*ocs_trauma_court/missing;
run;
proc freq data=LECC4;
table visit  LECC18 LECC18A LECC18B ocs_trauma_court visit*ocs_trauma_court/norow nocol nopercent nocum;
run;
proc freq data=WVA;
table visit WVA22 ocs_RB_house visit*ocs_RB_house/norow nocol nopercent nocum;
run;
proc freq data=SUA;
table visit SUA9A ocs_RB_house visit*ocs_RB_house/norow nocol nopercent nocum;
run;
proc freq data=RBFA;
table visit RBFA5 RBFA6 RBFA7 RBFA9A ocs_RB_house visit*ocs_RB_house/norow nocol nopercent nocum;
run;
proc freq data=SXA;
table visit SXA9A SXA9B SXA9C ocs_RB_peer visit*ocs_RB_peer/norow nocol nopercent nocum;
run;
proc freq data=RBFA5;
table visit rbfa15 rbfa16 rbfa18 rbfa19 rbfa20 rbfa21 rbfa22 rbfa23 ocs_RB_peer visit*ocs_RB_peer/norow nocol nopercent nocum;
run;
proc freq data=LEB6;
table visit LEB16 ocs_crime_victim visit*ocs_crime_victim/norow nocol nopercent nocum;
run;
proc freq data=LEB7;
table visit LEB20 LEB21 LEB21B LEB22 LEB23 LEB24 LEB25 ocs_wit_vio visit*ocs_wit_vio/norow nocol nopercent nocum;
run;
proc freq data=HWVA7;
table visit  HWVA2 HWVA3 HWVA4 HWVA5 HWVA6 HWVA7 HWVA2B ocs_wit_vio visit*ocs_wit_vio/norow nocol nopercent nocum;
run;
/*
proc freq data=rnab;
table visit RNA10D RNA10F RNA10G RNA10H RNA10J RNA10K ocs_neglect_beh visit*ocs_neglect_beh/norow nocol nopercent nocum;
run;
proc freq data=rnab10B;
table visit RNA10B ocs_harsh_pv visit*ocs_harsh_pv/norow nocol nopercent nocum;
run;
*/
title;
ODS RTF CLOSE;
