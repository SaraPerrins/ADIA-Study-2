options nofmterr;

libname dat  'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data';
libname MH	 'C:\Users\21983\OneDrive - ICF\ADIA\study 2\Data\MH';

Proc sort data=dat.ahsa1201; by center ID visit; run;
Proc sort data=dat.hrba1201; by center ID visit; run;
Proc sort data=MH.mddb1201; by center ID visit; run;
Proc sort data=MH.mddm1002; by center ID visit; run;
Proc sort data=MH.mdds1201; by center ID visit; run;
Proc sort data=dat.childace_l; by center ID visit; run;
proc contents data=dat.childace_l order=varnum; run;
proc freq data=dat.childace_l;
table visit * M: /norow nocol nopercent nocum;
run;
proc contents data=dat.child_ace order=varnum; run;

Data dat.childace_l;
set dat.childace_l;
label M='MMCS allegation'
	MP='phy mtx'
	MS='sexual mtx'
	MN='neglect'
	MF='neglect-ftp'
	ML='neglect-los'
	ME='emotional mtx'
	MM='moral mtx'
	MD='edu mtx'
	MA='alcohol/drug' ;
run;

proc summary data=dat.childace_l;
class ID;
var M MP MS MN MF ML ME MM MD;
output out=c_ace(drop=_type_ _freq_) sum= ;
run;
Data c_ace;
set c_ace;
if ID = '' then delete;
array var M MP MS MN MF ML ME MM MD;
do i = 1 to 9;
if var[i]=0 then var[i]=0;
if var[i]>0 then var[i]=1;
end; 
run;
proc freq data=c_ace;
table M:;
run;

Data sud;
length visit 8;
merge dat.ahsa1201 (in=a) dat.hrba1201 (in=b) MH.mddb1201 (in=c) MH.mddm1002 (in=d) MH.mdds1201 (in=e) ;
by center ID visit;
AHSA=a;
HRBA=b;
MDDB=c;
MDDM=d;
MDDS=e;
*if visit=18;
run;

Proc freq data=sud;
table visit* AHSA visit*HRBA visit*MDDB visit*MDDM visit*MDDS /norow nocol nopercent nocum;
run;

Data SUD1;
merge c_ace sud;
by ID;
run;

proc freq data=SUD1;
table visit M MP MS MN MF ML ME MM MD AHSA23
AHSA24
AHSA25
AHSA26
HRBA17
HRBA18
HRBA19
HRBA20
MDDB20
MDDB21
MDDB22
YMD020
YMD021
YMD022B
YMD020A
YMD020C
YMD021
YMD021E
YMD021F
YMD022
YMD022A
YMD022B
YMD022C
; run;



Data SUD2;
set SUD1;
if 	AHSA23	in (0 1)|
	AHSA24	in (0 1)|
	AHSA25	>=	1	|
	AHSA26	in (0 1)|
	HRBA17	in (0 1)|
	HRBA18	in (0 1)|
	HRBA19	>=	1	|
	HRBA20	in (0 1)|
	MDDB20	in (0 2)|
	MDDB21	in (0 2)|
	MDDB22	in (0 2)|
	YMD020	in (0 2)|
	YMD021	in (0 2)|
	YMD022B	in (0 2)|
	YMD020A	in (0 2)|
	YMD020C	in (0 2)|
	YMD021	in (0 2)|
	YMD021E	in (0 2)|
	YMD021F	in (0 2)|
	YMD022	in (0 2)|
	YMD022A	>=	1	|
	YMD022B	in (0 2)|
	YMD022C	>=	1 then anysud=0;	
if 	AHSA23	=1 |
	AHSA24	=1 |
	AHSA25	>=2 |
	AHSA26	=1 |
	HRBA17	=1 |
	HRBA18	=1 |
	HRBA19	>=1 |
	HRBA20	=1 |
	MDDB20	=2 |
	MDDB21	=2 |
	MDDB22	=2 |
	YMD020	=2 |
	YMD021	=2 |
	YMD022B	=2 |
	YMD020A	=2 |
	YMD020C	=2 |
	YMD021	=2 |
	YMD021E	=2 |
	YMD021F	=2 |
	YMD022	=2 |
	YMD022A	>=1 |
	YMD022B	=2 |
	YMD022C	>=1	then anysud=1;	
if visit >= 16;
run;

proc freq data=SUD2;
table visit anysud visit*anysud;
run;

Data dat.SUD16_18;
set SUD2;
run;

*sud indicator across age 16 & age18;
proc summary data=SUD2;
class center id;
var anysud;
output out=SUDacrosstime(drop=_type_ _freq_) max=;
run;
Data SUDacrosstime;
set SUDacrosstime;
if center ne ' ' & id ne ' ';
run;

proc freq data=SUDacrosstime;
table anysud; run;

Data dat.SUDacrosstime;
set SUDacrosstime;
run;

Data SUDacrosstime;
set dat.SUDacrosstime;
run;

*------------------------------------------------------------------------------------*;
*------------------------------Mental Health Outcome---------------------------------*;
*------------------------------------------------------------------------------------*;

****Major depression disorder;
Data mdds1201;
set MH.mdds1201;
run;
Data mddm1002;
set MH.mddm1002;
run;
Data mddb1201;
set MH.mddb1201;
run;
proc contents data=mdds1201;run;
proc contents data=mddm1002;run;
proc contents data=mddb1201;run;

proc means data=mdds1201;
class visit;
var YDYY YMDM YMDY YMDSYMP;
run;

Proc freq data=mdds1201;
table visit*ydyy visit*ymdy /norow nocol nopercent nocum;
run;



***other Mental Health disorder outcome;
*DISC-Generalized Anxiety Disorder-Youth:Scores;
Data gads1201;
set MH.gads1201;
Run;
Proc contents data=gads1201;Run;
Data gads;
set gads1201;
keep center id visit YGASYMP YGAY;
Run;
*DISC-Post-Traumatic Stress Disorder-Youth:Score;
Data ptss1201;
set MH.ptss1201;
Run;
Proc contents data=ptss1201;Run;
Data ptss;
set ptss1201;
keep center id visit YPTSYMP YPTY;
Run;
*DISC-Agoraphobia: scores;
Data agrs1201;
set MH.agrs1201;
Run;
Proc contents data=agrs1201;Run;
Data agrs;
set agrs1201;
keep center id visit  yagm yagy yagsymp;
Run;
*DISC-Social Phobia: Scores;
Data sops1201;
set MH.sops1201;
Run;
Proc contents data=sops1201;Run;
Data sops;
set sops1201;
keep center id visit ysom ysoy ysosymp ;
Run;
/*DISC-Specific Phobia: Scores--age 14 ;
Data spss1002;
set MH.spss1002;
Run;
Proc contents data=spss1002;Run;
Data spss;
set spss1002;
keep center id visit sps_:;
Run;
Proc means data=spss;
class visit;
var sps_:;
run;*/
*DISC-Panic Disorder- Youth: Scores;
Data pans1201;
set MH.pans1201;
Run;
Proc contents data=pans1201;Run;
Data pans;
set pans1201;
keep center id visit ypanagy ypasymp ypawagy ypay;
Run;
*DISC-Attention Deficit/Hyperactivity-Youth Scores;
Data adhs1201;
set MH.adhs1201;
Run;
Proc contents data=adhs1201; Run;
Data adhs;
set adhs1201;
keep center id visit yadsymp yahsymp yatsymp yady;
Run;
*DISC-Conduct Disorder- Youth: Scores;
Data cdds1201;
set MH.cdds1201;
Run;
Proc contents data=cdds1201;Run;
Data cdds;
set cdds1201;
if sum(of yasy ycdy)>=1 then ycd=1; else ycd=0;
if sum(of yasy ycdy)=. then ycd=.;
keep center id visit yasy ycdasy ycdy ycd;
Run;
*DISC-Obsessive Compulsive Disorder Y:Scores;
Data ocds1201;
set MH.ocds1201;
Run;
Proc contents data=ocds1201; Run;
Data ocds;
set ocds1201;
keep center id visit yocsymp yoccy yocy;
Run;
**DISC-Oppositional Defiant Disorder-Y:Score;
Data odds1201;
set MH.odds1201;
Run;
Proc contents data=odds1201;Run;
Data odds;
set odds1201;
keep center id visit yodsymp YODY;
Run;
*Mania/hypoMania;
Data MANS1201;
set MH.MANS1201;
run;
proc contents data=MANS1201; run;
Data MANS;
Set MANS1201;
keep center id visit YMASYMP YMAY;
run;
*Schizophrenia;
Data SCZS1201;
set MH.SCZS1201;
Run;
proc contents data=SCZS1201; run;
Data SCZS;
set SCZS1201;
keep center id visit  YSZSYMP;
run;
proc freq data=sczs;
table visit*YSZSYMP;
run;

proc sort data=gads; by center id visit; run;
Proc sort data=ptss; by center id visit; run;
proc sort data=agrs; by center id visit; run;
proc sort data=sops; by center id visit; run;
proc sort data=pans; by center id visit; run;
proc sort data=adhs; by center id visit; run;
proc sort data=cdds; by center id visit; run;
proc sort data=ocds; by center id visit; run;
proc sort data=odds; by center id visit; run;
proc sort data=mans; by center id visit; run;
proc sort data=sczs; by center id visit; run;

Data MH;
Merge gads ptss agrs sops pans adhs cdds ocds odds mans sczs;
by center id visit;
run;
proc means data=mh;
class visit;
var
YGASYMP
YPTSYMP
YAGSYMP
YSOSYMP
YPASYMP
YADSYMP
YASY
YCDY
YOCSYMP
YODSYMP
YMASYMP
YSZSYMP;
run;

Data MH;
set MH;
if sum(of YGAY
YPTY
YAGY
YSOY
YPAY
YADY
ycd
YOCY
YODY
YMAY
YSZSYMP)>=1 then anyMH=1;
if sum(of YGAY
YPTY
YAGY
YSOY
YPAY
YADY
ycd
YOCY
YODY
YMAY
YSZSYMP)=0 then anyMH=0;
if sum(of YGAY
YPTY
YAGY
YSOY
YPAY
YADY
ycd
YOCY
YODY
YMAY
YSZSYMP)=. then anyMH=.;
/*if sum(of YGASYMP
YPTSYMP
YAGSYMP
YSOSYMP
YPASYMP
YADSYMP
YASY
YCDY
YOCSYMP
YODSYMP
YMASYMP
YSZSYMP)>0 then anyMH=1;
if sum(of YGASYMP
YPTSYMP
YAGSYMP
YSOSYMP
YPASYMP
YADSYMP
YASY
YCDY
YOCSYMP
YODSYMP
YMASYMP
YSZSYMP)=0 then anyMH=0;
if sum(of YGASYMP
YPTSYMP
YAGSYMP
YSOSYMP
YPASYMP
YADSYMP
YASY
YCDY
YOCSYMP
YODSYMP
YMASYMP
YSZSYMP)=. then anyMH=.;*/
run;

proc freq data=MH;
table center visit anyMH;
run;

Data dat.MH18;
set MH;
run;

Data MH;
set dat.MH18;
run;
proc contents data=MH;run;

*------------------------------------------------------------------------------------*;
*------------------------------Depression Outcome---------------------------------*;
*------------------------------------------------------------------------------------*;


*update data LS_MH_PCE_anyt, Add depression outcome from mdds1201 all age18;
proc contents data=MH.mdds1201;run;
proc contents data=MH.cess1201;run;

Data mdds;
set MH.mdds1201;
keep center id  YDYY YMDM YMDY YMDSYMP;*depression outcome, set YMDSYMP>=7 as cut off for matching;
run;

Data cess(rename=(DEPBTOT=DEPBTOT16));
set MH.cess1201; /*cess1201 have age 12, 14 & 16*/
if visit=16;
keep center id  DEPBTOT;
run;

proc sort data=mdds; by center id; run;
proc sort data=cess; by center id; run;
Data dep;
merge mdds cess;
by center id;
run;

Data dat.dep16_18;
set dep;
run;

Data dep;
set dat.dep16_18;
run;
