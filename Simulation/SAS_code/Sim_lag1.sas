/* because Excel field names often have spaces */
options validvarname=any;

options nonotes;
ods graphics off;
ods exclude all;
ods noresults; 

%macro sim(N=256); 
%do i=1 %to &N;

libname VC XLSX "I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Data_lag1_&i..xlsx";

proc sort data=VC.SHEET1 out=SortedFile;
by SampleID;
run;

*Poisson;
proc glimmix data=SORTEDFILE method=rspl maxlmmupdate=100;
by SampleID;
class IDLEVEL2;
model Y = Time Trt1 Trt2 T1_Trt1 T2_Trt2/ link=log dist=poisson s cl ddfm=kenrog;
random int Trt1 Trt2/type=vc sub=IDLEVEL2;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_Poisson
           covparms=Random_effects_Poisson  
           FitStatistics=Fit_Poisson
           ConvergenceStatus=Convergence_Poisson;
run;

*Poisson Lag1AR1;
proc glimmix data=SORTEDFILE method=rspl maxlmmupdate=100;
by SampleID;
class IDLEVEL2;
model y = Time Trt1 Trt2 T1_Trt1 T2_Trt2 Trt1Lag1 Trt2Lag1/ link=log dist=poisson s cl ddfm=kenrog;
random int Trt1 Trt2/type=vc sub=IDLEVEL2;
random _residual_ / type=ar(1) sub=IDLEVEL2;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_Lag1AR1
           covparms=Random_effects_Lag1AR1  
           FitStatistics=Fit_Lag1AR1
           ConvergenceStatus=Convergence_Lag1AR1;
run;

*Poisson Lag1;
proc glimmix data=SORTEDFILE method=rspl maxlmmupdate=100;
by SampleID;
class IDLEVEL2;
model y = Time Trt1 Trt2 T1_Trt1 T2_Trt2 Trt1Lag1 Trt2Lag1/ link=log dist=poisson s cl ddfm=kenrog;
random int Trt1 Trt2/type=vc sub=IDLEVEL2;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_Lag1
           covparms=Random_effects_Lag1
           FitStatistics=Fit_Lag1
           ConvergenceStatus=Convergence_Lag1;
run;


proc export data=FIXED_EFFECTS_POISSON 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Fixed_effects_Poisson_&i..xlsx" 
replace;
run;
proc export data=RANDOM_EFFECTS_POISSON 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Random_effects_Poisson_&i..xlsx" 
replace;
run;
proc export data=FIT_POISSON 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Fit_Poisson_&i..xlsx" 
replace;
run;
proc export data=CONVERGENCE_POISSON 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Convergence_Poisson_&i..xlsx" 
replace;
run;

proc export data=FIXED_EFFECTS_Lag1AR1 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Fixed_effects_Lag1AR1_&i..xlsx" 
replace;
run;
proc export data=RANDOM_EFFECTS_Lag1AR1 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Random_effects_Lag1AR1_&i..xlsx" 
replace;
run;
proc export data=FIT_Lag1AR1  
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Fit_Lag1AR1_&i..xlsx" 
replace;
run;
proc export data=CONVERGENCE_Lag1AR1 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Convergence_Lag1AR1_&i..xlsx" 
replace;
run;

proc export data=FIXED_EFFECTS_Lag1 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Fixed_effects_Lag1_&i..xlsx" 
replace;
run;
proc export data=RANDOM_EFFECTS_Lag1 
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Random_effects_Lag1_&i..xlsx" 
replace;
run;
proc export data=FIT_Lag1  
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Fit_Lag1_&i..xlsx" 
replace;
run;
proc export data=CONVERGENCE_Lag1
dbms=xlsx outfile="I:/My Drive/Research project/SCED ATD/ATD/Simulation/ATD1/n1=30, n2=4/SAS/Sim_lag1/Convergence_Lag1_&i..xlsx" 
replace;
run;

%end;
%mend sim;
%sim(N=256);

ods graphics on;
ods exclude none;
ods results;
options notes;
