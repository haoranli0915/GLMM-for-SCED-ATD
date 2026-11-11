/*Example 1 - Andersen's data*/

/*import data*/
proc import datafile= '/home/haoranli1/Andersen-Combined.csv'
dbms=csv replace out=work.Data;
run; 


*Poisson;
proc glimmix data=Data method=rspl maxlmmupdate=100;
class IDLEVEL2;
model y = Time Treatment1 Treatment2 Time1_Treatment1 Time2_Treatment2/ link=log dist=poisson s cl ddfm=kenrog;
random int Time Treatment1 Treatment2 Time1_Treatment1 Time2_Treatment2/type=vc sub=IDLEVEL2 solution;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_Poisson
covparms=Random_effects_Poisson  
FitStatistics=Fit_Poisson
ConvergenceStatus=Convergence_Poisson
SolutionR=Rand_Poisson;

/*Summary of the results (Poisson model)*/
/* 2) Make the random effects wide: one row per subject, one column per effect */
  proc sort data=Rand_Poisson; by Subject Effect; run;
  proc transpose data=Rand_Poisson out=RandWide_Poisson prefix=b_;
  by Subject;
  id Effect;                 /* creates columns like b_Intercept b_Treatment1 ... */
    var Estimate;
  run;  
  
  /* 3) Make the fixed effects wide: one row with all β columns */
  proc transpose data=Fixed_effects_Poisson out=FixWide_Poisson prefix=Beta_;
  id Effect;                 /* creates Beta_Intercept Beta_Treatment1 ... */
    var Estimate;
  run;
  
  /* Build a clean table of fixed effects with 95% CIs and exp() */
    data Fixed_CI_exp_Poisson;
  set Fixed_effects_Poisson;   /* from your ODS OUTPUT ParameterEstimates=... */
    /* Normalize CI col names across SAS versions */
    LCL = coalescec(Lower);
    UCL = coalescec(Upper);
    
    /* Exponentiated versions  */
      Exp_Est = exp(Estimate);
      Exp_LCL = exp(LCL);
      Exp_UCL = exp(UCL);
      
      keep Effect Estimate LCL UCL Exp_Est Exp_LCL Exp_UCL;
      run;
      
      /* Inspect */
        proc print data=Fixed_CI_exp_Poisson(obs=10); run;
        
        
        /* 4) Combine: per-person coefficients = β + b_i */
          data PersonCoefs_Poisson;
        if _n_=1 then set FixWide_Poisson;               /* broadcast fixed effects to all rows */
          set RandWide_Poisson(rename=(Subject=IDLEVEL2)); /* Subject may be character or numeric */
          /*   Add defensively in case any column is missing */
          Beta_Intercept_i = coalesce(Beta_Intercept,0) + coalesce(b_Intercept,0);
          Beta_Time_i = coalesce(Beta_Time,0) + coalesce(b_Time,0);
          Beta_Treatment1_i      = coalesce(Beta_Treatment1,0)      + coalesce(b_Treatment1,0);
          Beta_Treatment2_i      = coalesce(Beta_Treatment2,0)      + coalesce(b_Treatment2,0);
          Beta_Time1_Treatment1_i   = coalesce(Beta_Time1_Treatment1,0)   + coalesce(b_Time1_Treatment1,0);
          Beta_Time2_Treatment2_i   = coalesce(Beta_Time2_Treatment2,0)   + coalesce(b_Time2_Treatment2,0);
          keep IDLEVEL2 Beta_:;
          
          /*   exponentiated  */
            Exp_Intercept_i = exp(Beta_Intercept_i);
            Exp_Time_i = exp(Beta_Time_i);
            Exp_Treatment1_i      = exp(Beta_Treatment1_i);
            Exp_Treatment2_i      = exp(Beta_Treatment2_i);
            Exp_Time1_Treatment1_i   = exp(Beta_Time1_Treatment1_i);
            Exp_Time2_Treatment2_i   = exp(Beta_Time2_Treatment2_i);
            
            /*   keep ONLY the per-person results (no fixed-only β columns) */
              keep IDLEVEL2
            Beta_Intercept_i Beta_Time_i Beta_Treatment1_i Beta_Treatment2_i Beta_Time1_Treatment1_i Beta_Time2_Treatment2_i
            Exp_Intercept_i  Exp_Time_i Exp_Treatment1_i  Exp_Treatment2_i  Exp_Time1_Treatment1_i  Exp_Time2_Treatment2_i;  
            
            run;
            
            /* 5) Inspect */
  proc print data=PersonCoefs_Poisson(obs=10);
  run;
  

*Poisson Lag1;
proc glimmix data=Data method=rspl maxlmmupdate=100;
class IDLEVEL2;
model y = Time Treatment1 Treatment2 Time1_Treatment1 Time2_Treatment2 Treatment1_Lag1 Treatment2_Lag1/ link=log dist=poisson s cl ddfm=kenrog;
random int Time Treatment1 Treatment2 Time1_Treatment1 Time2_Treatment2/type=vc sub=IDLEVEL2 solution;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_PoissonLag1
covparms=Random_effects_PoissonLag1  
FitStatistics=Fit_PoissonLag1
ConvergenceStatus=Convergence_PoissonLag1
SolutionR=Rand_PoissonLag1;

/*Summary of the results (PoissonLag1 model)*/
/* 2) Make the random effects wide: one row per subject, one column per effect */
  proc sort data=Rand_PoissonLag1; by Subject Effect; run;
  proc transpose data=Rand_PoissonLag1 out=RandWide_PoissonLag1 prefix=b_;
  by Subject;
  id Effect;                 /* creates columns like b_Intercept b_Treatment1 ... */
    var Estimate;
  run;  
  
  /* 3) Make the fixed effects wide: one row with all β columns */
  proc transpose data=Fixed_effects_PoissonLag1 out=FixWide_PoissonLag1 prefix=Beta_;
  id Effect;                 /* creates Beta_Intercept Beta_Treatment1 ... */
    var Estimate;
  run;
  
  /* Build a clean table of fixed effects with 95% CIs and exp() */
    data Fixed_CI_exp_PoissonLag1;
  set Fixed_effects_PoissonLag1;   /* from your ODS OUTPUT ParameterEstimates=... */
    /* Normalize CI col names across SAS versions */
    LCL = coalescec(Lower);
    UCL = coalescec(Upper);
    
    /* Exponentiated versions  */
      Exp_Est = exp(Estimate);
      Exp_LCL = exp(LCL);
      Exp_UCL = exp(UCL);
      
      keep Effect Estimate LCL UCL Exp_Est Exp_LCL Exp_UCL;
      run;
      
      /* Inspect */
        proc print data=Fixed_CI_exp_PoissonLag1(obs=10); run;
        
        
        /* 4) Combine: per-person coefficients = β + b_i */
          data PersonCoefs_PoissonLag1;
        if _n_=1 then set FixWide_PoissonLag1;               /* broadcast fixed effects to all rows */
          set RandWide_PoissonLag1(rename=(Subject=IDLEVEL2)); /* Subject may be character or numeric */
          /*   Add defensively in case any column is missing */
          Beta_Intercept_i = coalesce(Beta_Intercept,0) + coalesce(b_Intercept,0);
          Beta_Time_i = coalesce(Beta_Time,0) + coalesce(b_Time,0);
          Beta_Treatment1_i      = coalesce(Beta_Treatment1,0)      + coalesce(b_Treatment1,0);
          Beta_Treatment2_i      = coalesce(Beta_Treatment2,0)      + coalesce(b_Treatment2,0);
          Beta_Time1_Treatment1_i   = coalesce(Beta_Time1_Treatment1,0)   + coalesce(b_Time1_Treatment1,0);
          Beta_Time2_Treatment2_i   = coalesce(Beta_Time2_Treatment2,0)   + coalesce(b_Time2_Treatment2,0);
          keep IDLEVEL2 Beta_:;
          
          /*   exponentiated  */
            Exp_Intercept_i = exp(Beta_Intercept_i);
            Exp_Time_i = exp(Beta_Time_i);
            Exp_Treatment1_i      = exp(Beta_Treatment1_i);
            Exp_Treatment2_i      = exp(Beta_Treatment2_i);
            Exp_Time1_Treatment1_i   = exp(Beta_Time1_Treatment1_i);
            Exp_Time2_Treatment2_i   = exp(Beta_Time2_Treatment2_i);
            
            /*   keep ONLY the per-person results (no fixed-only β columns) */
              keep IDLEVEL2
            Beta_Intercept_i Beta_Time_i Beta_Treatment1_i Beta_Treatment2_i Beta_Time1_Treatment1_i Beta_Time2_Treatment2_i
            Exp_Intercept_i  Exp_Time_i Exp_Treatment1_i  Exp_Treatment2_i  Exp_Time1_Treatment1_i  Exp_Time2_Treatment2_i;  
            
            run;
            
            /* 5) Inspect */
  proc print data=PersonCoefs_PoissonLag1(obs=10);
  run;

    
*Lag1AR1;
proc glimmix data=Data method=rspl maxlmmupdate=100;
class IDLEVEL2;
model y = Time Treatment1 Treatment2 Time1_Treatment1 Time2_Treatment2 Treatment1_Lag1 Treatment2_Lag1/ link=log dist=poisson s cl ddfm=kenrog;
random int Time Treatment1 Treatment2 Time1_Treatment1 Time2_Treatment2/type=vc sub=IDLEVEL2 solution;
random _residual_ / type=ar(1) sub=IDLEVEL2;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_Lag1AR1
covparms=Random_effects_Lag1AR1  
FitStatistics=Fit_Lag1AR1
ConvergenceStatus=Convergence_Lag1AR1
SolutionR=Rand_Lag1AR1;

/*Summary of the results (Lag1AR1 model)*/
/* 2) Make the random effects wide: one row per subject, one column per effect */
  proc sort data=Rand_Lag1AR1; by Subject Effect; run;
  proc transpose data=Rand_Lag1AR1 out=RandWide_Lag1AR1 prefix=b_;
  by Subject;
  id Effect;                 /* creates columns like b_Intercept b_Treatment1 ... */
    var Estimate;
  run;  
  
  /* 3) Make the fixed effects wide: one row with all β columns */
  proc transpose data=Fixed_effects_Lag1AR1 out=FixWide_Lag1AR1 prefix=Beta_;
  id Effect;                 /* creates Beta_Intercept Beta_Treatment1 ... */
    var Estimate;
  run;
  
  /* Build a clean table of fixed effects with 95% CIs and exp() */
    data Fixed_CI_exp_Lag1AR1;
  set Fixed_effects_Lag1AR1;   /* from your ODS OUTPUT ParameterEstimates=... */
    /* Normalize CI col names across SAS versions */
    LCL = coalescec(Lower);
    UCL = coalescec(Upper);
    
    /* Exponentiated versions  */
      Exp_Est = exp(Estimate);
      Exp_LCL = exp(LCL);
      Exp_UCL = exp(UCL);
      
      keep Effect Estimate LCL UCL Exp_Est Exp_LCL Exp_UCL;
      run;
      
      /* Inspect */
        proc print data=Fixed_CI_exp_Lag1AR1(obs=10); run;
        
        
        /* 4) Combine: per-person coefficients = β + b_i */
          data PersonCoefs_Lag1AR1;
        if _n_=1 then set FixWide_Lag1AR1;               /* broadcast fixed effects to all rows */
          set RandWide_Lag1AR1(rename=(Subject=IDLEVEL2)); /* Subject may be character or numeric */
          /*   Add defensively in case any column is missing */
          Beta_Intercept_i = coalesce(Beta_Intercept,0) + coalesce(b_Intercept,0);
          Beta_Time_i = coalesce(Beta_Time,0) + coalesce(b_Time,0);
          Beta_Treatment1_i      = coalesce(Beta_Treatment1,0)      + coalesce(b_Treatment1,0);
          Beta_Treatment2_i      = coalesce(Beta_Treatment2,0)      + coalesce(b_Treatment2,0);
          Beta_Time1_Treatment1_i   = coalesce(Beta_Time1_Treatment1,0)   + coalesce(b_Time1_Treatment1,0);
          Beta_Time2_Treatment2_i   = coalesce(Beta_Time2_Treatment2,0)   + coalesce(b_Time2_Treatment2,0);
          keep IDLEVEL2 Beta_:;
          
          /*   exponentiated  */
            Exp_Intercept_i = exp(Beta_Intercept_i);
            Exp_Time_i = exp(Beta_Time_i);
            Exp_Treatment1_i      = exp(Beta_Treatment1_i);
            Exp_Treatment2_i      = exp(Beta_Treatment2_i);
            Exp_Time1_Treatment1_i   = exp(Beta_Time1_Treatment1_i);
            Exp_Time2_Treatment2_i   = exp(Beta_Time2_Treatment2_i);
            
            /*   keep ONLY the per-person results (no fixed-only β columns) */
              keep IDLEVEL2
            Beta_Intercept_i Beta_Time_i Beta_Treatment1_i Beta_Treatment2_i Beta_Time1_Treatment1_i Beta_Time2_Treatment2_i
            Exp_Intercept_i  Exp_Time_i Exp_Treatment1_i  Exp_Treatment2_i  Exp_Time1_Treatment1_i  Exp_Time2_Treatment2_i;  
            
            run;
            
            /* 5) Inspect */
  proc print data=PersonCoefs_Lag1AR1(obs=10);
  run;
    


/**********************Model when treatment 1 is the reference group***********************/
*Lag1AR1;
proc glimmix data=Data method=rspl maxlmmupdate=100;
class IDLEVEL2;
model y = Time1 Control Treatment2 Time_Control Time2_Treatment2 Treatment1_Lag1 Treatment2_Lag1/ link=log dist=poisson s cl ddfm=kenrog;
random int Time1 Control Treatment2 Time_Control Time2_Treatment2/type=vc sub=IDLEVEL2 solution;
random _residual_ / type=ar(1) sub=IDLEVEL2;
*nloptions tech=NRRIDG;
ods output ParameterEstimates=Fixed_effects_Lag1AR1
           covparms=Random_effects_Lag1AR1  
           FitStatistics=Fit_Lag1AR1
           ConvergenceStatus=Convergence_Lag1AR1
           SolutionR=Rand_Lag1AR1;

/*Summary of the results*/
/* 2) Make the random effects wide: one row per subject, one column per effect */
proc sort data=Rand_Lag1AR1; by Subject Effect; run;
proc transpose data=Rand_Lag1AR1 out=RandWide_Lag1AR1 prefix=b_;
  by Subject;
  id Effect;                 /* creates columns like b_Intercept b_Trt1 ... */
  var Estimate;
run;  
         
/* 3) Make the fixed effects wide: one row with all β columns */
proc transpose data=Fixed_effects_Lag1AR1 out=FixWide_Lag1AR1 prefix=Beta_;
  id Effect;                 /* creates Beta_Intercept Beta_Trt1 ... */
  var Estimate;
run;

/* Build a clean table of fixed effects with 95% CIs and exp() */
data Fixed_CI_exp_Lag1AR1;
  set Fixed_effects_Lag1AR1;   /* from your ODS OUTPUT ParameterEstimates=... */
  /* Normalize CI col names across SAS versions */
  LCL = coalescec(Lower);
  UCL = coalescec(Upper);

  /* Exponentiated versions (rate ratios for log link; odds ratios for logit) */
  Exp_Est = exp(Estimate);
  Exp_LCL = exp(LCL);
  Exp_UCL = exp(UCL);

  keep Effect Estimate LCL UCL Exp_Est Exp_LCL Exp_UCL;
run;

/* Inspect */
proc print data=Fixed_CI_exp_Lag1AR1(obs=10); run;


/* 4) Combine: per-person coefficients = β + b_i */
data PersonCoefs_Lag1AR1;
  if _n_=1 then set FixWide_Lag1AR1;              /* broadcast fixed effects to all rows */
  set RandWide_Lag1AR1(rename=(Subject=IDLEVEL2)); /* Subject may be character or numeric*/
  
/*   Add defensively in case any column is missing */
  Beta_Intercept_i = coalesce(Beta_Intercept,0) + coalesce(b_Intercept,0);
  Beta_Time1_i = coalesce(Beta_Time1,0) + coalesce(b_Time1,0);
  Beta_Control_i      = coalesce(Beta_Control,0)      + coalesce(b_Control,0);
  Beta_Treatment2_i      = coalesce(Beta_Treatment2,0)      + coalesce(b_Treatment2,0);
  Beta_Time_Control_i   = coalesce(Beta_Time_Control,0)   + coalesce(b_Time_Control,0);
  Beta_Time2_Treatment2_i   = coalesce(Beta_Time2_Treatment2,0)   + coalesce(b_Time2_Treatment2,0);
  keep IDLEVEL2 Beta_:;

/*   exponentiated */
  Exp_Intercept_i = exp(Beta_Intercept_i);
  Exp_Time1_i = exp(Beta_Time1_i);
  Exp_Control_i      = exp(Beta_Control_i);
  Exp_Treatment2_i      = exp(Beta_Treatment2_i);
  Exp_Time_Control_i   = exp(Beta_Time_Control_i);
  Exp_Time2_Treatment2_i   = exp(Beta_Time2_Treatment2_i);

/*   keep ONLY the per-person results (no fixed-only β columns) */
  keep IDLEVEL2
       Beta_Intercept_i Beta_Time1_i Beta_Control_i Beta_Treatment2_i Beta_Time_Control_i Beta_Time2_Treatment2_i
       Exp_Intercept_i  Exp_Time1_i Exp_Control_i  Exp_Treatment2_i  Exp_Time_Control_i  Exp_Time2_Treatment2_i;  
  
run;

/* 5) Inspect */
proc print data=PersonCoefs_Lag1AR1(obs=10);
run;






