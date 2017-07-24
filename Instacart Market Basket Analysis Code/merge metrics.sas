proc import datafile="C:/Users/chicheng3/Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Processed Data/data_pre_model.csv"
out = data dbms=csv replace;
getnames=yes;
run;

proc contents data=data;
run;

proc import datafile="C:/Users/chicheng3/Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Processed Data/top_aisle_depart_hod_dow_metrics.csv"
out = aisle_depart_hod_dow dbms=csv replace;
getnames=yes;
run;

proc contents data=aisle_depart_hod_dow;
run;

proc print data=aisle_depart_hod_dow(obs=100);
run;

proc import datafile="C:/Users/chicheng3/Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Processed Data/stability_metrics.csv"
out = stability dbms=csv replace;
getnames=yes;
run;

proc contents data=stability;
run;

data all;
merge data(in=a) aisle_depart_hod_dow(in=b) stability(in=c);
by user_id product_id;
if a and b and c;
run;

proc contents data=all;
run;

proc print data=all(obs=100);
run;

proc export data=all outfile="C:/Users/chicheng3/Desktop/Kaggle & Coursera/Kaggle Projects/Instacart Market Basket Analysis/Processed Data/data_pre_model_0723.csv"
dbms=csv replace;
run;
