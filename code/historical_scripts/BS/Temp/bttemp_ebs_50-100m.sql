/* THIS SCRIPT CALCULATES AVERAGE BOTTOM AND SURFACE TEMPERATURE, WEIGHTED BY */
/* STRATA AREA, FOR THE STANDARD EBS SHELF WHERE STRATA <= 43 (BOTTOM DEPTH < 50M) */

REM MODIFIED 9/12/2022 updated racebase.stratum year = 2022, haehnr

/*  THIS SECTION GETS THE PROPER HAUL TABLE */
REM ***THIS GRABS THE MOST UP-TO-DATE CRUISEJOINS***

drop  table haulname; 
drop  view haulname; 
create table haulname as 
SELECT  to_number(to_char(a.start_time,'yyyy')) year,A.*
FROM RACEBASE.HAUL A
JOIN RACE_DATA.V_CRUISES B
ON (B.CRUISEJOIN = A.CRUISEJOIN)
WHERE A.PERFORMANCE >= 0
AND A.HAUL_TYPE = 3
AND A.STATIONID IS NOT NULL
AND A.STRATUM IN (31,32,41,42,43)
AND B.SURVEY_DEFINITION_ID = 98;

drop table totarea;
drop view totarea;
create table totarea as 
select sum(area) totareas from 
racebase.stratum where 
region='BS' and year=2022 
and stratum in (31,32,41,42,43);

drop table stratwgt;
drop view stratwgt;
create table stratwgt as 
select stratum, (area/totareas) ratio 
from racebase.stratum r, totarea f 
where region='BS' and year=2022 
and stratum in (31,32,41,42,43);

drop table pretemptab;
drop view pretemptab;
create table pretemptab as 
select to_char(start_time,'YYYY') year, haul, r.stratum,(gear_temperature*ratio) pregtemp, 
(surface_temperature*ratio) prestemp from haulname r, stratwgt f 
where f.stratum=r.stratum;

drop table geartemp_subarea;
drop view geartemp_subarea;
create table geartemp_subarea as 
select year, stratum, avg(pregtemp) gtempwgt, avg(prestemp) stempwgt from pretemptab 
group by year, stratum;

drop table bttemp_ebs_50_100m;
drop view bttemp_ebs_50_100m;
create table bttemp_ebs_50_100m as
select year, sum(gtempwgt) avgbsbt, sum(stempwgt) avgbsst from geartemp_subarea 
group by year;


select * from bttemp_ebs_50_100m order by year;



