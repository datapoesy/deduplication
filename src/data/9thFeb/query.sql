create table dbstable as (select userid, recoid, someid, firstname, lastname, 
gender, dob_dd,dob_mm,dob_yy, email, mobilenumber, homenumber, address1, address2, cityname, 
postcode,county, nis from userdetails);


GRANT ALL PRIVILEGES ON TABLE dbstable TO dddev;

GRANT ALL PRIVILEGES ON TABLE userdetails TO dddev;
=======================================================

create table dupli as (select userid, recoid, someid, firstname, lastname, 
gender, dob_dd,dob_mm,dob_yy, email, mobilenumber, homenumber, address1, address2, cityname, 
postcode,county, nis from userdetails);



GRANT ALL PRIVILEGES ON TABLE dupli TO dddev;

delete from dupli;
=======================================================

create table nondupli as (select userid, recoid, someid, firstname, lastname, 
gender, dob_dd,dob_mm,dob_yy, email, mobilenumber, homenumber, address1, address2, cityname, 
postcode,county, nis from userdetails);


GRANT ALL PRIVILEGES ON TABLE nondupli TO dddev;

delete from nondupli;
=======================================================


delete from userdetails

UPDATE dbstable SET email='' where userdetail_id = 402

select * from dbstable

select * from userdetails

drop table dbstable;

drop table dupli;

select * from dupli

select * from nondupli

select * from dbstable 

SELECT count(*) from dbstable

select * from cemreport where cemname='Robyn'

delete from cemreport where cemname='Trevion'

update  cemreport set region='South Yorkshire' where cemname='Trevion'

update cemreport set region='South Yorkshire', division='South West' where cemname='Trevion'
