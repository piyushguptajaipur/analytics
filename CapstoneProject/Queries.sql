--select * from ProductServices where UID = 27242

--select * from Registration where EmailID = 'dhruvsaini@live.in'

--select TOP 94306 * from InvoiceDetails where UID in (25457) order by InvoiceID asc

--select count(*) from InvoiceDetails where UID = 25457

select UID,City from 
Registration where 
UID in (23752,23862,23884,23751,24850,23861,24353,27824,22331,24516,24702,23574,24249,25637,23577,27171,24144,25172,25786,24539,24349,25364)


--select count(*) from Invoice where UID in (23752)
--and DATALENGTH(CustomerMobileNo) > 0
--
--select CustomerMobileNo, count(*) from Invoice where UID in (23752) 
--and DATALENGTH(CustomerMobileNo) > 0
--group by CustomerMobileNo order by count(*) desc


--select * from ProductServices where UID = 25457

--select * from Registration where UID in 
--(
--select UID from Invoice where UID in 
--(select UID from Registration where IsDemoStore = 'false') 
--group by UID having count(*) > 30000
--) order by TypeOfBusiness asc

--select * from KeywordMaster where KeywordCategory = 'TypeOfBusiness' order by KeywordMasterID asc

--select UID, count(*) from Invoice where UID in (27808,25457,27822,27242) group by UID


-- Salon based Registrations
--select UID, count(*) from Invoice where UID in (
--select UID from Registration where TypeOfBusiness = 36 and IsDemoStore = 'false'
--) group by UID order by count(*) desc
