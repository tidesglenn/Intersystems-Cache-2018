-- ref_flat_selects.sql (attached copy)
SELECT COUNT(*) AS n FROM ref_flat_referral;
SELECT COUNT(*) AS n FROM ref_flat_referralline;
SELECT COUNT(*) AS n FROM ref_flat_referraleditxn;
SELECT hmo,id,rin,refnum,status FROM ref_flat_referral ORDER BY hmo,id,rin LIMIT 20;
SELECT hmo,id,rin,line,units,unittype,freq FROM ref_flat_referralline ORDER BY hmo,id,rin LIMIT 20;
SELECT hmo,id,rin,tradingpartner,dt,tm FROM ref_flat_referraleditxn ORDER BY hmo,id,rin LIMIT 20;
