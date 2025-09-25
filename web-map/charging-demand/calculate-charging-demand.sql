/* =============================================================================
   Calculation of a demand forecast for public home charging for cars without private parking
   =============================================================================
   Purpose
     Build per-DeSO per-year forecasts of BEV/PHEV public charging demand
     (energy, sessions, average kWh/session), stored in the table
     RISE_DeSO_charging_demand_forecast

   Dataflow (parents are derived from children)
		(table provided in MONA by SCB)
		((table imported into MONA from file))

		Fordon2023_corrected
		  < (Fordon2023_New)

		RISE_andel_el_per_arsmod_2023
		  < Fordon2023_corrected

		RISE_exp_andel_elfordon_formansbilar_per_deso
		  < Fordon2023_corrected
		  < RISE_andel_el_per_arsmod_2023
		  < (Ind_2023)
		  < (GDB_Ind2023)
		  < (HushallBoende_2023)

		RISE_tilldelade_formansbilar
		  < Fordon2023_corrected
		  < RISE_exp_andel_elfordon_formansbilar_per_deso
		  < (IoT2022)
		  < (GDB_Ind2023)
		  < (Ind_2022)

		RISE_Fordon2023_privata_inkl_formansbilar
		  < Fordon2023_corrected
		  < RISE_tilldelade_formansbilar

		RISE_antal_bilar_per_deso_2023
		  < RISE_Fordon2023_privata_inkl_formansbilar
		  < (GDB_Ind2023)

		RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel
		  < ((RISE_folkmängd_prognos_efter_kommun_år_unpivot))

		RISE_Model_year_BEV_share
		  < Fordon2023_corrected

		RISE_BEV_andel_2023_per_DeSO
		  < RISE_antal_bilar_per_deso_2023
		  < RISE_Model_year_BEV_share

		RISE_BEV_andel_prognos_per_DeSO
		  < years
		  < RISE_antal_bilar_per_deso_2023
		  < RISE_Model_year_BEV_share
		  < RISE_BEV_andel_2023_per_DeSO
		  < RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel

		RISE_Fordon2023_Parkering
		  < RISE_Fordon2023_privata_inkl_formansbilar
		  < (GDB_Ind2023)
		  < (HushallBoende_2023)
		  < ((deso_areas_and_centroids))

		RISE_deso_BEV_energy_consumption
		  < RISE_Fordon2023_privata_inkl_formansbilar
		  < (GDB_Ind2023)

		RISE_kommun_medeltemp_kWh_per_km
		  < ((RISE_län_kommun))
		  < ((RISE_län_medeltemp))

		RISE_DeSO_charging_demand_forecast
		  < years
		  < RISE_BEV_andel_prognos_per_DeSO
		  < RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel
		  < RISE_Fordon2023_privata_inkl_formansbilar
		  < RISE_Fordon2023_Parkering
		  < RISE_deso_BEV_energy_consumption
		  < RISE_kommun_medeltemp_kWh_per_km
		  < (GDB_Ind2023)
		  < (HushallBoende_2023)

   =============================================================================
*/


/* ---------------------------------------------------------------------------
   Correction of mislabled old EVs
--------------------------------------------------------------------------- */
create or alter view Fordon2023_corrected as
	select
		*,
		case when Elfordon='LADDHYBRID' then 'LADDHYBRID' -- likely undersamples old PHEVs, but we lack the variable 'utslappsklass'
			when Drivm1!=3 or Elfordon = 'ELHYBRID' then 'ICEV'
			when Drivm1=3 and Drivm2=0 or Elfordon='EL' then 'EL'
			else 'OVRIG' end as Elfordon_corrected
	from Fordon2023_New
;

/* ---------------------------------------------------------------------------
   Several steps to assign company cars to individuals taxed for a car benefit (förmånsbilar)
--------------------------------------------------------------------------- */
create or alter view RISE_andel_el_per_arsmod_2023 as 
select
	fslag,
	Arsmod,
	avg(case when Elfordon_corrected='EL' then 1.0 else 0.0 end) as EL_andel,
	avg(case when Elfordon_corrected='LADDHYBRID' then 1.0 else 0.0 end) as LADDHYBRID_andel
from Fordon2023_corrected
where Status=2
group by 
	fslag,
	Arsmod
;

create or alter view RISE_exp_andel_elfordon_formansbilar_per_deso as
with
-- Calculate the observed number of BEVs and PHEVs per DeSO
fordon_per_deso_arsmod_boendeform_2023 as (
	select
		i2.DeSO,
		Arsmod,
		Boendeform,
		1.0 * count(*) as antal_fordon,
		sum(case when Elfordon_corrected='EL' then 1.0 else 0.0 end) as antal_EL,
		sum(case when Elfordon_corrected='LADDHYBRID' then 1.0 else 0.0 end) as antal_LADDHYBRID
	from
		Fordon2023_corrected f
		left join Ind_2023 i on i.P1376_Lopnr_personnr = f.P1376_Lopnr_AgNr
		left join GDB_Ind2023 i2 on i2.P1376_Lopnr_PersonNr = i.P1376_Lopnr_personnr
		left join HushallBoende_2023 h on h.P1376_LopNr_PersonNr = i.P1376_Lopnr_personnr
	where
		f.Status = 2 and f.P1376_Lopnr_AgNr is not null and fslag='PB'
	group by
		i2.DeSO, Arsmod, Boendeform
),
-- Calculate age-normalized EV ratios (vs. expected from the age distribution)
freg_per_deso as (
	select
		a.DeSO,
		sum(antal_fordon) as f_antal_fordon,
		sum(a.antal_EL) as f_antal_EL_obs,
		sum(a.antal_fordon * b.EL_andel) as f_antal_EL_exp,
		case when sum(a.antal_fordon * b.EL_andel) = 0 then 1 else sum(a.antal_EL) / sum(a.antal_fordon * b.EL_andel) end as f_andel_EL_norm,
		sum(a.antal_LADDHYBRID) as f_antal_LADDHYBRID_obs,
		sum(a.antal_fordon * b.LADDHYBRID_andel) as f_antal_LADDHYBRID_exp,
		case when sum(a.antal_fordon * b.LADDHYBRID_andel) = 0 then 1 else sum(a.antal_LADDHYBRID) / sum(a.antal_fordon * b.LADDHYBRID_andel) end as f_andel_LADDHYBRID_norm
	from fordon_per_deso_arsmod_boendeform_2023 a
		join RISE_andel_el_per_arsmod_2023 b on b.Arsmod = a.Arsmod and b.fslag = 'PB'
	group by
		a.DeSO),
-- Calculate the EV ratio of all förmånsbilar (the correct data doesn't exist and we assume the same ratios as for leased company cars)
formansbilar_ev_ratio as (
	select
		sum(case when Elfordon_corrected = 'EL' then 1.0 else 0.0 end) / count(*) as fb_andel_el,
		sum(case when Elfordon_corrected = 'LADDHYBRID' then 1.0 else 0.0 end) / count(*) as fb_andel_laddhybrid
	from
		Fordon2023_corrected f
	where Status=2 and fslag='PB' and P1376_Lopnr_AgNr is null and Leas=1
)
-- Calculate the expected share of EVs among förmånsbilar per DeSO
select
	DeSO,
	least(1, f_andel_EL_norm * fb_andel_el) as andel_el,
	least(1 - f_andel_EL_norm * fb_andel_el, f_andel_LADDHYBRID_norm * fb_andel_laddhybrid) as andel_laddhybrid,
	greatest(0, 1 - f_andel_EL_norm * fb_andel_el - f_andel_LADDHYBRID_norm * fb_andel_laddhybrid) as andel_ovriga
from (
	select
		f.*,
		fb.fb_andel_el,
		fb.fb_andel_laddhybrid
	from
		freg_per_deso f
		join formansbilar_ev_ratio fb on 1=1 -- only one row
	) T
;

drop table RISE_tilldelade_formansbilar;
with
antal_individer as (
	select
		ii.DeSO,
		max(i.kommun) as kommun,
		count(*) as antal_fbesk_ind
	from Ind_2022 i
	join GDB_Ind2023 ii on ii.P1376_Lopnr_PersonNr = i.P1376_Lopnr_personnr
	join IoT2022 iot on iot.P1376_LopNr_PersonNr = i.P1376_Lopnr_personnr
	where iot.TBILF > 0
	group by ii.DeSO
),
antal_formanstagare_per_deso_drivmedel as (
	select 
		a.DeSO,
		a.antal_fbesk_ind * ef.andel_el as antal_el,
		a.antal_fbesk_ind * ef.andel_laddhybrid as antal_laddhybrid,
		a.antal_fbesk_ind * ef.andel_ovriga as antal_ovriga
	from
		RISE_exp_andel_elfordon_formansbilar_per_deso as ef
		join antal_individer a on a.DeSO = ef.DeSO
),
antal_formanstagare_per_kommun_drivmedel as (
	select 
		left(DeSO, 4) as kommun,
		sum(antal_el) as antal_el,
		sum(antal_laddhybrid) as antal_laddhybrid,
		sum(antal_ovriga) as antal_ovriga
	from antal_formanstagare_per_deso_drivmedel
	group by left(DeSO, 4)
),
-- dela upp lokala bilägare på drivmedel efter andelar
formanstagare_tilldelning_drivmedel as (
	select
		T.*,
		row_number() over (partition by Elfordon_tilldelning order by TBILF) as rownum
	from (
		select
			P1376_LopNr_PersonNr,
			r.Kommun,
			r.DeSO,
			r.TBILF,
			case when rnd_rnk < antal_el then 'EL' when rnd_rnk < antal_el+antal_laddhybrid then 'LADDHYBRID' else 'OVRIG' end as Elfordon_tilldelning
		from (
			select
				ii.P1376_LopNr_PersonNr,
				DeSO,
				left(DeSO,4) as Kommun,
				ii.TBILF,
				row_number() over (partition by DeSO order by newid()) as rnd_rnk
			from
				IoT2022 ii
				join GDB_Ind2023 di on di.P1376_Lopnr_PersonNr = ii.P1376_LopNr_PersonNr
			where ii.tbilf > 0
			) r
			join antal_formanstagare_per_deso_drivmedel a on a.DeSO = r.DeSO
		) T
),
-- sampla tjänstebilar att fördela ut
foretagsbilar as (
	select
		P1376_Lopnr_FordonsID,
		case when f.Elfordon_corrected in ('EL', 'LADDHYBRID') then f.Elfordon_corrected else 'OVRIG' end as Elfordon_enkel,
		row_number() over (partition by (case when f.Elfordon_corrected in ('EL', 'LADDHYBRID') then f.Elfordon_corrected else 'OVRIG' end) order by newid()) rownum
	from Fordon2023_corrected f
	where Status = 2 and fslag = 'PB' and P1376_Lopnr_AgNr is null and Arsmod > 2019
)
select
	P1376_Lopnr_FordonsID, p.P1376_LopNr_PersonNr
into RISE_tilldelade_formansbilar
from
	formanstagare_tilldelning_drivmedel p
	join foretagsbilar fb on fb.Elfordon_enkel = p.Elfordon_tilldelning and fb.rownum = p.rownum
;


drop table if exists RISE_Fordon2023_privata_inkl_formansbilar;
select 
	--ROW_NUMBER() over (ORDER BY P1376_Lopnr_FordonsID) as RISE_Lopnr_FordonsID,
	*
into RISE_Fordon2023_privata_inkl_formansbilar
from (
	select 
		f.P1376_Lopnr_FordonsID,
		P1376_Lopnr_AgNr,
		P1376_Lopnr_AgNr_PeOrgNr,
		fb.P1376_LopNr_PersonNr as P1376_LopNr_AgNr_tjbil, 
		1 as Formansbil, 
		Arsmod,	fslag, Elfordon_corrected, Bredd, DatFReg, FKatEU, TjVikt, TotVikt, AgKat, Alder, AntAg, DatOvlAgare, Drivm1, Drivm2, Drivm3, EnergiK, EnergiK_WLTP_driv1, Rackvidd_WLTP_driv1, YTaxi,
		Kon, Leas, korstracka, modellskatt, nyreg, nypris, kommun, lan
	from Fordon2023_corrected f
		join RISE_tilldelade_formansbilar fb on fb.P1376_Lopnr_FordonsID = f.P1376_Lopnr_FordonsID -- no vans
	union
	select 
		P1376_Lopnr_FordonsID,
		P1376_Lopnr_AgNr,
		P1376_Lopnr_AgNr_PeOrgNr,
		null as P1376_LopNr_AgNr_tjbil, 
		0 as Formansbil, 
		Arsmod,	fslag, Elfordon_corrected, Bredd, DatFReg, FKatEU, TjVikt, TotVikt, AgKat, Alder, AntAg, DatOvlAgare, Drivm1, Drivm2, Drivm3, EnergiK, EnergiK_WLTP_driv1, Rackvidd_WLTP_driv1, YTaxi,
		Kon, Leas, korstracka, modellskatt, nyreg, nypris, kommun, lan
	from Fordon2023_corrected
	where P1376_Lopnr_AgNr is not null and Status=2 -- include vans
) T
;
create clustered index idx_v2 on RISE_Fordon2023_privata_inkl_formansbilar (P1376_Lopnr_AgNr, P1376_Lopnr_AgNr_tjbil, P1376_Lopnr_AgNr_PeOrgNr);
create unique index idx_v3 on RISE_Fordon2023_privata_inkl_formansbilar (P1376_Lopnr_FordonsID);

/* ---------------------------------------------------------------------------
   Utility years table
--------------------------------------------------------------------------- */
DROP TABLE IF EXISTS years;
WITH x AS (SELECT n FROM (VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8),(9)) v(n))
SELECT ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS year
INTO years
FROM x ones, x tens, x hundreds, x thousands
ORDER BY 1;

/* ---------------------------------------------------------------------------
   Base counts per DeSO & model year for 2023 fleet (used in BEV share scaling)
--------------------------------------------------------------------------- */
DROP TABLE IF EXISTS RISE_antal_bilar_per_deso_2023;
SELECT 
	DeSO, 
	fslag,
	Arsmod, 
	sum(case when Elfordon_corrected='EL' then 1 else 0 end) AS El, 
	sum(case when Elfordon_corrected='LADDHYBRID' then 1 else 0 end) AS Laddhybrid, 
	sum(case when Elfordon_corrected in ('EL', 'LADDHYBRID') then 1 else 0 end) AS Ovrig,
	count(*) AS Total
INTO RISE_antal_bilar_per_deso_2023
FROM RISE_Fordon2023_privata_inkl_formansbilar f
LEFT JOIN GDB_Ind2023 i2 on i2.P1376_Lopnr_PersonNr = coalesce(f.P1376_Lopnr_AgNr, f.P1376_Lopnr_AgNr_tjbil)
GROUP BY DeSO, fslag, Arsmod;

/* ---------------------------------------------------------------------------
   Municipal population growth (relative to 2022)
   DOC: RISE_folkmängd_prognos_efter_kommun_år_unpivot must be imported already.
--------------------------------------------------------------------------- */
DROP VIEW IF EXISTS RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel;
CREATE VIEW RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel as
SELECT
	b.*, 1.0*b.Befolkning / b2022.Befolkning AS Befolkning_rel
FROM
	RISE_folkmängd_prognos_efter_kommun_år_unpivot b
	JOIN RISE_folkmängd_prognos_efter_kommun_år_unpivot b2022 
      ON b.kommunkod = b2022.kommunkod AND b2022."År"=2022;

/* ---------------------------------------------------------------------------
   Model-year BEV/PHEV shares (historic + forward logistic projection)
   DOC: Uses Fordon2023_corrected (pre-existing import).
--------------------------------------------------------------------------- */
DROP TABLE IF EXISTS RISE_Model_year_BEV_share;
SELECT * INTO RISE_Model_year_BEV_share
FROM (
	SELECT 
		year as model_year, 
		1.0 / (1.0 + exp((-year+2010+16)/2.5)) as BEV_share,
		1.0 / (1.0 + exp((-year+2010+14)/2.5)) 
          - 1.0 / (1.0 + exp((-year+2010+16)/2.5)) as PHEV_share,
		1 - 1.0 / (1.0 + exp((-year+2010+14)/2.5)) as ICEV_share
	FROM years where year between 2024 and 2070
	UNION
	SELECT 
		Arsmod as model_year,
		sum(case when Elfordon_corrected = 'EL' then 1.0 else 0.0 end) / count(*) as BEV_share,
		sum(case when Elfordon_corrected = 'LADDHYBRID' then 1.0 else 0.0 end) / count(*) as PHEV_share,
		sum(case when Elfordon_corrected not in ('EL', 'LADDHYBRID') then 1.0 else 0.0 end) as ICEV_share
	FROM Fordon2023_corrected
	WHERE Arsmod < 2024
	GROUP BY Arsmod
) T
ORDER BY model_year;

/* ---------------------------------------------------------------------------
   Clamp national model-year shares to local observed 2023 DeSO composition
--------------------------------------------------------------------------- */
CREATE OR ALTER VIEW RISE_BEV_andel_2023_per_DeSO as
SELECT
	DeSO,
	fslag,
	sum(El) AS BEV_obs,
	sum(Total * BEV_andel_est) AS BEV_est,
	case when sum(BEV_andel_est) = 0 then 1 else sum(El) / sum(Total * BEV_andel_est) end as BEV_obs_est_ratio,
	sum(Laddhybrid) AS PHEV_obs,
	sum(Total * PHEV_andel_est) AS PHEV_est,
	case when sum(PHEV_andel_est) = 0 then 1 else sum(Laddhybrid) / sum(Total * PHEV_andel_est) end as PHEV_obs_est_ratio
FROM (
	SELECT 
		DeSO, 
		fslag,
		Arsmod, 
		El,
		Laddhybrid,
		Total,
		1.0 * El / Total AS BEV_andel_obs,
		1.0 * Laddhybrid / Total AS PHEV_andel_obs,
		r.BEV_share AS BEV_andel_est,
		r.PHEV_share AS PHEV_andel_est
	FROM 
		RISE_antal_bilar_per_deso_2023 b
		JOIN RISE_Model_year_BEV_share r ON r.model_year = b.arsmod
) T
GROUP BY DeSO, fslag;

/* ---------------------------------------------------------------------------
   BEV/PHEV shares by DeSO × calendar year × vehicle age
--------------------------------------------------------------------------- */
CREATE OR ALTER VIEW RISE_BEV_andel_prognos_per_DeSO as
SELECT
	DeSO, fslag, Calendar_year, Model_year, Vehicle_age, Age_share, Total_pred,
	least((mix_ratio + (1-mix_ratio) * BEV_obs_est_ratio) * BEV_pred, Total_pred) AS BEV_pred,
	least((mix_ratio + (1-mix_ratio) * BEV_obs_est_ratio) * BEV_share_pred, 1) AS BEV_share_pred,
	least((mix_ratio + (1-mix_ratio) * PHEV_obs_est_ratio) * BEV_pred, Total_pred) AS PHEV_pred,
	least((mix_ratio + (1-mix_ratio) * PHEV_obs_est_ratio) * PHEV_share_pred, 1) AS PHEV_share_pred
FROM (
	SELECT 
		T_age_distr.DeSO,
		T_age_distr.fslag,
		Y.Calendar_year,
		Y.Calendar_year - T_age_distr.Vehicle_age AS Model_year,
		T_age_distr.Vehicle_age, 
		T_age_distr.Age_share,
		COALESCE(T_age_distr.Total * pop.Befolkning_rel, 0) AS Total_pred,
		COALESCE(T_age_distr.Total * pop.Befolkning_rel, 0) * COALESCE(BEV_share, 0) AS BEV_pred,
		COALESCE(BEV_share, 0) AS BEV_share_pred,
		R23.BEV_obs_est_ratio,
		COALESCE(T_age_distr.Total * pop.Befolkning_rel, 0) * COALESCE(PHEV_share, 0) AS PHEV_pred,
		COALESCE(PHEV_share, 0) AS PHEV_share_pred,
		R23.PHEV_obs_est_ratio,
		least(1, (Y.Calendar_year - 2023.0) / (2050.0 - 2023.0)) AS mix_ratio
	FROM
		/* Vehicle age distribution per DeSO (from 2023 fleet) */
		(SELECT 
			DeSO, 
			fslag,
			2024 - Arsmod AS Vehicle_age, 
			Total, 
			1.0 * Total / sum(Total) over (partition by DeSO) AS Age_share 
		 FROM (SELECT DeSO, fslag, Arsmod, Total FROM RISE_antal_bilar_per_deso_2023) T
		) T_age_distr
		/* Replicate for all calendar years of interest */
		CROSS JOIN (SELECT year AS Calendar_year FROM years WHERE year BETWEEN 2022 AND 2050) Y
		/* Append estimated shares per calendar year AND vehicle age */
		LEFT JOIN (
			SELECT *, Calendar_year - Model_year AS Vehicle_age
			FROM RISE_Model_year_BEV_share 
			CROSS JOIN (SELECT year AS Calendar_year FROM years WHERE year BETWEEN 2022 AND 2050) Y
			WHERE Calendar_year >= Model_year
		) T2 ON T2.Vehicle_age = T_age_distr.Vehicle_age AND T2.Calendar_year = Y.Calendar_year
		/* Local scaling factors and population change */
		JOIN RISE_BEV_andel_2023_per_DeSO AS R23 ON R23.DeSO = T_age_distr.DeSO and R23.fslag = T_age_distr.fslag
		JOIN RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel AS pop 
             ON pop.kommunkod = left(T_age_distr.DeSO, 4) AND pop."År" = Y.Calendar_year
) T;

/* ---------------------------------------------------------------------------
   Climate-normalized consumption factors
   DOC: RISE_län_medeltemp must be WIZARD-imported with *_str columns.
        We cast those to float first, then build the climate multipliers per municipality.
--------------------------------------------------------------------------- */
ALTER TABLE RISE_län_medeltemp ADD år float, januari float, juli float;
UPDATE RISE_län_medeltemp 
SET år = cast(år_str as float), januari = cast(januari_str as float), juli = cast(juli_str as float);

CREATE OR ALTER VIEW RISE_kommun_medeltemp_kWh_per_km as
SELECT
	KommunKod, Kommun, år, januari, juli,
	1 / (0.4+0.75 * exp(-power(år-21,2) / (2*power(18,2)))) AS år_kWh_per_km_norm,
	1 / (0.4+0.75 * exp(-power(januari-21,2) / (2*power(18,2)))) AS januari_kWh_per_km_norm,
	1 / (0.4+0.75 * exp(-power(juli-21,2) / (2*power(18,2)))) AS juli_kWh_per_km_norm
FROM
	RISE_län_kommun AS k
	JOIN RISE_län_medeltemp AS t ON t.Län = k.Län;

/* ---------------------------------------------------------------------------
   DeSO-level BEV energy consumption (uses climate factors)
--------------------------------------------------------------------------- */
CREATE OR ALTER VIEW RISE_deso_BEV_energy_consumption as
SELECT
	T.*, år_kWh_per_km_norm, januari_kWh_per_km_norm, juli_kWh_per_km_norm
FROM (
	SELECT 
		DeSO, 
		CASE WHEN count(*) > 9 
             THEN avg(coalesce(EnergiK, cast(EnergiK_WLTP_driv1 as float))) 
             ELSE 175.7 end AS wh_per_km,
		count(*) AS Sample_size
	FROM
		RISE_Fordon2023_privata_inkl_formansbilar f
		LEFT JOIN GDB_Ind2023 i2 
          ON i2.P1376_Lopnr_PersonNr = coalesce(f.P1376_Lopnr_AgNr, f.P1376_Lopnr_AgNr_tjbil)
	WHERE Elfordon_corrected = 'EL' AND EnergiK is not null AND Arsmod > 2018
	GROUP BY DeSO
) T
JOIN RISE_kommun_medeltemp_kWh_per_km k ON k.kommunkod = left(deso, 4);

/* ---------------------------------------------------------------------------
   Public-parking probability per vehicle
   DOC: Requires MANUAL import of deso_areas_and_centroids (CSV with DeSO density).
--------------------------------------------------------------------------- */
DROP TABLE IF EXISTS RISE_Fordon2023_Parkering;
WITH params AS (
  SELECT
    0.90 AS resident_share_mdh,
    1.00 AS resident_share_smahus,
    2.00 AS baseline_smahus_p_tal,
    0.85 AS mdh_utilization_cap,
    0.10 AS mdh_min_p_tal,
    1.20 AS mdh_max_p_tal
),
buildings AS (
	select
		i2.DeSO,
		P1376_LopNr_FastBeteckn,
		count(distinct coalesce(P1376_LopNr_UUID_Lgh, P1376_LopNr_FastBeteckn)) as antal_hushall,
		min(h.SCB_Byggar) as SCB_Byggar,
		min(case when left(h.Boendeform, 4) in ('Fler', 'Spec') then 'Fler'
			 when left(h.Boendeform, 6) = 'Smahus' then 'Smahus'
			 else 'Ovr' end) as Boendeform,
		min(d.befolkning_per_km2) as befolkning_per_km2
	from HushallBoende_2023 h
	  LEFT JOIN GDB_Ind2023 i2 on i2.P1376_Lopnr_PersonNr = h.P1376_LopNr_PersonNr
	  left join deso_areas_and_centroids d on d.DeSO = i2.DeSO
	group by
		i2.DeSO,
		P1376_LopNr_FastBeteckn
),
buildings_scored_1 AS (
  SELECT
	b.*,
    CASE
      WHEN Boendeform in ('Smahus', 'Ovr') THEN p.baseline_smahus_p_tal
      ELSE CASE
        WHEN b.SCB_Byggar <= 1945 THEN 0.20
        WHEN b.SCB_Byggar <= 1960 THEN 0.35
        WHEN b.SCB_Byggar <= 1975 THEN 0.85
        WHEN b.SCB_Byggar <= 1990 THEN 0.75
        WHEN b.SCB_Byggar <= 2005 THEN 0.65
        WHEN b.SCB_Byggar <= 2020 THEN 0.50
        ELSE 0.55
      END
    END AS p_tal_baseline,
    CASE
      WHEN b.befolkning_per_km2 >= 10000 THEN -0.20
      WHEN b.befolkning_per_km2 >=  5000 THEN -0.15
      WHEN b.befolkning_per_km2 >=  2000 THEN -0.10
      WHEN b.befolkning_per_km2 >=  1000 THEN -0.05
      WHEN b.befolkning_per_km2 >=   200 THEN  0.00
      ELSE 0.10
    END AS density_adj,
    p.resident_share_mdh, p.resident_share_smahus, p.mdh_utilization_cap, p.mdh_min_p_tal, p.mdh_max_p_tal
  FROM buildings b
  CROSS JOIN params p
),
buildings_scored_2 AS (
  SELECT
	DeSO, 
	b.P1376_LopNr_FastBeteckn, 
	antal_hushall *
	  CASE
		WHEN b.Boendeform in ('Smahus', 'Ovr')
		  THEN b.resident_share_smahus * (b.p_tal_baseline + b.density_adj)
		ELSE b.resident_share_mdh * b.mdh_utilization_cap 
           * LEAST(b.mdh_max_p_tal, GREATEST(b.mdh_min_p_tal, b.p_tal_baseline + b.density_adj))
	  END AS est_private_parking_spaces
  FROM buildings_scored_1 b
),
buildings_scored_3 as (
	select
		b.DeSO,
		b.P1376_LopNr_FastBeteckn,
		least(min(est_private_parking_spaces), count(distinct f.P1376_Lopnr_FordonsID)) as privately_parked_cars,
		greatest(0, count(distinct f.P1376_Lopnr_FordonsID) - min(est_private_parking_spaces)) as publicly_parked_cars
	from RISE_Fordon2023_privata_inkl_formansbilar f
		join GDB_Ind2023 ii on ii.P1376_Lopnr_PersonNr = coalesce(f.P1376_Lopnr_AgNr, f.P1376_Lopnr_AgNr_tjbil)
		join HushallBoende_2023 h on h.P1376_Lopnr_PersonNr = ii.P1376_Lopnr_PersonNr
 		join buildings_scored_2 b on b.DeSO = ii.DeSO and b.P1376_LopNr_FastBeteckn = h.P1376_LopNr_FastBeteckn
	group by b.DeSO, b.P1376_LopNr_FastBeteckn
)
SELECT
	f.P1376_Lopnr_FordonsID,
	(1.0 * b.publicly_parked_cars) / (b.publicly_parked_cars + b.privately_parked_cars) as public_parking_prob
INTO RISE_Fordon2023_Parkering
FROM
	RISE_Fordon2023_privata_inkl_formansbilar f
	JOIN GDB_Ind2023 ii on ii.P1376_Lopnr_PersonNr = coalesce(f.P1376_Lopnr_AgNr, f.P1376_Lopnr_AgNr_tjbil)
	JOIN HushallBoende_2023 h on h.P1376_Lopnr_PersonNr = ii.P1376_Lopnr_PersonNr
 	JOIN buildings_scored_3 b on b.DeSO = ii.DeSO and b.P1376_LopNr_FastBeteckn = h.P1376_LopNr_FastBeteckn;

/* ---------------------------------------------------------------------------
   FINAL: RISE_DeSO_charging_demand_forecast
--------------------------------------------------------------------------- */
DROP TABLE IF EXISTS RISE_DeSO_charging_demand_forecast;

WITH vehicles AS (
	SELECT
		/* rows are cars or vans */
		f.P1376_Lopnr_FordonsID,
		i.DeSO,
		f.fslag, /* include vans, but treat them as cars in terms of charging demand */
		case when left(h.Boendeform, 4) = 'Fler' then 1.0 else 0.0 end as is_mud,
		2024 - f.Arsmod as vehicle_age,
		10 * (CASE WHEN f.Arsmod < 2023 THEN f.korstracka ELSE f.korstracka * 365.0 / DATEDIFF(day, f.DatFReg, '2024-01-01') END) as km_per_year,
		coalesce(w.wh_per_km, 175.7) / 1000 as kWh_per_km_wltp_2024, /* national avg if missing */
		k.år_kWh_per_km_norm as kWh_per_km_climate_multiple,
		p.public_parking_prob,
		case when f.Formansbil = 1 then 0.15 else 0.5 end as electric_share_if_phev
	FROM
		RISE_Fordon2023_privata_inkl_formansbilar f
		LEFT JOIN RISE_Fordon2023_Parkering p ON p.P1376_Lopnr_FordonsID = f.P1376_Lopnr_FordonsID
		LEFT JOIN GDB_Ind2023 i ON i.P1376_Lopnr_PersonNr = coalesce(f.P1376_Lopnr_AgNr, f.P1376_Lopnr_AgNr_tjbil)
		LEFT JOIN RISE_deso_BEV_energy_consumption w ON w.DeSO = i.DeSO
		LEFT JOIN RISE_kommun_medeltemp_kWh_per_km k ON k.KommunKod = left(i.DeSO, 4)
		LEFT JOIN HushallBoende_2023 h ON h.P1376_LopNr_PersonNr = i.P1376_Lopnr_PersonNr
),
vehicles_2 AS (
	SELECT /* compensate for population change per municipality */
		f.P1376_Lopnr_FordonsID,
		f.DeSO,
		y.year as calendar_year,
		f.is_mud,
		pop.Befolkning_rel,
		f.vehicle_age,
		y.year - f.vehicle_age as model_year,
		f.km_per_year,
		/* Energy per km trend + climate factor */
		kWh_per_km_wltp_2024 * power(0.995, y.year - f.vehicle_age - 2023) as kWh_per_km_wltp,
		kWh_per_km_wltp_2024 * power(0.995, y.year - f.vehicle_age - 2023) * kWh_per_km_climate_multiple as kWh_per_km_real,
		/* Range & battery (simple trend) */
		greatest(100, 400 + (y.year - f.vehicle_age - 2020) * 700.0/30.0) as BEV_range_km_wltp,
		greatest(100, 400 + (y.year - f.vehicle_age - 2020) * 700.0/30.0)
			* kWh_per_km_wltp_2024 * power(0.995, y.year - f.vehicle_age - 2023) as BEV_battery_kWh,
		b.BEV_share_pred,
		b.PHEV_share_pred,
		f.electric_share_if_phev,
		f.public_parking_prob
	FROM 
		vehicles f
		CROSS JOIN years y
		LEFT JOIN RISE_BEV_andel_prognos_per_DeSO b 
          ON b.DeSO = f.DeSO AND b.fslag = f.fslag 
         AND b.Calendar_year = y.year AND b.Vehicle_age = f.vehicle_age
		LEFT JOIN RISE_folkmängd_prognos_efter_kommun_år_unpivot_rel pop 
          ON pop.Kommunkod = left(f.deso, 4) AND pop."År" = y.year
	WHERE (y.year BETWEEN 2022 AND 2035 OR y.year IN (2035, 2040, 2045, 2050))
),
vehicles_3 AS (
	SELECT
		*,
		km_per_year * kWh_per_km_real as kWh_per_year,
		/* BEV/PHEV energy per charging session (simple SoC deltas) */
		(0.9 * BEV_range_km_wltp - (100 + 0.3 * BEV_range_km_wltp) / 2) / BEV_range_km_wltp 
			* BEV_battery_kWh as BEV_kWh_per_charging_session,
		(0.9 * (BEV_range_km_wltp * 0.2) - 10) / (BEV_range_km_wltp * 0.2)
			* (BEV_battery_kWh * 0.2) as PHEV_kWh_per_charging_session
	FROM vehicles_2
),
forecasts AS (
	SELECT
		DeSO,
		calendar_year,
		round(sum(Befolkning_rel),0)                                   as vehicles_n,
		round(sum(Befolkning_rel * is_mud),0)                          as vehicles_n_mud,
		round(sum(Befolkning_rel * public_parking_prob),0)             as vehicles_n_pp,

		round(sum(Befolkning_rel * BEV_share_pred),0)                  as BEV_n,
		round(sum(Befolkning_rel * BEV_share_pred * is_mud),0)         as BEV_n_mud,
		round(sum(Befolkning_rel * BEV_share_pred * public_parking_prob),0) as BEV_n_pp,
		round(sum(Befolkning_rel * BEV_share_pred * kWh_per_year),0)   as total_BEV_kWh_per_year,
		round(sum(Befolkning_rel * BEV_share_pred * is_mud * kWh_per_year),0)  as total_BEV_kWh_per_year_mud,
		round(sum(Befolkning_rel * BEV_share_pred * public_parking_prob * kWh_per_year),0) as total_BEV_kWh_per_year_pp,
		round(sum(BEV_share_pred * public_parking_prob * BEV_kWh_per_charging_session) 
              / nullif(sum(BEV_share_pred * public_parking_prob) ,0) ,0) as BEV_avg_kWh_per_charging_session_pp,
		round(sum(BEV_share_pred * public_parking_prob * kWh_per_year 
              / nullif(BEV_kWh_per_charging_session, 0)),0) as BEV_charging_sessions_n_per_year_pp,

		round(sum(Befolkning_rel * PHEV_share_pred),0)                 as PHEV_n,
		round(sum(Befolkning_rel * PHEV_share_pred * is_mud),0)        as PHEV_n_mud,
		round(sum(Befolkning_rel * PHEV_share_pred * public_parking_prob),0) as PHEV_n_pp,
		round(sum(Befolkning_rel * PHEV_share_pred * electric_share_if_phev * kWh_per_year),0) as total_PHEV_kWh_per_year,
		round(sum(Befolkning_rel * PHEV_share_pred * is_mud * electric_share_if_phev * kWh_per_year),0) as total_PHEV_kWh_per_year_mud,
		round(sum(Befolkning_rel * PHEV_share_pred * public_parking_prob * electric_share_if_phev * kWh_per_year),0) as total_PHEV_kWh_per_year_pp,
		round(sum(PHEV_share_pred * public_parking_prob * PHEV_kWh_per_charging_session) 
              / nullif(sum(PHEV_share_pred * public_parking_prob), 0), 0) as PHEV_avg_kWh_per_charging_session_pp,
		round(sum(PHEV_share_pred * public_parking_prob * electric_share_if_phev * kWh_per_year 
              / nullif(PHEV_kWh_per_charging_session, 0)),0) as PHEV_charging_sessions_n_per_year_pp
	FROM vehicles_3
	GROUP BY DeSO, calendar_year
)
SELECT
	*,
	round( (BEV_charging_sessions_n_per_year_pp * BEV_avg_kWh_per_charging_session_pp 
          + PHEV_charging_sessions_n_per_year_pp * PHEV_avg_kWh_per_charging_session_pp) 
          / nullif(BEV_charging_sessions_n_per_year_pp + PHEV_charging_sessions_n_per_year_pp, 0), 0)
		as EV_avg_kWh_per_charging_session_pp,
	BEV_charging_sessions_n_per_year_pp + PHEV_charging_sessions_n_per_year_pp 
        as EV_charging_sessions_n_per_year_pp
INTO RISE_DeSO_charging_demand_forecast
FROM forecasts;

-- FINAL EXPORT
SELECT * FROM RISE_DeSO_charging_demand_forecast 
WHERE DeSO IS NOT NULL
ORDER BY DeSO, calendar_year;
