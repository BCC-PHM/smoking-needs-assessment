SELECT	
	GP_Code,
	GP_Name,
	PCN,
	Locality,
	IMD_Quintile,
	Gender_Desc AS Gender,
	ProxyAgeAtEOM AS Age,
	CASE
		WHEN Ethnic_Description_National = 'British' THEN 'White British'
		WHEN Ethnic_Description_National = 'Irish' THEN 'White Irish'
		WHEN Ethnic_Description_National = 'African' THEN 'Black African'
		WHEN Ethnic_Description_National = 'Caribbean' THEN 'Black Caribbean'
		ELSE Ethnic_Description_National
	END AS Ethnic_Description_National,
	SUM(1) AS BSOL_Registrants,
	BSOL_Table_Snapshot AS Extract_Date
FROM 
	EAT_Reporting_BSOL.Demographic.BSOL_Registered_Population
GROUP BY 
	GP_Code,
	GP_Name,
	PCN,
	Locality,
	IMD_Quintile,
	Gender_Desc,
	ProxyAgeAtEOM,
	Ethnic_Description_National,
	BSOL_Table_Snapshot