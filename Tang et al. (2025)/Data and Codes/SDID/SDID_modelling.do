// Import data from the GSCM csv file, take 'Southeast_Asia' as an example here.
// You can change the country_or_region variable to 'Brazil', 'Peru', 'Colombia', 'Africa' and 'Pantropical'
local country_or_region "Southeast_Asia"
import delimited "../GSCM/`country_or_region'_sc_data_for_GSCM.csv", clear

// Execute SDID and output the ATT result and graph
destring year dem slope road_dist water_dist soil_cec soil_soc deforest_hotspot settlement_cluster npp, replace
sdid deforest_rate region year treated, vce(placebo) method(sdid) seed(1013) reps(1000) covariates(dem slope road_dist water_dist soil_cec soil_soc deforest_hotspot settlement_cluster npp, optimized) graph g1on g1_opt(xtitle("") scheme(plotplainblind))  g2_opt(xlabel(2000(5)2025) ytitle("Deforestion rate (%)") xtitle("Year") scheme(plotplainblind)) graph_export("`country_or_region'_sdid_", .eps)

// Output the ATT result of each project start year
matlist e(tau)
