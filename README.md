# PEATREST: A lifecycle assessment (LCA) model of carbon fluxes for restored afforested peatlands

## Jacob O'Sullivan 2026

The life cycle assessment (LCA) PEATREST is a tool for assessing the carbon fluxes associated with restoring peatland previously drained for forestry. The LCA quantifies the carbon fluxes due to restoration interventions and ecosystem recovery compared against a ‘counterfactual’ of forestry retention. The model predicts two summary statistics: the carbon payback time and the carbon flux intercept. These represent the time following harvesting at which the peatland is predicted to first store or sequester more carbon than if the forestry had been retained. 

For the counterfactual, the model accounts for three key processes: carbon sequestration by the forest, gaseous emissions (CO2/CH4) from the drained peat under the forest stand and leaching of aquatic carbon (DOC and POC). The restoration scenario represents emissions associated with harvesting and management, carbon loss due to decay of forest biomass, gaseous emissions from the peatland and aquatic carbon losses. 

![](auto-generated-path-to-file-when-you-upload-image "Carbon mitigation times predicted using the PEATREST LCA")


To run the model the user must input the following:
-	Area to be harvested/restored (ha)
-	Tree species (currently Sitka spruce and Scots pine are available)
-	Either a) Yield class or b) average tree height (m) and stand age (years)
-	Drainage depths at the site (m) at the time of tree removal
-	Water table depth under the trees (m)
-	Peat depth at the site (m)
-	Expected restoration time (years) following restoration
-	Restoration strategy including whether the forestry will be harvested and removed for processing or felled and left on site. 

Key assumptions:
- For tractability, forest growth is represented using a simplified model which is parameterised to reproduce the productivity of the forestry but ignores environmental fluctuations or long-term change. 
- Emissions rates from peats are predicted from water table depths only which are represented by annual averages; stochastic or intra-annual variation is ignored
- Additional drivers of emissions rates such as temperature, soil chemistry or litter input from the trees are not considered
- Aquatic carbon losses are represented using empirically estimated ratios to gaseous emissions
- The restoration of ecosystem function is represented using arbitrary functions chosen for their qualitative properties but not derived from first principles
