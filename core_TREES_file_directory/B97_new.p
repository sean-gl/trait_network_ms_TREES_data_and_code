1425.0	altitude, m@Larm
40.447 41.367 	latitude
-104.647	longitude
3.5	z_ref, m
0.1 2.0 20.0	lai, single sided [sean] changing.  Default = 0.06, 2.0, 20.0
2.0	canopy_height, m
4.5	lai_at_full_canopy_height   [sean] Default = 4.5 (50 per increase and decrease = 6.75, 2.25)
1.2	l_angle, spherical, may sample
0.97	canopy emissivity
0.5 0.01	fPAR_beam, fraction of direct beam solar radiation that is PAR
0.5 0.99	fPAR_diff, fraction of diffuse solar radiation that is PAR
0.92	alpha_PAR
0.2	alpha_NIR
0.5 0.5 1.00	omega
1.5	p_crown
0.67	d_factor, C&Nfig5.5
0.1	zm_factor, C&Nfig5.5
0.2	zh_factor, 7.19
3 1 2   ps_model, photosynthesis model to use (1 = original C3, 2 = experimental C3, 3 = C4)
0.01 0.001 0.010      Rd_mult, Rd=Rd_mult*Vcmax
1.85 3.27 3.0    Jmax_mult, ratio of Jmax to Vcmax
0.93 0.7 0.99     thetaJ, curvature parameter
0.32 0.4 0.5   phiJ_sun, quantum yield, e-/photon
0.32 0.2 0.5   phiJ_shd, quantum yield, e-/photon
0.001815 0.0900    Nleaf [sean] default=0.001815 0.0900
0.0 0.785138 1.0 N_fixed_proportion
0.2 0.16 0.2  Nrubisco, proportion
38.67764 23.9 62.9  Kc25, (Pa) MM const carboxylase, 25 deg C
2.1    q10Kc, (DIM) Q_10 for kc (default 2.1)
26123.26 17600 51600   Ko25, (Pa) MM const oxygenase, 25 deg C
1.2    q10Ko, (DIM) Q_10 for ko
3.6    act25, (umol/mgRubisco/min) Rubisco activity
2.4 2.0 3.0    q10act, (DIM) Q_10 for Rubisco activity (default was 2.4)
45.72 46.0 60.0 Vcmax25, maximum Rubisco activity at 25 C, umol m-2 s-1
40.00 58.0 100.0 Vpmax25, maximum PEP carbolylase activity at 25 C, umol m-2 [sean] default = 454.83
175.0 400.0 400.0 Jmax25, maximum electron transport rate at 25 C, umol m-2 s-1
38.6 38.6 38.6 gammaStar25, compensation point at 25 C, umol
80.0 80.0 80.0 Kp25, Michaelis constant of PEP carboxylase for CO2 at 25 C, ubar
80.0 80.0 80.0 Vpr, PEP regeneration rate, umol m-2 s-1
0.0 0.15 0.15 f, correction for spectral quality of light
0.4 0.4 0.4 x, partitioning factor of electron transport rate
0.92 0.85 0.85 absorptance, fraction of irradiance absorbed
67.294 67.294 67.294 E_Vcmax, activation energy, maximum carboxylation rate, kJ mol-1
70.373 70.373 70.373 E_Vpmax, activation energy, maximum PEP rate, kJ mol-1
77.9 77.9 77.9 E_Jmax, activation energy, electron transport, kJ mol-1
36.3 36.3 36.3 E_Kp, activation energy, Michaelis reaction of PEP, kJ mol-1
59.36 59.36 59.36 E_kc, activation energy, Michaelis reaction of carboxylation, kJ mol-1
35.94 35.94 35.94 E_ko, activation energy, Michaelis reaction of oxygenation, kJ mol-1
66.3 66.3 66.3  E_Rd, activation energy, Michaelis reaction of mitochondrial respiration, kJ mol-1
23.4 23.4 23.4 E_gammaStar, activation energy, Michaelis reaction of compensation point, kJ mol-1
1.78 1.78 1.78 gm, mesophyll conductance to CO2, mol m-2 s-1
0.003 0.003 0.003 gbs, conductance of the bundle sheath, mol m-2 s-1
0.09 0.09 0.09 alphaGmax, fraction of glycolate carbon diverted to glycine during photorespiration
0.38 0.38 0.38 alphaSmax, fraction of glycolate carbon diverted to serine during photorespiration
1.5 1.5 1.5 Nmax, maximum rate of de novo nitrogen supply to the chloroplast, umol N m-2 s-1
0.303 0.269 0.258   	Gsref0, reference canopy stomatal conductance (mol m-2 s-1) [sean] this is total gs (both sides), default=.303
0.54 0.50 0.61  	m (proportion of Gsref0)
1 1 0  isAmphistomatous, (1 or 0) has stomata on both sides of leaf
-0.05   Md, used for diagnosing hydraulic model - if pressure goes higher than this value you get an error
-1.7 -1.6 midday_at_sat_kl
4.00 2.78 3.0 e_at_saturated_kl [sean] default=4.22 2.78 3.0 [5.00 6.75]
3.0 rhizosphere_width_(mm)
4 soilshells
0.21 GMP_(mm)_geometric_mean_particle_diameter [sean] default=0.21
12.0 GSD_geometric_standard_deviation_of_particle_size
1.40 1.34 BD_(MG/m3)_soil_bulk_density [sean] default=1.40, 1.34
0.47 0.43  0.5 porosity
0.09 silt_fraction [sean] default=0.09 [fine=0.17]
0.15 clay_fraction [sean] default=0.17 [fine=0.30]
0.06 residual, residual water content m3 m-3 [sean] default=0.06 set to 0.5 if clay=30
1.0 frac_absorbing_length, keep this at 1 unless you have a good reason to change it
0.01 0.1 10.0 Capacitance_(mol/Mpa*m2)_on_leaf_area_basis [sean] default=0.01 0.1 10.0
1.0 axK:latKr_shoot_modules, keep this at 1 unless you have a good reason to change it
1.0 axkr:latKr_root_modules, keep this at 1 unless you have a good reason to change it
50.0 %total_R_in_root_system, keep this at 50 unless you have a good reason to change it
-0.3 -0.1 0.05  pd_at_sat_kl
1.9 0.9 1.5 ax_Shoot-b_value_(weibull)    [sean] default= 1.942 0.9 1.5    [sean 1.9 or 2.7]
2.128 0.9 1.5 ax_Shoot-c_value_(weibull)    [sean] default= 2.128 0.9 1.5
1.9 0.9 1.5 lat_Shoot-b_value_(weibull)   [sean] default= 1.942 0.9 1.5    [sean 1.9 or 2.7]
2.128 0.9 1.5 lat_Shoot-c_value_(weibull)   [sean] default= 2.128 0.9 1.5
1.9 1.5 ax_Root-b_value_(weibull)         [sean] default= 1.942 1.5        [sean 1.9 or 2.7]
2.128 1.5 ax_Root-c_value_(weibull)         [sean] default= 2.128 1.5
1.9 1.383 1.5 lat_Root-b_value_(weibull)  [sean] default= 1.942 1.383 1.5  [sean 1.9 or 2.7]
2.128 1.48 1.5 lat_Root-c_value_(weibull)   [sean] default= 2.128 1.48 1.5
10.0 initial_conductivity(root)   [sean] default = 20.0
0.02 decrement(root)- default 0.001 [sean default=0.02]
15.0 initial_conductivity(shoot) [sean] default = 30.0
0.03 decrement(shoot) [sean] default=0.03
0.22 0.05 0.48 theta_opt [sean] field capacity?
15.0 25.0 45.0 optimal_soil_T
1.0   growth_resp_proportion
0.0011 resp_coef_root, kg kg-1 day-1 deg
0.0002 resp_coefficient_stem, kg kg-1 day-1 deg
0.0004 resp_coefficient_leaf, kg kg-1 day-1 deg [sean] default=0.0004
0.05 0.085 resp_coefficient (Q10), degC-1
72.26 71.22 73.30 EaSx, kjmol-1
0.000000995 0.000000877 0.00000111 kMsx, gCcm-3soil
0.000000000538 0.000000000347 0.000000000834 mgCcm-3soilh-1
0.00425 0.0085 0.004256 kd, d-1
0.6 0.6 0.6 kn, m3 d-1 gC-1
0.16 0.0800 0.2 kea, m3 d-1 gC-1 (for exudates) 0.16 0.0800 0.2
0.75 0.3733 0.6 kes, m3 d-1 gC-1 (for exudates) 0.75 0.3733 0.6
0.000065 0.00001625 0.000065 kl, m3 d-1 gC-1
0.0000025 0.0000025 kh, m3 d-1 gC-1
79200.0 214000.0 19782.4 Cbelowground, kg ha-1
0.00015 0.00001 0.0152229  Clitter_frac, dim
0.0012 0.0009 0.021 Croot_frac, dim
1.00 13710.0 29460.0 Cstem, kg ha-1
1.0 2.0 Csapwood, kg ha-1
0.0012 0.0009 0.1 Croot_coarse_frac, dim
0.005 0.000001 0.05 litter_capacity, m
0.20 0.48 theta_deep0, initial     [sean] now hardcoded (default 0.17 0.48)
0.40 0.48 theta_mid0, initial      [sean] now hardcoded (default 0.19 0.48)
0.20 0.48 theta_shallow0, initial  [sean] now hardcoded (default 0.22 0.48)
0.0 0.001 0.05 litter_store, initial [sean] default=0.0 0.001 0.05
55.0 60.0 28.6 SLA, m2 kgC-1 leaf  [sean] default=55.0
350.0 550.0 SRL1, m gC-1 specific root length at root diameter of 250 um
0.000125 0.000125 minRootDiam, m diameter of finest root
0.020 0.064 maxRootDiam, m diameter of thickest root
0.33 0.333 0.04 minRootLifespan, years, lifespan of finest root at lowest C:N ratio
0.5 0.1 1.0 LWP_spring_minimum, -MPa
2.35 1.5 2.5 LWP_stomatal_closure, -MPa [sean] not doing anything (default=2.35)
0 is_bryophyte (1 is yes, 0 is no) [sean] does this just eliminate apoplastic transport?
0.0 0.0 1.0 capRiseScalar, (0 to 1)  [sean] changing this (default=0.0 0.0 1.0)
1.0 1.0 1.0 precipReduction [sean] very convenient way to manipulate precip without changing climate file
1.0 0.0 1.0 drainScalar, (0 to 1) proportion of drainage absorbed by water table
0.08 0.01 0.1 leafNSCscalar (proportion of leaf structural carbon)
0 usePhenology
1.0 leafLife Span  [sean] default=1.0
10 max_iteration(the_max_number_of_iterations_to_achieve_convergence_Delta<THR
2.0 1.0 10.0 microbiomeScalar, unitless, multiplier for the initial nutrient status of microbiome [sean] default=200
0.0 snowpack_water_equivalent, m
0.0 snowpack_E_deficit_max, d1g C
0.0015 melt_Rcoef, m degC-1 31-min-1
1 0 1 useHydraulics [sean] turning off (default = 1 0 1)
0 0 1 useInputStress
1 0 0 useRefilling [sean] not doing anything
1 0 0 forceRefilling [sean] not doing anything.. commented out line 239 hydromodel
0 useLeafModule
11.211 leafAreaMax // K
0.939134 initialLeafSize //A_pot_in
0.000495803 leafArea_Rate //r
11024.02 dur_LeafExpansion//d_exp
400.0 SLA_max //SLA_max
71.1034 SLA_min //SLA_min
60.0 leaf_insertAngle // leaf insertion angle
2.5 leaf_len_to_width // leaf length to width ratio
0.90 proportion_CD //a
1970.534 phyllochron //phyllochron
13507.5 floweringTime //TTF [sean] doesn't do anything
0.96 Tbase //Tb
4926.335 therm_plant
15.0 projectedArea_init // projected shoot area at initiation
40.0 pot_size //max projected area, cm2
0.43     root_to_shoot [sean] not doing anything
1.59    leaf_to_stem   [sean] not doing anything
0 useLeafGamma
439.166 Kalpha
39.1181 Kbeta
23.6077 Nalpha
25.0841 Nbeta
123.08 ralpha
248167.0 rbeta
10 0.00001 100.0 sd_err_Ec
2.12692 0.001 15.0 sd_err_NEE
0.0 0.0 1.0 sd_err_Ec_weight
