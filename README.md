# connectivity-modeling-system (CMS), for more information please read and cite Paris et al. 2013EMS

  The CMS is a multiscale stochastic Lagrangian framework developed by Professor Claire B. Paris at the Paris' Lab of the University of Miami's Rosenstiel School of Marine, Atmospheric, and Earth Science to trace complex planktonic migrations and give probabilistic estimates of dispersion and migration, marine population and oceanographic connectivity, transport and fate of oil and gas, plastics, and other pollutants, and other Lagrangian phenomena. This repository FACILITATES & ENCOURAGES COMMUNITY CONTRIBUTIONS TO CMS MODULES.

The CMS is an open-source Fortran toolbox for the multi-scale tracking of biotic and abiotic particles in the ocean. The tool is inherently multiscale, allowing for the seamless moving of particles between grids at different resolutions.

The CMS has been used on velocity fields from OFES, HYCOM, NEMO, MITgcm, UVic, ECCO2, SOSE, MOM and many other ocean models to computation dispersion, connectivity and fate in applications including large scale oceanography, marine reserve planning, and movement of marine biota all across the world.

The CMS uses RK4 timestepping and tricubic interpolation and is designed to be modular, meaning that it is relatively easy to add additional `behaviours' on the particles. Modules distributed with the code include random walk diffusion, mortality, diel migration, and mixed layer mixing. 
