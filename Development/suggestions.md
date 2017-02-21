#CMS SUGGESTIONS
I haven’t included bugs/fixes here as they are already in github issues.
 
##User guide: [Sal/Bex]
1.	p.13 Fig. 8 remove input_florida as slightly confusing? (I can’t edit figures)
2.	Remove all references to ‘orthogrid’?
3.	p.15 (5.1) to actually run getdata type ./getdata expt_name nest_number?
4.	Add info on the changes you need to make before running cms after getdata, e.g. replacing the source data fill_value used in getdata with the cms one?
5.	p.21 figure legends should actually read:
		xstart = -90, xend = -45, ystart =15, yend =30.
not:
xstart = -90, xend = -60, ystart =-15, yend =30.
6.	Add notes on efficient running: Only have the nest files you need in the /nests dir, copy to local dir for running etc?

##Code development:
1.      Remove all references to ‘orthogrid’?
2.      Add offset option to getdata (SST/saln)
3.      Option to turn trajectory output off (code already partially modified for this, but using the old code)
4.      Add TMORT/TPC modules using diffpart module as template
5.      Stop getdata creating /output and /SCRATCH dirs? Unnecessary and confusing.
6.  	Get getdata to interpolate between or at least duplicate nests to deal with missing days? 

 
##Software management/github:
1.	Check carriage returns at end of all example input files on github (cat -v)?
2.	Upload edited user guide (as pdf?) [Sal]
3.	Add INSTALL.md file in top-level directory giving full instructions on how to compile and install the software. It should finish with instructions on how to run a simple calculation, providing all input and example output files so that the user can check that the software has been installed correctly. This means people can install CMS quickly without going through the user guide (as people may not always bother). Assume NO computing experience. [Sal - 1 month]
4.	Switch to cmake for build system (tutorial:https://cmake.org/cmake-tutorial/ or google "cmake fortran guide" or "cmake tutorial"). This will allow you to automatically find and test dependencies, so that users don't need to edit the makefiles. It will also allow you to write tests to ensure that the dependencies are ok, i.e. netcdf has been compiled with the right things. If cmake runs, then you know all the dependencies are there, so the code should compile. This will make support easier, as you will know that the user has installed all dependencies directly. [Sal - 4 months]
5.	Look to bundle dependencies into the code, i.e. use cmake to compile and install a netcdf / curl etc. libraries within the installation directory of the software. This will involve using "LD_LIBRARY_PATH" or "ORIGIN" to let the executable know where to find its dependent libraries. This is needed to allow the software to be installed into a user's home directory, thereby avoiding the need for a system administrator to have to install and update the software. [Sal - 8 months]
6.	Once you have bundled everything, and can compile to install within a single directory (i.e. $HOME/install/bin and $HOME/install/lib) you can then explore how to package this directory up into a zipped file that is sharable with users. This is a route towards having a binary that can be downloaded and installed from the website. [Sal - 1 year]
 
##I/O efficiency:
[Sal] testing whether copying all input and writing output to local dir (where I/O is faster), before copying to /output on completion improves run speed.  [Sal - 1 week]

Output: netcdf uses a huge amount of memory (it reads all the file before writing the data points we are sending to it). Ascii also problematic as most code is now done for netcdf, and files are too big for matlab, etc.
Input: Before, it only looped thought dates that we do have a release. Now, it loops since our first date of the nest file until the end (loop.f90). That’s a waste of time, since sometimes we have 4 years of data, but are dispersing for a month. Another suggestion is to read “tiles” of the netcdf, so only the positions that we need for the larvae/particles, not the entire file. The same idea could be used for polygons, not loop through all polygons, but only the polygons located on “tiles” or grid points, close to the larvae.
