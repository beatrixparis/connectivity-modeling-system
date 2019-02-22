#

include ../arch/gfortran_compiler

.SUFFIXES: 
.SUFFIXES: .cpp .c .F .f .o .f90

.F:
	@echo "Must have an explicit rule for" $*
.f:
	@echo "Must have an explicit rule for" $*
.f90:
	@echo "Must have an explicit rule for" $*
.c:
	@echo "Must have an explicit rule for" $*

default: cms getdata

MODS =  mod_kinds.o def_constants.o def_nests.o def_particle.o def_globalvariables.o   \
        mod_random.o mod_netcdf.o mod_iounits.o mod_nciorw.o mod_calendar.o mod_ibio.o mod_netcdfoutput.o \
        mod_turb.o mod_strata.o mod_reef.o mod_diffpart.o mod_directory.o mod_mixedlayerphysics.o

OBJS =  input.o output.o restart.o util.o loop.o rungekutta.o findnest.o move.o updateloc.o \
        getphysicaldata.o getfilename.o getnestinfo.o lonlat2ij.o fldinterp.o dealloc.o cms.o 

MODS_getdata = mod_kinds.o mod_netcdf.o mod_iounits.o mod_nciorw.o mod_calendar.o mod_directory.o mod_getdata.o 
        
OBJS_getdata =  util.o getdata.o 

MODS_larva = $(MODS) mod_buoyancy_larva.o mod_mort_larva.o
OBJS_larva = $(OBJS)

cms	:	$(MODS_larva) $(OBJS_larva)
	        $(LD) $(MODS_larva) $(OBJS_larva) $(LDFLAGS) -o cms $(EXTRALIBS)	        	       
	  
getdata :	$(MODS_getdata) $(OBJS_getdata)
		$(LD) $(MODS_getdata) $(OBJS_getdata)  $(LDFLAGS) -o getdata $(EXTRALIBS)	        
	        
cms.o: cms.f90
getdata.o: getdata.f90

def_nests.o: def_nests.f90
def_particle.o: def_particle.f90
def_globalvariables.o: def_globalvariables.f90
def_constants.o: def_constants.f90

input.o: input.f90
rungekutta.o: rungekutta.f90
findnest.o: findnest.f90
move.o: move.f90
getphysicaldata.o: getphysicaldata.f90
updateloc.o: updateloc.f90
getfilename.o: getfilename.f90
getnestinfo.o: getnestinfo.f90
lonlat2ij.o: lonlat2ij.f90
fldinterp.o: fldinterp.f90
loop.o: loop.f90
dealloc.o: dealloc.f90
output.o: output.f90
restart.o: restart.f90
util.o: util.f90

mod_random.o: mod_random.f90
mod_netcdfoutput.o: mod_netcdfoutput.f90
mod_directory.o: mod_directory.c mod_directory.h
mod_ibio: mod_ibio.f90
mod_strata.o: mod_strata.f90
mod_turb.o: mod_turb.f90
mod_mort_oil.o: mod_mort_oil.f90
mod_mort_larva.o: mod_mort_larva.f90
mod_buoyancy_larva.o: mod_buoyancy_larva.f90
mod_buoyancy_oil.o: mod_buoyancy_oil.f90 
mod_reef.o: mod_reef.f90
mod_calendar.o: mod_calendar.f90
mod_kinds.o: mod_kinds.f90
mod_netcdf.o: mod_netcdf.f90
mod_iounits.o: mod_iounits.f90
mod_diffpart.o: mod_diffpart.f90
mod_nciorw.o: mod_nciorw.f90
mod_mixedlayerphysics.o: mod_mixedlayerphysics.f90
mod_getdata.o: mod_getdata.f90

.PHONY: clean
clean:   
	rm *.mod *.o cms getdata
