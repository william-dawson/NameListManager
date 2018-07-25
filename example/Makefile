# A makefile for the example.
FC=gfortran
EXE=example

$(EXE): driver/main.o readers/scf_reader.o modules/scf_input_module.o \
	modules/io_common_module.o
	$(FC) $^ -o $@

driver/main.o: driver/main.f90 modules/scfinputmodule.mod
	$(FC) -c $< -o $@ -I modules/

readers/scf_reader.o: readers/scf_reader.f90 modules/scfinputmodule.mod \
	  modules/iocommonmodule.mod
	$(FC) -c $< -o $@ -I modules/

modules/scf_input_module.o modules/scfinputmodule.mod: \
    modules/scf_input_module.f90 modules/iocommonmodule.mod
	$(FC) -c $< -o modules/scf_input_module.o -J modules -I modules

modules/io_common_module.o modules/iocommonmodule.mod: \
    modules/io_common_module.f90
	$(FC) -c $< -o modules/io_common_module.o -J modules -I modules

clean:
	rm doc/*.mod doc/*.o driver/*.mod \
	   driver/*.o modules/*.mod modules/*.o \
     readers/*.mod readers/*.o \
		 *.o example

test:
	./example
