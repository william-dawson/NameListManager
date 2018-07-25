# A makefile for the example.
FC=gfortran
EXE=example

$(EXE): driver/main.o readers/scf_reader.o \
	modules/scf_input_module.o modules/io_common_module.o
	$(FC) $? -o $@

driver/main.o: driver/main.f90 modules/scf_input_module.mod
	$(FC) -c $< -o $@ -I modules/

readers/scf_reader.o: readers/scf_reader.f90 modules/scf_input_module.mod \
	  modules/io_common_module.mod
	$(FC) -c $< -o $@ -I modules/

modules/scf_input_module.mod modules/scf_input_module.o: \
    modules/scf_input_module.f90 modules/io_common_module.mod
	$(FC) -c $< -J modules -o $@

modules/io_common_module.mod modules/io_common_module.o: \
    modules/io_common_module.f90
	$(FC) -c $< -J modules -o $@

clean:
	rm doc/*.mod doc/*.o driver/*.mod \
	   driver/*.o modules/*.mod modules/*.o \
     readers/*.mod readers/*.o \
		 example

test:
	./example
