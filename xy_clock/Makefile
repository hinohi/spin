FC := gfortran
# FC := ifort
WARM :=
OOPT :=-O3
ifeq ($(FC),gfortran)
	WARM += -Wall
	# OOPT += -static
	# OOPT += -fbounds-check -fbacktrace
	# OOPT += -g #-fcheck=all
else
	WARM += -warn all
	OOPT += -ipo -no-prec-div -xHOST #-check all
endif

NET := sq2
L := 4
SB := 1

PROGRAM = clock$(SB)_$(NET)_L$(L).out

all: $(PROGRAM)

random.o: random.f90
	$(FC) $(WARM) $(OOPT) -o $@ -c $^

$(PROGRAM): net.py random.o xy.f90
	python net.py $(NET) $(L) $(SB)
	$(FC) $(WARM) $(OOPT) -o $@ random.o net_mod.f90 xy.f90
