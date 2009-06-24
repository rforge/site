all: make_dirs make_sample_repository

make_dirs:
	mkdir -p rsync/pkgs
	mkdir -p pkgs
	mkdir -p R.check
	mkdir -p R-Forge/src/contrib

make_sample_repository:
	R --vanilla < make_sample_repository.R

clean:	
	rm -rf rsync
	rm -rf pkgs
	rm -rf R.check
	rm -rf R-Forge
	rm -f *~

