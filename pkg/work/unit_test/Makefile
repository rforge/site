all: make_dirs make_sample_repository copy_check_script

copy_check_script:
	cp ~/bin/rforge_check_packages scripts
	patch -p0 scripts/rforge_check_packages < rforge_check_packages.diff

make_dirs:
	mkdir logs
	mkdir pkgs
	mkdir R.check
	mkdir -p R-Forge/src/contrib
	mkdir -p rsync/pkgs
	mkdir scripts

make_sample_repository:
	R --vanilla < make_sample_repository.R

clean:	
	rm -rf logs
	rm -rf pkgs
	rm -rf R.check
	rm -rf R-Forge
	rm -rf rsync
	rm -rf scripts
	rm -f *~

