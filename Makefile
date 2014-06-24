all:
	(cd mk; make all)

clean:
	(cd mk; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:sw/sosc

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/sosc
