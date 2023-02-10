##
#  Guix System Configuration
#
# @file
# @version 0.1

GUIX_PROFILE=build/profiles/guix
GUIX=${GUIX_PROFILE}/bin/guix
PULL_EXTRA_OPTIONS=
# --allow-downgrades
BUILD_OPTIONS=--keep-failed
HOME_DIR=home
SYS_DIR=.config/guix/systems


all: build install

guix: build/profiles/guix.lock

build/profiles:
	mkdir -p build/profiles

build/profiles/guix.lock:  build/channels-lock.scm
	make build/profiles/guix

build/channels-lock.scm: build/channels.scm
	echo -e "(use-modules (guix channels))\n" > ./build/channels-lock-tmp.scm
	guix time-machine -C ./build/channels.scm -- \
	describe -f channels >> ./build/channels-lock-tmp.scm
	mv ./build/channels-lock-tmp.scm ./build/channels-lock.scm

build/profiles/guix: build/profiles build/channels-lock.scm
	guix pull -C build/channels-lock.scm -p ${GUIX_PROFILE} \
	${PULL_EXTRA_OPTIONS}

home/build: guix
	${GUIX} home build ${BUILD_OPTIONS} ${HOME_DIR}/acg.scm

home/install: home/build
	${GUIX} home reconfigure ${BUILD_OPTIONS} ${HOME_DIR}/acg.scm

system/build: guix
	${GUIX} system build ${BUILD_OPTIONS} ${SYS_DIR}/acg.scm

system/install: system/build
	sudo -E ${GUIX} system reconfigure ${BUILD_OPTIONS} ${SYS_DIR}/acg.scm

# end
