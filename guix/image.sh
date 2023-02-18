#!/usr/bin/env bash

GREEN='\033[1;32m'
RED='\033[1;30m'
NC='\033[0m'

GUIX_PROFILE=build/profiles/guix
GUIX="./pre-inst-env ${GUIX_PROFILE}/bin/guix"
IMG_LOC=/tmp/img.qcow2
IMG_SIZE=64G
VM_OPTIONS=--image-type=qcow2
BUILD_OPTIONS="--keep-failed --image-size=${IMG_SIZE}"
QEMU_OPTIONS="-enable-kvm -hda ${IMG_LOC} -m 2048"
QEMU_BIOS="-bios $(${GUIX} build ovmf)/share/firmware/ovmf_x64.bin"

echo
echo -e "${GREEN}BIOS${NC}: ${QEMU_BIOS}"
echo -e "${GREEN}Executing${NC}: ${GUIX} system image ${BUILD_OPTIONS} ${VM_OPTIONS} $1"
echo
IMG=$(${GUIX} system image ${BUILD_OPTIONS} ${VM_OPTIONS} $1)
echo
echo -e "${GREEN}Copy-Image${NC}: ${IMG} -> ${IMG_LOG}"
echo
cp ${IMG} ${IMG_LOC}
chmod +w ${IMG_LOC}
echo -e "${GREEN}Starting-Image${NC}: quemu-system-x86_64 ${QUEMU_OPTIONS} ${QEMU_BIOS}"
qemu-system-x86_64 ${QEMU_OPTIONS} ${QEMU_BIOS}
