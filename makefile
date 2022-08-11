DOSBOX = dosbox
CURRENT_DIR = $(shell pwd)
SET_PATH = "SET PATH=%PATH%;C:\TOOLS\FRASM"
CONFIG_FILE = dosbox.conf
ASM = pong
BUILD = "BUILD $(ASM)"
NASM = "NASM $(ASM)"

all:
	dosbox -conf $(CONFIG_FILE) -c "MOUNT C: ./" -c "C:" -c $(SET_PATH) -c $(BUILD)

dos:
	dosbox -conf $(CONFIG_FILE) -c "MOUNT C: ./" -c "C:" -c $(SET_PATH)

nasm:
	dosbox -conf $(CONFIG_FILE) -c "MOUNT C: ./" -c "C:" -c $(SET_PATH) -c $(NASM)
