TARGET := FamiMemEditor

.PHONY : all clean exec

all : $(TARGET).nes exec

FamiMemEditor.nes : src/_base.asm src/main.asm src/_routine.asm makefile
	python hira2hex.py src/main.asm
	asm6f -l -m -c src/_base.asm $@
	rm src/*.lst
	rm src/main.1.asm
	mv src/_base.cdl $(basename $@).cdl

clean :
	rm *.nes *.mlb *.cdl

exec : $(TARGET).nes
	start $<
