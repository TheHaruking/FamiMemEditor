TARGET := FamiMemEditor

.PHONY : all clean exec

all : $(TARGET).nes exec

src/main.1.asm : src/main.asm
	python hira2hex.py src/main.asm

FamiMemEditor.nes : src/_base.asm src/main.1.asm makefile
	asm6f -l -m -c src/_base.asm $@
	rm src/*.lst
	rm src/main.1.asm
	mv src/_base.cdl $(basename $@).cdl

clean :
	rm *.nes *.mlb *.cdl

exec : $(TARGET).nes
	start $<
