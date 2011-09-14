.PHONY: all
all:
	runhaskell -W ClutchControl.hs

.PHONY: clean
clean:
	-rm *.trace
	-rm *.c *.h
	-rm *.ads
	-rm *.adb
	-rm *.mdl
	-rm *.mo
