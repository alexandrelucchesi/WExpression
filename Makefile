wxpi: WExpression.hs WxpI.hs
	ghc --make WxpI.hs -o wxpi

clean:
	rm *.o *.hi wxpi

