WExpression
===========

Running
=======

1. Directly
===========
=> Run "runhaskell WxpI.hs <file path>".

2. Compiling
============
=> Run "make";
=> Execute "./wxpi" optionally passing as argument the full path to the file containing your source code (function declarations).

3. GHCi (not recommended) *
===========================
=> Start GHCi;
=> Load the modules: ":l WxpI.hs";
=> Type "main".

* This way, IO() becomes even more awful (you can't hit backspace).

RELEASE NOTES
=============
=> When an error occurs while evaluating an expression (eg.: the user enters an expression which references an unbounded identifier), the application quits. Although not desirable, this is expected, once the default behaviour of the provided implementation of WExpression was kept. Simply relaunch the application and do it right next time ;).
