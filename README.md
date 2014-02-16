LambdaMC
========

A demo Haskell Android app to calculate option prices by Monte Carlo simulation.
Based on the example at https://github.com/neurocyte/android-haskell-activity

Prerequisites
-------------
1. An Android cross-compiler built with the ghc-android tool at https://github.com/neurocyte/ghc-android
2. The foreign-jni package from https://github.com/neurocyte/foreign-jni
3. The Haskell packages base, deepseq, transformers, mtl, text, time

Building
--------
./build_workaround.sh
( to work around the -shared and -r issue described at https://github.com/neurocyte/android-haskell-activity/issues/3 )

Install
-------
sudo adb install -r bin/LambdaMC-debug.apk

