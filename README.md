LambdaMC
========

A demo Haskell Android app to calculate option prices by Monte Carlo simulation.
Based on the example app at https://github.com/neurocyte/android-haskell-activity

Functionality is limited, but it may be a useful reference for anybody developing a Haskell Android app:
* Android Pause / Resume functionality.
* Screen Rotation support.
* Background worker thread updates the UI when the calculation is complete.
* Interaction between various Android UI elements and Haskell code.

Google Play listing:
https://play.google.com/store/apps/details?id=org.dragongate_technologies


Files
-----
* LambdaMC.hs           :  Haskell app code
* DgAndr.hs             :  Haskell wrapper for various Android library functions
* DgMath.hs             :  Haskell option pricing code
* LambdaMC.java         :  Minimal Java code to support the main Activity
* LicenseActivity.java  :  Java Activity to display license information (need to move more code to Haskell-land)

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

