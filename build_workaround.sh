# work around for the -shared and -r issue described here https://github.com/neurocyte/android-haskell-activity/issues/3
cp LambdaMC.cabal_1 LambdaMC.cabal
ant debug
cp LambdaMC.cabal_2 LambdaMC.cabal
ant debug
