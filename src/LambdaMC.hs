{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings #-}
-----------------------------------------------------------------------------
-- 
-- Anthony Mulcahy, Dragongate Technologies
-- borzoi@dragongate-technologies.com
-- 
-- This software is a product of Dragongate Technologies which has been
-- released to the public domain. Dragongate Technologies assumes no 
-- responsibility whatsoever for its use by other parties, and makes no 
-- guarantees, expressed or implied, about its quality, reliability, or any 
-- other characteristic.
-- 
-----------------------------------------------------------------------------
module LambdaMC where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Int
import Data.Time
import Foreign.JNI
import Foreign.JNI.Lookup
import Data.Text (Text, append, pack, replace, unpack)
import Data.Text.Read (double)
import DgAndr
import DgMath

-- defaults
defOutputText = "todo defOutputText"
defOptionParams = pack . show $ VanillaOption 36 40 1 EuroPut
defmcSim = pack . show $ MCSimulation 10000 1 1 Gaussian
defRate = "0.06"
defVolatility = "0.2"

getRate ::  JObject -> JNI Text
getRate act = getViewById act "editTextRate" "id" >>= editTextGetText

setRate ::  JObject -> Text -> JNI ()
setRate act txt = getViewById act "editTextRate" "id" >>= editTextSetText txt

getVolatility ::  JObject -> JNI Text
getVolatility act = getViewById act "editTextVolatility" "id" >>= editTextGetText

setVolatility ::  JObject -> Text -> JNI ()
setVolatility act txt = getViewById act "editTextVolatility" "id" >>= editTextSetText txt

getOutputText ::  JObject -> JNI Text
getOutputText activity = getViewById activity "textView3" "id" >>= textViewGetText

setOutputText ::  JObject -> Text -> JNI ()
setOutputText activity txt = do
  --logd "setOutputText"
  --tv <- getViewById activity "textView3" "id"
  --textViewSetText txt tv
  getViewById activity "textView3" "id" >>= textViewSetText txt

getOptionParams ::  JObject -> JNI OptionParams
getOptionParams activity = do
  logd "getOptionParams"
  stockPrice' <- doubleFromEditText activity "editText1"
  strike' <- doubleFromEditText activity "editText2"
  expiry' <- doubleFromEditText activity "editText3"
  optionType' <- textFromSpinner activity "spinner1"
  let payoffFn' = read $ unpack optionType'
      optionParams' = VanillaOption stockPrice' strike' expiry' payoffFn'
  return optionParams'

setOptionParams ::  JObject -> OptionParams -> JNI ()
setOptionParams activity optionParams = do
  logd "setOptionParams"
  let stockPrice' =  pack $ show $ stockPrice optionParams
      strike' = pack $ show $ strike optionParams
      expiry' = pack $ show $ expiry optionParams
      payoffFn' = pack $ show $ payoffFn optionParams
  getViewById activity "editText1" "id" >>= editTextSetText stockPrice'
  getViewById activity "editText2" "id" >>= editTextSetText strike'
  getViewById activity "editText3" "id" >>= editTextSetText expiry'
  getViewById activity "spinner1" "id" >>= spinnerSetSelection payoffFn'

getMCSim ::  JObject -> JNI MCSimulation
getMCSim activity = do
  logd "getMCSim"
  numPaths' <- intFromEditText activity "editText4"
  numSteps' <- intFromEditText activity "editText5"
  seed' <- int32FromEditText activity "editText6"
  stochProc' <- textFromSpinner activity "spinner2"
  let stochProc = read $ unpack stochProc'
      mcSim' = MCSimulation numPaths' numSteps' seed' stochProc
  return mcSim'

setMCSim ::  JObject -> MCSimulation -> JNI ()
setMCSim activity mcSim = do
  logd "setMCSim"
  let numPaths' =  pack $ show $ numPaths mcSim
      numSteps' = pack $ show $ numSteps mcSim
      seed' = pack $ show $ seed mcSim
      stochasticProcess' = pack $ show $ stochasticProcess mcSim
  getViewById activity "editText4" "id" >>= editTextSetText numPaths'
  getViewById activity "editText5" "id" >>= editTextSetText numSteps'
  getViewById activity "editText6" "id" >>= editTextSetText seed'
  getViewById activity "spinner2" "id" >>= spinnerSetSelection stochasticProcess'
  return ()

onCreate :: JNIEnv -> JObject -> JObject -> IO ()
onCreate env activity _bundle = runJNISafe () env $ do
  getIdentifier activity "layout" "main" >>= setContentView activity

onCreateLicense :: JNIEnv -> JObject -> JObject -> IO ()
onCreateLicense env activity _bundle = runJNISafe () env $ do
  getIdentifier activity "layout" "license_activity" >>= setContentView activity

onStart :: JNIEnv -> JObject -> IO ()
onStart env activity = runJNI env $ do 
  logd "onStart"
  return ()

onRestart :: JNIEnv -> JObject -> IO ()
onRestart env activity = runJNI env $ do 
  logd "onRestart"
  return ()

onResume :: JNIEnv -> JObject -> IO ()
onResume env activity = runJNI env $ do 
  logd "onResume"
  readTextState activity "outputText" defOutputText >>= setOutputText activity
  readTextState activity "rate" defRate >>= setRate activity
  readTextState activity "volatility" defVolatility >>= setVolatility activity
  optxt <- readTextState activity "optionParams" defOptionParams
  setOptionParams activity (read $ unpack optxt)
  mctxt <- readTextState activity "mcSim" defmcSim
  setMCSim activity (read $ unpack mctxt)

onPause :: JNIEnv -> JObject -> IO ()
onPause env activity = runJNI env $ do 
  logd "onPause"
  getOutputText activity >>= saveTextState activity "outputText"
  getRate activity >>= saveTextState activity "rate"
  getVolatility activity >>= saveTextState activity "volatility"
  optionParams <- getOptionParams activity
  mcSim <- getMCSim activity
  let optionParams' = pack $ show optionParams
      mcSim' = pack $ show mcSim
  saveTextState activity "optionParams" optionParams'
  saveTextState activity "mcSim" mcSim'
  return ()

onStop :: JNIEnv
       -> JObject
       -> IO ()
onStop env activity = runJNI env $ do 
  logd "onStop"
  return ()

onDestroy :: JNIEnv
          -> JObject
          -> IO ()
onDestroy env activity = runJNI env $ do 
  logd "onDestroy"
  return ()

appendOutput :: JNIEnv
             -> JObject
             -> JString
             -> IO ()
appendOutput env activity jtxt = runJNI env $ do 
  logd "appendOutput"
  txt <- fromJString jtxt
  oldtxt <- getOutputText activity
  setOutputText activity $ oldtxt `append` "\n" `append` txt

onBtnClk1 :: JNIEnv
          -> JObject
          -> JObject
          -> IO ()
onBtnClk1 env activity view = runJNI env $ do 
  logd "onBtnClick1"
  activity' <- newGlobalRef activity
  updateText activity' "onBtnClick1" -- crashes if this is not executed
  setCancel activity' False
  pd <- showPrgDlg activity "LambdaMC Dialog" "LambdaMC Dialog Message" activity
  pd' <- newGlobalRef pd 
  -- callVoidMethod pd (jmethodid progressDialogClass "setOnCancelListener" "(Landroid/content/DialogInterface$OnCancelListener;)V") [jv activity']
  -- callVoidMethod pd (jmethodid progressDialogClass "show" "()V") []
  rate <- doubleFromEditText activity "editTextRate"
  volatility <- doubleFromEditText activity "editTextVolatility"
  optionParams' <- getOptionParams activity
  mcSim' <- getMCSim activity
  let resultStr' = pack (prettyPrint $ evalMonteCarlo optionParams' rate volatility mcSim')
      resultStr = "onBtnClk1\n" `append` pack (show optionParams') `append` "\n" `append` pack (show mcSim') `append` "\n" `append` resultStr'
  mst <- liftIO newEmptyMVar
  tId <- liftIO $ forkWorker' resultStr mst
  forkJNI $ watcher activity' (dismissAndDeletePrgDlg pd') False True tId mst
  logd "onBtnClick1-2"
  return ()

onBtnClk2 :: JNIEnv
          -> JObject
          -> JObject
          -> IO ()
onBtnClk2 env activity view = runJNI env $ do 
  logd "onBtnClick2"
  setOutputText activity ""

onBtnClk3 :: JNIEnv
          -> JObject
          -> JObject
          -> IO ()
onBtnClk3 env activity view = runJNI env $ do 
  getOutputText activity >>= clipboardSaveText activity
  getStringById activity "copyMsg" >>= toastShow activity

getCancel :: JObject -> JNI JBoolean
getCancel act = callBooleanMethod act (jmethodid myClass "getCancel" "()Z") []

setCancel :: JObject -> Bool -> JNI ()
setCancel act cancel = callVoidMethod act (jmethodid myClass "setCancel" "(Z)V") [ if cancel then JVBool 1 else JVBool 0 ]

updateText :: JObject -> Text -> JNI ()
updateText activity txt = do
  jtxt <- newString txt
  callVoidMethod activity (jmethodid myClass "updateText" "(Ljava/lang/String;)V") [jv jtxt]

onBtnClk4 :: JNIEnv
          -> JObject
          -> JObject
          -> IO ()
onBtnClk4 env act view = runJNI env $ do 
  logd "onBtnClick4"
  title <- getStringById act "app_name"
  mesg <- getStringById act "help_content"
  showAlertDlg act title mesg
  return ()

loadTextFile :: JObject
             -> JInt
             -> JNI Text
loadTextFile act id = do
  callMethod act (jmethodid licenseActivity "loadTextFile" "(I)Ljava/lang/String;") [jv id] >>= fromJString

onBtnClkLicense :: JNIEnv
                -> JObject
                -> JObject
                -> IO ()
onBtnClkLicense env act view = runJNI env $ do 
  logd "onBtnClickLicense"
  tag <- viewGetTag view
  let title = replace "_license" "" tag
      filename = replace "-" "_" tag
  id <- getIdentifier act "raw" filename
  license <- loadTextFile act id
  showAlertDlg act title license
  return ()

forkJNI :: JNI () -> JNI ()
forkJNI jaction =  do
  logd "forkJNI"
  (Right jvm) <- getJavaVM
  liftIO $ do
    forkOS $ do
      (Right env2) <- attachCurrentThread jvm
      runJNI env2 $ do
        jaction
        return ()
      err2 <- detachCurrentThread jvm
      return ()
    return ()
  return ()

forkWorker' :: Text -> MVar (Text, Text) -> IO ThreadId
forkWorker' workStr m = do
  tId <- forkOS $ do
    putMVar m ("workerRunning", "worker running")
    start <- getCurrentTime
    workStr `deepseq` return ()
    stop <- getCurrentTime
    let s = " Duration: " `append` pack (show (diffUTCTime stop start))
    swapMVar m ("workerComplete", workStr `append` s)
    return ()
  return tId

watcher :: JObject -> JNI () -> Bool -> Bool -> ThreadId -> MVar (Text, Text) -> JNI ()
watcher activity cleanUp _ False tid m = do
  logd "watcher running is false"
  cleanUp
  deleteGlobalRef activity
watcher activity cleanUp True True tid m = do
  logd "watcher -killThread-"
  liftIO $ killThread tid
  cleanUp
  deleteGlobalRef activity
  return ()
watcher activity cleanUp False True tid m = do
  liftIO $ threadDelay 100000
  cancel' <- getCancel activity
  mst <- liftIO $ readMVar m
  case mst of
    ("workerRunning", "worker running") -> do
      watcher activity cleanUp (fromJBoolean cancel') True tid m
    ("workerComplete", resultStr) -> do
      logd resultStr
      updateText activity resultStr
      watcher activity cleanUp (fromJBoolean cancel') False tid m
    otherwise -> do
      logd "otherwise"
      watcher activity cleanUp (fromJBoolean cancel') False tid m

doubleFromEditText :: JObject -> Text -> JNI Double
doubleFromEditText activity name = do
  txt <- getViewById activity name "id" >>= editTextGetText
  let Right (d, _) = double txt -- todo
  return d

intFromEditText :: JObject -> Text -> JNI Int
intFromEditText activity name = do
  d <- doubleFromEditText activity name
  return $ truncate d

int32FromEditText :: JObject -> Text -> JNI Int32
int32FromEditText activity name = do
  d <- doubleFromEditText activity name
  return $ truncate d

onCancel :: JNIEnv -> JObject -> JObject -> IO ()
onCancel env activity dlg = runJNISafe () env $ do
  jtxt <- newString "Simulation Cancelled"
  liftIO $ appendOutput env activity jtxt
  setCancel activity True

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onCancel"
  onCancel :: JNIEnv -> JObject -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onCreateHS"
  onCreate :: JNIEnv -> JObject -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onStartHS"
  onStart :: JNIEnv -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onRestartHS"
  onRestart :: JNIEnv -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onResumeHS"
  onResume :: JNIEnv -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onPauseHS"
  onPause :: JNIEnv -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onStopHS"
  onStop :: JNIEnv -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onDestroyHS"
  onDestroy :: JNIEnv -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_appendOutputHS"
  appendOutput :: JNIEnv
               -> JObject
               -> JString
               -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onBtnClk1HS"
  onBtnClk1 :: JNIEnv
            -> JObject
            -> JObject
            -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onBtnClk2HS"
  onBtnClk2 :: JNIEnv
            -> JObject
            -> JObject
            -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onBtnClk3HS"
  onBtnClk3 :: JNIEnv
            -> JObject
            -> JObject
            -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LambdaMC_onBtnClk4HS"
  onBtnClk4 :: JNIEnv
            -> JObject
            -> JObject
            -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LicenseActivity_onCreateLicenseHS"
  onCreateLicense :: JNIEnv -> JObject -> JObject -> IO ()

foreign export ccall
  "Java_org_dragongate_1technologies_LicenseActivity_onBtnClkLicense"
  onBtnClkLicense :: JNIEnv
            -> JObject
            -> JObject
            -> IO ()

