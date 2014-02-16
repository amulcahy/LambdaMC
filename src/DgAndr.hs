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
module DgAndr where

--import Control.Concurrent
--import Control.Concurrent.MVar
--import Control.DeepSeq
--import Control.Monad.IO.Class
--import Data.Int
--import Data.Time
import Data.Text (Text, append, pack, unpack)
--import Data.Text.Read (double)
import Foreign.JNI
import Foreign.JNI.Lookup

-- classes

contextClass :: JClass
contextClass = jclass "android/content/Context"

sharedPreferencesClass :: JClass
sharedPreferencesClass = jclass "android/content/SharedPreferences"

sharedPreferencesEditorClass :: JClass
sharedPreferencesEditorClass = jclass "android/content/SharedPreferences$Editor"

clipboardManagerClass :: JClass
clipboardManagerClass = jclass "android/text/ClipboardManager"

toastClass :: JClass
toastClass = jclass "android/widget/Toast"

editTextClass :: JClass
editTextClass = jclass "android/widget/EditText"

arrayAdapterClass :: JClass
arrayAdapterClass = jclass "android/widget/ArrayAdapter"

absSpinnerClass :: JClass
absSpinnerClass = jclass "android/widget/AbsSpinner"

spinnerClass :: JClass
spinnerClass = jclass "android/widget/Spinner"

viewClass :: JClass
viewClass = jclass "android/view/View"

logClass :: JClass
logClass = jclass "android/util/Log"

myClass :: JClass
myClass = jclass "org/dragongate_technologies/LambdaMC"

licenseActivity :: JClass
licenseActivity = jclass "org/dragongate_technologies/LicenseActivity"

objectClass :: JClass
objectClass = jclass "java/lang/Object"

alertDialogClass :: JClass
alertDialogClass = jclass "android/app/AlertDialog"

progressDialogClass :: JClass
progressDialogClass = jclass "android/app/ProgressDialog"

resClass :: JClass
resClass = jclass "android/content/res/Resources"

spannableStringBuilderClass :: JClass
spannableStringBuilderClass = jclass "android/text/SpannableStringBuilder"

textViewClass :: JClass
textViewClass = jclass "android/widget/TextView"

-- todo name this section

objectToString :: JObject
               -> JNI Text
objectToString obj = callObjectMethod obj (jmethodid objectClass "toString" "()Ljava/lang/String;") [] >>= fromJString

getIdentifier :: JObject
              -> Text
              -> Text
              -> JNI JInt
getIdentifier act defType name = do
  actClass <- getObjectClass act
  name' <- newString name
  defType' <- newString defType
  res <- callObjectMethod act (jmethodid actClass "getResources" "()Landroid/content/res/Resources;") []
  defPackage <- callObjectMethod act (jmethodid actClass "getPackageName" "()Ljava/lang/String;") []
  callIntMethod res (jmethodid resClass "getIdentifier" "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)I") [jv name', jv defType', jv defPackage]

findViewById :: JObject
             -> JInt
             -> JNI JObject
findViewById act id = do
  actClass <- getObjectClass act
  callMethod act (jmethodid actClass "findViewById" "(I)Landroid/view/View;") [jv id]

findStringById :: JObject
               -> JInt
               -> JNI Text
findStringById act id = do
  actClass <- getObjectClass act
  callMethod act (jmethodid actClass "getString" "(I)Ljava/lang/String;") [jv id] >>= fromJString

getStringById :: JObject
              -> Text
              -> JNI Text
getStringById act name =  getIdentifier act "string" name >>= findStringById act

getViewById :: JObject
            -> Text
            -> Text
            -> JNI JObject
getViewById act name defType = getIdentifier act defType name >>= findViewById act

viewGetTag :: JObject
           -> JNI Text
viewGetTag v = callObjectMethod v (jmethodid viewClass "getTag" "()Ljava/lang/Object;") [] >>= objectToString

spinnerGetText :: JObject
               -> JNI Text
spinnerGetText et = callObjectMethod et (jmethodid spinnerClass "getSelectedItem" "()Ljava/lang/Object;") [] >>= objectToString

spinnerSetSelection :: Text
                    -> JObject
                    -> JNI ()
spinnerSetSelection txt sp = do
  jtxt <- newString txt
  jadapter <- callObjectMethod sp (jmethodid absSpinnerClass "getAdapter" "()Landroid/widget/SpinnerAdapter;") []
  jid <- callIntMethod jadapter (jmethodid arrayAdapterClass "getPosition" "(Ljava/lang/Object;)I") [jv jtxt]
  callVoidMethod sp (jmethodid spinnerClass "setSelection" "(I)V") [jv jid]

textFromSpinner :: JObject
                -> Text
                -> JNI Text
textFromSpinner act name = getIdentifier act "id" name >>= findViewById act >>= spinnerGetText

logd :: Text
     -> JNI JInt
logd msg = do
  tag <- newString "fromHaskell" -- todo
  jtxt <- newString msg
  logId <- getStaticMethodID logClass "d" "(Ljava/lang/String;Ljava/lang/String;)I"
  callStaticIntMethod logClass logId [jv tag, jv jtxt]

editTextGetText :: JObject
                -> JNI Text
editTextGetText et = callObjectMethod et (jmethodid editTextClass "getText" "()Ljava/lang/CharSequence;") [] >>= objectToString

editTextSetText :: Text
                -> JObject
                -> JNI ()
editTextSetText txt et = do
  jtxt <- newString txt
  callMethod et (jmethodid editTextClass "setText" "(Ljava/lang/CharSequence;)V") [jv jtxt]

textViewNew :: JObject
             -> JNI JObject
textViewNew ctx = newObject textViewClass (jmethodid textViewClass "<init>" "(Landroid/content/Context;)V") [jv ctx]

textViewGetText :: JObject
                -> JNI Text
textViewGetText tv = callObjectMethod tv (jmethodid textViewClass "getText" "()Ljava/lang/CharSequence;") [] >>= objectToString

textViewSetText :: Text
                -> JObject
                -> JNI ()
textViewSetText txt tv = do
  jtxt <- newString txt
  callMethod tv (jmethodid textViewClass "setText" "(Ljava/lang/CharSequence;)V") [jv jtxt]

toastShow :: JObject
          -> Text
          -> JNI ()
toastShow act msg = do
  jtxt <- newString msg
  makeTextId <- getStaticMethodID toastClass "makeText" "(Landroid/content/Context;Ljava/lang/CharSequence;I)Landroid/widget/Toast;"
  toast <- callStaticObjectMethod toastClass makeTextId [jv act, jv jtxt, JVInt 0]
  callVoidMethod toast (jmethodid toastClass "show" "()V") []

clipboardSaveText :: JObject
                  -> Text
                  -> JNI ()
clipboardSaveText activity txt = do
  jtxt1 <- newString txt
  jtxt <- newString "clipboard"
  clipboard <- callObjectMethod activity (jmethodid contextClass "getSystemService" "(Ljava/lang/String;)Ljava/lang/Object;") [jv jtxt]
  callVoidMethod clipboard (jmethodid clipboardManagerClass "setText" "(Ljava/lang/CharSequence;)V") [jv jtxt1]

setContentView :: JObject
               -> JInt
               -> JNI ()
setContentView act id = do
  cls <- getObjectClass act
  callMethod act (jmethodid cls "setContentView" "(I)V") [jv id] :: JNI ()

readTextState :: JObject -> Text -> Text -> JNI Text
readTextState activity key defVal = do
  logd "readTextState"
  jkey <- newString key 
  jdefVal <- newString defVal
  jstr <- newString "HaskellActivityPrefsFile"
  settings <- callObjectMethod activity (jmethodid contextClass "getSharedPreferences" "(Ljava/lang/String;I)Landroid/content/SharedPreferences;") [jv jstr, JVInt 0]
  callObjectMethod settings (jmethodid sharedPreferencesClass "getString" "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;") [jv jkey, jv jdefVal] >>= fromJString

saveTextState :: JObject -> Text -> Text -> JNI ()
saveTextState activity key val = do
  logd ("saveTextState " `append` key `append` " " `append` val)
  jkey <- newString key 
  jval <- newString val
  jstr <- newString "HaskellActivityPrefsFile"
  settings <- callObjectMethod activity (jmethodid contextClass "getSharedPreferences" "(Ljava/lang/String;I)Landroid/content/SharedPreferences;") [jv jstr, jv (0 :: JInt)]
  editor <- callObjectMethod settings (jmethodid sharedPreferencesClass "edit" "()Landroid/content/SharedPreferences$Editor;") []
  callObjectMethod editor (jmethodid sharedPreferencesEditorClass "putString" "(Ljava/lang/String;Ljava/lang/String;)Landroid/content/SharedPreferences$Editor;") [jv jkey, jv jval]
  callVoidMethod editor (jmethodid sharedPreferencesEditorClass "apply" "()V") []

showPrgDlg :: JObject
           -> Text
           -> Text
           -> JObject
           -> JNI JObject
showPrgDlg act title mesg listener = do
  pd <- newObject progressDialogClass (jmethodid progressDialogClass "<init>" "(Landroid/content/Context;)V") [jv act]
  jtitle <- newString title
  jmesg <- newString mesg
  callVoidMethod pd (jmethodid progressDialogClass "setTitle" "(Ljava/lang/CharSequence;)V") [jv jtitle]
  callVoidMethod pd (jmethodid progressDialogClass "setMessage" "(Ljava/lang/CharSequence;)V") [jv jmesg]
  callVoidMethod pd (jmethodid progressDialogClass "setProgressStyle" "(I)V") [JVInt 0] -- STYLE_SPINNER = 0
  callVoidMethod pd (jmethodid progressDialogClass "setCancelable" "(Z)V") [JVBool 1]
  callVoidMethod pd (jmethodid progressDialogClass "setOnCancelListener" "(Landroid/content/DialogInterface$OnCancelListener;)V") [jv listener]
  callVoidMethod pd (jmethodid progressDialogClass "show" "()V") []
  return pd 

showAlertDlg :: JObject
           -> Text
           -> Text
           -> JNI JObject
showAlertDlg act title mesg = do
  jtitle <- newString title
  jmesg <- newString mesg
  ad <- newObject alertDialogClass (jmethodid alertDialogClass "<init>" "(Landroid/content/Context;)V") [jv act]
  callVoidMethod ad (jmethodid alertDialogClass "setTitle" "(Ljava/lang/CharSequence;)V") [jv jtitle]
  callVoidMethod ad (jmethodid alertDialogClass "setMessage" "(Ljava/lang/CharSequence;)V") [jv jmesg]
  callVoidMethod ad (jmethodid alertDialogClass "show" "()V") []
  return ad 

dismissAndDeletePrgDlg :: JObject -> JNI ()
dismissAndDeletePrgDlg pd = do
  callVoidMethod pd (jmethodid progressDialogClass "dismiss" "()V") []
  deleteGlobalRef pd
  
