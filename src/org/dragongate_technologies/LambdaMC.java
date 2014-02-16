package org.dragongate_technologies;

import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.util.Log;
import android.view.View;

public class LambdaMC extends Activity
  implements OnCancelListener{

  final String TAG = "LambdaMC";
  static boolean cancel = false;

  public boolean getCancel() {
    return cancel;
  }

  public void setCancel(boolean b) {
    cancel = b;
  }

  public void updateText(final String s) {
    runOnUiThread(new Runnable(){
      @Override
      public void run() {
	appendOutputHS(s);
      }
    });
  }

  public void onBtnClkShowLicense(View v) {
    Intent i = new Intent(this, LicenseActivity.class);
    i.putExtra("btnId", v.getId()); // optional parameters
    startActivity(i);
  }

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    onCreateHS(savedInstanceState);
  }

  @Override
  public void onStart() {
    super.onStart();
    onStartHS();
  }

  @Override
  public void onRestart() {
    super.onRestart();
    onRestartHS();
  }

  @Override
  public void onResume() {
    super.onResume();
    onResumeHS();
  }

  @Override
  public void onPause() {
    onPauseHS();
    super.onPause();
  }

  @Override
  public void onStop() {
    onStopHS();
    super.onStop();
  }

  @Override
  public void onDestroy() {
    onDestroyHS();
    super.onDestroy();
  }

  @Override
  public native void onCancel(DialogInterface dialog);

  public native void onCreateHS(Bundle savedInstanceState);
  //public native void onCreateHS(); not required?
  public native void onStartHS();
  public native void onRestartHS();
  public native void onResumeHS();
  public native void onPauseHS();
  public native void onStopHS();
  public native void onDestroyHS();

  public native void appendOutputHS(String s);
  public native void onBtnClk1HS(View v);
  public native void onBtnClk2HS(View v);
  public native void onBtnClk3HS(View v);
  public native void onBtnClk4HS(View v);
  static { System.loadLibrary("haskell"); }

}
