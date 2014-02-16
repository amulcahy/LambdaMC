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
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class LicenseActivity extends Activity {
  @Override
  public void onCreate(Bundle bundle) {
    super.onCreate(bundle);
    onCreateLicenseHS(bundle);
  }

  public String loadTextFile(int id) {
    InputStream is = this.getResources().openRawResource(id);
    BufferedReader br = new BufferedReader(new InputStreamReader(is));
    String line;
    StringBuilder text = new StringBuilder();

    try {
      while ((line = br.readLine()) != null) {
	text.append(line);
	text.append('\n');
      }
      is.close();
    } catch (IOException e) {
      return "loadTextFile Error";
    }
    return text.toString();
  }

  public native void onCreateLicenseHS(Bundle savedInstanceState);
  public native void onBtnClkLicense(View v);
}

