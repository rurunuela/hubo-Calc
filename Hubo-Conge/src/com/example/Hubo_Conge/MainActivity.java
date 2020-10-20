package com.example.Hubo_Conge;

import android.app.Activity;
import android.os.Bundle;
import android.view.Display;
import android.view.Surface;
import android.widget.ListView;

public class MainActivity extends Activity {
    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.main);

        MultiViewMain adapter = new MultiViewMain(this);
        ListView list = (ListView)findViewById(R.id.list);

        list.setAdapter(adapter);




    }
}
