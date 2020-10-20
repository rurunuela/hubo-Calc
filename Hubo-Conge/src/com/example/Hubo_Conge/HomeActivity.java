package com.example.Hubo_Conge;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.widget.ListView;

/**
 * Created by richard on 19/11/14.
 */
public class HomeActivity extends Activity {

    public Environnement env;
    // Lancement autre activité
    static final int GET_CONTRAT_DETAIL=1;
    HomeListAdapter adapter;
    ListView list;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.home);
        this.env = new Environnement();

        adapter = new HomeListAdapter(this);
         list= (ListView)findViewById(R.id.listHome);

        list.setAdapter(adapter);
    }


    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {

        // Check which request we're responding to
        if (requestCode == GET_CONTRAT_DETAIL) {
            // Make sure the request was successful
            if (resultCode == RESULT_OK) {
                Environnement E =  data.getParcelableExtra("env");

               Log.d(" [ RETOUR Contrat detail]",this.env+""+ E );
                Log.d(" [ RETOUR Contrat Bouton]",this.env.boutonCalculerEstActive + " " + E.boutonCalculerEstActive);
                this.env = data.getParcelableExtra("env");
                // The user picked a contact.
                // The Intent's data Uri identifies which contact was selected.
                adapter.notifyDataSetChanged();
                // Do something with the contact here (bigger example below)
            }
        }
    }


}