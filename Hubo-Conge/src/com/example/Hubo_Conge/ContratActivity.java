package com.example.Hubo_Conge;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.CheckBox;
import android.widget.ListView;
import android.widget.RadioButton;

/**
 * Created by richard on 18/11/14.
 */
public class ContratActivity extends Activity {
    public Environnement env;
    ContratDetailAdapter adapter;
    ListView list;






    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bundle objetbunble = this.getIntent().getExtras();
        this.env = objetbunble.getParcelable("env");

        Log.d("[PASSAGE Contrat ] ",this.env+"");


        setContentView(R.layout.contratvue);
        adapter = new ContratDetailAdapter(this);
       // adapter.contrat =
      list = (ListView)findViewById(R.id.detailContrat);
        if (this.env.hasEndDate){

        }else {
            adapter.removeItemFilteredListe(adapter.POS_CHOIX_DATE_FIN);
            adapter.removeItemFilteredListe(adapter.POS_HEAD_DATE_FIN);
        }

        list.setAdapter(adapter);

        // ajoute fleche gauche retour
        //getActionBar().setDisplayHomeAsUpEnabled(true);

    }

    //OVerride back button BAD idea
   /* @Override
    public void onBackPressed() {
        Log.d("CDA", "onBackPressed Called");
       /* Intent setIntent = new Intent(Intent.ACTION_MAIN);
        setIntent.addCategory(Intent.CATEGORY_HOME);
        setIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        startActivity(setIntent);
    }*/

    @Override
    public void finish() {

        Intent data = new Intent();

        Bundle objetbunble = new Bundle();
        env.boutonCalculerEstActive = true;
        RadioButton radioCDD = (RadioButton) this.findViewById(R.id.radio_cdd);
        if (radioCDD.isChecked()) {
            env.contratType="CDD";
        }
        else {
            env.contratType = "CDI";

        }
        objetbunble.putParcelable("env",env);
        data.putExtras(objetbunble );
        setResult(RESULT_OK, data);
        super.finish();
    }

    public void onCheckboxClicked(final View view){
        final CheckBox chexbox = (CheckBox)this.findViewById(R.id.checkbox_fin_contrat);
        //Animation anim = AnimationUtils.loadAnimation(this, R.anim.fade_anim);


        if (view.getId()!= chexbox.getId()){
            if (chexbox.isChecked()) chexbox.setChecked(false);
            else chexbox.setChecked(true);
        }
        else if (chexbox.isChecked()) {
            this.env.setEndDate(false);
            Animation anim = AnimationUtils.loadAnimation(
                    ContratActivity.this, android.R.anim.slide_out_right
            );
            Animation anim2 = AnimationUtils.loadAnimation(
                    ContratActivity.this, android.R.anim.slide_out_right
            );
            anim.setDuration(500);
            anim2.setDuration(500);
            list.getChildAt(5).startAnimation(anim );
            list.getChildAt(6).startAnimation(anim2 );
            adapter.removeItemFilteredListe(adapter.POS_CHOIX_DATE_FIN);
            adapter.removeItemFilteredListe(adapter.POS_HEAD_DATE_FIN);
            new Handler().postDelayed(new Runnable() {
                public void run() {

                    adapter.notifyDataSetChanged();

                }
            }, anim2.getDuration());
        }
        else {

            this.env.setEndDate(true);
            adapter.addItemFilteredListe(adapter.POS_CHOIX_DATE_FIN);
            adapter.addItemFilteredListe(adapter.POS_HEAD_DATE_FIN);

            adapter.notifyDataSetChanged();




        }



      // if (adapter != null ) adapter.notifyDataSetChanged();


    }
    public void onChooseContratClicked(View view){
        RadioButton radioCDD = (RadioButton) this.findViewById(R.id.radio_cdd);
        RadioButton radioCDI = (RadioButton) this.findViewById(R.id.radio_cdi);
        if ((view.getId()!=radioCDD.getId()  ) &&
                (view.getId()!=radioCDI.getId()  )){
            if (radioCDD.isChecked()) {
                radioCDD.setChecked(false);
                radioCDI.setChecked(true);
            }
            else {
                radioCDD.setChecked(true);
                radioCDI.setChecked(false);
            }

        }


        if (radioCDD.isChecked()) {
            env.contratType="CDD";
        }
        else {
            env.contratType = "CDI";

        }
        adapter.notifyDataSetChanged();
    }


}