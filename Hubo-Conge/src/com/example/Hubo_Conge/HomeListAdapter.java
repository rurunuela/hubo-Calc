package com.example.Hubo_Conge;

import android.app.Activity;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.TextView;

/**
 * Created by richard on 19/11/14.
 */
public class HomeListAdapter extends BaseAdapter {

    /*
    Structure du menu


     */

    static final int POS_BOUTON_CALC=0;
    static final int POS_ITEM_CONTRAT=1;
    static final int NUMBER_ITEM=5;
    static final String MESSAGE_BOUTON_CALC= "CALCULER";
    static final String MESSAGE_BOUTON_CALC_DISABLE= "CALCULER";




    HomeActivity context;
    private LayoutInflater inflater;

    public HomeListAdapter(HomeActivity context) {
        super();
        this.inflater = LayoutInflater.from(context);
        this.context= context;

    }

    @Override
    public int getCount() {
        return NUMBER_ITEM;
    }

    @Override
    public Object getItem(int position) {
        return null;
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {

        int ItemViewType = getItemViewType(position);
        //if (convertView == null) {


            if (position == POS_BOUTON_CALC){
                convertView = inflater.inflate(R.layout.boutonrow, null);
                Button b = (Button) convertView.findViewById(R.id.buttonCalculer);

                // Bouton calculer est actif S
                if (this.context.env.boutonCalculerEstActive) {
                    b.setBackgroundResource(R.drawable.btn_blue);
                    b.setText(MESSAGE_BOUTON_CALC);
                    b.setEnabled(true);
                }
                else {
                    b.setBackgroundResource(R.color.gray);
                    b.setText(MESSAGE_BOUTON_CALC_DISABLE);
                    b.setEnabled(false);

                }

            }
            else if (position == POS_ITEM_CONTRAT){
                convertView = inflater.inflate(R.layout.contratcell, null);

                TextView text1 = (TextView) convertView.findViewById(R.id.contratDebut);
               // TextView text2 = (TextView) convertView.findViewById(R.id.contraFin);

                text1.setText(context.env.getDateDesciption());

                TextView text2 = (TextView) convertView.findViewById(R.id.contratTitre);
                text2.setText("Contrat "+context.env.contratType);
                //text2.setText("au ?");
                //Action
                convertView.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {


                            Intent i = new Intent(context, ContratActivity.class);
                            Bundle bundle = new Bundle();
                            bundle.putParcelable("env",context.env);
                            i.putExtras(bundle);
                            context.startActivityForResult(i,context.GET_CONTRAT_DETAIL);

                        }

                });


            }
            else {
                convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null);
                convertView.setBackgroundColor(Color.parseColor("#FFFFFF"));
                TextView text1 = (TextView) convertView.findViewById(android.R.id.text1);
                text1.setText(" TODO " + position);
            }

       /* } else {


        }*/

        return convertView;
    }



}
