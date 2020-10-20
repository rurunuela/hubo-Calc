package com.example.Hubo_Conge;

import android.app.DatePickerDialog;
import android.app.Dialog;
import android.app.TimePickerDialog;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.text.format.DateFormat;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.*;

import java.util.*;

/**
 * Created by richard on 19/11/14.
 * date picker
 * http://developer.android.com/guide/topics/ui/controls/pickers.html
 */
public class ContratDetailAdapter extends BaseAdapter {


    private final ContratActivity context;
    private LayoutInflater inflater;

    public  final int POS_BOUTON_RET=0;
    public final int POS_CHOIX_CONTRAT=1;
    public final int POS_HEAD_DATE_DEBUT=2;
    public final int POS_CHOIX_DATE_DEBUT=3;
    public final int POS_CHOIX_DATE_IS_FIN=4;
    public final int POS_HEAD_DATE_FIN=5;
    public final int POS_CHOIX_DATE_FIN=6;

    final String MESSAGE_BOUTON_RET="RETOUR";
    final int NUMBER_ITEM=7;


    //ANimation

    ///
    List <Integer>mfilteredListe = new ArrayList<Integer>(){{
            for (int i=0; i< NUMBER_ITEM; i++){
                add(i);
            }
    }};


    public void removeItemFilteredListe(int num) {
        if ((num<0 ) || (num>NUMBER_ITEM)){
            throw new RuntimeException("Erreur Item not in list");

        }
        for (int i=0; i< mfilteredListe.size(); i++){
            if (mfilteredListe.get(i)==num){
                mfilteredListe.remove(i);
            }
        }

    }

    public void addItemFilteredListe(int num){
        if ((num<0 ) || (num>NUMBER_ITEM)){
            throw new RuntimeException("Erreur Item not in list");

        }
        boolean flag =false ;
        for (int i=0; i< mfilteredListe.size(); i++){
            if (mfilteredListe.get(i)==num){
                flag = true;
            }
        }
        if (!flag)mfilteredListe.add(num);
        Collections.sort(mfilteredListe);
    }
    final int AFFICHE_DATE_DEBUT =400;
    RadioButton radioCDD;
    RadioButton radioCDI;

    public ContratDetailAdapter(ContratActivity context) {
        super();
        this.inflater = LayoutInflater.from(context);
        this.context= context;

        //renseigne les dates de début et de fin
     /*   debutYear = c.get(Calendar.YEAR);
        debutMonth = c.get(Calendar.MONTH);
        debutDay = c.get(Calendar.DAY_OF_MONTH);
        finYear = c.get(Calendar.YEAR);
        finMonth = c.get(Calendar.MONTH);
        finDay = c.get(Calendar.DAY_OF_MONTH);*/

    }

    @Override
    public int getCount() {
        return this.mfilteredListe.size();
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
        final ContratDetailAdapter listContext = this;
       // if (convertView == null) {
        switch (mfilteredListe.get(position) ) {
            case POS_HEAD_DATE_DEBUT:
                convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null);
                convertView.setBackgroundColor(Color.parseColor("#FFFFFF"));
                TextView text1 = (TextView) convertView.findViewById(android.R.id.text1);
                text1.setText("Date de début du contrat ");
                break;

            case POS_BOUTON_RET:
                convertView = inflater.inflate(R.layout.boutonrowleftarrow, null);
                Button b = (Button) convertView.findViewById(R.id.buttonCalculer);


                b.setBackgroundResource(R.drawable.btn_blue);
                b.setText(MESSAGE_BOUTON_RET);
                b.setEnabled(true);

                b.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        //TO-DO Vérifier la validé des champs

                        context.finish();
                    }
                });
                break;
            case POS_CHOIX_DATE_IS_FIN:
                convertView = inflater.inflate(R.layout.datedefin, null);
                CheckBox chexbox = (CheckBox) convertView.findViewById(R.id.checkbox_fin_contrat);

                if (context.env.hasEndDate) {
                    chexbox.setChecked(false);

                } else {
                    chexbox.setChecked(true);
                }
                break;

            case POS_HEAD_DATE_FIN:

                convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null);
                convertView.setBackgroundColor(Color.parseColor("#FFFFFF"));
                TextView text2 = (TextView) convertView.findViewById(android.R.id.text1);
                text2.setText("Date de Fin du contrat ");
                if (!context.env.hasEndDate) {
                    //   convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null);
                    //  return convertView;
                    //  convertView.setVisibility(View.INVISIBLE);
                    //  convertView.setVisibility(View.GONE);
                }


                break;

            case POS_CHOIX_CONTRAT:
                convertView = inflater.inflate(R.layout.typecontrat, parent, false);
                radioCDD = (RadioButton) convertView.findViewById(R.id.radio_cdd);
                radioCDI = (RadioButton) convertView.findViewById(R.id.radio_cdi);

                if (context.env.contratType.compareToIgnoreCase("CDD") == 0) {
                    radioCDD.setChecked(true);
                    radioCDI.setChecked(false);

                } else {
                    radioCDD.setChecked(false);
                    radioCDI.setChecked(true);
                }


                break;
            case POS_CHOIX_DATE_FIN:

                convertView = inflater.inflate(R.layout.dateselection, null);
                final View finalConvertView = convertView;
                TextView text3 = (TextView) convertView.findViewById(R.id.dateContratSelection);
                text3.setText(context.env.getDateFinDesciption());
                // renseigne la date de début

                if (convertView != null) {
                    convertView.setOnClickListener(new View.OnClickListener() {
                        @Override
                        public void onClick(View v) {

                            DatePickerDialog dpd = new DatePickerDialog(context,
                                    new DatePickerDialog.OnDateSetListener() {

                                        @Override
                                        public void onDateSet(DatePicker view, int year,
                                                              int monthOfYear, int dayOfMonth) {

                                            context.env.finDay = dayOfMonth;
                                            context.env.finMonth = monthOfYear;
                                            context.env.finYear = year;
                                            listContext.notifyDataSetChanged();

                                        }
                                    }, context.env.finYear, context.env.finMonth, context.env.finDay
                            );
                            dpd.show();

                        }
                    });
                }

                break;
            case POS_CHOIX_DATE_DEBUT:
                convertView = inflater.inflate(R.layout.dateselection, null);
                TextView text4 = (TextView) convertView.findViewById(R.id.dateContratSelection);
                text4.setText(context.env.getDateDebutDesciption());
                if (convertView != null) {
                    convertView.setOnClickListener(new View.OnClickListener() {
                        @Override
                        public void onClick(View v) {

                            DatePickerDialog dpd = new DatePickerDialog(context,
                                    new DatePickerDialog.OnDateSetListener() {

                                        @Override
                                        public void onDateSet(DatePicker view, int year,
                                                              int monthOfYear, int dayOfMonth) {
                                            context.env.debutDay = dayOfMonth;
                                            context.env.debutMonth = monthOfYear;
                                            context.env.debutYear = year;
                                            listContext.notifyDataSetChanged();


                                        }
                                    }, context.env.debutYear, context.env.debutMonth, context.env.debutDay
                            );
                            dpd.show();


                        }
                    });
                }


                break;
            default:
                convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null);
                convertView.setBackgroundColor(Color.parseColor("#FFFFFF"));
                TextView text5 = (TextView) convertView.findViewById(android.R.id.text1);
                text5.setText(" TODO " + position);
        }
       /* }
        else {

        }*/ // convertview == null

        return convertView;
    }



    @Override
    public boolean hasStableIds(){
        return true;
    }

    @Override
    public int getItemViewType(int pos){
        return IGNORE_ITEM_VIEW_TYPE;
    }

    @Override
    public int getViewTypeCount(){
        return 1;
    }




}
