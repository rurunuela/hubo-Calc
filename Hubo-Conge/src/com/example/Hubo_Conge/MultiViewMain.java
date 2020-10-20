package com.example.Hubo_Conge;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

/**
 * Created by richard on 18/11/14.
 */
public class MultiViewMain extends BaseAdapter {
    public Context context;

    boolean boutonVisible = true;
    // data
    private String[] fruits = new String[] { "Abricot", "Banana", "Blueberry", "Mango", };
    private String[] vegetables = new String[] { "Aubergene", "Broccoli", "Chard" };

    private LayoutInflater inflater;

    public MultiViewMain(Context context) {
        super();
        this.inflater = LayoutInflater.from(context);
        this.context= context;

    }

    @Override
    public int getCount() {
        return fruits.length + vegetables.length + 2;
    }

    @Override
    public Object getItem(int position) {
        if (position == 0) {
            return "Fruits";
        } else if (position < (1 + fruits.length)) {
            return fruits[position - 1];
        } else if (position < (1 + fruits.length + 1)) {
            return "Vegetables";
        } else {
            return vegetables[position - 1 - fruits.length - 1];
        }
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {
        // A ViewHolder keeps references to children views to avoid uneccessary calls
        // to findViewById() on each row.
        ViewHolder viewHolder;
        int ItemViewType = getItemViewType(position);

        // When convertView is not null, we can reuse it directly, there is no need
        // to reinflate it. We only inflate a new View when the convertView supplied
        // by ListView is null.
        if (convertView == null) {
            viewHolder = new ViewHolder();

            // Creates a ViewHolder and store references to the children views
            // we want to bind data to.
            if (ItemViewType == TYPE_HEADER) {
                // create a header view
                convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_1, null);
                convertView.setBackgroundColor(Color.parseColor("#FFFFFF"));
                viewHolder.text1 = (TextView) convertView.findViewById(android.R.id.text1);
                viewHolder.text1.setTextColor(Color.parseColor("#000000"));

            } else if (ItemViewType == TYPE_BOUTON) {
                convertView = inflater.inflate(R.layout.boutonrow, null);
                //change FOND
                //convertView.setBackgroundResource(R.drawable.btn_red);
                //Get bouton
                Button b = (Button) convertView.findViewById(R.id.buttonCalculer);
                b.setBackgroundResource(R.drawable.btn_blue);

                b.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        boutonVisible = false;
                        Log.v("[LOG ]", " bOuton set off");

                        //context.notifyDataSetChanged();
                        // Intent i = new Intent(context, ContratActivity.class);
                        // context.startActivity(i);

                    }
                });


            } else {
                // create a fruit/vegetable view
                //  convertView = inflater.inflate(android.R.layout.simple_expandable_list_item_2, null);
                convertView = inflater.inflate(R.layout.list_row, null);
                //viewHolder.text1 = (TextView) convertView.findViewById(android.R.id.text1);
                viewHolder.text1 = (TextView) convertView.findViewById(R.id.tvCity);
                //viewHolder.text2 = (TextView) convertView.findViewById(android.R.id.text2);
                viewHolder.text2 = (TextView) convertView.findViewById(R.id.tvCondition);

                convertView.setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View v) {
                        Log.v("[ CLICK ------>", "] " + position);
                        if (position == 2) {
                            Intent i = new Intent(context, ContratActivity.class);
                            Bundle bundle = new Bundle();
                            //i.put
                            context.startActivity(i);

                        }
                    }
                });

            }

            convertView.setTag(viewHolder);
        } else {
            // Get the ViewHolder back to get fast access to the TextView
            // and the ImageView.
            viewHolder = (ViewHolder) convertView.getTag();
        }

        // Bind the data efficiently with the holder.
        String label = (String) getItem(position);
        if (ItemViewType != TYPE_BOUTON){ viewHolder.text1.setText(label);}
        if (ItemViewType == TYPE_FRUIT) {
            viewHolder.text2.setText("fruit");
        } else if (ItemViewType == TYPE_VEGETABLE) {
            viewHolder.text2.setText("vegetable");
        }

        return convertView;
    }
    static class ViewHolder {
        TextView text1;
        TextView text2;
    }

    private static final int TYPE_HEADER = 0;
    private static final int TYPE_FRUIT = 1;
    private static final int TYPE_VEGETABLE = 2;


    private static final int TYPE_BOUTON = 3;

    @Override
    public int getViewTypeCount() {
        return 4;
    }

    /**
     * Return the type depending on the position
     */
    @Override
    public int getItemViewType(int position) {
        if ((position == 0) || (position == (1 + fruits.length))) {
            return TYPE_HEADER;

        }
        else if((position == 1)){
          //  if (boutonVisible)  return TYPE_BOUTON;
          //  else return TYPE_HEADER;
            return TYPE_BOUTON;

        }
        else if (position < (2 + fruits.length)) {
            return TYPE_FRUIT;
        } else {
            return TYPE_VEGETABLE;
        }

    }


}
