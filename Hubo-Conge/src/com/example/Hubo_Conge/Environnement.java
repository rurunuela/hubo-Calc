package com.example.Hubo_Conge;

import android.os.Parcel;
import android.os.Parcelable;

import java.util.Calendar;

/**
 * Created by richard on 19/11/14.
 */
public class Environnement implements Parcelable {
    /** Scene HOME **/
    public boolean boutonCalculerEstActive = false;
    int num=42;
   int debutYear;
  int debutDay;
   int debutMonth;
   int finYear;
     int finDay;
     int finMonth;
    String contratType="CDD";
    public boolean hasEndDate=true;

    public String getDateDesciption(){
        return ("du " +
                this.debutDay+"/"+
                (this.debutMonth+1)+"/"+
                this.debutYear+" au "+
                this.finDay+"/"+
                this.finMonth+"/"+
                this.finYear);
    }
    public String getDateDebutDesciption(){
        return (
                this.debutDay+"/"+
                        (this.debutMonth +1)+"/"+
                this.debutYear);
    }
    public String getDateFinDesciption(){
        return (
                this.finDay+"/"+
                        (this.finMonth+1)+"/"+
                this.finYear);
    }

    public String toString(){
        return (this.num + " " +
                this.getDateDesciption());
    }
    public Environnement(){
        final Calendar c = Calendar.getInstance();
        debutYear = c.get(Calendar.YEAR);
        debutMonth = c.get(Calendar.MONTH);
        debutDay = c.get(Calendar.DAY_OF_MONTH);
        finYear = c.get(Calendar.YEAR);
        finMonth = c.get(Calendar.MONTH);
        finDay = c.get(Calendar.DAY_OF_MONTH);


    }
    public Environnement(Parcel in){
        readFromParcel(in);


    }
    public void readFromParcel(Parcel source){
        num = source.readInt();
        this.debutDay= source.readInt();
        this.debutMonth= source.readInt();
        this.debutYear= source.readInt();
        this.finDay= source.readInt();
        this.finMonth= source.readInt();
        this.finYear= source.readInt();
        boutonCalculerEstActive = Boolean.parseBoolean(source.readString());
        this.contratType=source.readString();
        this.hasEndDate=  Boolean.parseBoolean(source.readString());

    }
    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeInt(this.num);
        dest.writeInt(this.debutDay);
        dest.writeInt(this.debutMonth);
        dest.writeInt(this.debutYear);
        dest.writeInt(this.finDay);
        dest.writeInt(this.finMonth);
        dest.writeInt(this.finYear);
        dest.writeString(this.boutonCalculerEstActive+"");
        dest.writeString(this.contratType);
        dest.writeString(this.hasEndDate+"");

    }
    public static final Parcelable.Creator<Environnement> CREATOR =
            new Parcelable.Creator<Environnement>(){

                @Override
                public Environnement createFromParcel(Parcel source) {
                    return new Environnement(source);
                }

                @Override
                public Environnement[] newArray(int size) {
                    return new Environnement[size];
                }
            };

    public boolean isCDI() {
        return (this.contratType.compareToIgnoreCase("CDI")==0);
    }

    public void setEndDate(boolean endDate) {
        this.hasEndDate = endDate;
    }
}
