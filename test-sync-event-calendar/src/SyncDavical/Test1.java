package SyncDavical;

import biweekly.Biweekly;
import biweekly.ICalendar;
import biweekly.component.VEvent;
import biweekly.property.DateStart;
import biweekly.property.Summary;
import biweekly.util.Duration;
import biweekly.util.Recurrence;
import biweekly.util.org.apache.commons.codec.binary.Base64;
import com.sun.deploy.net.HttpResponse;
import sun.net.www.http.HttpClient;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.util.Date;


/*
http://stackoverflow.com/questions/9293815/caldav-protocol
http://sourceforge.net/p/biweekly/wiki/Quick%20Start/
caldav test

get EVENT
curl -X GET --user richard:secret --output test.ics "http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/"

-F file=@file.epub
test :
curl --basic --request PUT --header "Content-Type: text/calendar; charset=utf-8" --user richard:secret --data-binary @addevent.ics "http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/"
curl --basic --request PUT --header "Content-Type: text/calendar; charset=utf-8" --user richard:secret -F file=@addevent.ics "http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/"
curl --basic --request PUT --header "Content-Type: text/calendar; charset=utf-8" --user richard:secret -H 'Content-Type: application/json' -d @addevent.ics "http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/"


INFO :
https://bugzilla.mozilla.org/show_bug.cgi?id=373370
http://greenbytes.de/tech/webdav/draft-dusseault-caldav-04.html
http://sourceforge.net/p/biweekly/wiki/Quick%20Start/


Note
Attention
l'identifiant unique pour un evenement ajoute dans un calendrier est deifnit par son URL cad l'ajout
dans l'url du calendrier

dans ICAL faire ajouter abonnement et non ouvrir le lien dans ICAL pour pour voir voir les mise à jour .


 */
/**
 * Created by richard on 17/12/14.
 */
public class Test1 {

    public static void main(String args[]){
        System.out.println(" demarrage sync ");
        String str =
                "BEGIN:VCALENDAR\n" +
                        "VERSION:2.0\n" +
                        "PRODID:-////NONSGML kigkonsult.se iCalcreator 2.14//\n" +
                        "CALSCALE:GREGORIAN\n" +
                        "BEGIN:VTIMEZONE\n" +
                        "TZID:Europe/Madrid\n" +
                        "X-LIC-LOCATION:Europe/Madrid\n" +
                        "BEGIN:STANDARD\n" +
                        "DTSTART:20151025T030000\n" +
                        "TZOFFSETFROM:+0200\n" +
                        "TZOFFSETTO:+0100\n" +
                        "TZNAME:CET\n" +
                        "END:STANDARD\n" +
                        "BEGIN:DAYLIGHT\n" +
                        "DTSTART:20150329T020000\n" +
                        "TZOFFSETFROM:+0100\n" +
                        "TZOFFSETTO:+0200\n" +
                        "TZNAME:CEST\n" +
                        "END:DAYLIGHT\n" +
                        "END:VTIMEZONE\n" +
                        "BEGIN:VEVENT\n" +
                        "UID:08B933DC-19BF-4A84-B207-51D17445A2D8\n" +
                        "DTSTAMP:20141217T135723Z\n" +
                        "CLASS:PUBLIC\n" +
                        "CREATED:20141217T135723Z\n" +
                        "DTSTART;TZID=Europe/Madrid:20141210T150000\n" +
                        "DTEND;TZID=Europe/Madrid:20141210T160000\n" +
                        "LAST-MODIFIED:20141217T135723Z\n" +
                        "SEQUENCE:0\n" +
                        "SUMMARY:TEST ADD\n" +
                        "TRANSP:OPAQUE\n" +
                        "END:VEVENT\n" +
                        "END:VCALENDAR";
        //parse the first iCalendar object from the data stream
        ICalendar ical2 = Biweekly.parse(str).first();

//or parse all objects from the data stream
//List<ICalendar> icals = Biweekly.parse(str).all();

        VEvent event2 = ical2.getEvents().get(0);
        String summary2 = event2.getSummary().getValue();
        System.out.println(summary2);


        //Get event from serveur
        ICalendar ical = new ICalendar();

        VEvent event = new VEvent();

        Summary summary = event.setSummary("T V2");
        summary.setLanguage("en-us");

        Date start = new Date();
        event.setDateStart(new DateStart(start, false));
        event.setDuration(new Duration.Builder().days(1).build()); //all-day event

        //Recurrence recur = new Recurrence.Builder(Recurrence.Frequency.YEARLY).build();
        //event.setRecurrenceRule(recur);

        ical.addEvent(event);

        File file = new File("/tmp/conference.ics");
        HttpURLConnection httpURLConnection = null;

        try {
            Biweekly.write(ical).go(file);
        } catch (IOException e) {
            e.printStackTrace();
        }


        //HTTP PUT
        URL url = null;
        DataOutputStream dataOutputStream;
        try {

            //GET ICS
            url = new URL("http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/");

            String authString = "richard:secret";
            byte[] authEncBytes = Base64.encodeBase64(authString.getBytes());
            String authStringEnc = new String(authEncBytes);


           /* HttpURLConnection httpCon = ((HttpURLConnection) url.openConnection());
            httpCon.setRequestProperty("Authorization", "Basic " + authStringEnc);


            httpCon.setDoOutput(true);
            httpCon.setRequestMethod("GET");

            //OutputStreamWriter out = new OutputStreamWriter(
              //      httpCon.getOutputStream());
           // String request = Biweekly.write(ical).toString();
           // System.out.println(request);
           // out.write(request);
            //out.close();
            //httpCon.getInputStream();

            InputStream is = httpCon.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);

            int numCharsRead;
            char[] charArray = new char[1024];
            StringBuffer sb = new StringBuffer();
            while ((numCharsRead = isr.read(charArray)) > 0) {
                sb.append(charArray, 0, numCharsRead);
            }
            String result = sb.toString();
           // System.out.println(result);
*/
            //PUT ICS


            String str2 =
                    "<<<__EOD\n" +
                            "BEGIN:VCALENDAR\n" +
                            "VERSION:2.0\n" +
                            "BEGIN:VEVENT\n" +
                            "DTSTAMP:20141216T075554Z\n" +
                            "DTSTART:20141216T065554Z\n" +
                            "DTEND:20141217T085554Z\n" +
                            "DESCRIPTION:TEST ADD JAVA 4 \n" +
                            "LOCATION:Office\n" +
                            "SUMMARY:TEST ADD JAVA 4\n" +
                            "END:VEVENT\n" +
                            "END:VCALENDAR\n" +
                            "__EOD;";

            url = new URL("http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/hometest");

            httpURLConnection = (HttpURLConnection) url.openConnection();
            httpURLConnection.setRequestProperty("Authorization", "Basic " + authStringEnc);
            httpURLConnection.setRequestProperty("Content-Type", "text/calendar");
            httpURLConnection.setRequestProperty("Content-Length",str2.length()+"");
            httpURLConnection.setRequestProperty("Accept","*/*");
            //'If-None-Match: *', //Possibly this line causing a problem - unsure of what it does?
           //
            // Cette ligne permet de ne pas rajouter un evt si il existe deja
           //httpURLConnection.setRequestProperty("If-None-Match","*");
            ///

            httpURLConnection.setRequestMethod("PUT");
         //   httpURLConnection.setDoInput(true);
            httpURLConnection.setDoOutput(true);
            dataOutputStream = new DataOutputStream(httpURLConnection.getOutputStream());
            dataOutputStream.write( ( (Biweekly.write(ical).go())).getBytes("UTF8"));
           // System.out.println(Biweekly.write(ical).go());
            // MARCHE
            //dataOutputStream.write((str2).getBytes("UTF8"));
            httpURLConnection.connect();
            dataOutputStream.flush();
            dataOutputStream.close();

            System.err.println(httpURLConnection.getResponseCode());


            httpURLConnection.disconnect();



            //TEst delete
            url = new URL("http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/home4d");
            httpURLConnection = (HttpURLConnection) url.openConnection();
            httpURLConnection.setRequestProperty("Authorization", "Basic " + authStringEnc);
            httpURLConnection.setRequestProperty("Content-Type", "text/calendar");
            httpURLConnection.setDoOutput(true);

            httpURLConnection.setRequestMethod("DELETE");
            httpURLConnection.connect();
            System.err.println(httpURLConnection.getResponseCode());








        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (ProtocolException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }


    }
}
/*

DELETE
URL url = new URL("http://www.example.com/resource");
HttpURLConnection httpCon = (HttpURLConnection) url.openConnection();
httpCon.setDoOutput(true);
httpCon.setRequestProperty(
    "Content-Type", "application/x-www-form-urlencoded" );
httpCon.setRequestMethod("DELETE");
httpCon.connect();

 */