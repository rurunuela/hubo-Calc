

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import net.fortuna.ical4j.model.Component;
import net.fortuna.ical4j.model.ComponentList;
import net.fortuna.ical4j.model.Property;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.parameter.Value;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.auth.AuthScope;
import org.osaf.caldav4j.CalDAVCollection;
import org.osaf.caldav4j.CalDAVConstants;
import org.osaf.caldav4j.exceptions.CalDAV4JException;
import org.osaf.caldav4j.methods.CalDAV4JMethodFactory;
import org.osaf.caldav4j.methods.HttpClient;
import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
//import org.osaf.caldav4j.credential.CaldavCredential;
import org.osaf.caldav4j.model.request.CalendarQuery;
import org.osaf.caldav4j.util.GenerateQuery;

//@RunWith(JUnit4.class)

/*

voir http://build.mnode.org/projects/ical4j/apidocs/net/fortuna/ical4j/model/component/VEvent.html

 */
public class test {


    private static  double calcDuration(VEvent ve) {
        return (    ve.getEndDate().getDate().getTime()
                - ve.getStartDate().getDate().getTime())
                / (1000. * 60. * 60.);
    }

    //  @Test
    //public void testList() throws CalDAV4JException
    public static void main(String args[]) throws CalDAV4JException {
        HttpClient httpClient = new HttpClient();
        // I tried it with zimbra - but I had no luck using google calendar
        httpClient.getHostConfiguration().setHost("37.59.121.99", 80, "http");
        String username = "admin";
        UsernamePasswordCredentials httpCredentials = new UsernamePasswordCredentials(username, "davical1602");
           httpClient.getState().setCredentials(AuthScope.ANY, httpCredentials);
         httpClient.getParams().setAuthenticationPreemptive(true);
        // Need a proxy?
        //httpClient.getHostConfiguration().setProxy("phost", 8080);
//http://37.59.121.99/davical/caldav.php/admin/calendar/
        CalDAVCollection collection = new CalDAVCollection(
                "/davical/caldav.php/"+ "richard" +"/96EECC04-B54C-4365-ACE0-D872DC2F116F/",

                (HostConfiguration) httpClient.getHostConfiguration().clone(),
                new CalDAV4JMethodFactory(),
                CalDAVConstants.PROC_ID_DEFAULT
        );
        GenerateQuery gq=new GenerateQuery();
       // TODO you might want to adjust the date
       /* try {
           gq.setFilter("VEVENT [20140101T000000Z;20141212T000000Z] : STATUS!=CANCELLED");
         //   gq.setFilter("VEVENT [20141010T000000Z;20141212T000000Z]");
        } catch (CalDAV4JException e) {
            e.printStackTrace();
        }*/
        // Get the raw caldav query
        //System.out.println("Query: "+ gq.prettyPrint());
        CalendarQuery calendarQuery = gq.generate();
       // List<net.fortuna.ical4j.model.Calendar> calendars = collection.queryCalendars(httpClient, calendarQuery);
        System.out.println("-->" +collection.getCalendar(httpClient,""));
        System.out.println("-->" +collection.queryCalendars(httpClient,calendarQuery));

        net.fortuna.ical4j.model.Calendar calendar =collection.getCalendar(httpClient,"");
     /*   for (net.fortuna.ical4j.model.Calendar calendar : calendars) {
       */     ComponentList componentList = calendar.getComponents().getComponents(Component.VEVENT);
            Iterator<VEvent> eventIterator = componentList.iterator();
            while (eventIterator.hasNext()) {
                VEvent ve = eventIterator.next();
                System.out.println("Event: "+ ve.toString());
                System.out.println("Duration (h): "+ String.format("%.2f", calcDuration(ve)));
                System.out.println("\n\n");
            }
        //}

        //ADD EVENT: //TEST
        java.util.Calendar cal = java.util.Calendar.getInstance();
        cal.set(java.util.Calendar.MONTH, java.util.Calendar.DECEMBER);
        cal.set(java.util.Calendar.DAY_OF_MONTH, 25);
        //new net.fortuna.ical4j.model.Component(cal.getTime(), "Christmas Day");
      //  VEvent christmas = new VEvent(cal.getTime(), "Christmas Day");


        //END ADD EVENT


    }
}