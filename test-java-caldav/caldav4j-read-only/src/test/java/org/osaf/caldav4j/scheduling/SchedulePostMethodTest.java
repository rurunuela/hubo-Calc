package org.osaf.caldav4j.scheduling;

import java.io.IOException;
import java.net.URISyntaxException;
import java.text.ParseException;

import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.Component;
import net.fortuna.ical4j.model.DateTime;
import net.fortuna.ical4j.model.ParameterList;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.parameter.PartStat;
import net.fortuna.ical4j.model.property.Attendee;
import net.fortuna.ical4j.model.property.Organizer;
import net.fortuna.ical4j.model.property.Uid;
import net.fortuna.ical4j.model.property.XProperty;

import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpException;
import org.junit.Ignore;
import org.junit.Test;
import org.osaf.caldav4j.BaseTestCase;
import org.osaf.caldav4j.exceptions.CalDAV4JException;
import org.osaf.caldav4j.methods.HttpClient;
import org.osaf.caldav4j.methods.PostMethod;
import org.osaf.caldav4j.methods.PutMethod;
import org.osaf.caldav4j.scheduling.methods.CalDAV4JScheduleMethodFactory;
import org.osaf.caldav4j.scheduling.methods.SchedulePostMethod;
import org.osaf.caldav4j.scheduling.util.ITipUtils;
import org.osaf.caldav4j.util.ICalendarUtils;

// TODO: work in progress
@Ignore
public class SchedulePostMethodTest extends BaseTestCase {


	private CalDAV4JScheduleMethodFactory scheduleMethodFactory = new CalDAV4JScheduleMethodFactory();

	HttpClient http = createHttpClient();
	HostConfiguration hostConfig = createHostConfiguration();
	
	public static final String BEDEWORK_RTSVC_URL = "/pubcaldav/rtsvc";
	/**
	 * create a simple meeting POSTing to /Outbox
	 * and process a response
	 */
    @Test
	public void testSimpeMeetingInvite_Accept() {


		Calendar invite = this
		.getCalendarResource("scheduling/meeting_invitation.ics");
		Uid myUid = new Uid(new DateTime().toString());
		ICalendarUtils.addOrReplaceProperty(invite.getComponent(Component.VEVENT), myUid);
		Calendar refreshEvent = this
		.getCalendarResource("scheduling/meeting_reply.ics");
		ICalendarUtils.addOrReplaceProperty(refreshEvent.getComponent(Component.VEVENT), myUid);

		SchedulePostMethod request = scheduleMethodFactory.createSchedulePostMethod();
		request.setPath(BEDEWORK_RTSVC_URL);
		request.setHostConfiguration(hostConfig);
		request.setRequestBody(invite);
		try {
			http.executeMethod(request);
			if (request.getStatusCode() != 200) {
				log.info("error: " + request.getStatusText()); 
			}
			log.info(request.getResponseBodyAsString());
		} catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		//refresh invitation
		PostMethod refresh = fixture.getMethodFactory().createPostMethod();
		refresh.setPath(BEDEWORK_RTSVC_URL);
		refresh.setHostConfiguration(hostConfig);
		refresh.setRequestBody(refreshEvent);
		refresh.setRequestHeader("Originator","mailto:r@r.it");
		refresh.setRequestHeader("Recipient","mailto:r@r.it");
		refresh.setRequestHeader("Recipient","mailto:robipolli@gmail.com");
		try {
			http.executeMethod(refresh);
			if (refresh.getStatusCode() != 200) {
				log.info("error: " + refresh.getStatusText()); 
			}
			log.info(refresh.getResponseBodyAsString());
		} catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * POST a meeting in r@r.it Outbox using this user to provide invitation to g@r.it
	 * @throws URISyntaxException 
	 */
    @Test
	public void testRealTimeScheduling_SimpleMeetingInvitation() throws URISyntaxException {


		Calendar invite = this
		.getCalendarResource("scheduling/meeting_invitation.ics");


		// replace fields from template
		VEvent event = (VEvent) invite.getComponent(Component.VEVENT);
		ICalendarUtils.addOrReplaceProperty(event,
				new Organizer("mailto:rpolli@babel.it")
		);
		ParameterList plist = new ParameterList();
		plist.add(new PartStat("NEED-ACTION"));

		ICalendarUtils.addOrReplaceProperty(event, 
				new Attendee(plist, "mailto:g@r.it"));
		event.getProperties().add(new Attendee(plist, "mailto:roberto.polli@babel.it"));
		event.getProperties().add(new Attendee(plist, "mailto:robipolli@gmail.com"));


		ICalendarUtils.addOrReplaceProperty(event, 
				new Uid(new DateTime().toString()));

		SchedulePostMethod request = scheduleMethodFactory.createSchedulePostMethod();
		request.setPath(BEDEWORK_RTSVC_URL);
		request.setHostConfiguration(hostConfig);
		request.setRequestBody(invite);
		try {
			http.executeMethod(request);
			if (request.getStatusCode() != 200) {
				log.info("error: " + request.getStatusText()); 
			}
			log.info(request.getResponseBodyAsString());
		} catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	/**
	 * process a REPLY retrieved from email
	 * @throws URISyntaxException 
	 * @throws IOException 
	 * @throws HttpException 
	 * @throws ParseException 
	 */
    @Test
	public void testRealTimeScheduling_SimpleMeetingReply() 
	throws URISyntaxException, HttpException, IOException, ParseException, CalDAV4JException
	{
		Calendar invite = BaseTestCase
		.getCalendarResource("scheduling/meeting_invitation.ics");

		VEvent event = (VEvent) invite.getComponent(Component.VEVENT);


		// r@r.it invites GMAIL
		ICalendarUtils.addOrReplaceProperty(event,new Organizer("mailto:r@r.it"));


		for (int j=0; j<10; j++) {
			Uid myUid = new Uid(new DateTime().toString() + j);
			ICalendarUtils.addOrReplaceProperty(event, myUid);
			// Create meeting in /calendar 
			log.info("PUT...");

			PutMethod request = fixture.getMethodFactory().createPutMethod();
			request.setPath(caldavCredential.home + "/calendar/" + event.getUid().getValue() + ".ics");
			request.setHostConfiguration(hostConfig);
			request.setRequestBody(invite);

			http.executeMethod(request);
			if (request.getStatusCode() != 200) {
				log.info("error: " + request.getStatusText()); 
			}
			log.info(request.getResponseBodyAsString());

			// update event like a REPLY from robipolli@gmail.com
			Calendar response = ITipUtils.ReplyInvitation(invite, new Attendee("mailto:robipolli@gmail.com"), PartStat.ACCEPTED);

			// POST to /rtsvc a REPLY from GMAIL
			log.info("REPLY...#" + j);
			PostMethod reply = fixture.getMethodFactory().createPostMethod();
			reply.setPath(BEDEWORK_RTSVC_URL);
			reply.setHostConfiguration(hostConfig);
			reply.setRequestBody(response);
			reply.setRequestHeader("originator", "mailto:r@r.it");
			reply.setRequestHeader("recipient", "mailto:r@r.it");

			http.executeMethod(reply);
			if (request.getStatusCode() != 200) {
				log.info("error: " + reply.getStatusText()); 
			}
			log.info(reply.getResponseBodyAsString());
		}

	}

	/**
	 * POST a meeting in an user's inbox
	 * @throws URISyntaxException 
	 */
    @Test
	public void testSimpleMeetingInvitation() throws URISyntaxException {
		HttpClient http = createHttpClient();
		HostConfiguration hostConfig = createHostConfiguration();

		Calendar invite = this
		.getCalendarResource("scheduling/meeting_invitation.ics");


		VEvent event = (VEvent) invite.getComponent(Component.VEVENT);
		ICalendarUtils.addOrReplaceProperty(
				event,
				new Organizer("mailto:rpolli@babel.it")
		);
		ParameterList plist = new ParameterList();
		plist.add(new PartStat("NEED-ACTION"));

		ICalendarUtils.addOrReplaceProperty(event, 
				new Attendee(plist, "mailto:r@r.it"));
		ICalendarUtils.addOrReplaceProperty(event,
				new XProperty("X-BEDEWORK-SUBMITTEDBY", "r@r.it"));
		ICalendarUtils.addOrReplaceProperty(event, 
				new Uid(new DateTime().toString()));

		SchedulePostMethod request = scheduleMethodFactory
		.createSchedulePostMethod();
		request.setPath(caldavCredential.home + "/Outbox/");
		request.setHostConfiguration(hostConfig);
		request.setRequestBody(invite);
		try {
			http.executeMethod(request);
			if (request.getStatusCode() != 200) {
				log.info("error: " + request.getStatusText()); 
			}
			log.info(request.getResponseBodyAsString());
		} catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
