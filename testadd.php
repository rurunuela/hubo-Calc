<?php

$account = array(
    'server'=> 'http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F',
    'id'    => 'richard',
    'user'  => 'richard',
    'pass'  => 'secret'
);


$url = 'http://37.59.121.99/davical/caldav.php/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/home';
$userpwd = $account['user'] .":". $account['pass'];
$description = 'Test event description 2';
$summary = 'Test event ADD 2';
$tstart = gmdate("Ymd\THis\Z", strtotime("-1 days"));
$tend = gmdate("Ymd\THis\Z", strtotime("-1 days"));
$tstamp = gmdate("Ymd\THis\Z");

$body = <<<__EOD
BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VEVENT
DTSTAMP:$tstamp
DTSTART:$tstart
DTEND:$tend
UID:$uid
DESCRIPTION:$description
LOCATION:Office
SUMMARY:$summary
END:VEVENT
END:VCALENDAR
__EOD;
echo("--->".$tend);
$headers = array(
    'Content-Type: text/calendar; charset=utf-8',
   // 'If-None-Match: *', //Possibly this line causing a problem - unsure of what it does?
    'Content-Length: '.strlen($body),
);

$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, $userpwd);
curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'PUT');
curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
$res = curl_exec($ch);
curl_close($ch);

print_r($res);

?>