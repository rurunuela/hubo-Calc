

$(document).ready(OnReady); // Abonne le callback à exécuter lorsque tout le DOM est chargé
function OnReady(){
    $("form").submit(OnSubmit); // Abonne un callback à l'évènement "submit" du formulaire

    /* This is the function that will get executed after the DOM is fully loaded */

  /*  $( "#datepicker" ).datepicker({
      changeMonth: true,//this option for allowing user to select month
      changeYear: true //this option for allowing user to select from year range
    });*/
   $("#datepickerdebut").datepicker();
   $("#datepickerfin").datepicker();

}

  


function OnSubmit(){
 
var markers  = [{"type":$("#typeContrat option:selected").text(),
				"datedebut":$("#datepickerdebut").datepicker({ dateFormat: 'yy-mm-dd' }).val(),
				"datefin":$("#datepickerfin").datepicker({ dateFormat: 'yy-mm-dd' }).val(),

			}];

console.log(JSON.stringify({ Markers: markers }));
console.log($("#typeContrat option:selected").text());
console.log($("#datepickerdebut").datepicker({ dateFormat: 'yy-mm-dd' }).val());
$.ajax({
    type: "POST",
    url: "http://localhost/testPostJson",
    // The key needs to match your method's input parameter (case-sensitive).
    data: JSON.stringify({ Markers: markers }),
    contentType: "application/json; charset=utf-8",
    dataType: "json",
    success:  OnSuccess,
    //function(data){alert(data);},
    failure: function(errMsg) {
        alert(" ERREUR " + errMsg);
    }
});
    return false; // Annule l'envoi classique du formulaire
}


function OnSuccess(result){
	console.log("----> " ,result.Markers[0].position);
	
$("#result").html(JSON.stringify(result));
    //$("#result").html(result); // Insère le résultat dans la balise d'id "result"
}