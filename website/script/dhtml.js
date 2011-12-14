function loadHtml(divId, url) {
	var request;
	if (window.XMLHttpRequest) {
		// code for IE7+, Firefox, Chrome, Opera, Safari
		request=new XMLHttpRequest();
	} else {
		// code for IE6, IE5
		request=new ActiveXObject("Microsoft.XMLHTTP");
	}
	
	request.onreadystatechange=function() {
		if (request.readyState==4 && request.status==200) {
			document.getElementById(divId).innerHTML=request.responseText;
		}
	}
	request.open("GET", url+"?"+(9999999999+Math.random()), true);
	request.send();
}

function cancelEvent(event) {
	if (event.cancelBubble) event.cancelBubble = true;
	if (event.stopPropagation) event.stopPropagation();
}

function drawLombokRibbon() {
	var canvas = document.getElementById('pepper');
	var ctx = canvas.getContext('2d');
	ctx.translate(0,0);
	ctx.rotate(-90 * Math.PI/360);
	ctx.shadowColor = "rgb(120,80,80)";
	ctx.shadowBlur = 15;
	
	var grd=ctx.createRadialGradient(60,65,1,60,65,160);
	grd.addColorStop(0,"rgb(255,230,230)");
	grd.addColorStop(1,"rgb(255,40,40)");
	ctx.fillStyle = grd;
	ctx.fillRect(-100,60,200,30);
	ctx.fillStyle = "black";
	ctx.font = "bold 16px 'Helvetica'";
	ctx.textBaseline = "hanging"
	ctx.shadowBlur = 1;
	ctx.fillText("Project Lombok", -60, 65);
}
