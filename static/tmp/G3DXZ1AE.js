var src = new EventSource("http://lindorf.de/feed");
var lblStatus = document.getElementById('lblStatus');
var btnLightOff = document.getElementById('btnLightOff');
var btnLightOn = document.getElementById('btnLightOn');
src.onmessage = function(msg) {
  alert(msg);
  switch(msg) {
  case 'portstatus':
    if msg.data[0] == '1':
      lblStatus.innerHTML = '_{MsgLightsAreOn}';
    else
      lblStatus.innerHTML = '_{MsgLightsAreOff}';
  }
}