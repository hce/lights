var src=new EventSource("http://lindorf.de/feed"),lblStatus=document.getElementById('lblStatus'),btnLightOff=document.getElementById('btnLightOff'),btnLightOn=document.getElementById('btnLightOn');src.onmessage=function(msg){if(msg.data=="SwitchOff 1")return;if(msg.data=="SwitchOn 1")return;alert(msg.data[0]);if(msg.data[0]=='1'){lblStatus.innerHTML='_{MsgLightsAreOn}'}else lblStatus.innerHTML='_{MsgLightsAreOff}'};btnLightOn.onclick=turn(0);btnLightOff.onclick=turn(0)
function turn(onoroff){switch(onoroff){case 0:what="lightOff";break;case 1:what="lightOn";break};return function(){xrr=new XmlRpcRequest("http://lindorf.de/control","post");xrr.addParam(what+"=yes");xrr.send();alert("Oh yeah!");return false}}