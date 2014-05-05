var ws;

$(document).ready(function () {
    WebSocketTest1();  
    $("#mkh_pair_button").click( send_pair_request );
    $("#mkh_send_command_button").click( send_command_request );
});

function send_pair_request(){
    ws.send("pair:" + $('#mkh_pair_master_guid').val());        
}

function send_command_request(){
    ws.send("command:" + $('#mkh_master_command').val());        
}

function WebSocketTest1()
{
    if ("WebSocket" in window)
    {
        console.log("WebSocket is supported by your Browser!");
        // Let us open a web socket
        var uri = location.hostname+(location.port ? ':'+location.port: '');
        console.log("URI:" + uri);
        ws = new WebSocket("ws://"+uri+"/mkh_mkbd");
        ws.onopen = function()
        {
            // Web Socket is connected, send data using send()
            ws.send("get_pair_guid");
            console.log("Message is sent...");
        };
        ws.onmessage = function (evt) 
        { 
            var received_msg = evt.data;
            var obj = JSON.parse(received_msg);
            console.log("Message is received...", received_msg);
            process_response(obj);
        };
        ws.onclose = function()
        { 
            // websocket is closed.
            console.log("Connection is closed..."); 
        };
    }
    else
    {
        // The browser doesn't support WebSocket
        alert("WebSocket NOT supported by your Browser!");
    }
}

function process_response(obj){
    if(obj.type){
        if(obj.type == "get_pair_guid"){
            var gpg = obj.value;
            $('#mkh_master_guid').text(gpg);
        }
        if(obj.type == "event" && obj.event_name == "paired"){ process_paired_mode(obj)}
        if(obj.type == "event" && obj.event_name == "unpair_s"){ process_unpair_slave(obj)}
        if(obj.type == "event" && obj.event_name == "unpair_m"){ process_unpair_master(obj)}
        if(obj.type == "event" && obj.event_name == "s_goto"){ process_slave_goto(obj)}
        if(obj.type == "event" && obj.event_name == "s_proxy"){ process_slave_proxy(obj)}
        if(obj.type == "pair"){
            if(obj.status == "ok"){
                process_master_mode(obj)
            } else {
                console.log("pairing failed");
            }
        }
    }else{
        console.log("unknown message type", obj);
    }    
}

function process_slave_goto(obj)
{
    console.log("SGOTO:", obj.src, obj);
    $('#mkh_slv_ifr').attr('src', obj.src);    
}


function process_slave_proxy(obj)
{
    console.log("SPROXY:", obj.src, obj);
    $('#mkh_slv_ifr').attr('src', '/__mkh_proxy?__mkh_i=1&__mkh_to=' + obj.src);    
}

function process_unpair_slave(obj)
{
    console.log("unpair");    
    $('#mkh_slave_panel').css('display', 'none');
    $('#mkh_master_panel').css('display', 'none');
    $('#mkh_m_div').css('display', 'block');
    $('#mkh_pair_title').css('display', 'block');
}
function process_unpair_master(obj)
{
    console.log("unpair");    
    $('#mkh_slave_panel').css('display', 'none');
    $('#mkh_master_panel').css('display', 'none');
}

function process_paired_mode(obj)
{
    console.log("paired_mode");    
    $('#mkh_slave_panel').css('display', 'block');
    $('#mkh_m_div').css('display', 'none');
    $('#mkh_pair_title').css('display', 'none');
}

function process_master_mode(obj)
{ 
    console.log("master_mode");    
    $('#mkh_master_panel').css('display', 'block');
}
