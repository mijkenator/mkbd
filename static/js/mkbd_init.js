
mkh_main();

function mkh_main() 
{
    $(function(){
        console.log("MKH_MAIN start");
        var pointer = $('<div id="mkh_pointer" style="z-index:1000;width:32px;height:32px;position:absolute;top:100px;left:100px;background-image:url(\'/img/mouse_pointer.png\')"></div>');
        $("body").append(pointer);
    });

}

function mkh_pointer_move(direction)
{
    ws.send("command:pointer_move:"+direction);        
}

