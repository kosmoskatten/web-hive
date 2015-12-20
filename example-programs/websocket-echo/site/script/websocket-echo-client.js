$("document").ready(function() {
    // Create a WebSocket.
    var ws=new WebSocket("ws://localhost:8888/echo");

    // Add listeners for the WebSocket.
    ws.onopen=function() {
        alert("The WebSocket is now open.");
    }

    ws.onmessage=function(evt) {
        $("#theecho").text(evt.data);
    }

    ws.onclose=function() {
        alert("The WebSocket has closed.");
    }

    $("#submit").click(function() {
        var $textarea=$("#thetext");
        var str=$textarea.val().trim();
        $textarea.val("");

        ws.send(str);
    });
});
