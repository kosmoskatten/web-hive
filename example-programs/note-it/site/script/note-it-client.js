$("document").ready(function() {
    var $layout=createLayout();
    $("body").append($layout);
    
    $("#notebutton").attr("disabled", true);

    $("#notetextarea").on("input", function() {
        var str=$(this).val().trim();
        if (str.length > 0) {
            $("#notebutton").attr("disabled", false);
        } else {
            $("#notebutton").attr("disabled", true);
        }
    });

    $("#notebutton").click(function() {
        var str=$("#notetextarea").val().trim();
        alert(str);
    });
});

// Create the main layout.
function createLayout() {
    var $layout=$("<div/>", {class: "layout"});
    var $box=$("<div/>", {class: "box"});
    var $note$div=$("<div/>", {class: "note"});
    var $note$textarea=$("<textarea/>", 
                         {class: "note",
                          autofocus: "autofocus",
                          placeholder: "Note something ...",
                          id: "notetextarea"}
                        );
    var $note$button=$("<button/>",
                       {class: "note",
                        id: "notebutton",
                        text: "Note It!"}
                      );

    $note$div.append($note$textarea);
    $note$div.append($note$button);
    $box.append($note$div);
    $layout.append($box);

    return $layout;
}
