$("document").ready(function() {
    var $layout=createLayout();
    $("body").append($layout);
    
    disableNoteButton();

    $("#notetextarea").on("input", function() {
        var str=$(this).val().trim();
        if (str.length > 0) {
            enableNoteButton();
        } else {
            disableNoteButton();
        }
    });

    $("#notebutton").click(function() {
        var $note$textarea=$("#notetextarea");
        var str=$note$textarea.val().trim();
        alert(str);
        disableNoteButton();
        $note$textarea.val("");
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

function disableNoteButton() {
    var $note$button=$("#notebutton");
    $note$button.attr("disabled", true);
    $note$button.removeClass("note-enabled").addClass("note-disabled");
}

function enableNoteButton() {
    var $note$button=$("#notebutton");
    $note$button.attr("disabled", false);
    $note$button.removeClass("note-disabled").addClass("note-enabled");
}
