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
        postNewNote(str);
        disableNoteButton();
        $note$textarea.val("");
    });

    $("#box").on("click", "img.entry", function() {
        var $id=$(this).attr("id");
        var $idStr="#e" + $id;
        alert($idStr);
        var $entry=$($idStr);
        alert($entry.attr("class"));

        $entry.remove();
    });
});

// Create the main layout.
function createLayout() {
    var $layout=$("<div/>", {class: "layout"});
    var $box=$("<div/>", {class: "box", id: "box"});
    var $note$div=$("<div/>", {class: "note", id: "notediv"});
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

function postNewNote(str) {
    $.post("/note", JSON.stringify({newNote: str}), function(obj) {
        //alert(data.resourceId);
        var $entry=createEntry(obj);
        $entry.hide();
        $("#notediv").after($entry);
        $entry.show("slow");
    }, "json");
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

function createEntry(obj) {
    var $entry=$("<div/>", {class: "entry-box", id: "e123"});
    var $note=$("<p/>", {class: "entry", text: obj.note});
    var $del=$("<img/>", {class: "entry",
                          id: "123",
                          src: "/image/delete.png",
                          title: "Delete entry"});
    $entry.append($note);
    $entry.append($del);

    return $entry;
}
