$("document").ready(function() {
    var $layout=createLayout();
    $("body").append($layout);
    
    disableNoteButton();
    addServerStoredEntries();

    // Add listener to the textarea to determine if the button shall be
    // enabler or not.
    $("#notetextarea").on("input", function() {
        var str=$(this).val().trim();
        if (str.length > 0) {
            enableNoteButton();
        } else {
            disableNoteButton();
        }
    });

    // Add listener to button clicks. Action will add note to DOM and
    // send note to server.
    $("#notebutton").click(function() {
        var $note$textarea=$("#notetextarea");
        var str=$note$textarea.val().trim();
        postNewNote(str);
        disableNoteButton();
        $note$textarea.val("");
    });

    // Add listen to delete "button" clicks. Action will remove note from
    // DOM and server.
    $("#box").on("click", "img.entry", function() {
        // Remote from the DOM.
        var $id=$(this).attr("id");
        var $idStr="#e" + $id;
        var $entry=$($idStr);
        $entry.remove();

        // Remote from the server.
        $.ajax({url: "/note/" + $id, 
                type: "DELETE",
                success: function () {
                }
        });
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

// Post the string a NewNote object to the server.
function postNewNote(str) {
    $.post("/note", JSON.stringify({newNote: str}), function(obj) {
        var $entry=createEntry(obj);
        $entry.hide();
        $("#notediv").after($entry);
        $entry.show("slow");
    }, "json");
}

// Disable the note button.
function disableNoteButton() {
    var $note$button=$("#notebutton");
    $note$button.attr("disabled", true);
    $note$button.removeClass("note-enabled").addClass("note-disabled");
}

// Enable the note button.
function enableNoteButton() {
    var $note$button=$("#notebutton");
    $note$button.attr("disabled", false);
    $note$button.removeClass("note-disabled").addClass("note-enabled");
}

// Get all entries from the server and add them to the DOM.
function addServerStoredEntries() {
    $.get("/note", function(objs) {
        objs.forEach(function(obj) {
            var $entry=createEntry(obj);
            $("#notediv").after($entry);
        });
    }, "json");
}

// Create a new entry from the given object.
function createEntry(obj) {
    var $entry=$("<div/>", {class: "entry-box", 
                            id: "e" + obj.resourceId});
    var $note=$("<p/>", {class: "entry", text: obj.note});
    var $del=$("<img/>", {class: "entry",
                          id: obj.resourceId,
                          src: "/image/delete.png",
                          title: "Delete entry"});
    $entry.append($note);
    $entry.append($del);

    return $entry;
}
