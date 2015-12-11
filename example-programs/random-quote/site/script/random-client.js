// Application entry.
$(document).ready(function () {
    var $layout=createLayout();
    $("body").append($layout);

    $("#quoteButton").click(function () {
        $.get("/random-quote", function (data) {
            var $newEntry = createQuoteEntry(data.image, 
                                             data.author, 
                                             data.quote);
            $("#quoteButton").after($newEntry);
            $newEntry.show("slow");
        }, "json");
    });
});

// Create the main layout.
function createLayout() {
    var $layout=$("<div/>", {class: "layout"});
    var $box=$("<div/>", {id: "box", class: "box"});
    var $quoteButton=$("<button/>", {text: "Add New Quote",
                                     id: "quoteButton", 
                                     class: "quote"});
    $box.append($quoteButton);
    $layout.append($box);

    return $layout;
}

// Create a dynamic quote entry.
function createQuoteEntry(image, author, quote) {
    var $quoteEntry=$("<div/>", {class: "quoteEntry"});
    $quoteEntry.hide();
    $quoteEntry.append(createQuoteImage(image),
                       createQuoteTextField(author, quote));

    return $quoteEntry;
}

// Create a quote image.
function createQuoteImage(image) {
    return $("<img/>", {class: "quote", src: image});
}

// Create a text field with auther and quote texts.
function createQuoteTextField(author, quote) {
    var theQuote="\"" + quote + "\"";

    var $newTextField=$("<div/>", {class: "quoteTextField"});
    var $caption=$("<p/>", {text: author, class: "author"});
    var $quote=$("<p/>", {text: theQuote, class: "quote"});
    $newTextField.append($caption, $quote);

    return $newTextField;
}
