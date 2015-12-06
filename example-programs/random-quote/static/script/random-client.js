// Application entry.
$(document).ready(function () {
    createLayout();
    $("#quoteButton").click(function () {
        $.get("/random-quote", function (data) {
            var $newBox = createQuoteBox(data.image, 
                                         data.author, 
                                         data.quote);
            $("#quoteButton").after($newBox);
            $newBox.show("slow");
        }, "json");
    });
});

function createLayout() {
    var $layout=$("<div/>", {id: "layout", class: "layout"});
    var $quoteButton=$("<button/>", {text: "Add New Quote",
                                     id: "quoteButton", 
                                     class: "quote"});
    $("body").append($layout);
    $("#layout").append($quoteButton);
}

function createQuoteBox(image, author, quote) {
    var $newBox=$("<div/>", {class: "quoteBox"});
    $newBox.hide();
    $newBox.append(createQuoteImageBox(image),
                   createQuoteTextBox(author, quote));

    return $newBox;
}

function createQuoteImageBox(image) {
    var $newImageBox=$("<div/>", {class: "quoteImageBox"});
    var $newImage=$("<img/>", {class: "quote", src: image});
    $newImageBox.append($newImage);

    return $newImageBox;
}

function createQuoteTextBox(author, quote) {
    var $newTextBox=$("<div/>", {class: "quoteTextBox"});
    var $caption=$("<p/>", {text: author, class: "author"});
    var $quote=$("<p/>", {text: quote, class: "quote"});
    $newTextBox.append($caption, $quote);

    return $newTextBox;
}
