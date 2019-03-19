var express = require("../node_modules/express")
var app = express();

app.listen("8000");

app.get("/about/:id", function(req, res){
    var id = req.params.id;
    res.send("About"+id)
});

app.use(function(req, res, next){
    res.type('text/plain');
    res.status(404);
    res.send('404-Not Found');
})
