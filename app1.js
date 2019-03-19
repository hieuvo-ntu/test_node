var express = require('express')

var app = express();
app.listen(8000);

app.set('view engine','ejs');
app.set('views','views');
app.use(express.static(__dirname+'/public'));
app.get('',function(req, res){
    res.send("Homepage")
})
// app.get('/detail/:id',function(req, res){
//     var i = req.params.id
//     res.send('<h1>Detail'+i+'</h1>')
// })

app.get('/detail', function(req, res){
    res.render('cs1',{ten:[
                            "hieuvo",
                            "1",
                            "hai",
                            "bar"
                    ]})
})

